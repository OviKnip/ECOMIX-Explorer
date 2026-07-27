# One-off data-prep script (not run as part of the Shiny app itself).
#
# The 4 projection datasets under data/ are Hive-partitioned into tens of
# thousands of tiny parquet files each (e.g. DB_Proj_Month/DB_Proj_Percentiles
# are ~94,000 files each). Building an Arrow Dataset handle for these at app
# startup (safe_open_dataset() in app.R) means enumerating every one of those
# files, which dominates cold-start time. This script collapses each dataset
# into a single flat, pre-sorted parquet file so app.R's existing
# safe_open_dataset(here("data/DB_Proj_Forcing")) etc. calls need no code
# changes - only what's on disk underneath changes.
#
# Rows are sorted before writing (primary variable column, then subbasin, ...)
# so Parquet row-group min/max statistics can prune scans the way the Hive
# directory structure used to, without needing directory partitions at all.
#
# This script is idempotent: it always reads from the current data/<name>
# Hive tree and always overwrites data_consolidated/<name>. Re-run it
# whenever the upstream data/ folders are refreshed with new/updated data.
#
# Outputs (staged in a sibling directory, NOT written into data/ directly -
# see scripts/validate_consolidation.R and the plan doc for the rollout step
# that swaps these into place after validation):
#   data_consolidated/DB_Proj_Forcing/part-0.parquet
#   data_consolidated/DB_Proj_Month/part-0.parquet
#   data_consolidated/DB_Proj_Percentiles/part-0.parquet
#   data_consolidated/DB_Proj_Year/part-0.parquet
#   data_consolidated/dataset_choices.rds   - ssp / period dropdown choices,
#                                             replacing get_partition_values()

library(arrow)
library(dplyr)
library(here)

# Kept in sync with the identically-named helpers in app.R (app.R:21-45).
safe_open_dataset <- function(dataset_path) {
  open_dataset(
    dataset_path,
    factory_options = list(
      exclude_invalid_files = TRUE,
      selector_ignore_prefixes = c(".", "_")
    )
  )
}

collect_quiet <- function(x) {
  withCallingHandlers(
    collect(x),
    warning = function(w) {
      if (grepl("Invalid metadata\\$r", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Some parquet columns (seen so far only in DB_Proj_Forcing's p10/p50/p90)
# were written with a stray R "names" attribute left over from quantile()
# output, which is what triggers the "Invalid metadata$r" warning muffled
# above. collect() already fails to reattach it, but strip any surviving
# non-"class" attribute from every column defensively before writing fresh
# parquet files, so nothing malformed can propagate into the consolidated
# output even if a future arrow version changes how collect() handles it.
strip_stray_attributes <- function(df) {
  df[] <- lapply(df, function(col) {
    keep <- intersect(names(attributes(col)), c("class", "levels", "tzone"))
    attributes(col) <- attributes(col)[keep]
    col
  })
  df
}

# Row-group size: aim for roughly one group per (primary_col, subbasin) cell
# so groups stay small enough for tight pruning without an excessive number
# of row-group footer entries.
rows_per_group_for <- function(df, primary_col) {
  n_groups <- dplyr::n_distinct(df[[primary_col]]) * dplyr::n_distinct(df$subbasin)
  n <- ceiling(nrow(df) / max(n_groups, 1))
  max(50, min(n, 5000))
}

out_dir <- here("data_consolidated")
dir.create(out_dir, showWarnings = FALSE)

consolidate <- function(name, sort_cols, primary_col) {
  cat(sprintf("== %s ==\n", name))

  src <- here("data", name)
  old_files <- list.files(src, recursive = TRUE, pattern = "\\.parquet$")
  old_size <- sum(file.size(file.path(src, old_files)))
  cat(sprintf("  reading %d files (%s)...\n", length(old_files), format(structure(old_size, class = "object_size"), units = "auto")))

  df <- safe_open_dataset(src) %>% collect_quiet()
  df <- strip_stray_attributes(df)
  df <- df %>% dplyr::arrange(across(all_of(sort_cols)))

  dest_dir <- file.path(out_dir, name)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest_file <- file.path(dest_dir, "part-0.parquet")
  write_parquet(df, dest_file, chunk_size = rows_per_group_for(df, primary_col))

  new_size <- file.size(dest_file)
  cat(sprintf(
    "  wrote 1 file, %d rows (%s -> %s)\n\n",
    nrow(df),
    format(structure(old_size, class = "object_size"), units = "auto"),
    format(structure(new_size, class = "object_size"), units = "auto")
  ))

  df
}

df_forcing <- consolidate(
  "DB_Proj_Forcing",
  sort_cols = c("variable", "subbasin", "ssp", "time_aggregation", "period", "month"),
  primary_col = "variable"
)

df_year <- consolidate(
  "DB_Proj_Year",
  sort_cols = c("hype_variable", "subbasin", "ssp", "year"),
  primary_col = "hype_variable"
)

df_month <- consolidate(
  "DB_Proj_Month",
  sort_cols = c("hype_variable", "subbasin", "ssp", "period", "month"),
  primary_col = "hype_variable"
)

df_percentiles <- consolidate(
  "DB_Proj_Percentiles",
  sort_cols = c("hype_variable", "subbasin", "ssp", "period", "prediction_percentile"),
  primary_col = "hype_variable"
)

# ---- Dropdown-choice metadata (replaces get_partition_values() in app.R) ----
#
# get_partition_values() (app.R) is only ever used to derive the ssp and
# period dropdown choices - never subbasin or hype_variable, which come from
# elsewhere (a shapefile and a hardcoded vector respectively). It works by
# parsing Hive folder names, which won't exist once the datasets above are
# flattened, so precompute the same values here while the data is already in
# memory and save them for app.R to read directly.
dataset_choices <- list(
  ssp = sort(unique(c(df_forcing$ssp, df_year$ssp, df_month$ssp, df_percentiles$ssp))),
  climate_period = sort(unique(df_forcing$period)),
  prediction_period = setdiff(sort(unique(df_month$period)), "2000-2022")
)
saveRDS(dataset_choices, file.path(out_dir, "dataset_choices.rds"))
cat("wrote data_consolidated/dataset_choices.rds\n")
cat(sprintf(
  "  ssp: %s\n  climate_period: %s\n  prediction_period: %s\n",
  paste(dataset_choices$ssp, collapse = ", "),
  paste(dataset_choices$climate_period, collapse = ", "),
  paste(dataset_choices$prediction_period, collapse = ", ")
))

cat("\nDone. Run scripts/validate_consolidation.R before swapping these into data/.\n")
