# One-off validation script (not run as part of the Shiny app itself).
#
# Confirms the flat files written by scripts/consolidate_proj_data.R into
# data_consolidated/ are row-for-row equivalent to the current Hive-
# partitioned data/ folders, and reports the startup-time improvement from
# opening a single file instead of walking the old partition tree. Run this
# AFTER consolidate_proj_data.R and BEFORE swapping data_consolidated/* into
# data/ (see the rollout steps in the plan doc / script header comments).
#
# Exits with a non-zero status (via stop()) on the first mismatch found, so
# it's safe to gate the rollout on this script succeeding end-to-end.

library(arrow)
library(dplyr)
library(here)

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

# Sorts collected rows by every column and compares values, so row order
# differences (irrelevant now that Hive directory order no longer applies)
# don't register as false mismatches.
expect_equivalent <- function(old, new, label) {
  if (nrow(old) != nrow(new)) {
    stop(sprintf("[%s] row count mismatch: old=%d new=%d", label, nrow(old), nrow(new)))
  }
  if (!setequal(names(old), names(new))) {
    stop(sprintf("[%s] column mismatch: old={%s} new={%s}", label,
                 paste(sort(names(old)), collapse = ","), paste(sort(names(new)), collapse = ",")))
  }

  cols <- sort(names(old))
  old_s <- old %>% dplyr::select(all_of(cols)) %>% dplyr::arrange(across(everything()))
  new_s <- new %>% dplyr::select(all_of(cols)) %>% dplyr::arrange(across(everything()))

  ok <- isTRUE(all.equal(as.data.frame(old_s), as.data.frame(new_s), tolerance = 1e-8, check.attributes = FALSE))
  if (!ok) {
    stop(sprintf("[%s] value mismatch between old and new datasets", label))
  }
  cat(sprintf("  [ok] %s (%d rows)\n", label, nrow(old)))
}

validate_dataset <- function(name, filter_variants) {
  cat(sprintf("== %s ==\n", name))

  old_path <- here("data", name)
  new_path <- here("data_consolidated", name)

  t_old <- system.time(ds_old <- safe_open_dataset(old_path))
  t_new <- system.time(ds_new <- safe_open_dataset(new_path))
  cat(sprintf("  open_dataset(): old=%.3fs  new=%.3fs\n", t_old["elapsed"], t_new["elapsed"]))

  n_old <- ds_old %>% summarise(n = n()) %>% collect_quiet() %>% pull(n)
  n_new <- ds_new %>% summarise(n = n()) %>% collect_quiet() %>% pull(n)
  expect_equivalent(data.frame(n = n_old), data.frame(n = n_new), paste0(name, " / total row count"))

  for (variant in filter_variants) {
    old_df <- ds_old %>% variant$filter_fn() %>% collect_quiet()
    new_df <- ds_new %>% variant$filter_fn() %>% collect_quiet()
    expect_equivalent(old_df, new_df, paste0(name, " / ", variant$label))
  }

  list(open_time_old = t_old["elapsed"], open_time_new = t_new["elapsed"])
}

# ---- Pick a few representative sample values from the consolidated data ----
# (cheap: each is a single flat file, unlike sampling from the old Hive tree)

sample_values <- function(ds_path, cols) {
  safe_open_dataset(ds_path) %>%
    dplyr::distinct(across(all_of(cols))) %>%
    collect_quiet()
}

forcing_samples <- sample_values(here("data_consolidated/DB_Proj_Forcing"), c("subbasin", "variable"))
month_samples <- sample_values(here("data_consolidated/DB_Proj_Month"), c("subbasin", "hype_variable"))

pick <- function(df, i) df[max(1, min(i, nrow(df))), ]

forcing_pick_first <- pick(forcing_samples, 1)
forcing_pick_mid <- pick(forcing_samples, ceiling(nrow(forcing_samples) / 2))
forcing_pick_last <- pick(forcing_samples, nrow(forcing_samples))

month_pick_first <- pick(month_samples, 1)
month_pick_mid <- pick(month_samples, ceiling(nrow(month_samples) / 2))
month_pick_last <- pick(month_samples, nrow(month_samples))

# ---- DB_Proj_Forcing: mirrors output$climate_plot (app.R) and the
#      precip/temp branch of build_tabular_download() ----
forcing_variants <- list(
  list(label = "climate_plot-shape (first subbasin/variable)", filter_fn = function(ds) {
    ds %>% filter(variable == forcing_pick_first$variable, subbasin == forcing_pick_first$subbasin,
                  ssp %in% c("Baseline", "SSP585"), time_aggregation == "monthly")
  }),
  list(label = "climate_plot-shape (mid subbasin/variable)", filter_fn = function(ds) {
    ds %>% filter(variable == forcing_pick_mid$variable, subbasin == forcing_pick_mid$subbasin,
                  ssp %in% c("Baseline", "SSP585"), time_aggregation == "monthly")
  }),
  list(label = "climate_plot-shape (last subbasin/variable)", filter_fn = function(ds) {
    ds %>% filter(variable == forcing_pick_last$variable, subbasin == forcing_pick_last$subbasin,
                  ssp %in% c("Baseline", "SSP585"), time_aggregation == "monthly")
  }),
  list(label = "build_tabular_download-shape (whole dataset by variable)", filter_fn = function(ds) {
    ds %>% filter(variable == "Precipitation", time_aggregation == "monthly")
  })
)

# ---- DB_Proj_Month: mirrors output$projections_monthly_plot and the HYPE
#      branch of build_tabular_download() ----
month_variants <- list(
  list(label = "monthly_plot-shape (first subbasin/hype_variable)", filter_fn = function(ds) {
    ds %>% filter(subbasin == month_pick_first$subbasin, hype_variable == month_pick_first$hype_variable,
                  ssp %in% c("Baseline", "SSP585"), prediction_percentile %in% c("p10", "p50", "p90"))
  }),
  list(label = "monthly_plot-shape (mid subbasin/hype_variable)", filter_fn = function(ds) {
    ds %>% filter(subbasin == month_pick_mid$subbasin, hype_variable == month_pick_mid$hype_variable,
                  ssp %in% c("Baseline", "SSP585"), prediction_percentile %in% c("p10", "p50", "p90"))
  }),
  list(label = "monthly_plot-shape (last subbasin/hype_variable)", filter_fn = function(ds) {
    ds %>% filter(subbasin == month_pick_last$subbasin, hype_variable == month_pick_last$hype_variable,
                  ssp %in% c("Baseline", "SSP585"), prediction_percentile %in% c("p10", "p50", "p90"))
  }),
  list(label = "build_tabular_download-shape (whole dataset by hype_variable/percentile)", filter_fn = function(ds) {
    ds %>% filter(hype_variable == month_pick_first$hype_variable, prediction_percentile == "p50")
  })
)

# ---- DB_Proj_Year: mirrors output$projections_yearly_plot ----
year_variants <- list(
  list(label = "yearly_plot-shape", filter_fn = function(ds) {
    ds %>% filter(subbasin == month_pick_first$subbasin, hype_variable == month_pick_first$hype_variable,
                  ssp %in% c("Baseline", "SSP585"), prediction_percentile %in% c("p10", "p50", "p90"))
  })
)

# ---- DB_Proj_Percentiles: mirrors output$projections_cfc_plot ----
percentiles_variants <- list(
  list(label = "cfc_plot-shape", filter_fn = function(ds) {
    ds %>% filter(subbasin == month_pick_first$subbasin, hype_variable == month_pick_first$hype_variable,
                  ssp %in% c("Baseline", "SSP585"))
  })
)

timings <- list(
  DB_Proj_Forcing = validate_dataset("DB_Proj_Forcing", forcing_variants),
  DB_Proj_Month = validate_dataset("DB_Proj_Month", month_variants),
  DB_Proj_Year = validate_dataset("DB_Proj_Year", year_variants),
  DB_Proj_Percentiles = validate_dataset("DB_Proj_Percentiles", percentiles_variants)
)

cat("\n== open_dataset() timing summary ==\n")
for (name in names(timings)) {
  cat(sprintf("  %-22s old=%.3fs  new=%.3fs\n", name, timings[[name]]$open_time_old, timings[[name]]$open_time_new))
}

cat("\nAll checks passed. Safe to proceed with the rollout swap described in the plan doc.\n")
