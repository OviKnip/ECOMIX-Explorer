#!/usr/bin/env python3
"""Combine the wide-format chemical concentration CSVs into compressed
long-format parquet files.

Two datasets are built:

1. Daily-by-site (`1_DAILY_BY_SITE_conc_ng_L/`): one row per date, one
   column per SITE_ID (e.g. `6PPDQ_mean_conc_by_SITE_ID.csv`). Reshaped to
   long format (date, site_id, subbasin, chemical, stat, conc_ng_L) using
   `Subbasin_Siteids_Key.csv` to attach the subbasin.

2. Monthly-by-waterbody (`2_Modelling_results_by_waterbody_monthly_conc_ng_per_L/`):
   one row per month-end date, one column per short numeric waterbody id
   (e.g. `6PPDQ_mean_conc_by_ds_waterbody.csv`). Reshaped to long format
   (date, waterbody_id, chemical, stat, conc_ng_L), replacing the short id
   with the full EA waterbody code via `marthaWBid2EAWBid.csv`.

Each dataset is streamed into its own parquet file so memory use stays
bounded regardless of how many files/rows are combined.
"""

import re
from pathlib import Path

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

REPO_ROOT = Path(__file__).resolve().parent.parent

DAILY_DIR = REPO_ROOT / "data-chem" / "1_DAILY_BY_SITE_conc_ng_L"
DAILY_OUTPUT_PATH = DAILY_DIR / "1_DAILY_BY_SITE_conc_ng_L.parquet"
DAILY_KEY_CSV = DAILY_DIR / "Subbasin_Siteids_Key.csv"
DAILY_FILENAME_RE = re.compile(r"^(?P<chemical>.+)_(?P<stat>90th|mean|median)_conc_by_SITE_ID\.csv$")

DAILY_SCHEMA = pa.schema(
    [
        ("date", pa.date32()),
        ("site_id", pa.int64()),
        ("subbasin", pa.int64()),
        ("chemical", pa.dictionary(pa.int32(), pa.string())),
        ("stat", pa.dictionary(pa.int32(), pa.string())),
        ("conc_ng_L", pa.float64()),
    ]
)

MONTHLY_DIR = REPO_ROOT / "data-chem" / "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L"
MONTHLY_OUTPUT_PATH = MONTHLY_DIR / "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet"
MONTHLY_KEY_CSV = MONTHLY_DIR / "marthaWBid2EAWBid.csv"
MONTHLY_FILENAME_RE = re.compile(r"^(?P<chemical>.+)_(?P<stat>90th|mean|median)_conc_by_ds_waterbody\.csv$")

MONTHLY_SCHEMA = pa.schema(
    [
        ("date", pa.date32()),
        ("waterbody_id", pa.string()),
        ("chemical", pa.dictionary(pa.int32(), pa.string())),
        ("stat", pa.dictionary(pa.int32(), pa.string())),
        ("conc_ng_L", pa.float64()),
    ]
)


def load_subbasin_key() -> pd.Series:
    key = pd.read_csv(DAILY_KEY_CSV, encoding="utf-8-sig")
    key["SITE_ID"] = key["SITE_ID"].astype("int64")
    key["subbasin"] = key["subbasin"].astype("int64")
    return key.set_index("SITE_ID")["subbasin"]


def load_waterbody_key() -> pd.Series:
    key = pd.read_csv(MONTHLY_KEY_CSV, encoding="utf-8-sig")
    key["id_waterbody"] = key["id_waterbody"].astype("int64")
    return key.set_index("id_waterbody")["water_body"]


def melt_daily_file(path: Path, chemical: str, stat: str, subbasin_by_site: pd.Series) -> pd.DataFrame:
    df = pd.read_csv(path)
    date_col = df.columns[0]
    df = df.rename(columns={date_col: "date"})

    long_df = df.melt(id_vars="date", var_name="site_id", value_name="conc_ng_L")
    long_df["date"] = pd.to_datetime(long_df["date"]).dt.date
    long_df["site_id"] = long_df["site_id"].astype("int64")
    long_df["subbasin"] = long_df["site_id"].map(subbasin_by_site).astype("Int64")
    long_df["chemical"] = chemical
    long_df["stat"] = stat
    long_df = long_df.dropna(subset=["conc_ng_L"])

    return long_df[["date", "site_id", "subbasin", "chemical", "stat", "conc_ng_L"]]


def melt_monthly_file(path: Path, chemical: str, stat: str, waterbody_by_id: pd.Series) -> pd.DataFrame:
    df = pd.read_csv(path)
    date_col = df.columns[0]
    df = df.rename(columns={date_col: "date"})

    long_df = df.melt(id_vars="date", var_name="waterbody_id", value_name="conc_ng_L")
    long_df["date"] = pd.to_datetime(long_df["date"]).dt.date
    long_df["waterbody_id"] = long_df["waterbody_id"].astype("int64").map(waterbody_by_id)
    long_df["chemical"] = chemical
    long_df["stat"] = stat
    long_df = long_df.dropna(subset=["conc_ng_L"])

    return long_df[["date", "waterbody_id", "chemical", "stat", "conc_ng_L"]]


def build_daily_parquet() -> None:
    subbasin_by_site = load_subbasin_key()

    files = sorted(
        (path, m.group("chemical"), m.group("stat"))
        for path in DAILY_DIR.glob("*.csv")
        if (m := DAILY_FILENAME_RE.match(path.name))
    )
    print(f"[daily-by-site] Found {len(files)} chemical concentration CSVs to combine.")

    writer = pq.ParquetWriter(DAILY_OUTPUT_PATH, DAILY_SCHEMA, compression="zstd")
    total_rows = 0
    try:
        for i, (path, chemical, stat) in enumerate(files, start=1):
            long_df = melt_daily_file(path, chemical, stat, subbasin_by_site)
            long_df["subbasin"] = long_df["subbasin"].astype("int64")

            table = pa.Table.from_pandas(long_df, schema=DAILY_SCHEMA, preserve_index=False)
            writer.write_table(table)

            total_rows += len(long_df)
            print(f"[daily-by-site] [{i}/{len(files)}] {path.name}: {len(long_df):,} rows")
    finally:
        writer.close()

    print(f"[daily-by-site] Done. Wrote {total_rows:,} rows to {DAILY_OUTPUT_PATH}")


def build_monthly_parquet() -> None:
    waterbody_by_id = load_waterbody_key()

    files = sorted(
        (path, m.group("chemical"), m.group("stat"))
        for path in MONTHLY_DIR.glob("*.csv")
        if (m := MONTHLY_FILENAME_RE.match(path.name))
    )
    print(f"[monthly-by-waterbody] Found {len(files)} chemical concentration CSVs to combine.")

    writer = pq.ParquetWriter(MONTHLY_OUTPUT_PATH, MONTHLY_SCHEMA, compression="zstd")
    total_rows = 0
    try:
        for i, (path, chemical, stat) in enumerate(files, start=1):
            long_df = melt_monthly_file(path, chemical, stat, waterbody_by_id)

            table = pa.Table.from_pandas(long_df, schema=MONTHLY_SCHEMA, preserve_index=False)
            writer.write_table(table)

            total_rows += len(long_df)
            print(f"[monthly-by-waterbody] [{i}/{len(files)}] {path.name}: {len(long_df):,} rows")
    finally:
        writer.close()

    print(f"[monthly-by-waterbody] Done. Wrote {total_rows:,} rows to {MONTHLY_OUTPUT_PATH}")


def main() -> None:
    build_daily_parquet()
    build_monthly_parquet()


if __name__ == "__main__":
    main()
