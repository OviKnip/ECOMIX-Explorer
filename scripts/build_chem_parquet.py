#!/usr/bin/env python3
"""Combine the wide-format daily-by-site chemical concentration CSVs into a
single long-format parquet file.

Each input CSV (e.g. `6PPDQ_mean_conc_by_SITE_ID.csv`) has one row per date
and one column per SITE_ID. This script reshapes every such file to long
format (date, site_id, chemical, stat, conc_ng_L), attaches the subbasin
lookup, and streams the result into one parquet file so memory use stays
bounded regardless of how many files/rows are combined.
"""

import re
from pathlib import Path

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

DATA_DIR = Path(__file__).resolve().parent.parent / "data-chem" / "1_DAILY_BY_SITE_conc_ng_L"
OUTPUT_PATH = DATA_DIR / "1_DAILY_BY_SITE_conc_ng_L.parquet"
KEY_CSV = DATA_DIR / "Subbasin_Siteids_Key.csv"

FILENAME_RE = re.compile(r"^(?P<chemical>.+)_(?P<stat>90th|mean|median)_conc_by_SITE_ID\.csv$")

SCHEMA = pa.schema(
    [
        ("date", pa.date32()),
        ("site_id", pa.int64()),
        ("subbasin", pa.int64()),
        ("chemical", pa.dictionary(pa.int32(), pa.string())),
        ("stat", pa.dictionary(pa.int32(), pa.string())),
        ("conc_ng_L", pa.float64()),
    ]
)


def load_subbasin_key() -> pd.DataFrame:
    key = pd.read_csv(KEY_CSV, encoding="utf-8-sig")
    key["SITE_ID"] = key["SITE_ID"].astype("int64")
    key["subbasin"] = key["subbasin"].astype("int64")
    return key.set_index("SITE_ID")["subbasin"]


def melt_file(path: Path, chemical: str, stat: str, subbasin_by_site: pd.Series) -> pd.DataFrame:
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


def main() -> None:
    subbasin_by_site = load_subbasin_key()

    files = sorted(
        (path, m.group("chemical"), m.group("stat"))
        for path in DATA_DIR.glob("*.csv")
        if (m := FILENAME_RE.match(path.name))
    )
    print(f"Found {len(files)} chemical concentration CSVs to combine.")

    writer = pq.ParquetWriter(OUTPUT_PATH, SCHEMA, compression="zstd")
    total_rows = 0
    try:
        for i, (path, chemical, stat) in enumerate(files, start=1):
            long_df = melt_file(path, chemical, stat, subbasin_by_site)
            long_df["subbasin"] = long_df["subbasin"].astype("int64")

            table = pa.Table.from_pandas(long_df, schema=SCHEMA, preserve_index=False)
            writer.write_table(table)

            total_rows += len(long_df)
            print(f"[{i}/{len(files)}] {path.name}: {len(long_df):,} rows")
    finally:
        writer.close()

    print(f"Done. Wrote {total_rows:,} rows to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
