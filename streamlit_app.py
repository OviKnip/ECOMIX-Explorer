from __future__ import annotations

import io
import tempfile
from pathlib import Path

import folium
import geopandas as gpd
import pandas as pd
import plotly.graph_objects as go
import pyarrow.dataset as ds
import streamlit as st
from branca.colormap import LinearColormap, linear
from pyarrow.dataset import Expression
from streamlit.components.v1 import iframe
from streamlit_folium import st_folium


BASE_DIR = Path(__file__).resolve().parent
DATA_DIR = BASE_DIR / "data"
JULIA_DASH_URL = "http://127.0.0.1:8050"

SCENARIOS = ["Baseline", "SSP126", "SSP585"]
CLIMATE_VARIABLES = ["Precipitation", "Temperature"]
CLIMATE_PERIODS = [
    "2000-2022",
    "2020-2029",
    "2030-2039",
    "2040-2049",
    "2050-2059",
    "2060-2069",
    "2070-2080",
]
PROJECTION_PERIODS = [
    "2020-2029",
    "2030-2039",
    "2040-2049",
    "2050-2059",
    "2060-2069",
    "2070-2080",
]
PREDICTION_VARIABLES = {
    "Discharge": "discharge",
    "Soil Moisture": "Soil moisture",
    "Water Temperature": "water temperature",
    "Susp. Sediments": "Susp. Sediments",
    "Inorganic Nitrogen": "Inorganic Nitrogen",
}
PERCENTILE_CHOICES = {
    "Low (10th percentile)": "p10",
    "Average (50th percentile)": "p50",
    "High (90th percentile)": "p90",
}
DOWNLOAD_VARIABLES = {
    "Precipitation": "precip",
    "Temperature": "temp",
    "Discharge": "runoff",
    "Soil Moisture": "soil_moisture",
    "Water Temperature": "water_temperature",
    "Susp. Sediments": "susp_sediments",
    "Inorganic Nitrogen": "inorganic_nitrogen",
}
CLIMATE_DOWNLOAD_LOOKUP = {
    "precip": "Precipitation",
    "temp": "Temperature",
}
HYPE_DOWNLOAD_LOOKUP = {
    "runoff": "discharge",
    "soil_moisture": "Soil moisture",
    "water_temperature": "water temperature",
    "susp_sediments": "Susp. Sediments",
    "inorganic_nitrogen": "Inorganic Nitrogen",
}
LINE_DASHES = {
    "p10": "dot",
    "p50": "solid",
    "p90": "dash",
}
PERCENTILE_LABELS = {
    "p10": "Low (10th percentile)",
    "p50": "Average (50th percentile)",
    "p90": "High (90th percentile)",
}
SERIES_COLORS = [
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#6F4E7C",
    "#7F3C8D",
    "#11A579",
    "#3969AC",
]


st.set_page_config(page_title="ECOMIX Explorer", layout="wide")
st.markdown(
    """
    <style>
      .block-container {
        padding-top: 1.2rem;
        padding-bottom: 2rem;
      }
      .ecomix-card {
        background: linear-gradient(145deg, #f8fbff 0%, #edf4ef 100%);
        border: 1px solid rgba(26, 73, 46, 0.10);
        border-radius: 18px;
        padding: 1rem 1.1rem;
        min-height: 108px;
      }
      .ecomix-card-label {
        color: #46665a;
        font-size: 0.86rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }
      .ecomix-card-value {
        color: #123524;
        font-size: 1.6rem;
        font-weight: 700;
        line-height: 1.2;
        margin-top: 0.35rem;
      }
      .ecomix-section {
        margin-top: 0.4rem;
      }
    </style>
    """,
    unsafe_allow_html=True,
)


def safe_dataset(path: Path) -> ds.Dataset:
    return ds.dataset(
        path,
        format="parquet",
        partitioning="hive",
        exclude_invalid_files=True,
        ignore_prefixes=[".", "_"],
    )


@st.cache_resource(show_spinner=False)
def load_resources() -> dict[str, object]:
    catchment = gpd.read_file(DATA_DIR / "Wharfe_catchments_wgs.shp")
    subbasins = gpd.read_file(DATA_DIR / "Wharfe_subbasins_wgs.shp")
    subbasins["Id"] = pd.to_numeric(subbasins["Id"], errors="coerce").astype("Int64")

    climate = pd.read_csv(DATA_DIR / "subbasin_climate.csv")
    climate["subbasin"] = pd.to_numeric(climate["subbasin"].astype(str).str.replace("X", "", regex=False))

    landcover = pd.read_csv(DATA_DIR / "subbasin_lc.csv")
    landcover["subbasin"] = pd.to_numeric(landcover["subbasin"], errors="coerce")

    historical_ds = safe_dataset(DATA_DIR / "DB_Historical_Sim_Obs")
    historical = historical_ds.to_table().to_pandas()
    historical["date"] = pd.to_datetime(historical["date"])
    observed_subbasins = historical[["subbasin", "variable"]].drop_duplicates().sort_values(["subbasin", "variable"])

    extremes = pd.read_parquet(DATA_DIR / "Subbasin_Extremes.gz.parquet")
    extremes = extremes.loc[
        (extremes["prediction_percentile"] == 99.9)
        & (extremes["ssp"] == "SSP585")
        & (extremes["period"] == "2070-2080")
    ].copy()

    return {
        "catchment": catchment,
        "subbasins": subbasins,
        "climate": climate,
        "landcover": landcover,
        "historical": historical,
        "observed_subbasins": observed_subbasins,
        "proj_forcing": safe_dataset(DATA_DIR / "DB_Proj_Forcing"),
        "proj_year": safe_dataset(DATA_DIR / "DB_Proj_Year"),
        "proj_month": safe_dataset(DATA_DIR / "DB_Proj_Month"),
        "proj_percentiles": safe_dataset(DATA_DIR / "DB_Proj_Percentiles"),
        "extremes": extremes,
    }


RESOURCES = load_resources()


def combine_filters(*parts: Expression | None) -> Expression | None:
    expr: Expression | None = None
    for part in parts:
        if part is None:
            continue
        expr = part if expr is None else expr & part
    return expr


def in_filter(column: str, values: list[object] | tuple[object, ...] | set[object] | None) -> Expression | None:
    if not values:
        return None
    return ds.field(column).isin(list(values))


def query_dataset(dataset: ds.Dataset, filters: list[Expression | None], columns: list[str] | None = None) -> pd.DataFrame:
    expression = combine_filters(*filters)
    table = dataset.to_table(filter=expression, columns=columns)
    return table.to_pandas()


def selected_subbasin_climate(subbasin_id: int | None) -> pd.DataFrame:
    if subbasin_id is None:
        return pd.DataFrame()
    return RESOURCES["climate"].loc[RESOURCES["climate"]["subbasin"] == subbasin_id]


def selected_subbasin_landcover(subbasin_id: int | None) -> pd.DataFrame:
    if subbasin_id is None:
        return pd.DataFrame()
    return RESOURCES["landcover"].loc[RESOURCES["landcover"]["subbasin"] == subbasin_id]


def selected_subbasin_historical(subbasin_id: int | None) -> pd.DataFrame:
    if subbasin_id is None:
        return pd.DataFrame()
    return RESOURCES["historical"].loc[RESOURCES["historical"]["subbasin"] == subbasin_id].copy()


def observation_choices(subbasin_id: int | None) -> list[str]:
    if subbasin_id is None:
        return []
    observed = RESOURCES["observed_subbasins"]
    choices = observed.loc[observed["subbasin"] == subbasin_id, "variable"].dropna().tolist()
    return sorted(choices)


def get_series_color(name: str, known: dict[str, str]) -> str:
    if name not in known:
        known[name] = SERIES_COLORS[len(known) % len(SERIES_COLORS)]
    return known[name]


def rgba(hex_color: str, alpha: float) -> str:
    hex_color = hex_color.lstrip("#")
    red = int(hex_color[0:2], 16)
    green = int(hex_color[2:4], 16)
    blue = int(hex_color[4:6], 16)
    return f"rgba({red}, {green}, {blue}, {alpha})"


def add_band(
    fig: go.Figure,
    frame: pd.DataFrame,
    x_col: str,
    lower_col: str,
    center_col: str,
    upper_col: str,
    color: str,
    name: str,
    dash: str = "solid",
    showlegend: bool = True,
) -> None:
    fig.add_trace(
        go.Scatter(
            x=frame[x_col],
            y=frame[upper_col],
            mode="lines",
            line={"width": 0},
            hoverinfo="skip",
            showlegend=False,
        )
    )
    fig.add_trace(
        go.Scatter(
            x=frame[x_col],
            y=frame[lower_col],
            mode="lines",
            line={"width": 0},
            fill="tonexty",
            fillcolor=rgba(color, 0.18),
            hoverinfo="skip",
            showlegend=False,
        )
    )
    fig.add_trace(
        go.Scatter(
            x=frame[x_col],
            y=frame[center_col],
            mode="lines",
            name=name,
            line={"color": color, "width": 2, "dash": dash},
            showlegend=showlegend,
        )
    )


def make_base_figure(title: str, x_title: str, y_title: str, log_y: bool = False) -> go.Figure:
    fig = go.Figure()
    fig.update_layout(
        title=title,
        xaxis_title=x_title,
        yaxis_title=y_title,
        template="plotly_white",
        legend_title_text="",
        margin={"l": 20, "r": 20, "t": 60, "b": 20},
        height=430,
    )
    if log_y:
        fig.update_yaxes(type="log")
    return fig


def build_climate_plot(
    subbasin_id: int,
    climate_variable: str,
    climate_resolution: str,
    scenarios: list[str],
    periods: list[str],
) -> go.Figure | None:
    plot_periods = list(periods)
    if "Baseline" in scenarios and "2000-2022" not in plot_periods:
        plot_periods = ["2000-2022", *plot_periods]

    df_plot = query_dataset(
        RESOURCES["proj_forcing"],
        [
            ds.field("variable") == climate_variable,
            ds.field("subbasin") == int(subbasin_id),
            in_filter("ssp", scenarios),
            ds.field("time_aggregation") == climate_resolution,
        ],
    )
    if df_plot.empty:
        return None

    df_plot = df_plot.copy()
    color_map: dict[str, str] = {}

    if climate_resolution == "monthly":
        df_plot = df_plot.loc[df_plot["period"].isin(plot_periods)].copy()
        if df_plot.empty:
            return None
        df_plot["series"] = df_plot["ssp"] + " (" + df_plot["period"] + ")"
        x_col = "month"
        x_title = "Month"
    else:
        df_plot["series"] = df_plot["ssp"]
        x_col = "year"
        x_title = "Year"

    y_title = f"{climate_resolution.title()} {climate_variable} [{df_plot['unit'].dropna().iloc[0]}]"
    fig = make_base_figure(f"{climate_resolution.upper()} {climate_variable.upper()}", x_title, y_title)

    for series_name, frame in df_plot.groupby("series"):
        frame = frame.sort_values(x_col)
        add_band(
            fig,
            frame,
            x_col,
            "p10",
            "p50",
            "p90",
            get_series_color(series_name, color_map),
            series_name,
        )

    fig.update_xaxes(dtick=1 if climate_resolution == "monthly" else None)
    return fig


def build_observation_plot(subbasin_id: int, variable: str) -> go.Figure | None:
    df_plot = selected_subbasin_historical(subbasin_id)
    df_plot = df_plot.loc[df_plot["variable"] == variable].copy()
    if df_plot.empty:
        return None

    station_label = df_plot["station_label"].dropna().iloc[0]
    station_id = df_plot["id_station"].dropna().iloc[0]
    title = f"{station_label.upper()} (Station {station_id})"

    if variable == "discharge":
        fig = make_base_figure(title, "Year", "Discharge [m3/s]")
        observed = df_plot[["date", "obs_min", "obs", "obs_max"]].rename(
            columns={"obs_min": "low", "obs": "mid", "obs_max": "high"}
        )
        simulated = df_plot[["date", "sim_P10", "sim_P50", "sim_P90"]].rename(
            columns={"sim_P10": "low", "sim_P50": "mid", "sim_P90": "high"}
        )
        add_band(fig, observed.sort_values("date"), "date", "low", "mid", "high", "#0072B2", "Observation")
        add_band(fig, simulated.sort_values("date"), "date", "low", "mid", "high", "#D55E00", "Simulation")
        return fig

    if df_plot[["sim_P90", "obs"]].dropna(how="all").empty:
        return None

    unit = df_plot["unit"].dropna().iloc[0]
    fig = make_base_figure(title, "Year", f"{variable} [{unit}]")
    ordered = df_plot.sort_values("date")
    add_band(fig, ordered, "date", "sim_P10", "sim_P50", "sim_P90", "#D55E00", "Simulation")
    fig.add_trace(
        go.Scatter(
            x=ordered["date"],
            y=ordered["obs"],
            mode="markers",
            name="Observation",
            marker={"color": "#3a3a3a", "size": 7, "symbol": "x"},
        )
    )
    return fig


def build_yearly_projection_plot(
    subbasin_id: int,
    hype_variable: str,
    scenarios: list[str],
    percentiles: list[str],
    plot_type: str,
) -> go.Figure | None:
    df_plot = query_dataset(
        RESOURCES["proj_year"],
        [
            ds.field("subbasin") == int(subbasin_id),
            ds.field("hype_variable") == hype_variable,
            in_filter("ssp", ["Baseline", *scenarios]),
            in_filter("prediction_percentile", percentiles),
        ],
    )
    if df_plot.empty:
        return None

    color_map: dict[str, str] = {}

    if plot_type == "Absolute":
        df_plot = df_plot.loc[df_plot["ssp"].isin(scenarios)].copy()
        df_plot = df_plot.loc[~((df_plot["ssp"] == "Baseline") & (df_plot["year"] > 2020))]
        if df_plot.empty:
            return None
        unit = df_plot["unit"].dropna().iloc[0]
        fig = make_base_figure(hype_variable.upper(), "Year", f"{hype_variable} [{unit}]")
        for (scenario, percentile), frame in df_plot.groupby(["ssp", "prediction_percentile"]):
            frame = frame.sort_values("year")
            add_band(
                fig,
                frame,
                "year",
                "p10_ensemble",
                "p50_ensemble",
                "p90_ensemble",
                get_series_color(scenario, color_map),
                f"{scenario} | {PERCENTILE_LABELS.get(percentile, percentile)}",
                dash=LINE_DASHES.get(percentile, "solid"),
            )
        return fig

    baseline = (
        df_plot.loc[df_plot["ssp"] == "Baseline"]
        .groupby(["subbasin", "prediction_percentile"], as_index=False)[["p10_ensemble", "p50_ensemble", "p90_ensemble"]]
        .mean()
        .rename(
            columns={
                "p10_ensemble": "p10_base",
                "p50_ensemble": "p50_base",
                "p90_ensemble": "p90_base",
            }
        )
    )
    projected = df_plot.loc[df_plot["ssp"] != "Baseline"].copy()
    if projected.empty or baseline.empty:
        return None
    projected = projected.merge(baseline, on=["subbasin", "prediction_percentile"], how="left")
    projected["p10_anomaly"] = projected["p10_ensemble"] - projected["p10_base"]
    projected["p50_anomaly"] = projected["p50_ensemble"] - projected["p50_base"]
    projected["p90_anomaly"] = projected["p90_ensemble"] - projected["p90_base"]
    projected["low_uci"] = projected[["p10_anomaly", "p50_anomaly", "p90_anomaly"]].min(axis=1)
    projected["high_uci"] = projected[["p10_anomaly", "p50_anomaly", "p90_anomaly"]].max(axis=1)

    fig = make_base_figure(
        f"{hype_variable.upper()} anomalies (change to 2000-2020 baseline)",
        "Year",
        f"Change to baseline: {hype_variable} [{projected['unit'].dropna().iloc[0]}]",
    )
    for (scenario, percentile), frame in projected.groupby(["ssp", "prediction_percentile"]):
        frame = frame.sort_values("year")
        add_band(
            fig,
            frame,
            "year",
            "low_uci",
            "p50_anomaly",
            "high_uci",
            get_series_color(scenario, color_map),
            f"{scenario} | {PERCENTILE_LABELS.get(percentile, percentile)}",
            dash=LINE_DASHES.get(percentile, "solid"),
        )
    return fig


def build_monthly_projection_plot(
    subbasin_id: int,
    hype_variable: str,
    scenarios: list[str],
    periods: list[str],
    percentiles: list[str],
    plot_type: str,
) -> go.Figure | None:
    plot_periods = list(periods)
    if "Baseline" in scenarios and "2000-2022" not in plot_periods:
        plot_periods = ["2000-2022", *plot_periods]

    df_plot = query_dataset(
        RESOURCES["proj_month"],
        [
            ds.field("subbasin") == int(subbasin_id),
            ds.field("hype_variable") == hype_variable,
            in_filter("ssp", ["Baseline", *scenarios]),
            in_filter("prediction_percentile", percentiles),
            in_filter("period", plot_periods),
        ],
    )
    if df_plot.empty:
        return None

    color_map: dict[str, str] = {}

    if plot_type == "Absolute":
        df_plot = df_plot.loc[df_plot["ssp"].isin(scenarios)].copy()
        if df_plot.empty:
            return None
        df_plot["series"] = df_plot["ssp"] + " (" + df_plot["period"] + ")"
        unit = df_plot["unit"].dropna().iloc[0]
        fig = make_base_figure(hype_variable.upper(), "Month", f"{hype_variable} [{unit}]")
        for (series_name, percentile), frame in df_plot.groupby(["series", "prediction_percentile"]):
            frame = frame.sort_values("month")
            add_band(
                fig,
                frame,
                "month",
                "p10_ensemble",
                "p50_ensemble",
                "p90_ensemble",
                get_series_color(series_name, color_map),
                f"{series_name} | {PERCENTILE_LABELS.get(percentile, percentile)}",
                dash=LINE_DASHES.get(percentile, "solid"),
            )
        fig.update_xaxes(dtick=1)
        return fig

    baseline = (
        df_plot.loc[df_plot["ssp"] == "Baseline"]
        .groupby(["subbasin", "month", "prediction_percentile"], as_index=False)[["p10_ensemble", "p50_ensemble", "p90_ensemble"]]
        .mean()
        .rename(
            columns={
                "p10_ensemble": "p10_base",
                "p50_ensemble": "p50_base",
                "p90_ensemble": "p90_base",
            }
        )
    )
    projected = df_plot.loc[df_plot["ssp"] != "Baseline"].copy()
    if projected.empty or baseline.empty:
        return None
    projected = projected.merge(baseline, on=["subbasin", "month", "prediction_percentile"], how="left")
    projected["p10_anomaly"] = projected["p10_ensemble"] - projected["p10_base"]
    projected["p50_anomaly"] = projected["p50_ensemble"] - projected["p50_base"]
    projected["p90_anomaly"] = projected["p90_ensemble"] - projected["p90_base"]
    projected["low_uci"] = projected[["p10_anomaly", "p50_anomaly", "p90_anomaly"]].min(axis=1)
    projected["high_uci"] = projected[["p10_anomaly", "p50_anomaly", "p90_anomaly"]].max(axis=1)

    fig = make_base_figure(
        f"{hype_variable.upper()} anomalies (change to 2000-2020 baseline)",
        "Month",
        f"Change to baseline: {hype_variable} [{projected['unit'].dropna().iloc[0]}]",
    )
    for (scenario, percentile), frame in projected.groupby(["ssp", "prediction_percentile"]):
        frame = frame.sort_values("month")
        add_band(
            fig,
            frame,
            "month",
            "low_uci",
            "p50_anomaly",
            "high_uci",
            get_series_color(scenario, color_map),
            f"{scenario} | {PERCENTILE_LABELS.get(percentile, percentile)}",
            dash=LINE_DASHES.get(percentile, "solid"),
        )
    fig.update_xaxes(dtick=1)
    return fig


def build_cfc_plot(subbasin_id: int, hype_variable: str, scenarios: list[str], periods: list[str]) -> go.Figure | None:
    plot_periods = list(periods)
    if "Baseline" in scenarios and "2000-2022" not in plot_periods:
        plot_periods = ["2000-2022", *plot_periods]

    df_plot = query_dataset(
        RESOURCES["proj_percentiles"],
        [
            ds.field("subbasin") == int(subbasin_id),
            ds.field("hype_variable") == hype_variable,
            in_filter("ssp", scenarios),
            in_filter("period", plot_periods),
        ],
    )
    if df_plot.empty:
        return None

    df_plot = df_plot.loc[df_plot["p50_ensemble"] > 0].copy()
    if df_plot.empty:
        return None
    df_plot["series"] = df_plot["ssp"] + " (" + df_plot["period"] + ")"
    unit = df_plot["unit"].dropna().iloc[0]
    fig = make_base_figure(
        f"Cumulative Frequency Curve for {hype_variable}".upper(),
        "Cumulative Frequency [%]",
        f"{hype_variable} [{unit}]",
        log_y=True,
    )
    color_map: dict[str, str] = {}
    for series_name, frame in df_plot.groupby("series"):
        frame = frame.sort_values("prediction_percentile")
        add_band(
            fig,
            frame,
            "prediction_percentile",
            "p10_ensemble",
            "p50_ensemble",
            "p90_ensemble",
            get_series_color(series_name, color_map),
            series_name,
        )
    fig.update_xaxes(tickvals=[0, 25, 50, 75, 100], ticktext=["0", "25", "50 (Median)", "75", "100"])
    return fig


@st.cache_data(show_spinner=False)
def build_tabular_download(download_variable: str) -> pd.DataFrame:
    if download_variable in CLIMATE_DOWNLOAD_LOOKUP:
        label = CLIMATE_DOWNLOAD_LOOKUP[download_variable]
        df_download = query_dataset(
            RESOURCES["proj_forcing"],
            [
                ds.field("variable") == label,
                ds.field("time_aggregation") == "monthly",
            ],
            columns=["subbasin", "ssp", "period", "month", "variable", "p50", "unit"],
        ).rename(columns={"ssp": "scenario", "p50": "value"})
    else:
        label = HYPE_DOWNLOAD_LOOKUP[download_variable]
        df_download = query_dataset(
            RESOURCES["proj_month"],
            [
                ds.field("hype_variable") == label,
                ds.field("prediction_percentile") == "p50",
            ],
            columns=["subbasin", "ssp", "period", "month", "hype_variable", "prediction_percentile", "p50_ensemble", "unit"],
        ).rename(columns={"ssp": "scenario", "p50_ensemble": "value"})

    df_download = df_download.assign(value=lambda frame: frame["value"].round(3))
    return df_download[["subbasin", "scenario", "period", "month", "value", "unit"]]


def spatial_download_frame(download_variable: str, spatial_layer: str) -> gpd.GeoDataFrame:
    tabular = build_tabular_download(download_variable)

    if spatial_layer == "catchment":
        catchment = RESOURCES["catchment"].copy()
        if tabular.empty:
            return catchment.assign(
                spatial_layer=spatial_layer,
                variable=download_variable,
                n_records=0,
                value_mean=pd.NA,
                value_min=pd.NA,
                value_max=pd.NA,
                unit=pd.NA,
            )

        n_records = int(tabular["value"].size)
        value_mean = round(float(tabular["value"].mean()), 3)
        value_min = round(float(tabular["value"].min()), 3)
        value_max = round(float(tabular["value"].max()), 3)
        unit = tabular["unit"].dropna().iloc[0] if not tabular["unit"].dropna().empty else pd.NA
        return catchment.assign(
            spatial_layer=spatial_layer,
            variable=download_variable,
            n_records=n_records,
            value_mean=value_mean,
            value_min=value_min,
            value_max=value_max,
            unit=unit,
        )

    summary_by_subbasin = (
        tabular.assign(subbasin=lambda frame: pd.to_numeric(frame["subbasin"], errors="coerce"))
        .groupby("subbasin", as_index=False)
        .agg(
            n_records=("value", "size"),
            value_mean=("value", "mean"),
            value_min=("value", "min"),
            value_max=("value", "max"),
            unit=("unit", "first"),
        )
    )
    if not summary_by_subbasin.empty:
        for column in ["value_mean", "value_min", "value_max"]:
            summary_by_subbasin[column] = summary_by_subbasin[column].round(3)
    subbasins = RESOURCES["subbasins"].copy()
    return subbasins.merge(summary_by_subbasin, left_on="Id", right_on="subbasin", how="left").assign(
        spatial_layer=spatial_layer,
        variable=download_variable,
    )


def dataframe_download_bytes(frame: pd.DataFrame, download_format: str) -> bytes:
    if download_format == "csv":
        return frame.to_csv(index=False).encode("utf-8")
    if download_format == "xlsx":
        buffer = io.BytesIO()
        with pd.ExcelWriter(buffer, engine="openpyxl") as writer:
            frame.to_excel(writer, index=False, sheet_name="ecomix")
        return buffer.getvalue()
    return frame.to_parquet(index=False)


def geodataframe_download_bytes(frame: gpd.GeoDataFrame, download_format: str) -> bytes:
    export_frame = frame.to_crs(27700)

    if download_format == "shp":
        with tempfile.TemporaryDirectory() as tmpdir:
            shp_path = Path(tmpdir) / "ecomix_spatial.shp"
            export_frame.to_file(shp_path, driver="ESRI Shapefile")
            buffer = io.BytesIO()
            import zipfile

            with zipfile.ZipFile(buffer, mode="w") as archive:
                for item in Path(tmpdir).iterdir():
                    archive.write(item, arcname=item.name)
            return buffer.getvalue()

    suffix = ".gpkg" if download_format == "gpkg" else ".parquet"
    with tempfile.NamedTemporaryFile(suffix=suffix) as tmp:
        if download_format == "gpkg":
            export_frame.to_file(tmp.name, driver="GPKG")
        else:
            export_frame.to_parquet(tmp.name, index=False)
        return Path(tmp.name).read_bytes()


def build_download_filename(
    download_variable: str,
    data_type: str,
    download_format: str,
    spatial_layer: str,
    subbasin_id: int | None,
) -> str:
    extension = {
        "csv": "csv",
        "xlsx": "xlsx",
        "parquet": "parquet",
        "shp": "zip",
        "gpkg": "gpkg",
        "geoparquet": "parquet",
    }[download_format]
    prefix = "ecomix_spatial" if data_type == "spatial" else "ecomix_tabular"
    layer_suffix = f"_{spatial_layer}" if data_type == "spatial" else ""
    if data_type == "spatial":
        id_suffix = "all_subbasins" if spatial_layer == "subbasins" else "catchment"
    else:
        id_suffix = f"subbasin_{subbasin_id}"
    return f"{prefix}{layer_suffix}_{id_suffix}_{download_variable}.{extension}"


def metric_card(label: str, value: str) -> None:
    st.markdown(
        f"""
        <div class="ecomix-card">
          <div class="ecomix-card-label">{label}</div>
          <div class="ecomix-card-value">{value}</div>
        </div>
        """,
        unsafe_allow_html=True,
    )


def subbasin_summary(subbasin_id: int | None) -> dict[str, str]:
    climate = selected_subbasin_climate(subbasin_id)
    landcover = selected_subbasin_landcover(subbasin_id)
    if climate.empty or landcover.empty:
        return {
            "Selected Subcatchment": "Not selected",
            "Upstream Area": "-",
            "Average precipitation": "-",
            "Annual Temperature": "-",
        }

    upstream = landcover.loc[landcover["variable"] == "Upstream area", "value"]
    upstream_area = "-"
    if not upstream.empty:
        upstream_area = f"{upstream.iloc[0] / 1_000_000:.2f} km2"

    return {
        "Selected Subcatchment": str(int(climate["subbasin"].iloc[0])),
        "Upstream Area": upstream_area,
        "Average precipitation": f"{climate['precip'].iloc[0]:.0f} mm",
        "Annual Temperature": f"{climate['maat'].iloc[0]:.1f} C",
    }


def build_subbasin_map(selected_id: int | None) -> folium.Map:
    subbasins = RESOURCES["subbasins"]
    center_lat = float(subbasins.geometry.centroid.y.mean())
    center_lon = float(subbasins.geometry.centroid.x.mean())
    feature_frame = subbasins.copy()

    fmap = folium.Map(location=[center_lat, center_lon], zoom_start=9, tiles="CartoDB positron")

    def style_function(feature: dict[str, object]) -> dict[str, object]:
        sub_id = int(feature["properties"]["Id"])
        is_selected = selected_id is not None and sub_id == int(selected_id)
        return {
            "fillColor": "#0f8b8d" if is_selected else "#ffffff",
            "fillOpacity": 0.45 if is_selected else 0.05,
            "color": "#123524" if is_selected else "#303030",
            "weight": 3 if is_selected else 1,
        }

    folium.GeoJson(
        feature_frame,
        name="Subbasins",
        style_function=style_function,
        highlight_function=lambda _: {"weight": 3, "fillOpacity": 0.25},
        tooltip=folium.GeoJsonTooltip(fields=["Id", "Area"], aliases=["Subbasin", "Area"]),
    ).add_to(fmap)

    bounds = feature_frame.total_bounds
    fmap.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])
    return fmap


def build_prediction_map(hype_variable: str) -> folium.Map:
    subbasins = RESOURCES["subbasins"].copy()
    extremes = RESOURCES["extremes"]
    merged = subbasins.merge(extremes.loc[extremes["hype_variable"] == hype_variable], left_on="Id", right_on="subbasin", how="left")
    merged = merged.loc[merged["p50_ensemble"].notna()].copy()

    if merged.empty:
        fmap = folium.Map(location=[53.75, -1.16], zoom_start=8, tiles="CartoDB positron")
        bounds = subbasins.total_bounds
        fmap.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])
        return fmap

    colormap_factory = getattr(linear, "Viridis_09", None) or getattr(linear, "viridis", None)
    if colormap_factory is None:
        scale = LinearColormap(
            colors=["#440154", "#31688e", "#35b779", "#fde725"],
            vmin=float(merged["p50_ensemble"].min()),
            vmax=float(merged["p50_ensemble"].max()),
        )
    else:
        scale = colormap_factory.scale(float(merged["p50_ensemble"].min()), float(merged["p50_ensemble"].max()))
    scale.caption = "P99.9"

    fmap = folium.Map(location=[53.75, -1.16], zoom_start=8, tiles="CartoDB positron")
    folium.GeoJson(
        merged,
        style_function=lambda feature: {
            "fillColor": scale(feature["properties"]["p50_ensemble"]),
            "fillOpacity": 0.75,
            "color": "#262626",
            "weight": 1,
        },
        tooltip=folium.GeoJsonTooltip(
            fields=["Id", "p50_ensemble", "unit"],
            aliases=["Subbasin", "P99.9 value", "Unit"],
            localize=True,
        ),
    ).add_to(fmap)
    scale.add_to(fmap)
    bounds = merged.total_bounds
    fmap.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])
    return fmap


def require_subbasin(subbasin_id: int | None) -> bool:
    if subbasin_id is not None:
        return True
    st.info("Select a subbasin from the sidebar or by clicking the map.")
    return False


def sync_selected_subbasin_from_widget() -> None:
    st.session_state.selected_subbasin = st.session_state.selected_subbasin_widget


def main() -> None:
    st.title("ECOMIX Explorer")
    st.caption("Streamlit port of the original Shiny dashboard for exploring ECOMIX climate and water quality outputs.")

    subbasin_ids = sorted(RESOURCES["subbasins"]["Id"].dropna().astype(int).tolist())
    if "selected_subbasin" not in st.session_state:
        st.session_state.selected_subbasin = None
    if "selected_subbasin_widget" not in st.session_state:
        st.session_state.selected_subbasin_widget = st.session_state.selected_subbasin
    elif st.session_state.selected_subbasin_widget != st.session_state.selected_subbasin:
        st.session_state.selected_subbasin_widget = st.session_state.selected_subbasin
    if "scenarios" not in st.session_state:
        st.session_state.scenarios = ["Baseline"]
    if "spatial_map_nonce" not in st.session_state:
        st.session_state.spatial_map_nonce = 0

    with st.sidebar:
        st.header("Controls")
        st.selectbox(
            "Selected subbasin",
            options=[None, *subbasin_ids],
            format_func=lambda value: "Choose a subbasin" if value is None else str(value),
            key="selected_subbasin_widget",
            on_change=sync_selected_subbasin_from_widget,
        )
        st.multiselect("Scenarios", options=SCENARIOS, default=st.session_state.scenarios, key="scenarios")
        if not st.session_state.scenarios:
            st.warning("Select at least one scenario to populate the plots.")

    selected_subbasin = st.session_state.selected_subbasin
    selected_scenarios = st.session_state.scenarios

    tab_map, tab_explorer, tab_spatial, tab_download, tab_foodweb = st.tabs(
        ["Map", "Data Explorer", "Spatial Datasets", "Data Downloader", "Food Web Dynamics"]
    )

    with tab_map:
        st.markdown("<div class='ecomix-section'></div>", unsafe_allow_html=True)
        map_col, info_col = st.columns([1.8, 1.0])
        with map_col:
            st.caption("Click a polygon to update the selected subbasin.")
            map_result = st_folium(
                build_subbasin_map(selected_subbasin),
                height=640,
                width=None,
                returned_objects=["last_active_drawing"],
                key="subbasin-map",
            )
            feature = map_result.get("last_active_drawing") if map_result else None
            properties = feature.get("properties", {}) if isinstance(feature, dict) else {}
            clicked_subbasin = properties.get("Id")
            if clicked_subbasin is not None:
                clicked_subbasin = int(clicked_subbasin)
                if selected_subbasin != clicked_subbasin:
                    st.session_state.selected_subbasin = clicked_subbasin
                    st.rerun()

            st.caption("Data compiled by Durham University (2026).")

        with info_col:
            st.subheader("Subbasin information")
            if selected_subbasin is None:
                st.info("Please select a subbasin by clicking on the map.")
            else:
                summary = subbasin_summary(selected_subbasin)
                for label, value in summary.items():
                    metric_card(label, value)

    with tab_explorer:
        summary = subbasin_summary(selected_subbasin)
        card_columns = st.columns(4)
        for column, (label, value) in zip(card_columns, summary.items(), strict=False):
            with column:
                metric_card(label, value)

        st.markdown("<div class='ecomix-section'></div>", unsafe_allow_html=True)
        climate_column, observation_column = st.columns(2)

        with climate_column:
            st.subheader("Climate")
            climate_variable = st.selectbox("Climate variable", CLIMATE_VARIABLES, key="climate-variable")
            climate_resolution_label = st.selectbox("Resolution", ["Monthly", "Yearly"], key="climate-resolution")
            climate_resolution = "monthly" if climate_resolution_label == "Monthly" else "annual"
            climate_period = st.multiselect(
                "Climate period(s)",
                options=CLIMATE_PERIODS,
                default=["2000-2022"],
                key="climate-period",
            )
            if require_subbasin(selected_subbasin) and selected_scenarios:
                climate_figure = build_climate_plot(
                    selected_subbasin,
                    climate_variable,
                    climate_resolution,
                    selected_scenarios,
                    climate_period,
                )
                if climate_figure is None:
                    st.warning("No climate records matched the current selection.")
                else:
                    st.plotly_chart(climate_figure, use_container_width=True)

        with observation_column:
            st.subheader("Simulations vs Observations")
            available_observations = observation_choices(selected_subbasin)
            observation_variable = st.selectbox(
                "Observed variable",
                options=available_observations if available_observations else ["No observations available"],
                key="observation-variable",
                disabled=not available_observations,
            )
            if require_subbasin(selected_subbasin):
                if not available_observations:
                    st.info("No observations are available for the selected subbasin.")
                else:
                    observation_figure = build_observation_plot(selected_subbasin, observation_variable)
                    if observation_figure is None:
                        st.warning("No simulation or observation records matched the current selection.")
                    else:
                        st.plotly_chart(observation_figure, use_container_width=True)

        st.subheader("Projections")
        projection_tabs = st.tabs(["Yearly", "Monthly", "Distributions"])
        prediction_variable_label = st.selectbox("Projection variable", list(PREDICTION_VARIABLES), key="prediction-variable")
        prediction_variable = PREDICTION_VARIABLES[prediction_variable_label]

        with projection_tabs[0]:
            yearly_left, yearly_right = st.columns([1, 3])
            with yearly_left:
                yearly_percentiles = st.multiselect(
                    "Prediction percentile(s)",
                    options=list(PERCENTILE_CHOICES),
                    default=["Average (50th percentile)"],
                    key="yearly-percentiles",
                )
                yearly_plot_type = st.selectbox("Plot type", ["Absolute", "Relative"], key="yearly-plot-type")
            with yearly_right:
                if require_subbasin(selected_subbasin) and selected_scenarios:
                    yearly_figure = build_yearly_projection_plot(
                        selected_subbasin,
                        prediction_variable,
                        selected_scenarios,
                        [PERCENTILE_CHOICES[item] for item in yearly_percentiles],
                        yearly_plot_type,
                    )
                    if yearly_figure is None:
                        st.warning("No yearly projection records matched the current selection.")
                    else:
                        st.plotly_chart(yearly_figure, use_container_width=True)

        with projection_tabs[1]:
            monthly_left, monthly_right = st.columns([1, 3])
            with monthly_left:
                monthly_periods = st.multiselect(
                    "Projection period(s)",
                    options=PROJECTION_PERIODS,
                    default=["2070-2080"],
                    key="monthly-periods",
                )
                monthly_percentiles = st.multiselect(
                    "Prediction percentile(s)",
                    options=list(PERCENTILE_CHOICES),
                    default=["Average (50th percentile)"],
                    key="monthly-percentiles",
                )
                monthly_plot_type = st.selectbox("Plot type", ["Absolute", "Relative"], key="monthly-plot-type")
            with monthly_right:
                if require_subbasin(selected_subbasin) and selected_scenarios:
                    monthly_figure = build_monthly_projection_plot(
                        selected_subbasin,
                        prediction_variable,
                        selected_scenarios,
                        monthly_periods,
                        [PERCENTILE_CHOICES[item] for item in monthly_percentiles],
                        monthly_plot_type,
                    )
                    if monthly_figure is None:
                        st.warning("No monthly projection records matched the current selection.")
                    else:
                        st.plotly_chart(monthly_figure, use_container_width=True)

        with projection_tabs[2]:
            distribution_left, distribution_right = st.columns([1, 3])
            with distribution_left:
                distribution_periods = st.multiselect(
                    "Distribution period(s)",
                    options=PROJECTION_PERIODS,
                    default=["2070-2080"],
                    key="distribution-periods",
                )
            with distribution_right:
                if require_subbasin(selected_subbasin) and selected_scenarios:
                    cfc_figure = build_cfc_plot(
                        selected_subbasin,
                        prediction_variable,
                        selected_scenarios,
                        distribution_periods,
                    )
                    if cfc_figure is None:
                        st.warning("No distribution records matched the current selection.")
                    else:
                        st.plotly_chart(cfc_figure, use_container_width=True)

    with tab_spatial:
        spatial_controls_col, spatial_button_col = st.columns([4, 1])
        with spatial_controls_col:
            spatial_variable_label = st.selectbox("Spatial variable", list(PREDICTION_VARIABLES), key="spatial-variable")
        with spatial_button_col:
            st.write("")
            st.write("")
            if st.button("Reset zoom", key="spatial-reset-zoom"):
                st.session_state.spatial_map_nonce += 1
                st.rerun()

        spatial_variable = PREDICTION_VARIABLES[spatial_variable_label]
        st_folium(
            build_prediction_map(spatial_variable),
            height=700,
            width=None,
            key=f"prediction-map-{spatial_variable}-{st.session_state.spatial_map_nonce}",
        )

    with tab_download:
        control_column, table_column = st.columns([1, 2.3])
        with control_column:
            download_variable_label = st.selectbox("Variable", list(DOWNLOAD_VARIABLES), key="download-variable")
            download_variable = DOWNLOAD_VARIABLES[download_variable_label]
            download_data_type = st.selectbox("Data type", ["tabular", "spatial"], key="download-data-type")
            spatial_layer = st.selectbox(
                "Spatial layer",
                ["subbasins", "catchment"],
                key="download-spatial-layer",
                disabled=download_data_type != "spatial",
            )
            if download_data_type == "tabular":
                download_format = st.selectbox("Download format", ["csv", "xlsx", "parquet"], key="download-format-tabular")
            else:
                download_format = st.selectbox(
                    "Download format",
                    ["shp", "geoparquet", "gpkg"],
                    format_func=lambda value: {"shp": "Shapefile (.zip)", "geoparquet": "GeoParquet", "gpkg": "GPKG"}[value],
                    key="download-format-spatial",
                )

            if download_data_type == "tabular":
                preview_frame = build_tabular_download(download_variable)
                if selected_subbasin is not None:
                    preview_frame = preview_frame.loc[preview_frame["subbasin"] == selected_subbasin].copy()
                download_disabled = selected_subbasin is None or preview_frame.empty
                download_help = None
                if selected_subbasin is None:
                    download_help = "Select a subbasin to enable tabular downloads."
                elif preview_frame.empty:
                    download_help = "No tabular data is available for this selection."
                payload = dataframe_download_bytes(preview_frame, download_format) if not download_disabled else b""
            else:
                preview_geo = spatial_download_frame(download_variable, spatial_layer)
                preview_frame = preview_geo.drop(columns="geometry")
                if {"value_mean", "value_min", "value_max"}.issubset(preview_frame.columns):
                    preview_frame = preview_frame.loc[
                        ~(preview_frame["value_mean"].isna() & preview_frame["value_min"].isna() & preview_frame["value_max"].isna())
                    ].copy()
                download_disabled = preview_geo.empty
                download_help = "No spatial data is available for this selection." if download_disabled else None
                spatial_format = "parquet" if download_format == "geoparquet" else download_format
                payload = geodataframe_download_bytes(preview_geo, spatial_format) if not download_disabled else b""

            st.download_button(
                label="Download data",
                data=payload,
                file_name=build_download_filename(
                    download_variable,
                    download_data_type,
                    download_format,
                    spatial_layer,
                    selected_subbasin,
                ),
                disabled=download_disabled,
                help=download_help,
            )

        with table_column:
            st.subheader("Preview")
            if download_data_type == "tabular" and selected_subbasin is None:
                st.info("Please select a subbasin by clicking on the map or using the sidebar.")
            elif preview_frame.empty:
                st.warning("No download rows matched the current selection.")
            else:
                st.dataframe(preview_frame, use_container_width=True, hide_index=True)

    with tab_foodweb:
        st.subheader("Food Web Dynamics")
        st.caption("This panel embeds the Julia Dash application expected at 127.0.0.1:8050.")
        iframe(JULIA_DASH_URL, height=720, width=None, scrolling=True)


if __name__ == "__main__":
    main()