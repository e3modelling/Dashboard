from pathlib import Path
from io import BytesIO
import os

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from iamc_loader import load_iamc_dashboard, non_overlapping_rows, overview_metrics


BASE_DIR = Path(__file__).resolve().parent
MIF_FILE = BASE_DIR / "reporting.mif"
DEMO_FILE = BASE_DIR / "data" / "demo.mif"

COLORS = {
    "shell": "#0B1118",
    "sidebar": "#101923",
    "page": "#E9EDF3",
    "surface": "#F7F9FC",
    "border": "#D8E0EA",
    "text": "#182230",
    "muted": "#617083",
    "accent": "#1F9A8A",
    "danger": "#C2414B",
    "energy": "#2364AA",
    "renew": "#2F855A",
    "capacity": "#4C6FFF",
}

FUEL_COLS = {
    "Coal": "#3d3d3d", "Lignite": "#555555",
    "Oil": "#6B4226", "Crude Oil and Feedstocks": "#7a5230",
    "Gas": "#A8DADC", "Natural Gas": "#90cfd2",
    "Nuclear": "#D5A940", "Wind": "#457B9D", "Solar": "#E6B422",
    "Hydro": "#1D4E89", "Biofuels": "#588157", "Biomass and Waste": "#6aad65",
    "Geothermal and other renewable sources": "#2A9D8F",
    "Hydrogen": "#48CAE4", "Electricity": "#F6C85F", "Heat": "#D45087",
    "Other fuels": "#8A94A6", "Other Gases": "#9AA3B2",
    "Other Liquids": "#AAB2BF", "Renewables": "#43aa8b", "Total": "#B8C0CC",
}

GAS_COLS = {
    "CO2": "#C2414B", "CH4": "#E07A5F", "N2O": "#2A9D8F",
    "Kyoto Gases": "#7C3AED", "F-gases": "#457B9D",
    "HFC": "#D5A940", "SF6": "#48CAE4", "PFC": "#588157",
}


st.set_page_config(
    page_title="OPEN-PROM Results Explorer",
    page_icon="",
    layout="wide",
    initial_sidebar_state="expanded",
)


st.markdown(
    """
    <style>
    :root {
      --dash-shell: #0B1118;
      --dash-sidebar: #101923;
      --dash-page: #E9EDF3;
      --dash-surface: #F7F9FC;
      --dash-border: #D8E0EA;
      --dash-text: #182230;
      --dash-muted: #617083;
      --dash-accent: #1F9A8A;
    }
    .stApp {
      background: linear-gradient(180deg, #F1F5F9 0%, var(--dash-page) 38%, #E5EBF2 100%);
      color: var(--dash-text);
    }
    section[data-testid="stSidebar"] {
      background: var(--dash-sidebar);
      border-right: 1px solid rgba(255,255,255,.08);
    }
    section[data-testid="stSidebar"] * { color: #E8EEF5; }
    section[data-testid="stSidebar"] .stSelectbox [role="group"],
    section[data-testid="stSidebar"] .stSelectbox input,
    section[data-testid="stSidebar"] .stSelectbox button {
      background-color: #182634 !important;
      color: #E8EEF5 !important;
      border-color: #334556 !important;
    }
    h1, h2, h3 { color: var(--dash-text); letter-spacing: 0; }
    h1 { font-size: 1.55rem !important; margin-bottom: .1rem; }
    h2 { font-size: 1.1rem !important; margin-top: .7rem; }
    .subtle { color: var(--dash-muted); font-size: .9rem; }
    .metric-card {
      background: linear-gradient(180deg, #FFFFFF 0%, #F6F8FB 100%);
      border: 1px solid var(--dash-border);
      border-top: 3px solid var(--accent);
      border-radius: 8px;
      box-shadow: 0 12px 26px rgba(22,34,51,.08);
      padding: 16px 18px 14px;
      min-height: 126px;
    }
    .metric-title {
      color: var(--dash-muted);
      font-size: .72rem;
      font-weight: 800;
      letter-spacing: .08em;
      text-transform: uppercase;
    }
    .metric-value {
      color: var(--dash-text);
      font-size: 2rem;
      font-weight: 800;
      line-height: 1.1;
      margin-top: .55rem;
    }
    .metric-unit {
      color: var(--dash-muted);
      font-size: .82rem;
      font-weight: 700;
      margin-left: .35rem;
    }
    .metric-footer {
      border-top: 1px solid #E5EBF2;
      color: var(--dash-muted);
      font-size: .76rem;
      margin-top: .8rem;
      padding-top: .55rem;
    }
    div[data-testid="stMetric"] {
      background: #FFFFFF;
      border: 1px solid var(--dash-border);
      border-radius: 8px;
      padding: 1rem;
    }
    div[data-testid="stDataFrame"] {
      border: 1px solid var(--dash-border);
      border-radius: 8px;
      overflow: hidden;
    }
    .block-container { padding-top: 1.4rem; padding-bottom: 2rem; }
    </style>
    """,
    unsafe_allow_html=True,
)


@st.cache_data(show_spinner="Reading IAMC/MIF data...")
def get_data(contents):
    """Cache selections with shares/aggregates excluded (selection revision 2)."""
    return load_iamc_dashboard(BytesIO(contents))


def plotly_layout(fig, height=430, legend=True):
    fig.update_layout(
        height=height,
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=dict(family="Inter, Segoe UI, Arial, sans-serif", color=COLORS["text"], size=12),
        margin=dict(l=60, r=24, t=50, b=100),
        title=dict(x=0.01, y=0.98, yanchor="top"),
        legend=dict(
            orientation="h",
            title_text="",
            yanchor="top",
            y=-0.23,
            xanchor="left",
            x=0,
            bgcolor="rgba(247,249,252,.92)",
            bordercolor=COLORS["border"],
            borderwidth=1,
        ) if legend else None,
    )
    fig.update_xaxes(gridcolor="#DCE4ED", linecolor=COLORS["border"], zerolinecolor="#DCE4ED")
    fig.update_yaxes(gridcolor="#DCE4ED", linecolor=COLORS["border"], zerolinecolor="#DCE4ED")
    return fig


def empty_msg(text):
    st.info(text, icon="ℹ")


def year_range(label, years, default=(2020, 2060)):
    lo, hi = int(min(years)), int(max(years))
    d0 = max(lo, default[0])
    d1 = min(hi, default[1])
    return st.slider(label, lo, hi, (d0, d1), step=1)


def select_year(label, years, target=2030):
    years = sorted(int(y) for y in years)
    default = min(years, key=lambda y: abs(y - target))
    return st.selectbox(label, years, index=years.index(default))


def metric_card(title, value, unit, accent, footer):
    st.markdown(
        f"""
        <div class="metric-card" style="--accent:{accent};">
          <div class="metric-title">{title}</div>
          <div class="metric-value">{value}<span class="metric-unit">{unit}</span></div>
          <div class="metric-footer">{footer}</div>
        </div>
        """,
        unsafe_allow_html=True,
    )


def show_metrics(data, scenario, regions, year):
    specs = [
        ("co2", "CO2 Emissions", "danger", "End-year sum", 1),
        ("primary", "Primary Energy", "energy", "End-year sum", 1),
        ("renewable", "Renewable Elec.", "renew", "Share of electricity", 1),
        ("capacity", "Power Capacity", "capacity", "End-year total", 0),
    ]
    for col, (key, title, accent, footer, decimals) in zip(st.columns(4), specs):
        with col:
            try:
                value, unit = overview_metrics(data, scenario, regions, year, metric=key)
            except ValueError as exc:
                metric_card(title, "N/A", "", COLORS[accent], "Check selected units")
                st.warning(f"{title}: {exc}")
                continue
            metric_card(title, "N/A" if value is None else f"{value:,.{decimals}f}",
                        unit, COLORS[accent], footer)


def options(series):
    return sorted(series.dropna().unique().tolist())


def line_or_area(df, x, y, color=None, line_color=None, title=None, area=False, color_map=None):
    if area and color:
        fig = px.area(df, x=x, y=y, color=color, title=title, color_discrete_map=color_map)
    elif color:
        fig = px.line(df, x=x, y=y, color=color, markers=True, title=title, color_discrete_map=color_map)
    else:
        fig = go.Figure()
        if area:
            fig.add_trace(go.Scatter(x=df[x], y=df[y], mode="lines", fill="tozeroy",
                                     line=dict(color=line_color or COLORS["accent"])))
        else:
            fig.add_trace(go.Scatter(x=df[x], y=df[y], mode="lines+markers",
                                     line=dict(color=line_color or COLORS["accent"])))
        fig.update_layout(title=title)
    if "unit" in df and df["unit"].nunique() == 1:
        fig.update_yaxes(title_text=df["unit"].iloc[0])
    fig.update_xaxes(title_text="Year" if x == "year" else x)
    return plotly_layout(fig)


st.sidebar.markdown("## OPEN-PROM")
st.sidebar.caption("Results Explorer")
sources = ["Synthetic demo", "Local reporting.mif", "Upload MIF"]
source = st.sidebar.selectbox("Data source", sources,
                              index=1 if MIF_FILE.exists() and os.getenv("OPEN_PROM_DEMO") != "1" else 0,
                              key="data_source")
try:
    if source == "Upload MIF":
        uploaded = st.sidebar.file_uploader("IAMC/MIF file", type=["mif", "csv"])
        if uploaded is None:
            st.info("Upload a semicolon-delimited IAMC file, or select Synthetic demo.")
            st.stop()
        contents = uploaded.getvalue()
    else:
        contents = (DEMO_FILE if source == "Synthetic demo" else MIF_FILE).read_bytes()
    data = get_data(contents)
except (OSError, ValueError) as exc:
    st.error(f"Could not load data: {exc}")
    st.info("Select Synthetic demo to explore the dashboard with populated example data.")
    st.stop()
raw = data["raw"]
scenarios = options(raw["scenario"])
regions = options(raw["region"])
years = options(raw["year"])

page = st.sidebar.radio(
    "View",
    [
        "Overview",
        "Regional Dashboard",
        "Primary Energy",
        "Electricity Mix",
        "Final Energy",
        "Emissions",
        "Power Capacity",
        "Carbon Capture",
        "Energy Flows",
        "Heat Map",
        "Scatter Plot",
        "Regional Map",
        "Scenario Comparison",
        "Indicators",
        "Data Table",
    ],
)

st.title("OPEN-PROM Results Explorer")
st.markdown(
    '<div class="subtle">Explore energy transitions, emissions and regional scenario differences.</div>',
    unsafe_allow_html=True,
)
if source == "Synthetic demo":
    st.info("Synthetic demo · 3 scenarios · 3 regions · 2020–2060. Illustrative data, not OPEN-PROM model results.")


if page == "Overview":
    with st.container():
        c1, c2, c3 = st.columns([1.2, 1.5, 1.3])
        scenario = c1.selectbox("Scenario", scenarios)
        default_region = "World" if "World" in regions else regions[0]
        selected_regions = c2.multiselect("Region(s)", regions, default=[default_region])
        yr = c3.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    selected_regions = selected_regions or [default_region]
    end_year = yr[1]

    show_metrics(data, scenario, selected_regions, end_year)
    em = data["emissions"]

    left, right = st.columns(2)
    co2_trend = (
        em.loc[
            (em["scenario"] == scenario) & em["region"].isin(selected_regions)
            & (em["gas"] == "CO2") & em["domain"].isna()
            & em["year"].between(yr[0], yr[1])
        ]
        .groupby(["year", "unit"], as_index=False)["value"].sum()
    )
    with left:
        if co2_trend.empty:
            empty_msg("No CO2 emissions data.")
        else:
            st.plotly_chart(line_or_area(co2_trend, "year", "value", line_color=COLORS["danger"],
                                         title="CO2 Emissions", area=True), width="stretch")

    pe_mix = (
        data["primary_energy"].loc[
            (data["primary_energy"]["scenario"] == scenario)
            & data["primary_energy"]["region"].isin(selected_regions)
            & (data["primary_energy"]["fuel"] != "Total")
            & data["primary_energy"]["year"].between(yr[0], yr[1])
        ]
        .groupby(["fuel", "year", "unit"], as_index=False)["value"].sum()
    )
    with right:
        if pe_mix.empty:
            empty_msg("No primary energy data.")
        else:
            fig = px.area(pe_mix, x="year", y="value", color="fuel", title="Primary Energy Mix",
                          color_discrete_map=FUEL_COLS,
                          labels={"year": "Year", "value": pe_mix["unit"].iloc[0]})
            st.plotly_chart(plotly_layout(fig), width="stretch")

    tabs = st.tabs(["Electricity", "Final Energy"])
    with tabs[0]:
        se = (
            data["secondary_elec"].loc[
                (data["secondary_elec"]["scenario"] == scenario)
                & data["secondary_elec"]["region"].isin(selected_regions)
                & data["secondary_elec"]["year"].between(yr[0], yr[1])
            ]
            .groupby(["source", "year", "unit"], as_index=False)["value"].sum()
        )
        if se.empty:
            empty_msg("No electricity generation data.")
        else:
            fig = px.area(se, x="year", y="value", color="source", title="Electricity Generation Mix",
                          color_discrete_map=FUEL_COLS,
                          labels={"year": "Year", "value": se["unit"].iloc[0]})
            st.plotly_chart(plotly_layout(fig), width="stretch")
    with tabs[1]:
        fe = (
            data["final_energy_sector"].loc[
                (data["final_energy_sector"]["scenario"] == scenario)
                & data["final_energy_sector"]["region"].isin(selected_regions)
                & data["final_energy_sector"]["year"].between(yr[0], yr[1])
            ]
            .groupby(["sector", "year", "unit"], as_index=False)["value"].sum()
        )
        if fe.empty:
            empty_msg("No final energy data.")
        else:
            fig = px.area(fe, x="year", y="value", color="sector", title="Final Energy by Sector")
            st.plotly_chart(plotly_layout(fig), width="stretch")


elif page == "Regional Dashboard":
    c1, c2, c3 = st.columns([1, 1, 1.3])
    scenario = c1.selectbox("Scenario", scenarios)
    default_region = "EU" if "EU" in regions else ("World" if "World" in regions else regions[0])
    region = c2.selectbox("Region", regions, index=regions.index(default_region))
    yr = c3.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    end_year = yr[1]

    show_metrics(data, scenario, [region], end_year)

    chart_specs = [
        ("CO2 Emissions Trend", data["emissions"], "gas", "CO2", "domain", None, "value", COLORS["danger"]),
        ("Primary Energy Mix", data["primary_energy"], "fuel", None, None, None, "value", None),
        ("Electricity Generation", data["secondary_elec"], "source", None, None, None, "value", None),
        ("Final Energy by Sector", data["final_energy_sector"], "sector", None, None, None, "value", None),
        ("Power Capacity", data["capacity"], "tech", None, None, None, "value", None),
    ]
    for i in range(0, len(chart_specs), 2):
        row = st.columns(2)
        for col, spec in zip(row, chart_specs[i:i + 2]):
            title, df, group_col, fixed_value, null_col, null_value, value_col, single_color = spec
            sub = df.loc[(df["scenario"] == scenario) & (df["region"] == region) & df["year"].between(yr[0], yr[1])].copy()
            if fixed_value is not None:
                sub = sub.loc[sub[group_col] == fixed_value]
            if null_col is not None:
                sub = sub.loc[sub[null_col].isna()]
            if group_col == "fuel":
                sub = sub.loc[sub["fuel"] != "Total"]
            with col:
                if sub.empty:
                    empty_msg("No data for " + title)
                elif fixed_value is not None:
                    fig = line_or_area(sub, "year", value_col, line_color=single_color, title=title, area=True)
                    st.plotly_chart(fig, width="stretch")
                else:
                    fig = px.area(sub, x="year", y=value_col, color=group_col, title=title,
                                  color_discrete_map=FUEL_COLS,
                                  labels={"year": "Year", "value": sub["unit"].iloc[0]})
                    st.plotly_chart(plotly_layout(fig, height=360), width="stretch")

    ind = data["indicators"].loc[
        (data["indicators"]["scenario"] == scenario) & (data["indicators"]["region"] == region)
        & data["indicators"]["variable"].isin(["GDP|PPP", "Population", "Price|Carbon"])
        & data["indicators"]["year"].between(yr[0], yr[1])
    ]
    if not ind.empty:
        fig = px.line(ind, x="year", y="value", color="variable", facet_row="variable",
                      title="Key Indicators", markers=True)
        st.plotly_chart(plotly_layout(fig, height=520, legend=False), width="stretch")


elif page in ["Primary Energy", "Electricity Mix", "Power Capacity"]:
    c1, c2, c3 = st.columns([1, 1.5, 1.3])
    scenario = c1.selectbox("Scenario", scenarios)
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    yr = c3.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    selected_regions = selected_regions or regions

    if page == "Primary Energy":
        use_gic = st.checkbox("Gross Inland Consumption")
        source = data["gic"] if use_gic else data["primary_energy"]
        df = (
            source.loc[
                (source["scenario"] == scenario) & source["region"].isin(selected_regions)
                & (source["fuel"] != "Total") & source["year"].between(yr[0], yr[1])
            ]
            .groupby(["fuel", "year", "unit"], as_index=False)["value"].sum()
        )
        title = "Gross Inland Consumption" if use_gic else "Primary Energy Mix"
        color = "fuel"
    elif page == "Electricity Mix":
        df = (
            data["secondary_elec"].loc[
                (data["secondary_elec"]["scenario"] == scenario)
                & data["secondary_elec"]["region"].isin(selected_regions)
                & data["secondary_elec"]["year"].between(yr[0], yr[1])
            ]
            .groupby(["source", "year", "unit"], as_index=False)["value"].sum()
        )
        title = "Electricity Generation Mix"
        color = "source"
    else:
        mode = st.radio("Show", ["Installed", "Additions"], horizontal=True)
        source = data["capacity"] if mode == "Installed" else data["capacity_additions"]
        df = (
            source.loc[
                (source["scenario"] == scenario) & source["region"].isin(selected_regions)
                & source["year"].between(yr[0], yr[1])
            ]
            .groupby(["tech", "year", "unit"], as_index=False)["value"].sum()
        )
        title = "Installed Power Capacity" if mode == "Installed" else "Capacity Additions"
        color = "tech"
    if df.empty:
        empty_msg("No data for this selection.")
    else:
        fig = px.area(df, x="year", y="value", color=color, title=title, color_discrete_map=FUEL_COLS)
        st.plotly_chart(plotly_layout(fig, height=540), width="stretch")


elif page == "Final Energy":
    c1, c2, c3, c4 = st.columns([1.2, 1.5, .8, 1])
    selected_scenarios = c1.multiselect("Scenario(s)", scenarios, default=scenarios)
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    year = c3.selectbox("Year", years, index=years.index(min([y for y in years if y >= 2030] or years)))
    mode = c4.radio("Group by", ["Sector", "Carrier"], horizontal=True)
    source = data["final_energy_sector"] if mode == "Sector" else data["final_energy_carrier"]
    group_col = "sector" if mode == "Sector" else "carrier"
    df = (
        source.loc[
            source["scenario"].isin(selected_scenarios or scenarios)
            & source["region"].isin(selected_regions or regions)
            & (source["year"] == int(year))
        ]
        .groupby(["scenario", group_col, "unit"], as_index=False)["value"].sum()
    )
    if df.empty:
        empty_msg("No final energy data.")
    else:
        fig = px.bar(df, x="scenario", y="value", color=group_col, title=f"Final Energy by {mode} - {year}",
                     color_discrete_map=FUEL_COLS)
        st.plotly_chart(plotly_layout(fig, height=540), width="stretch")


elif page == "Emissions":
    c1, c2, c3, c4 = st.columns([1.2, 1.4, 1, 1.2])
    selected_scenarios = c1.multiselect("Scenario(s)", scenarios, default=scenarios)
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    gases = options(data["emissions"]["gas"])
    selected_gases = c3.multiselect("Gas(es)", gases, default=[g for g in ["CO2", "Kyoto Gases"] if g in gases] or gases[:1])
    domains = ["All"] + options(data["emissions"]["domain"])
    selected_domains = c4.multiselect("Domain", domains, default=["All"])
    c5, c6, c7 = st.columns([1.5, .7, .7])
    yr = c5.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    gross = c6.checkbox("Gross")
    stack = c7.checkbox("Stack")
    source = data["gross_emissions"] if gross else data["emissions"]
    df = source.loc[
        source["scenario"].isin(selected_scenarios or scenarios)
        & source["region"].isin(selected_regions or regions)
        & source["gas"].isin(selected_gases or gases)
        & source["year"].between(yr[0], yr[1])
    ].copy()
    if "All" not in selected_domains:
        df = df.loc[df["domain"].isin(selected_domains)]
    df = non_overlapping_rows(df)
    df = df.groupby(["scenario", "region", "gas", "year", "unit"], as_index=False)["value"].sum()
    if df.empty:
        empty_msg("No emissions data.")
    else:
        if stack:
            fig = px.area(df, x="year", y="value", color="gas", facet_col="scenario",
                          title="Emissions", color_discrete_map=GAS_COLS)
        else:
            fig = px.line(df, x="year", y="value", color="gas", line_dash="region",
                          facet_col="scenario", markers=True, title="Emissions",
                          color_discrete_map=GAS_COLS)
        st.plotly_chart(plotly_layout(fig, height=560), width="stretch")


elif page == "Carbon Capture":
    c1, c2, c3, c4 = st.columns([1.2, 1.4, 1.2, 1.3])
    selected_scenarios = c1.multiselect("Scenario(s)", scenarios, default=scenarios)
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    cats = options(data["ccs"]["category"])
    selected_cats = c3.multiselect("Category", cats, default=cats)
    yr = c4.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    df = (
        non_overlapping_rows(data["ccs"]).loc[
            data["ccs"]["scenario"].isin(selected_scenarios or scenarios)
            & data["ccs"]["region"].isin(selected_regions or regions)
            & data["ccs"]["category"].isin(selected_cats or cats)
            & data["ccs"]["year"].between(yr[0], yr[1])
        ]
        .groupby(["scenario", "category", "year", "unit"], as_index=False)["value"].sum()
    )
    if df.empty:
        empty_msg("No CCS data.")
    else:
        fig = px.line(df, x="year", y="value", color="category", facet_col="scenario",
                      markers=True, title="Carbon Capture and Storage")
        st.plotly_chart(plotly_layout(fig, height=540), width="stretch")


elif page == "Energy Flows":
    c1, c2, c3 = st.columns(3)
    scenario = c1.selectbox("Scenario", scenarios)
    region = c2.selectbox("Region", regions)
    year = c3.selectbox("Year", years, index=years.index(min([y for y in years if y >= 2030] or years)))
    df = data["flows"].loc[
        (data["flows"]["scenario"] == scenario)
        & (data["flows"]["region"] == region)
        & (data["flows"]["year"] == int(year))
        & (data["flows"]["value"] > 0)
    ]
    if df.empty:
        empty_msg("No flow data.")
    else:
        nodes = pd.Index(pd.unique(pd.concat([df["source"], df["target"]], ignore_index=True)))
        fig = go.Figure(
            data=[
                go.Sankey(
                    node=dict(label=nodes.tolist(), pad=12, thickness=18, color="#D8E0EA"),
                    link=dict(
                        source=df["source"].map(nodes.get_loc),
                        target=df["target"].map(nodes.get_loc),
                        value=df["value"],
                        color="rgba(31,154,138,.28)",
                    ),
                )
            ]
        )
        fig.update_layout(title="Primary to Final Energy Flows")
        st.plotly_chart(plotly_layout(fig, height=560, legend=False), width="stretch")


elif page == "Heat Map":
    c1, c2, c3 = st.columns([1, 2, 1.3])
    scenario = c1.selectbox("Scenario", scenarios)
    all_vars = options(raw["variable"])
    default_var = "Emissions|CO2" if "Emissions|CO2" in all_vars else all_vars[0]
    variable = c2.selectbox("Variable", all_vars, index=all_vars.index(default_var))
    yr = c3.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    df = (
        raw.loc[
            (raw["scenario"] == scenario) & (raw["variable"] == variable)
            & raw["year"].between(yr[0], yr[1])
        ]
        .groupby(["region", "year"], as_index=False)["value"].sum()
    )
    if df.empty:
        empty_msg("No data for this variable.")
    else:
        mat = df.pivot(index="region", columns="year", values="value")
        fig = px.imshow(mat, aspect="auto", color_continuous_scale=["#1D4E89", "#F6C85F", "#C2414B"],
                        title="Region x Year Intensity")
        st.plotly_chart(plotly_layout(fig, height=620, legend=False), width="stretch")


elif page == "Scatter Plot":
    all_vars = options(raw["variable"])
    c1, c2, c3, c4, c5 = st.columns([1.2, 1.6, 1.6, 1.3, .8])
    selected_scenarios = c1.multiselect("Scenario(s)", scenarios, default=scenarios)
    x_var = c2.selectbox("X Variable", all_vars, index=all_vars.index("GDP|PPP") if "GDP|PPP" in all_vars else 0)
    y_var = c3.selectbox("Y Variable", all_vars, index=all_vars.index("Emissions|CO2") if "Emissions|CO2" in all_vars else min(1, len(all_vars) - 1))
    selected_regions = c4.multiselect("Region(s)", regions, default=regions)
    year = c5.selectbox("Year", years, index=years.index(min([y for y in years if y >= 2030] or years)))
    xdf = raw.loc[
        (raw["variable"] == x_var) & raw["scenario"].isin(selected_scenarios or scenarios)
        & raw["region"].isin(selected_regions or regions) & (raw["year"] == int(year)),
        ["scenario", "region", "value", "unit"],
    ].rename(columns={"value": "x", "unit": "xunit"})
    ydf = raw.loc[
        (raw["variable"] == y_var) & raw["scenario"].isin(selected_scenarios or scenarios)
        & raw["region"].isin(selected_regions or regions) & (raw["year"] == int(year)),
        ["scenario", "region", "value", "unit"],
    ].rename(columns={"value": "y", "unit": "yunit"})
    df = xdf.merge(ydf, on=["scenario", "region"])
    if df.empty:
        empty_msg("No overlapping data for these variables.")
    else:
        fig = px.scatter(df, x="x", y="y", color="region", symbol="scenario",
                         hover_data=["scenario", "region"], title=f"{x_var} vs {y_var} - {year}")
        st.plotly_chart(plotly_layout(fig, height=560), width="stretch")


elif page == "Regional Map":
    c1, c2 = st.columns([1, 1])
    scenario = c1.selectbox("Scenario", scenarios)
    year = c2.selectbox("Year", years, index=years.index(min([y for y in years if y >= 2030] or years)))
    df = data["regional"].loc[
        (data["regional"]["scenario"] == scenario) & (data["regional"]["year"] == int(year))
    ]
    if df.empty:
        empty_msg("No map data.")
    else:
        fig = px.scatter_geo(
            df,
            lat="lat",
            lon="lon",
            size="value",
            color="value",
            hover_name="region",
            color_continuous_scale=["#1D4E89", "#F6C85F", "#C2414B"],
            projection="natural earth",
            title="CO2 Emissions - Regional Map",
        )
        fig.update_geos(showland=True, landcolor="#F0F3F7", showcountries=True, countrycolor="#C8D2DE")
        st.plotly_chart(plotly_layout(fig, height=600, legend=False), width="stretch")


elif page == "Scenario Comparison":
    all_vars = options(raw["variable"])
    c1, c2, c3 = st.columns([2, 1.6, .8])
    default_var = "Emissions|CO2" if "Emissions|CO2" in all_vars else all_vars[0]
    variable = c1.selectbox("Variable", all_vars, index=all_vars.index(default_var))
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    year = c3.selectbox("Year", years, index=years.index(min([y for y in years if y >= 2030] or years)))
    df = raw.loc[
        (raw["variable"] == variable)
        & raw["region"].isin(selected_regions or regions)
        & (raw["year"] == int(year))
    ]
    if df.empty:
        empty_msg("No scenario comparison data.")
    else:
        fig = px.bar(df, x="region", y="value", color="scenario", barmode="group",
                     title=f"{variable} - {year}",
                     labels={"region": "Region", "value": df["unit"].iloc[0]})
        st.plotly_chart(plotly_layout(fig, height=560), width="stretch")


elif page == "Indicators":
    ind_vars = options(data["indicators"]["variable"])
    c1, c2, c3, c4 = st.columns([1.4, 1.2, 1.4, 1.3])
    variable = c1.selectbox("Variable", ind_vars)
    selected_scenarios = c2.multiselect("Scenario(s)", scenarios, default=scenarios)
    selected_regions = c3.multiselect("Region(s)", regions, default=regions)
    yr = c4.slider("Year range", int(min(years)), int(max(years)), (max(min(years), 2020), min(max(years), 2060)))
    df = data["indicators"].loc[
        (data["indicators"]["variable"] == variable)
        & data["indicators"]["scenario"].isin(selected_scenarios or scenarios)
        & data["indicators"]["region"].isin(selected_regions or regions)
        & data["indicators"]["year"].between(yr[0], yr[1])
    ]
    if df.empty:
        empty_msg("No indicator data.")
    else:
        fig = px.line(df, x="year", y="value", color="scenario", line_dash="region",
                      markers=True, title=variable)
        st.plotly_chart(plotly_layout(fig, height=540), width="stretch")


elif page == "Data Table":
    c1, c2, c3 = st.columns(3)
    selected_scenarios = c1.multiselect("Scenario(s)", scenarios, default=scenarios)
    selected_regions = c2.multiselect("Region(s)", regions, default=regions)
    all_vars = options(raw["variable"])
    selected_vars = c3.multiselect("Variable(s)", all_vars)
    df = raw.loc[
        raw["scenario"].isin(selected_scenarios or scenarios)
        & raw["region"].isin(selected_regions or regions)
    ].copy()
    if selected_vars:
        df = df.loc[df["variable"].isin(selected_vars)]
    st.dataframe(
        df[["model", "scenario", "region", "variable", "unit", "year", "value"]],
        width="stretch",
        height=650,
    )
    st.download_button(
        "Download filtered CSV",
        df.to_csv(index=False).encode("utf-8"),
        file_name="open_prom_filtered.csv",
        mime="text/csv",
    )
