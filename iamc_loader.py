import pandas as pd


CARRIERS = [
    "Biodiesel", "Biofuels", "Biogasoline", "Biokerosene", "Biomass and Waste",
    "Coal", "Crude Oil and Feedstocks", "Diesel Oil", "Electricity", "Ethanol",
    "Fossil Liquids", "Gas", "Gasoline", "Geothermal and other renewable sources",
    "Hard Coal; Coke and Other Solids", "Heat", "Hydro", "Hydrogen", "Kerosene",
    "Lignite", "Liquefied Petroleum Gas", "Methanol", "Natural Gas", "Nuclear",
    "Oil", "Other fuels", "Other Gases", "Other Liquids", "Renewables",
    "Residual Fuel Oil", "Solar", "Solids", "Steam", "Wind",
]

REN_SOURCES = [
    "Hydro",
    "Wind",
    "Solar",
    "Biofuels",
    "Geothermal and other renewable sources",
]


def _seg(series, n):
    return series.str.split("|").str[n - 1]


def quantities_only(df):
    """Exclude diagnostic shares, budgets and cumulative series from flow views.

    Keep these records in raw data for explicit variable selection elsewhere.
    Missing units are retained so validation can flag them, not hide them.
    """
    auxiliary = df["variable"].str.contains(
        r"(?i)\b(?:share|cumulated|cumulative|budget\w*)\b", na=False
    )
    dimensionless = df["unit"].fillna("").str.strip().str.lower().isin(
        ["1", "%", "percent", "fraction", "dimensionless"]
    )
    return df.loc[~auxiliary & ~dimensionless].copy()


def energy_components(df, label, keep_total=False):
    """Select individual fuels/technologies, not overlapping summary categories."""
    excluded = {"Renewables", "Fossil", "Non-Fossil", "Demand"}
    if not keep_total:
        excluded.add("Total")
    df = quantities_only(df)
    return df.loc[~df[label].isin(excluded)].copy()


def read_mif(path):
    """Read semicolon-delimited IAMC/MIF wide data and return long IAMC rows."""
    raw = pd.read_csv(path, sep=";", encoding="utf-8-sig", dtype=str)
    raw.columns = [str(c).strip().lower() for c in raw.columns]
    raw = raw.loc[:, [c for c in raw.columns if c and not c.startswith("unnamed")]]

    required = {"model", "scenario", "region", "variable", "unit"}
    missing = required.difference(raw.columns)
    if missing:
        raise ValueError("Missing IAMC columns: " + ", ".join(sorted(missing)))

    year_cols = [c for c in raw.columns if c.isdigit()]
    if not year_cols:
        raise ValueError("No year columns found in IAMC file.")

    long = raw.melt(
        id_vars=["model", "scenario", "region", "variable", "unit"],
        value_vars=year_cols,
        var_name="year",
        value_name="value",
    )
    long["year"] = long["year"].astype(int)
    long["value"] = pd.to_numeric(long["value"], errors="coerce")
    long = long.dropna(subset=["value"]).copy()
    if long.empty:
        raise ValueError("No numeric observations found in the IAMC file.")
    return long


def non_overlapping_rows(df):
    """Prefer reported parents over descendants within each observation group.

    Apply domain filters first. A reported zero parent still takes precedence;
    missing parents allow available descendants through, without imputing totals.
    """
    if df.empty:
        return df.copy()
    keys = ["model", "scenario", "region", "year", "unit"]
    pieces = []
    for _, group in df.groupby(keys, dropna=False, sort=False):
        variables = set(group["variable"])
        keep = group["variable"].map(
            lambda v: not any(
                "|".join(v.split("|")[:i]) in variables
                for i in range(1, len(v.split("|")))
            )
        )
        pieces.append(group.loc[keep])
    return pd.concat(pieces)


def overview_metrics(data, scenario, regions, year, *, metric=None):
    """Calculate KPIs, or one named KPI so its error cannot hide other cards."""
    def selected(name):
        df = data[name]
        return df.loc[(df["scenario"] == scenario) & df["region"].isin(regions)
                      & (df["year"] == year)]

    def total(df):
        if df.empty:
            return None, ""
        units = df["unit"].dropna().unique()
        if len(units) != 1 or df["unit"].isna().any() or df["unit"].str.strip().eq("").any():
            detail = ", ".join(str(unit) for unit in units) or "none"
            raise ValueError(f"KPI selection contains mixed or missing units (found: {detail}).")
        return df["value"].sum(), units[0]

    def calculate(key):
        if key == "co2":
            emissions = selected("emissions")
            # A cumulative stock or a budget is never an annual emissions total.
            return total(emissions.loc[emissions["variable"] == "Emissions|CO2"])
        if key == "primary":
            primary = selected("primary_energy")
            return total(primary.loc[primary["variable"] == "Primary Energy"])
        if key == "capacity":
            return total(selected("capacity"))
        if key == "renewable":
            electricity = selected("secondary_elec")
            generation, _ = total(electricity)
            renewable = electricity.loc[electricity["source"].isin(REN_SOURCES), "value"].sum()
            share = None if generation is None or generation <= 0 else 100 * renewable / generation
            return share, "%"
        raise ValueError(f"Unknown KPI: {key}")

    if metric is not None:
        return calculate(metric)
    return {key: calculate(key) for key in ["co2", "primary", "capacity", "renewable"]}


def derive_flows(primary_energy, final_energy_sector):
    src = primary_energy.loc[primary_energy["fuel"] != "Total"].copy()
    src["source"] = src["fuel"]
    src["target"] = "Primary Energy"
    src = src[["scenario", "region", "year", "source", "target", "value"]]
    src = src.loc[src["value"] > 0]

    tgt = final_energy_sector.loc[
        ~final_energy_sector["sector"].isin(["Total", "w/o bunkers"])
    ].copy()
    tgt["source"] = "Primary Energy"
    tgt["target"] = tgt["sector"]
    tgt = tgt[["scenario", "region", "year", "source", "target", "value"]]
    tgt = tgt.loc[tgt["value"] > 0]

    return (
        pd.concat([src, tgt], ignore_index=True)
        .groupby(["scenario", "region", "year", "source", "target"], as_index=False)["value"]
        .sum()
    )


def derive_regional(emissions):
    centroids = pd.DataFrame(
        [
            ("CHA", 35.9, 104.2), ("EUR", 50.1, 10.4), ("EU", 50.1, 10.4),
            ("USA", 38.9, -77.0), ("IND", 20.6, 78.9), ("JPN", 36.2, 138.3),
            ("RUS", 61.5, 90.0), ("BRA", -14.2, -51.9), ("AFR", -8.8, 34.5),
            ("MEA", 29.3, 42.5), ("OAS", 15.0, 100.0), ("LAM", -15.0, -60.0),
            ("World", 20.0, 10.0),
        ],
        columns=["region", "lat", "lon"],
    )
    top_level = emissions["domain"].isna()
    co2 = emissions.loc[(emissions["gas"] == "CO2") & top_level]
    regional = co2.groupby(["scenario", "region", "year"], as_index=False)["value"].sum()
    return regional.merge(centroids, on="region", how="left").dropna(subset=["lat"])


def derive_datasets(long):
    emissions = long.loc[long["variable"].str.startswith("Emissions|", na=False)].copy()
    emissions["gas"] = _seg(emissions["variable"], 2)
    emissions["domain"] = _seg(emissions["variable"], 3)
    emissions["side"] = _seg(emissions["variable"], 4)
    emissions["sector"] = _seg(emissions["variable"], 5)

    gross_emissions = long.loc[
        long["variable"].str.startswith("Gross Emissions|", na=False)
    ].copy()
    gross_emissions["gas"] = _seg(gross_emissions["variable"], 2)
    gross_emissions["domain"] = _seg(gross_emissions["variable"], 3)
    gross_emissions["side"] = _seg(gross_emissions["variable"], 4)
    gross_emissions["sector"] = _seg(gross_emissions["variable"], 5)

    primary_energy = long.loc[long["variable"].str.startswith("Primary Energy", na=False)].copy()
    primary_energy["fuel"] = primary_energy["variable"].where(
        primary_energy["variable"].eq("Primary Energy"),
        primary_energy["variable"].str.replace(r"^Primary Energy\|", "", regex=True),
    )
    primary_energy.loc[primary_energy["variable"].eq("Primary Energy"), "fuel"] = "Total"
    primary_energy = primary_energy.loc[~primary_energy["fuel"].str.contains(r"\|", na=False)]

    gic = long.loc[long["variable"].str.startswith("Gross Inland Consumption", na=False)].copy()
    gic["fuel"] = gic["variable"].where(
        gic["variable"].eq("Gross Inland Consumption"),
        gic["variable"].str.replace(r"^Gross Inland Consumption\|", "", regex=True),
    )
    gic.loc[gic["variable"].eq("Gross Inland Consumption"), "fuel"] = "Total"
    gic = gic.loc[~gic["fuel"].str.contains(r"\|", na=False)]

    secondary_elec = long.loc[
        long["variable"].str.startswith("Secondary Energy|Electricity|", na=False)
        & ~long["variable"].isin(["Secondary Energy|Electricity", "Secondary Energy|Electricity|Demand"])
    ].copy()
    secondary_elec["source"] = secondary_elec["variable"].str.replace(
        r"^Secondary Energy\|Electricity\|", "", regex=True
    )
    secondary_elec = secondary_elec.loc[~secondary_elec["source"].str.contains(r"\|", na=False)]

    secondary_heat = long.loc[
        long["variable"].str.startswith("Secondary Energy|Heat|", na=False)
    ].copy()
    secondary_heat["source"] = secondary_heat["variable"].str.replace(
        r"^Secondary Energy\|Heat\|", "", regex=True
    )
    secondary_heat = secondary_heat.loc[~secondary_heat["source"].str.contains(r"\|", na=False)]

    secondary_hydrogen = long.loc[
        long["variable"].str.startswith("Secondary Energy|Hydrogen|", na=False)
    ].copy()
    secondary_hydrogen["source"] = secondary_hydrogen["variable"].str.replace(
        r"^Secondary Energy\|Hydrogen\|", "", regex=True
    )
    secondary_hydrogen = secondary_hydrogen.loc[
        ~secondary_hydrogen["source"].str.contains(r"\|", na=False)
    ]

    final_energy_base = long.loc[
        long["variable"].str.startswith("Final Energy|", na=False)
        & ~long["variable"].str.startswith("Final Energy w/o", na=False)
    ].copy()

    final_energy_sector = final_energy_base.copy()
    final_energy_sector["sector"] = final_energy_sector["variable"].str.replace(
        r"^Final Energy\|", "", regex=True
    )
    final_energy_sector = final_energy_sector.loc[
        ~final_energy_sector["sector"].str.contains(r"\|", na=False)
        & ~final_energy_sector["sector"].isin(["w/o bunkers"] + CARRIERS)
    ]

    final_energy_carrier = final_energy_base.copy()
    final_energy_carrier["carrier"] = final_energy_carrier["variable"].str.replace(
        r"^Final Energy\|", "", regex=True
    )
    final_energy_carrier = final_energy_carrier.loc[
        ~final_energy_carrier["carrier"].str.contains(r"\|", na=False)
        & final_energy_carrier["carrier"].isin(CARRIERS)
    ]

    capacity = long.loc[
        long["variable"].str.startswith("Capacity|Electricity|", na=False)
    ].copy()
    capacity["tech"] = capacity["variable"].str.replace(
        r"^Capacity\|Electricity\|", "", regex=True
    )
    capacity = capacity.loc[~capacity["tech"].str.contains(r"\|", na=False)]

    capacity_additions = long.loc[
        long["variable"].str.startswith("Capacity Additions|Electricity|", na=False)
    ].copy()
    capacity_additions["tech"] = capacity_additions["variable"].str.replace(
        r"^Capacity Additions\|Electricity\|", "", regex=True
    )
    capacity_additions = capacity_additions.loc[
        ~capacity_additions["tech"].str.contains(r"\|", na=False)
    ]

    ccs = long.loc[long["variable"].str.startswith("Carbon Capture|", na=False)].copy()
    ccs["category"] = _seg(ccs["variable"], 2)
    ccs["sub"] = _seg(ccs["variable"], 3)

    indicators = long.loc[
        long["variable"].isin(["GDP|PPP", "Population", "Price|Carbon"])
        | long["variable"].str.startswith("Price|Carbon", na=False)
    ].copy()

    # Keep raw records available for the table, scatter and variable-comparison
    # views, but never mix diagnostic indicators into physical quantity stacks.
    emissions = quantities_only(emissions)
    gross_emissions = quantities_only(gross_emissions)
    primary_energy = energy_components(primary_energy, "fuel", keep_total=True)
    gic = energy_components(gic, "fuel", keep_total=True)
    secondary_elec = energy_components(secondary_elec, "source")
    secondary_heat = energy_components(secondary_heat, "source")
    secondary_hydrogen = energy_components(secondary_hydrogen, "source")
    capacity = energy_components(capacity, "tech")
    capacity_additions = energy_components(capacity_additions, "tech")
    final_energy_sector = quantities_only(final_energy_sector)
    final_energy_carrier = quantities_only(final_energy_carrier)
    flows = derive_flows(primary_energy, final_energy_sector)
    regional = derive_regional(emissions)

    return {
        "raw": long,
        "emissions": emissions,
        "gross_emissions": gross_emissions,
        "primary_energy": primary_energy,
        "gic": gic,
        "secondary_elec": secondary_elec,
        "secondary_heat": secondary_heat,
        "secondary_hydrogen": secondary_hydrogen,
        "final_energy_sector": final_energy_sector,
        "final_energy_carrier": final_energy_carrier,
        "capacity": capacity,
        "capacity_additions": capacity_additions,
        "ccs": ccs,
        "indicators": indicators,
        "flows": flows,
        "regional": regional,
    }


def load_iamc_dashboard(path):
    long = read_mif(path)
    return derive_datasets(long)
