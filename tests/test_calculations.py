"""Analytical regression tests using independently calculated expectations."""
from io import StringIO
from pathlib import Path
import unittest

import pandas as pd
from reporting_fixture import reporting_fixture

from iamc_loader import (
    derive_datasets, load_iamc_dashboard, non_overlapping_rows,
    overview_metrics, read_mif,
)

ROOT = Path(__file__).resolve().parents[1]


class CalculationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = load_iamc_dashboard(ROOT / "data" / "demo.mif")

    def test_demo_covers_all_analytical_datasets(self):
        raw = self.data["raw"]
        self.assertEqual(raw["scenario"].nunique(), 3)
        self.assertEqual(set(raw["region"]), {"EU", "USA", "IND"})
        self.assertEqual(set(raw["year"]), {2020, 2030, 2040, 2050, 2060})
        self.assertEqual(raw["model"].unique().tolist(), ["Synthetic Demo"])
        for name, df in self.data.items():
            with self.subTest(dataset=name):
                self.assertFalse(df.empty)
        keys = ["model", "scenario", "region", "variable", "unit", "year"]
        self.assertFalse(raw.duplicated(keys).any())

    def test_parser_reshapes_years_and_skips_missing_values(self):
        source = StringIO("\ufeffModel;Scenario;Region;Variable;Unit;2020;2030;2040;\n"
                          "M;S;EU;Population;million;450;;invalid;\n")
        result = read_mif(source)
        self.assertEqual(len(result), 1)
        self.assertEqual(result.iloc[0]["year"], 2020)
        self.assertEqual(result.iloc[0]["value"], 450)

    def test_invalid_inputs_have_actionable_errors(self):
        fixtures = [
            ("Model;2020\nM;1\n", "Missing IAMC columns"),
            ("Model;Scenario;Region;Variable;Unit\nM;S;EU;Population;million\n", "No year columns"),
            ("Model;Scenario;Region;Variable;Unit;2020\nM;S;EU;Population;million;\n", "No numeric observations"),
        ]
        for contents, message in fixtures:
            with self.subTest(message=message), self.assertRaisesRegex(ValueError, message):
                read_mif(StringIO(contents))

    def test_overview_2020_has_known_totals_and_units(self):
        metrics = overview_metrics(self.data, "Baseline", ["EU"], 2020)
        self.assertEqual(metrics["co2"], (3200, "Mt CO2/yr"))
        self.assertEqual(metrics["primary"], (58, "EJ/yr"))
        self.assertEqual(metrics["capacity"], (540, "GW"))
        # Hydro + wind + solar + biofuels = 3 EJ/yr out of 9 EJ/yr.
        self.assertAlmostEqual(metrics["renewable"][0], 100 / 3)

    def test_scenario_region_year_filters_are_independent(self):
        metrics = overview_metrics(self.data, "Accelerated transition", ["EU"], 2060)
        self.assertAlmostEqual(metrics["co2"][0], 480)
        combined = overview_metrics(self.data, "Baseline", ["EU", "USA"], 2020)
        self.assertAlmostEqual(combined["co2"][0], 8000)
        self.assertAlmostEqual(combined["primary"][0], 145)
        self.assertAlmostEqual(combined["renewable"][0], 100 / 3)

    def test_emissions_totals_do_not_double_count_children(self):
        emissions = self.data["emissions"]
        df = emissions.loc[(emissions.scenario == "Baseline") &
                           (emissions.region == "EU") & (emissions.year == 2020)]
        self.assertEqual(non_overlapping_rows(df)["value"].sum(), 3200)
        # Domain selection must preserve the domain total, not add supply/demand.
        energy = non_overlapping_rows(df.loc[df.domain == "Energy"])
        self.assertEqual(energy["value"].sum(), 2880)
        # When the overall total is absent, sum the disjoint Energy/Land Use branches.
        without_total = df.loc[df.variable != "Emissions|CO2"]
        self.assertEqual(non_overlapping_rows(without_total)["value"].sum(), 3200)

    def test_hierarchy_is_scoped_by_observation_and_zero_parent_wins(self):
        df = pd.DataFrame([
            ["M", "S", "EU", "Emissions|CO2", "Mt CO2/yr", 2020, 0],
            ["M", "S", "EU", "Emissions|CO2|Energy", "Mt CO2/yr", 2020, 8],
            ["M", "S", "USA", "Emissions|CO2|Energy", "Mt CO2/yr", 2020, 9],
            ["M", "T", "EU", "Emissions|CO2|Energy", "Mt CO2/yr", 2020, 10],
            ["M", "S", "EU", "Emissions|CO2|Energy", "Mt CO2/yr", 2030, 11],
        ], columns=["model", "scenario", "region", "variable", "unit", "year", "value"])
        self.assertEqual(sorted(non_overlapping_rows(df)["value"]), [0, 9, 10, 11])

    def test_final_energy_carriers_are_not_sector_flows(self):
        sectors = set(self.data["final_energy_sector"]["sector"])
        carriers = set(self.data["final_energy_carrier"]["carrier"])
        self.assertEqual(sectors, {"Industry", "Transport", "Buildings"})
        self.assertEqual(carriers, {"Electricity", "Gas", "Oil"})
        flows = self.data["flows"]
        flow = flows.loc[(flows.scenario == "Baseline") &
                         (flows.region == "EU") & (flows.year == 2020)]
        self.assertEqual(flow.loc[flow.target == "Primary Energy", "value"].sum(), 58)
        self.assertEqual(flow.loc[flow.source == "Primary Energy", "value"].sum(), 35)
        self.assertFalse(flow["target"].isin(carriers).any())

    def test_regional_map_uses_only_top_level_co2(self):
        regional = self.data["regional"]
        point = regional.loc[(regional.scenario == "Baseline") &
                             (regional.region == "EU") & (regional.year == 2020)]
        self.assertEqual(point["value"].tolist(), [3200])
        self.assertFalse(point[["lat", "lon"]].isna().any().any())

    def test_missing_zero_and_mixed_units_are_distinct(self):
        missing = overview_metrics(self.data, "Missing", ["EU"], 2020)
        self.assertIsNone(missing["co2"][0])
        raw = self.data["raw"].copy()
        raw.loc[raw.variable.str.startswith("Capacity|"), "value"] = 0
        raw.loc[raw.variable.str.startswith("Secondary Energy|Electricity|"), "value"] = 0
        metrics = overview_metrics(derive_datasets(raw), "Baseline", ["EU"], 2020)
        self.assertEqual(metrics["capacity"], (0, "GW"))
        self.assertIsNone(metrics["renewable"][0])
        raw.loc[(raw.variable == "Primary Energy") & (raw.region == "USA"), "unit"] = "Mtoe/yr"
        with self.assertRaisesRegex(ValueError, "mixed or missing units"):
            overview_metrics(derive_datasets(raw), "Baseline", ["EU", "USA"], 2020)

    def test_reporting_diagnostics_do_not_change_kpis(self):
        data = derive_datasets(reporting_fixture())
        metrics = overview_metrics(data, "Baseline", ["EU"], 2020)
        self.assertEqual(metrics["co2"], (3200, "Mt CO2/yr"))
        self.assertEqual(metrics["primary"], (58, "EJ/yr"))
        self.assertEqual(metrics["capacity"], (540, "GW"))
        self.assertAlmostEqual(metrics["renewable"][0], 100 / 3)
        self.assertFalse(data["emissions"].variable.str.contains("Budget|Cumulated").any())
        self.assertFalse(data["primary_energy"].fuel.isin(["Fossil Share", "Renewables"]).any())
        self.assertFalse(data["secondary_elec"].source.isin(
            ["Renewables", "Renewables Share", "Total", "Demand"]).any())
        self.assertFalse(data["flows"]["source"].isin(["Fossil Share", "Renewables"]).any())
        self.assertFalse(data["final_energy_sector"].sector.eq("Electricity Share").any())
        # Diagnostics remain accessible in explicit-variable views and export.
        self.assertTrue(data["raw"].variable.eq("Emissions|CO2|Cumulated").any())
        self.assertTrue(data["raw"].variable.eq("Primary Energy|Fossil Share").any())

    def test_missing_annual_total_does_not_substitute_cumulative_or_sectors(self):
        raw = reporting_fixture()
        raw = raw.loc[raw.variable != "Emissions|CO2"]
        value, unit = overview_metrics(derive_datasets(raw), "Baseline", ["EU"], 2020, metric="co2")
        self.assertIsNone(value)

    def test_unit_failure_isolated_to_one_kpi_and_missing_units_not_ignored(self):
        raw = reporting_fixture()
        raw.loc[(raw.region == "EU") & (raw.variable == "Capacity|Electricity|Wind"), "unit"] = None
        data = derive_datasets(raw)
        with self.assertRaisesRegex(ValueError, "mixed or missing units"):
            overview_metrics(data, "Baseline", ["EU"], 2020, metric="capacity")
        self.assertEqual(overview_metrics(data, "Baseline", ["EU"], 2020, metric="co2"),
                         (3200, "Mt CO2/yr"))
        self.assertAlmostEqual(overview_metrics(data, "Baseline", ["EU"], 2020, metric="renewable")[0],
                               100 / 3)


if __name__ == "__main__":
    unittest.main()
