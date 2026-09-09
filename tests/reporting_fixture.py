"""Synthetic observations matching the troublesome reporting variable names."""
from pathlib import Path

import pandas as pd

from iamc_loader import read_mif


def reporting_fixture():
    raw = read_mif(Path(__file__).resolve().parents[1] / "data" / "demo.mif")
    extras = [
        ("Emissions|CO2|Cumulated", "Gt CO2", 100),
        ("Emissions|CO2|Budget1p5C", "Gt CO2/yr", 500),
        ("Emissions|CO2|Budget2C", "Mt CO2/yr", 600),
        ("Primary Energy|Fossil Share", "1", 0.8),
        ("Primary Energy|Renewables", "EJ/yr", 7),
        ("Gross Inland Consumption|Fossil Share", "%", 80),
        ("Secondary Energy|Electricity|Renewables", "EJ/yr", 3),
        ("Secondary Energy|Electricity|Renewables Share", "1", 1 / 3),
        ("Secondary Energy|Electricity|Total", "EJ/yr", 9),
        ("Secondary Energy|Electricity|Demand", "EJ/yr", 9),
        ("Capacity|Electricity|Renewables", "GW", 180),
        ("Capacity|Electricity|Total", "GW", 540),
        ("Final Energy|Electricity Share", "1", 8 / 35),
    ]
    rows = [["Synthetic Demo", "Baseline", "EU", name, unit, 2020, value]
            for name, unit, value in extras]
    return pd.concat([raw, pd.DataFrame(rows, columns=raw.columns)], ignore_index=True)
