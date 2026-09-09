"""Exercise the real Streamlit page code with the bundled synthetic dataset."""
from pathlib import Path
import unittest
import json
from unittest.mock import patch

import streamlit as st
from iamc_loader import derive_datasets
from reporting_fixture import reporting_fixture

from streamlit.testing.v1 import AppTest

ROOT = Path(__file__).resolve().parents[1]


class DashboardSmokeTests(unittest.TestCase):
    def test_regional_chart_excludes_totals_and_diagnostics(self):
        data = derive_datasets(reporting_fixture())
        st.cache_data.clear()
        try:
            with patch("iamc_loader.load_iamc_dashboard", return_value=data):
                app = AppTest.from_file(str(ROOT / "streamlit_app.py"), default_timeout=30)
                app.session_state["data_source"] = "Synthetic demo"
                app.run()
                app.sidebar.radio[0].set_value("Regional Dashboard").run()
                app.selectbox[0].set_value("Baseline").run()
                self.assertFalse(app.exception, str(app.exception))
                self.assertFalse(app.error, str(app.error))
                self.assertFalse(app.warning, str(app.warning))
                charts = [json.loads(chart.proto.spec) for chart in app.get("plotly_chart")]
                primary = next(chart for chart in charts if chart["layout"]["title"]["text"] == "Primary Energy Mix")
                names = {trace["name"] for trace in primary["data"]}
                self.assertNotIn("Total", names)
                self.assertNotIn("Fossil Share", names)
                self.assertNotIn("Renewables", names)
                self.assertIn("Coal", names)
        finally:
            st.cache_data.clear()

    def test_one_bad_kpi_does_not_hide_valid_cards(self):
        raw = reporting_fixture()
        raw.loc[(raw.region == "EU") & (raw.variable == "Capacity|Electricity|Wind"), "unit"] = None
        st.cache_data.clear()
        try:
            with patch("iamc_loader.load_iamc_dashboard", return_value=derive_datasets(raw)):
                app = AppTest.from_file(str(ROOT / "streamlit_app.py"), default_timeout=30)
                app.session_state["data_source"] = "Synthetic demo"
                app.run()
                self.assertFalse(app.exception, str(app.exception))
                self.assertEqual(len(app.warning), 1)
                self.assertIn("Power Capacity", app.warning[0].value)
                cards = [item.value for item in app.markdown if 'class="metric-card"' in item.value]
                self.assertEqual(len(cards), 4)
                self.assertEqual(sum("N/A" in card for card in cards), 1)
        finally:
            st.cache_data.clear()

    def test_every_demo_page_renders_without_exceptions(self):
        app = AppTest.from_file(str(ROOT / "streamlit_app.py"), default_timeout=30)
        # Set source before first render, even when a private local file exists.
        app.session_state["data_source"] = "Synthetic demo"
        app.run()
        self.assertFalse(app.exception, str(app.exception))
        self.assertTrue(any("Synthetic demo" in info.value for info in app.info))
        views = app.sidebar.radio[0].options
        self.assertEqual(len(views), 15)
        for view in views:
            with self.subTest(view=view):
                app.sidebar.radio[0].set_value(view).run()
                self.assertFalse(app.exception, str(app.exception))
                self.assertFalse(app.error, str(app.error))
                if view == "Data Table":
                    self.assertGreater(len(app.dataframe[0].value), 0)
                else:
                    self.assertGreater(len(app.get("plotly_chart")), 0)


if __name__ == "__main__":
    unittest.main()
