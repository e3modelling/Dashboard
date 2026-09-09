# OPEN-PROM Results Explorer

An interactive dashboard for exploring energy, emissions, capacity, carbon-capture, and socioeconomic results from the OPEN-PROM integrated assessment model. The repository provides two versions of the dashboard:

- A Python application built with Streamlit and Plotly
- An R application built with Shiny, bs4Dash, Plotly, Leaflet, and networkD3

Both applications read the same IAMC-style MIF data and provide filters for comparing scenarios, regions, variables, and years.

## Dashboard features

The explorer includes:

- Overview and regional KPI dashboards
- Primary-energy and gross inland consumption trends
- Electricity-generation mixes
- Final-energy analysis by sector or carrier
- Installed power capacity and capacity additions
- Greenhouse-gas emissions and carbon-capture trends
- Sankey diagrams of primary-to-final energy flows
- Region-by-year heatmaps and variable scatter plots
- Regional CO2 maps
- Scenario comparisons and socioeconomic indicators
- Filterable data tables and CSV export

## Input data

Place an IAMC-format data file named `reporting.mif` in the repository root, next to the application files:

```text
dashboard/
|-- reporting.mif
|-- streamlit_app.py
|-- iamc_loader.py
|-- app.R
|-- requirements.txt
`-- README.md
```

The file must be semicolon-delimited and contain these columns:

```text
Model;Scenario;Region;Variable;Unit;2010;2011;...;2100
```

The applications convert the year columns into a long-form dataset when they start. Values that cannot be parsed as numbers are ignored. Variable names are expected to follow the hierarchical IAMC convention, for example:

```text
Emissions|CO2
Primary Energy|Coal
Secondary Energy|Electricity|Wind
Final Energy|Industry
Capacity|Electricity|Solar
```

The included `test.mif` demonstrates the expected file structure, but it may not contain populated values. To use it as a template, copy or rename it to `reporting.mif` and add model results.

## Run the Python/Streamlit application

### 1. Create a virtual environment

From PowerShell:

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
```

On macOS or Linux:

```bash
python3 -m venv .venv
source .venv/bin/activate
```

### 2. Install dependencies

```bash
python -m pip install -r requirements.txt
```

### 3. Add the data file

Copy the model output into the repository root and name it `reporting.mif`.

### 4. Start the dashboard

```bash
streamlit run streamlit_app.py
```

Streamlit will print a local address, normally `http://localhost:8501`, which can be opened in a web browser.

## Run the R/Shiny application

### 1. Install the required R packages

Run the following once in R:

```r
install.packages(c(
  "shiny",
  "bs4Dash",
  "fresh",
  "plotly",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "networkD3",
  "leaflet",
  "DT",
  "scales",
  "shinycssloaders"
))
```

### 2. Add the data file

Copy the model output into the repository root and name it `reporting.mif`.

### 3. Start the dashboard

Open the repository as the working directory in R or RStudio and run:

```r
shiny::runApp("app.R")
```

Alternatively, open `app.R` in RStudio and select **Run App**.

The R application creates `dashboard_cache.rds` after parsing the input for the first time. It reuses the cache on later starts while the cache is newer than `reporting.mif`. Updating the MIF file automatically triggers a rebuild.

## Using the dashboard

1. Select an analysis page from the sidebar.
2. Choose one or more scenarios and regions.
3. Set a year or year range, depending on the selected view.
4. Use page-specific filters such as fuel, sector, technology, gas, or indicator.
5. Hover over visualizations to inspect exact values and categories.
6. Open **Data Table** to review the underlying records. In the Streamlit version, use **Download filtered CSV** to export the current selection.

## Troubleshooting

### `reporting.mif` was not found

Confirm that the input file is named exactly `reporting.mif` and is stored in the same directory as `streamlit_app.py` and `app.R`.

### Missing IAMC columns

The input must include `Model`, `Scenario`, `Region`, `Variable`, and `Unit`, followed by one or more four-digit year columns.

### A chart is empty

The selected combination may not exist in the input data, or its variables may not use the expected IAMC hierarchy. Check the **Data Table** view and compare the variable names with the examples above.

### The R dashboard displays old data

Restart the application after updating `reporting.mif`. If necessary, delete `dashboard_cache.rds`; the application will recreate it on the next start.

## Repository structure

- `streamlit_app.py` - Streamlit interface and Plotly visualizations
- `iamc_loader.py` - IAMC/MIF parser and dataset transformations
- `app.R` - R/Shiny implementation, including data processing and UI
- `requirements.txt` - Python dependencies
- `test.mif` - Example IAMC/MIF file structure

