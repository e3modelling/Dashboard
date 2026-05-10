# =============================================================================
# OPEN-PROM Results Dashboard  —  bs4Dash edition
# =============================================================================

library(shiny)
library(bs4Dash)
library(fresh)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(networkD3)
library(leaflet)
library(DT)
library(scales)
library(shinycssloaders)

SCRIPT_DIR <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) getwd()
)
MIF_FILE <- file.path(SCRIPT_DIR, "reporting.mif")

# =============================================================================
# GLOBALS
# =============================================================================

CARRIERS <- c(
  "Biodiesel", "Biofuels", "Biogasoline", "Biokerosene", "Biomass and Waste",
  "Coal", "Crude Oil and Feedstocks", "Diesel Oil", "Electricity", "Ethanol",
  "Fossil Liquids", "Gas", "Gasoline", "Geothermal and other renewable sources",
  "Hard Coal; Coke and Other Solids", "Heat", "Hydro", "Hydrogen", "Kerosene",
  "Lignite", "Liquefied Petroleum Gas", "Methanol", "Natural Gas", "Nuclear",
  "Oil", "Other fuels", "Other Gases", "Other Liquids", "Renewables",
  "Residual Fuel Oil", "Solar", "Solids", "Steam", "Wind"
)
REN_SOURCES <- c("Hydro", "Wind", "Solar", "Biofuels",
                 "Geothermal and other renewable sources")

# =============================================================================
# READ MIF
# =============================================================================

read_mif <- function(path) {
  raw <- read_delim(path, delim = ";", quote = '"', escape_double = TRUE,
    col_types = cols(.default = col_character()), trim_ws = TRUE,
    locale = locale(encoding = "UTF-8"))
  names(raw)[1] <- str_remove(names(raw)[1], "^\xef\xbb\xbf")
  names(raw)    <- tolower(str_trim(names(raw)))
  year_cols <- names(raw)[grepl("^\\d{4}$", names(raw))]
  raw %>%
    pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(year), value = suppressWarnings(as.numeric(value))) %>%
    filter(!is.na(value))
}

# =============================================================================
# HELPERS
# =============================================================================

seg <- function(variable, n)
  sapply(strsplit(variable, "\\|"), function(x) if (length(x) >= n) x[n] else NA_character_)

# =============================================================================
# DERIVE SUB-DATASETS
# =============================================================================

derive_datasets <- function(long) {
  emissions <- long %>%
    filter(str_starts(variable, "Emissions\\|")) %>%
    mutate(gas = seg(variable, 2), domain = seg(variable, 3),
           side = seg(variable, 4), sector = seg(variable, 5))

  gross_emissions <- long %>%
    filter(str_starts(variable, "Gross Emissions\\|")) %>%
    mutate(gas = seg(variable, 2), domain = seg(variable, 3),
           side = seg(variable, 4), sector = seg(variable, 5))

  primary_energy <- long %>%
    filter(str_starts(variable, "Primary Energy")) %>%
    mutate(fuel = if_else(variable == "Primary Energy", "Total",
                          str_remove(variable, "^Primary Energy\\|"))) %>%
    filter(!str_detect(fuel, "\\|"))

  gic <- long %>%
    filter(str_starts(variable, "Gross Inland Consumption")) %>%
    mutate(fuel = if_else(variable == "Gross Inland Consumption", "Total",
                          str_remove(variable, "^Gross Inland Consumption\\|"))) %>%
    filter(!str_detect(fuel, "\\|"))

  secondary_elec <- long %>%
    filter(str_starts(variable, "Secondary Energy\\|Electricity\\|"),
           variable != "Secondary Energy|Electricity",
           variable != "Secondary Energy|Electricity|Demand") %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Electricity\\|")) %>%
    filter(!str_detect(source, "\\|"))

  secondary_heat <- long %>%
    filter(str_starts(variable, "Secondary Energy\\|Heat\\|")) %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Heat\\|")) %>%
    filter(!str_detect(source, "\\|"))

  secondary_hydrogen <- long %>%
    filter(str_starts(variable, "Secondary Energy\\|Hydrogen\\|")) %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Hydrogen\\|")) %>%
    filter(!str_detect(source, "\\|"))

  final_energy_sector <- long %>%
    filter(str_starts(variable, "Final Energy\\|"),
           !str_starts(variable, "Final Energy w/o")) %>%
    mutate(sector = str_remove(variable, "^Final Energy\\|")) %>%
    filter(!str_detect(sector, "\\|"), !sector %in% c("w/o bunkers", CARRIERS))

  final_energy_carrier <- long %>%
    filter(str_starts(variable, "Final Energy\\|"),
           !str_starts(variable, "Final Energy w/o")) %>%
    mutate(carrier = str_remove(variable, "^Final Energy\\|")) %>%
    filter(!str_detect(carrier, "\\|"), carrier %in% CARRIERS)

  capacity <- long %>%
    filter(str_starts(variable, "Capacity\\|Electricity\\|")) %>%
    mutate(tech = str_remove(variable, "^Capacity\\|Electricity\\|")) %>%
    filter(!str_detect(tech, "\\|"))

  capacity_additions <- long %>%
    filter(str_starts(variable, "Capacity Additions\\|Electricity\\|")) %>%
    mutate(tech = str_remove(variable, "^Capacity Additions\\|Electricity\\|")) %>%
    filter(!str_detect(tech, "\\|"))

  ccs <- long %>%
    filter(str_starts(variable, "Carbon Capture\\|")) %>%
    mutate(category = seg(variable, 2), sub = seg(variable, 3))

  indicators <- long %>%
    filter(variable %in% c("GDP|PPP", "Population", "Price|Carbon") |
             str_starts(variable, "Price\\|Carbon"))

  flows    <- derive_flows(primary_energy, final_energy_sector)
  regional <- derive_regional(emissions)

  list(raw = long, emissions = emissions, gross_emissions = gross_emissions,
       primary_energy = primary_energy, gic = gic,
       secondary_elec = secondary_elec, secondary_heat = secondary_heat,
       secondary_hydrogen = secondary_hydrogen,
       final_energy_sector = final_energy_sector,
       final_energy_carrier = final_energy_carrier,
       capacity = capacity, capacity_additions = capacity_additions,
       ccs = ccs, indicators = indicators, flows = flows, regional = regional)
}

derive_flows <- function(pe, fe) {
  src <- pe %>% filter(fuel != "Total") %>%
    mutate(source = fuel, target = "Primary Energy") %>%
    select(scenario, region, year, source, target, value) %>% filter(value > 0)
  tgt <- fe %>% filter(!sector %in% c("Total", "w/o bunkers")) %>%
    mutate(source = "Primary Energy", target = sector) %>%
    select(scenario, region, year, source, target, value) %>% filter(value > 0)
  bind_rows(src, tgt) %>%
    group_by(scenario, region, year, source, target) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

derive_regional <- function(emissions) {
  centroids <- tribble(
    ~region, ~lat,   ~lon,
    "CHA", 35.9, 104.2, "EUR", 50.1,  10.4, "EU",  50.1,  10.4,
    "USA", 38.9, -77.0, "IND", 20.6,  78.9, "JPN", 36.2, 138.3,
    "RUS", 61.5,  90.0, "BRA",-14.2, -51.9, "AFR", -8.8,  34.5,
    "MEA", 29.3,  42.5, "OAS", 15.0, 100.0, "LAM",-15.0, -60.0,
    "World", 20.0, 10.0
  )
  emissions %>%
    filter(gas == "CO2", is.na(domain)) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(centroids, by = "region") %>% filter(!is.na(lat))
}

# =============================================================================
# LOAD DATA  —  runs once at startup, cached to .rds for fast restarts
# =============================================================================

RDS_CACHE <- file.path(SCRIPT_DIR, "dashboard_cache.rds")

APP_DATA <- local({
  # Use RDS cache if it exists and is newer than the MIF file
  if (file.exists(RDS_CACHE) && file.exists(MIF_FILE) &&
      file.mtime(RDS_CACHE) >= file.mtime(MIF_FILE)) {
    cat("Loading from cache (dashboard_cache.rds)...\n")
    return(readRDS(RDS_CACHE))
  }
  if (!file.exists(MIF_FILE)) { cat("ERROR: File not found:", MIF_FILE, "\n"); return(NULL) }
  cat("Parsing MIF file (first run or file updated)...\n")
  long <- tryCatch(read_mif(MIF_FILE),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(long) || nrow(long) == 0) return(NULL)
  d <- tryCatch(derive_datasets(long),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(d)) {
    saveRDS(d, RDS_CACHE)
    cat("Cache saved to dashboard_cache.rds\n")
  }
  d
})

# =============================================================================
# COLOUR PALETTES & CHART THEMES
# =============================================================================

FUEL_COLS <- c(
  "Coal" = "#3d3d3d", "Lignite" = "#555555",
  "Oil" = "#6B4226", "Crude Oil and Feedstocks" = "#7a5230",
  "Gas" = "#A8DADC", "Natural Gas" = "#90cfd2",
  "Nuclear" = "#E9C46A", "Wind" = "#457B9D", "Solar" = "#F4D35E",
  "Hydro" = "#1D3557", "Biofuels" = "#588157", "Biomass and Waste" = "#6aad65",
  "Geothermal and other renewable sources" = "#2A9D8F",
  "Hydrogen" = "#90E0EF", "Electricity" = "#FFD166", "Heat" = "#EF476F",
  "Other fuels" = "#aaaaaa", "Other Gases" = "#bbbbbb",
  "Other Liquids" = "#cccccc", "Renewables" = "#43aa8b", "Total" = "#E8EAF0"
)
GAS_COLS <- c(
  "CO2" = "#E63946", "CH4" = "#F4A261", "N2O" = "#2A9D8F",
  "Kyoto Gases" = "#9B5DE5", "F-gases" = "#457B9D",
  "HFC" = "#E9C46A", "SF6" = "#A8DADC", "PFC" = "#588157"
)
TECH_COLS <- c(
  "Coal" = "#3d3d3d", "Coal|w/ CCS" = "#666666", "Coal|w/o CCS" = "#444444",
  "Gas" = "#A8DADC", "Gas|w/ CCS" = "#c8eaec", "Gas|w/o CCS" = "#90cfd2",
  "Oil" = "#6B4226", "Oil|w/o CCS" = "#855a3a", "Nuclear" = "#E9C46A",
  "Wind" = "#457B9D", "Hydro" = "#1D3557", "Solar" = "#F4D35E",
  "Biofuels" = "#588157", "Biofuels|w/ CCS" = "#6aad65",
  "Biofuels|w/o CCS" = "#4d7a4a",
  "Geothermal and other renewable sources" = "#2A9D8F", "Hydrogen" = "#90E0EF"
)

dtheme <- function() {
  theme_minimal(base_family = "sans") +
  theme(
    plot.background   = element_rect(fill = "transparent", colour = NA),
    panel.background  = element_rect(fill = "transparent", colour = NA),
    text              = element_text(colour = "#E8EAF0"),
    axis.text         = element_text(colour = "#9a9db0"),
    strip.text        = element_text(colour = "#2A9D8F", face = "bold"),
    panel.grid.major  = element_line(colour = "#2a2d3a"),
    panel.grid.minor  = element_blank(),
    legend.position   = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key        = element_rect(fill = "transparent", colour = NA)
  )
}

dtheme_sm <- function() {
  dtheme() + theme(
    legend.position = "right",
    legend.key.size = unit(0.32, "cm"),
    legend.text     = element_text(size = 7),
    axis.text       = element_text(size = 7),
    plot.margin     = margin(2, 2, 2, 2)
  )
}

ptheme <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font   = list(color = "#E8EAF0", family = "sans"),
    xaxis  = list(gridcolor = "#2a2d3a", zerolinecolor = "#2a2d3a"),
    yaxis  = list(gridcolor = "#2a2d3a", zerolinecolor = "#2a2d3a"),
    legend = list(bgcolor = "rgba(22,24,32,0.8)", bordercolor = "#2a2d3a", borderwidth = 1),
    margin = list(t = 20, b = 40, l = 60, r = 20)
  )
}

# =============================================================================
# FRESH THEME  (AdminLTE colour overrides)
# =============================================================================

app_theme <- create_theme(
  adminlte_color(
    light_blue = "#2A9D8F",
    teal       = "#2A9D8F"
  ),
  adminlte_sidebar(
    dark_bg          = "#161820",
    dark_hover_bg    = "#1e2130",
    dark_color       = "#9a9db0",
    dark_hover_color = "#E8EAF0"
  ),
  adminlte_global(
    content_bg = "#0F1117",
    box_bg     = "#161820",
    info_box_bg = "#161820"
  )
)

# Helper: compact year slider
ysl <- function(id, lo = 2020, hi = 2060)
  sliderInput(id, NULL, 2010, 2100, c(lo, hi), step = 1, sep = "", width = "100%")

# Helper: collapsible filter card that spans full width
frow <- function(...) {
  fluidRow(
    bs4Card(
      width = 12, title = tagList(icon("sliders"), " Filters"),
      status = "primary", solidHeader = FALSE,
      collapsible = TRUE, collapsed = FALSE, elevation = 1,
      ...
    )
  )
}

# =============================================================================
# CUSTOM CSS
# =============================================================================

CUSTOM_CSS <- tags$style(HTML("
  /* Body & sidebar */
  body, .wrapper { background-color: #0F1117 !important; }
  .main-sidebar  { background-color: #161820 !important; }
  .sidebar-menu > li.header {
    color: #2A9D8F !important;
    font-size: 0.68rem !important;
    letter-spacing: .08em;
    padding: 10px 10px 4px;
  }
  /* Cards */
  .card { background-color: #161820 !important; border: 1px solid #2a2d3a !important; }
  .card-header { background-color: #1a1d2e !important; border-bottom: 1px solid #2a2d3a !important;
                 color: #E8EAF0 !important; font-size: .82rem; }
  .card-body { background-color: #161820 !important; }
  /* Value boxes */
  .small-box { border-radius: 6px !important; }
  .small-box h3 { font-size: 1.6rem !important; }
  /* Navbar */
  .main-header .navbar { background-color: #0F1117 !important; border-bottom: 1px solid #2a2d3a; }
  .main-header .brand-link { background-color: #161820 !important;
    border-bottom: 1px solid #2a2d3a !important; font-size: .95rem; }
  /* Scrollbar */
  ::-webkit-scrollbar { width: 5px; height: 5px; }
  ::-webkit-scrollbar-track { background: #0F1117; }
  ::-webkit-scrollbar-thumb { background: #2a2d3a; border-radius: 3px; }
  /* Select inputs */
  .selectize-control .selectize-input { background: #1a1d2e !important;
    border-color: #2a2d3a !important; color: #E8EAF0 !important; }
  .selectize-dropdown { background: #1a1d2e !important; border-color: #2a2d3a !important; }
  .selectize-dropdown .option { color: #E8EAF0 !important; }
  .selectize-dropdown .option:hover,
  .selectize-dropdown .active { background: #2a2d3a !important; }
  /* Sliders */
  .irs--shiny .irs-bar { background: #2A9D8F !important; border-color: #2A9D8F !important; }
  .irs--shiny .irs-handle { background: #2A9D8F !important; border-color: #2A9D8F !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #2A9D8F !important; }
  /* Table */
  .dataTables_wrapper { color: #E8EAF0 !important; }
  table.dataTable thead th { background: #1a1d2e !important; color: #2A9D8F !important;
    border-bottom: 1px solid #2a2d3a !important; }
  table.dataTable tbody tr { background: #161820 !important; color: #E8EAF0 !important; }
  table.dataTable tbody tr:hover { background: #1e2130 !important; }
  /* Checkbox */
  .checkbox label { color: #9a9db0 !important; }
  /* Radio */
  .radio label { color: #9a9db0 !important; }
  /* Content padding */
  .content-wrapper { padding: 12px; }
"))

# =============================================================================
# UI
# =============================================================================

ui <- bs4DashPage(
  title       = "OPEN-PROM Dashboard",
  freshTheme  = app_theme,
  dark        = TRUE,
  help        = FALSE,
  scrollToTop = TRUE,

  controlbar = bs4DashControlbar(
    skin = "dark",
    title = "App Settings",
    width = 300,
    pinned = FALSE,
    bs4Card(
      title = "Global Preferences",
      width = 12,
      elevation = 0,
      sliderInput("global_yr", "Default Year Range", 2010, 2100, c(2020, 2060), step = 1, sep = "")
    )
  ),

  header = bs4DashNavbar(
    skin  = "dark",
    title = bs4DashBrand(
      title = tags$span(
        style = "font-family:monospace;color:#2A9D8F;letter-spacing:.06em;font-weight:700;",
        "OPEN-PROM  ",
        tags$small(style = "font-size:.6rem;color:#9a9db0;font-weight:400;", "Results Explorer")
      ),
      color = "primary",
      href  = "#"
    )
  ),

  sidebar = bs4DashSidebar(
    skin   = "dark",
    status = "primary",
    bs4SidebarMenu(
      id = "sidebar_menu",
      bs4SidebarMenuItem("Overview",          tabName = "overview",   icon = icon("table-cells-large")),

      sidebarHeader("Regional Analysis"),
      bs4SidebarMenuItem("Regional Dashboard", tabName = "regional",  icon = icon("map-location-dot")),
      bs4SidebarMenuItem("Heat Map",           tabName = "heatmap",   icon = icon("grip")),
      bs4SidebarMenuItem("Scatter Plot",       tabName = "scatter",   icon = icon("circle-dot")),

      sidebarHeader("Energy"),
      bs4SidebarMenuItem("Primary Energy",     tabName = "pe",        icon = icon("bolt")),
      bs4SidebarMenuItem("Electricity Mix",    tabName = "elec",      icon = icon("plug")),
      bs4SidebarMenuItem("Final Energy",       tabName = "fe",        icon = icon("industry")),
      bs4SidebarMenuItem("Energy Flows",       tabName = "sankey",    icon = icon("water")),

      sidebarHeader("Capacity & CCS"),
      bs4SidebarMenuItem("Power Capacity",     tabName = "capacity",  icon = icon("tower-broadcast")),
      bs4SidebarMenuItem("Carbon Capture",     tabName = "ccs",       icon = icon("cloud-arrow-down")),

      sidebarHeader("Emissions"),
      bs4SidebarMenuItem("Emissions",          tabName = "emissions", icon = icon("chart-line")),
      bs4SidebarMenuItem("Regional Map",       tabName = "map",       icon = icon("globe")),

      sidebarHeader("Scenarios & Data"),
      bs4SidebarMenuItem("Scenario Comparison",tabName = "scenario",  icon = icon("chart-bar")),
      bs4SidebarMenuItem("Indicators",         tabName = "indicators",icon = icon("gauge")),
      bs4SidebarMenuItem("Data Table",         tabName = "datatable", icon = icon("table"))
    )
  ),

  body = bs4DashBody(
    tags$head(CUSTOM_CSS),
    tabItems(

      # ── OVERVIEW ────────────────────────────────────────────────────────────
      tabItem("overview",
        frow(
          column(3, selectInput("ov_sc",  "Scenario",   NULL)),
          column(4, selectInput("ov_reg", "Region(s)",  NULL, multiple = TRUE)),
          column(5, ysl("ov_yr"))
        ),
        fluidRow(
          bs4ValueBox(textOutput("vb_em"),  "CO2 Emissions",    icon("smog"),            color = "danger",  width = 3, footer = "end-year sum"),
          bs4ValueBox(textOutput("vb_pe"),  "Primary Energy",   icon("bolt"),            color = "primary", width = 3, footer = "end-year sum"),
          bs4ValueBox(textOutput("vb_ren"), "Renewable Elec.",  icon("leaf"),            color = "success", width = 3, footer = "share of electricity"),
          bs4ValueBox(textOutput("vb_cap"), "Power Capacity",   icon("tower-broadcast"), color = "info",    width = 3, footer = "end-year total GW")
        ),
        fluidRow(
          bs4Card(width = 6, title = "CO2 Emissions",        maximizable = TRUE, collapsible = FALSE, plotlyOutput("plot_ov_em", height = "300px")),
          bs4Card(width = 6, title = "Primary Energy Mix",   maximizable = TRUE, collapsible = FALSE, plotlyOutput("plot_ov_pe", height = "300px"))
        ),
        fluidRow(
          bs4TabCard(width = 12, title = "Energy Mix Breakdown", maximizable = TRUE, collapsible = FALSE, status = "primary",
            tabPanel("Electricity", plotlyOutput("plot_ov_se", height = "300px")),
            tabPanel("Final Energy (Sector)", plotlyOutput("plot_ov_fe", height = "300px"))
          )
        )
      ),

      # ── REGIONAL DASHBOARD ──────────────────────────────────────────────────
      tabItem("regional",
        frow(
          column(3, selectInput("rd_sc",  "Scenario", NULL)),
          column(4, selectInput("rd_reg", "Region",   NULL)),
          column(5, ysl("rd_yr"))
        ),
        fluidRow(
          bs4ValueBox(textOutput("rd_vb_co2"), "CO2 Emissions",   icon("smog"),            color = "danger",  width = 3, footer = "end-year"),
          bs4ValueBox(textOutput("rd_vb_pe"),  "Primary Energy",  icon("bolt"),            color = "primary", width = 3, footer = "end-year"),
          bs4ValueBoxOutput("rd_vb_ren", width = 3),
          bs4ValueBox(textOutput("rd_vb_cap"), "Power Capacity",  icon("tower-broadcast"), color = "info",    width = 3, footer = "end-year GW")
        ),
        fluidRow(
          bs4Card(width = 6, title = "CO2 Emissions Trend",    maximizable = TRUE, collapsible = FALSE, plotlyOutput("rd_co2", height = "260px")),
          bs4Card(width = 6, title = "Primary Energy Mix",     maximizable = TRUE, collapsible = FALSE, plotlyOutput("rd_pe",  height = "260px"))
        ),
        fluidRow(
          bs4Card(width = 6, title = "Electricity Generation", maximizable = TRUE, collapsible = FALSE, plotlyOutput("rd_se",  height = "260px")),
          bs4Card(width = 6, title = "Final Energy by Sector", maximizable = TRUE, collapsible = FALSE, plotlyOutput("rd_fe",  height = "260px"))
        ),
        fluidRow(
          bs4Card(width = 6, title = "Power Capacity",         maximizable = TRUE, collapsible = FALSE, plotlyOutput("rd_cap", height = "260px")),
          bs4Card(width = 6, title = "Key Indicators",         maximizable = TRUE, collapsible = FALSE, 
                  ribbon = bs4Ribbon(text = "Updated", color = "teal"),
                  plotlyOutput("rd_ind", height = "260px"))
        )
      ),

      # ── HEAT MAP ────────────────────────────────────────────────────────────
      tabItem("heatmap",
        frow(
          column(3, selectInput("hm_sc",  "Scenario", NULL)),
          column(4, selectInput("hm_var", "Variable", NULL)),
          column(5, ysl("hm_yr"))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Region × Year Intensity", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_hm", height = "520px"))
        )
      ),

      # ── SCATTER PLOT ────────────────────────────────────────────────────────
      tabItem("scatter",
        frow(
          column(2, selectInput("sp_sc",    "Scenario(s)", NULL, multiple = TRUE)),
          column(3, selectInput("sp_xvar",  "X Variable",  NULL)),
          column(3, selectInput("sp_yvar",  "Y Variable",  NULL)),
          column(2, selectInput("sp_reg",   "Region(s)",   NULL, multiple = TRUE)),
          column(2, selectInput("sp_yr_pt", "Year",        NULL))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Cross-Variable Scatter", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_sp", height = "520px"))
        )
      ),

      # ── PRIMARY ENERGY ──────────────────────────────────────────────────────
      tabItem("pe",
        frow(
          column(3, selectInput("pe_sc",  "Scenario",   NULL)),
          column(3, selectInput("pe_reg", "Region(s)",  NULL, multiple = TRUE)),
          column(4, ysl("pe_yr")),
          column(2, checkboxInput("pe_gic", "Gross Inland Consumption", FALSE))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Primary Energy Mix", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_pe", height = "500px"))
        )
      ),

      # ── ELECTRICITY MIX ─────────────────────────────────────────────────────
      tabItem("elec",
        frow(
          column(3, selectInput("se_sc",  "Scenario",  NULL)),
          column(4, selectInput("se_reg", "Region(s)", NULL, multiple = TRUE)),
          column(5, ysl("se_yr"))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Electricity Generation Mix", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_se", height = "500px"))
        )
      ),

      # ── FINAL ENERGY ────────────────────────────────────────────────────────
      tabItem("fe",
        frow(
          column(3, selectInput("fe_sc",    "Scenario(s)", NULL, multiple = TRUE)),
          column(3, selectInput("fe_reg",   "Region(s)",   NULL, multiple = TRUE)),
          column(2, selectInput("fe_yr_pt", "Year",        NULL)),
          column(4, radioButtons("fe_mode", "Group by",
                                 c("Sector" = "sector", "Carrier" = "carrier"), inline = TRUE))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Final Energy", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_fe", height = "500px"))
        )
      ),

      # ── ENERGY FLOWS ────────────────────────────────────────────────────────
      tabItem("sankey",
        frow(
          column(4, selectInput("sk_sc",  "Scenario", NULL)),
          column(4, selectInput("sk_reg", "Region",   NULL)),
          column(4, selectInput("sk_yr",  "Year",     NULL))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Primary → Final Energy Flows", maximizable = TRUE, collapsible = FALSE,
                  shinycssloaders::withSpinner(sankeyNetworkOutput("plot_sk", height = "500px"), type = 8, color = "#2A9D8F"))
        )
      ),

      # ── POWER CAPACITY ──────────────────────────────────────────────────────
      tabItem("capacity",
        frow(
          column(3, selectInput("cap_sc",  "Scenario(s)", NULL, multiple = TRUE)),
          column(3, selectInput("cap_reg", "Region(s)",   NULL, multiple = TRUE)),
          column(4, ysl("cap_yr")),
          column(2, radioButtons("cap_mode", "Show",
                                 c("Installed" = "cap", "Additions" = "add"), inline = TRUE))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Power Capacity", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_cap", height = "500px"))
        )
      ),

      # ── CARBON CAPTURE ──────────────────────────────────────────────────────
      tabItem("ccs",
        frow(
          column(3, selectInput("ccs_sc",  "Scenario(s)", NULL, multiple = TRUE)),
          column(3, selectInput("ccs_reg", "Region(s)",   NULL, multiple = TRUE)),
          column(3, selectInput("ccs_cat", "Category",    NULL, multiple = TRUE)),
          column(3, ysl("ccs_yr"))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Carbon Capture & Storage", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_ccs", height = "500px"))
        )
      ),

      # ── EMISSIONS ───────────────────────────────────────────────────────────
      tabItem("emissions",
        frow(
          column(2, selectInput("em_sc",  "Scenario(s)", NULL, multiple = TRUE)),
          column(2, selectInput("em_reg", "Region(s)",   NULL, multiple = TRUE)),
          column(2, selectInput("em_gas", "Gas(es)",     NULL, multiple = TRUE)),
          column(2, selectInput("em_dom", "Domain",      NULL, multiple = TRUE)),
          column(3, ysl("em_yr")),
          column(1, tags$div(style = "margin-top:6px;",
            checkboxInput("em_gross", "Gross", FALSE),
            checkboxInput("em_stack", "Stack", FALSE)
          ))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Emissions", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_em", height = "500px"))
        )
      ),

      # ── REGIONAL MAP ────────────────────────────────────────────────────────
      tabItem("map",
        frow(
          column(6, selectInput("map_sc", "Scenario", NULL)),
          column(6, selectInput("map_yr", "Year",     NULL))
        ),
        fluidRow(
          bs4Card(width = 12, title = "CO2 Emissions — Regional Map", maximizable = TRUE, collapsible = FALSE,
                  leafletOutput("plot_map", height = "500px"))
        )
      ),

      # ── SCENARIO COMPARISON ─────────────────────────────────────────────────
      tabItem("scenario",
        frow(
          column(4, selectInput("sc_var",   "Variable",  NULL)),
          column(4, selectInput("sc_reg",   "Region(s)", NULL, multiple = TRUE)),
          column(4, selectInput("sc_yr_pt", "Year",      NULL))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Scenario Comparison", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_sc", height = "500px"))
        )
      ),

      # ── INDICATORS ──────────────────────────────────────────────────────────
      tabItem("indicators",
        frow(
          column(3, selectInput("ind_var", "Variable",    NULL)),
          column(3, selectInput("ind_sc",  "Scenario(s)", NULL, multiple = TRUE)),
          column(3, selectInput("ind_reg", "Region(s)",   NULL, multiple = TRUE)),
          column(3, ysl("ind_yr"))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Indicators", maximizable = TRUE, collapsible = FALSE,
                  plotlyOutput("plot_ind", height = "500px"))
        )
      ),

      # ── DATA TABLE ──────────────────────────────────────────────────────────
      tabItem("datatable",
        frow(
          column(4, selectInput("tbl_sc",  "Scenario(s)", NULL, multiple = TRUE)),
          column(4, selectInput("tbl_reg", "Region(s)",   NULL, multiple = TRUE)),
          column(4, selectInput("tbl_var", "Variable(s)", NULL, multiple = TRUE))
        ),
        fluidRow(
          bs4Card(width = 12, title = "Raw Data", collapsible = FALSE,
                  DTOutput("data_table"))
        )
      )
    )
  )
)

# =============================================================================
# SERVER  (logic unchanged from previous version)
# =============================================================================

server <- function(input, output, session) {

  dat <- reactive({
    validate(need(!is.null(APP_DATA),
      paste0("Cannot load '", MIF_FILE, "'. Place reporting.mif next to app.R.")))
    APP_DATA
  })

  # ── POPULATE DROPDOWNS ──────────────────────────────────────────────────────
  observe({
    d   <- dat()
    sc  <- sort(unique(d$raw$scenario))
    reg <- sort(unique(d$raw$region))
    yr  <- sort(unique(d$raw$year))
    yl  <- min(yr); yh <- max(yr)
    ylo <- max(yl, 2020); yhi <- min(yh, 2060)
    yp  <- min(yr[yr >= 2030], na.rm = TRUE)

    gases    <- sort(unique(d$emissions$gas))
    domains  <- sort(na.omit(unique(d$emissions$domain)))
    ccs_cat  <- sort(unique(d$ccs$category))
    ind_vars <- sort(unique(d$indicators$variable))
    all_vars <- sort(unique(d$raw$variable))

    upd <- function(id, ch, sel = ch) updateSelectInput(session, id, choices = ch, selected = sel)
    usl <- function(id) updateSliderInput(session, id, min = yl, max = yh, value = c(ylo, yhi))

    rd_def <- if ("EU" %in% reg) "EU" else if ("World" %in% reg) "World" else reg[1]

    upd("ov_sc",  sc,  sc[1]);  upd("ov_reg", reg, if ("World" %in% reg) "World" else reg[1]); usl("ov_yr")
    upd("rd_sc",  sc,  sc[1]);  upd("rd_reg", reg, rd_def); usl("rd_yr")
    upd("hm_sc",  sc,  sc[1]);  upd("hm_var", all_vars, if ("Emissions|CO2" %in% all_vars) "Emissions|CO2" else all_vars[1]); usl("hm_yr")
    upd("sp_sc",  sc,  sc);
    upd("sp_xvar", all_vars, if ("GDP|PPP" %in% all_vars) "GDP|PPP" else all_vars[1])
    upd("sp_yvar", all_vars, if ("Emissions|CO2" %in% all_vars) "Emissions|CO2" else all_vars[min(2, length(all_vars))])
    upd("sp_reg", reg, reg);    upd("sp_yr_pt", yr, yp)
    upd("em_sc",  sc,  sc);     upd("em_reg", reg, reg)
    upd("em_gas", gases, intersect(c("CO2", "Kyoto Gases"), gases))
    upd("em_dom", c("All", domains), "All"); usl("em_yr")
    upd("pe_sc",  sc,  sc[1]);  upd("pe_reg", reg, reg); usl("pe_yr")
    upd("se_sc",  sc,  sc[1]);  upd("se_reg", reg, reg); usl("se_yr")
    upd("fe_sc",  sc,  sc);     upd("fe_reg", reg, reg); upd("fe_yr_pt", yr, yp)
    upd("cap_sc", sc,  sc);     upd("cap_reg", reg, reg); usl("cap_yr")
    upd("ccs_sc", sc,  sc);     upd("ccs_reg", reg, reg); upd("ccs_cat", ccs_cat, ccs_cat); usl("ccs_yr")
    upd("ind_var", ind_vars, ind_vars[1]); upd("ind_sc", sc, sc); upd("ind_reg", reg, reg); usl("ind_yr")
    upd("sc_var",  all_vars, if ("Emissions|CO2" %in% all_vars) "Emissions|CO2" else all_vars[1])
    upd("sc_reg",  reg, reg);   upd("sc_yr_pt", yr, yp)
    upd("sk_sc",  sc,  sc[1]);  upd("sk_reg", reg, reg[1]); upd("sk_yr", yr, yp)
    upd("map_sc", sc,  sc[1]);  upd("map_yr", yr, yp)
    upd("tbl_sc", sc,  sc);     upd("tbl_reg", reg, reg); upd("tbl_var", all_vars, NULL)
  })

  # ── OVERVIEW KPIs ───────────────────────────────────────────────────────────
  output$vb_em <- renderText({
    d <- dat(); req(input$ov_sc, input$ov_reg, input$ov_yr)
    val <- d$emissions %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg,
             gas == "CO2", is.na(domain), year == input$ov_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val)) "N/A" else paste0(round(val, 1))
  })
  output$vb_pe <- renderText({
    d <- dat(); req(input$ov_sc, input$ov_reg, input$ov_yr)
    val <- d$primary_energy %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg,
             fuel == "Total", year == input$ov_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val)) "N/A" else paste0(round(val, 1))
  })
  output$vb_ren <- renderbs4ValueBox({
    d <- dat(); req(input$ov_sc, input$ov_reg, input$ov_yr)
    df <- d$secondary_elec %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg, year == input$ov_yr[2])
    tot <- sum(df$value, na.rm = TRUE); ren <- sum(df$value[df$source %in% REN_SOURCES], na.rm = TRUE)
    val_pct <- if (!tot || is.na(tot)) 0 else round(100 * ren / tot, 1)
    val_text <- if (!tot || is.na(tot)) "N/A" else paste0(val_pct, "%")
    
    bs4ValueBox(
      value = val_text,
      subtitle = "Renewable Elec.",
      icon = icon("leaf"),
      color = "success",
      footer = "share of electricity",
      width = NULL
    )
  })
  output$vb_cap <- renderText({
    d <- dat(); req(input$ov_sc, input$ov_reg, input$ov_yr)
    val <- d$capacity %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg, year == input$ov_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val) || val == 0) "N/A" else paste0(round(val, 0), " GW")
  })

  output$plot_ov_em <- renderPlotly({
    d <- dat(); req(input$ov_sc, input$ov_reg)
    df <- d$emissions %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg, gas == "CO2", is.na(domain),
             year >= input$ov_yr[1], year <= input$ov_yr[2]) %>%
      group_by(year, unit) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value)) +
      geom_area(fill = "#E63946", alpha = 0.15) + geom_line(colour = "#E63946", linewidth = 1.2) +
      labs(x = NULL, y = unique(df$unit)[1]) + dtheme()
    ggplotly(p) %>% ptheme()
  })
  output$plot_ov_pe <- renderPlotly({
    d <- dat(); req(input$ov_sc, input$ov_reg)
    df <- d$primary_energy %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg, fuel != "Total",
             year >= input$ov_yr[1], year <= input$ov_yr[2]) %>%
      group_by(fuel, year, unit) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value, fill = fuel, text = paste0(fuel, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      scale_fill_manual(values = FUEL_COLS, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$plot_ov_se <- renderPlotly({
    d <- dat(); req(input$ov_sc, input$ov_reg)
    df <- d$secondary_elec %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg,
             year >= input$ov_yr[1], year <= input$ov_yr[2]) %>%
      group_by(source, year, unit) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value, fill = source, text = paste0(source, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$plot_ov_fe <- renderPlotly({
    d <- dat(); req(input$ov_sc, input$ov_reg)
    df <- d$final_energy_sector %>%
      filter(scenario == input$ov_sc, region %in% input$ov_reg,
             year >= input$ov_yr[1], year <= input$ov_yr[2]) %>%
      group_by(sector, year, unit) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value, fill = sector, text = paste0(sector, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── REGIONAL DASHBOARD KPIs ─────────────────────────────────────────────────
  output$rd_vb_co2 <- renderText({
    d <- dat(); req(input$rd_sc, input$rd_reg, input$rd_yr)
    val <- d$emissions %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             gas == "CO2", is.na(domain), year == input$rd_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val)) "N/A" else paste0(round(val, 1), " Mt")
  })
  output$rd_vb_pe <- renderText({
    d <- dat(); req(input$rd_sc, input$rd_reg, input$rd_yr)
    val <- d$primary_energy %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             fuel == "Total", year == input$rd_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val)) "N/A" else paste0(round(val, 1))
  })
  output$rd_vb_ren <- renderbs4ValueBox({
    d <- dat(); req(input$rd_sc, input$rd_reg, input$rd_yr)
    df <- d$secondary_elec %>%
      filter(scenario == input$rd_sc, region == input$rd_reg, year == input$rd_yr[2])
    tot <- sum(df$value, na.rm = TRUE); ren <- sum(df$value[df$source %in% REN_SOURCES], na.rm = TRUE)
    val_pct <- if (!tot || is.na(tot)) 0 else round(100 * ren / tot, 1)
    val_text <- if (!tot || is.na(tot)) "N/A" else paste0(val_pct, "%")
    
    bs4ValueBox(
      value = val_text,
      subtitle = "Renewable Elec.",
      icon = icon("leaf"),
      color = "success",
      footer = "share",
      width = NULL
    )
  })
  output$rd_vb_cap <- renderText({
    d <- dat(); req(input$rd_sc, input$rd_reg, input$rd_yr)
    val <- d$capacity %>%
      filter(scenario == input$rd_sc, region == input$rd_reg, year == input$rd_yr[2]) %>%
      pull(value) %>% sum(na.rm = TRUE)
    if (!length(val) || is.na(val) || val == 0) "N/A" else paste0(round(val, 0), " GW")
  })

  # ── REGIONAL DASHBOARD CHARTS ───────────────────────────────────────────────
  output$rd_co2 <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$emissions %>%
      filter(scenario == input$rd_sc, region == input$rd_reg, gas == "CO2", is.na(domain),
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No CO2 data."))
    p <- ggplot(df, aes(year, value)) +
      geom_area(fill = "#E63946", alpha = 0.15) + geom_line(colour = "#E63946", linewidth = 1.1) +
      labs(x = NULL, y = unique(df$unit)[1]) + dtheme_sm()
    ggplotly(p) %>% ptheme()
  })
  output$rd_pe <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$primary_energy %>%
      filter(scenario == input$rd_sc, region == input$rd_reg, fuel != "Total",
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No primary energy data."))
    p <- ggplot(df, aes(year, value, fill = fuel, text = paste0(fuel, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      scale_fill_manual(values = FUEL_COLS, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme_sm()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$rd_se <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$secondary_elec %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No electricity data."))
    p <- ggplot(df, aes(year, value, fill = source, text = paste0(source, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme_sm()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$rd_fe <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$final_energy_sector %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No final energy data."))
    p <- ggplot(df, aes(year, value, fill = sector, text = paste0(sector, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme_sm()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$rd_cap <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$capacity %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No capacity data."))
    p <- ggplot(df, aes(year, value, fill = tech, text = paste0(tech, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = 0.85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL) + dtheme_sm()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })
  output$rd_ind <- renderPlotly({
    d <- dat(); req(input$rd_sc, input$rd_reg)
    df <- d$indicators %>%
      filter(scenario == input$rd_sc, region == input$rd_reg,
             variable %in% c("GDP|PPP", "Population", "Price|Carbon"),
             year >= input$rd_yr[1], year <= input$rd_yr[2])
    validate(need(nrow(df) > 0, "No indicator data for this region."))
    p <- ggplot(df, aes(year, value, colour = variable, group = variable,
                        text = paste0(variable, "\n", round(value, 3), " ", unit))) +
      geom_line(linewidth = 1.0) + geom_point(size = 1.2) +
      facet_wrap(~variable, scales = "free_y", ncol = 1) +
      labs(x = NULL, y = NULL, colour = NULL) +
      dtheme_sm() + theme(legend.position = "none", strip.text = element_text(size = 6.5))
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── EMISSIONS ───────────────────────────────────────────────────────────────
  output$plot_em <- renderPlotly({
    d <- dat(); req(input$em_sc, input$em_reg, input$em_gas)
    src <- if (input$em_gross) d$gross_emissions else d$emissions
    df <- src %>%
      filter(scenario %in% input$em_sc, region %in% input$em_reg,
             gas %in% input$em_gas, year >= input$em_yr[1], year <= input$em_yr[2])
    if (!is.null(input$em_dom) && !"All" %in% input$em_dom && length(input$em_dom) > 0)
      df <- df %>% filter(domain %in% input$em_dom)
    df <- df %>%
      group_by(scenario, region, gas, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value, colour = gas, fill = gas,
        group = interaction(scenario, gas, region),
        text = paste0(scenario, " | ", region, " | ", gas,
                      "\nYear: ", year, "\n", round(value, 2), " ", unit))) +
      { if (input$em_stack) geom_area(alpha = .7, position = "stack")
        else list(geom_line(linewidth = 1.1), geom_point(size = 1.5)) } +
      scale_colour_manual(values = GAS_COLS, na.value = "#aaa") +
      scale_fill_manual(values = GAS_COLS, na.value = "#aaa") +
      facet_wrap(~scenario) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL, fill = NULL) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── PRIMARY ENERGY ──────────────────────────────────────────────────────────
  output$plot_pe <- renderPlotly({
    d <- dat(); req(input$pe_sc, input$pe_reg)
    src <- if (input$pe_gic) d$gic else d$primary_energy
    df <- src %>%
      filter(scenario %in% input$pe_sc, region %in% input$pe_reg,
             year >= input$pe_yr[1], year <= input$pe_yr[2], fuel != "Total") %>%
      group_by(scenario, fuel, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data."))
    title_str <- if (input$pe_gic) "Gross Inland Consumption" else "Primary Energy Mix"
    p <- ggplot(df, aes(year, value, fill = fuel,
        text = paste0(fuel, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = FUEL_COLS, na.value = "#888") +
      facet_wrap(~scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = title_str) +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── ELECTRICITY MIX ─────────────────────────────────────────────────────────
  output$plot_se <- renderPlotly({
    d <- dat(); req(input$se_sc, input$se_reg)
    df <- d$secondary_elec %>%
      filter(scenario %in% input$se_sc, region %in% input$se_reg,
             year >= input$se_yr[1], year <= input$se_yr[2]) %>%
      group_by(scenario, source, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No electricity data."))
    p <- ggplot(df, aes(year, value, fill = source,
        text = paste0(source, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      facet_wrap(~scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = "Electricity Generation Mix") +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── FINAL ENERGY ────────────────────────────────────────────────────────────
  output$plot_fe <- renderPlotly({
    d <- dat(); req(input$fe_sc, input$fe_reg, input$fe_yr_pt)
    if (input$fe_mode == "sector") {
      df <- d$final_energy_sector %>%
        filter(scenario %in% input$fe_sc, region %in% input$fe_reg,
               year == as.integer(input$fe_yr_pt)) %>%
        group_by(scenario, sector, unit) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        rename(group = sector)
    } else {
      df <- d$final_energy_carrier %>%
        filter(scenario %in% input$fe_sc, region %in% input$fe_reg,
               year == as.integer(input$fe_yr_pt)) %>%
        group_by(scenario, carrier, unit) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        rename(group = carrier)
    }
    validate(need(nrow(df) > 0, "No final energy data."))
    extra <- setdiff(unique(df$group), names(FUEL_COLS))
    col_map <- c(FUEL_COLS, setNames(hue_pal()(length(extra)), extra))
    p <- ggplot(df, aes(scenario, value, fill = group,
        text = paste0(group, "\n", scenario, "\n", round(value, 1), " ", unit))) +
      geom_col(position = "stack", alpha = .9) +
      scale_fill_manual(values = col_map, na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL,
           title = paste("Final Energy by", tools::toTitleCase(input$fe_mode), "—", input$fe_yr_pt)) +
      dtheme() + theme(legend.position = "right", axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── POWER CAPACITY ──────────────────────────────────────────────────────────
  output$plot_cap <- renderPlotly({
    d <- dat(); req(input$cap_sc, input$cap_reg)
    src <- if (input$cap_mode == "cap") d$capacity else d$capacity_additions
    df <- src %>%
      filter(scenario %in% input$cap_sc, region %in% input$cap_reg,
             year >= input$cap_yr[1], year <= input$cap_yr[2]) %>%
      group_by(scenario, tech, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No capacity data."))
    title_str <- if (input$cap_mode == "cap") "Installed Power Capacity" else "Capacity Additions"
    p <- ggplot(df, aes(year, value, fill = tech,
        text = paste0(tech, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      facet_wrap(~scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = title_str) +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── CCS ─────────────────────────────────────────────────────────────────────
  output$plot_ccs <- renderPlotly({
    d <- dat(); req(input$ccs_sc, input$ccs_reg, input$ccs_cat)
    df <- d$ccs %>%
      filter(scenario %in% input$ccs_sc, region %in% input$ccs_reg,
             category %in% input$ccs_cat, year >= input$ccs_yr[1], year <= input$ccs_yr[2]) %>%
      group_by(scenario, category, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No CCS data."))
    p <- ggplot(df, aes(year, value, colour = category,
        group = interaction(scenario, category),
        text = paste0(category, "\n", scenario, "\nYear: ", year, "\n", round(value, 2), " ", unit))) +
      geom_line(linewidth = 1.1) + geom_point(size = 1.5) +
      facet_wrap(~scenario) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── INDICATORS ──────────────────────────────────────────────────────────────
  output$plot_ind <- renderPlotly({
    d <- dat(); req(input$ind_var, input$ind_sc, input$ind_reg)
    df <- d$indicators %>%
      filter(variable %in% input$ind_var, scenario %in% input$ind_sc,
             region %in% input$ind_reg, year >= input$ind_yr[1], year <= input$ind_yr[2])
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(year, value, colour = scenario, linetype = region,
        group = interaction(scenario, region),
        text = paste0(scenario, " | ", region, "\nYear: ", year, "\n", round(value, 3), " ", unit))) +
      geom_line(linewidth = 1.1) + geom_point(size = 1.5) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL, linetype = NULL,
           title = input$ind_var) + dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── SCENARIO COMPARISON ─────────────────────────────────────────────────────
  output$plot_sc <- renderPlotly({
    d <- dat(); req(input$sc_var, input$sc_reg, input$sc_yr_pt)
    df <- d$raw %>%
      filter(variable == input$sc_var, region %in% input$sc_reg,
             year == as.integer(input$sc_yr_pt))
    validate(need(nrow(df) > 0, "No data."))
    p <- ggplot(df, aes(region, value, fill = scenario,
        text = paste0(scenario, " / ", region, "\n", round(value, 3), " ", unit))) +
      geom_col(position = "dodge", alpha = .9) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL,
           title = paste(input$sc_var, "—", input$sc_yr_pt)) +
      dtheme() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── HEAT MAP ────────────────────────────────────────────────────────────────
  output$plot_hm <- renderPlotly({
    d <- dat(); req(input$hm_sc, input$hm_var)
    df <- d$raw %>%
      filter(scenario == input$hm_sc, variable == input$hm_var,
             year >= input$hm_yr[1], year <= input$hm_yr[2]) %>%
      group_by(region, year) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data for this variable."))
    mat  <- df %>% pivot_wider(names_from = year, values_from = value)
    regs <- mat$region
    yrs  <- sort(as.integer(setdiff(names(mat), "region")))
    zmat <- as.matrix(mat[, as.character(yrs)])
    plot_ly(x = yrs, y = regs, z = zmat, type = "heatmap",
      colorscale = list(c(0, "#1D3557"), c(0.5, "#E9C46A"), c(1, "#E63946")),
      hovertemplate = "Year: %{x}<br>Region: %{y}<br>Value: %{z:.2f}<extra></extra>") %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "#E8EAF0", family = "sans"),
             xaxis = list(title = "Year", gridcolor = "#2a2d3a"),
             yaxis = list(title = NULL),
             margin = list(t = 20, b = 50, l = 80, r = 20))
  })

  # ── SCATTER PLOT ────────────────────────────────────────────────────────────
  output$plot_sp <- renderPlotly({
    d <- dat(); req(input$sp_sc, input$sp_xvar, input$sp_yvar, input$sp_reg, input$sp_yr_pt)
    xdf <- d$raw %>%
      filter(variable == input$sp_xvar, scenario %in% input$sp_sc,
             region %in% input$sp_reg, year == as.integer(input$sp_yr_pt)) %>%
      select(scenario, region, x = value, xunit = unit)
    ydf <- d$raw %>%
      filter(variable == input$sp_yvar, scenario %in% input$sp_sc,
             region %in% input$sp_reg, year == as.integer(input$sp_yr_pt)) %>%
      select(scenario, region, y = value, yunit = unit)
    df <- inner_join(xdf, ydf, by = c("scenario", "region"))
    validate(need(nrow(df) > 0, "No overlapping data for these two variables."))
    p <- ggplot(df, aes(x, y, colour = region, shape = scenario,
        text = paste0(region, " | ", scenario,
                      "\nX: ", round(x, 3), " ", xunit,
                      "\nY: ", round(y, 3), " ", yunit))) +
      geom_point(size = 3.5, alpha = 0.9) +
      labs(x = paste0(input$sp_xvar, "  [", unique(df$xunit)[1], "]"),
           y = paste0(input$sp_yvar, "  [", unique(df$yunit)[1], "]"),
           colour = NULL, shape = NULL,
           title = paste(input$sp_xvar, "vs", input$sp_yvar, "—", input$sp_yr_pt)) +
      dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── SANKEY ──────────────────────────────────────────────────────────────────
  output$plot_sk <- renderSankeyNetwork({
    d <- dat(); req(input$sk_sc, input$sk_reg, input$sk_yr)
    df <- d$flows %>%
      filter(scenario == input$sk_sc, region == input$sk_reg,
             year == as.integer(input$sk_yr)) %>%
      group_by(source, target) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      filter(value > 0)
    if (nrow(df) == 0) {
      toast(
        title = "No Data Found",
        body = "Try selecting a different scenario or region.",
        options = list(class = "bg-warning", autohide = TRUE, delay = 3000)
      )
    }
    validate(need(nrow(df) > 0, "No flow data."))
    nodes   <- data.frame(name = unique(c(df$source, df$target)))
    df$s_id <- match(df$source, nodes$name) - 1
    df$t_id <- match(df$target, nodes$name) - 1
    sankeyNetwork(Links = df, Nodes = nodes, Source = "s_id", Target = "t_id",
      Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 20,
      nodePadding = 10, colourScale = JS("d3.scaleOrdinal(d3.schemeTableau10)"),
      sinksRight = FALSE)
  })

  # ── REGIONAL MAP ────────────────────────────────────────────────────────────
  output$plot_map <- renderLeaflet({
    d <- dat(); req(input$map_sc, input$map_yr)
    df <- d$regional %>%
      filter(scenario == input$map_sc, year == as.integer(input$map_yr))
    validate(need(nrow(df) > 0, "No map data."))
    pal <- colorNumeric(c("#1D3557", "#E9C46A", "#E63946"), domain = df$value)
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
        radius = ~rescale(value, to = c(8, 35)),
        fillColor = ~pal(value), fillOpacity = .8, stroke = FALSE,
        popup = ~paste0("<b>", region, "</b><br>CO2: ", round(value, 1), " Mt/yr")) %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = "CO2 (Mt/yr)", opacity = .9)
  })

  # ── DATA TABLE ──────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    d <- dat()
    df <- d$raw
    if (length(input$tbl_sc)  > 0) df <- df %>% filter(scenario %in% input$tbl_sc)
    if (length(input$tbl_reg) > 0) df <- df %>% filter(region   %in% input$tbl_reg)
    if (length(input$tbl_var) > 0) df <- df %>% filter(variable %in% input$tbl_var)
    validate(need(nrow(df) > 0, "No data."))
    datatable(
      df %>% select(model, scenario, region, variable, unit, year, value),
      filter = "top", rownames = FALSE,
      extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("csv", "excel"),
                     pageLength = 25, scrollX = TRUE)
    ) %>% formatRound("value", digits = 4)
  })
}

shinyApp(ui, server)
