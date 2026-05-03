# =============================================================================
# OPEN-PROM Results Dashboard
# Reads reporting.mif — IAMC semicolon-separated format
# Columns: Model;Scenario;Region;Variable;Unit;2010;2011;...;2100
#
# Variable branches present in this model:
#   Emissions|*                         -> Emissions tab
#   Gross Emissions|*                   -> Emissions tab (gross)
#   Primary Energy|*                    -> Primary Energy tab
#   Secondary Energy|Electricity|*      -> Electricity Mix tab
#   Secondary Energy|Heat|*             -> Heat tab
#   Secondary Energy|Hydrogen|*         -> Hydrogen tab
#   Final Energy|*                      -> Final Energy tab
#   Gross Inland Consumption|*          -> Gross Inland Consumption tab
#   Capacity|Electricity|*              -> Power Capacity tab
#   Capacity Additions|Electricity|*    -> Power Capacity tab
#   Carbon Capture|*                    -> CCS tab
#   Price|Carbon                        -> Indicators tab
#   Price|Final Energy|*                -> Indicators tab
#   GDP|PPP                             -> Indicators tab
#   Population                          -> Indicators tab
#   Share Equipment Capacity|*          -> Equipment Shares tab
# =============================================================================

library(shiny)
library(bslib)
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

MIF_FILE <- "data/reporting.mif"

# =============================================================================
# READ MIF
# =============================================================================

read_mif <- function(path) {
  raw <- read_delim(
    path,
    delim         = ";",
    quote         = '"',
    escape_double = TRUE,
    col_types     = cols(.default = col_character()),
    trim_ws       = TRUE,
    locale        = locale(encoding = "UTF-8")
  )
  names(raw)[1] <- str_remove(names(raw)[1], "^\xef\xbb\xbf")
  names(raw)    <- tolower(str_trim(names(raw)))

  year_cols <- names(raw)[grepl("^\\d{4}$", names(raw))]

  raw %>%
    pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "value") %>%
    mutate(year  = as.integer(year),
           value = suppressWarnings(as.numeric(value))) %>%
    filter(!is.na(value))
}

# =============================================================================
# HELPERS — pipe-segment extractors
# =============================================================================

seg <- function(variable, n) {
  # Extract the n-th pipe-segment (1-indexed)
  sapply(strsplit(variable, "\\|"), function(x) if (length(x) >= n) x[n] else NA_character_)
}

depth <- function(variable) {
  str_count(variable, fixed("|"))
}

# =============================================================================
# DERIVE SUB-DATASETS
# =============================================================================

derive_datasets <- function(long) {

  # ── EMISSIONS ──────────────────────────────────────────────────────────────
  # Emissions|<gas>                         depth 1  -> top-level per gas
  # Emissions|<gas>|<domain>               depth 2  -> Energy / AFOLU / etc.
  # Emissions|<gas>|Energy|Demand|<sector> depth 4+ -> sectoral demand
  # Emissions|<gas>|Energy|Supply|<source> depth 4+ -> supply side
  emissions <- long %>%
    filter(str_starts(variable, "Emissions|")) %>%
    mutate(
      gas    = seg(variable, 2),
      domain = seg(variable, 3),   # Energy, AFOLU, Industrial Processes, Waste; NA if depth 1
      side   = seg(variable, 4),   # Demand / Supply; NA if depth < 3
      sector = seg(variable, 5)    # specific sector; NA if depth < 4
    )

  # ── GROSS EMISSIONS ────────────────────────────────────────────────────────
  gross_emissions <- long %>%
    filter(str_starts(variable, "Gross Emissions|")) %>%
    mutate(
      gas    = seg(variable, 3),
      domain = seg(variable, 4),
      side   = seg(variable, 5),
      sector = seg(variable, 6)
    )

  # ── PRIMARY ENERGY ─────────────────────────────────────────────────────────
  # Primary Energy              -> total
  # Primary Energy|<fuel>       -> one level: Coal, Oil, Gas, Nuclear, etc.
  primary_energy <- long %>%
    filter(str_starts(variable, "Primary Energy")) %>%
    mutate(fuel = if_else(variable == "Primary Energy", "Total",
                          str_remove(variable, "^Primary Energy\\|"))) %>%
    filter(!str_detect(fuel, "\\|"))   # one level only — no double-counting

  # ── GROSS INLAND CONSUMPTION ───────────────────────────────────────────────
  gic <- long %>%
    filter(str_starts(variable, "Gross Inland Consumption")) %>%
    mutate(fuel = if_else(variable == "Gross Inland Consumption", "Total",
                          str_remove(variable, "^Gross Inland Consumption\\|"))) %>%
    filter(!str_detect(fuel, "\\|"))

  # ── SECONDARY ENERGY ──────────────────────────────────────────────────────
  # Secondary Energy|Electricity|<source>       one level deep
  # Secondary Energy|Electricity|<source>|w/ CCS / w/o CCS  two levels
  secondary_elec <- long %>%
    filter(str_starts(variable, "Secondary Energy|Electricity|"),
           variable != "Secondary Energy|Electricity",
           variable != "Secondary Energy|Electricity|Demand") %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Electricity\\|")) %>%
    filter(!str_detect(source, "\\|"))   # one level, no CCS breakdown

  secondary_heat <- long %>%
    filter(str_starts(variable, "Secondary Energy|Heat|")) %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Heat\\|")) %>%
    filter(!str_detect(source, "\\|"))

  secondary_hydrogen <- long %>%
    filter(str_starts(variable, "Secondary Energy|Hydrogen|")) %>%
    mutate(source = str_remove(variable, "^Secondary Energy\\|Hydrogen\\|")) %>%
    filter(!str_detect(source, "\\|"))

  # ── FINAL ENERGY ──────────────────────────────────────────────────────────
  # Final Energy|<sector>                   top-level sector (Industry, Transportation…)
  # Final Energy|<sector>|<carrier>         sector × carrier
  # Final Energy|<carrier>                  carrier totals (Electricity, Gas, etc.)
  #
  # For the sector stacked bar we want: Final Energy|<sector> (depth 1, not "w/o bunkers")
  final_energy_sector <- long %>%
    filter(str_starts(variable, "Final Energy|"),
           !str_starts(variable, "Final Energy w/o")) %>%
    mutate(sector = str_remove(variable, "^Final Energy\\|")) %>%
    filter(!str_detect(sector, "\\|"),
           !sector %in% c("w/o bunkers"))

  # For carrier breakdown: Final Energy|<carrier> (depth 1)
  final_energy_carrier <- long %>%
    filter(str_starts(variable, "Final Energy|"),
           !str_starts(variable, "Final Energy w/o")) %>%
    mutate(carrier = str_remove(variable, "^Final Energy\\|")) %>%
    filter(!str_detect(carrier, "\\|"),
           carrier %in% c("Electricity","Gas","Oil","Coal","Hydrogen",
                          "Biofuels","Heat","Biodiesel","Biogasoline",
                          "Natural Gas","Diesel Oil","Gasoline","Kerosene",
                          "Liquefied Petroleum Gas","Lignite","Nuclear",
                          "Solar","Wind","Hydro","Residual Fuel Oil",
                          "Other Gases","Other Liquids","Other fuels",
                          "Renewables","Solids","Steam","Methanol",
                          "Hydrogen","Ethanol","Biomass and Waste",
                          "Crude Oil and Feedstocks","Fossil Liquids"))

  # ── POWER CAPACITY ────────────────────────────────────────────────────────
  capacity <- long %>%
    filter(str_starts(variable, "Capacity|Electricity|")) %>%
    mutate(tech = str_remove(variable, "^Capacity\\|Electricity\\|")) %>%
    filter(!str_detect(tech, "\\|"))   # one level only

  capacity_additions <- long %>%
    filter(str_starts(variable, "Capacity Additions|Electricity|")) %>%
    mutate(tech = str_remove(variable, "^Capacity Additions\\|Electricity\\|")) %>%
    filter(!str_detect(tech, "\\|"))

  # ── CARBON CAPTURE ────────────────────────────────────────────────────────
  ccs <- long %>%
    filter(str_starts(variable, "Carbon Capture|")) %>%
    mutate(category = seg(variable, 2),
           sub      = seg(variable, 3))

  # ── INDICATORS ────────────────────────────────────────────────────────────
  indicators <- long %>%
    filter(variable %in% c("GDP|PPP", "Population", "Price|Carbon") |
             (str_starts(variable, "Price|Carbon")))

  # ── SANKEY: Primary Energy -> Final Energy sectors ─────────────────────────
  flows <- derive_flows(primary_energy, final_energy_sector)

  # ── REGIONAL MAP ──────────────────────────────────────────────────────────
  regional <- derive_regional(emissions)

  list(
    raw                 = long,
    emissions           = emissions,
    gross_emissions     = gross_emissions,
    primary_energy      = primary_energy,
    gic                 = gic,
    secondary_elec      = secondary_elec,
    secondary_heat      = secondary_heat,
    secondary_hydrogen  = secondary_hydrogen,
    final_energy_sector = final_energy_sector,
    final_energy_carrier= final_energy_carrier,
    capacity            = capacity,
    capacity_additions  = capacity_additions,
    ccs                 = ccs,
    indicators          = indicators,
    flows               = flows,
    regional            = regional
  )
}

derive_flows <- function(pe, fe) {
  src <- pe %>% filter(fuel != "Total") %>%
    mutate(source = fuel, target = "Primary Energy") %>%
    select(scenario, region, year, source, target, value) %>%
    filter(value > 0)

  tgt <- fe %>% filter(!sector %in% c("Total", "w/o bunkers")) %>%
    mutate(source = "Primary Energy", target = sector) %>%
    select(scenario, region, year, source, target, value) %>%
    filter(value > 0)

  bind_rows(src, tgt) %>%
    group_by(scenario, region, year, source, target) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

derive_regional <- function(emissions) {
  centroids <- tribble(
    ~region, ~lat,   ~lon,
    "CHA",   35.9,  104.2,
    "EUR",   50.1,   10.4,
    "EU",    50.1,   10.4,
    "USA",   38.9,  -77.0,
    "IND",   20.6,   78.9,
    "JPN",   36.2,  138.3,
    "RUS",   61.5,   90.0,
    "BRA",  -14.2,  -51.9,
    "AFR",   -8.8,   34.5,
    "MEA",   29.3,   42.5,
    "OAS",   15.0,  100.0,
    "LAM",  -15.0,  -60.0,
    "World", 20.0,   10.0
  )
  emissions %>%
    filter(gas == "CO2", is.na(domain)) %>%   # top-level Emissions|CO2 only
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(centroids, by = "region") %>%
    filter(!is.na(lat))
}

# =============================================================================
# LOAD DATA
# =============================================================================

load_data <- function() {
  if (!file.exists(MIF_FILE)) return(NULL)
  long <- tryCatch(read_mif(MIF_FILE), error = function(e) { message(e); NULL })
  if (is.null(long) || nrow(long) == 0) return(NULL)
  derive_datasets(long)
}

# =============================================================================
# COLOUR PALETTES
# =============================================================================

FUEL_COLS <- c(
  "Coal"     = "#3d3d3d", "Lignite"  = "#555555",
  "Oil"      = "#6B4226", "Crude Oil and Feedstocks" = "#7a5230",
  "Gas"      = "#A8DADC", "Natural Gas" = "#90cfd2",
  "Nuclear"  = "#E9C46A",
  "Wind"     = "#457B9D",
  "Solar"    = "#F4D35E",
  "Hydro"    = "#1D3557",
  "Biofuels" = "#588157", "Biomass and Waste" = "#6aad65",
  "Geothermal and other renewable sources" = "#2A9D8F",
  "Hydrogen" = "#90E0EF",
  "Electricity" = "#FFD166",
  "Heat"     = "#EF476F",
  "Other fuels" = "#aaaaaa", "Other Gases" = "#bbbbbb",
  "Other Liquids" = "#cccccc", "Renewables" = "#43aa8b",
  "Total"    = "#E8EAF0"
)

GAS_COLS <- c(
  "CO2"         = "#E63946",
  "CH4"         = "#F4A261",
  "N2O"         = "#2A9D8F",
  "Kyoto Gases" = "#9B5DE5",
  "F-gases"     = "#457B9D",
  "HFC"         = "#E9C46A",
  "SF6"         = "#A8DADC",
  "PFC"         = "#588157"
)

TECH_COLS <- c(
  "Coal"     = "#3d3d3d", "Coal|w/ CCS" = "#666666", "Coal|w/o CCS" = "#444444",
  "Gas"      = "#A8DADC", "Gas|w/ CCS"  = "#c8eaec", "Gas|w/o CCS"  = "#90cfd2",
  "Oil"      = "#6B4226", "Oil|w/o CCS" = "#855a3a",
  "Nuclear"  = "#E9C46A",
  "Wind"     = "#457B9D", "Hydro" = "#1D3557",
  "Solar"    = "#F4D35E",
  "Biofuels" = "#588157", "Biofuels|w/ CCS" = "#6aad65", "Biofuels|w/o CCS" = "#4d7a4a",
  "Geothermal and other renewable sources" = "#2A9D8F",
  "Hydrogen" = "#90E0EF"
)

# Shared dark ggplot theme
dtheme <- function() {
  theme_minimal(base_family = "IBM Plex Mono") +
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

ptheme <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = "#E8EAF0", family = "IBM Plex Mono"),
    xaxis         = list(gridcolor = "#2a2d3a", zerolinecolor = "#2a2d3a"),
    yaxis         = list(gridcolor = "#2a2d3a", zerolinecolor = "#2a2d3a"),
    legend        = list(bgcolor = "rgba(15,17,23,0.7)",
                         bordercolor = "#2a2d3a", borderwidth = 1),
    margin        = list(t = 20, b = 40, l = 60, r = 20)
  )
}

fsb <- function(...) {
  sidebar(width = 260, bg = "#161820",
    tags$p(style = "color:#2A9D8F;font-size:.7rem;font-family:'IBM Plex Mono',monospace;margin:0 0 6px;",
           "FILTERS"),
    ...
  )
}

yr_slider <- function(id, lo = 2020, hi = 2060)
  sliderInput(id, "Year range", 2010, 2100, c(lo, hi), step = 1, sep = "")

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  theme = bs_theme(
    bg = "#0F1117", fg = "#E8EAF0",
    primary = "#2A9D8F", secondary = "#457B9D",
    base_font    = font_google("IBM Plex Mono"),
    heading_font = font_google("Space Mono"),
    font_scale   = 0.85
  ),
  title = tags$span(
    style = "font-family:'Space Mono',monospace;color:#2A9D8F;letter-spacing:.05em;",
    "OPEN-PROM // Results Explorer"),
  bg = "#0F1117", fillable = TRUE,

  # 1. EMISSIONS
  nav_panel("Emissions", icon = icon("chart-line"),
    layout_sidebar(sidebar = fsb(
      selectInput("em_sc",  "Scenario(s)", NULL, multiple = TRUE),
      selectInput("em_reg", "Region(s)",   NULL, multiple = TRUE),
      selectInput("em_gas", "Gas(es)",     NULL, multiple = TRUE),
      selectInput("em_dom", "Domain",      NULL, multiple = TRUE),
      yr_slider("em_yr"),
      checkboxInput("em_gross", "Show gross emissions", FALSE),
      checkboxInput("em_stack", "Stack by gas",         FALSE)
    ), plotlyOutput("plot_em", height = "540px"))
  ),

  # 2. PRIMARY ENERGY
  nav_panel("Primary Energy", icon = icon("bolt"),
    layout_sidebar(sidebar = fsb(
      selectInput("pe_sc",  "Scenario", NULL),
      selectInput("pe_reg", "Region(s)", NULL, multiple = TRUE),
      yr_slider("pe_yr"),
      checkboxInput("pe_gic", "Show Gross Inland Consumption instead", FALSE)
    ), plotlyOutput("plot_pe", height = "540px"))
  ),

  # 3. ELECTRICITY MIX
  nav_panel("Electricity Mix", icon = icon("plug"),
    layout_sidebar(sidebar = fsb(
      selectInput("se_sc",  "Scenario", NULL),
      selectInput("se_reg", "Region(s)", NULL, multiple = TRUE),
      yr_slider("se_yr")
    ), plotlyOutput("plot_se", height = "540px"))
  ),

  # 4. FINAL ENERGY
  nav_panel("Final Energy", icon = icon("industry"),
    layout_sidebar(sidebar = fsb(
      selectInput("fe_sc",  "Scenario(s)", NULL, multiple = TRUE),
      selectInput("fe_reg", "Region(s)",   NULL, multiple = TRUE),
      selectInput("fe_yr_pt", "Year",      NULL),
      radioButtons("fe_mode", "Group by",
                   c("Sector" = "sector", "Carrier" = "carrier"), inline = TRUE)
    ), plotlyOutput("plot_fe", height = "540px"))
  ),

  # 5. POWER CAPACITY
  nav_panel("Power Capacity", icon = icon("tower-broadcast"),
    layout_sidebar(sidebar = fsb(
      selectInput("cap_sc",   "Scenario(s)", NULL, multiple = TRUE),
      selectInput("cap_reg",  "Region(s)",   NULL, multiple = TRUE),
      yr_slider("cap_yr"),
      radioButtons("cap_mode", "Show",
                   c("Installed capacity" = "cap",
                     "Capacity additions" = "add"), inline = TRUE)
    ), plotlyOutput("plot_cap", height = "540px"))
  ),

  # 6. CCS
  nav_panel("Carbon Capture", icon = icon("cloud-arrow-down"),
    layout_sidebar(sidebar = fsb(
      selectInput("ccs_sc",  "Scenario(s)", NULL, multiple = TRUE),
      selectInput("ccs_reg", "Region(s)",   NULL, multiple = TRUE),
      selectInput("ccs_cat", "Category",    NULL, multiple = TRUE),
      yr_slider("ccs_yr")
    ), plotlyOutput("plot_ccs", height = "540px"))
  ),

  # 7. INDICATORS
  nav_panel("Indicators", icon = icon("gauge"),
    layout_sidebar(sidebar = fsb(
      selectInput("ind_var", "Variable", NULL),
      selectInput("ind_sc",  "Scenario(s)", NULL, multiple = TRUE),
      selectInput("ind_reg", "Region(s)",   NULL, multiple = TRUE),
      yr_slider("ind_yr")
    ), plotlyOutput("plot_ind", height = "540px"))
  ),

  # 8. SCENARIO COMPARISON
  nav_panel("Scenario Comparison", icon = icon("chart-bar"),
    layout_sidebar(sidebar = fsb(
      selectInput("sc_var",    "Variable", NULL),
      selectInput("sc_reg",    "Region(s)", NULL, multiple = TRUE),
      selectInput("sc_yr_pt",  "Year",     NULL)
    ), plotlyOutput("plot_sc", height = "540px"))
  ),

  # 9. ENERGY FLOWS (SANKEY)
  nav_panel("Energy Flows", icon = icon("water"),
    layout_sidebar(sidebar = fsb(
      selectInput("sk_sc",  "Scenario", NULL),
      selectInput("sk_reg", "Region",   NULL),
      selectInput("sk_yr",  "Year",     NULL)
    ), sankeyNetworkOutput("plot_sk", height = "540px"))
  ),

  # 10. REGIONAL MAP
  nav_panel("Regional Map", icon = icon("globe"),
    layout_sidebar(sidebar = fsb(
      selectInput("map_sc", "Scenario", NULL),
      selectInput("map_yr", "Year",     NULL)
    ), leafletOutput("plot_map", height = "540px"))
  ),

  # 11. DATA TABLE
  nav_panel("Data Table", icon = icon("table"),
    layout_sidebar(sidebar = fsb(
      selectInput("tbl_sc",  "Scenario(s)", NULL, multiple = TRUE),
      selectInput("tbl_reg", "Region(s)",   NULL, multiple = TRUE),
      selectInput("tbl_var", "Variable(s)", NULL, multiple = TRUE)
    ), DTOutput("data_table"))
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  dat <- reactive({
    d <- load_data()
    validate(need(!is.null(d),
      paste0("Cannot load '", MIF_FILE, "'. Put reporting.mif in data/ next to app.R.")))
    d
  })

  # ── POPULATE DROPDOWNS ────────────────────────────────────────────────────
  observe({
    d   <- dat()
    sc  <- sort(unique(d$raw$scenario))
    reg <- sort(unique(d$raw$region))
    yr  <- sort(unique(d$raw$year))
    yl  <- min(yr); yh <- max(yr)
    ylo <- max(yl, 2020); yhi <- min(yh, 2060)
    yp  <- min(yr[yr >= 2030], na.rm = TRUE)

    gases   <- sort(unique(d$emissions$gas))
    domains <- sort(na.omit(unique(d$emissions$domain)))
    ccs_cat <- sort(unique(d$ccs$category))
    ind_vars <- sort(unique(d$indicators$variable))
    all_vars <- sort(unique(d$raw$variable))

    updateSelectInput(session, "em_sc",  choices = sc,      selected = sc)
    updateSelectInput(session, "em_reg", choices = reg,     selected = reg)
    updateSelectInput(session, "em_gas", choices = gases,   selected = intersect(c("CO2","Kyoto Gases"), gases))
    updateSelectInput(session, "em_dom", choices = c("All", domains), selected = "All")
    updateSliderInput(session, "em_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "pe_sc",  choices = sc,  selected = sc[1])
    updateSelectInput(session, "pe_reg", choices = reg, selected = reg)
    updateSliderInput(session, "pe_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "se_sc",  choices = sc,  selected = sc[1])
    updateSelectInput(session, "se_reg", choices = reg, selected = reg)
    updateSliderInput(session, "se_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "fe_sc",     choices = sc,  selected = sc)
    updateSelectInput(session, "fe_reg",    choices = reg, selected = reg)
    updateSelectInput(session, "fe_yr_pt",  choices = yr,  selected = yp)

    updateSelectInput(session, "cap_sc",  choices = sc,  selected = sc)
    updateSelectInput(session, "cap_reg", choices = reg, selected = reg)
    updateSliderInput(session, "cap_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "ccs_sc",  choices = sc,      selected = sc)
    updateSelectInput(session, "ccs_reg", choices = reg,     selected = reg)
    updateSelectInput(session, "ccs_cat", choices = ccs_cat, selected = ccs_cat)
    updateSliderInput(session, "ccs_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "ind_var", choices = ind_vars, selected = ind_vars[1])
    updateSelectInput(session, "ind_sc",  choices = sc,  selected = sc)
    updateSelectInput(session, "ind_reg", choices = reg, selected = reg)
    updateSliderInput(session, "ind_yr",  min = yl, max = yh, value = c(ylo, yhi))

    updateSelectInput(session, "sc_var",   choices = all_vars, selected = "Emissions|CO2")
    updateSelectInput(session, "sc_reg",   choices = reg, selected = reg)
    updateSelectInput(session, "sc_yr_pt", choices = yr,  selected = yp)

    updateSelectInput(session, "sk_sc",  choices = sc,  selected = sc[1])
    updateSelectInput(session, "sk_reg", choices = reg, selected = reg[1])
    updateSelectInput(session, "sk_yr",  choices = yr,  selected = yp)

    updateSelectInput(session, "map_sc", choices = sc, selected = sc[1])
    updateSelectInput(session, "map_yr", choices = yr, selected = yp)

    updateSelectInput(session, "tbl_sc",  choices = sc,       selected = sc)
    updateSelectInput(session, "tbl_reg", choices = reg,      selected = reg)
    updateSelectInput(session, "tbl_var", choices = all_vars, selected = NULL)
  })

  # ── 1. EMISSIONS ─────────────────────────────────────────────────────────
  output$plot_em <- renderPlotly({
    d <- dat()
    req(input$em_sc, input$em_reg, input$em_gas)

    src <- if (input$em_gross) d$gross_emissions else d$emissions

    df <- src %>%
      filter(scenario %in% input$em_sc,
             region   %in% input$em_reg,
             gas      %in% input$em_gas,
             year     >= input$em_yr[1],
             year     <= input$em_yr[2])

    # Domain filter
    if (!is.null(input$em_dom) && !"All" %in% input$em_dom && length(input$em_dom) > 0)
      df <- df %>% filter(domain %in% input$em_dom)

    df <- df %>%
      group_by(scenario, region, gas, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No data for these filters."))

    p <- ggplot(df, aes(year, value,
      colour = gas, fill = gas,
      group  = interaction(scenario, gas, region),
      text   = paste0(scenario, " | ", region, " | ", gas,
                      "\nYear: ", year, "\n", round(value, 2), " ", unit))) +
      { if (input$em_stack)
          geom_area(alpha = .7, position = "stack")
        else
          list(geom_line(linewidth = 1.1), geom_point(size = 1.5)) } +
      scale_colour_manual(values = GAS_COLS, na.value = "#aaa") +
      scale_fill_manual(values   = GAS_COLS, na.value = "#aaa") +
      facet_wrap(~ scenario) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL, fill = NULL) +
      dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 2. PRIMARY ENERGY ────────────────────────────────────────────────────
  output$plot_pe <- renderPlotly({
    d <- dat()
    req(input$pe_sc, input$pe_reg)

    src <- if (input$pe_gic) d$gic else d$primary_energy
    fuel_col <- if (input$pe_gic) "fuel" else "fuel"

    df <- src %>%
      filter(scenario %in% input$pe_sc,
             region   %in% input$pe_reg,
             year     >= input$pe_yr[1],
             year     <= input$pe_yr[2],
             fuel     != "Total") %>%
      group_by(scenario, fuel, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No data for these filters."))

    title_str <- if (input$pe_gic) "Gross Inland Consumption" else "Primary Energy Mix"
    p <- ggplot(df, aes(year, value, fill = fuel,
      text = paste0(fuel, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = FUEL_COLS, na.value = "#888") +
      facet_wrap(~ scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = title_str) +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 3. ELECTRICITY MIX ───────────────────────────────────────────────────
  output$plot_se <- renderPlotly({
    d <- dat()
    req(input$se_sc, input$se_reg)

    df <- d$secondary_elec %>%
      filter(scenario %in% input$se_sc,
             region   %in% input$se_reg,
             year     >= input$se_yr[1],
             year     <= input$se_yr[2]) %>%
      group_by(scenario, source, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No secondary electricity data for these filters."))

    p <- ggplot(df, aes(year, value, fill = source,
      text = paste0(source, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      facet_wrap(~ scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = "Electricity Generation Mix") +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 4. FINAL ENERGY ──────────────────────────────────────────────────────
  output$plot_fe <- renderPlotly({
    d <- dat()
    req(input$fe_sc, input$fe_reg, input$fe_yr_pt)

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

    validate(need(nrow(df) > 0, "No final energy data for these filters."))

    p <- ggplot(df, aes(scenario, value, fill = group,
      text = paste0(group, "\n", scenario, "\n", round(value, 1), " ", unit))) +
      geom_col(position = "stack", alpha = .9) +
      scale_fill_manual(values = c(FUEL_COLS,
        setNames(scales::hue_pal()(20), unique(df$group)[!unique(df$group) %in% names(FUEL_COLS)])),
        na.value = "#888") +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL,
           title = paste("Final Energy by", tools::toTitleCase(input$fe_mode), "—", input$fe_yr_pt)) +
      dtheme() + theme(legend.position = "right", axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 5. POWER CAPACITY ────────────────────────────────────────────────────
  output$plot_cap <- renderPlotly({
    d <- dat()
    req(input$cap_sc, input$cap_reg)

    src <- if (input$cap_mode == "cap") d$capacity else d$capacity_additions

    df <- src %>%
      filter(scenario %in% input$cap_sc,
             region   %in% input$cap_reg,
             year     >= input$cap_yr[1],
             year     <= input$cap_yr[2]) %>%
      group_by(scenario, tech, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No capacity data for these filters."))

    title_str <- if (input$cap_mode == "cap") "Installed Power Capacity" else "Capacity Additions"
    p <- ggplot(df, aes(year, value, fill = tech,
      text = paste0(tech, "\nYear: ", year, "\n", round(value, 1), " ", unit))) +
      geom_area(alpha = .85, position = "stack") +
      scale_fill_manual(values = TECH_COLS, na.value = "#888") +
      facet_wrap(~ scenario) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL, title = title_str) +
      dtheme() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 6. CCS ───────────────────────────────────────────────────────────────
  output$plot_ccs <- renderPlotly({
    d <- dat()
    req(input$ccs_sc, input$ccs_reg, input$ccs_cat)

    df <- d$ccs %>%
      filter(scenario %in% input$ccs_sc,
             region   %in% input$ccs_reg,
             category %in% input$ccs_cat,
             year     >= input$ccs_yr[1],
             year     <= input$ccs_yr[2]) %>%
      group_by(scenario, category, year, unit) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No CCS data for these filters."))

    p <- ggplot(df, aes(year, value, colour = category,
      group = interaction(scenario, category),
      text  = paste0(category, "\n", scenario,
                     "\nYear: ", year, "\n", round(value, 2), " ", unit))) +
      geom_line(linewidth = 1.1) + geom_point(size = 1.5) +
      facet_wrap(~ scenario) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL, title = "Carbon Capture") +
      dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 7. INDICATORS ────────────────────────────────────────────────────────
  output$plot_ind <- renderPlotly({
    d <- dat()
    req(input$ind_var, input$ind_sc, input$ind_reg)

    df <- d$indicators %>%
      filter(variable %in% input$ind_var,
             scenario %in% input$ind_sc,
             region   %in% input$ind_reg,
             year     >= input$ind_yr[1],
             year     <= input$ind_yr[2])

    validate(need(nrow(df) > 0, "No data for these filters."))

    p <- ggplot(df, aes(year, value, colour = scenario, linetype = region,
      group = interaction(scenario, region),
      text  = paste0(scenario, " | ", region,
                     "\nYear: ", year, "\n", round(value, 3), " ", unit))) +
      geom_line(linewidth = 1.1) + geom_point(size = 1.5) +
      labs(x = NULL, y = unique(df$unit)[1], colour = NULL, linetype = NULL,
           title = input$ind_var) +
      dtheme()
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 8. SCENARIO COMPARISON ───────────────────────────────────────────────
  output$plot_sc <- renderPlotly({
    d <- dat()
    req(input$sc_var, input$sc_reg, input$sc_yr_pt)

    df <- d$raw %>%
      filter(variable == input$sc_var,
             region   %in% input$sc_reg,
             year     == as.integer(input$sc_yr_pt))

    validate(need(nrow(df) > 0, "No data for this variable / year."))

    p <- ggplot(df, aes(region, value, fill = scenario,
      text = paste0(scenario, " / ", region, "\n", round(value, 3), " ", unit))) +
      geom_col(position = "dodge", alpha = .9) +
      labs(x = NULL, y = unique(df$unit)[1], fill = NULL,
           title = paste(input$sc_var, "—", input$sc_yr_pt)) +
      dtheme() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>% ptheme()
  })

  # ── 9. SANKEY ────────────────────────────────────────────────────────────
  output$plot_sk <- renderSankeyNetwork({
    d <- dat()
    req(input$sk_sc, input$sk_reg, input$sk_yr)

    df <- d$flows %>%
      filter(scenario == input$sk_sc,
             region   == input$sk_reg,
             year     == as.integer(input$sk_yr)) %>%
      group_by(source, target) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      filter(value > 0)

    validate(need(nrow(df) > 0,
      "No flow data. Requires Primary Energy and Final Energy variables with actual values."))

    nodes   <- data.frame(name = unique(c(df$source, df$target)))
    df$s_id <- match(df$source, nodes$name) - 1
    df$t_id <- match(df$target, nodes$name) - 1

    sankeyNetwork(Links = df, Nodes = nodes, Source = "s_id", Target = "t_id",
      Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 20,
      nodePadding = 10, colourScale = JS("d3.scaleOrdinal(d3.schemeTableau10)"),
      sinksRight = FALSE)
  })

  # ── 10. REGIONAL MAP ─────────────────────────────────────────────────────
  output$plot_map <- renderLeaflet({
    d <- dat()
    req(input$map_sc, input$map_yr)

    df <- d$regional %>%
      filter(scenario == input$map_sc,
             year     == as.integer(input$map_yr))

    validate(need(nrow(df) > 0,
      "No map data. Add region centroids in derive_regional() for your region codes."))

    pal <- colorNumeric(c("#2A9D8F","#E9C46A","#E63946"), domain = df$value)
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
        radius      = ~rescale(value, to = c(8, 35)),
        fillColor   = ~pal(value), fillOpacity = .8, stroke = FALSE,
        popup = ~paste0("<b>", region, "</b><br>CO2: ", round(value, 1), " Mt/yr")) %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = "CO2 (Mt/yr)", opacity = .9)
  })

  # ── 11. DATA TABLE ───────────────────────────────────────────────────────
  output$data_table <- renderDT({
    d <- dat()
    df <- d$raw

    if (length(input$tbl_sc)  > 0) df <- df %>% filter(scenario %in% input$tbl_sc)
    if (length(input$tbl_reg) > 0) df <- df %>% filter(region   %in% input$tbl_reg)
    if (length(input$tbl_var) > 0) df <- df %>% filter(variable %in% input$tbl_var)

    validate(need(nrow(df) > 0, "No data for these filters."))

    datatable(
      df %>% select(model, scenario, region, variable, unit, year, value),
      filter = "top", rownames = FALSE,
      extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("csv","excel"),
                     pageLength = 25, scrollX = TRUE)
    ) %>% formatRound("value", digits = 4)
  })
}

shinyApp(ui, server)
