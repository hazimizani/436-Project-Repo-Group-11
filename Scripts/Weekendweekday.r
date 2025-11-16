library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)

tmap_mode("view")  

# Data load & prep 
raw = read.csv("../Datasets/Crashes.csv")

title_borough = function(x) dplyr::recode(
  toupper(trimws(x)),
  "BRONX" = "Bronx",
  "BROOKLYN" = "Brooklyn",
  "MANHATTAN" = "Manhattan",
  "QUEENS" = "Queens",
  "STATEN ISLAND" = "Staten Island",
  .default = NA_character_
)

crashes_all = raw %>%
  transmute(
    crash_date = as.Date(`CRASH DATE`, format = "%m/%d/%Y"),
    year = lubridate::year(crash_date),
    month = lubridate::month(crash_date),
    crash_time = trimws(`CRASH TIME`),
    hour= suppressWarnings(as.integer(sub(":.*", "", crash_time))),
    borough_tc = title_borough(`BOROUGH`),
    latitude = suppressWarnings(as.numeric(`LATITUDE`)),
    longitude = suppressWarnings(as.numeric(`LONGITUDE`)),
    factor1 = trimws(`CONTRIBUTING FACTOR VEHICLE 1`),
    factor2 = trimws(`CONTRIBUTING FACTOR VEHICLE 2`),
    factor3 = trimws(`CONTRIBUTING FACTOR VEHICLE 3`),
    factor4 = trimws(`CONTRIBUTING FACTOR VEHICLE 4`),
    factor5 = trimws(`CONTRIBUTING FACTOR VEHICLE 5`)
  ) %>%
  filter(!is.na(crash_date),
         !is.na(latitude), !is.na(longitude),
         !is.na(borough_tc),
         !is.na(hour), hour >= 0, hour <= 23)

# Borough boundaries from  local CSV 
borough_boundaries = read_csv("borough_boundaries.csv", show_col_types = FALSE)
boroughs_sf = borough_boundaries %>%
  mutate(geometry = sf::st_as_sfc(the_geom, crs = 4326)) %>%
  sf::st_as_sf(sf_column_name = "geometry") %>%
  transmute(boro_name = BoroName, geometry) %>%
  sf::st_make_valid()

# Controls
available_years = sort(unique(crashes_all$year))
available_borough = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

factor_choices = crashes_all %>%
  select(factor1:factor5) %>%
  pivot_longer(everything(), values_to = "factor") %>%
  mutate(factor = str_squish(factor)) %>%
  filter(!is.na(factor), factor != "", str_detect(factor, "[A-Za-z]")) %>%
  distinct(factor) %>%
  arrange(factor) %>%
  pull(factor)

# Helper function - hex grid with meters input
# Build a hex grid over a borough in EPSG:2263 and also return a grid clipped to the borough
build_hex_grid = function(boro_sf, hex_size_m = 600) {
  boro_2263 = sf::st_transform(boro_sf, 2263)
  m_to_ft = 3.2808399
  cell_ft= hex_size_m * m_to_ft
  
  grid = sf::st_make_grid(boro_2263, cellsize = cell_ft, square = FALSE) |>
    sf::st_as_sf() |>
    dplyr::mutate(grid_id = dplyr::row_number())
  
  grid_clip = sf::st_intersection(grid, boro_2263) |>
    dplyr::mutate(grid_id = dplyr::row_number())  # new ids after clipping
  
  list(grid = grid, grid_clip = grid_clip, boro_2263 = boro_2263)
}


# UI
ui = fluidPage(
  titlePanel("NYC Crashes - Weekday vs Weekend Twin Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("borough_tc", "Borough", choices = available_borough, selected = "Brooklyn"),
      selectInput("factor", "Contributing factor", choices = factor_choices,
                  selected = "Driver Inattention/Distraction"),
      selectInput("year", "Year", choices = available_years,
                  selected = max(available_years, na.rm = TRUE)),
      numericInput("hex_m", "Hex size (meters)", value = 600, min = 200, max = 1500, step = 50),
      helpText("Brush hours on the line plot to filter both maps.")
    ),
    mainPanel(
      plotOutput("hourPlot", height = 240,
                 brush = brushOpts(id = "hourBrush", direction = "x")),
      fluidRow(
        column(6,
               h4("Weekday"),
               tmapOutput("mapWeekday", height = "520px")),
        column(6,
               h4("Weekend"),
               tmapOutput("mapWeekend", height = "520px"))
      ),
      br(),
      tableOutput("kpis")
    )
  )
)


# Server
server = function(input, output, session) {
  
  # Filter by borough/year/factor
  crashes_filtered = reactive({
    req(input$borough_tc, input$factor, input$year)
    crashes_all %>%
      filter(borough_tc == input$borough_tc,
             year == as.integer(input$year)) %>%
      filter(if_any(factor1:factor5, ~ .x == input$factor))
  })
  
  # Brushed hour window (defaults 0–23)
  hour_window = reactive({
    b = input$hourBrush
    if (is.null(b)) c(0L, 23L) else c(max(0L, floor(b$xmin)), min(23L, ceiling(b$xmax)))
  })
  
  # Hour-of-day profile
  output$hourPlot = renderPlot({
    df = crashes_filtered()
    validate(need(nrow(df) > 0, "No rows for this selection. Try a different factor/year."))
    
    htab = df %>% dplyr::count(hour, name = "n")
    
    ggplot(htab, aes(hour, n)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = 0:23) +
      labs(
        title = sprintf("%s — %s (%d)", input$borough_tc, input$factor, as.integer(input$year)),
        x = "Hour of day", y = "Crash count"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # Selected borough shape & hex grid
  boro_selected = reactive({
    boroughs_sf %>% filter(boro_name == input$borough_tc)
  })
  hex_data = reactive({
    build_hex_grid(boro_selected(), hex_size_m = input$hex_m)
  })
  
  # Aggregate to grid n Weekday/Weekend within brushed hours
  grid_summary = reactive({
    df = crashes_filtered()
    validate(need(nrow(df) > 0, "No rows for this selection."))
    
    # Filter to brushed hours
    hr = hour_window()
    df = df %>% dplyr::filter(hour >= hr[1], hour <= hr[2])
    
    # Weekend flag (Sun=1, Sat=7)
    df = df %>%
      dplyr::mutate(
        date_time  = as.POSIXct(paste0(crash_date, " ", sprintf("%02d:00:00", hour)),
                                tz = "America/New_York"),
        is_weekend = lubridate::wday(date_time) %in% c(1, 7)
      )
    
    hg = hex_data()  # has $grid, $grid_clip, $boro_2263
    
    # Points - sf - project - clip to the borough polygon
    pts_sf = df %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
      sf::st_transform(2263) %>%
      sf::st_intersection(hg$boro_2263)
    
    # If nothing in the brush, return an empty-count layer (so maps still render)
    if (nrow(pts_sf) == 0) {
      return(hg$grid_clip %>%
               dplyr::mutate(n_weekday = 0L, n_weekend = 0L))
    }
    
    # Join points to the (unclipped) grid to compute per-cell counts
    pts_joined = sf::st_join(pts_sf, hg$grid, join = sf::st_within, left = FALSE)
    
    agg = pts_joined %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(week_part = dplyr::if_else(is_weekend, "Weekend", "Weekday")) %>%
      dplyr::count(grid_id, week_part, name = "n") %>%
      tidyr::pivot_wider(names_from = week_part, values_from = n, values_fill = 0) %>%
      dplyr::rename(n_weekday = Weekday, n_weekend = Weekend)
    
    # attach counts to the clipped grid so only in-borough hexes draw
    hg$grid_clip %>%
      dplyr::left_join(agg, by = "grid_id") %>%
      dplyr::mutate(
        n_weekday = tidyr::replace_na(n_weekday, 0L),
        n_weekend = tidyr::replace_na(n_weekend, 0L)
      )
  })
  
  
  # Shared color breaks so legends are comparable
  common_breaks = reactive({
    g = grid_summary()
    vals = c(g$n_weekday, g$n_weekend)
    if (length(vals) == 0 || all(is.na(vals)) || max(vals, na.rm = TRUE) == 0) return(c(0, 1))
    qs = quantile(vals, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
    qs = unique(qs)
    if (length(qs) < 3) qs = c(0, 1, 2, 3, 5, 8, 13)
    qs
  })
  
  # Weekday map
  output$mapWeekday = renderTmap({
    g_all = grid_summary()
    brks  = common_breaks()
    
    # outlines for every hex and fills only where count > 0
    g_bd = g_all
    g_nz = g_all %>% dplyr::mutate(count = n_weekday) %>% dplyr::filter(count > 0)
    
    # avoid zero bin in the fill breaks
    brks_nz = brks[brks > 0]
    if (length(brks_nz) < 2) {
      mx = ifelse(nrow(g_nz) == 0, 1, max(g_nz$count, na.rm = TRUE))
      brks_nz = unique(c(1, pretty(c(1, mx), n = 5)))
    }
    
    tm_basemap("CartoDB.Positron") +
      # 1) constant light outlines so zeros are visible as a grid
      tm_shape(g_bd) + tm_borders(col = "grey60", lwd = 0.35, alpha = 0.9) +
      # 2) semi-transparent fills for non-zero cells (red = highest)
      { if (nrow(g_nz) > 0)
        tm_shape(g_nz) +
          tm_polygons(
            "count",
            breaks = brks_nz,
            palette = "YlOrRd",
            alpha = 0.55,  # =- transparency so streets show through
            border.alpha = 0,
            id = "count", popup.vars = TRUE
          )
        else tm_shape(g_bd) + tm_empty() } +
      tm_view(legend.position = c("left","bottom")) +
      tm_layout(
        inner.margins = c(0.02, 0.02, 0.18, 0.02),   # space for bottom legend
        legend.bg.color = "white",
        legend.bg.alpha = 0.6,
        legend.text.size = 0.65,
        legend.title.size = 0.85
      )
  })
  
  
  # Weekend map
  output$mapWeekend = renderTmap({
    g_all = grid_summary()
    brks  = common_breaks()
    
    g_bd = g_all
    g_nz = g_all %>% dplyr::mutate(count = n_weekend) %>% dplyr::filter(count > 0)
    
    brks_nz = brks[brks > 0]
    if (length(brks_nz) < 2) {
      mx = ifelse(nrow(g_nz) == 0, 1, max(g_nz$count, na.rm = TRUE))
      brks_nz = unique(c(1, pretty(c(1, mx), n = 5)))
    }
    
    tm_basemap("CartoDB.Positron") +
      tm_shape(g_bd) + tm_borders(col = "grey60", lwd = 0.35, alpha = 0.9) +
      { if (nrow(g_nz) > 0)
        tm_shape(g_nz) +
          tm_polygons(
            "count",
            breaks = brks_nz,
            palette = "YlOrRd",
            alpha = 0.55,
            border.alpha = 0,
            id = "count", popup.vars = TRUE
          )
        else tm_shape(g_bd) + tm_empty() } +
      tm_view(legend.position = c("left","bottom")) +
      tm_layout(
        inner.margins = c(0.02, 0.02, 0.18, 0.02),
        legend.bg.color = "white",
        legend.bg.alpha = 0.6,
        legend.text.size = 0.65,
        legend.title.size = 0.85
      )
  })
  
  
  # Tiny KPI table
  output$kpis = renderTable({
    df = crashes_filtered()
    hr = hour_window()
    df = df %>% filter(hour >= hr[1], hour <= hr[2]) %>%
      mutate(is_weekend = wday(as.POSIXct(paste0(crash_date, " ", sprintf("%02d:00:00", hour)),
                                          tz = "America/New_York")) %in% c(1, 7))
    tibble(
      Borough = input$borough_tc,
      Factor  = input$factor,
      Year    = input$year,
      Hours   = sprintf("%02d–%02d", hr[1], hr[2]),
      Crashes_Total   = nrow(df),
      Crashes_Weekday = sum(!df$is_weekend, na.rm = TRUE),
      Crashes_Weekend = sum(df$is_weekend,  na.rm = TRUE)
    )
  }, bordered = TRUE, striped = TRUE, digits = 0)
}

shinyApp(ui, server)
