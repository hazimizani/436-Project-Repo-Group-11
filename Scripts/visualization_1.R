## Load libraries

require(shiny)
require(dplyr)
require(ggplot2)

## Read in data

fp <- getwd()

crash_data <- read.csv(paste0(fp, '/Datasets/Motor_Vehicle_Collisions_-_Crashes_20251017.csv'))

# Clean crash data --------------------------------------------------------

analysis_df <- crash_data |> 
  mutate(date_time = with(crash_data, paste(CRASH.DATE, CRASH.TIME)) |> 
  lubridate::mdy_hm(tz = "EST"))

## Have NA values properly for boroughs
analysis_df$BOROUGH <- analysis_df$BOROUGH |> replace(analysis_df$BOROUGH == "", NA)
  
# skimr::skim(analysis_df)

analysis_df <- analysis_df[!is.na(analysis_df$BOROUGH),]

shiny_df <- analysis_df |> 
  mutate(year = year(date_time) |> as.numeric(),
         month = month(date_time) |> as.numeric(),
         day = day(date_time),
         hour = format(date_time, format = "%H") |> as.numeric()) |> 
  dplyr::select(all_of(c("BOROUGH", "date_time", "ZIP.CODE", "LATITUDE", "LONGITUDE", "year", "month", "day", "hour")))

# Functions -----------------------------------------------------------

my_line_graph_function <- function(dat){
  
  dat |> 
    ggplot(aes(x = hour,
               y = n_accidents,
               group = BOROUGH,
               col = BOROUGH)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom") +
    labs(title = "Number of Accidents by Time of Day")
  
}

# ui ----------------------------------------------------------------------

years <- shiny_df$year |> unique()
months <- shiny_df$month |> unique()

ui <- fluidPage(
  fluidRow(
    titlePanel("Accidents by Time of Day"),
    sliderInput("month", "Month", min = min(shiny_df$month), max = max(shiny_df$month), c(1, 12), sep = ""),
    sliderInput("year", "Year", min = min(shiny_df$year), max = max(shiny_df$year), c(2012, 2025), sep = ""),
    sliderInput("hour", "Hour", min = min(shiny_df$hour), max = max(shiny_df$hour), c(0, 23), sep = ""),
    plotOutput("line"))
)

# shiny_df |> 
#   group_by(BOROUGH, hour) |> 
#   summarize(n_accidents = n()) |> 
#   my_line_graph_function()

# server ------------------------------------------------------------------

server <- function(input, output) {
  
  ## browser()
  
  shiny_df_subset <- reactive({
    shiny_df %>%
      mutate(selected = 1 * (
        (hour >= input$hour[1]) &
          (hour <= input$hour[2]) &
          (month >= input$month[1]) &
          (month >= input$month[2]) &
          (year >= input$year[1]) &
          (year <= input$year[2])
      )) |> 
      filter(selected == 1) |> 
      group_by(BOROUGH, hour) |> 
      summarize(n_accidents = n())
  })
  
  output$line <- renderPlot(my_line_graph_function(shiny_df_subset()))
  
}

shinyApp(ui, server)