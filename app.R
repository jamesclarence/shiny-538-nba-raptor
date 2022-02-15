# Shiny App: 538 NBA Raptor
# https://projects.fivethirtyeight.com/nba-player-ratings/

# Load packages -----
library(shiny)
library(shinyWidgets)
library(tidyverse)

# Source helper functions -----
#source("helpers.R")

# Download 538 RAPTOR data ------------------------------------------------

# Current RAPTOR CSV
current_raptor_url <- "https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_team.csv"

# Read CSV and add today's date
current_raptor <- read_csv(current_raptor_url) %>% 
  select(player_name,
         team,
         mp,
         war_total,
         raptor_total,
         raptor_offense,
         raptor_defense
  ) %>% 
  arrange(team, desc(war_total, raptor_total, raptor_offense, raptor_defense))

# Define UI ----
ui <- fluidPage(
  titlePanel("Shiny App: FiveThirtyEight's Best NBA Players"),
  
  sidebarLayout(sidebarPanel(
    h2("A Shiny app by James Fisher"),
    sliderInput("range", 
                label = "Minimum Minutes Played",
                min = 0, max = max(current_raptor$mp), value = c(0, max(current_raptor$mp))),
    br(),
    selectInput("team", 
                label = "NBA Teams",
                choices = c("All", current_raptor$team)
                ),
  ),
    mainPanel(
      h1("FiveThirtyEight's Best NBA Players, According to Raptor"),
      br(),
      p("For FiveThirtyEight's RAPTOR site, ",
        a("click here.", 
          href = "https://projects.fivethirtyeight.com/nba-player-ratings/")),
      br(),
      textOutput("range"),
      br(),
      tableOutput("table") ## reference current_raptor data frame
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Change player list by Minute Range  
  output$range <- renderText({ 
    paste("You have chosen a minutes range from",
          input$range[1], "to", input$range[2])
  })
  
  # Change data based on minutes range and team selection
  # SO help: https://stackoverflow.com/questions/56319618/implement-select-all-option-in-reactive-shiny
  filtered <- reactive({
    rows <- (input$team == "All" | current_raptor$team == input$team) &
      (current_raptor$mp <= input$range[2] & current_raptor$mp >= input$range[1])
    current_raptor[rows,,drop = FALSE] %>% arrange(desc(war_total, raptor_total, raptor_offense, raptor_defense))
  })
  output$table <- renderTable(filtered())

}

# Run the app ----
shinyApp(ui = ui, server = server)
