# Shiny App: 538 NBA Raptor
# https://projects.fivethirtyeight.com/nba-player-ratings/


# https://shiny.rstudio.com/tutorial/
# runExample("01_hello")

# Load packages -----
library(shiny)
library(tidyverse)

# Source helper functions -----
#source("helpers.R")

# Download 538 RAPTOR data ------------------------------------------------

# Current RAPTOR CSV
current_raptor_url <- "https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_team.csv"

# Read CSV and add today's date
current_raptor <- read_csv(current_raptor_url) %>% mutate(date=Sys.Date())

# Define UI ----
ui <- fluidPage(
  titlePanel("Shiny App: FiveThirtyEight's Best NBA Players"),
  
  sidebarLayout(sidebarPanel(
    h2("A Shiny app by James Fisher"),
    selectInput("var", 
                label = "NBA Teams",
                choices = list("Percent White", 
                               "Percent Black",
                               "Percent Hispanic", 
                               "Percent Asian"),
                selected = "Percent White"),
    
    sliderInput("range", 
                label = "Minimum Minutes Played",
                min = 0, max = 5000, value = c(0, 5000)),
    textInput("Search Player", label = h3("Search for a Player"), value = "Search for a player..."),
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

  # Change player list by Team
  # output$var <- 
  
  # Change player list by Minute Range  
  output$range <- renderText({ 
    paste("You have chosen a minutes range from",
          input$range[1], "to", input$range[2])
  })
  
  # Render a data table of all results from current_raptor data frame
  output$table <- renderTable(
    current_raptor
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)


# Design
# sliderInput - Minutes
# selectInput - Teams
# textInput - Names




# https://github.com/fivethirtyeight/data/tree/master/nba-raptor

# Current 2022 season RAPTOR
# https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_player.csv
# 
# current_raptor_url <- "https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_player.csv"
# 
# # Read CSV and add today's date
# current_raptor <- read_csv(current_raptor_url) %>% mutate(date=Sys.Date())