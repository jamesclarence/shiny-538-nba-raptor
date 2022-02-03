# Shiny App: 538 NBA Raptor
# https://projects.fivethirtyeight.com/nba-player-ratings/


# https://shiny.rstudio.com/tutorial/
# runExample("01_hello")

# Load packages
# library(shiny)
# library(tidyverse)
# library(rvest)
# library(janitor)


# Define UI ----
ui <- fluidPage(
  titlePanel("Shiny App: FiveThirtyEight's Best NBA Players"),
  
  sidebarLayout(sidebarPanel(
    h2("A Shiny app by James Fisher")
  ),
                mainPanel(
                  h1("FiveThirtyEight's Best NBA Players, According to Raptor"),
                  br(),
                  p("For FiveThirtyEight's RAPTOR site, ",
                    a("click here.", 
                      href = "https://projects.fivethirtyeight.com/nba-player-ratings/"))
                )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

# https://github.com/fivethirtyeight/data/tree/master/nba-raptor

# Current 2022 season RAPTOR
# https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_player.csv
# 
# current_raptor_url <- "https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_player.csv"
# 
# # Read CSV and add today's date
# current_raptor <- read_csv(current_raptor_url) %>% mutate(date=Sys.Date())