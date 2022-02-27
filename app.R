# Shiny App: 538 NBA Raptor
# https://projects.fivethirtyeight.com/nba-player-ratings/

# Load packages -----
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(gt)
library(gtExtras)

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
  arrange(team, desc(war_total), desc(raptor_total), desc(raptor_offense), desc(raptor_defense)) %>% 
  mutate(rank_nba = min_rank(desc(war_total))) %>%
  group_by(team) %>% 
  mutate(rank_team = min_rank(desc(war_total))) %>% 
  ungroup() %>% 
  select(rank_nba, rank_team, everything())

# Define UI ----
ui <- fluidPage(
  titlePanel("Shiny App: FiveThirtyEight's Best NBA Players, According to Raptor"),
  
  sidebarLayout(sidebarPanel(
    h2("A Shiny app by James Fisher"),
    sliderInput("range", 
                label = "Minimum Minutes Played",
                min = 0, 
                max = max(current_raptor$mp), 
                value = c(815, max(current_raptor$mp))),
    br(),
    selectInput("team", 
                label = "NBA Teams",
                choices = c("All", current_raptor$team)
                ),
      p("For FiveThirtyEight's RAPTOR site, ",
        a("click here.", 
          href = "https://projects.fivethirtyeight.com/nba-player-ratings/")),
      p("RAPTOR: Robust Algorithm (using) Player Tracking (and) On/Off Ratings.
        According to 538, RAPTOR is 'a plus-minus statistic that measures the number of points a player contributes to his team's offense and defense per 100 possessions, relative to a league-average player.'"),
      p("WAR: Wins Above Replacement. How many wins a player will contribute compared to a replacement-level player, which 538 determines to be -2.75 points per 100 possessions.")
  ),
    mainPanel(
      gt_output(outputId = "table") # GT Table
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
    current_raptor[rows,,drop = FALSE] %>% arrange(desc(war_total), desc(raptor_total), desc(raptor_offense), desc(raptor_defense))
  })
  
  output$table <-
    render_gt(
      expr = filtered() %>% 
        gt() %>% # add gt table
        fmt_number(
          columns = 6:9,
          decimals = 1
        ) %>% 
        gt_theme_538() %>% 
        tab_spanner(
          label = "OVERALL RAPTOR",
          columns = c(raptor_total, raptor_offense, raptor_defense)) %>% 
        tab_spanner(
          label = "WAR RANKING",
          columns = c(rank_nba, rank_team)
      ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "darkgrey"),
            cell_text(weight = "bold",
                      color = "white")
          ),
          locations = cells_column_labels(
            columns = war_total
          )
        ) %>% 
        data_color(
          columns = c(raptor_total, raptor_offense, raptor_defense),
          colors = scales::col_numeric(
            palette = c("pink", "white", "lightblue"),
            domain = NULL)
        ) %>% 
        cols_align(
          align = "center"
        )
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)

# Chart
# current_raptor %>%
#   filter(mp > 829) %>% 
#   ggplot() + 
#   geom_rect(aes(
#     xmin = 0, xmax = 10, ymin = 0, ymax = 10),
#     fill = "lightblue"
#   ) +  
#   geom_rect(aes(
#     xmin = 0, xmax = -10, ymin = 0, ymax = -10),
#     fill = "pink"
#   ) +
#   geom_point(aes(x = raptor_offense,
#                  y = raptor_defense),
#              shape = 21, colour = "black", fill = "white", size = 5, stroke = 2) +
#   xlim(-10, 10) +
#   ylim(-10, 10)
