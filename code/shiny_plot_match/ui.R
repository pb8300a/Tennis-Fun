library(shiny)

source("C:/Users/F400563/Desktop/tennis/global_info.R")


ui = fluidPage(
        tabsetPanel(
                tabPanel(title = "Plot a Match",
                         sidebarLayout(
                            sidebarPanel(                                         
                                    tags$head(
                                    tags$style(type="text/css", "select { max-width: 300px; }"),
                                    tags$style(type="text/css", ".span4 { max-width: 300px; }"),
                                    tags$style(type="text/css", ".well { max-width: 300px; }")
                                    ),
                                    actionButton(inputId = "start",
                                                 label = "Update"
                                    ),
                                    br(),
                                    br(),
                                    selectInput(inputId = "player1",
                                        label = "Player 1",
                                                choices = player_choices,
                                                selected = initial_player_1,
                                                multiple = FALSE
                                    ),
                                    selectInput(inputId = "player2",
                                                label = " Player 2",
                                                choices = player_choices,
                                                selected = initial_player_2,
                                                multiple = FALSE
                                    ),
                                    selectInput(inputId = "tournament",
                                                label = "Tournament",
                                                choices = tournament_choices,
                                                selected = initial_tournament,
                                                multiple = FALSE
                                    ),
                                    selectInput(inputId = "year",
                                                label = "Year",
                                                choices = year_choices,
                                                selected = initial_year,
                                                multiple = FALSE
                                    )
                            ),
                            mainPanel(
                                    fluidRow(
                                            br()
                                    ),
                                    fluidRow(
                                             plotOutput("plot_match",width = "100%", height = 800,
                                                       brush = brushOpts(id = "plot_brush",fill = "green",clip = FALSE))
                                    )
                            )
                    )
                )         
        )
        
)
