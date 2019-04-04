library(shiny)
library(shinyWidgets)
source("C:/Users/F400563/Desktop/tennis/point_shiny/court_squares.R")

ui = fluidPage(
        tabsetPanel(
                tabPanel(title = "Plot a Point",
                         sidebarLayout(
                            sidebarPanel(                                         
                                    tags$head(
                                    tags$style(type="text/css", "select { max-width: 300px; }"),
                                    tags$style(type="text/css", ".span4 { max-width: 300px; }"),
                                    tags$style(type="text/css", ".well { max-width: 300px; }")
                                    ), 
                                    actionButton(inputId = "selectpoint",
                                                 label = "Select Point",
                                                 style = 'padding:5px;font-size:100%;font-weight: bold;'),
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
                                    ),
                                    selectInput(inputId = "pointnumber",
                                                label = "Score (Match, Set, Game)",
                                                choices = point_choices,
                                                selected = initial_point,
                                                multiple = FALSE
                                    ),
                                    br(),
                                    tags$h3("Watch the Point"),
                                    actionButton(inputId = "nextshot",
                                                 label = "Next Shot"
                                    ),
                                    actionButton(inputId = "prevshot",
                                                 label = "Prior Shot"
                                    )
                            ),
                            mainPanel(
                                     fluidRow(
                                             plotOutput("plot_point",width = "100%", height = 800,
                                                        brush = brushOpts(id = "plot_brush",fill = "green",clip = FALSE))
                                     )
                            )
                    )
                )         
        )
        
)
