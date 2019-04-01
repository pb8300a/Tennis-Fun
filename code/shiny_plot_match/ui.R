library(shiny)
source("C:/Users/F400563/Desktop/tennis/point_shiny/court_squares.R")
setwd("C:/Users/F400563/Desktop/tennis")
#raw_points = read.csv("charting-m-points.csv", header = TRUE)
#match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
#point_lvl = merge(raw_points,match_metadata , by = "match_id")
setwd("C:/Users/F400563/Desktop/tennis/point_shiny")


player_choices = c("Roger Federer", "Novak Djokovic",   "Rafael Nadal", "Andy Murray")
initial_player_1 = "Andy Murray"
initial_player_2 = "Rafael Nadal"

#setwd("C:/Users/F400563/Desktop/tennis")
#match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
tournament_choices = c("US Open","Wimbledon","Roland Garros", "Australian Open")
tournament_choices = names(table(match_metadata$Tournament))

initial_tournament = "Australian Open"
year_choices = c(2005: 2010)
initial_year = 2010

point_choices=c(1:300)
initial_point = 1
#setwd("C:/Users/F400563/Desktop/tennis")
#raw_points = read.csv("charting-m-points.csv", header = TRUE)
#match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
#point_lvl = merge(raw_points,match_metadata , by = "match_id")

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
                                                label = "Point Number",
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
                                                        brush = brushOpts(id = "plot_brush",fill = "green",clip = FALSE)),
                                             textOutput("selected_var")
                                     ) 
                            )
                    )
                )         
        )
        
)
