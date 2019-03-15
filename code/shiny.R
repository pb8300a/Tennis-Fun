library(shiny)

player_choices = c("Roger Federer", "Novak Djokovic",   "Rafael Nadal", "Andy Murray")
initial_player_1 = "Roger Federer"
initial_player_2 = "Rafael Nadal"
tournament_choices = c("US Open","Wimbledon","Roland Garros", "Australian Open")
initial_tournament = "Wimbledon"
year_choices = c(2005: 2017)
initial_year = 2008

setwd("C:/Users/F400563/Desktop/tennis")
raw_points = read.csv("charting-m-points.csv", header = TRUE)
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
point_lvl = merge(raw_points,match_metadata , by = "match_id")

ui = fluidPage(
        tabsetPanel(
                tabPanel(title = "Plot a Match",
                         sidebarLayout(
                            sidebarPanel(
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
                                    h3(textOutput("caption")),
                                    fluidRow(
                                            br(),
                                            tags$p("Tennis Match"),
                                            br()
                                    ),
                                    fluidRow(
                                            plotOutput("plot_match",width = "100%", height = 500,
                                                       brush = brushOpts(id = "plot_brush",fill = "green",clip = FALSE))
                                    ),
                                    width = 10
                            )
                    )
                )         
        )
        
)
server <- function(input, output,session) {
        match_data = eventReactive(input$start, {
                #Initialze start year for use below
                #point_lvl = read.csv("charting-m-points.csv", header = TRUE)
                point_match1 = point_lvl[((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                          (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                           point_lvl$Tournament == input$tournament & substr(point_lvl$Date,1,4) == input$year,]
                finaldata = point_match1
                
        },ignoreNULL = FALSE
        )
        
        get_match = function(){
                match_data_plt = match_data()
                set_counter = table(rowSums(match_data_plt[,c("Set1","Set2")]))
                player_name = c(as.character(match_data_plt$Player.1)[1],as.character(match_data_plt$Player.2)[1])
                
                par(mfrow =c(length(set_counter),1),mar = c(1,5,3,1))
                
                for(i in names(set_counter)){
                        #i = "0"
                        point_set=match_data_plt[rowSums(match_data_plt[,c("Set1","Set2")])==as.numeric(i),]
                        
                        start_serving = point_set$Svr[1]
                        plot(point_set$PtWinner, type = "p", pch = 16,ylim = c(0,3), cex = 0.5,
                             yaxt = "n",ylab = "",xlab = "",xaxt = "n",xaxs = "r", 
                             main = paste("Set", as.numeric(i)+1))
                        
                        axis(side = 2, at = c(0.75,2.25), labels = c(sub(" ", "\n", player_name)),las = 2, tck = 0)
                        abline(h=1.5)
                        last_pt = aggregate(point_set$Pt, by = list(point_set$Gm.), FUN = tail, n = 1)
                        game_1 = aggregate(point_set$Gm1.1, by = list(point_set$Gm.), FUN = tail, n = 1)
                        game_2 = aggregate(point_set$Gm2.1, by = list(point_set$Gm.), FUN = tail, n = 1)
                        server =aggregate(point_set$Svr, by = list(point_set$Gm.), FUN = tail, n = 1)
                        names(server)[2]="server"
                        all = merge(last_pt, game_1, by = "Group.1")
                        all = merge(all, game_2, by = "Group.1")
                        all = merge(all, server, by = "Group.1")
                        
                        names(all) = c("Group", "Pt", "Gm1","Gm2","server")
                        
                        if(i !="0"){all$Pt = all$Pt - all$Pt[1] +(all$Pt[1] - point_set$Pt[1] +1)}
                        all =all[order(as.numeric(as.character(all$Group))),]
                        count = 0
                        lastpoint=0
                        
                        for(j in all$Pt){
                                if(count != 0){lastpoint = all$Pt[count]}
                                midpoint = (lastpoint + j+1)/2
                                count =count+1
                                if(j != max(all$Pt)){abline(v = j+0.5, col = "gray")}
                                text(x=midpoint,y = 0.5, all$Gm1[count])
                                text(x=midpoint,y = 2.5, all$Gm2[count])
                                if(all$server[count]==1){text(x=midpoint,y = 0.2, "(s)",cex = 0.65)}
                                if(all$server[count]==2){text(x=midpoint,y = 2.8, "(s)",cex = 0.65)}
                                
                        }
                        
                }
        }
        
        
        #####Output Plot#####
        output$plot_match = renderPlot({
                input$start
                get_match()  
        })
}

#ddd = point_lvl[((point_lvl$Player.1 == "Roger Federer" & point_lvl$Player.2 == "Rafael Nadal") |
#                                  (point_lvl$Player.1 == "Rafael Nadal" & point_lvl$Player.2 == "Roger Federer")) &
#                                 point_lvl$Tournament == "Wimbledon" & substr(point_lvl$Date,1,4) == "2008",]

