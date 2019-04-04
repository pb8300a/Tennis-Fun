
server <- function(input, output,session) {
        
        ##########################################################
        #### Make Tournament Choices reflect Players selected ####
        ##########################################################
        
        observe({input$player1
                #Update player2 options based on player1 selection
                some_match = match_metadata[(match_metadata$Player.1 == input$player1) | (match_metadata$Player.2 == input$player1),]
                player2choices = names(table(some_match$Player.2)[table(some_match$Player.2)!=0])
                player2choices = player2choices[player2choices!=input$player1]
                updateSelectInput(session,inputId = "player2",choices = player2choices)
                
        })
        
        observe({input$player2      
                #Update tournament options based on player2 selection
                matches_played = match_metadata[(match_metadata$Player.1 == input$player1 & match_metadata$Player.2 == input$player2) |
                                                        (match_metadata$Player.1 == input$player2 & match_metadata$Player.2 == input$player1),]
                tournaments = table(matches_played$Tournament)[table(matches_played$Tournament) != 0]
                tourneychoices = (names(tournaments))
                updateSelectInput(session, inputId="tournament", choices = tourneychoices)
        })
        
        observe({input$tournament      
                #Update year options based on tournament
                matches_played = match_metadata[(match_metadata$Player.1 == input$player1 & match_metadata$Player.2 == input$player2) |
                                                        (match_metadata$Player.1 == input$player2 & match_metadata$Player.2 == input$player1),]
                year_data = matches_played[matches_played$Tournament == input$tournament,]
                year = substr(year_data$Date,1,4)
                years = table(year)[table(year) != 0]
                year_choices = as.numeric(names(years))
                updateSelectInput(session,inputId = "year",choices = year_choices)
        })
        

        ############################
        #### Get the Match Data ####
        ############################
        match_data = eventReactive(input$start, {
                #Initialze start year for use below
                #point_lvl = read.csv("charting-m-points.csv", header = TRUE)
                point_match1 = point_lvl[((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                          (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                           point_lvl$Tournament == input$tournament & substr(point_lvl$Date,1,4) == input$year,]
                point_match1 = point_match1[order(point_match1$Pt),]
                finaldata = point_match1
                
        },ignoreNULL = FALSE
        )
        
        #############################
        #### Plot the Match Data ####
        #############################
        get_match = function(){
                match_data_plt = match_data()
                set_counter = table(rowSums(match_data_plt[,c("Set1","Set2")]))
                player_name = c(as.character(match_data_plt$Player.1)[1],as.character(match_data_plt$Player.2)[1])
                
                par(mfrow =c(length(set_counter),1),mar = c(1,5,3,3))
                par(mfrow =c(5,1),mar = c(1,5,3,3))
                
                for(i in names(set_counter)){
                        #i = "0"
                        point_set=match_data_plt[rowSums(match_data_plt[,c("Set1","Set2")])==as.numeric(i),]
                        
                        start_serving = point_set$Svr[1]
                        plot(point_set$PtWinner, type = "p", pch = 16,ylim = c(0,3), cex = 1.25,
                             yaxt = "n",ylab = "",xlab = "",xaxt = "n",xaxs = "r", 
                             main = paste("Set", as.numeric(i)+1))
                        
                        axis(side = 2, at = c(0.75,2.25), labels = c(sub(" ", "\n", player_name)),las = 2, tck = 0,cex = 1.5)
                        abline(h=1.5)
                        
                        ########################################################################
                        #### At the game's last point, how many games has each player won? ####
                        ########################################################################
                        realgame = as.numeric(substr(as.character(point_set$Gm.),1,2)) #to clean cases where Gm. ~ '14 (1) ' 
                        last_pt = aggregate(point_set$Pt, by = list(realgame), FUN = tail, n = 1)
                        game_1 = aggregate(point_set$Gm1.1, by = list(realgame), FUN = tail, n = 1)
                        game_2 = aggregate(point_set$Gm2.1, by = list(realgame), FUN = tail, n = 1)
                        server =aggregate(point_set$Svr, by = list(realgame), FUN = tail, n = 1)
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
                                text(x=midpoint,y = 0.5, all$Gm1[count],cex = 1.5)
                                text(x=midpoint,y = 2.5, all$Gm2[count],cex = 1.5)
                                if(all$server[count]==1){text(x=midpoint,y = 0.2, "(s)",cex = 1.25)}
                                if(all$server[count]==2){text(x=midpoint,y = 2.8, "(s)",cex = .251)}
                                
                        }
                        
                }
        }
        #########################################################
        ### This is used to help determine the height of the
        ### residual plots. We need to know the number of plots 
        ### to know how large to make the graph window.
        #########################################################
        
        number_of_plots = eventReactive(input$start,{
                match_data_plt = match_data()
                numplots = length(table(rowSums(match_data_plt[,c("Set1","Set2")])))
                },ignoreNULL = FALSE)
        
        number_plots_func = function(){
                return(number_of_plots()*200)
                
        }
        
        #####Output Plot#####
        output$plot_match = renderPlot({
                 plot(c(1,1))
                 input$start
                 get_match() 
         },
         #height = number_plots_func,
         height = 1000,
         
         width = 800,
         units = "px")
}
