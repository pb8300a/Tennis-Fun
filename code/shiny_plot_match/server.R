#setwd("C:/Users/F400563/Desktop/tennis")
#raw_points = read.csv("charting-m-points.csv", header = TRUE)
#match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
#point_lvl = merge(raw_points,match_metadata , by = "match_id")
#setwd("C:/Users/F400563/Desktop/tennis/point_shiny")

server <- function(input, output,session) {
        
        ############################
        #### Update UI Options   ###
        ############################
        observe({
                
                matches_played = match_metadata[(match_metadata$Player.1 == input$player1 & match_metadata$Player.2 == input$player2) |
                                                (match_metadata$Player.1 == input$player2 & match_metadata$Player.2 == input$player1),]
                tournaments = table(matches_played$Tournament)[table(matches_played$Tournament) != 0]
                tourneychoices = (names(tournaments))
                updateSelectInput(session, inputId="tournament", choices = tourneychoices,
                                  selected = input$tournament)
                
                year_data = matches_played[matches_played$Tournament == input$tournament,]
                year = substr(year_data$Date,1,4)
                years = table(year)[table(year) != 0]
                year_choices = names(years)
                updateSelectInput(session,inputId = "year",choices = year_choices,
                                  selected = input$year)
                
                
                point_match = point_lvl[((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                                  (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                                 point_lvl$Tournament == input$tournament & substr(point_lvl$Date,1,4) == input$year,]
                point_numbers = point_match[,"Pt"]
                updateSelectInput(session,inputId = "pointnumber",choices = point_numbers,
                                  selected = input$pointnumber)

        })

        ############################
        #### Get the Match Data ####
        ############################
        match_data = eventReactive(input$selectpoint, {
                #vals <- reactiveValues(shotnumber = 0)
                
                point_match1 = point_lvl[(((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                          (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                          (point_lvl$Tournament == input$tournament) & 
                                          (substr(point_lvl$Date,1,4) == input$year) &
                                          (point_lvl$Pt == input$pointnumber)),]
                finaldata = point_match1
                
        },ignoreNULL = FALSE
        )
        
        rallylength = function(){
                match_data_plt = match_data()
                rally_len = nchar(as.character(match_data_plt$rallyNoDirection))
        }
        #############################
        #### Get Shot Number     ####
        #############################

        vals <- reactiveValues(shotnumber = 0)
        observeEvent(input$selectpoint, {vals$shotnumber = 0})
        observeEvent(input$prevshot, {vals$shotnumber = max(0,vals$shotnumber - 1)})
        observeEvent(input$nextshot, {vals$shotnumber = min(rallylength(),vals$shotnumber + 1)})
        
      

        plot_pt = function(){
                match_data_plt = match_data()
                tmp <- data.frame(matrix(nrow=as.numeric(as.character(match_data_plt$rallyCount)), ncol=3)) 
                tmp$X2 = 100
                tmp$X3 = 100
                
                rally = as.character(match_data_plt$rallyNoDirection)
                point = as.character(match_data_plt$rallyNoSpec)
        
                numeric = strsplit(gsub("[[:alpha:]]"," ",point)," ")[[1]]
                numeric = numeric[-1]
                numeric[numeric==""] = "100"
                if(length(numeric)<nchar(rally)){numeric = c(numeric, "100")}        

                for(i in 1:nchar(rally)){
                        tmp[i,1] =  substr(rally,i,i)
                        if(nchar(numeric[i])==1){
                                tmp[i,2] = numeric[i]
                                tmp[i,3] = 0
                        }
                        if(nchar(numeric[i])==2){
                                tmp[i,2] = substr(numeric[i],1,1)
                                tmp[i,3] = substr(numeric[i],2,2)
                        }
                }
                rect(20,35,120,42,col = "white",border = "white")
                blank_court()
                lines_court()
                if(vals$shotnumber %% 2 == 0 & vals$shotnumber >0 &
                  (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally))))
                        { ##Returner shot
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_deuce();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_deuce_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_deuce_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_deuce_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_mid_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_mid_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_mid_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_ad();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_ad_short()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_ad_mid();lines_court();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_ad_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==100){blank_court();lines_court()}  #Cases where the shot location is not given
                }else if(vals$shotnumber %% 2 == 1 & vals$shotnumber >0 &
                         (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally))))
                        {  ##server shot
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_deuce();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_deuce_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_deuce_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_deuce_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_mid_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_mid_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_mid_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_ad();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_ad_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_ad_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_ad_deep();lines_court()}   
                        if(as.numeric((tmp$X2[vals$shotnumber]))==100){blank_court();lines_court()}  #Cases where the shot location is not given
                        
                }
                        if(tmp$X1[vals$shotnumber]=="b"){text(x=60,y=40, "Backhand")}
                if(tmp$X1[vals$shotnumber]=="f"){text(x=60,y=40, "Forehand")}
                if(tmp$X1[vals$shotnumber]=="r"){text(x=60,y=40, "Forehand Slice")}
                if(tmp$X1[vals$shotnumber]=="s"){text(x=60,y=40, "Backhand Slice")}
                if(tmp$X1[vals$shotnumber]=="v"){text(x=60,y=40, "Forehand Volley")}
                if(tmp$X1[vals$shotnumber]=="z"){text(x=60,y=40, "Backhand Volley")}
                if(tmp$X1[vals$shotnumber]=="o"){text(x=60,y=40, "Overhead")}
                if(tmp$X1[vals$shotnumber]=="p"){text(x=60,y=40, "Backhand Overhead")}
                if(tmp$X1[vals$shotnumber]=="u"){text(x=60,y=40, "Forehand Drop Shot")}
                if(tmp$X1[vals$shotnumber]=="y"){text(x=60,y=40, "Backhand Drop Shot")}
                if(tmp$X1[vals$shotnumber]=="l"){text(x=60,y=40, "Forehand Lob")}
                if(tmp$X1[vals$shotnumber]=="m"){text(x=60,y=40, "Backhand Lob")}
                if(tmp$X1[vals$shotnumber]=="h"){text(x=60,y=40, "Forehand Half Volley")}
                if(tmp$X1[vals$shotnumber]=="i"){text(x=60,y=40, "Backhand Half Volley")}
                if(tmp$X1[vals$shotnumber]=="j"){text(x=60,y=40, "Forehand Swinging Volley")}
                if(tmp$X1[vals$shotnumber]=="k"){text(x=60,y=40, "Backhand Swinging Volley")}
                        
                        
                                
        }

        serve_pt = function(){
                match_data_plt = match_data()
                first_serve = as.numeric(substr(match_data_plt$Sv1,1,1))
                second_serve = as.numeric(substr(match_data_plt$Sv2,1,1))
                blank_court()
                lines_court()
                if(is.na(second_serve)){
                        if(match_data_plt$Pts %in% c("0-0","15-15","30-0","0-30","15-40","40-15","30-30","40-40")){
                                if(first_serve ==4){
                                        serve_deuce_wide()
                                        text(x=60,y=40, "First Serve out Wide")
                                        }
                                if(first_serve ==5){serve_deuce_body()
                                        text(x=60,y=40, "First Serve to the Body")
                                        }
                                if(first_serve ==6){serve_deuce_t()
                                        text(x=60,y=40, "First Serve down the T")
                                        }   
                        }else{
                                if(first_serve ==4){serve_ad_wide()
                                        text(x=60,y=40, "First Serve out Wide")
                                        }
                                if(first_serve ==5){serve_ad_body()
                                        text(x=60,y=40, "First Serve to the Body")
                                        }
                                if(first_serve ==6){serve_ad_t()
                                        text(x=60,y=40, "First Serve down the T")
                                        }     
                        }
                        
                }else{
                        if(match_data_plt$Pts %in% c("0-0","15-15","30-0","0-30","15-40","40-15","30-30","40-40")){
                                if(second_serve ==4){serve_deuce_wide()
                                        text(x=60,y=40, "First Serve out Wide")
                                        }
                                if(second_serve ==5){serve_deuce_body()
                                        text(x=60,y=40, "First Serve to the Body")
                                        }
                                if(second_serve ==6){serve_deuce_t()
                                        text(x=60,y=40, "First Serve down the T")
                                        }   
                        }else{
                                if(second_serve ==4){serve_ad_wide()
                                        text(x=60,y=40, "Second Serve out Wide")
                                        }
                                if(second_serve ==5){serve_ad_body()   
                                        text(x=60,y=40, "Second Serve to the Body")
                                        }
                                if(second_serve ==6){serve_ad_t()
                                        text(x=60,y=40, "Second Serve down the T")
                                        }     
                        } 
                } 
        }
        
        
        output$plot_point = renderPlot({
                input$prevshot
                if(vals$shotnumber == 0){
                    serve_pt()
                }else{
                    plot_pt()
                }
                match_data_plt=match_data()
                
                text(x = 50,y = 20, vals$shotnumber)
                text(x = 50,y = -5, match_data_plt$Rally)
                
                if(vals$shotnumber == rallylength()){
                        if(match_data_plt$PtWinner==1){text(x = 60, y = 35, paste(match_data_plt$Player.1,"wins point",sep = " "))}
                        if(match_data_plt$PtWinner==2){text(x = 60, y = 35, paste(match_data_plt$Player.2,"wins point",sep = " "))}
                        if(match_data_plt$isRallyWinner==1){text(x = 60, y = 33, "due to a winner")}
                        if(match_data_plt$isForced==1){text(x = 60, y = 33, "due to a forced error")}
                        if(match_data_plt$isUnforced==1){text(x = 60, y = 33, "due to an unforced error")}
                        if(match_data_plt$isAce==1){text(x = 60, y = 33, "with an ace")}
                        if(match_data_plt$isDouble==1){text(x = 60, y = 33, "due to a double fault")}
                        
                        
                }
                if(match_data_plt$Svr ==1){
                        text(x = 20, y = 35, match_data_plt$Player.1)
                        text(x = 100, y = 35, match_data_plt$Player.2)
                }else{
                        text(x = 20, y = 35, match_data_plt$Player.2)
                        text(x = 100, y = 35, match_data_plt$Player.1)
                }
        },height = 500,width = 800,units = "px")
        
        output$plot_point = renderPlot({
                input$nextshot
                if(vals$shotnumber == 0){
                   serve_pt()
                }else{
                   plot_pt()
                }
                match_data_plt=match_data()
                text(x = 50,y = 20, vals$shotnumber)
                text(x = 50,y = -5, match_data_plt$Rally)
                
                if(vals$shotnumber == rallylength()){
                        if(match_data_plt$PtWinner==1){text(x = 60, y = 35, paste(match_data_plt$Player.1,"wins point",sep = " "))}
                        if(match_data_plt$PtWinner==2){text(x = 60, y = 35, paste(match_data_plt$Player.2,"wins point",sep = " "))}
                        if(match_data_plt$isRallyWinner==1){text(x = 60, y = 33, "due to a winner")}
                        if(match_data_plt$isForced==1){text(x = 60, y = 33, "due to a forced error")}
                        if(match_data_plt$isUnforced==1){text(x = 60, y = 33, "due to an unforced error")}
                }
                if(match_data_plt$Svr ==1){
                        text(x = 20, y = 35, match_data_plt$Player.1)
                        text(x = 100, y = 35, match_data_plt$Player.2)
                }else{  text(x = 20, y = 35, match_data_plt$Player.2)
                        text(x = 100, y = 35, match_data_plt$Player.1)
                }
        },height = 500,width = 800,units = "px")
        output$selected_var <- renderText({ 
                match_data_plt = match_data()
                names(match_data_plt)
        })
        
}
