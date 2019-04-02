
server <- function(input, output,session) {
        
        ############################
        #### Update UI Options   ###
        ############################
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
        observe({input$year
                #Update point options based on year
                point_match = point_lvl[((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                                  (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                                 point_lvl$Tournament == input$tournament & substr(point_lvl$Date,1,4) == input$year,]
                point_numbers = point_match[,c("Pt","score")]
                point_numbers = point_numbers[order(point_numbers$Pt),"score"]
                updateSelectInput(session,inputId = "pointnumber",choices = point_numbers)
        })

        ############################
        #### Get the Match Data ####
        ############################
        match_data = eventReactive(input$selectpoint, {
                validate(
                        need(length(input$year) >0,"Please select a valid year")
                )
                point_match1 = point_lvl[(((point_lvl$Player.1 == input$player1 & point_lvl$Player.2 == input$player2) |
                                          (point_lvl$Player.1 == input$player2 & point_lvl$Player.2 == input$player1)) &
                                          (point_lvl$Tournament == input$tournament) & 
                                          (substr(point_lvl$Date,1,4) == input$year) &
                                          (point_lvl$score == input$pointnumber)),]
                #point_match1 = point_match1[1,]
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
                if(vals$shotnumber %% 2 == 0 & vals$shotnumber >0 & match_data_plt$Svr == 1 & 
                  (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally))))
                        { ##server shot
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
                }else if(vals$shotnumber %% 2 == 1 & vals$shotnumber >0 & match_data_plt$Svr == 1 & 
                         (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally))))
                        {  ##returner shot
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
                }else if(vals$shotnumber %% 2 == 0 & vals$shotnumber >0 & match_data_plt$Svr == 2 & 
                    (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally)))){ ##server shot
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_deuce();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_deuce_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_deuce_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_deuce_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_mid_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_mid_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_mid_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==0){p2_ad();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==7){p2_ad_short()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==8){p2_ad_mid();lines_court();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==9){p2_ad_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==100){blank_court();lines_court()}  #Cases where the shot location is not given
                }else if(vals$shotnumber %% 2 == 1 & vals$shotnumber >0 & match_data_plt$Svr == 2 & 
                         (vals$shotnumber != nchar(rally) | (match_data_plt$isRallyWinner == 1 & vals$shotnumber == nchar(rally)))){  ##returner shot
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_deuce();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_deuce_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_deuce_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==1 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_deuce_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_mid_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_mid_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==2 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_mid_deep();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==0){p1_ad();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==7){p1_ad_short();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==8){p1_ad_mid();lines_court()}
                        if(as.numeric((tmp$X2[vals$shotnumber]))==3 & as.numeric(tmp$X3[vals$shotnumber])==9){p1_ad_deep();lines_court()}   
                        if(as.numeric((tmp$X2[vals$shotnumber]))==100){blank_court();lines_court()}  #Cases where the shot location is not given
                }
                
                
                
                else if(vals$shotnumber ==nchar(rally)){
                        if(length(grep("n",as.character(match_data_plt$Rally)))>0){blank_court();lines_court();lines(x=c(60,60),y = c(0,30),lty = 2,lwd = 3, col = "red")
                        }else if(length(grep("3w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_ad_wide_out();lines_court()
                        }else if(length(grep("3w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_ad_wide_out();lines_court()
                        }else if(length(grep("3w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_ad_wide_out();lines_court()
                        }else if(length(grep("3w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_ad_wide_out();lines_court()
                                
                        }else if(length(grep("1w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_deuce_wide_out();lines_court()
                        }else if(length(grep("1w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_deuce_wide_out();lines_court()
                        }else if(length(grep("1w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_deuce_wide_out();lines_court()
                        }else if(length(grep("1w",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_deuce_wide_out();lines_court()       
                            
                        }else if(length(grep("3x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_ad_wide_deep_out();lines_court()
                        }else if(length(grep("3x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_ad_wide_deep_out();lines_court()
                        }else if(length(grep("3x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_ad_wide_deep_out();lines_court()
                        }else if(length(grep("3x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_ad_wide_deep_out();lines_court()
                                
                        }else if(length(grep("1x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_deuce_wide_deep_out();lines_court()
                        }else if(length(grep("1x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_deuce_wide_deep_out();lines_court()
                        }else if(length(grep("1x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_deuce_wide_deep_out();lines_court()
                        }else if(length(grep("1x",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_deuce_wide_deep_out();lines_court()            
                                    
                        }else if(length(grep("1d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_deuce_deep_out();lines_court()
                        }else if(length(grep("1d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_deuce_deep_out();lines_court()
                        }else if(length(grep("1d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_deuce_deep_out();lines_court()
                        }else if(length(grep("1d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_deuce_deep_out();lines_court()       
                                
                        }else if(length(grep("2d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_mid_deep_out();lines_court()
                        }else if(length(grep("2d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_mid_deep_out();lines_court()
                        }else if(length(grep("2d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_mid_deep_out();lines_court()
                        }else if(length(grep("2d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_mid_deep_out();lines_court()
                                
                        }else if(length(grep("3d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 1){p1_ad_deep_out();lines_court()
                        }else if(length(grep("3d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 1){p2_ad_deep_out();lines_court()
                        }else if(length(grep("3d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 0 & match_data_plt$Svr == 2){p2_ad_deep_out();lines_court()
                        }else if(length(grep("3d",as.character(match_data_plt$Rally)))>0 & vals$shotnumber %% 2 == 1 & match_data_plt$Svr == 2){p1_ad_deep_out();lines_court()
                        }
                }
                
                #### label the type of shot ####
                if(tmp$X1[vals$shotnumber]=="b"){text(x=60,y=40,cex=1.25, "Backhand",col = "white")}
                if(tmp$X1[vals$shotnumber]=="f"){text(x=60,y=40,cex=1.25, "Forehand",col = "white")}
                if(tmp$X1[vals$shotnumber]=="r"){text(x=60,y=40,cex=1.25, "Forehand Slice",col = "white")}
                if(tmp$X1[vals$shotnumber]=="s"){text(x=60,y=40,cex=1.25, "Backhand Slice",col = "white")}
                if(tmp$X1[vals$shotnumber]=="v"){text(x=60,y=40,cex=1.25, "Forehand Volley",col = "white")}
                if(tmp$X1[vals$shotnumber]=="z"){text(x=60,y=40,cex=1.25, "Backhand Volley",col = "white")}
                if(tmp$X1[vals$shotnumber]=="o"){text(x=60,y=40,cex=1.25, "Overhead",col = "white")}
                if(tmp$X1[vals$shotnumber]=="p"){text(x=60,y=40,cex=1.25, "Backhand Overhead",col = "white")}
                if(tmp$X1[vals$shotnumber]=="u"){text(x=60,y=40,cex=1.25, "Forehand Drop Shot",col = "white")}
                if(tmp$X1[vals$shotnumber]=="y"){text(x=60,y=40,cex=1.25, "Backhand Drop Shot",col = "white")}
                if(tmp$X1[vals$shotnumber]=="l"){text(x=60,y=40,cex=1.25, "Forehand Lob",col = ",white")}
                if(tmp$X1[vals$shotnumber]=="m"){text(x=60,y=40,cex=1.25, "Backhand Lob",col = ",white")}
                if(tmp$X1[vals$shotnumber]=="h"){text(x=60,y=40,cex=1.25, "Forehand Half Volley",col = "white")}
                if(tmp$X1[vals$shotnumber]=="i"){text(x=60,y=40,cex=1.25, "Backhand Half Volley",col = "white")}
                if(tmp$X1[vals$shotnumber]=="j"){text(x=60,y=40,cex=1.25, "Forehand Swinging Volley",col = "white")}
                if(tmp$X1[vals$shotnumber]=="k"){text(x=60,y=40,cex=1.25, "Backhand Swinging Volley",col = "white")}
                        
        }

        serve_pt = function(){
                match_data_plt = match_data()
                first_serve = as.numeric(substr(match_data_plt$Sv1,1,1))
                second_serve = as.numeric(substr(match_data_plt$Sv2,1,1))
                blank_court()
                lines_court()
                if(is.na(second_serve)){
                        if(match_data_plt$Pts2 %in% c("0-0","15-15","30-0","0-30","15-40","40-15","30-30","40-40") |
                           length(grep("deuce",match_data_plt$Pts2)>0)){
                                if((first_serve ==4 & match_data_plt$Svr == 1)){p1_serve_deuce_wide();text(x=60,y=40,cex=1.25, "First Serve out Wide",col = "white")}
                                if((first_serve ==4 & match_data_plt$Svr == 2)){p2_serve_deuce_wide();text(x=60,y=40,cex=1.25, "First Serve out Wide",col = "white")}
                                if((first_serve ==5 & match_data_plt$Svr == 1)){p1_serve_deuce_body();text(x=60,y=40,cex=1.25, "First Serve to the Body",col = "white")}
                                if((first_serve ==5 & match_data_plt$Svr == 2)){p2_serve_deuce_body();text(x=60,y=40,cex=1.25, "First Serve to the Body",col = "white")}
                                if((first_serve ==6 & match_data_plt$Svr == 1)){p1_serve_deuce_t();text(x=60,y=40,cex=1.25, "First Serve down the T",col = "white")}   
                                if((first_serve ==6 & match_data_plt$Svr == 2)){p2_serve_deuce_t();text(x=60,y=40,cex=1.25, "First Serve down the T",col = "white")}   
                                
                        }else{
                                if((first_serve ==4 & match_data_plt$Svr == 1)){p1_serve_ad_wide();text(x=60,y=40,cex=1.25, "First Serve out Wide",col = "white")}
                                if((first_serve ==4 & match_data_plt$Svr == 2)){p2_serve_ad_wide();text(x=60,y=40,cex=1.25, "First Serve out Wide",col = "white")}
                                if((first_serve ==5 & match_data_plt$Svr == 1)){p1_serve_ad_body();text(x=60,y=40,cex=1.25, "First Serve to the Body",col = "white")}
                                if((first_serve ==5 & match_data_plt$Svr == 2)){p2_serve_ad_body();text(x=60,y=40,cex=1.25, "First Serve to the Body",col = "white")}
                                if((first_serve ==6 & match_data_plt$Svr == 1)){p1_serve_ad_t();text(x=60,y=40,cex=1.25, "First Serve down the T",col = "white")}
                                if((first_serve ==6 & match_data_plt$Svr == 2)){p2_serve_ad_t();text(x=60,y=40,cex=1.25, "First Serve down the T",col = "white")}     
                                
                        }
                }else{
                        if(match_data_plt$Pts2 %in% c("0-0","15-15","30-0","0-30","15-40","40-15","30-30","40-40") |
                           length(grep("deuce",match_data_plt$Pts2)>0)){
                                if((second_serve ==4 & match_data_plt$Svr == 1)){p1_serve_deuce_wide();text(x=60,y=40,cex=1.25, "Second Serve out Wide",col = "white")}
                                if((second_serve ==4 & match_data_plt$Svr == 2)){p2_serve_deuce_wide();text(x=60,y=40,cex=1.25, "Second Serve out Wide",col = "white")}
                                if((second_serve ==5 & match_data_plt$Svr == 1)){p1_serve_deuce_body();text(x=60,y=40,cex=1.25, "Second Serve to the Body",col = "white")}
                                if((second_serve ==5 & match_data_plt$Svr == 2)){p2_serve_deuce_body();text(x=60,y=40,cex=1.25, "Second Serve to the Body",col = "white")}
                                if((second_serve ==6 & match_data_plt$Svr == 1)){p1_serve_deuce_t();text(x=60,y=40,cex=1.25, "Second Serve down the T",col = "white")}   
                                if((second_serve ==6 & match_data_plt$Svr == 2)){p2_serve_deuce_t();text(x=60,y=40,cex=1.25, "Second Serve down the T",col = "white")}   
                                
                        }else{
                                if((second_serve ==4 & match_data_plt$Svr == 1)){p1_serve_ad_wide();text(x=60,y=40,cex=1.25, "Second Serve out Wide",col = "white")}
                                if((second_serve ==4 & match_data_plt$Svr == 2)){p2_serve_ad_wide();text(x=60,y=40,cex=1.25, "Second Serve out Wide",col = "white")}
                                if((second_serve ==5 & match_data_plt$Svr == 1)){p1_serve_ad_body();text(x=60,y=40,cex=1.25, "Second Serve to the Body",col = "white")}
                                if((second_serve ==5 & match_data_plt$Svr == 2)){p2_serve_ad_body();text(x=60,y=40,cex=1.25, "Second Serve to the Body",col = "white")}
                                if((second_serve ==6 & match_data_plt$Svr == 1)){p1_serve_ad_t();text(x=60,y=40,cex=1.25, "Second Serve down the T",col = "white")}
                                if((second_serve ==6 & match_data_plt$Svr == 2)){p2_serve_ad_t();text(x=60,y=40,cex=1.25, "Second Serve down the T",col = "white")}       
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
                 validate(
                         need((input$year) >0,"Please select a valid year")
                 )
                text(x = 50,y = 20, vals$shotnumber)
                text(x = 50,y = -5, match_data_plt$Rally)
                text(x = 20, y = 37, match_data_plt$Player.1,col = "white",cex = 1.5)
                text(x = 100, y = 37, match_data_plt$Player.2,col = "white",cex = 1.5)

                if(vals$shotnumber == rallylength()){
                        if(match_data_plt$PtWinner==1){text(x = 60, y = 35,cex=1.25, paste(match_data_plt$Player.1,"wins point",sep = " "))}
                        if(match_data_plt$PtWinner==2){text(x = 60, y = 35,cex=1.25, paste(match_data_plt$Player.2,"wins point",sep = " "))}
                        if(match_data_plt$isRallyWinner==1){text(x = 60, y = 33,cex=1.25, "with a winner",col = "white")}
                        if(match_data_plt$isForced==1){text(x = 60, y = 33,cex=1.25, "due to a forced error",col = "white")}
                        if(match_data_plt$isUnforced==1){text(x = 60, y = 33,cex=1.25, "due to an unforced error",col = "white")}
                        if(match_data_plt$isAce==1){text(x = 60, y = 33,cex=1.25, "with an ace",col = "white")}
                        if(match_data_plt$isDouble==1){text(x = 60, y = 33,cex=1.25, "due to a double fault",col = "white")}

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
                text(x = 20, y = 37, match_data_plt$Player.1,col = "white",cex = 1.5)
                text(x = 100, y = 37, match_data_plt$Player.2,col = "white",cex = 1.5)

                if(vals$shotnumber == rallylength()){
                        if(match_data_plt$PtWinner==1){text(x = 60, y = 35,cex=1.25, paste(match_data_plt$Player.1,"wins point",sep = " "),col = "white")}
                        if(match_data_plt$PtWinner==2){text(x = 60, y = 35,cex=1.25, paste(match_data_plt$Player.2,"wins point",sep = " "),col = "white")}
                        if(match_data_plt$isRallyWinner==1){text(x = 60, y = 33,cex=1.25, "due to a winner",col = "white")}
                        if(match_data_plt$isForced==1){text(x = 60, y = 33,cex=1.25, "due to a forced error",col = "white")}
                        if(match_data_plt$isUnforced==1){text(x = 60, y = 33,cex=1.25, "due to an unforced error",col = "white")}
                }

        },height = 500,width = 800,units = "px")
        
}

