setwd("C:/Users/F400563/Desktop/tennis")
#raw_points = read.csv("charting-m-points.csv", header = TRUE)
#match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
#point_lvl = merge(raw_points,match_metadata , by = "match_id")
#setwd("C:/Users/F400563/Desktop/tennis/point_shiny")

#c("Set1_score","Set2_score","Gm1_score","Gm2_score","Pts","score","Gm1.1","Gm2.1","Set1.1","Set2.1")

### Change scores to account for multiple deuces/Ads ###
final_pts2 = NULL
set_number = rowSums(point_lvl[,c("Set1","Set2")])
point_lvl$match_id_gm = paste(point_lvl$match_id, set_number,point_lvl$Gm.,point_lvl$Pts,sep = "_")
point_lvl$asNumbers <- with(point_lvl, ave(match_id_gm,match_id_gm, FUN = seq_along))
Pts2 = as.character(point_lvl$Pts)
Pts2[point_lvl$Pts=="40-40"] = paste("deuce #",point_lvl$asNumbers[point_lvl$Pts=="40-40"],sep = "")
Pts2[point_lvl$Pts=="AD-40"] = paste("AD-40 #",point_lvl$asNumbers[point_lvl$Pts=="AD-40"],sep = "")
Pts2[point_lvl$Pts=="40-AD"] = paste("40-AD #",point_lvl$asNumbers[point_lvl$Pts=="40-AD"],sep = "")
point_lvl$Pts2 = Pts2
point_lvl$score = paste(point_lvl$Set1, "-", point_lvl$Set2,", ", point_lvl$Gm1, "-", point_lvl$Gm2,", ",point_lvl$Pts2, sep = "")




player_choices = unique(c(as.character(point_lvl$Player.1),as.character(point_lvl$Player.2)))
#player_choices = c("Roger Federer","Novak Djokovic","Andy Murray","Rafael Nadal")
initial_player_1 = "Andy Murray"
initial_player_2 = "Rafael Nadal"

#setwd("C:/Users/F400563/Desktop/tennis")
#tournament_choices = c("US Open","Wimbledon","Roland Garros", "Australian Open")
tournament_choices = names(table(match_metadata$Tournament))
initial_tournament = "Australian Open"

year_choices = c(1980: 2019)
initial_year = 2010

point_choices="0-0, 0-0, 0-0"
initial_point = "0-0, 0-0, 0-0"


#####################
### Rally Shot Areas  
#####################
p1_ad = function(){rect(60,0,120,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_ad_deep = function(){rect(105,0,120,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_ad_mid = function(){rect(90,0,105,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_ad_short = function(){rect(60,0,90,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_ad_deep_out = function(){lines(x=c(120,120),y = c(0*30,0.3*30),col = "red",lwd = 3)}
p1_ad_wide_out = function(){lines(x=c(60,120),y = c(0,0),col = "red",lwd = 3)}
p1_ad_wide_deep_out = function(){lines(x=c(117,120),y=c(0,0),col = "red",lwd = 3);lines(x=c(120,120),y=c(0,2),col = "red",lwd = 3)}

p1_mid = function(){rect(60,0.3*30,120,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_mid_deep =  function(){rect(105,0.3*30,120,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_mid_mid =   function(){rect(90, 0.3*30,105,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_mid_short = function(){rect(60, 0.3*30,90,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p1_mid_deep_out = function(){lines(x=c(120,120),y = c(0.3*30,0.7*30),col = "red",lwd = 3)}

p1_deuce = function(){rect(60,0.7*30,120,30, border ="dodgerblue4", col = "dodgerblue4")}
p1_deuce_deep =  function(){rect(105,0.7*30,120,30, border ="dodgerblue4", col = "dodgerblue4")}
p1_deuce_mid = function(){rect(90,0.7*30,   105,30, border ="dodgerblue4", col = "dodgerblue4")}
p1_deuce_short = function(){rect(60,0.7*30,90,30, border ="dodgerblue4", col = "dodgerblue4")}
p1_deuce_deep_out = function(){lines(x=c(120,120),y = c(0.7*30,30),col = "red",lwd = 3)}
p1_deuce_wide_out = function(){lines(x=c(60,120),y = c(30,30),col = "red",lwd = 3)}
p1_deuce_wide_deep_out = function(){lines(x=c(117,120),y=c(30,30),col = "red",lwd = 3);lines(x=c(120,120),y=c(28,30),col = "red",lwd = 3)}

p2_deuce = function(){rect(0,0,60,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_deuce_deep = function(){rect(0,0,15,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_deuce_mid = function(){rect(15,0,30,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_deuce_short = function(){rect(30,0,60,0.3*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_deuce_deep_out = function(){lines(x=c(0,0),y = c(0,0.3*30),col = "red",lwd = 3)}
p2_deuce_wide_out = function(){lines(x=c(0,60),y = c(0,0),col = "red",lwd = 3)}
p2_deuce_wide_deep_out = function(){lines(x=c(0,3),y=c(0,0),col = "red",lwd = 3);lines(x=c(0,0),y=c(0,2),col = "red",lwd = 3)}

p2_mid = function(){rect(0,0.3*30,60,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_mid_deep =  function(){rect(0,0.3*30,15,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_mid_mid =   function(){rect(15, 0.3*30,30,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_mid_short = function(){rect(30, 0.3*30,60,0.7*30, border ="dodgerblue4", col = "dodgerblue4")}
p2_mid_deep_out = function(){lines(x=c(0,0),y = c(0.3*30,0.7*30),col = "red",lwd = 3)}

p2_ad = function(){rect(0,0.7*30,60,30, border ="dodgerblue4", col = "dodgerblue4")}
p2_ad_deep =  function(){rect(0,0.7*30,15,30, border ="dodgerblue4", col = "dodgerblue4")}
p2_ad_mid = function(){rect(15,0.7*30,   30,30, border ="dodgerblue4", col = "dodgerblue4")}
p2_ad_short = function(){rect(30,0.7*30,60,30, border ="dodgerblue4", col = "dodgerblue4")}
p2_ad_deep_out = function(){lines(x=c(0,0),y = c(0.7*30,30),col = "red",lwd = 3)}
p2_ad_wide_out = function(){lines(x=c(0,60),y = c(30,30),col = "red",lwd = 3)}
p2_ad_wide_deep_out = function(){lines(x=c(0,3),y=c(30,30),col = "red",lwd = 3);lines(x=c(0,0),y=c(28,30),col = "red",lwd = 3)}

#####################
## Serve Location ###
#####################
p1_serve_deuce_wide = function(){rect(60,25,90,30, border ="dodgerblue4", col = "dodgerblue4")}
p1_serve_deuce_body = function(){rect(60,20,90,25, border ="dodgerblue4", col = "dodgerblue4")}
p1_serve_deuce_t = function(){rect(60,15,90,20, border ="dodgerblue4", col = "dodgerblue4")}

p1_serve_ad_wide = function(){rect(60,0,90,5, border ="dodgerblue4", col = "dodgerblue4")}
p1_serve_ad_body = function(){rect(60,5,90,10, border ="dodgerblue4", col = "dodgerblue4")}
p1_serve_ad_t = function(){rect(60,10,90,15, border ="dodgerblue4", col = "dodgerblue4")}

p2_serve_deuce_wide = function(){rect(30,0,60,5, border ="dodgerblue4", col = "dodgerblue4")}
p2_serve_deuce_body = function(){rect(30,5,60,10, border ="dodgerblue4", col = "dodgerblue4")}
p2_serve_deuce_t = function(){rect(30,10,60,15, border ="dodgerblue4", col = "dodgerblue4")}

p2_serve_ad_wide = function(){rect(30,25,60,30, border ="dodgerblue4", col = "dodgerblue4")}
p2_serve_ad_body = function(){rect(30,20,60,25, border ="dodgerblue4", col = "dodgerblue4")}
p2_serve_ad_t = function(){rect(30,15,60,20, border ="dodgerblue4", col = "dodgerblue4")}


blank_court = function(){
        #plot(x=c(0,0,120,120),y = c(0,30,30,0), col = "white", ylim = c(-12,42),xlim = c(-12,130))
        plot(x=c(0,0,120,120),y = c(0,30,30,0), col = "white", ylim = c(-12,42),xlim = c(-12,130),
             xlab = "",ylab = "", bty = "n",xaxt = "n",yaxt = "n")
        
        rect(-50,-50,300,300, col = "deepskyblue",border = "deepskyblue")
        rect(0,0,120,30, col = "chartreuse3",border = "white",lwd = 2)
}
lines_court = function(){
        lines(x=c(60,60),y = c(0,30),lty = 2,lwd = 3, col = "black")
        lines(x=c(30,30),y = c(0,30), col = "white",lwd = 2) 
        lines(x=c(90,90),y = c(0,30), col = "white",lwd = 2) 
        lines(x=c(30,90),y = c(15,15), col = "white",lwd = 2) 
        lines(x=c(0,1),y = c(15,15), col = "white",lwd = 2) 
        lines(x=c(119,120),y = c(15,15), col = "white",lwd = 2) 
}




        

        
        
