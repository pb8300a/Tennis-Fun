
setwd("C:/Users/F400563/Desktop/tennis")
data = read.csv("charting-w-stats-ServeBasics.csv")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
raw_points = read.csv("charting-m-points.csv", header = TRUE)

raw_points$num_id = seq(1:nrow(raw_points))
match_lvl= match_metadata[match_metadata$Player.1 == "Roger Federer" | match_metadata$Player.2 == "Roger Federer", ]
point_lvl = merge(raw_points,match_lvl , by = "match_id")
point_lvl = point_lvl[order(point_lvl$num_id),]
##############################
#####For a given match
##############################
point_match1 = point_lvl[point_lvl$match_id == "19981005-M-Basel-R32-Andre_Agassi-Roger_Federer",]


## A Timeline of the Match ##
set_counter = table(rowSums(point_match1[,c("Set1","Set2")]))
player_name = c(as.character(point_match1$Player.1)[1],as.character(point_match1$Player.2)[1])
par(mfrow =c(length(set_counter),1),mar = c(1,5,1,1))

for(i in names(set_counter)){
        i = "1"
        point_set=point_match1[rowSums(point_match1[,c("Set1","Set2")])==as.numeric(i),]
        
        plot(point_set$PtWinner, type = "p", pch = 16,ylim = c(0,3), cex = 0.5,
             yaxt = "n",ylab = "",xlab = "",xaxt = "n")
        axis(side = 2, at = c(1,2), labels = c(sub(" ", "\n", player_name)),las = 2)
        
        last_pt = aggregate(point_set$Pt, by = list(point_set$Gm.), FUN = tail, n = 1)
        game_1 = aggregate(point_set$Gm1.1, by = list(point_set$Gm.), FUN = tail, n = 1)
        game_2 = aggregate(point_set$Gm2.1, by = list(point_set$Gm.), FUN = tail, n = 1)
        
        all = merge(last_pt, game_1, by = "Group.1")
        all = merge(all, game_2, by = "Group.1")
        names(all) = c("Group", "Pt", "Gm1","Gm2")
        
        if(i !="0"){all$Pt = all$Pt - all$Pt[1] +(all$Pt[1] - point_set$Pt[1] +1)}
        all =all[order(as.numeric(as.character(all$Group))),]
        count = 0
        lastpoint=0
        for(j in all$Pt){
                if(count != 0){lastpoint = all$Pt[count]}
                midpoint = (lastpoint + j+1)/2
                count =count+1
                abline(v = j+0.5, col = "gray")
                text(x=midpoint,y = 0.5, all$Gm1[count])
                text(x=midpoint,y = 2.5, all$Gm2[count])
        }
      
}

