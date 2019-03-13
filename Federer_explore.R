setwd("C:/Users/F400563/Desktop/tennis")
data = read.csv("charting-w-stats-ServeBasics.csv")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
raw_points = read.csv("charting-m-points.csv", header = TRUE)

raw_points$num_id = seq(1:nrow(raw_points))
match_lvl= match_metadata[match_metadata$Player.1 == "Roger Federer" | match_metadata$Player.2 == "Roger Federer", ]
point_lvl = merge(raw_points,match_lvl , by = "match_id")
point_lvl = point_lvl[order(point_lvl$num_id),]

#####For a given match
point_match1 = point_lvl[point_lvl$match_id == "19981005-M-Basel-R32-Andre_Agassi-Roger_Federer",]

par(mfrow =c(1,1),mar = c(5,5,2,1))
plot(point_match1$PtWinner, type = "p", pch = 16,ylim = c(0,3), cex = 0.5,
     yaxt = "n",ylab = "",xlab = "",xaxt = "n")
axis(side = 2, at = c(1,2), 
     labels = c(as.character(point_match1$Player.1)[1],as.character(point_match1$Player.2)[1]),
     las = 2)

last_pt = aggregate(point_match1$Pt, by = list(point_match1$Gm.), FUN = tail, n = 1)
game_1 = aggregate(point_match1$Gm1.1, by = list(point_match1$Gm.), FUN = tail, n = 1)
game_2 = aggregate(point_match1$Gm2.1, by = list(point_match1$Gm.), FUN = tail, n = 1)
game_2 = game_2[order(as.numeric(as.character(game_2$Group.1))),]

all = merge(last_pt, game_1, by = "Group.1")
all = merge(all, game_2, by = "Group.1")
all =all[order(as.numeric(as.character(all$Group.1))),]
names(all) = c("Group", "Pt", "Gm1","Gm2")
count = 0
lastpoint=0
for(i in all$Pt){
        if(count != 0){lastpoint = all$Pt[count]}
        midpoint = (lastpoint + i+1)/2
        count =count+1
        abline(v = i+0.5, col = "gray")
        text(x=midpoint,y = 0.5, all$Gm1[count])
        text(x=midpoint,y = 2.5, all$Gm2[count])
}
