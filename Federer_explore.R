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
service_winners = cumsum(point_match1$isSvrWinner[point_match1$Svr == 1])

par(mfrow =c(1,1))
plot(point_match1$PtWinner, type = "p", pch = 16,ylim = c(0,3), cex = 0.5)

last_pt = aggregate(point_match1$Pt, by = list(point_match1$Gm.), FUN = tail, n = 1)
for(i in last_pt$x){abline(v = i, col = "gray")}
