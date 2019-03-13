setwd("C:/Users/F400563/Desktop/tennis")
data = read.csv("charting-w-stats-ServeBasics.csv")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
raw_points = read.csv("charting-m-points.csv", header = TRUE)

player_name = "Roger Federer"

raw_points$num_id = seq(1:nrow(raw_points))
match_lvl= match_metadata[match_metadata$Player.1 == player_name | match_metadata$Player.2 ==player_name , ]
point_lvl = merge(raw_points,match_lvl , by = "match_id")
point_lvl = point_lvl[order(point_lvl$num_id),]

sample_id = sample(unique(point_lvl$match_id), 1)
