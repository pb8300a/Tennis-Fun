
setwd("C:/Users/F400563/Desktop/tennis")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
#raw_points = read.csv("charting-m-points.csv", header = TRUE)
#point_lvl = merge(raw_points,match_metadata , by = "match_id")

player_choices = unique(c(as.character(point_lvl$Player.1),as.character(point_lvl$Player.2)))
initial_player_1 = "Rafael Nadal"
initial_player_2 = "Albert Montanes"

tournament_choices = names(table(match_metadata$Tournament))

initial_tournament = "Acapulco"
year_choices = c(1980: 2019)
initial_year = 2005

