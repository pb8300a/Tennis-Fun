setwd("C:/Users/F400563/Desktop/tennis")
data = read.csv("charting-w-stats-ServeBasics.csv")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
raw_points = read.csv("charting-m-points.csv", header = TRUE)
serve_direction = read.csv("charting-m-stats-ServeDirection.csv",header = TRUE) #delete last 5 cols of excel spreadsheet
serve_basic = read.csv("charting-m-stats-ServeBasics.csv",header = TRUE) #delete last 5 cols of excel spreadsheet
return_outcome = read.csv("charting-m-stats-ReturnOutcomes.csv",header = TRUE) #delete last 5 cols of excel spreadsheet


serve_basic_1s = serve_basic[serve_basic$row == "1 1",]


#Who is in this dataset?
Players = c(as.character(match_metadata$Player.1),as.character(match_metadata$Player.2))

Total_Matches = sum(table(Players))
Total_Players = length(unique(Players))
Matches_Per_Player= sort(table(Players),decreasing = TRUE)
sum(Matches_Per_Player[1:10])
