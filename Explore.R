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
sum(Matches_Per_Player[Matches_Per_Player<20])

top_100= barplot(Matches_Per_Player[1:100],width = rep(1,length(Matches_Per_Player)), col = "greenyellow",
                xlab = "",ylab = "# Matches",xaxt = "n",ylim = c(0,325),
                beside = TRUE,space = 0.1,lwd = 0.1)

top_10= barplot(Matches_Per_Player[1:10],width = rep(1,10), col = "greenyellow",
                xlab = "",ylab = "# Matches",xaxt = "n",ylim = c(0,325),
                beside = TRUE,space = 0.1,lwd = 0.1)
labs <- names(Matches_Per_Player)[1:10]
text(cex=1, x=top_10-.25, y=-25, labs, xpd=TRUE, srt=45)
box()
