setwd("C:/Users/F400563/Desktop/tennis")
data = read.csv("charting-w-stats-ServeBasics.csv")
match_metadata = read.csv("charting-m-matches.csv", header = TRUE) #delete ? in the one column name
raw_points = read.csv("charting-m-points.csv", header = TRUE)
serve_direction = read.csv("charting-m-stats-ServeDirection.csv",header = TRUE) #delete last 5 cols of excel spreadsheet
serve_basic = read.csv("charting-m-stats-ServeBasics.csv",header = TRUE) #delete last 5 cols of excel spreadsheet
return_outcome = read.csv("charting-m-stats-ReturnOutcomes.csv",header = TRUE) #delete last 5 cols of excel spreadsheet


################################################################################
#Who is in this dataset?
Players = c(tolower(as.character(match_metadata$Player.1)),tolower(as.character(match_metadata$Player.2)))
simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
}
for(i in 1:length(Players)){
        Players[i] = simpleCap(Players[i])
}

Total_Matches = sum(table(Players))
Total_Players = length(unique(Players))
Matches_Per_Player= sort(table(Players),decreasing = TRUE)
sum(Matches_Per_Player[1:10])
sum(Matches_Per_Player[Matches_Per_Player<20])

################################################################################################
####How many players, and how often are they repeated in this dataset?
################################################################################################
####This is not a random sample! The players are heavily skewed towards popular players.
top_50= barplot(Matches_Per_Player[1:50],width = rep(1,50), col = "greenyellow",
                xlab = "",ylab = "# Matches in Dataset",xaxt = "n",ylim = c(0,325),
                beside = TRUE, 
                main = paste("50 Players are involved in",
                             sprintf("%1.0f%%",round(sum(Matches_Per_Player[1:50]/sum(Matches_Per_Player)*100))),
                             "of the matches.",sep = " "))
text(x=quantile(top_50,0.5), y=-20, "Individual Players", xpd=TRUE, cex = 1.2)

box()
top_10= barplot(Matches_Per_Player[1:10],width = rep(1,10), col = "greenyellow",
                xlab = "",ylab = "# Matches in Dataset",xaxt = "n",ylim = c(0,325),
                beside = TRUE,space = 0.1,lwd = 0.1, 
                main = paste("10 Players are involved in",
                             sprintf("%1.0f%%",round(sum(Matches_Per_Player[1:10]/sum(Matches_Per_Player)*100))),
                             "of the matches.",sep = " "))
labs <- names(Matches_Per_Player)[1:10]
text(x=top_10-.25, y=-20, labs, xpd=TRUE, srt=45, cex = 0.8)
box()

library(wordcloud)
wordcloud(words = names(Matches_Per_Player), freq = Matches_Per_Player, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


################################
########What type of tournament?
################################
tournament = match_metadata$Tournament
##Group like-named tournaments together
tournament[tournament== "US Open "] = "US Open"
tournament[tournament== "Cincinnati"] = "Cincinnati Masters"
tournament[tournament== "Stockholm"] = "Stockholm Masters"
tournament[tournament== "Madrid"] = "Madrid Masters"
tournament[tournament== "Indian Wells"] = "Indian Wells Masters"
tournament[tournament== "Monte Carlo"] = "Monte Carlo Masters"
tournament[tournament== "Stuttgart"] = "Stuttgart Masters"

challenger = lapply(list(tournament), grep, pattern = "CH")[[1]]
grand_slam = which(tournament %in% c("US Open","Wimbledon", "Roland Garros","Australian Open"))
tournament_tbl = sort(table(tournament),decreasing = TRUE)
tournament_type = rep("Normal_ATP",length(tournament))
tournament_type[grand_slam] = "Grand_Slam"
tournament_type[challenger] = "Challenger"

par(mar=c(3,4,2,1))
tourn_type= barplot(table(tournament_type),width = rep(1,3), col = "greenyellow",
                xlab = "",ylab = "# Matches in Dataset",xaxt = "n",ylim = c(0,1000),
                beside = TRUE,space = 0.1,lwd = 0.1, 
                main = paste("Type of Tournament"))
text(x=tourn_type, y=-40, names(table(tournament_type)), xpd=TRUE, cex = 1)


#################################################################
####When are most of these matches played
################################################################
par(mfrow = c(1,2))
years = table(substr(match_metadata$Date,1,4))
time_var = as.numeric(names(years))
plot(x = time_var, y = as.vector(years),type = "l", ylab = "# Matches", main = "Number of Matches over Time",
     lwd = 2, col = "blue")
for(i in seq(1980,2020,by = 10)){abline(v = i, col = "gray", lty = 2)}

cum_sum_over_years = cumsum(years)

plot(x = time_var, y = as.vector(cum_sum_over_years),type = "l", ylab = "# Matches", main = "Cumulative Matches over Time",
     lwd = 2, col = "blue")
for(i in seq(1980,2020,by = 10)){abline(v = i, col = "gray", lty = 2)}

