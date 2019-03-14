setwd("C:/Users/F400563/Desktop/tennis/gamble")
library(plyr)
library(stringr)

for(i in 2001:2018){
        i=2018
        
        setwd("C:/Users/F400563/Desktop/tennis/gamble")
        bets_tmp = read.csv(paste(i,".csv",sep = ""))
        num_periods = str_count(as.character(bets_tmp$Winner), fixed("."))
        bets_tmp$ln_w[num_periods ==1] = substr(as.character(bets_tmp$Winner[num_periods ==1]),1, nchar(as.character(bets_tmp$Winner[num_periods ==1]))-3)
        bets_tmp$ln_w[num_periods ==2] = substr(as.character(bets_tmp$Winner[num_periods ==2]),1, nchar(as.character(bets_tmp$Winner[num_periods ==2]))-5)
        bets_tmp$ln_w = word(bets_tmp$ln_w, -1)
        
        num_periods = str_count(as.character(bets_tmp$Loser), fixed("."))
        bets_tmp$ln_l[num_periods ==1] = substr(as.character(bets_tmp$Loser[num_periods ==1]),1, nchar(as.character(bets_tmp$Loser[num_periods ==1]))-3)
        bets_tmp$ln_l[num_periods ==2] = substr(as.character(bets_tmp$Loser[num_periods ==2]),1, nchar(as.character(bets_tmp$Loser[num_periods ==2]))-5)
        bets_tmp$ln_l = word(bets_tmp$ln_l, -1)
        
        ### Manual Changes ####
        bets_tmp$ln_w[bets_tmp$ln_w == "Carreno-Busta"] = "Busta"
        bets_tmp$ln_w[bets_tmp$ln_w == "Bautista"] = "Agut"
        bets_tmp$ln_w[bets_tmp$ln_w == "Roger-Vasselin"] = "Vasselin"
        bets_tmp$ln_w[bets_tmp$ln_w == "Garcia-Lopez"] = "Lopez"
        bets_tmp$ln_w[bets_tmp$ln_w == "Gimeno-Traver"] = "Traver"
        bets_tmp$ln_w[bets_tmp$ln_w == "Ramos-Vinolas"] = "Ramos"
        bets_tmp$ln_w[bets_tmp$ln_w == "Menendez-Maceiras"] = "Maceiras"
        bets_tmp$ln_w[bets_tmp$ln_w == "Auger-Aliassime"] = "Aliassime"
        
        bets_tmp$ln_l[bets_tmp$ln_l == "Carreno-Busta"] = "Busta"
        bets_tmp$ln_l[bets_tmp$ln_l == "Bautista"] = "Agut"
        bets_tmp$ln_l[bets_tmp$ln_l == "Roger-Vasselin"] = "Vasselin"
        bets_tmp$ln_l[bets_tmp$ln_l == "Garcia-Lopez"] = "Lopez"
        bets_tmp$ln_l[bets_tmp$ln_l == "Gimeno-Traver"] = "Traver"
        bets_tmp$ln_l[bets_tmp$ln_l == "Ramos-Vinolas"] = "Ramos"
        bets_tmp$ln_l[bets_tmp$ln_l == "Menendez-Maceiras"] = "Maceiras"
        bets_tmp$ln_l[bets_tmp$ln_l == "Auger-Aliassime"] = "Aliassime"
        
        
        
        bets_tmp$date_of_play = as.Date(bets_tmp$Date, format = ("%m/%d/%Y"))
        min_date = aggregate(bets_tmp$date_of_play, by = list(bets_tmp$Tournament), FUN = min)
        names(min_date) = c("Tournament", "truedate")
        bets_tmp = merge(bets_tmp, min_date, by = "Tournament")
        
        setwd("C:/Users/F400563/Desktop/tennis/atp_result")
        matches_tmp = read.csv(paste("atp_matches_",i,".csv",sep = ""))
        matches_tmp$ln_w = word(as.character(matches_tmp$winner_name),-1)
        matches_tmp$ln_l = word(as.character(matches_tmp$loser_name),-1)
        matches_tmp$truedate = as.Date(as.character(matches_tmp$tourney_date), format = ("%Y%m%d"))


        all_temp = merge(bets_tmp, matches_tmp, by = c("ln_w", "ln_l","truedate"))
        assign(paste("all_temp",i,sep = "_"),all_temp)
        print(c(nrow(bets_tmp),nrow(matches_tmp),nrow(all_temp)))
}

### Checking for why we don't match
b = intersect(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
c = union(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
n = as.numeric(row.names(c)[!c$ln_w %in% b$ln_w])
c[n,]

b = intersect(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
c = union(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
n = as.numeric(row.names(c)[!c$ln_l %in% b$ln_l])
dim(c[n,])

b = intersect(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
c = union(matches_tmp[,c("ln_w", "ln_l","truedate")],bets_tmp[,c("ln_w", "ln_l","truedate")])
n = as.numeric(row.names(c)[!c$truedate %in% b$truedate])
dim(c[n,])

final_data = rbind.fill(all_temp_2001,all_temp_2002,all_temp_2003,all_temp_2004,
                        all_temp_2005,all_temp_2006,all_temp_2007,all_temp_2008,
                        all_temp_2009,all_temp_2010,all_temp_2011,all_temp_2012,
                        all_temp_2013,all_temp_2014,all_temp_2015,all_temp_2016,
                        all_temp_2017,all_temp_2018
)
Win_bet =c("CBW","GBW","IWW","SBW","B365W","B.WW","EXW","PSW","UBW","LBW","SJW") 
lose_bet =c("CBL","GBL","ILL","SBL","B365L","B.LL","EXL","PSL","UBL","LBL","SJL")
avg_w = rowMeans(final_data[,Win_bet],na.rm = TRUE)
avg_l = rowMeans(final_data[,Win_bet],na.rm = TRUE)
