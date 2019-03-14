setwd("C:/Users/peter/Desktop/tennis/tennis_atp-master")

data=read.csv("atp_matches_1968.csv")[1,]
for(i in 1968:2018){
  data_tmp = read.csv(paste("atp_matches_",i,".csv",sep = ""))
  if(i>1968){
    data = rbind(data, data_tmp)
  }
}
data = data[-1,]
