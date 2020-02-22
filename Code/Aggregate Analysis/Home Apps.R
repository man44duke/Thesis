#sample data
library(data.table)


#replaces all na and infinity with na
fixNA <- function(col){
  col[which(is.nan(col))] = NA
  col[which(col==Inf)] = NA
}


loans <- loans_2007
rm(homeLoans)

#Finds percent of loans rejected by race
#Uses derived race (both applicants same race)
race <- loans[, .(denied, derived_race)]
ave <- race[,.(mean(denied)*100, .N),by= race$derived_race]
colnames(ave) <- c("race", "denied%", "numApps")
total <- sum(ave$numApps)
ave[, 'total%' := numApps/total*100]
ave <- ave[order(-ave$`total%`)]


aveHome <- ave
#Finds percent of loans rejected by race
#Uses applicant race 1 
race2 <- loans[, .(denied, `applicant_race-1`)]
ave2 <- race2[,.(mean(denied)*100,nrows(denied)),by= `applicant_race-1`]
colnames(ave2) <- c("race", "denied%", "numApps")
total2 <- sum(ave2$numApps)
ave2[, 'total%' := numApps/total2*100]
ave2 <- ave2[order(-ave2$`total%`)]


