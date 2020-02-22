small <- homeLoans[1:1000,]
smallL <- loans[1:1000,]


raceMissing <- loans[,.(median(income, na.rm = TRUE), mean(tract_minority_population_percent), mean(interest_rate, na.rm = TRUE)),by= loans$derived_race]
colnames(raceMissing) <- c("race", "med_income", "minority _pop", "ave_rate") 

raw = read.csv('NFWBS_PUF_2016_data.csv')

 own <- subset(raw, HOUSING == 1)

 
 own.rate = nrow(own)/nrow(raw)

 black.own <- subset(own, PPETHM == 2) 

 black <- subset(raw, PPETHM == 2) 
 
 black.rate = nrow(black.own)/nrow(black)
 

 head(homeLoans$`applicant_race-1`)
 