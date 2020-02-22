

#####################
#Loan Purchaser
###################


#######################
#All purchased loans
totals <- loans[loans$purchaser_type != 0]

###############

#Fannie
fannie <- totals[purchaser_type == 1]
fannie <- fannie[, .N,by= derived_race]
colnames(fannie) <- c("race", "fannie")

#Ginnie
ginnie <- totals[purchaser_type == 2]
ginnie <- ginnie[, .N,by= derived_race]
colnames(ginnie) <- c("race", "ginnie")

#Freddir
freddie <- totals[purchaser_type == 3]
freddie <- freddie[, .N,by= derived_race]
colnames(freddie) <- c("race", "freddie")


#Farmer Mac
farmer <- totals[purchaser_type == 4]
farmer <- farmer[, .N,by= derived_race]
colnames(farmer) <- c("race", "farmer")

#Private
private <- totals[purchaser_type == 5]
private <- private[, .N,by= derived_race]
colnames(private) <- c("race", "private")

#Banks
bank <- totals[purchaser_type == 6]
bank <- bank[, .N,by= derived_race]
colnames(bank) <- c("race", "bank")

#Mortgage company or Credit Union
company <- totals[purchaser_type == 71]
company <- company[, .N,by= derived_race]
colnames(company) <- c("race", "company")

#insurance
insurance <- totals[purchaser_type == 72]
insurance <- insurance[, .N,by= derived_race]
colnames(insurance) <- c("race", "insurance")

#affiliate
affiliate <- totals[purchaser_type == 8]
affiliate <- affiliate[, .N,by= derived_race]
colnames(affiliate) <- c("race", "affiliate")

#other
other <- totals[purchaser_type == 9]
other <- other[, .N,by= derived_race]
colnames(other) <- c("race", "other")

totals <- totals[,.N,by= derived_race]
colnames(totals) <- c("race", "numTotal")

totals <- freddie[totals, on="race"]
totals <- fannie[totals, on="race"]
totals <- ginnie[totals, on="race"]
totals <- farmer[totals, on="race"]
totals <- private[totals, on="race"]
totals <- bank[totals, on="race"]
totals <- company[totals, on="race"]
totals <- insurance[totals, on="race"]
totals <- affiliate[totals, on="race"]
totals <- other[totals, on="race"]

totals <- totals[, freddie := freddie/numTotal*100]
totals <- totals[, fannie := fannie/numTotal*100]
totals <- totals[, ginnie := ginnie/numTotal*100]
totals <- totals[, farmer := farmer/numTotal*100]
totals <- totals[, private := private/numTotal*100]
totals <- totals[, bank := bank/numTotal*100]
totals <- totals[, company := company/numTotal*100]
totals <- totals[, insurance := insurance/numTotal*100]
totals <- totals[, affiliate := affiliate/numTotal*100]
totals <- totals[, other := other/numTotal*100]

purchaser <- totals

save(purchaser, file = "purchaser.RData")

