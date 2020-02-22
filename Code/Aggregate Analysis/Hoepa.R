#####################
#High cost
###################

#Pick one
#######################
#Total number denied (use for breakdown of denied loans)
totals <- loans[hoepa_status != 3]

###############

#high cost
high <- totals[ hoepa_status == 1]
high <- high[, .N,by= derived_race]
colnames(high) <- c("race", "high")

#low guarenteed
low <- totals[ hoepa_status == 2]
low <- low[, .N,by= derived_race]
colnames(low) <- c("race", "low")


totals <- totals[,.N,by= derived_race]
colnames(totals) <- c("race", "numTotal")

totals <- high[totals, on="race"]
totals <- low[totals, on="race"]

totals <- totals[, high := high/numTotal*100]
totals <- totals[, low := low/numTotal*100]

hoepa <- totals
save(hoepa, file = "hoepa.RData")