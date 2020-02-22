#####################
#Agency by race
###################

#Pick one
#######################
#Total number denied (use for breakdown of denied loans)
totals <- loans[loans$action_taken == 3 & `denial_reason-1` != 10]

#Total number of loans (use for breakdown of percent denied by reason)
totals <- loans
###############

#Convential loans
conven <- totals[ loan_type == 1]
conven <- conven[, .N,by= derived_race]
colnames(conven) <- c("race", "conven")

#FHA guarenteed
fha <- totals[ loan_type == 2]
fha <- fha[, .N,by= derived_race]
colnames(fha) <- c("race", "fha")

#VA guarenteed
va <- totals[ loan_type == 3]
va <- va[, .N,by= derived_race]
colnames(va) <- c("race", "va")

#FSA guarenteed
fsa <- totals[ loan_type == 4]
fsa <- fsa[, .N,by= derived_race]
colnames(fsa) <- c("race", "fsa")


totals <- totals[,.N,by= derived_race]
colnames(totals) <- c("race", "numTotal")

totals <- conven[totals, on="race"]
totals <- fha[totals, on="race"]
totals <- va[totals, on="race"]
totals <- fsa[totals, on="race"]

totals <- totals[, va := va/numTotal*100]
totals <- totals[, conven := conven/numTotal*100]
totals <- totals[, fha := fha/numTotal*100]
totals <- totals[, fsa := fsa/numTotal*100]

agency <- totals
save(agency, file = "agency.RData")