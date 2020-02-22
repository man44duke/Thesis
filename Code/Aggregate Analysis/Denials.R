

#####################
#Why denied by race
###################

#Pick one
#######################
#Total number denied (use for breakdown of denied loans)
totals <- loans[loans$action_taken == 3 & `denial_reason-1` != 10]

#Total number of loans (use for breakdown of percent denied by reason)
totals <- loans
###############

#Number denied for debt to income
dti <- totals[loans$action_taken == 3 & `denial_reason-1` == 1]
dti <- dti[, .N,by= derived_race]
colnames(dti) <- c("race", "DTI")

#Number denied for Employment history
employ <- totals[loans$action_taken == 3 & `denial_reason-1` == 2]
employ <- employ[, .N,by= derived_race]
colnames(employ) <- c("race", "Employ")

#Number denied for credit history
credit <- totals[loans$action_taken == 3 & `denial_reason-1` == 3]
credit <- credit[, .N,by= derived_race]
colnames(credit) <- c("race", "Credit")


#Number denied for collateral
collat <- totals[loans$action_taken == 3 & `denial_reason-1` == 4]
collat <- collat[, .N,by= derived_race]
colnames(collat) <- c("race", "Collat")

#Number denied for Insufficient cash
cash <- totals[loans$action_taken == 3 & `denial_reason-1` == 5]
cash <- cash[, .N,by= derived_race]
colnames(cash) <- c("race", "Cash")

#Number denied for unverifiable information
unver <- totals[loans$action_taken == 3 & `denial_reason-1` == 6]
unver <- unver[, .N,by= derived_race]
colnames(unver) <- c("race", "Unver")

#Number denied for credit application incomplete
credIm <- totals[loans$action_taken == 3 & `denial_reason-1` == 7]
credIm <- credIm[, .N,by= derived_race]
colnames(credIm) <- c("race", "CredIm")

#Insure
insure <- totals[loans$action_taken == 3 & `denial_reason-1` == 8]
insure <- insure[, .N,by= derived_race]
colnames(insure) <- c("race", "Insure")

#Other
other <- totals[loans$action_taken == 3 & `denial_reason-1` == 9]
other <- other[, .N,by= derived_race]
colnames(other) <- c("race", "Other")

totals <- totals[,.N,by= derived_race]
colnames(totals) <- c("race", "numTotal")

totals <- credit[totals, on="race"]
totals <- dti[totals, on="race"]
totals <- employ[totals, on="race"]
totals <- collat[totals, on="race"]
totals <- cash[totals, on="race"]
totals <- unver[totals, on="race"]
totals <- credIm[totals, on="race"]
totals <- insure[totals, on="race"]
totals <- other[totals, on="race"]

totals <- totals[, Credit := Credit/numTotal*100]
totals <- totals[, DTI := DTI/numTotal*100]
totals <- totals[, Employ := Employ/numTotal*100]
totals <- totals[, Collat := Collat/numTotal*100]
totals <- totals[, Cash := Cash/numTotal*100]
totals <- totals[, Unver := Unver/numTotal*100]
totals <- totals[, CredIm := CredIm/numTotal*100]
totals <- totals[, Insure := Insure/numTotal*100]
totals <- totals[, Other := Other/numTotal*100]

deniedBreakdown <- totals

save(deniedBreakdown, file = "deniedBreakdown.RData")

