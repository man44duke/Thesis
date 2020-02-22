library(knitr)
library(leaps)
library(MASS)

colnames(state.data)

reg.subset = regsubsets(percent_denied_black ~ .-state_code, data = state.data, really.big=T , nvmax=15)
coef(reg.subset ,10)

#############################
# Select Data
###########################
load("RData/State/states_values.RData")
data <- state.data

data$difference <- data$percent_denied_black - data$percent_denied_white
row.names(data) <- data$state_code

state.adjusted <- c("MT", "ID", "ME", "VT", "NH", "UT", "WY")
data <- data[!(row.names(data) %in% state.adjusted), ]
##########################

##############################
# Black Denials
##############################
regress1 <- lm(data$percent_denied_black ~  percent_denied_white + loan_term_black + percent_female_black 
               + purchaser_private_black + purchaser_private_total   + tract_msa_income_percentage_total  + data$average_income_black + data$debt_to_income_low_black + data$average_income_white, data = data)


regress2 <- lm(percent_denied_black ~ average_income_black + average_income_white + percent_denied_white + hoepa_status_black + total_loan_costs_black + loan_term_black
                 + percent_female_black + purchaser_ginnie_black + purchaser_private_black + debt_to_income_low_black + msa_median_income_black + tract_msa_income_percentage_black, data = data)

summary(regress2)
plot(regress2)
##############################


##################################
# Difference Denials
##################################
regress4 <- lm(difference ~ average_income_black + average_income_white + hoepa_status_black + total_loan_costs_black  + percent_denied_total + loan_term_total
                 + percent_female_black + purchaser_ginnie_black + purchaser_private_black + debt_to_income_low_black + msa_median_income_black + tract_msa_income_percentage_black, data = data)
summary(regress4)
plot(regress4)
###################################

##################################
# Total denials
##################################

regress3 <- lm(percent_denied_total ~ average_income_total  + hoepa_status_total + total_loan_costs_total + loan_term_total
               + percent_female_total + purchaser_ginnie_total + purchaser_private_total + debt_to_income_low_total + msa_median_income_total + tract_msa_income_percentage_total + percent_population_black, data = data)


summary(regress3)
plot(regress3)
###################################

##################################
# Residual plot
#################################
preds <- data.frame(predict(regress2, data= data))
preds$state_code <- data$state_code
output <- data[,c("state_code","percent_denied_black")]
predictions <-merge(x=output, y=preds,by="state_code", all.x = TRUE)
colnames(predictions) <- c("state_code", "actual", "predicted")
predictions$residuals <-  predictions$actual-predictions$predicted
write.csv(data, "stateData.csv")
##################################

#############################
# Just Totals Data
###########################
indexes <- seq(4, 79, by=3)
indexes <- c(1, indexes, 80)
data_small <- county.data[,indexes]
data <- data[ , -which(names(data) %in% c("denied_credit_total", "denied_dti_total", "denied_employment_total", "tract_minority_population_percent_black"))]
data$percent_population_black <- county.data$percent_population_black
###########################



