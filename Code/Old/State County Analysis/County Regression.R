library(knitr)
library(leaps)
library(MASS)


#############################
# Select Data
###########################
#county_data_fred <- county_data_fred[-c(672, 941, 1578, 1318),]
#save(county_data_fred, file = "county_data_fred.RData")
load("RData/County/county_data_fred.RData")
data <- county_data_fred
############################

##################################
# Total denials
##################################
dataNA <-na.omit(data) 
linear.fit <- lm(loan_term_total ~ . - county_code,  data = dataNA)
stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE, )
step.coefs <- names(stepwise$coefficients)[-1]
coefs <- gsub("`", "", step.coefs)
coefs <- c("loan_term_total" , coefs)
data.subset <- subset(data, select = coefs)
regress <- lm(loan_term_total ~ ., data.subset)

regressDenial <- lm(percent_denied_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total, data = data)

regressDenial <- lm(percent_denied_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total +
                      percent_manufactured_total + loan_term_total, data = data)

regressDenial <- lm(percent_denied_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total 
                    + origination_charges_total + loan_term_total + purchaser_fannie_total + purchaser_freddie_total + purchaser_ginnie_total  + purchaser_private_total + percent_manufactured_total, data = data)

regressDenial <- lm(percent_denied_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total 
                    + percent_manufactured_total+ loan_term_total + origination_charges_total + purchaser_fannie_total + purchaser_freddie_total + purchaser_ginnie_total  + purchaser_private_total
                    + Per.Capita.Personal.Income.by.County..Dollars. + Bachelor.s.Degree.or.Higher..5.year.estimate..by.County..Percent.  + Estimated.Percent.of.People.of.All.Ages.In.Poverty.by.County..Percent. + + Median.Age.of.the.Population.by.County..Years.of.Age. +  Equifax.Subprime.Credit.Population.by.County..Percent.
                    + White.to.Non.White.Racial.Dissimilarity.Index.by.County..Percent., data = data)


summary(regressDenial)
plot(regressDenial)

############################
#Rate Spread
###########################
regressSpread <- lm(rate_spread_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total, data = data)

regressSpread <- lm(rate_spread_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total +
                  percent_manufactured_total + loan_term_total, data = data)

regressSpread <- lm(rate_spread_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total 
                  + origination_charges_total + loan_term_total + purchaser_fannie_total + purchaser_freddie_total + purchaser_ginnie_total  + purchaser_private_total + percent_manufactured_total, data = data)

regressSpread <- lm(rate_spread_total ~ percent_population_black + average_income_total  + loan_amount_total + percent_female_total + debt_to_income_ratio + loan_to_value_ratio_total 
                  + percent_manufactured_total+ loan_term_total + origination_charges_total + purchaser_fannie_total + purchaser_freddie_total + purchaser_ginnie_total  + purchaser_private_total
                  + Per.Capita.Personal.Income.by.County..Dollars. + Bachelor.s.Degree.or.Higher..5.year.estimate..by.County..Percent.  + Estimated.Percent.of.People.of.All.Ages.In.Poverty.by.County..Percent. + + Median.Age.of.the.Population.by.County..Years.of.Age. +  Equifax.Subprime.Credit.Population.by.County..Percent.
                  + White.to.Non.White.Racial.Dissimilarity.Index.by.County..Percent., data = data)


summary(regressSpread)
plot(regressSpread)
###################################

##################################
#City Predictions
#################################

#New York = 36061, Durham = 37065
city_predictions <- function(code){
  
  county <- county_data_fred[which(county_data_fred$county_code == code),]
  vars <- names(regressFred$coefficients)
  vars[1] <- "county_code" 
  coefs <- regressFred$coefficients
  county.vars <- county[vars]
  county.vars$county_code[1] <- 1
  predicts <- predict(regressFred, type = "terms", newdata = county) 
  prediction = county.vars * coefs
  return(prediction)
}

predictions <- city_predictions( 37065)
write.csv(predictions, file = "CSV/Durham_coefs_NoTerm.csv" )
sum(predictions)
############################################################

######################################################
#Testing
###################################################

testing <- lm(interest_rate_total ~ loan_term_total + origination_charges_total, data = county_data_fred)

testing <- lm(percent_denied_total ~ Equifax.Subprime.Credit.Population.by.County..Percent., data = county_data_fred)
summary(testing)


durham <- county_data_fred[which(county_data_fred$county_code == 37065),]
################################################





