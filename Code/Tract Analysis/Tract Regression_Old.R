library(knitr)
library(leaps)
library(MASS)
library(plm)


#############################
# Select Data
###########################

data <- tract_data


data$majority_black = 0
data$majority_black[data$percent_population_black >= .5] = 1 
############################


############################
#Rate Spread
###########################
regressSpread <- lm(rate_spread ~ majority_black, data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio, data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + percent_manufactured + loan_term 
                    + origination_charges + discount_points + loan_conventional , data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + total_points_and_fees + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + percent_manufactured
                    , data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + purchaser_private + percent_manufactured
                    + tract_msa_income_percentage  + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                    , data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                    + denied_credit + denied_employment
                    + tract_msa_income_percentage  + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                    + market_share 
                    , data = data)

regressSpread <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + percent_manufactured+ loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie 
                    + denied_credit + denied_employment 
                    + tract_msa_income_percentage  + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                    + market_share 
                    , index = c("county_code")
                    , model="within"
                    , data = data)

regressSpread <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                     + origination_charges + discount_points + loan_conventional
                     + percent_manufactured+ loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie
                     + denied_credit + denied_employment
                     + tract_msa_income_percentage  + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                     + market_share
                     , index = c("mode_lender")
                     , model="within"
                     , data = data)

regressSpread <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                    + denied_credit + denied_employment
                    + tract_msa_income_percentage  + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                    + market_share + factor(mode_lender) * market_share
                    , data = data)

regressSpread <- lm(rate_spread ~ . -census_tract - mode_lender - percent_population_black - percent_population_white, data = data)

summary(regressSpread)
plot(regressSpread)
###################################

################################
# Random analysis
##########################

sum(data$majority_black)/length(data$majority_black)

######################
# Best Subset
##################
dataNA <-na.omit(data) 
linear.fit <- lm(loan_term_total ~ . - county_code,  data = dataNA)
stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE, )
step.coefs <- names(stepwise$coefficients)[-1]
coefs <- gsub("`", "", step.coefs)
coefs <- c("loan_term_total" , coefs)
data.subset <- subset(data, select = coefs)
regress <- lm(loan_term_total ~ ., data.subset)

data$downpayment = (100-data$loan_to_value_ratio)*data$loan_amount


##################################
# Total denials
##################################

regressDenial <- lm(percent_denied ~ majority_black, data = data)

regressDenial <- lm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio, data = data)

regressDenial <- lm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + percent_manufactured + loan_term 
                    + total_points_and_fees + origination_charges + discount_points + loan_conventional , data = data)

regressDenial <- lm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + purchaser_private + percent_manufactured
                    , data = data)

regressDenial <- lm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                    + origination_charges + discount_points + loan_conventional
                    + loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + purchaser_private + percent_manufactured
                    + denied_credit + denied_employment 
                    + tract_msa_income_percentage  + percent_population_white + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                    + market_share
                    , data = data)

regressDenial <- plm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                     + origination_charges + discount_points + loan_conventional
                     + percent_manufactured+ loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + purchaser_private
                     + denied_credit + denied_employment
                     + tract_msa_income_percentage + percent_population_white + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                     + market_share
                     , index = c("county_code")
                     , model="within"
                     , data = data)

regressDenial <- plm(percent_denied ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                     + origination_charges + discount_points + loan_conventional
                     + percent_manufactured+ loan_term + purchaser_fannie + purchaser_freddie + purchaser_ginnie  + purchaser_private
                     + denied_credit + denied_employment
                     + tract_msa_income_percentage + percent_population_white + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                     + market_share
                     , index = c("mode_lender")
                     , model="within"
                     , data = data)

regressDenial <- lm(percent_denied ~ . -census_tract - mode_lender, data = data)

summary(regressDenial)
plot(regressDenial)