library(knitr)
library(leaps)
library(MASS)
library(plm)
library(stargazer)
library(boot)
library(simpleboot)
library(matrixStats)
library(lme4)
#############################
# Select Data
###########################

tract_data <- readRDS("RData/Tract/tract_data_fixed.RDS")
tract_data_30 <- readRDS("RData/Tract/tract_data_fixed_30.RDS")
tract_data_census <- readRDS("RData/Tract/tract_data_census.RDS")
tract_data_median <- readRDS("RData/Tract/tract_data_median.RDS")

data <- tract_data_census
data <- tract_data_median
data <- tract_data_30
data <- tract_data_not30
############################
subset = data[which(data$average_income < 96),]
quantile(data$average_income, na.rm = TRUE, c(.95))
############################
#Rate Spread
###########################
regressSpread <- lm(rate_spread ~ majority_black, data = data)


regressSpread.borrower <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                          + origination_charges + discount_points + loan_conventional
                          + loan_term + percent_manufactured
                          + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                          , data = data)

regressSpread.tract <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                          + origination_charges + discount_points + loan_conventional
                          + loan_term + percent_manufactured
                          + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                          + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                          + denied_credit + tract_msa_income_percentage + market_share 
                          , data = data)

regressSpread.county <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                          + origination_charges + discount_points + loan_conventional
                          + loan_term + percent_manufactured
                          + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                          + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                          + denied_credit + tract_msa_income_percentage + market_share 
                          , index = c("county_code")
                          , model="within"
                          , data = data)
summary(regressSpread.county)

regressSpread.county.census <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                            + origination_charges + discount_points + loan_conventional
                            + loan_term + percent_manufactured
                            + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                            + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                            + denied_credit + tract_msa_income_percentage + market_share 
                            + Bachelors_rate + vacantcy + Unemployment_rate + Percent_snap + Rental_rate + Uninsured_rate
                            , index = c("county_code")
                            , model="within"
                            , data = data)
summary(regressSpread.county.census)

regressSpread.lender <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                            + origination_charges + discount_points + loan_conventional
                            + loan_term + percent_manufactured
                            + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                            + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                            + denied_credit + tract_msa_income_percentage + market_share
                            , index = c("mode_lender")
                            , model="within"
                            , data = data)

regressSpread.both <- plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                             + origination_charges + discount_points + loan_conventional
                             + loan_term + percent_manufactured
                             + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                             + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                             + denied_credit + tract_msa_income_percentage + market_share + factor(mode_lender) 
                             , index = c("county_code")
                             , model="within"
                             , data = data)

regressSpread.both <- lm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                          + origination_charges + discount_points + loan_conventional
                          + loan_term + percent_manufactured
                          + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                          + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                          + denied_credit + tract_msa_income_percentage + market_share + factor(mode_lender) + factor(county_code)
                          , data = data)

regressSpread.weighted <- plm(rate_spreadX ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
                            + origination_charges + discount_points + loan_conventional
                            + loan_term + percent_manufactured
                            + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
                            + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
                            + denied_credit + tract_msa_income_percentage + market_share + factor(mode_lender) * market_share
                            , index = c("county_code")
                            , model="within"
                            , data = data)

regressSpread <- lm(rate_spread ~ . -census_tract - mode_lender - percent_population_black - percent_population_white, data = data)

summary(regressSpread.county.census)
plot(regressSpread.county.census)
###################################

################################
# Random analysis
################################

stargazer(regressSpread.county, regressSpread.county.census,  type = "text", omit= c("mode_lender", "county_code", "factor"))

saveRDS(regressSpread.both, file = "regress_spreads_both.RDS")
################################

#####################
#Bootstrapping
###################



test <- function(data, i) {
  coef(plm(rate_spread ~ majority_black + average_income  + loan_amount + percent_female + debt_to_income_ratio + loan_to_value_ratio 
           + origination_charges + discount_points + loan_conventional
           + loan_term + percent_manufactured
           + percent_population_asian + percent_population_native + percent_population_hispanic + percent_population_pacificislander
           + purchaser_fannie + purchaser_freddie + purchaser_ginnie   + percent_manufactured
           + denied_credit + tract_msa_income_percentage + market_share + factor(mode_lender)
           , index = c("county_code")
           , model="within"
           , data = data[i,]))[0:21]
}


boot.both.100 <- boot(data, test, R = 100)

test = data.frame(test)

sd(test$X18)
saveRDS(boot.both.100, file = "both_boot_100.RDS")
##############################3
# Descriptive Statistics
#######################
mean(data[which(data$majority_black == 1),]$Bachelors_rate, na.rm = TRUE)
mean(data[which(data$majority_black == 0),]$Bachelors_rate, na.rm = TRUE)

mean(data[which(data$majority_black == 1),]$Uninsured_rate, na.rm = TRUE)
mean(data[which(data$majority_black == 0),]$Uninsured_rate, na.rm = TRUE)

mean(data[which(data$majority_black == 1 & data$Percent_snap >= 0),]$Percent_snap, na.rm = TRUE)
mean(data[which(data$majority_black == 0 & data$Percent_snap >= 0),]$Percent_snap, na.rm = TRUE)

mean(data[which(data$majority_black == 1),]$Unemployment_rate, na.rm = TRUE)
mean(data[which(data$majority_black == 0),]$Unemployment_rate, na.rm = TRUE)

mean(data[which(data$majority_black == 1),]$vacantcy, na.rm = TRUE)
mean(data[which(data$majority_black == 0),]$vacantcy, na.rm = TRUE)

data.30 = tract.data.30

subset = data[ , -which(names(data) %in% c("county_code","census_tract", "mode_lender"))]
subset.30 = data.30[ , -which(names(data.30) %in% c("county_code","census_tract", "mode_lender"))]

meanCols = sapply(subset, mean, na.rm = T)
sdCols = sapply(subset, sd, na.rm = T)
meanCols.30 = sapply(subset.30, mean, na.rm = T)
sdCols.30 = sapply(subset.30, sd, na.rm = T)

variables = as.data.frame(colnames(subset))
variables$mean = meanCols
variables$sd = sdCols
variables$mean_30 = meanCols.30
variables$sd_30 = sdCols.30
########################################3

