
loans <- homeLoans[derived_race == "Race Not Available", derived_race := NA]
rm(homeLoans)

loans$loan_to_value_ratio <- as.integer(loans$loan_to_value_ratio)
#simple regression
simpleLinear <- lm(denied ~ income + derived_race + loans$loan_to_value_ratio, data = loans)
summary(simpleLinear)






notCred <- loans[loans$`denial_reason-1` != 3]
#Leaving out credit rejections regression
subset <- notCred[,.(denied,  income, derived_race, loan_amount, loan_to_value_ratio, interest_rate, debt_to_income_ratio, derived_sex, tract_minority_population_percent, tract_to_msa_income_percentage, ffiec_msa_md_median_family_income)]
subset <- subset[, msa_income_percentage :=  as.integer(subset$tract_to_msa_income_percentage)]
subset <- subset[, minority_population_percent :=  as.integer(subset$tract_minority_population_percent)]
linear <- lm(subset$denied ~ income+ debt_to_income_ratio + subset$derived_race + loan_amount + derived_sex + minority_population_percent + msa_income_percentage, data = subset)
summary(linear)