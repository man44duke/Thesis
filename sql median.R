##################################
#Open Connection
###################################

library('RPostgreSQL')
library('tidyverse')

pg = dbDriver("PostgreSQL")

con = dbConnect(pg, user="postgres", password="Mich@el98",
                host="localhost", port=5432, dbname="test")

#######################################
#Queries to construct tract level data
#######################################
queries <- c()
queries <- c(queries, "Select  census_tract, median(income) as average_income", "Select  census_tract, ((count(*) filter (where action_taken = 3))::decimal  / count(*)) as percent_denied")
queries <- c(queries,  "Select  census_tract, median(loan_amount::integer) as loan_amount")
queries <- c(queries, "Select  census_tract, median(interest_rate::decimal) filter (where interest_rate != 'Exempt') as interest_rate", "Select  census_tract, median(rate_spread::decimal) filter (where rate_spread != 'Exempt') as rate_spread")
queries <- c(queries,"Select  census_tract, ((count(*) filter (where hoepa_status = 1))::decimal  / count(*)) as hoepa_status", "Select  census_tract, median(total_loan_costs::decimal) filter (where total_loan_costs != 'Exempt') as total_loan_costs" )
queries <- c(queries, "Select  census_tract, median(total_points_and_fees::decimal) filter (where total_points_and_fees != 'Exempt') as total_points_and_fees", "Select  census_tract, median(origination_charges::decimal) filter (where origination_charges != 'Exempt') as origination_charges")
queries <- c(queries, "Select  census_tract, median(discount_points::decimal) filter (where discount_points != 'Exempt' and discount_points != '') as discount_points", "Select  census_tract, median(lender_credits::decimal) filter (where lender_credits != 'Exempt' and lender_credits != '') as lender_credits")
queries <- c(queries, "Select  census_tract, median(loan_term::decimal) filter (where loan_term != 'Exempt' and loan_term != '') as loan_term" )
queries <- c(queries, "Select  census_tract, ((count(*) filter (where conforming_loan_limit = 'NC'))::decimal  / count(*)) as conforming_loan_limit", "Select  census_tract, ((count(*) filter (where derived_sex = 'Female'))::decimal  / count(*)) as percent_female")
queries <- c(queries, "Select  census_tract, ((count(*) filter (where purchaser_type = 1))::decimal  / count(*)) as purchaser_Fannie","Select  census_tract, ((count(*) filter (where purchaser_type = 2))::decimal  / count(*)) as purchaser_Ginnie", "Select  census_tract, ((count(*) filter (where purchaser_type = 3))::decimal  / count(*)) as purchaser_Freddie", "Select  census_tract, ((count(*) filter (where purchaser_type = 5))::decimal  / count(*)) as purchaser_private")
queries <- c(queries, "Select  census_tract, ((count(*) filter (where loan_type = 1))::decimal  / count(*)) as loan_conventional", "Select  census_tract, median(property_value::decimal) filter (where property_value != 'Exempt' and property_value != '') as property_value") 
queries <- c(queries, "Select  census_tract, ((count(*) filter (where construction_method = 2))::decimal  / count(*)) as percent_manufactured")
queries <- c(queries, "Select  census_tract, ((count(*) filter (where debt_to_income_ratio = '<20%' or debt_to_income_ratio = '20%-<15%'))::decimal  / count(*)) as debt_to_income_low", 'Select  census_tract, ((count(*) filter (where home."denial_reason.1" = 1))::decimal  / count(*)) as denied_dti')             
queries <- c(queries, "Select  census_tract, median(tract_minority_population_percent::decimal) as tract_minority_population_percent", "Select  census_tract, median(ffiec_msa_md_median_family_income::decimal) as msa_median_income", "Select  census_tract, median(tract_to_msa_income_percentage::decimal) as tract_msa_income_percentage")
#########################################

##############################
#Run queries above
#############################
tract.data <- dbGetQuery(con, "Select distinct census_tract from home")
save(tract.data, file = "tract_values.RData")

for(q in queries){
  query.total <- paste(q, " 
                From home
                Where total_units = '1' 
                Group By  census_tract
                Order By census_tract", sep = "")
  out.total <- dbGetQuery(con, query.total)
  
  tract.data <-merge(x=tract.data, y=out.total,by="census_tract", all.x = TRUE)
}
#######################################  



##############################
#Applicant population by race
###############################
query.black <- paste(
  "Select census_tract, ((count(*) filter (Where race = 'Black or African American'))::decimal  / count(*)) as percent_population_black 
                From home
                Where total_units = '1' 
                Group By census_tract
                Order By census_tract", sep = "")
out.black <- dbGetQuery(con, query.black)

query.white <- paste(
  "Select census_tract, ((count(*) filter (Where race = 'White'))::decimal  / count(*)) as percent_population_white 
                From home
                Where total_units = '1' 
                Group By census_tract
                Order By census_tract", sep = "")
out.white <- dbGetQuery(con, query.white)

##HERE
query.native <- paste(
  "Select census_tract, ((count(*) filter (Where race = 'American Indian or Alaska Native'))::decimal  / count(*)) as percent_population_native 
                From home
                Where total_units = '1'
                Group By census_tract
                Order By census_tract", sep = "")
out.native <- dbGetQuery(con, query.native)

query.asian <- paste(
  "Select census_tract, ((count(*) filter (Where race = 'Asian'))::decimal  / count(*)) as percent_population_asian 
                From home
                Where total_units = '1' 
                Group By census_tract
                Order By census_tract", sep = "")
out.asian <- dbGetQuery(con, query.asian)

query.pacificIslander <- paste(
  "Select census_tract, ((count(*) filter (Where race = 'Native Hawaiian or Other Pacific Islander'))::decimal  / count(*)) as percent_population_pacificIslander 
                From home
                Where total_units = '1'
                Group By census_tract
                Order By census_tract", sep = "")
out.pacificIslander <- dbGetQuery(con, query.pacificIslander)

tract.data <-merge(x=tract.data, y=out.black,by="census_tract", all.x = TRUE)
tract.data <-merge(x=tract.data, y=out.white,by="census_tract", all.x = TRUE)
tract.data <-merge(x=tract.data, y=out.asian,by="census_tract", all.x = TRUE)
tract.data <-merge(x=tract.data, y=out.pacificIslander,by="census_tract", all.x = TRUE)
tract.data <-merge(x=tract.data, y=out.native,by="census_tract", all.x = TRUE)

############################
# Population by Ethnicity
############################
query.hispanic <- paste(
  "Select census_tract, ((count(*) filter (Where derived_ethnicity = 'Hispanic or Latino'))::decimal  / count(*)) as percent_population_hispanic 
                From home
                Where total_units = '1'
                Group By census_tract
                Order By census_tract", sep = "")
out.hispanic <- dbGetQuery(con, query.hispanic)

tract.data <-merge(x=tract.data, y=out.hispanic,by="census_tract", all.x = TRUE)



##############
# Tract DTI
##############
query.dti <- paste(
  "Select census_tract, median(debt_to_income_ratio::integer) filter (where debt_to_income_ratio != 'Exempt' and debt_to_income_ratio != '') as debt_to_income_ratio
    From home
    Where total_units = '1'
    Group By census_tract
    Order By census_tract;", sep = "")
tract.dti <- dbGetQuery(con, query.dti)
tract.dti$census_tract <- as.numeric(tract.dti$census_tract)
tract.data$census_tract <- as.numeric(tract.data$census_tract)

tract.data.dti <- merge(x=tract.data, y=tract.dti, by = "census_tract", all.x = TRUE)
tract.data.dti$census_tract <- as.numeric(tract.data.dti$census_tract)
tract.data.dti <- tract.data.dti[-c(72254, 72255, 72256, 72257),]
tract.data <- tract.data.dti

tract.data$county_code <- substr(tract.data$census_tract,1,nchar(tract.data$census_tract)-6)
tract.data$county_code <- as.numeric(tract.data$county_code)

##############
# Tract Most Common Lender and Marketshare
##############
query.lender <- paste(
  "SELECT census_tract, mode() WITHIN GROUP (ORDER BY lei) AS mode_lender 
   FROM home 
   Group By census_tract;", sep = "")
tract.lender <- dbGetQuery(con, query.lender)
tract.lender$census_tract <- as.numeric(tract.lender$census_tract)
tract.data$census_tract <- as.numeric(tract.data$census_tract)

tract.data <- merge(x=tract.data, y=tract.lender, by = "census_tract", all.x = TRUE)

#Market Share
query.marketShare <- paste(
  "Select home.census_tract, ((count(*) filter (where home.lei = modes.mode_lender))::decimal  / count(*)) as market_share
   FROM home, modes
   Where home.census_tract = modes.census_tract
   Group by home.census_tract;", sep = "")
tract.marketShare <- dbGetQuery(con, query.marketShare)
tract.marketShare$census_tract <- as.numeric(tract.marketShare$census_tract)
tract.data$census_tract <- as.numeric(tract.data$census_tract)

tract.data <- merge(x=tract.data, y=tract.marketShare, by = "census_tract", all.x = TRUE)

#############
# LTV
############

query.ltv <- paste(
  "Select  census_tract, median(loan_to_value_ratio::decimal) filter (where loan_to_value_ratio != 'Exempt') as loan_to_value_ratio
   FROM home
   Where total_units = '1' 
   Group by census_tract;", sep = "")
tract.ltv <- dbGetQuery(con, query.ltv)
tract.ltv$census_tract <- as.numeric(tract.ltv$census_tract)

tract.data <- merge(x=tract.data, y=tract.ltv, by = "census_tract", all.x = TRUE)

###############
# Denied Credit
###############

query.credit <- 
  'Select  census_tract, ((count(*) filter (where home."denial_reason.1" = 3))::decimal  / count(*)) as denied_credit
   FROM home
   Group by census_tract;'
tract.credit <- dbGetQuery(con, query.credit)
tract.credit$census_tract <- as.numeric(tract.credit$census_tract)

tract.data <- merge(x=tract.data, y=tract.credit, by = "census_tract", all.x = TRUE)


###############
# Denied employment
###############

query.employment <- 
  'Select  census_tract, ((count(*) filter (where home."denial_reason.1" = 2))::decimal  / count(*)) as denied_employment
   FROM home
   Group by census_tract;'
tract.employment <- dbGetQuery(con, query.employment)
tract.employment$census_tract <- as.numeric(tract.employment$census_tract)

tract.data <- merge(x=tract.data, y=tract.employment, by = "census_tract", all.x = TRUE)

####################################
# Majority Black Binary Varible
#################################
tract.data$majority_black = 0
tract.data$majority_black[tract.data$percent_population_black >= .5] = 1 

tract.data$black_25 = 0
tract.data$black_25[tract.data$percent_population_black >= .25] = 1 

####################################
# Majority white Binary Varible
#################################

tract.data$majority_white = 0
tract.data$majority_white[tract.data$percent_population_white >= .5] = 1 

tract.data$white_25 = 0
tract.data$white_25[tract.data$percent_population_white >= .25] = 1 
#############################
# Normalize
#########################


tract.data$loan_amount = tract.data$loan_amount/ 10000
tract.data$total_loan_costs = tract.data$total_loan_costs / 1000
tract.data$total_points_and_fees = tract.data$total_points_and_fees /1000
tract.data$origination_charges = tract.data$origination_charges /1000
tract.data$discount_points = tract.data$discount_points /1000
tract.data$lender_credits = tract.data$lender_credits /1000
tract.data$msa_median_income = tract.data$msa_median_income /1000
tract.data$property_value = tract.data$property_value /1000
tract.data$tract_msa_income_percentage = tract.data$tract_msa_income_percentage/100

tract_data_census <- readRDS("RData/Tract/tract_data_census.RDS")
tract_data_subset <- tract_data_census %>% select(Total_population:Percent_snap)
tract_data_subset$census_tract <- tract_data_census$census_tract

tract.data <- merge(x=tract.data, y=tract_data_subset, by = "census_tract", all.x = TRUE)

saveRDS(tract.data, file = "tract_data_median.RDS")
