library(sqldf)

###########################
#Initialize home
#########################
data <- homeLoans
rm(homeLoans)


names(data)[names(data)=="race"] <- "race1"
names(data)[names(data)=="derived_race"] <- "race"


counts <- sqldf("Select race, COUNT(*)
                 FROM data
                 Group By  race") 
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

colnames(data) = dbSafeNames(colnames(data))
colnames(small) = dbSafeNames(colnames(small))

library('RPostgreSQL')

pg = dbDriver("PostgreSQL")

con = dbConnect(pg, user="postgres", password="Mich@el98",
                host="localhost", port=5432, dbname="test")

datadf <- data.frame(data)
dbWriteTable(con,'home', datadf, row.names=FALSE)
###########################################################33

##################################
#Building by state data
###################################

library('RPostgreSQL')

pg = dbDriver("PostgreSQL")

con = dbConnect(pg, user="postgres", password="Mich@el98",
                host="localhost", port=5432, dbname="test")

#######################################
#Queries to construct state level data
#######################################
queries <- c()
queries <- c(queries, "Select state_code, AVG(income) as average_income", "Select state_code, ((count(*) filter (where action_taken = 3))::decimal  / count(*)) as percent_denied")
queries <- c(queries,  "Select state_code, AVG(loan_amount) as loan_amount", "Select state_code, AVG(loan_to_value_ratio::decimal) filter (where loan_to_value_ratio != 'Exempt') as loan_to_value_ratio")
queries <- c(queries, "Select state_code, AVG(interest_rate::decimal) filter (where interest_rate != 'Exempt') as interest_rate", "Select state_code, AVG(rate_spread::decimal) filter (where rate_spread != 'Exempt') as rate_spread")
queries <- c(queries,"Select state_code, ((count(*) filter (where hoepa_status = 1))::decimal  / count(*)) as hoepa_status", "Select state_code, AVG(total_loan_costs::decimal) filter (where total_loan_costs != 'Exempt') as total_loan_costs" )
queries <- c(queries, "Select state_code, AVG(total_points_and_fees::decimal) filter (where total_points_and_fees != 'Exempt') as total_points_and_fees", "Select state_code, AVG(origination_charges::decimal) filter (where origination_charges != 'Exempt') as origination_charges")
queries <- c(queries, "Select state_code, AVG(discount_points::decimal) filter (where discount_points != 'Exempt' and discount_points != '') as discount_points", "Select state_code, AVG(lender_credits::decimal) filter (where lender_credits != 'Exempt' and lender_credits != '') as lender_credits")
queries <- c(queries, "Select state_code, AVG(loan_term::decimal) filter (where loan_term != 'Exempt' and loan_term != '') as loan_term" )
queries <- c(queries, "Select state_code, ((count(*) filter (where conforming_loan_limit = 'NC'))::decimal  / count(*)) as conforming_loan_limit", "Select state_code, ((count(*) filter (where derived_sex = 'Female'))::decimal  / count(*)) as percent_female")
queries <- c(queries, "Select state_code, ((count(*) filter (where purchaser_type = 1))::decimal  / count(*)) as purchaser_Fannie","Select state_code, ((count(*) filter (where purchaser_type = 2))::decimal  / count(*)) as purchaser_Ginnie", "Select state_code, ((count(*) filter (where purchaser_type = 3))::decimal  / count(*)) as purchaser_Freddie", "Select state_code, ((count(*) filter (where purchaser_type = 5))::decimal  / count(*)) as purchaser_private")
queries <- c(queries, "Select state_code, ((count(*) filter (where loan_type = 1))::decimal  / count(*)) as loan_conventional", "Select state_code, AVG(property_value::decimal) filter (where property_value != 'Exempt' and property_value != '') as property_value") 
queries <- c(queries, "Select state_code, ((count(*) filter (where construction_method = 2))::decimal  / count(*)) as percent_manufactured")
queries <- c(queries, "Select state_code, ((count(*) filter (where debt_to_income_ratio = '<20%' or debt_to_income_ratio = '20%-<15%'))::decimal  / count(*)) as debt_to_income_low", 'Select state_code, ((count(*) filter (where home."denial_reason.1" = 3))::decimal  / count(*)) as denied_credit', 'Select state_code, ((count(*) filter (where home."denial_reason.1" = 1))::decimal  / count(*)) as denied_dti')             
queries <- c(queries, 'Select state_code, ((count(*) filter (where home."denial_reason.1" = 2))::decimal  / count(*)) as denied_employment')
queries <- c(queries, "Select state_code, AVG(tract_minority_population_percent::decimal) as tract_minority_population_percent", "Select state_code, AVG(ffiec_msa_md_median_family_income::decimal) as msa_median_income", "Select state_code, AVG(tract_to_msa_income_percentage::decimal) as tract_msa_income_percentage")
#########################################



##############################
#Run queries above
#############################
for(q in queries){
  query.black <- paste(q, "_black", "
                From home
                Where race = 'Black or African American'
                Group By state_code
                Order By state_code", sep = "")
 out.black <- dbGetQuery(con, query.black)
 
 
 
  query.white <- paste(q, "_white", "
                From home
                Where race = 'White'
                Group By state_code
                Order By state_code", sep = "")
  out.white <- dbGetQuery(con, query.white)
  
  query.total <- paste(q, "_total", "
                From home
                Group By state_code
                Order By state_code", sep = "")
  out.total <- dbGetQuery(con, query.total)
  
  state.data <-merge(x=state.data, y=out.black,by="state_code", all.x = TRUE)
  state.data <-merge(x=state.data, y=out.white,by="state_code", all.x = TRUE)
  state.data <-merge(x=state.data, y=out.total,by="state_code", all.x = TRUE)
}
#######################################  

  

#Applicant population by race
query.black <- paste(
                "Select state_code, ((count(*) filter (Where race = 'Black or African American'))::decimal  / count(*)) as percent_population_black 
                From home
                Group By state_code
                Order By state_code", sep = "")
out.black <- dbGetQuery(con, query.black)

query.white <- paste(
                "Select state_code, ((count(*) filter (Where race = 'White'))::decimal  / count(*)) as percent_population_white 
                From home
                Group By state_code
                Order By state_code", sep = "")
out.white <- dbGetQuery(con, query.white)

state.data <-merge(x=state.data, y=out.black,by="state_code", all.x = TRUE)
state.data <-merge(x=state.data, y=out.white,by="state_code", all.x = TRUE)

state.data <- state.data[1:54,]
state.data <- state.data[-49,]
state.data <- state.data[-40,]
state.data <- state.data[-12,]

save(state.15, file = "15year_values.RData")
####################################################  





state.15 <- state.data
##############################
#15 year only
#############################
for(q in queries){
  query.black <- paste(q, "_black", "
                From home
                Where race = 'Black or African American' and loan_term = '360'
                Group By state_code
                Order By state_code", sep = "")
  out.black <- dbGetQuery(con, query.black)
  
  
  
  query.white <- paste(q, "_white", "
                From home
                Where race = 'White' and loan_term = '360'
                Group By state_code
                Order By state_code", sep = "")
  out.white <- dbGetQuery(con, query.white)
  
  query.total <- paste(q, "_total", "
                From home
                Where loan_term = '180'
                Group By state_code
                Order By state_code", sep = "")
  out.total <- dbGetQuery(con, query.total)
  
  state.15 <-merge(x=state.15, y=out.black,by="state_code", all.x = TRUE)
  state.15 <-merge(x=state.15, y=out.white,by="state_code", all.x = TRUE)
  state.15 <-merge(x=state.15, y=out.total,by="state_code", all.x = TRUE)
}

#Applicant population by race
query.black <- paste(
  "Select state_code, ((count(*) filter (Where race = 'Black or African American'))::decimal  / count(*)) as percent_population_black 
                From home
                where loan_term = '180'
                Group By state_code
                Order By state_code", sep = "")
out.black <- dbGetQuery(con, query.black)

query.white <- paste(
  "Select state_code, ((count(*) filter (Where race = 'White'))::decimal  / count(*)) as percent_population_white 
                From home
                where loan_term = '180'
                Group By state_code
                Order By state_code", sep = "")
out.white <- dbGetQuery(con, query.white)

state.15 <-merge(x=state.15, y=out.black,by="state_code", all.x = TRUE)
state.15 <-merge(x=state.15, y=out.white,by="state_code", all.x = TRUE)

state.15 <- state.15[1:54,]
state.15 <- state.15[-49,]
state.15 <- state.15[-41,]
state.15 <- state.15[-12,]
#######################################  

loan_terms <- dbGetQuery(con, "
                        Select distinct loan_term, count(*) 
                        From home 
                        Group By loan_term 
                        Order By 2 Desc")
sum = sum(loan_terms$count)
term.15 = 	loan_terms$count[which(loan_terms$loan_term == 360)]/sum
term.15 = 	loan_terms$count[which(loan_terms$loan_term == 180)]/sum
term.short = sum(loan_terms$count[c(which(loan_terms$loan_term == 180), which(loan_terms$loan_term == 276), which(loan_terms$loan_term == 240))])/sum
term.typical = sum(loan_terms$count[c(which(loan_terms$loan_term == 360), which(loan_terms$loan_term == 180), which(loan_terms$loan_term == 276), which(loan_terms$loan_term == 240))])/sum
terms = subset(loan_terms, subset = loan_term %in% c(360, 180, 276, 240))

qplot(x=terms$loan_term, y=terms$count)

race.loan_terms <- dbGetQuery(con, "
                        Select race, ((count(*) filter (Where loan_term = '360'))::decimal  / count(*)) as thirty_year,  ((count(*) filter (Where loan_term = '180'))::decimal  / count(*)) as fifteen_year,  ((count(*) filter (Where loan_term = '180' or loan_term = '276' or loan_term = '240'))::decimal  / count(*)) as short_terms, ((count(*) filter (Where loan_term <> '180'and loan_term <> '276' and loan_term <> '240' and loan_term <> '360'))::decimal  / count(*)) as other
                        From home 
                        Group By race 
                        Order By race")


############################
# By County
############################



#######################################
#Queries to construct county level data
#######################################
queries <- c()
queries <- c(queries, "Select  county_code, AVG(income) as average_income", "Select  county_code, ((count(*) filter (where action_taken = 3))::decimal  / count(*)) as percent_denied")
queries <- c(queries,  "Select  county_code, AVG(loan_amount) as loan_amount", "Select  county_code, AVG(loan_to_value_ratio::decimal) filter (where loan_to_value_ratio != 'Exempt') as loan_to_value_ratio")
queries <- c(queries, "Select  county_code, AVG(interest_rate::decimal) filter (where interest_rate != 'Exempt') as interest_rate", "Select  county_code, AVG(rate_spread::decimal) filter (where rate_spread != 'Exempt') as rate_spread")
queries <- c(queries,"Select  county_code, ((count(*) filter (where hoepa_status = 1))::decimal  / count(*)) as hoepa_status", "Select  county_code, AVG(total_loan_costs::decimal) filter (where total_loan_costs != 'Exempt') as total_loan_costs" )
queries <- c(queries, "Select  county_code, AVG(total_points_and_fees::decimal) filter (where total_points_and_fees != 'Exempt') as total_points_and_fees", "Select  county_code, AVG(origination_charges::decimal) filter (where origination_charges != 'Exempt') as origination_charges")
queries <- c(queries, "Select  county_code, AVG(discount_points::decimal) filter (where discount_points != 'Exempt' and discount_points != '') as discount_points", "Select  county_code, AVG(lender_credits::decimal) filter (where lender_credits != 'Exempt' and lender_credits != '') as lender_credits")
queries <- c(queries, "Select  county_code, AVG(loan_term::decimal) filter (where loan_term != 'Exempt' and loan_term != '') as loan_term" )
queries <- c(queries, "Select  county_code, ((count(*) filter (where conforming_loan_limit = 'NC'))::decimal  / count(*)) as conforming_loan_limit", "Select  county_code, ((count(*) filter (where derived_sex = 'Female'))::decimal  / count(*)) as percent_female")
queries <- c(queries, "Select  county_code, ((count(*) filter (where purchaser_type = 1))::decimal  / count(*)) as purchaser_Fannie","Select  county_code, ((count(*) filter (where purchaser_type = 2))::decimal  / count(*)) as purchaser_Ginnie", "Select  county_code, ((count(*) filter (where purchaser_type = 3))::decimal  / count(*)) as purchaser_Freddie", "Select  county_code, ((count(*) filter (where purchaser_type = 5))::decimal  / count(*)) as purchaser_private")
queries <- c(queries, "Select  county_code, ((count(*) filter (where loan_type = 1))::decimal  / count(*)) as loan_conventional", "Select  county_code, AVG(property_value::decimal) filter (where property_value != 'Exempt' and property_value != '') as property_value") 
queries <- c(queries, "Select  county_code, ((count(*) filter (where construction_method = 2))::decimal  / count(*)) as percent_manufactured")
queries <- c(queries, "Select  county_code, ((count(*) filter (where debt_to_income_ratio = '<20%' or debt_to_income_ratio = '20%-<15%'))::decimal  / count(*)) as debt_to_income_low", 'Select  county_code, ((count(*) filter (where home."denial_reason.1" = 3))::decimal  / count(*)) as denied_credit', 'Select  county_code, ((count(*) filter (where home."denial_reason.1" = 1))::decimal  / count(*)) as denied_dti')             
queries <- c(queries, 'Select  county_code, ((count(*) filter (where home."denial_reason.1" = 2))::decimal  / count(*)) as denied_employment')
queries <- c(queries, "Select  county_code, AVG(tract_minority_population_percent::decimal) as tract_minority_population_percent", "Select  county_code, AVG(ffiec_msa_md_median_family_income::decimal) as msa_median_income", "Select  county_code, AVG(tract_to_msa_income_percentage::decimal) as tract_msa_income_percentage")
#########################################

##############################
#Run queries above
#############################
county.data <- dbGetQuery(con, "Select distinct county_code from home")
save(county.data, file = "county_values.RData")

for(q in queries){
  query.black <- paste(q, "_black", "
                From home
                Where race = 'Black or African American'
                Group By  county_code
                Order By county_code", sep = "")
  out.black <- dbGetQuery(con, query.black)
  
  
  
  query.white <- paste(q, "_white", "
                From home
                Where race = 'White'
                Group By  county_code
                Order By county_code", sep = "")
  out.white <- dbGetQuery(con, query.white)
  
  query.total <- paste(q, "_total", "
                From home
                Group By  county_code
                Order By county_code", sep = "")
  out.total <- dbGetQuery(con, query.total)
  
  county.data <-merge(x=county.data, y=out.black,by="county_code", all.x = TRUE)
  county.data <-merge(x=county.data, y=out.white,by="county_code", all.x = TRUE)
  county.data <-merge(x=county.data, y=out.total,by="county_code", all.x = TRUE)
}
#######################################  



#Applicant population by race
query.black <- paste(
  "Select county_code, ((count(*) filter (Where race = 'Black or African American'))::decimal  / count(*)) as percent_population_black 
                From home
                Group By county_code
                Order By county_code", sep = "")
out.black <- dbGetQuery(con, query.black)

query.white <- paste(
  "Select county_code, ((count(*) filter (Where race = 'White'))::decimal  / count(*)) as percent_population_white 
                From home
                Group By county_code
                Order By county_code", sep = "")
out.white <- dbGetQuery(con, query.white)

county.data <-merge(x=county.data, y=out.black,by="county_code", all.x = TRUE)
county.data <-merge(x=county.data, y=out.white,by="county_code", all.x = TRUE)


county.data <- county.data[-c(1,2,3,4, 3215, 3231, 3232),]


query.dti <- paste(
  "Select county_code, median(debt_to_income_ratio::integer) filter (where debt_to_income_ratio != 'Exempt' and debt_to_income_ratio != '') as debt_to_income_ratio
    From home
    Group By county_code
    Order By county_code;", sep = "")
county.dti <- dbGetQuery(con, query.dti)
county.dti$county_code <- as.integer(county.dti$county_code)

county_data_fred <- merge(x=county_data_fred, y=county.dti, by = "county_code", all.x = TRUE)
