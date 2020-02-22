library(censusapi)
library(fredr)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY= "0744822d25566561a19c04275853cc2ae1a5aa83")


households <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = variables, 
                        region = "county:*", 
                        regionin = "state:*")
households <- getCensus(name = "acs/acs5/subject",
                        vintage = 2017, 
                        vars = variables_subject, 
                        region = "county:*", 
                        regionin = "state:*")

variables = c("B01003_001E","B01001_002E","B01001_026E", "B00002_001E","B25002_003E", "B25002_002E", "B23006_009E", "B23006_016E", "B23006_023E", "B23006_001E", "B23006_002E", "B20005A_027E", "B20005_074E", "C18120_006E")
variables.subject = c("S2201_C04_037E")
tracts <- NULL
for (f in fips) {
  stateget <- paste("state:", f, sep="")
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2017,
                    vars = variables,
                    region = "tract:*",
                    regionin = stateget)
  tracts <- rbind(tracts, temp)
}

tract.variables = tracts
names = c("state", "county", "tract", "Total_population","Male_population","Female_population", "Total_households", "vacant", "occupied", "highschool", "some_college", "Bachelors", "All_Education", "Less_than_highschool", "Male_no_earnings", "Female_no_earnings", "Unemployed")
colnames(tract.variables) = names

tract.variables$census_tract = paste0(tract.variables$state, tract.variables$county, tract.variables$tract)
tract.variables$census_tract = as.numeric(tract.variables$census_tract)

variables.subject = c("S2201_C04_001E", "S2701_C04_001E", "S2502_C06_001E")
tracts <- NULL
for (f in fips) {
  stateget <- paste("state:", f, sep="")
  temp <- getCensus(name = "acs/acs5/subject",
                    vintage = 2017,
                    vars = variables.subject,
                    region = "tract:*",
                    regionin = stateget)
  tracts <- rbind(tracts, temp)
}

tract.subject = tracts
names.subject = c("state", "county", "tract", "Not_Snap", "not insured", "Rental_rate")
colnames(tract.subject) = names.subject

tract.subject$census_tract = paste0(tract.subject$state, tract.subject$county, tract.subject$tract)
tract.subject$census_tract = as.numeric(tract.subject$census_tract)

tract.data <-merge(x=tract_data, y=tract.variables,by="census_tract", all.x = TRUE)
tract.data <-merge(x=tract_data_census, y=tract.subject,by="census_tract", all.x = TRUE)

tract.data$No_earnings = (tract.data$Male_no_earnings + tract.data$Female_no_earnings)/ tract.data$Total_population
tract.data$vacantcy = tract.data$vacant/(tract.data$vacant + tract.data$occupied)
tract.data$Bachelors_rate = tract.data$Bachelors/tract.data$All_Education
tract.data$Less_than_highschool_rate = tract.data$Less_than_highschool/tract.data$All_Education
tract.data$Rental_rate = tract.data$rental / (tract.data$occupied + tract.data$vacant)
tract.data$Uninsured_rate = tract.data$`not insured`/tract.data$Total_population
tract.data$Percent_snap = tract.data$Not_Snap

saveRDS(tract.data, file = "tract_data_census.RDS")
