library(censusapi)
library(fredr)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY= "0744822d25566561a19c04275853cc2ae1a5aa83")

tracts <- NULL
for (f in fips) {
  stateget <- paste("state:", f, sep="")
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2017,
                    vars = c("NAME", "B00002_001E"),
                    region = "tract:*",
                    regionin = stateget)
  tracts <- rbind(tracts, temp)
}