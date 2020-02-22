library(data.table)
memory.limit(size = 16000)
library("data.table")

#read in 2018 data into data.table
data <- fread("Data/2017/2017_lar.txt", header = F, sep = "|")

Record Identifier	Respondent-ID	Agency Code	Loan Type	Property Type	Loan Purpose	Owner Occupancy	Loan Amount	Preapprovals	Type of Action Taken	Metropolitan Statistical Area/Metropolitan Division	State Code	County Code	Census Tract	Applicant Ethnicity	Co-Applicant Ethnicity	Applicant Race: 1	Applicant Race: 2	Applicant Race: 3	Applicant Race: 4	Applicant Race: 5	Co-Applicant Race: 1	Co-Applicant Race: 2	Co-Applicant Race: 3	Co-Applicant Race: 4	Co-Applicant Race: 5	Applicant Sex	Co-Applicant Sex	Applicant Income	Type of Purchaser	Denial Reason: 1	Denial Reason: 2	Denial Reason: 3	Rate Spread	HOEPA Status	Lien Status