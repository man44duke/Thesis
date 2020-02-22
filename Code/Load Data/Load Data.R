
###############
#Pick one of following
##############
all <- FALSE
homeOnly <- FALSE
buesinessOnly <- TRUE
investProp <- FALSE


########################
#Looks at all loans application with withdrawn, purchased and incomplete apps removed
#Includes loans and preapprovals for mortguages, reverse mortguages, line of credit and business loans 
if(all == TRUE){
  load("C:/Users/goutm/Desktop/Mortgages/Thesis/2018.RData")
  loans <- data[!(action_taken == 4 | action_taken == 5 | action_taken == 6 | action_taken == 7 | action_taken == 9)]
  rm(data)
  
  #Add rejection col
  loans[,denied:=0]
  loans[loans$action_taken == 3, denied:=1]
  loans[loans$action_taken == 7, denied:=1]
  
  allLoans <- loans
  save(allLoans, file = "allLoans.RData")
}
#######################

############################
#Looks at Primary First lien Home Loans with withdrawn, purchased and incomplete apps removed
#Prapproval applications are removed
if(homeOnly == TRUE){
  load("C:/Users/goutm/Desktop/Mortgages/Thesis/2018.RData")
  loans <- data[data$action_taken == 1 || data$action_taken ==  2 || data$action_taken == 3]
  rm(data)
  
  # Home purchase, Not commericaial, Principal Residence, First Lien, not reverse mortgage, Not open end line of credit, 1 unit
  loans <- loans[loan_purpose == 1 & business_or_commercial_purpose == 2 & occupancy_type == 1 & lien_status == 1 & reverse_mortgage == 2 & loans$`open-end_line_of_credit` == 2 & total_units == "1"]
  
  #Add rejection col
  loans[,denied:=0]
  loans[loans$action_taken == 3, denied:=1]
  
  homeLoans <- loans
  save(homeLoans, file = "homeLoans.RData")
}
##########################

############################
#Looks at Business Loans
if(buesinessOnly == TRUE){
  load("C:/Users/goutm/Desktop/Mortgages/Thesis/2018.RData")
  loans <- data[data$action_taken == 1 || data$action_taken ==  2 || data$action_taken == 3]
  rm(data)
  
  loans <- loans[loans$business_or_commercial_purpose == 1]
  
  #Add rejection col
  loans[,denied:=0]
  loans[loans$action_taken == 3, denied:=1]
  
  bisLoans <- loans
  save(bisLoans, file = "bisLoans.RData")
}
##########################

############################
#Looks at Investor Loans
if(investProp == TRUE){
  load("C:/Users/goutm/Desktop/Mortgages/Thesis/2018.RData")
  loans <- data[data$action_taken == 1 || data$action_taken ==  2 || data$action_taken == 3]
  rm(data)
  
  loans <- loans[occupancy_type == 3]
  
  #Add rejection col
  loans[,denied:=0]
  loans[loans$action_taken == 3, denied:=1]
  
  investLoans <- loans
  save(investLoans, file = "investLoans.RData")
}
##########################