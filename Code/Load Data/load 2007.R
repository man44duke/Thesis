library(readr)
library(data.table)

loans_2007 <-  read_fwf("Data/lars.final.2007.dat", fwf_widths(c(4,10,1,1,1,1,5,1,5,2,3,7,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,7)))

loans_2007 <- setDT(loans_2007)

save(loans_2007, file = "loans_2007.RData")
