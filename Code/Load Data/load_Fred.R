library("readxl")

filenames <- list.files("Fred", pattern="*.xls", full.names=TRUE)

data <- read.csv("county_data_totals.csv")[,-1]
#data <- county.data
for (filename in filenames){
  excel_data <- read_excel(filename)
  name <- colnames(excel_data)[1]
  colnames(excel_data)[4] <- name
  colnames(excel_data)[3] <- "county_code"
  excel_data <- excel_data[-1,c(3,4)]
  excel_data <- data.frame(sapply( excel_data, as.numeric ))
  excel_data <- excel_data[!is.na(as.numeric(as.character(excel_data[,2]))),]
  data <- merge(x=data, y=excel_data, by = "county_code", all.x = TRUE)
  
}

