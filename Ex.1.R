library(tidyr)
library(dplyr)
library(readr)
#read csv file in RStudio
refine_original.csv <-read_csv("~/Downloads/refine_original.csv")
#put the file in the tbl_df
csvtbl <- tbl_df(refine_original.csv)
#sort out "company" colmn as a chracter vector
cvector <- as.vector(csvtbl$company)
View(cvector)
＃clean up brand names
namep <- grep("s$|S$",cvector)
cvector[namep] <-"philips"
namea <- grep("^a|^A", cvector)
cvector[namea] <-"akzo"
nameu <-grep("^V|^v", cvector)
cvector[nameu] <- "van houten"
namev <-grep("r$", cvector)
cvector[namev] <- "unilever"
csvtbl$company <-cvector
#Separate product code and number
#assign
pcn <-csvtbl$`Product code / number`
#reg
product_code <-gsub("-.*","", pcn)
product_number <-gsub(".*-","", pcn)
#
csvtbl$`Product code / number` <-NULL
csvtbl <-cbind(csvtbl,product_number)
csvtbl <-cbind(csvtbl,product_code)
#assign
product_code_catagory <- product_code
product_code_catagory <- gsub("p","Smartphone", product_code_catagory)
product_code_catagory <- gsub("v", "TV", product_code_catagory)
product_code_catagory <- gsub("x", "Laptop", product_code_catagory)
product_code_catagory <- gsub("q", "Tablet", product_code_catagory)
csvtbl <-cbind(csvtbl, product_code_catagory)
#Combine three columns into a new Geocode address column
csvtbl <- unite(csvtbl, full_address, address, city, country, sep =",", remove=FALSE)
#I just want to see whether it works