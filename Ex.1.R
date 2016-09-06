library(tidyr)
library(dplyr)
library(readr)

#read csv file in RStudio
refine_original.csv <-read_csv("~/Downloads/refine_original.csv")
#put the file into  tbl_df
csvtbl <- tbl_df(refine_original.csv)
#parse out "company" column as a chracter vector
cvector <- as.character(csvtbl$company)
# clean up brand names
name <- grep("s$|S$",cvector)
cvector[name] <-"philips"
name <- grep("^a|^A", cvector)
cvector[name] <-"akzo"
name <-grep("^V|^v", cvector)
cvector[name] <- "van houten"
name <-grep("r$", cvector)
cvector[name] <- "unilever"
csvtbl$company <-cvector 

#parse out the procuct code / number column
pcn <-csvtbl$`Product code / number`
#separately create product code and product number
product_code <-gsub("-.*","", pcn)
product_number <-gsub(".*-","", pcn)
#create two new columns containing the product ode and number
csvtbl$`Product code / number` <-NULL
csvtbl <-cbind(csvtbl,product_number)
csvtbl <-cbind(csvtbl,product_code)
#add a new readable product categories
product_code_catagory <- product_code
product_code_catagory <- gsub("p","Smartphone", product_code_catagory) 
product_code_catagory <- gsub("v", "TV", product_code_catagory)
product_code_catagory <- gsub("x", "Laptop", product_code_catagory)
product_code_catagory <- gsub("q", "Tablet", product_code_catagory)
csvtbl <-cbind(csvtbl, product_code_catagory)
#Combine three columns into a new Geocode address column
csvtbl <- unite(csvtbl, full_address, address, city, country, sep =", ", remove=FALSE)
#create dummy variables
company_philips <-cvector
company_akzo <- cvector
company_van_houten <-cvector
company_unilever <-cvector
product_smartphone <- product_code
product_tv <- product_code
product_laptop <- product_code
product_tablet <- product_code
csvtbl %>% 
  gather(company, name, starts_with("company_")) %>% 
  mutate(present = 1) %>% 
  select(-company) %>% 
  spread(name, present, fill = 0)

csvtbl %>% 
  gather(product_code, name, starts_with("product_")) %>% 
  mutate(present = 1) %>% 
  select(-product_code) %>% 
  spread(name, present, fill = 0)


