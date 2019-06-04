#first we'll run the previously saved script that connects to the database with your credentials
source("00_connecttodb.R") # this is my file name -- use yours here instead

#load the necessary R libraries
library(tidyverse)
library(lubridate)
library(writexl)
library(dbplyr)

#pull in the filing table with the summaries from postgres db
data <- tbl(con, "cycle_2020_filing")

#look at the list of columns
glimpse(data)

#show the top handful of records
head(data)


#filter out only active presidential filings and download to LOCAL dataframe
prezdata <- data %>% 
  filter(active==TRUE,
         status == "ACTIVE",
         form == "F3P") %>% 
  collect() #this is what downloads the slice of the data locally

#now let's open it in the viewer to see what we have
prezdata %>% 
  View()

#export our results to an Excel file 
write_xlsx(prezdata, "prezdata_summaries.xlsx") #this file show up in your existing folder
