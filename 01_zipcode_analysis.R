#run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)

contribs_db <- tbl(con, "cycle_2020_schedulea")

glimpse(contribs_db)

#filter out only individual contributions, and active ones
#create zip5 field by pulling out just first five digits
contribs_db <- contribs_db %>% 
  filter(status == "ACTIVE",
         entity_type == "IND") %>% 
  mutate(
    zip5 = str_sub(str_trim(contributor_zip), 1, 5)
  )


#group by zip
contribs_db %>% 
  group_by(filer_committee_id_number, zip5) %>% 
  summarise(sumcontribs = sum(contribution_amount)) %>% 
  arrange(desc(sumcontribs))

#collect into local dataframe
#group by zip
by_zip_and_filer <- contribs_db %>% 
  group_by(filer_committee_id_number, zip5) %>% 
  summarise(sumcontribs = sum(contribution_amount)) %>% 
  arrange(desc(sumcontribs)) %>% 
  collect()



##### BRING IN CANDIDATE TABLE TO JOIN ####
## get the associated name from fec cmte id
## we'll also limit to just Prez cmtes

cand_db <- tbl(con, "cycle_2020_candidate")

candnames <- cand_db %>% 
  filter(district == "US") %>% 
  select(name, fec_committee_id) %>% 
  collect()

#join
contribs_by_zip <- inner_join(by_zip_and_filer, candnames, by = c("filer_committee_id_number" = "fec_committee_id"))

#reorder columns, arrange
contribs_by_zip <- contribs_by_zip %>% 
  select(name, everything()) %>% 
  arrange(name, desc(sumcontribs))



#### BRING IN COMMERCIAL ZIP CODE LOOKUP TABLE #####
## add named location associated with each zip code

ziplookup <- read_csv("zip-codes-database-STANDARD.csv")

ziplookup <- ziplookup %>% 
  clean_names() %>% 
  select(zip_code, city, state) %>% 
  unique()

#join
joined <- left_join(contribs_by_zip, ziplookup, by = c("zip5" = "zip_code"))

#create column for just last name of candidate
joined$lastname <- str_split(joined$name, ",", simplify = TRUE)[,1]

#final table
byzip_bycand <- joined %>% 
  ungroup %>% 
  select(lastname, everything(), -name)

byzip_bycand

#write to file
write_csv(byzip_bycand, "output/byzip_bycand.csv")


## top 10 zips for each candidate
top10_byzip_bycand <- byzip_bycand %>% 
  group_by(lastname) %>% 
  top_n(n = 10, wt = sumcontribs) %>% 
  ungroup()

top10_byzip_bycand

#write to file
write_csv(top10_byzip_bycand, "output/top10_byzip_bycand.csv")

#any common zips?
top10_byzip_bycand %>% 
  count(zip5, city) %>% 
  arrange(desc(n))
  


# an alternative structure -- with reshaping ####
###SOMETHING NOT RIGHT HERE-- ZIPS ARE REPEATING

test <- byzip_bycand %>% 
  select(lastname, zip5, sumcontribs)

test_wide <- test %>% 
  tibble::rowid_to_column() %>% 
  spread(lastname, sumcontribs)

byzip_bycand_wide <- test_wide

#write to file
write_csv(byzip_bycand_wide, "output/byzip_bycand_wide.csv")
  
