#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)


# get prez committee IDs from candidate table

#pull candidate table from postgres db
cand_db <- tbl(con, "cycle_2020_candidate")


#filter only for presidential 
prez_cands <- cand_db %>% 
  filter(district == "US") %>%
  collect()

# create vector of IDS
prez_ids <- prez_cands %>% 
  filter(district == "US") %>% 
  select(fec_committee_id) %>% 
  pull()


#pull in the schedule A table from postgres db
contribs_db <- tbl(con, "cycle_2020_schedulea")

glimpse(contribs_db)

#filter out only individual contributions, and active ones
#download locally to dataframe
## ***NOTE: This will change per Alex's suggestions
prez_contribs <- contribs_db %>% 
  filter(status == "ACTIVE",
         entity_type == "IND",
         filer_committee_id_number %in% prez_ids) %>% 
  collect()


# format date
prez_contribs$contribution_date <- ymd(prez_contribs$contribution_date)

#group by date
prez_bydate <- prez_contribs %>% 
  group_by(filer_committee_id_number, contribution_date) %>% 
  summarise(sumcontribs = sum(contribution_amount)) 


#bring in name from cand table
prez_names <- prez_cands %>% 
  select(fec_committee_id, name)

tempp1 <- inner_join(prez_bydate, prez_names, by = c("filer_committee_id_number" = "fec_committee_id"))

#final table
prez_bydate <- tempp1 %>% 
  select(filer_committee_id_number, name, contribution_date, sumcontribs) %>% 
  arrange(name, contribution_date)

#save to file
write_csv(prez_by_date, "output/prez_by_date.csv")


