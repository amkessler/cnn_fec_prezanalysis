#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)
library(writexl)
options(scipen = 999)


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
prez_contribs <- contribs_db %>% 
  filter(
    filer_committee_id_number %in% prez_ids,
    active==TRUE,
    status=="ACTIVE",
    entity_type=="IND"
    ) %>% 
  collect()


#alternate method
# prez_contribs <- contribs_db %>% 
#   mutate(form_type = str_to_upper(form_type)) %>% 
#   filter(
#     active==TRUE,
#     filer_committee_id_number %in% prez_ids,
#     form_type %in% c("SA17A", #individuals other than cmtes
#                      "SA18", #transfers from other cmtes
#                      "SB28A") #refunds to individuals
#   ) %>% 
#   collect()




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

#filter for only Q2
tempp1 <- tempp1 %>% 
  filter(contribution_date >= as_date("2019-04-01"))


#final table
prez_bydate <- tempp1 %>% 
  select(filer_committee_id_number, name, contribution_date, sumcontribs) %>% 
  arrange(name, contribution_date)

#save to file
write_xlsx(prez_bydate, "output/prez_by_date.xlsx")

prez_bydate

#### PLOTS ####

#faceted with all cands
p <- ggplot(data = prez_bydate, aes(contribution_date, sumcontribs)) +
  geom_line(color = "steelblue", size = 1) +
  # geom_point(color = "steelblue") +
  labs(title = "Q2 Daily Totals - Individual contributions (itemized)",
       subtitle = "",
       y = "Dollars", x = "") + 
  facet_wrap(~ name) +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0))
  )

p

