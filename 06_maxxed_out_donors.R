#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)
library(writexl)
options(scipen = 999)


#list the tables in the database
src_dbi(con)

#pull in the schedule A table from postgres db
contribs_db <- tbl(con, "cycle_2020_schedulea")

glimpse(contribs_db)

#filter out only individual contributions, and active ones
#create contributor_zip5 field by pulling out just first five digits
contribs_db <- contribs_db %>% 
  filter(active==TRUE,
         status == "ACTIVE",
         entity_type == "IND") %>% 
  mutate(
    contributor_zip5 = str_sub(str_trim(contributor_zip), 1, 5)
  )


contribs_db <- contribs_db %>%
  mutate(form_type = str_to_upper(form_type)) %>%
  filter(
    active==TRUE,
    form_type %in% c("SA17A", #individuals other than cmtes
                     "SA18", #transfers from other cmtes
                     "SB28A") #refunds to individuals
  )




#### FILTERING OUT ALL BUT THE TOP FIVE PREZ CANDIDATES #####

#filter
prez_contribs <- contribs_db %>% 
  filter(filer_committee_id_number %in% c("C00703975",
                                          "C00697441",
                                          "C00693234",
                                          "C00694455",
                                          "C00696948"))

#sum totals
prez_contribs %>%
  group_by(filer_committee_id_number) %>% 
  summarise(totdollars = sum(contribution_amount))


#date totals
dates <- prez_contribs %>%
  group_by(contribution_date) %>% 
  summarise(totdollars = sum(contribution_amount)) %>% 
  collect()

dates$contribution_date <- ymd(dates$contribution_date)

dates %>% 
  arrange(desc(contribution_date))


#collect
prez_contribs <- contribs_db %>% 
  filter(filer_committee_id_number %in% c("C00703975",
                                          "C00697441",
                                          "C00693234",
                                          "C00694455",
                                          "C00696948")) %>% 
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


##### BRING IN CANDIDATE TABLE TO JOIN ####

## get the associated name from fec cmte id
## we'll also limit to just Prez cmtes

#pull candidate table from postgres db
cand_db <- tbl(con, "cycle_2020_candidate")

#filter only for presidential
candnames <- cand_db %>% 
  filter(district == "US") %>% 
  select(name, fec_committee_id) %>% 
  collect()

#join
temp <- inner_join(prez_contribs, candnames, by = c("filer_committee_id_number" = "fec_committee_id"))

#place name first in order
contribs_selected_formax <- temp %>% 
  select(name, everything())

#save as RDS
saveRDS(contribs_selected_formax, "holding/contribs_selected_formax.rds")



#### LOAD SAVED DATA - START HERE #### -------------------------------------

### pull in saved RDS created 
contribs_selected <- readRDS("holding/contribs_selected_formax.rds")



##### FINDING MAX DONORS (PRIMARY; 2800+) #### 

#see what happens if key off aggregate (revisit this more, but as initial pass)
ge2800 <- contribs_selected %>% 
  filter(contribution_aggregate >= 2800)

#trying to find how many unique donors
names(ge2800)

ge2800 %>% 
  count(contributor_last_name, contributor_first_name, contributor_zip5)


ge2800 %>% 
  count(filer_committee_id_number, contributor_last_name, contributor_first_name, contributor_zip5) %>% 
  count(filer_committee_id_number)




#total donation records per candidate?
totrecs <- contribs_selected %>% 
  count(filer_committee_id_number) %>% 
  rename(totrecs = n)

#total donation records with 2800+ aggregates
ge2800_recs <- contribs_selected %>% 
  filter(contribution_aggregate >= 2800) %>% 
  count(filer_committee_id_number) %>% 
  rename(ge2800_recs = n)

#join
join_recs <- inner_join(ge2800_recs, totrecs)

join_recs

#percentages
join_recs <- join_recs %>% 
  mutate(
    pct_ge2800 = round_half_up(ge2800_recs/totrecs * 100, 2)
  )




#### METHOD USING NAME/ZIP FOR UNIQUE DONORS FOR 2800 BREAKDOWN ####

#create the unique donor id string
contribs_selected_formax <- contribs_selected_formax %>% 
  mutate(
    donorstringid = str_c(contributor_last_name, contributor_first_name, contributor_zip5),
    donorstringid = str_squish(str_to_upper(donorstringid))
  ) 


#group donors
uniquedonor_bycand <- contribs_selected_formax %>% 
  group_by(name, donorstringid) %>% 
  summarise(cnt = n(), totcontribs = sum(contribution_amount))


#add flag for whether donor is maxxed (2800 primary per cand or not
uniquedonor_bycand <- uniquedonor_bycand %>% 
  filter(totcontribs > 0) %>% 
  mutate(
    maxxed = if_else(totcontribs >= 2800, "Y", "N")
  ) 


#calculate share of maxxed donors for each candidate
cand_max <- uniquedonor_bycand %>% 
  group_by(name, maxxed) %>% 
  summarise(total = sum(totcontribs)) 

cand_max <- cand_max %>% 
  group_by(name) %>% 
  mutate(
    percent =  (total/sum(total))*100
  )

write_xlsx(cand_max, "output/cand_max.xlsx")



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

