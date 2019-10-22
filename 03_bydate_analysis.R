#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)
library(writexl)
library(plotly)
library(scales)
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

# prez_contribs <- contribs_db %>% 
#   filter(
#     filer_committee_id_number %in% prez_ids,
#     active==TRUE,
#     status=="ACTIVE",
#     entity_type=="IND"
#     ) %>% 
#   collect()

#filter for just ONE candidate: Booker

prez_contribs <- contribs_db %>% 
  filter(
    filer_committee_id_number == "C00695510",
    active==TRUE,
    status=="ACTIVE",
    entity_type=="IND"
  ) %>% 
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

#filter for only Q3
tempp1 <- tempp1 %>% 
  filter(contribution_date >= as_date("2019-07-01"))


#final table
prez_bydate <- tempp1 %>% 
  select(filer_committee_id_number, name, contribution_date, sumcontribs) %>% 
  arrange(name, contribution_date)

#save to file
write_xlsx(prez_bydate, "output/prez_by_date.xlsx")

prez_bydate



#### Booker analysis of 10-day fundraising sprint ####

#total for 10-day period
prez_bydate %>% 
  filter(contribution_date >= "2019-09-21") %>% 
  summarise(sum(sumcontribs))











#### PLOTS #### --------------------------------------------

#line chart
p <- ggplot(data = prez_bydate, aes(contribution_date, sumcontribs)) +
  geom_line(color = "steelblue", size = 1) +
  # geom_point(color = "steelblue") +
  labs(title = "Booker Daily Totals - Individual contributions (itemized)",
       subtitle = "",
       y = "", x = "") + 
  # facet_wrap(~ name) +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0))
  ) +
  scale_y_continuous(labels = dollar)

p



#bar chart
d <- ggplot(data = prez_bydate, aes(x = contribution_date, y = sumcontribs)) +
  geom_col(
    # color = "#848484",
    fill = "lightblue") +
  # coord_flip() +
  theme_minimal()

d

d2 <- d + labs(title = "Booker Daily Totals - Individual contributions (itemized)",
               x ="", 
               y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # scale_fill_manual(values=cbPalette) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = dollar)

d2

dd <- ggplotly(d2)

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu


