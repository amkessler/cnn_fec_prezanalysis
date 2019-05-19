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

#collect into local dataframe for joining
#group by zip
by_zip_and_filer <- contribs_db %>% 
  group_by(filer_committee_id_number, zip5) %>% 
  summarise(sumcontribs = sum(contribution_amount)) %>% 
  arrange(desc(sumcontribs)) %>% 
  ungroup() %>% 
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

#see if there are any missing zips and save for review
zip_missing <- contribs_by_zip %>% 
  filter(is.na(zip5))

#reorder columns, arrange
contribs_by_zip <- contribs_by_zip %>% 
  filter(!is.na(zip5)) %>% 
  select(name, everything()) %>% 
  arrange(name, desc(sumcontribs))


#any missing zips?
contribs_by_zip %>% 
  filter(is.na(zip5))

#any repeated zips?
contribs_by_zip %>% 
  count(name, zip5) %>% 
  filter(n > 1)



#### BRING IN COMMERCIAL ZIP CODE LOOKUP TABLE #####
## add named location associated with each zip code

ziplookup_raw <- read_csv("zip-codes-database-STANDARD.csv", 
                          col_types = cols(StateFIPS = col_character()))

ziplookup <- ziplookup_raw %>% 
  clean_names() %>% 
  select(zip_code, city, state, county, state_fips, county_fips, latitude, longitude) %>% 
  distinct(zip_code, .keep_all = TRUE) 

#any repeated zips?
ziplookup %>% 
  count(zip_code, state) %>% 
  filter(n > 1)

ziplookup %>% 
  filter(zip_code == "01062") 

ziplookup

# join 
joined <- left_join(contribs_by_zip, ziplookup, by = c("zip5" = "zip_code"))

#create column for just last name of candidate
joined$lastname <- str_split(joined$name, ",", simplify = TRUE)[,1]

#final table
byzip_bycand <- joined %>% 
  ungroup %>% 
  select(lastname, everything(), -name) %>% 
  mutate(fips = paste0(state_fips, county_fips)) 

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
# write_csv(byzip_bycand_wide, "output/byzip_bycand_wide.csv")
  


#### CALCULATING COUNTY-LEVEL TOTALS BASED ON ZIPS ####
#### For Magic Wall

#start with existing zip breakdowns
byzip_bycand

#group by candidate, county
bycounty_bycand <- byzip_bycand %>% 
  group_by(lastname, fips, county, state) %>% 
  summarise(sum_in_county = sum(sumcontribs)) %>% 
  ungroup()

#write to file
write_csv(bycounty_bycand, "output/bycounty_bycand.csv")

#reshaped version to wide

# test_c <- bycounty_bycand %>% 
#   select(lastname, fips, sum_in_county)
# 
# test_c_wide <- test_c %>% 
#   # tibble::rowid_to_column() %>% 
#   spread(lastname, sum_in_county)
# 



#.................................................................
#### COMPARING TWO DIFFERENT CANDIDATES' ZIP CODE PERFORMANCE ####

# rename(!!cand1:= sumcontribs) #this pulls the variable name into the rename function

# select first candidate
cand1 <- "Booker"

z_cand1 <- byzip_bycand %>% 
  filter(lastname == cand1) %>% 
  select(zip5, cand1_contribs = sumcontribs)
  
# select second candidate
cand2 <- "Harris"

z_cand2 <- byzip_bycand %>% 
  filter(lastname == cand2) %>% 
  select(zip5, cand2_contribs = sumcontribs)  


### join to compare cand1 and cand2 in each zip
zipcompare <- full_join(z_cand1, z_cand2)

#change NAs to 0
zipcompare <- zipcompare %>% 
  replace(., is.na(.), 0)

#calculate winner and money spread
zipcompare <- zipcompare %>% 
  mutate(
    winner = ifelse(cand1_contribs>cand2_contribs, cand1, cand2),
    advantage = abs(cand1_contribs-cand2_contribs) 
  ) %>% 
  rename(!!cand1:=cand1_contribs,
         !!cand2:=cand2_contribs) #this pulls the variable name into the rename function

#join with zip lookup table
joined_temp <- left_join(zipcompare, ziplookup, by = c("zip5" = "zip_code"))
zipcompare <- joined_temp

#save to file
saveRDS(zipcompare, "zipcompare.rds")

