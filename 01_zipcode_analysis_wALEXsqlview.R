#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(matrixStats)
library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)
library(writexl)
library(matrixStats)

#list the tables in the database
src_dbi(con)

#pull in the saved VIEW created from Alex's sql statement
zips <- tbl(con, "zips_by_prezcand")

glimpse(zips)

#save locally as a dataframe
zips_collected <- zips %>% 
  collect()

head(zips_collected)

#group by candidate, zip for INDIVIDUALS only
zips_by_prezcands <- zips_collected %>% 
  filter(entity_type == "IND") %>% 
  group_by(pres_cand, contributor_zip5) %>% 
  summarise(sumcontribs = sum(total))

#any missing zips?
zips_by_prezcands %>% 
  filter(is.na(contributor_zip5))

#any repeated zips?
zips_by_prezcands %>% 
  count(pres_cand, contributor_zip5) %>% 
  filter(n > 1)







#### BRING IN ZIP CODE LOOKUP TABLE ##### ------------------

## we'll add named location associated with each zip code

ziplookup_raw <- read_csv("zip-codes-database-STANDARD.csv", 
                          col_types = cols(StateFIPS = col_character()))

# keep only one distinct record per zip code
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


# join 
joined <- left_join(zips_by_prezcands, ziplookup, by = c("contributor_zip5" = "zip_code"))

#create column for just last name of candidate
joined$lastname <- str_split(joined$pres_cand, ",", simplify = TRUE)[,1]

#resulting table
byzip_bycand_joined1 <- joined %>% 
  ungroup %>% 
  select(lastname, everything(), -pres_cand) %>% 
  mutate(fips = paste0(state_fips, county_fips)) 

byzip_bycand_joined1




### ADDING IN IRS ZIP CODE ECONOMIC DATA #### -----------------------------------

irs_zip_raw <- read_csv("16zpallagi.csv", 
                        col_types = cols(STATEFIPS = col_character(), 
                                         zipcode = col_character()))

irszips_allcols <- irs_zip_raw %>% 
  clean_names()

names(irszips_allcols)


#looking for any 4 digit zips by mistake?
irszips_allcols %>% 
  filter(str_length(zipcode)==4)

#fix 4-digit zips to add back preceding 0
zipscorrected <- if_else(str_length(irszips_allcols$zipcode)==4, paste0("0", irszips_allcols$zipcode), irszips_allcols$zipcode)
irszips_allcols$zipscorrected <- zipscorrected
#check cols
irszips_allcols %>% 
  select(zipcode, zipscorrected) %>% 
  filter(zipcode != zipscorrected)
#make replacement
irszips_allcols$zipcode <- irszips_allcols$zipscorrected


#select just AGI columns
irs_zips_agi <- irszips_allcols %>% 
  select(statefips, state, zipcode, agi_stub, total_returns = n1, agi_value = a00100)

#take out the state-wide totals, zip = 0. Also remove "other" designation, 99999
irs_zips_agi <- irs_zips_agi %>% 
  filter(zipcode != "0",
         zipcode != "99999")

head(irs_zips_agi)


#looking for any 4 digit zips by mistake?
irs_zips_agi %>% 
  filter(str_length(zipcode)==4)


### create column translating agi_stub to description, per documentation
# 1 = $1 under $25,000
# 2 = $25,000 under $50,000
# 3 = $50,000 under $75,000
# 4 = $75,000 under $100,000
# 5 = $100,000 under $200,000
# 6 = $200,000 or more

irs_zips_agi <- irs_zips_agi %>% 
  mutate(
    agi_stubcategory = case_when(
      agi_stub == 1 ~ "under $25K",
      agi_stub == 2 ~ "$25K to under $50K",
      agi_stub == 3 ~ "$50K to under $75K",
      agi_stub == 4 ~ "$75K to under $100K",
      agi_stub == 5 ~ "$100K to under $200K",
      agi_stub == 6 ~ "$200K or more",
      TRUE ~ "other"
    )
  )


#sum up all agi/returns for each zip
irs_zips_agi_grouped <- irs_zips_agi %>% 
  group_by(zipcode) %>% 
  summarise(num_returns = sum(total_returns), total_agi_value = sum(agi_value)) %>% 
  mutate(avg_agi = total_agi_value/num_returns*1000)

head(irs_zips_agi_grouped)

#rank by agi
irs_zips_agi_grouped <- irs_zips_agi_grouped %>% 
  mutate(avg_agi_rank = rank(desc(avg_agi))) %>% 
  arrange(avg_agi_rank)

irs_zips_agi_grouped

# join candidates by zip with processed IRS table
byzip_bycand <- left_join(byzip_bycand_joined1, irs_zips_agi_grouped, by = c("contributor_zip5" = "zipcode"))


#write to file
write_csv(byzip_bycand, "output/byzip_bycand.csv")






#### TOP ZIPS FOR EACH CANDIDATE #### -------------------------------------

#group by zip
topzipsonly_bycand <- byzip_bycand %>% 
  group_by(lastname) %>% 
  top_n(n = 10, wt = sumcontribs) %>% #pulls top 10 by sumcontribs value
  ungroup()

topzipsonly_bycand <- topzipsonly_bycand %>% 
  arrange(lastname, desc(sumcontribs))

#write to file
write_csv(topzipsonly_bycand, "output/topzipsonly_bycand.csv")

#any common zips?
topzipsonly_bycand %>% 
  count(contributor_zip5, city) %>% 
  arrange(desc(n))
  




#### RESHAPE TO WIDE format as an alternative table structure #### ------------------------
test <- byzip_bycand %>% 
  select(lastname, contributor_zip5, sumcontribs)

test_wide <- test %>% 
  spread(lastname, sumcontribs)

# add zip code lookup info
# join 
joined_wide <- left_join(test_wide, ziplookup, by = c("contributor_zip5" = "zip_code"))

names(joined_wide)

#select column order
byzip_bycand_wide <- joined_wide %>% 
  select(
    zipcode = contributor_zip5,
    city,
    state,
    state_fips,
    Harris,
    everything(),
    -county,
    -county_fips
  )

#take out the state-wide totals, zip = 0. Also remove "other" designation, 99999
byzip_bycand_wide <- byzip_bycand_wide %>% 
  filter(zipcode != "0",
         zipcode != "00000",
         zipcode != "99999")


#write to file
# write_csv(byzip_bycand_wide, "output/byzip_bycand_wide.csv")
write_xlsx(byzip_bycand_wide, "output/byzip_bycand_wide.xlsx")




# CALIFORNIA ONLY - FOR HARRIS ANALSYSIS #### --------------------------------------------------------
byzip_bycand_wide_CAonly <- byzip_bycand_wide %>% 
  filter(state == "CA")

#write to file
# write_csv(byzip_bycand_wide_CAonly, "output/byzip_bycand_wide_CAonly.csv")
write_xlsx(byzip_bycand_wide_CAonly, "output/byzip_bycand_wide_CAonly.xlsx")

#Harris analysis

ca_harriszips <- byzip_bycand_wide_CAonly
names(ca_harriszips)







#### CALCULATING COUNTY-LEVEL TOTALS BASED ON ZIPS #### ---------------------------------------------
## For Magic Wall

#start with existing zip breakdowns
byzip_bycand

#group by candidate, county
bycounty_bycand <- byzip_bycand %>% 
  filter(!is.na(county)) %>% 
  group_by(lastname, fips, county, state) %>% 
  summarise(sum_in_county = sum(sumcontribs)) 

#any repeated fips?
bycounty_bycand %>% 
  count(lastname, fips) %>% 
  filter(n > 1)

#write to file
write_csv(bycounty_bycand, "output/bycounty_bycand.csv")




#### COMPARING TWO DIFFERENT CANDIDATES' ZIP CODE PERFORMANCE ####

# select first candidate
cand1 <- "Buttigieg"

z_cand1 <- byzip_bycand %>% 
  filter(lastname == cand1) %>% 
  select(contributor_zip5, cand1_contribs = sumcontribs)
  
# select second candidate
cand2 <- "Harris"

z_cand2 <- byzip_bycand %>% 
  filter(lastname == cand2) %>% 
  select(contributor_zip5, cand2_contribs = sumcontribs)  


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
  rename(!!cand1:=cand1_contribs, #this special coding pulls the variable name into the rename function
         !!cand2:=cand2_contribs) 

#join with zip lookup table
joined_temp <- left_join(zipcompare, ziplookup, by = c("contributor_zip5" = "zip_code"))
zipcompare <- joined_temp

#save to file
saveRDS(zipcompare, "zipcompare.rds")




#### STATEWIDE TOTALS ##### -------------------------------------------

bystate_bycand <- byzip_bycand %>% 
  group_by(lastname, state) %>% 
  summarise(sum_amt = sum(sumcontribs))

#CA only
bystate_bycand_CAonly <- bystate_bycand %>% 
  filter(state == "CA") %>% 
  arrange(desc(sum_amt))

#Top N only
#group by zip
topstatesonly_bycand <- bystate_bycand %>% 
  group_by(lastname) %>% 
  top_n(n = 10, wt = sum_amt) %>% #pulls top 10 by sumcontribs value
  ungroup()

topstatesonly_bycand <- topstatesonly_bycand %>% 
  arrange(lastname, desc(sum_amt))

#write to file
write_csv(topstatesonly_bycand, "output/topstatesonly_bycand.csv")
