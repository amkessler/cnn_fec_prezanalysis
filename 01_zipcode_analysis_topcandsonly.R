#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)

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

#group by zip
# contribs_db %>% 
#   group_by(filer_committee_id_number, zip5) %>% 
#   summarise(sumcontribs = sum(contribution_amount)) %>% 
#   arrange(desc(sumcontribs))

#to see the actual SQL statement generated add show_query() to the above

#collect into local dataframe for joining
#group by zip
by_zip_and_filer <- contribs_db %>% 
  group_by(filer_committee_id_number, contributor_zip5) %>% 
  summarise(sumcontribs = sum(contribution_amount)) %>% 
  arrange(desc(sumcontribs)) %>% 
  ungroup() %>% 
  collect()


#any repeated zips?
by_zip_and_filer %>% 
  count(filer_committee_id_number, contributor_zip5) %>% 
  filter(n > 1)


#any missing zips?
by_zip_and_filer %>% 
  filter(is.na(contributor_zip5))




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
contribs_by_zip <- inner_join(by_zip_and_filer, candnames, by = c("filer_committee_id_number" = "fec_committee_id"))

#see if there are any missing zips and save for reference 
zip_missing <- contribs_by_zip %>% 
  filter(is.na(contributor_zip5))

#reorder columns, arrange
contribs_by_zip <- contribs_by_zip %>% 
  filter(!is.na(contributor_zip5),
         contributor_zip5 != "0",
         contributor_zip5 != "00000",
         contributor_zip5 != "99999") %>% 
  select(name, everything()) %>% 
  arrange(name, desc(sumcontribs))

#any missing zips?
contribs_by_zip %>% 
  filter(is.na(contributor_zip5))

#any repeated zips?
contribs_by_zip %>% 
  count(name, contributor_zip5) %>% 
  filter(n > 1)



#### FILTERING OUT ALL BUT THE TOP FIVE PREZ CANDIDATES #####

contribs_by_zip %>% 
  count(name, filer_committee_id_number)

#filter
contribs_by_zip <- contribs_by_zip %>% 
  filter(filer_committee_id_number %in% c("C00703975",
                                          "C00697441",
                                          "C00693234",
                                          "C00694455",
                                          "C00696948"))

#check results to confirm
contribs_by_zip %>% 
  count(name, filer_committee_id_number)




#### BRING IN ZIP CODE LOOKUP TABLE #####

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
joined <- left_join(contribs_by_zip, ziplookup, by = c("contributor_zip5" = "zip_code"))

#create column for just last name of candidate
joined$lastname <- str_split(joined$name, ",", simplify = TRUE)[,1]

#final table
byzip_bycand <- joined %>% 
  ungroup %>% 
  select(lastname, everything(), -name) %>% 
  mutate(fips = paste0(state_fips, county_fips)) 

byzip_bycand

#write to file
# write_csv(byzip_bycand, "output/byzip_bycand.csv")





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
         zipcode != "00000",
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
byzip_bycand <- left_join(byzip_bycand, irs_zips_agi_grouped, by = c("contributor_zip5" = "zipcode"))


#write to file
write_csv(byzip_bycand, "output/byzip_bycand.csv")







#### TOP 10 ZIPS FOR EACH CANDIDATE ####

#group by zip
top10_byzip_bycand <- byzip_bycand %>% 
  group_by(lastname) %>% 
  top_n(n = 10, wt = sumcontribs) %>% #pulls top 10 by sumcontribs value
  ungroup()

top10_byzip_bycand

#write to file
write_csv(top10_byzip_bycand, "output/top10_byzip_bycand.csv")

#any common zips?
top10_byzip_bycand %>% 
  count(contributor_zip5, city) %>% 
  arrange(desc(n))
  
#write to file
top10_byzip_bycand %>% 
  count(contributor_zip5, city) %>% 
  arrange(desc(n)) %>% 
write_csv("output/top10zips_mulitiplecands.csv")

# reshape to wide format as an alternative table structure ####
test <- byzip_bycand %>% 
  select(lastname, contributor_zip5, sumcontribs)

test_wide <- test %>% 
  spread(lastname, sumcontribs)

byzip_bycand_wide <- test_wide

#write to file
write_csv(byzip_bycand_wide, "output/byzip_bycand_wide.csv", na = "")


# 
# #### CALCULATING COUNTY-LEVEL TOTALS BASED ON ZIPS ####
# ## For Magic Wall
# 
# #start with existing zip breakdowns
# byzip_bycand
# 
# #group by candidate, county
# bycounty_bycand <- byzip_bycand %>% 
#   filter(!is.na(county)) %>% 
#   group_by(lastname, fips, county, state) %>% 
#   summarise(sum_in_county = sum(sumcontribs)) 
# 
# #any repeated fips?
# bycounty_bycand %>% 
#   count(lastname, fips) %>% 
#   filter(n > 1)
# 
# #write to file
# # write_csv(bycounty_bycand, "output/bycounty_bycand.csv")
# 
# 
# #reshaped version to wide
# test_c <- bycounty_bycand %>%
#   select(lastname, fips, sum_in_county)
# 
# test_c_wide <- test_c %>%
#   spread(lastname, sum_in_county)
# 
# bycounty_bycand_wide <- test_c_wide
# 
# #write to file
# # write_csv(bycounty_bycand_wide, "output/bycounty_bycand_wide.csv")
# 



#### COMPARING TWO DIFFERENT CANDIDATES' ZIP CODE PERFORMANCE ####

# select first candidate
cand1 <- "Sanders"

z_cand1 <- byzip_bycand %>% 
  filter(lastname == cand1) %>% 
  select(contributor_zip5, cand1_contribs = sumcontribs)
  
# select second candidate
cand2 <- "Warren"

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
write_csv(zipcompare, paste0("output/zipcompare_", cand1, "_vs_", cand2, ".csv"), na = "")
saveRDS(zipcompare, "zipcompare.rds")