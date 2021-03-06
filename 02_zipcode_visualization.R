source("01_zipcode_analysis_wALEXsqlview.R")
# source("01_zipcode_analysis.R")

library(tidyverse)
library(janitor)
library(tidycensus)
library(tigris)
library(sf)
library(leaflet)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(widgetframe)
library(sp)
library(raster)
library(mapview)
library(scales)
library(viridis)
options(tigris_class = "sf")


### ZIP CODE MAPPING ####

#checking to make sure zip results loaded
byzip_bycand

#filter out non-continental US 
byzip_bycand %>% 
  count(state)

us <- unique(fips_codes$state)[1:51]

zip_map <- byzip_bycand %>% 
  filter(state %in% us)

zip_map <- zip_map %>% 
  filter(state !="HI",
         state !="AK",
         !is.na(state))
  
zip_map %>% 
  count(state)


  
#pull out just a single candidate
zip_map %>% 
  count(lastname)

zip_map <- zip_map %>% 
  filter(lastname == "Harris")


#### MAPPING POINTS #####

#labels
labs1 <- lapply(seq(nrow(zip_map)), function(i) {
  paste0( '<p>', 'Zip code: ', '<strong>', zip_map[i, "contributor_zip5"], '</strong></p>',
          '<p></p>', 
          "1st quarter donations: $", zip_map[i, "sumcontribs"]
  ) 
})

m1 <- leaflet(zip_map) %>% 
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% #alternative tile background
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             # stroke = FALSE,
             # fillOpacity = .50,
             radius = ~sqrt(sumcontribs) * 300, 
             # fillColor = ~pal(cmag_d_spotcnt),
             label = lapply(labs1, HTML)
  ) %>%
  addControl("Individual donations (itemized) by zip code", position = "topright") %>% 
  setView(-96, 37.8, zoom=4)


m1

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m1),'candidate_byzip.html')





#.................................................................
#### COMPARING TWO DIFFERENT CANDIDATES' ZIP CODE PERFORMANCE ####

#check zipcompare file from step 01
zipcompare

#give it a new name for this process
zipcompare_map <- zipcompare

zipcompare_map$GEOID <- zipcompare_map$contributor_zip5

#remove any negative values 
zipcompare_map <- zipcompare_map %>% 
  filter(!!cand1 >= 0,
         !!cand2 >= 0)

# zipcompare <- zipcompare %>% 
#   filter(!str_detect(GEOID, "^99"),
#          !str_detect(GEOID, "^96"),
#          !str_detect(GEOID, "^006"),
#          !str_detect(GEOID, "^007"),
#          !str_detect(GEOID, "^009")
#   )

# #add dollar formatting
# zip_map$demdisplay <- dollar(zip_map$demtotal)
# zip_map$gopdisplay <- dollar(zip_map$goptotal)

# color palette for map
zipcompare_map$winner_text <- zipcompare_map$winner
zipcompare_map$winner <- as.factor(zipcompare_map$winner)
factpal <- colorFactor(c("green","orange"), zipcompare_map$winner)

#labels
labs2 <- lapply(seq(nrow(zipcompare_map)), function(i) {
  paste0( '<p>', 'Zip code: ', '<strong>', zipcompare_map[i, "GEOID"], '</strong></p>',
          '<p></p>', 
          "Winner ", zipcompare_map[i, "winner_text"],
          '<p></p>', 
          "Advantage in Dollars: $", zipcompare_map[i, "advantage"]
  ) 
})

m2 <- leaflet(zipcompare_map) %>% 
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = .4,
             stroke = FALSE,
             fillOpacity = .35,
             radius = ~sqrt(advantage) * 400, 
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>% 
  setView(-96, 37.8, zoom=4)

m2

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m2),'cand_zipcompare.html')


#### COMPARISON MAP OF ONLY NEW YORK CITY ####

m2_nyc <- leaflet(zipcompare_map) %>% 
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = .4,
             stroke = FALSE,
             fillOpacity = .45,
             radius = ~sqrt(advantage) * 20, 
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>% 
  setView(-73.9840, 40.7549, zoom=12)

m2_nyc

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m2_nyc),'cand_zipcompare_nyc.html')




#### COMPARISON MAP OF ONLY LA AREA ####

m4 <- leaflet(zipcompare_map) %>%
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(lng = ~longitude,
             lat = ~latitude,
             weight = .4,
             stroke = FALSE,
             fillOpacity = .45,
             radius = ~sqrt(advantage) * 20,
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>%
  setView(-118.2437, 34.0522, zoom=10)

m4

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m4),'cand_zipcompare_la.html')





#### COMPARISON MAP OF CALIFORNIA ####

m5 <- leaflet(zipcompare_map) %>%
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(lng = ~longitude,
             lat = ~latitude,
             weight = .4,
             stroke = FALSE,
             fillOpacity = .35,
             radius = ~sqrt(advantage) * 40,
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>%
  setView(-119.7871, 36.1078, zoom=7)

m5

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m5),'cand_zipcompare_california.html')



#### COMPARISON MAP OF ONLY SF AREA ####

m6 <- leaflet(zipcompare_map) %>%
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(lng = ~longitude,
             lat = ~latitude,
             weight = .4,
             stroke = FALSE,
             fillOpacity = .35,
             radius = ~sqrt(advantage) * 10,
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>%
  setView(-122.4194, 37.7749, zoom=10)

m6

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m6),'cand_zipcompare_la.html')



#### COMPARISON ISOLATING ONLY A SPECIFIC STATE - FILTERING OUT THE REST ####

zipcompare_map_onestate <- zipcompare_map %>% 
  filter(state == "IA")

m7 <- leaflet(zipcompare_map_onestate) %>%
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(lng = ~longitude,
             lat = ~latitude,
             weight = .4,
             stroke = FALSE,
             fillOpacity = .35,
             radius = ~sqrt(advantage) * 200,
             fillColor = ~factpal(winner),
             label = lapply(labs2, HTML)
  ) %>%
  addLegend("bottomright", pal = factpal, values = ~winner) %>% 
  addControl("Individual donations (itemized) by zip code", position = "topright") %>%
  setView(-93.0977, 41.8780, zoom=7)

m7

