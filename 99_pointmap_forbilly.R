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


### GET AND CLEAN THE DATA ####

#use stored sample rds file
byzip_bycand <- readRDS("sample_byzip_bycand.rds")


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
  filter(lastname == "Sanders")




#### MAPPING POINTS #####

#make labels for tooltip popups
labs1 <- lapply(seq(nrow(zip_map)), function(i) {
  paste0( '<p>', 'Zip code: ', '<strong>', zip_map[i, "zip5"], '</strong></p>',
          '<p></p>', 
          "1st quarter donations: $", zip_map[i, "sumcontribs"]
  ) 
})


#create the leaftlet map object
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

#run it
m1

#save to frameable file for CMS
htmlwidgets::saveWidget(frameableWidget(m1),'candidate_byzip.html')


