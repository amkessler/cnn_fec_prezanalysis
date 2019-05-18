source("01_zipcode_analysis.R")

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
  filter(lastname == "Sanders")


#### MAPPING POINTS #####

#labels
labs1 <- lapply(seq(nrow(zip_map)), function(i) {
  paste0( '<p>', 'Zip code: ', '<strong>', zip_map[i, "zip5"], '</strong></p>',
          '<p></p>', 
          "1st quarter donations: ", zip_map[i, "sumcontribs"]
  ) 
})

m1 <- leaflet(zip_map) %>% 
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
             radius = ~sqrt(sumcontribs) * 300, 
             # fillColor = ~pal(cmag_d_spotcnt),
             label = lapply(labs1, HTML)
  ) %>%
  addControl("Individual contributions by zip code (Q1)", position = "topright") 
# %>% 
#   setView(-96, 37.8, zoom=4) 

m1

#save to frameable file for CMS

# htmlwidgets::saveWidget(frameableWidget(m1),'contribs_byzip_points.html')



#.........................................
#### MAPPING STATES ######################
#.........................................


