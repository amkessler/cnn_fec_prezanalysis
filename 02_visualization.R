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
  # addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% #alternative tile background
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
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




#pull geo from tigris
#for national map, 20m likely better here. For single states, 5m:

# cd <- congressional_districts(cb = TRUE, resolution = "20m")
state <- states(cb = TRUE, resolution = "20m")

#"simplify" file to reduce file size
# state <- ms_simplify(state, keep = 0.1)
state

#remove AK and HI
mymap <- mymap %>% 
  filter(state.name != "Alaska",
         state.name != "Hawaii")


mymap <- state
### TMAP ####

mymap_test <-  tm_shape(mymap) +
  tm_polygons("ALAND", id = "STUSPS") 

mymap_test




#### LEAFLET ####


#create test map layer

leaflet(mymap) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444") %>%
  setView(-96, 37.8, zoom=4)


### mapping ####

#palette
pal <- colorNumeric(
  palette = "Greens",
  domain = mymap$ALAND)

#labels
labs <- lapply(seq(nrow(mymap)), function(i) {
  paste0( '<p><strong>', mymap[i, "house_dist"], '</strong></p>',
          '<p></p>',
          "Pct with 4-year degree: ", mymap[i, "pct.ed.college.all"]
          # # '<p></p>', 
          # districts_wpoints[i, "cmag_r_spotcnt"]
  ) 
})

labs$Value[1]

#make map
leaflet(mymap) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = .5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~pal(pct.ed.college.all),
              label = lapply(labs, HTML),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~pct.ed.college.all,
            title = "Pct with Bachelor's or Higher",
            opacity = 1) 



#make map - key races
pal <- colorFactor(
  palette = c("#6284d9","#e76d6f","#4152ad","#b64f49","#3f842b"),
  domain = mymap$keyrace_rating)

leaflet(mymap) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = .5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~pal(keyrace_rating),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~keyrace_rating,
            title = "Key Race Ratings",
            opacity = 1) %>% 
  setView(-96, 37.8, zoom=4)






