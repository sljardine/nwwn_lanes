library(here)
library(sf)
library(tidyverse)
library(leaflet) 
library(htmlwidgets) #save maps as .html files

#load data
lanes <- st_read(here("data", "Towlanes_WASG_2007", 
  "Towlanes_WASG_2007.shp")) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326)

#plot data
lanes_map <- leaflet(lanes) %>% 
  addTiles() %>% 
  addPolygons()

saveWidget(lanes_map, file = here("output", "lanes_map.html"))
