library(here)
library(sf)
library(tidyverse)
library(leaflet) 
library(htmlwidgets) #save maps as .html files

#load data
dat <- read_csv(here("data", "Towlane_all_jk.csv")) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) 

dat <- cbind(dat, st_coordinates(dat %>% select(geometry)))

#inspect
leaflet(dat) %>% 
  addTiles() %>%
  addCircleMarkers(lng = ~X, lat = ~Y, 
    popup = ~line)

#process points -> polygons
lines <- dat %>% 
  group_by(line) %>% 
  summarize(m = mean(X), do_union=FALSE) %>% 
  st_cast("LINESTRING")

#inspect
leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = lines, label = ~line) %>%
  addCircleMarkers(data = dat, lng = ~X, lat = ~Y,
                   radius = 0.5, label = ~order)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = lines %>% filter(line == "y_2_"), label = ~line) %>%
  addCircleMarkers(data = dat, lng = ~X, lat = ~Y,
    radius = 0.5, label = ~order)
