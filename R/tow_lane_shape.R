library(here)
library(sf)
library(tidyverse)
library(leaflet) 
library(htmlwidgets) #save maps as .html files

dat <- read_csv(here("data", "towlane_order_2019.csv")) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) 

dat <- cbind(dat, st_coordinates(dat %>% select(geometry)))

towlane_order_2019 <- leaflet(dat) %>% 
  addTiles() %>%
  addCircleMarkers(lng = ~X, lat = ~Y, 
    popup = ~line)

saveWidget(towlane_order_2019, file = here("output", "towlane_order_2019.html"))


lines <- dat %>% arrange(order) %>% 
  group_by(line) %>% 
  summarize(m = mean(Y)) %>% 
  st_cast("LINESTRING")

#plot data
lanes_map <- leaflet(lines) %>% 
  addTiles() %>% 
  addPolygons()

lanes_map