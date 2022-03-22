library(here)
library(sf)
library(tidyverse)
library(leaflet) 
library(htmlwidgets) #save maps as .html files

#load data
lanes <- read_csv(here("data","Towlane_all_jk_ln.csv")) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"),crs = 4326) %>% st_make_valid()

dat <- lanes[lanes$type=="Year-round",]

dat <- cbind(dat, st_coordinates(dat %>% select(geometry)))

#df <- dat[c(18,9,10,11,12),]

#plot towlane coords
tow_lanes_all <- leaflet(dat) %>% 
  addTiles() %>%
  addCircleMarkers(lng = ~X, lat = ~Y,label=~label)

saveWidget(tow_lanes_all, file = here("output", "tow_lanes_yearround.html"))

# summarize() default is to also call st_union on points, which rearranges ordering. To keep 
# original ordering, set do_union to FALSE
polys <- dat %>% group_by(line) %>% summarize(do_union=FALSE) %>% st_cast("LINESTRING")

#plot polys - to map 
lanes_map <- leaflet(polys) %>% 
  addTiles() %>% 
  addPolylines()

saveWidget(lanes_map, file = here("output", "yearround_lanes_map.html"))

ggplot() +
  geom_sf(data = dat, color = "grey") + 
  geom_sf(data = polys, color = "blue") +
  geom_text(data = dat, 
            aes(x = X, y = Y,label="")) +
  coord_sf(xlim = c(-125.3, -124), ylim = c(41.5, 44)) +
  theme_bw()

