library(here)
library(sf)
library(tidyverse)

#info
# vessel type code 52 is for tug boats, 31 and 32 for tow
# (https://api.vtexplorer.com/docs/ref-aistypes.html)

#load data
lanes <- st_read(here("data", "tow_lanes.kml")) %>% 
  st_make_valid()

ais <- read_csv(here("data", "large_data", "AIS_2021_03_15.csv"))  

coast <- st_read(here("data", "us_medium_shoreline", 
  "us_medium_shoreline.shp"))

#coos bay
coosBay <- data.frame(longitude = -124.217888, latitude = 43.366501) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326)

buffer <- st_buffer(coosBay, dist = 1e5)

#clean data
tugTow <- ais %>% 
  filter(VesselType %in% c(52, 31, 32)) %>% 
  st_as_sf(., 
  coords = c('LON', 'LAT'), 
  crs = 4326) %>% 
  st_intersection(., buffer)

coast <- st_transform(coast, crs = st_crs(buffer)) %>% 
  st_intersection(., buffer)

# lanes <- lanes %>% 
#   filter(st_is_valid(lanes)) %>% 
#   st_intersection(., buffer)

#plot data
ggplot() +
  geom_sf(data = tugTow, aes(color = VesselName)) +
  geom_sf(data = lanes, fill = "transparent") +
  geom_sf(data = coast) +
  geom_sf(data = coosBay, color = "red") +
  coord_sf(xlim = c(-124.217888 - 1, -124.217888 + 1),
    ylim = c(43.366501 - 1, 43.366501 + 1)) +
  theme_bw() +
  theme(legend.position = "none")