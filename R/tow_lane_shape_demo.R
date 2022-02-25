library(here)
library(sf)
library(tidyverse)


dat <- read_csv(here("data", "Towlane_all.csv")) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) 

dat <- cbind(dat, st_coordinates(dat %>% select(geometry)))


ggplot() +
  geom_sf(data = dat) + 
  theme_bw()

line <- dat %>% slice(1 : 5) %>% summarize(m = mean(lat_s)) %>% st_cast("LINESTRING")

ggplot() +
  geom_sf(data = dat %>% slice(1 : 5), color = "grey") + 
  geom_sf(data = line, color = "blue") +
  geom_text(data = dat %>% slice(1 : 5), 
  aes(x = X, y = Y, label = label)) +
  coord_sf(xlim = c(-125.3, -124), ylim = c(46.925, 48.32778)) +
  theme_bw()