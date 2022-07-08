#' ---
#' title: "AIS Tracks - Data Merge and Split"
#' author: "L. Nguyen"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---
#' 
#+ include=F
library(tidyverse)
library(here)
library(sf)

#' # Overview
#' Previous steps of the analysis created separate data files for each vessel type. 
#' Here, the individual data files for each vessel type are merged together and 
#' then split into separate data sets based on when summer towlanes are active. 
#' For the 2019 Crabber Towboat Lane Agreement, year-round towlanes are always 
#' in effect, and summer towlanes are additional lanes available for vessel 
#' transit from April 15 to November 24.
#' 

# +
#' # Data Merge and Split
#' Data for each vessel type is combined and then split based on when the different 
#' towlanes are active. Separate data sets for summer `(sm)` and the rest of the 
#' year (identified as `yr` in this script) are exported as shapefiles.

dat <- rbind(read_sf(here("data","large_data","AISTracks_2020_VT_31.shp")),
             read_sf(here("data","large_data","AISTracks_2020_VT_32.shp")),
             read_sf(here("data","large_data","AISTracks_2020_VT_52.shp")))

dat.sm <- dat %>% filter(StartDt >= "2020-04-15" & EndDate <= "2020-11-24")
dat.yr <- dat %>% filter(StartDt <  "2020-04-15" | EndDate >  "2020-11-24")

st_write(obj = dat_sm, dsn = here("data","large_data","AISTracks_2020_sm.shp"), delete_layer = T)
st_write(obj = dat_yr, dsn = here("data","large_data","AISTracks_2020_yr.shp"), delete_layer = T)

#' # Plotting
#' Two separate figures are created to visualize the traffic during the separate periods
#' of the year when different towlanes (summer and year-round) are active. Additional data
#' used for the figures include the domain, towlanes, and coast line. Different vessel 
#' types are mapped as different colors in the figures.

domain <- st_read(here("data","domain.shp"))
lanes <- st_read(here("data","towlanes_2019.shp"))
coast <- st_read(here("data","us_medium_shoreline", "us_medium_shoreline.shp"))
coast <- st_transform(coast, crs = st_crs(domain)) %>% 
  st_intersection(.,domain)

map_yr <- ggplot() +
  geom_sf(data = coast, size = 0.05) +
  geom_sf(data = dat_yr %>% filter(VsslTyp == "31"), color = "red", size = 0.1, alpha = 0.5) +
  geom_sf(data = dat_yr %>% filter(VsslTyp == "52"), color = "green", size = 0.1, alpha = 0.5) +
  geom_sf(data = dat_yr %>% filter(VsslTyp == "32"), color = "orange", size = 0.1, alpha = 0.5) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue", size = 0.05) +  
  theme_bw()
ggsave(filename = here("output", "AISTracks_2020_yr.pdf"), device = "pdf")

map_sm <- ggplot() +
  geom_sf(data = coast, size = 0.05) +
  geom_sf(data = dat_sm %>% filter(VsslTyp == "31"), color = "red", size = 0.1, alpha = 0.5) +
  geom_sf(data = dat_sm %>% filter(VsslTyp == "52"), color = "green", size = 0.1, alpha = 0.5) +
  geom_sf(data = dat_sm %>% filter(VsslTyp == "32"), color = "orange", size = 0.1, alpha = 0.5) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue", size = 0.05) +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "blue", size = 0.05) +
  theme_bw()
ggsave(filename = here("output", "AISTracks_2020_sm.pdf"), device = "pdf")
