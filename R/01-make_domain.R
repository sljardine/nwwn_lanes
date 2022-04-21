#' ---
#' title: "Create Domain"
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
library(leaflet)
library(htmlwidgets)

# +
#' # Overview
#' This script creates a boundary polygon for the domain of the 
#' Crabber Towboat Lane Agreement area along the West Coast of the US.

domain <- tibble(
  group = c("domain", "domain", "domain", "domain"),
  point = c("nw", "ne", "se", "sw"),
  long  = c(-125.1, -124, -123.3, -125.1),
  lat   = c(48.5, 48.18, 38, 38)  
)

domain <- domain %>%
  st_as_sf(.,coords = c("long","lat"),crs = 4326) %>% 
  st_make_valid()

domain <- cbind(domain,(st_coordinates(domain %>% select(geometry))))

domain <- domain %>% 
  group_by(group) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

st_write(obj = domain, dsn = here("data","domain.shp"), delete_layer = T)

# +
#' # Map of Domain
#' To confirm our domain polygon encompases the area we want, 
#' plot the domain and towlanes together.

lanes <- st_read(here("data", "towlanes_2019.shp"))

boundary <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data = domain, color = "black") %>%
  addPolylines(data = lanes %>% filter(type == "Year-round"), 
               group = "Year-round", 
               color = "blue") %>%
  addPolylines(data = lanes %>% filter(type == "Summer"), 
               group = "Summer", 
               color = "green") %>%
  addPolylines(data = lanes %>% filter(type == "Advisory"), 
               group = "Advisory", 
               color = "red") %>%
  addLayersControl(overlayGroups = c("Year-round", "Summer", "Advisory"), 
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  addLegend(position = "topleft", 
            colors = c("blue","green","red"), 
            labels = c("Year-round","Summer","Advisory"), 
            opacity = 1)  

saveWidget(boundary, file = here("output","towlanes_domain_map.html"))

boundary