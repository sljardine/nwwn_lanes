#' ---
#' title: "2019 Crabber-Towboat Lanes Mapping"
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
library(here)
library(sf)
library(tidyverse)
library(leaflet) 
library(htmlwidgets) #save maps as .html files

# +
#' # Overview
#' This script creates workable forms of the 2019 West Coast Crabber-Towboat Lane Agreement Lanes.
#' The outputs of this script are: 1) a shape file, and 2) an interactive html file.

# +
#' # Load data
#' Read in data and create lines for each set of boundaries.
#' Note that the summarize() function defaults to calling st_union() on points. This 
#' rearranges the ordering of the points. To keep the original ordering, set do_union to FALSE.
lanes <- read_csv(here("data","Towlane_all_jk_ln.csv")) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"),crs = 4326) %>% st_make_valid()

lanes <- cbind(lanes, st_coordinates(lanes %>% select(geometry)))

lanes <- lanes %>% group_by(type, line) %>% summarize(do_union=FALSE) %>% st_cast("LINESTRING")

#' # Outputs
#' ## Shapefile
st_write(obj = lanes, dsn = here("data","towlanes_2019.shp"), delete_layer = T)

#' ## Interactive html
yr_lanes <- lanes %>% filter(type == "Year-round")
sm_lanes <- lanes %>% filter(type == "Summer")
ad_lanes <- lanes %>% filter(type == "Advisory")

lanes_map <- leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = yr_lanes, group = "Year-round", color = "blue") %>%
  addPolylines(data = sm_lanes, group = "Summer", color = "green") %>%
  addPolylines(data = ad_lanes, group = "Advisory", color = "red") %>%
  addLayersControl(overlayGroups = c("Year-round", "Summer", "Advisory"), 
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  addLegend(position = "topleft", colors = c("blue","green","red"), labels = c("Year-round","Summer","Advisory"), opacity = 1)

saveWidget(lanes_map, file = here("output", "towlanes_map.html"))

