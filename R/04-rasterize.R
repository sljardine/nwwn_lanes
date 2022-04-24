#' ---
#' title: "Rasterize AIS data"
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
library(stringr)
library(tidyverse)
library(here)
library(sf)
library(leaflet)
library(htmlwidgets)

# +
#' # Overview
#' To visualize vessel transit density along the West Coast, we will convert the 
#' AIS tracks data into a raster that represents the vessel counts through each 
#' grid cell. This script generates a grid for the domain, calculates the number 
#' of vessels that transit through each grid cell and plots the results in an 
#' interactive leaflet map.
#' 

# +
#' To begin, we will need three data sets for this analysis: 1) the domain shapefile, 
#' 2) the AIS vessel tracks data, and 3) the towlane shapefile. Because we have two 
#' separate periods of interest, we will set the period variable to either `sm` 
#' for summer or `yr` for year-round at the beginning of the script. The data will 
#' be selected based on the value of this `period` variable.

domain <- st_read(here("data", "domain.shp"))
lanes <- st_read(here("data", "towlanes_2019.shp")) 

# set period - either "yr" for year-round or "sm" for summer
period = "yr"
dat <- st_read(here("data", "large_data", str_c("AISTracks_2020_",period,".shp")))

#' # Make Grid
#' To make the grid, we will use the domain file as a starting point, and use 
#' `st_make_grid()` to create polygons (grid cells) with dimensions of 0.01 deg. 

grid <- st_make_grid(domain, what = "polygons", cellsize = .01, crs = st_crs(domain))
grid.df <- data.frame(geometry = grid) %>% 
  mutate(id = as.numeric(rownames(.)))

#' # Rasterize AIS Data
#' To convert the AIS tracks into a raster grid, we first use `st_intersects()` 
#' to find which AIS tracks intersect with which grid cells. This results in a 
#' sparse matrix, with each row representing the AIS observation and the values 
#' for each row representing the grid cell number. The sparse matrix is converted 
#' to a dataframe where we have counts of AIS observations for each grid cell.

# this step took ~10 min for summer data set and 1.5 min for year-round data set
dat.rast <- st_intersects(dat,grid) %>%
  as.data.frame() %>%
  rename(ais_obs = row.id, grid = col.id) %>% 
  group_by(grid) %>%
  summarize(count = n())

#' Once we have these counts, we can join the counts for each grid cell back with 
#' the grid itself to obtain the geometries and then convert the dataframe into 
#' a simple features object for plotting. We will also save out the data as a 
#' shapefile. With an inner join, there should be no NA values - only those values 
#' that are contained in both data sets will be retained. We will save this as 
#' a shapefile.

dat.rast.df <- inner_join(grid.df, dat.rast, by = c("id" = "grid"))
dat.rast.sf <- dat.rast.df %>% st_as_sf() %>% st_make_valid()

st_write(obj = dat.rast.sf, dsn = here("data",str_c("AIS_2020_",period,"_grid.shp")))

#' # Plot
#' We will plot the resulting raster in leaflet, using colorbins to indicate 
#' the different number of vessel counts in each grid cell. 

pal <- colorBin(palette = "viridis", domain = dat.rast.sf$count, bins = c(1,5,10,50,100,200,400))

# for summer map, comment hideGroup() call to show summer lanes
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = dat.rast.sf, 
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolylines(data = lanes %>% filter(type == "Year-round"),
               group = "Year-Round Lanes", 
               color = "red",
               opacity = 1,
               weight = 2.5) %>%
  addPolylines(data = lanes %>% filter(type == "Summer"),
               group = "Summer Lanes",
               color = "orange",
               opacity = 1,
               weight = 2.5) %>% 
  addLayersControl(overlayGroups = c("Year-Round Lanes", "Summer Lanes"), 
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  hideGroup("Summer Lanes") %>%
  addLegend(position = "topleft",
            pal = pal,
            values = c(1,10,50,100,200,400),
            opacity = 0.9)

saveWidget(map, file = here("output",str_c("AIS_2020_",period,"_map.html")))
  