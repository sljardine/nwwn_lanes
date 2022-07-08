#' ---
#' title: "Rasterize Monthly AIS data"
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
library(htmltools)

# +
#' # Overview
#' To visualize vessel transit counts along the West Coast, the AIS tracks data 
#' is converted into a raster that represents the number of vessels that transit 
#' through each grid cell. This rasterization is completed for each month of the year 
#' to expore the temporal change in vessel traffic between summer and non-summer 
#' towlane months. This script generates a grid for the domain, 
#' calculates the number of vessels that transit through each grid cell and 
#' plots the results in an interactive leaflet map.
#' 

# +
#' # Load Data
#' The data sets needed for this analysis are: 1) the domain shapefile, 
#' 2) the AIS vessel tracks data (summer and year-round), and 3) the towlane 
#' shapefile. The AIS data will be read in during the rasterization piece, 
#' since we have separate data files for summer vs. year-round time periods.

domain <- read_sf(here("data","domain.shp"))
lanes <- read_sf(here("data","towlanes_2019.shp")) 

#' # Make Grid
#' To make the grid, start with the domain shapefile and create a raster template 
#' using `st_make_grid()` to generate polygons (grid cells) with dimensions of 0.015 deg. 

grid <- st_make_grid(domain, what = "polygons", cellsize = 0.015, crs = st_crs(domain))
grid.df <- data.frame(geometry = grid) %>% 
  mutate(id = as.numeric(rownames(.)))

#' # Rasterize AIS Data
#' To convert the AIS tracks into a raster, use the `st_intersects()` function
#' to get the counts of how many AIS tracks intersect or are contained within each 
#' grid cell. This results in a sparse matrix, with each row representing the AIS 
#' observation and the values for each row represent the grid cell number. The 
#' sparse matrix is converted to a dataframe which have the counts of AIS 
#' observations for each grid cell. 
#' 

# +
#' To perform the analysis for each month, summer data must be evaluated separately 
#' from year-round data. The following rasterization analysis is completed once 
#' for the year-round data and then again for the summer data. 

periods = c("yr","sm")

for(period in periods) {
  
  print(paste("Working on", period))
  
  # set the months to loop through depending on the AIS data set
  # year-round data set will go up to April 15, and begin again on November 24
  # summer data set is the period between April 15 and November 24
  ifelse(period == "yr",
         months <- c("01", "02", "03", "04", "11", "12"),
         months <- c("04", "05", "06", "07", "08", "09", "10", "11"))
  
  dat <- read_sf(here("data", "large_data", str_c("AISTracks_2020_",period,".shp"))) %>% 
    mutate(mon = str_sub(StartDt, 6, 7))
  
  # data includes tracks that end in 2020, but begin in 2019 - for these tracks, 
  # re-assign month to be January instead of December
  dat <- dat %>% 
    mutate(mon = ifelse(StartDt < "2020-01-01","01",mon))

  for(month in months) {
    
    print(paste("  Working on", month))
    
    dat.month <- dat %>% filter(mon == month)
    dat.rast <- st_intersects(dat.month, grid) %>%
      as.data.frame() %>%
      rename(ais_obs = row.id, grid = col.id) %>% 
      group_by(grid) %>%
      summarize(count = n())
    
    dat.rast.df <- inner_join(grid.df, dat.rast, by = c("id" = "grid"))
    dat.rast.df <- dat.rast.df %>% 
      mutate(month = (ifelse(period == "yr" && month == "11",
                             "11-24",
                             ifelse(period == "sm" && month == "04",
                                    "04-15",month)))) %>% 
      st_as_sf() %>% st_make_valid()
    
    if(!exists("dat.comb")) {
      dat.comb = dat.rast.df
    } else {
      dat.comb = rbind(dat.comb,dat.rast.df)
    }
    
  }
  
}

dat.comb <- dat.comb %>% 
  arrange(month, id)

st_write(obj = dat.comb, dsn = here("data","large_data", "AIS_2020_all_monthly_grid.shp"), delete_dsn = T)

#' # Create Interactive Map
#' The resulting raster is plotted in leaflet, using 4 colorbins to indicate 
#' the different number of vessel counts in each grid cell. 

# +
#' ## Create map title
#' The code to create the map title is based off of  
#' [this Stack Overflow thread](https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map);

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(40px, 2px);
    position: fixed !important;
    left: 0%;
    text-align: left;
    padding-left: 5px;
    padding-right: 5px;
    padding-top: 0px;
    background: rgba(255,255,255,0.75);
  }
"))

title.text <- '<h2 style="margin-top:4px; margin-bottom:0px;"> 2020 Tug/Tow AIS Vessel Traffic </h2>'
subtitle.text <- 
  '<p style="margin-top:2px;margin-bottom:2px;">Vessel traffic data obtained from <a href="https://www.fisheries.noaa.gov/inport/item/64830">Marine Cadastre</a> in the form of AIS vessel tracks.<br><a href="https://api.vtexplorer.com/docs/ref-aistypes.html">Vessel codes</a> included in this analysis are: 31 (tow), 32 (tow), and 52 (tug).</p>'
title <- tags$div(tag.map.title, HTML(paste(title.text, subtitle.text)))

# +
#' ## Leaflet Map
#' The code for adding titles to the layers control element is based off of 
#' [this website](https://heds.nz/posts/add-titles-layer-control-box-leaflet-r/). 
#' The code for ensuring that overlay layers (i.e., the towlanes) are always drawn 
#' on top of the base layers (i.e., the monthly rasters) is based off of 
#' [this Stack Overflow thread](https://stackoverflow.com/questions/44122246/r-leaflet-is-drawing-base-layers-on-top-of-overlay-layers).

pal <- colorBin(palette = "YlOrRd", domain = dat.comb$count, bins = c(1,5,25,50,100))

map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = dat.comb %>% filter(month == "01"),
              group = "Jan",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "02"),
              group = "Feb",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "03"),
              group = "Mar",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "04"),
              group = "Apr 1-15",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "04-15"),
              group = "Apr 15-30 (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "05"),
              group = "May (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "06"),
              group = "Jun (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "07"),
              group = "Jul (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "08"),
              group = "Aug (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "09"),
              group = "Sep (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "10"),
              group = "Oct (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "11"),
              group = "Nov 1-24 (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "11-24"),
              group = "Nov 24-30",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "12"),
              group = "Dec",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolylines(data = lanes %>% filter(type == "Year-round"),
               group = "Year-Round Lanes",
               color = "blue",
               opacity = 1,
               weight = 2.5) %>%
  addPolylines(data = lanes %>% filter(type == "Summer"),
               group = "Summer Lanes (Apr 15 - Nov 24)",
               color = "green",
               opacity = 1,
               weight = 2.5) %>%
  addLayersControl(baseGroups = c("Jan", "Feb", "Mar", "Apr 1-15",
                                  "Apr 15-30 (summer)", "May (summer)",
                                  "Jun (summer)", "Jul (summer)",
                                  "Aug (summer)", "Sep (summer)",
                                  "Oct (summer)", "Nov 1-24 (summer)",
                                  "Nov 24-30", "Dec"),
                   overlayGroups = c("Year-Round Lanes", "Summer Lanes (Apr 15 - Nov 24)"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  addLegend(position = "topleft",
            title = "Vessel Count",
            colors = c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"),
            labels = c("1 - 4","5 - 24","25 - 49","50 - 100"),
            opacity = 0.9) %>%
  addScaleBar(position = "bottomright") %>%
  addMiniMap(zoomLevelFixed = 4, position = "bottomleft") %>% 
  setView(lng = -123.889, lat = 46.713, zoom = 8) %>% 
  htmlwidgets::onRender("function(el, x) {
                        this.on('baselayerchange',
                                function(e) {e.layer.bringToBack();})
                        }") %>% 
  htmlwidgets::onRender("function() {
                        $('.leaflet-control-layers-overlays').prepend('Lane Type');
                        $('.leaflet-control-layers-list').prepend('Months');
                        }") %>% 
  addControl(title, position = "topleft", className = "map-title")

saveWidget(map, file = here("output",str_c("AIS_2020_monthly_map.html")))

