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
#' To visualize vessel transit density along the West Coast, we will convert the 
#' AIS tracks data into a raster that represents the vessel counts through each 
#' grid cell. This script generates a grid for the domain, calculates the number 
#' of vessels that transit through each grid cell and plots the results in an 
#' interactive leaflet map.
#' 

# +
#' To begin, we will need three data sets for this analysis: 1) the domain shapefile, 
#' 2) the AIS vessel tracks data, and 3) the towlane shapefile. The AIS data will 
#' read in during the rasterization piece, since we have separate data files for 
#' summer vs. year-round time periods.

domain <- st_read(here("data", "domain.shp"), quiet = TRUE)
lanes <- st_read(here("data", "towlanes_2019.shp"), quiet = TRUE) 

#' # Make Grid
#' To make the grid, we will use the domain file as a starting point, and use 
#' `st_make_grid()` to create polygons (grid cells) with dimensions of 0.015 deg. 

grid <- st_make_grid(domain, what = "polygons", cellsize = 0.015, crs = st_crs(domain))
grid.df <- data.frame(geometry = grid) %>% 
  mutate(id = as.numeric(rownames(.)))

#' # Rasterize AIS Data
#' To convert the AIS tracks into a raster grid, we first use `st_intersects()` 
#' to find which AIS tracks intersect with which grid cells. This results in a 
#' sparse matrix, with each row representing the AIS observation and the values 
#' for each row representing the grid cell number. The sparse matrix is converted 
#' to a dataframe where we have counts of AIS observations for each grid cell.

# set period - either "yr" for year-round or "sm" for summer
periods = c("yr","sm")

for(period in periods) {
  
  print(paste("Working on", period))
  
  # set the months to loop through depending on the AIS data set
  # year-round data set will go up to April 15, and begin again on November 24
  # summer data set is the period between April 15 and November 24
  ifelse(period == "yr",
         months <- c("02", "03", "04", "11", "12"),
         months <- c("04", "05", "06", "07", "08", "09", "10", "11"))
  
  dat <- st_read(here("data", "large_data", str_c("AISTracks_2020_",period,".shp")), 
                 quiet = TRUE) %>% 
    mutate(mon = str_sub(StartDt, 6, 7))

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
    
    if(!exists("dat.comb")) {dat.comb = dat.rast.df}
    else {dat.comb = rbind(dat.comb,dat.rast.df)}
    
  }
  
}

dat.comb <- dat.comb %>% 
  arrange(month, id)

st_write(obj = dat.comb, dsn = here("data","large_data", str_c("AIS_2020_all_monthly_grid.shp")), delete_dsn = T)

#' # Plot
#' We will plot the resulting raster in leaflet, using colorbins to indicate 
#' the different number of vessel counts in each grid cell. 

pal <- colorBin(palette = "YlOrRd", domain = dat.comb$count, bins = c(1,5,25,50,100))

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

map <- leaflet() %>%
  addTiles() %>%
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
              group = "Apr",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "04-15"),
              group = "Apr 15 (summer)",
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
              group = "Nov (summer)",
              fillColor = ~pal(count),
              fillOpacity = 0.9,
              color = "gray",
              weight = 0.15) %>%
  addPolygons(data = dat.comb %>% filter(month == "11-24"),
              group = "Nov 24",
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
  addLayersControl(baseGroups = c("Feb", "Mar", "Apr",
                                  "Apr 15 (summer)", "May (summer)",
                                  "Jun (summer)", "Jul (summer)",
                                  "Aug (summer)", "Sep (summer)",
                                  "Oct (summer)", "Nov (summer)",
                                  "Nov 24", "Dec"),
                   overlayGroups = c("Year-Round Lanes", "Summer Lanes (Apr 15 - Nov 24)"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  addLegend(position = "topleft",
            pal = pal,
            title = "Vessel Count",
            values = c(1,5,25,50,100),
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

saveWidget(map, file = here("output",str_c("AIS_2020_monthly_map_v2.html")))


leaflet() %>%
  addTiles() %>%
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
  addLayersControl(baseGroups = c("Feb", "Mar"),
                   overlayGroups = c("Year-Round Lanes", "Summer Lanes (Apr 15 - Nov 24)"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  addLegend(position = "topleft",
            pal = pal,
            title = "Vessel Count",
            values = c(1,5,25,50,100),
            opacity = 0.9) %>% 
  htmlwidgets::onRender("function() {
                        $('.leaflet-control-layers-overlays').prepend('Lane Type');
                        $('.leaflet-control-layers-list').prepend('Months');
                        }") %>% 
  addControl(title, position = "topleft", className = "map-title")
  