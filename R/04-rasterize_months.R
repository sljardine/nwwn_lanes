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
#library(leaftime)
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
#' 2) the AIS vessel tracks data, and 3) the towlane shapefile. The AIS data will 
#' read in during the rasterization piece, since we have separate data files for 
#' summer vs. year-round time periods.

domain <- st_read(here("data", "domain.shp"), quiet = TRUE)
lanes <- st_read(here("data", "towlanes_2019.shp"), quiet = TRUE) 

#' # Make Grid
#' To make the grid, we will use the domain file as a starting point, and use 
#' `st_make_grid()` to create polygons (grid cells) with dimensions of 0.01 deg. 

grid <- st_make_grid(domain, what = "polygons", cellsize = 0.02, crs = st_crs(domain))
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

# dat.comb <- dat.comb %>% 
#   mutate(month = ifelse(month == "02", "02-01-2020",
#                         ifelse(month == "03", "03-01-2020",
#                                ifelse(month == "04", "04-01-2020", 
#                                       ifelse(month == "04-15", "04-15-2020",
#                                              ifelse(month == "05", "05-01-2020",
#                                                     ifelse(month == "06", "06-01-2020",
#                                                            ifelse(month == "07", "07-01-2020",
#                                                                   ifelse(month == "08", "08-01-2020",
#                                                                          ifelse(month == "09", "09-01-2020",
#                                                                                 ifelse(month == "10", "10-01-2020",
#                                                                                        ifelse(month == "11", "11-01-2020",
#                                                                                               ifelse(month == "11-24", "11-24-2020","12-01-2020")))))))))))))
# 
# dat.comb <- dat.comb %>% mutate(month = as.Date(month, format = "%m-%d-%Y"))

dat.comb <- dat.comb %>% 
  arrange(month, id)

st_write(obj = dat.comb, dsn = here("data","large_data", str_c("AIS_2020_all_monthly_grid.shp")), delete_dsn = T)

#' # Plot
#' We will plot the resulting raster in leaflet, using colorbins to indicate 
#' the different number of vessel counts in each grid cell. 

pal <- colorBin(palette = "YlOrRd", domain = dat.comb$count, bins = c(1,5,25,50,100))
#pal <- colorBin(palette = "viridis", domain = dat.rast.proj$count)

# for summer map, comment hideGroup() call to show summer lanes
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
              group = "Nov 24 (year-round)",
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
               group = "Summer Lanes",
               color = "green",
               opacity = 1,
               weight = 2.5) %>%
  addLayersControl(baseGroups = c("Feb", "Mar", "Apr",
                                  "Apr 15 (summer)",
                                  "Jun (summer)", "Jul (summer)",
                                  "Aug (summer)", "Sep (summer)",
                                  "Oct (summer)", "Nov (summer)",
                                  "Nov 24 (year-round)", "Dec"),
                   overlayGroups = c("Year-Round Lanes", "Summer Lanes"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topleft") %>%
  hideGroup("Summer Lanes") %>%
  addLegend(position = "topleft",
            pal = pal,
            values = c(1,5,10,25,50,75,100),
            opacity = 0.9) %>%
  htmlwidgets::onRender("function(el, x) {
    this.on('baselayerchange',
            function(e) {e.layer.bringToBack();})
    }")

saveWidget(map, file = here("output",str_c("AIS_2020_monthly_map_v2.html")))


map <- leaflet() %>%
  addTiles() %>%
  addTimeslider(data = dat.comb,
                stroke = TRUE,
                color = "gray",
                weight = 0.15,
                fillColor = ~pal(count),
                fillOpacity = 0.9,
                options = timesliderOptions(position = "topright",
                                            timeAttribute = "month",
                                            range = TRUE)) %>% 
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
            values = c(1,5,10,25,50,75,100),
            opacity = 0.9) %>% 
  htmlwidgets::onRender("function(el, x) {
    this.on('baselayerchange', 
            function(e) {e.layer.bringToBack();})
    }")

saveWidget(map, file = here("output",str_c("AIS_2020_monthly_map_slider.html")))

pal <- colorBin(palette = "plasma", domain = test.geom$count, bins = 4)


map <- leaflet() %>% 
  addTiles() %>% 
  addGeoJSON(test.json,
             fillColor = count,
             fillOpacity = 0.9,
             color = "gray",
             weight = 0.15)

saveWidget(map, file = here("output",str_c("test.html")))
