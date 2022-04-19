#' ---
#' title: "AIS Tracks - Data Acquisition"
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
library(stringr)
library(leaflet)
library(htmlwidgets)

# +
#' # Overview
#' This script uses AIS Vessel Tracks data for 2020 from 
#' [Marine Cadastre](https://www.fisheries.noaa.gov/inport/item/64830)
#' and crops the data to only include vessel types of interest (tugs and tows)
#' and data that is contained within the domain of the project (see script 01-make_domain.R). 
#' Due to the size of the data and limited RAM, each vessel type code is read in 
#' separately, transformed to the appropriate coordinate reference system (crs), 
#' cropped to the project domain, and exported as a shapefile to be used in 
#' subsequent steps of the analysis.
#'

download.file("https://marinecadastre.gov/downloads/data/ais/ais2020/AISVesselTracks2020.zip", 
              here("data", "large_data", "AISVesselTracks2020.zip"))
unzip(here("data", "large_data", "AISVesselTracks2020.zip"))
domain <- st_read(here("data","domain.shp"))
domain_nad <- st_transform(domain, crs = 4269)
vtypes <- c(52,32,31)

for (vtype in vtypes) {
  # use query argument to insert SQL call to limit data read into memory
  tracks <- st_read(here("data", "large_data", "AISVesselTracks2020.gdb"), 
                    layer = "AISVesselTracks2020", 
                    query = 'SELECT * FROM "AISVesselTracks2020" WHERE "VesselType" = vtype AND "Shape_Length" >0')
  
  # crs of original data is NAD83 (EPSG 4269)
  # need to transform to that of our domain which is WGS84 (EPSG 4326)
  # this step does not work as well with VesselType 31, because it is such a large dataset
  # therefore, use the domain_nad to find intersection of tracks and domain, and then transform
  ifelse(vtype == 31, 
         tracks <- st_intersection(.,domain_nad) %>% 
                   st_transform(tracks, crs = st_crs(domain)),
         tracks <- st_transform(tracks, crs = st_crs(domain)) %>%
                   st_intersection(.,domain))
	
  # split start/end date/times because shapefile cannot store both in same object
  tracks <- tracks %>% 
    separate(., TrackStartTime, c("StartDate", "StartTime"), sep = " ") %>%
    separate(., TrackEndTime, c("EndDate", "EndTime"), sep = " ")

  # Export to shapefile for future use
  st_write(obj = tracks, dsn = here("data", "large_data", str("AISTracks_2020_VT_",vtype,".shp")), delete_layer = T)
}