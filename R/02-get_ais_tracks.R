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

# +
#' # Overview
#' This script uses AIS Vessel Tracks data for 2020 from 
#' [Marine Cadastre](https://www.fisheries.noaa.gov/inport/item/64830)
#' and crops the data to only include vessel types of interest (tugs and tows)
#' and data that is contained within the domain of the project (see script 01-make_domain.R). 
#' Due to the size of the data and limited RAM, each vessel type code is read in 
#' separately, cropped to the project domain, transformed to the appropriate 
#' coordinate reference system (crs = 4326), and exported as a shapefile to be used in 
#' subsequent steps of the analysis. The individual vessel type shapefiles will be 
#' combined in the next step.
#'

if(!file.exists(here("data","large_data","AISVesselTracks2020.zip"))) {
  download.file("https://marinecadastre.gov/downloads/data/ais/ais2020/AISVesselTracks2020.zip", 
              here("data","large_data","AISVesselTracks2020.zip"))
  unzip(here("data","large_data","AISVesselTracks2020.zip"), exdir = here("data","large_data"))
}

#' The AIS data is in NAD83 or EPSG 4269 (see Spatial Information section of data source). 
#' Therefore, the domain is transformed to this crs for cropping. However, the final 
#' AIS datasets will be transformed to WGS84 or EPSG 4326. Note that for vessel 
#' type 31, this step can take up to 45 minutes. 
#' 

domain <- st_read(here("data","domain.shp"))
domain.nad <- st_transform(domain, crs = 4269)
vtypes <- c(31,32,52)

for (vtype in vtypes) {
  # use query argument to insert SQL call to limit data read into memory
  tracks <- read_sf(here("data","large_data","AISVesselTracks2020.gdb"), 
                    layer = "AISVesselTracks2020", 
                    query = str_c('SELECT * FROM "AISVesselTracks2020" WHERE "VesselType" = ', vtype, ' AND "Shape_Length" >0'))
  
  # use st_intersection to crop AIS tracks to only include tracks that are contained within the domain
  tracks <- tracks %>% 
    st_intersection(.,domain.nad) 
  
  # transform AIS tracks to WGS84
  tracks <- tracks %>% 
    st_transform(., crs = st_crs(domain))
	
  # split start/end date/times because shapefile cannot store both in same object
  tracks <- tracks %>% 
    separate(., TrackStartTime, c("StartDt","StartTm"), sep = " ") %>%
    separate(., TrackEndTime, c("EndDate","EndTime"), sep = " ")

  # Export to shapefile for future use
  st_write(obj = tracks, dsn = here("data","large_data",str("AISTracks_2020_VT_",vtype,".shp")), delete_layer = T)
  
  # clear out memory for next vtype
  rm(tracks)
}