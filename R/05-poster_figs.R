#' ---
#' title: "Figures for NWWN Conference Poster"
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
library(tidyverse)
library(sf)
library(ggplot2)

# +
#' # Overview
#' This script creates figures for NWWN Conference Poster. Figures include: 
#' 1) map of current tow lanes for each state (WA, OR, CA), and 
#' 2) map of WA and OR tug/tow traffic when summer lanes are active and not active.

#+ 
#' # Load Data
#' Read in the data for figures. Required data includes: 1) tow lanes, 2) state 
#' outlines (from Census Bureau website), 3) year-round tug/tow traffic grid, and 
#' 4) summer tug/tow traffic grid.
lanes <- read_sf(here("data","towlanes_2019.shp"))
domain <- read_sf(here("data","domain.shp"))
states <- read_sf(here("data","large_data","cb_us_states","cb_2018_us_state_500k.shp"))
dat.yr <- read_sf(here("data","large_data","AIS_2020_yr_grid.shp")) %>% 
  mutate(cat = ifelse(count >= 1 & count < 5, "1 - 4",
                      ifelse(count >=5 & count < 50, "5 - 49",
                             ifelse(count >= 50 & count < 100, "50 - 99",
                                    ifelse(count >= 100 & count < 200, "100 - 199","200 - 400")))))
dat.sm <- read_sf(here("data","large_data","AIS_2020_sm_grid.shp")) %>% 
  mutate(cat = ifelse(count >= 1 & count < 5, "1 - 4",
                      ifelse(count >=5 & count < 50, "5 - 49",
                             ifelse(count >= 50 & count < 100, "50 - 99",
                                    ifelse(count >= 100 & count < 200, "100 - 199","200 - 400")))))

#' # Map of Towlanes
#' ## Washington State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "green3") +
  coord_sf(xlim = c(-125.2, -123),
           ylim = c(46, 48.45)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA))
ggsave(filename = here("output","poster_figs","wa_lanes.png"))
fig

#' ## Oregon State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "green3") +
  coord_sf(xlim = c(-124.9, -122.5),
           ylim = c(41.9, 46.4)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA))
ggsave(filename = here("output","poster_figs","or_lanes.png"))
fig

#' ## California State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "green3") +
  coord_sf(xlim = c(-124.8, -122.7),
           ylim = c(38, 42.1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA))
ggsave(filename = here("output","poster_figs","ca_lanes.png"))
fig

# +
#' # Map of Vessel Data (main map)
#' Figures are gridded vessel data for each season for WA and OR.
#'

# Create city labels
wa_places <- data.frame(long = c(-123.65, -123.75, -124.3),
                        lat = c(46.14, 47.03, 48.38),
                        city = c("Astoria, OR", "Aberdeen, WA", "Neah Bay, WA"))

or_places <- data.frame(long = c(-123.6, -123.7, -123.75, -123.8),
                        lat = c(46.1, 44.6, 43.9826, 43.3665),
                        city = c("Astoria, OR", "Newport, OR", "Florence, OR", "Coos Bay, OR"))

#' ## Year-Round Figure for WA State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = dat.yr, aes(fill = factor(cat)),color = "transparent",
          show.legend = F) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_text(data = wa_places, aes(x = long, y = lat, label = city),
            color = "black", size = 2.5, fontface = "bold") +
  scale_fill_manual(name = "Vessel counts",
                    values = c("1 - 4" = "#ffffb2",
                               "5 - 49" = "#fecc5c",
                               "50 - 99" = "#fd8d3c",
                               "100 - 199" = "#f03b20",
                               "200 - 400" = "#bd0026")) +
  coord_sf(xlim = c(-125.1, -123.4),
           ylim = c(46, 48.45)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA))
ggsave(filename = here("output","poster_figs","wa_yr_grid_labels.png"))
fig

#' ## Summer Figure for WA State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = dat.sm, aes(fill = factor(cat)),color = "transparent",
          show.legend = F) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "green3") +
  geom_text(data = wa_places, aes(x = long, y = lat, label = city),
            color = "black", size = 2.5, fontface = "bold") +
  scale_fill_manual(name = "Vessel counts",
                    values = c("1 - 4" = "#ffffb2",
                               "5 - 49" = "#fecc5c",
                               "50 - 99" = "#fd8d3c",
                               "100 - 199" = "#f03b20",
                               "200 - 400" = "#bd0026")) +
  coord_sf(xlim = c(-125.1, -123.4),
           ylim = c(46, 48.45)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA))
ggsave(filename = here("output","poster_figs","wa_sm_grid_labels.png"))
fig

#' ## Year-Round Figure for OR State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = dat.yr, aes(fill = factor(cat)),color = "transparent") +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_text(data = or_places, aes(x = long, y = lat, label = city),
            color = "black", size = 2.5, fontface = "bold") +
  scale_fill_manual(name = "Vessel counts",
                    values = c("1 - 4" = "#ffffb2",
                               "5 - 49" = "#fecc5c",
                               "50 - 99" = "#fd8d3c",
                               "100 - 199" = "#f03b20",
                               "200 - 400" = "#bd0026")) +
  coord_sf(xlim = c(-124.9, -123),
           ylim = c(41.9, 46.4)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA),
        legend.justification = "top",
        legend.title = element_text(face = "bold"))
ggsave(filename = here("output","poster_figs","or_yr_grid_labels.png"))
fig

#' ## Summer Figure for OR State
fig <- ggplot() +
  geom_sf(data = states, size = 0.3) +
  geom_sf(data = dat.sm, aes(fill = factor(cat)),color = "transparent",
          show.legend = F) +
  geom_sf(data = lanes %>% filter(type == "Year-round"),color = "blue") +
  geom_sf(data = lanes %>% filter(type == "Summer"), color = "green3") +
  geom_text(data = or_places, aes(x = long, y = lat, label = city),
            color = "black", size = 2.5, fontface = "bold") +
  scale_fill_manual(name = "Vessel counts",
                    values = c("1 - 4" = "#ffffb2",
                               "5 - 49" = "#fecc5c",
                               "50 - 99" = "#fd8d3c",
                               "100 - 199" = "#f03b20",
                               "200 - 400" = "#bd0026")) +
  coord_sf(xlim = c(-124.9, -123),
           ylim = c(41.9, 46.4)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.border = element_rect(fill = NA),
        legend.justification = "top")
ggsave(filename = here("output","poster_figs","or_sm_grid_labels.png"))
fig
