rm(list=ls())

library(readxl)
library(dplyr)
library(sf)
library(prettymapr)
library(raster)
library(viridis)
library(lubridate)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggspatial)
library (patchwork)

Africa <- read_sf("data/shapefiles/Africa/Africa_Countries.shp")
Nigeria <- read_sf("data/shapefiles/Nigeria/nga_admbnda_adm0_osgof_20190417.shp")
states <- read_sf("data/shapefiles/Nigeria_states/nga_admbnda_adm1_osgof_20190417.shp")

#African Map
Africa_Map <- ggplot() +
  geom_sf(data = Africa, fill="grey90", color="grey40") +
  geom_sf(data = Nigeria, fill="green3", color="darkgreen", size=1) +  scale_fill_manual(values = c("Nigeria" = "green3",
                                                                                                    "Other African Countries" = "grey90"),
                                                                                         name = "Legend") +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.9) +
  theme_void() +
  theme(
    plot.title = element_text(size=16, face="bold", hjust=0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Geopolitical zones lookup table
zones <- data.frame(
  ADM1_EN = c(
    "Benue","Kogi","Kwara","Nasarawa","Niger","Plateau","Federal Capital Territory",                     # North Central
    "Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe",                     # North East
    "Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara",          # North West
    "Abia","Anambra","Ebonyi","Enugu","Imo",                                # South East
    "Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers",             # South South
    "Ekiti","Lagos","Ogun","Ondo","Osun","Oyo"                              # South West
  ),
  Zone = c(
    rep("North Central", 7),
    rep("North East", 6),
    rep("North West", 7),
    rep("South East", 5),
    rep("South South", 6),
    rep("South West", 6)
  )
)

#Merge zones into shapefiles

states2 <- merge(states, zones, by = "ADM1_EN")

#Colours for the 6 zones
zone_cols <- c(
  "North Central" = "gold",
  "North East" = "skyblue",
  "North West" = "orange",
  "South East" = "wheat1",
  "South South" = "wheat",
  "South West" = "wheat3"
)

par(mar=c(4,1,1,1))

# Plot states coloured by zone
Nigeria_Map <- ggplot() +
  geom_sf(data = states2, aes(fill = Zone), color = "grey40", size = 0.25) +
  
  # State labels
  geom_sf_text(data = states2,
               aes(label = ADM1_EN),
               size = 3,
               color = "black") +
  
  # Colour scale for zones
  scale_fill_manual(values = zone_cols, name = "Geopolitical Zones") +
  
  # Calibrated scale bar
  annotation_scale(location = "bl", width_hint = 0.20, text_cex = 0.7) +
  
  
  # Title + theme
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



combined <- (Africa_Map | Nigeria_Map)

ggsave ("combined_maps.jpeg", combined, width = 12, height = 10)

