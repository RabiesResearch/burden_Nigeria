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

Nigeria <- read_sf("data//Shapefiles/Nigeria/nga_admbnda_adm0_osgof_20190417.shp")
states <- read_sf("data/Shapefiles/Nigeria_states/nga_admbnda_adm1_osgof_20190417.shp")

#import dataset

Rabies_death_Map <- read.csv("output/Total_deaths.csv") %>%
  rename(Total_deaths = total_Median)



#Merge zones into shapefiles
Rabies_death_Map <- Rabies_death_Map %>%
  rename(states = state)#renames the column state to states

Rabies_deaths_Nigeria <- merge(states,Rabies_death_Map, by.x = "ADM1_EN", by.y = "states", all.x = TRUE)

# Create custom categories with your ranges

Rabies_deaths_Nigeria$death_category <- cut(
  Rabies_deaths_Nigeria$Total_deaths,
  breaks = c(300, 500, 1000, 2000, 3000, 4000, 5000, 6000),
  labels = c(
    "300-500", "500-1000", "1000-2000",
    "2000-3000", "3000-4000", "4000-5000", "5000-6000"
  ),
  include.lowest = TRUE
)

ggplot(Rabies_deaths_Nigeria) +
  geom_sf(aes(fill = death_category), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2) +
  scale_fill_manual(
    values = c(
      "300-500"   = "#fee0d2",
      "500-1000"  = "#fc9272",
      "1000-2000" = "#fb6a4a",
      "2000-3000" = "#ef3b2c",
      "3000-4000" = "#cb181d",
      "4000-5000" = "#800026",
      "5000-6000" = "#66001f"
    ),
    name = "Total deaths"
  ) +
  theme_void() +
  theme(legend.position = c(0.93, 0.3)) +
  labs(title = "Estimated Total Rabies Deaths by States in Nigeria (10-Year Period)")


    

















