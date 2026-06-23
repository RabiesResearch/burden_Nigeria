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

Nigeria <- read_sf("data/shapefiles//Nigeria/nga_admbnda_adm0_osgof_20190417.shp")
states <- read_sf("data/shapefiles/Nigeria_states/nga_admbnda_adm1_osgof_20190417.shp")

#import dataset

Rabies_death_Map<- read.csv("output/Total_deaths.csv")


#Merge zones into shapefiles
Rabies_deaths_Nigeria <- merge(states,Rabies_death_Map, by.x = "ADM1_EN", by.y = "states", all.x = TRUE)

# Create custom categories with your ranges


Rabies_deaths_Nigeria$death_category <- cut(
  Rabies_deaths_Nigeria$Total_deaths,
  breaks = c(0, 50, 100, 200, 300, 500, 1000, 2000, 3000, 4000, 5000, 6000),
  labels = c(
    "0–50", "50–100", "100–200", "200–300",
    "300–500", "500–1000", "1000–2000",
    "2000–3000", "3000–4000", "4000–5000", "5000–6000"
  ),
  include.lowest = TRUE
)
unique(Rabies_deaths_Nigeria$Total_deaths)

ggplot(Rabies_deaths_Nigeria) +
  geom_sf(aes(fill = death_category), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2, color = "black") +
  scale_fill_manual(
    values = c(
      
      "0–50" = "#fff5f0",
      "50–100" = "#fee0d2",
      "100–200" = "#fcbba1",
      "200–300" = "#fc9272",
      "300–500" = "#fb6a4a",
      "500–1000" = "#ef3b2c",
      "1000–2000" = "#cb181d",
      "2000–3000" = "#a50f15",
      "3000–4000" = "#800026",
      "4000–5000" = "#66001f",
      "5000–6000" = "#4a0015"
    ),
    
    name = "Total deaths"
    
  ) +
  theme_void() +
  theme(legend.position = c(0.92, 0.3)) +
  labs(title = "Annual total deaths per state")
















