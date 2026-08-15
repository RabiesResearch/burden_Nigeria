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
Rabies_Map <- read.csv("output/All_map_results_wide.csv") %>%
  rename(
    annual_rabies_deaths = ts_deaths,
    Annual_PEP_vac_vial_use = ts_vaccine_vials,
    Rabid_dogs_avg_per_year = ts_rabid_dogs,
    annual_rabies_deaths_UL = ts_deaths_UL,
    annual_rabies_deaths_LL = ts_deaths_LL,
    Annual_PEP_vac_vial_use_UL = ts_vaccine_vials_UL,
    Annual_PEP_vac_vial_use_LL = ts_vaccine_vials_LL,
    Rabid_dogs_avg_per_year_UL = ts_rabid_dogs_UL,
    Rabid_dogs_avg_per_year_LL = ts_rabid_dogs_LL
  )

Rabies_all_varible_Nigeria <- merge(states,Rabies_Map, by.x = "ADM1_EN", by.y = "state", all.x = TRUE)

Rabies_all_varible_Nigeria$avg_deaths_cat <- cut(
  Rabies_all_varible_Nigeria$annual_rabies_deaths,
  breaks = c(0, 50, 100, 200, 300, 400,600,800),
  labels = c("0-50", "50-100", "100-200", "200-300", "300-400", "400-600","600-800"),
  include.lowest = TRUE
)
ggplot(Rabies_all_varible_Nigeria) +
  geom_sf(aes(fill = avg_deaths_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2) +
  scale_fill_manual(
    values = c(
      
      "0-50"    = "#fee5d9",
      "50-100"  = "#fee0d2",
      "100-200" = "#ef4434",
      "200-300" = "#de2d26",
      "300-400" = "#a50f15",
      "400-600" = "#800026",
      "600-800" = "#67000d"
    ),
    
    
    name = "Annual deaths"
  ) +
  theme_void() +
  theme(legend.position = c(0.90, 0.25))


Rabies_all_varible_Nigeria$rabid_dog_cat <- cut(
  Rabies_all_varible_Nigeria$Rabid_dogs_avg_per_year,
  breaks = c(0,1500, 2500, 5000, 10000, 20000, 35000),
  labels = c("0-1500", "1500-2500", "2500-5000", "5000-10000", "10000-20000", "20000-35000"),
  include.lowest = TRUE
)

ggplot(Rabies_all_varible_Nigeria) +
  geom_sf(aes(fill = rabid_dog_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2) +
  scale_fill_manual(
    values = c(
      
      "0-1500" = "#fff7ec",
      "1500-2500" = "#fee0d2",
      "2500-5000" = "#fc8d56",
      "5000-10000" = "#fb6a4a",
      "10000-20000" = "#cb181d",
      "20000-35000" = "#67000d"
    ),
    
    
    name = "Number rabid dog"
  ) +
  theme_void() +
  theme(legend.position = c(0.90, 0.25))



#plot avg PEP vac vial use
Rabies_all_varible_Nigeria$avg_PEP_use_cat <- cut(
  Rabies_all_varible_Nigeria$Annual_PEP_vac_vial_use,
  breaks = c(0, 500, 1000, 2000, 5000, 9000, 12000),
  labels = c("0-500", "500-1000", "1000-2000","2000-5000" ,"5000-9000", "9000-12000"),
  include.lowest = TRUE
)
 ggplot(Rabies_all_varible_Nigeria) +
  geom_sf(aes(fill = avg_PEP_use_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2) +
  scale_fill_manual(
    values = c(
      "0-500" = "#eff6ff",
      "500-1000" = "#dbeafe",
      "1000-2000" = "#bfdbfe",
      "2000-5000" = "#93c5fd",
      "5000-9000" = "#60a5fa",
      "9000-12000" = "#2563eb"
    ),
    name = "PEP vac vial use"
  ) +
  theme_void() +
  theme(legend.position = c(0.90, 0.25))



#plot an incidence map
incidendce_per_100k<-read.csv("output/Annual_death_Incidence_Per_100,000_year.csv")%>%
  rename(states=state)

Incidence_per_100k_Nigeria <- merge(states,incidendce_per_100k, by.x = "ADM1_EN", by.y = "states", all.x = TRUE)


Incidence_per_100k_Nigeria$incidence_category <- cut(
  Incidence_per_100k_Nigeria$mean_annual_incidence,
  breaks = c(0, 2, 4, 6, 8, 10, 14),
  labels = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-14"),
  include.lowest = TRUE
)
ggplot(Incidence_per_100k_Nigeria) +
  geom_sf(aes(fill =incidence_category ), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2) +
  scale_fill_manual(
    values = c(
      
      
      "0-2"   = "#edf8fb",
      "2-4"   = "#bfd3e6",
      "4-6"   = "#9ebcda",
      "6-8"   = "#8c96c6",
      "8-10"  = "#8856a7",
      "10-14" = "#810f7c"
    ),
    name = " Annual incidence"
  ) +theme_void() +
  theme(legend.position = c(0.90, 0.25)) 



   
   