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


  incidendce_per_100k<-read.csv("output/Annual_death_Incidence_Per_100,000_year.csv")%>%
    rename(states=state)

  Incidence_per_100k_Nigeria <- merge(states,incidendce_per_100k, by.x = "ADM1_EN", by.y = "states", all.x = TRUE)
  
  
  Incidence_per_100k_Nigeria$incidence_category <- cut(
    Incidence_per_100k_Nigeria$mean_annual_incidence,
    breaks = c(0, 2, 4, 6, 8, 10, 12),
    labels = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12"),
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
        "10-12" = "#810f7c"
      ),
        name = " Average incidenence"
    ) +theme_void() +
    theme(legend.position = c(0.93, 0.25)) +
    labs(title = "Estimated  incidence per 100,000 per year by states in Nigeria")
  

   Rabies_Map <- read.csv("output/All_map_variables.csv") %>%
    rename(
      states=state,
      Avg_annual_deaths = ts_deaths_mean,
      Avg_PEP_use_per_year = ts_exp_PEP_mean,
      Rabid_dogs_avg_per_year = ts_rabid_dogs_mean
    )
  
   Rabies_all_varible_Nigeria <- merge(states,Rabies_Map, by.x = "ADM1_EN", by.y = "states", all.x = TRUE)
   
   Rabies_all_varible_Nigeria$avg_deaths_cat <- cut(
     Rabies_all_varible_Nigeria$Avg_annual_deaths,
     breaks = c(0, 50, 100, 200, 300, 400, 500,600),
     labels = c("0-50", "50-100", "100-200", "200-300", "300-400", "400-500","500-600"),
     include.lowest = TRUE
   )
   ggplot(Rabies_all_varible_Nigeria) +
     geom_sf(aes(fill = avg_deaths_cat), color = "grey60", linewidth = 0.2) +
     geom_sf_text(aes(label = ADM1_EN), size = 2) +
     scale_fill_manual(
       values = c(
         
           "0-50"    = "#fee5d9",
           "50-100"  = "#fcae91",
           "100-200" = "#fb6a4a",
           "200-300" = "#de2d26",
           "300-400" = "#a50f15",
           "400-500" = "#67000d",
           "500-600" = "#3b0007"
         ),
         
         
        name = "Average deaths per year"
     ) +
     theme_void() +
     theme(legend.position = c(0.98, 0.3)) +
     labs(title = "Estimated average  yearly Rabies deaths by states ")
   
   
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
         
         "0-1500" = "#ffffcc",
         "1500-2500" = "#ffeda0",
         "2500-5000" = "#feb24c",
         "5000-10000" = "#fd8d3c",
         "10000-20000" = "#f03b20",
         "20000-35000" = "#bd0026"
       ),
       
       
       name = "Number_of _Rabid_dogs"
     ) +
     theme_void() +
     theme(legend.position = c(0.98, 0.3)) +
     labs(title = "Estimated Number of Rabid dogs per state in Nigeria ")
   
   





