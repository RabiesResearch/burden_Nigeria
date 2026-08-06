# Create figures for project

rm(list=ls())
library(ggplot2)
library(readxl)
library(dplyr)
library(sf)
library(prettymapr)
library(raster)
library(viridis)
library(lubridate)
library(RColorBrewer)
library(classInt)

##import dataset

pop_df <- read.csv("data/state_data_with_burden_params_new.csv")

# Shapefiles
Nigeria <- read_sf("data/shapefiles/Nigeria/nga_admbnda_adm0_osgof_20190417.shp")
states <- read_sf("data/shapefiles/Nigeria_states/nga_admbnda_adm1_osgof_20190417.shp")
names(states)
names(pop_df)

#Merge the data sets and shapefiles
Maps_Nigeria <- merge(states,pop_df, by.x = "ADM1_EN", by.y = "state", all.x = TRUE)

## Map of Nigeria

# Create custom categories with your ranges

Maps_Nigeria$vax_cat <- cut(
  Maps_Nigeria$vc,
  breaks = c(0, 0.10, 0.20, 0.30, 0.40,0.50,0.60),
  labels = c( "0–0.10", "0.10–0.20","0.20-0.30","0.30-0.40","0.40-0.50","0.50-0.60"
  ),
  include.lowest = TRUE
)

ggplot(Maps_Nigeria) +
  geom_sf(aes(fill = vax_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2, color = "black") +
      scale_fill_manual( values =  c(
        
         "0–0.10"    = "#f7fcf5",
        "0.10–0.20" = "#e5f5e0",
        "0.20–0.30" = "#c7e9c0",
        "0.30–0.40" = "#a1d99b",
        "0.40–0.50" = "#74c476",
        "0.50–0.60" = "#238b45"
      ),
                                    
    name = "vaccination_coverage"
  ) +theme_void()+
  theme(
    legend.position = c(0.95, 0.2)
  ) 

Maps_Nigeria$pPEP_cat <- cut(
  Maps_NigeriapPEP,
  breaks = c(0, 0.4, 0.5,0.7, 1.0),
  labels = c("0-0.4","0.4-0.5","0.5-0.7","0.7-1.0"
  ),
  include.lowest = TRUE
)

ggplot(Maps_Nigeria) +
  geom_sf(aes(fill =pPEP_cat ), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2, color = "black") +
  scale_fill_manual( values =  c(
    "#f7fbff",
    "#deebf7",
    "#c6dbef",
    "#9ecae1",
    "#6baed6",
    "#3182bd",
    "#08519c"
    
  ),
  name = "pPEP"
  ) +theme_void()+
  theme(
    legend.position = c(0.95, 0.2)
  ) 

