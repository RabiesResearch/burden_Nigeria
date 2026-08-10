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
library(patchwork)

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

Maps_Nigeria$pop_millions <- Maps_Nigeria$humans/ 1e6

# Create custom categories with your ranges

Maps_Nigeria$pop_cat <- cut(
  Maps_Nigeria$pop_millions,
  breaks = c(1, 3, 6, 9, 12, 15),
  labels = c("1–3", "3–6", "6–9", "9–12", "12–15"),
  include.lowest = TRUE
)

pop_map<-ggplot(Maps_Nigeria) +
  geom_sf(aes(fill = pop_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2, color = "black") +
  
  scale_fill_manual(
                      values = c(
                      
                        "1–3" = "#f2f0f7",
                        "3–6" = "#dadaeb",
                        "6–9" = "#bcbddc",
                        "9–12" = "#807dba",
                        "12–15" = "#4a1486"
        
                      ),
            
           name = "population in millions" ) +
           theme_void() +
  theme(
    legend.position = c(0.90,0.2))
   




Maps_Nigeria$pPEP_cat <- cut(
  Maps_Nigeria$pPEP,
  breaks = c(0.4, 0.5,0.7, 1.0),
  labels = c("0.4-0.5","0.5-0.7","0.7-1.0"
  ),
  include.lowest = TRUE
)

PEP_map <-ggplot(Maps_Nigeria) +
  geom_sf(aes(fill =pPEP_cat ), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2.5, color = "black") +
  scale_fill_manual( 
    values =  c(
      
      "0.4-0.5"=  "#deebf7",
      "0.5-0.7"=  "#3182bd",
      "0.7-1.0"=  "#08519c"
      
    ),
    name = "pPEP"
  ) +theme_void()+
  theme(
    legend.position = c(0.90,0.2))

Maps_Nigeria$hdr_cat <- cut(
  Maps_Nigeria$hdr,
  breaks = c(3, 5, 10, 15, 20, 25,30),
  labels = c("3-5","5-10","10-15","15-20","20-25","25-30"),
  include.lowest = TRUE
)

hdr_map <-ggplot(Maps_Nigeria) +
  geom_sf(aes(fill = hdr_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2.5, color = "black") +
  
  scale_fill_manual(
    values = c(
      
      "3-5" = "#fff5eb",
      "5-10" = "#fee6ce",
      "10-15" = "#fdd0a2",
      "15-20" = "#fdae6b",
      "20-25" = "#fd8d3c",
      "25-30" = "#e6550d"
    ),
    
    name = "hdr" ) +
  theme_void() +
  theme(
    legend.position = c(0.90, 0.2))


Maps_Nigeria$vax_cat <- cut(
  Maps_Nigeria$vc,
  breaks = c(0.00 ,0.05, 0.10, 0.15, 0.25,0.35),
  labels = c("0.00-0.05" ,"0.05-0.10", "0.10–0.15","0.15-0.25","0.25-0.35"
  ),
  include.lowest = TRUE
)

Vc_map <-ggplot(Maps_Nigeria) +
  geom_sf(aes(fill = vax_cat), color = "grey60", linewidth = 0.2) +
  geom_sf_text(aes(label = ADM1_EN), size = 2.5, color = "black") +
  scale_fill_manual( values =  c(
    
    "0.00-0.05"  ="#f7fcf5",
    "0.05-0.10" = "#c7e9c0",
    "0.10–0.15" = "#a1d99b",
    "0.15-0.25" = "#74c476",
    "0.25-0.35" ="#006d2c"
  ),
  name = "Vaccination covererage"
  ) +theme_void()+
  theme(
    legend.position = c(0.9,0.2))
   

combined <- (pop_map | hdr_map) / (Vc_map| PEP_map) +
  theme(legend.position = " top right")+
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold"),
      plot.tag.position = c(0.3,0.80)
    )
  )
combined

ggsave("combined_params_maps.jpeg", combined, width = 12, height =10)

