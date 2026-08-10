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
#filter out average of all state parameters

All_result_params<- read.csv("output/State_summaries3.csv")

All_map_results_params <- All_result_params %>%
filter(output %in% c("ts_deaths","ts_vaccine_vials", "ts_rabid_dogs")) %>%
group_by(state, output) %>%
summarise(
  Mean = mean(Median, na.rm = TRUE),
  LL = mean(LL, na.rm = TRUE),
  UL = mean(UL, na.rm = TRUE),
  .groups = "drop"
)
All_map_results_wide <- All_map_results_params %>%
pivot_wider(
  names_from = output,
  values_from = c(Mean, LL, UL),
  names_glue = "{output}_{.value}"
)

# Remove "_Mean" from the main estimate columns
names(All_map_results_wide) <- gsub("_Mean", "", names(All_map_results_wide))

All_map_results_wide <- All_map_results_wide[, c( "state","ts_deaths","ts_deaths_UL",
  "ts_deaths_LL",
  "ts_rabid_dogs",
  "ts_rabid_dogs_UL",
  "ts_rabid_dogs_LL",
  "ts_vaccine_vials",
  "ts_vaccine_vials_UL",
  "ts_vaccine_vials_LL"
)]
write.csv(All_map_results_wide,
          "output/All_map_results_wide.csv",
          row.names = FALSE)

#create a barplot with upper and lower limit of the total rabies deaths
Total_deaths <- read.csv("output/Total_deaths.csv")
Total_deaths <- Total_deaths %>%
  arrange(total_Median)
ggplot(Total_deaths, aes(x = reorder(state, total_Median), y = total_Median)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = total_LL, ymax = total_UL),
    width = 0.3,
    colour = "black"
  ) +
  coord_flip() +
  labs(
    x = "State",
    y = "Total deaths"
  ) +
  theme_minimal()+
  theme(
    plot.margin = margin(10, 30, 10, 10)
  )
ggsave("Total_deaths.jpeg", width = 12, height =10,dpi=300)

