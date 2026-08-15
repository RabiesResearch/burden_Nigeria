library(dplyr)
library(ggplot2)

# Read data
Total_deaths <- read.csv("output/Total_deaths.csv")

# Geopolitical zones lookup table
zones <- data.frame(
  state = c(
    "Benue","Kogi","Kwara","Nasarawa","Niger","Plateau","Federal Capital Territory",
    "Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe",
    "Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara",
    "Abia","Anambra","Ebonyi","Enugu","Imo",
    "Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers",
    "Ekiti","Lagos","Ogun","Ondo","Osun","Oyo"
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

# Merge zones into Total_deaths
Total_deaths <- left_join(Total_deaths, zones, by = "state")

# Order by median deaths
Total_deaths <- Total_deaths %>%
  arrange(total_Median)

# Zone colours

zone_cols <- c(
  "North Central" = "gold",
  "North East" = "skyblue",
  "North West" = "orange",
  "South East" = "lemonchiffon",
  "South South" = "wheat3",
  "South West" = "plum1"
)

