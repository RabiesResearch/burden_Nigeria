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

# Plot
p <- ggplot(
  Total_deaths,
  aes(
    x = reorder(state, total_Median),
    y = total_Median,
    fill = Zone
  )
) +
geom_col() +
geom_errorbar(
  aes(ymin = total_LL, ymax = total_UL),
  width = 0.3,
  colour = "black"
) +
coord_flip() +
scale_fill_manual(values = zone_cols) +
labs(
  x = "State",
  y = "Total deaths",
  fill = "Geopolitical Zone"
) +
theme_minimal() +
theme(
  plot.margin = margin(10, 30, 10, 10),
  axis.text.y = element_text(size = 16),
  axis.text.x = element_text(size = 10),
  axis.title.x = element_text(size = 18),
  axis.title.y = element_text(size = 18)
)

# Display plot
print(p)

# Save plot
ggsave("Total_deaths_map.jpeg",
  plot = p,
  width = 12,
  height = 10,
  dpi = 300
)

