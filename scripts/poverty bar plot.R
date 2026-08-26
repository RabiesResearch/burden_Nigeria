library(dplyr)
library(ggplot2)

# Read data
State_data <- read.csv("data/state_data_with_burden_params_new.csv")

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
State_data <- left_join(State_data, zones, by = "state")

# Poverty plot
State_data <- State_data %>%
mutate(poverty_pct = poverty * 100) %>%
arrange(poverty_pct)
# Zone colours

zone_cols <- c(
  "North Central" = "gold",
  "North East" = "skyblue",
  "North West" = "orange",
  "South East" = "lemonchiffon",
  "South South" = "wheat3",
  "South West" = "plum1"
)
ggplot(
  State_data,
  aes(
    x = reorder(state, poverty),
    y = poverty,
    fill = Zone
  )
) +
geom_col() +
coord_flip() +
scale_fill_manual(values = zone_cols) +
theme_minimal() +
theme(legend.position = "none") +
labs(
  x = "State",
  y = "Poverty Index"
) +
theme_minimal() +
theme(
  legend.position = "none",
  axis.text.y = element_text(size = 16,face= "bold"),
  axis.text.x = element_text(size = 14),
  axis.title.x = element_text(size = 20,face= "bold"),
  axis.title.y = element_text(size = 20,face="bold")
)
ggsave("Poverty_Index.jpeg",width = 12,height = 10,dpi = 300)

