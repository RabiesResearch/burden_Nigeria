#=======================================================================
# Create a faceted time-series plot showing annual estimated  rabies
# deaths for each state over the model horizon.

# Plot elements:
# Gold line: median estimated annual  rabies deaths
#  Blue ribbon: lower and upper uncertainty limits

# - Separate panel: each state
# ==============================================================================

library(dplyr)    #load packages
library(tidyverse)
#Load dataset
deaths_ts<-read.csv("output/deaths_ts.csv") 
ggplot(deaths_ts, aes(x = as.numeric(factor(year)), y = Median, group = 1)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), # Add the uncertainty interval around the median estimate.
                                         # LL represents the lower limit and UL represents the upper limit.
              fill = "blue", alpha = 0.2) + #adds colored line to upper and lower bounds
  geom_line(color = "gold", size = 1) +   #color/shade the median 
  facet_wrap(~ state, scales = "free_y", ncol=5) +scales = "free_y" #allows each state to have its own y-axis range,
  labs( # Add descriptive axis labels
    y = "Annual deaths",
    x = "Year"
  ) +  #adjust plot scale
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() +
  theme( #add clean black and white theme
    # Adjust text sizes and other visual elements.
    axis.text.x = element_text( size =10,vjust = 0.9),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "top")
#save plot
ggsave ("Time series.jpeg",width = 12, height = 10)

