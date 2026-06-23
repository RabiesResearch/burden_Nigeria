deaths_ts<-read.csv("output/deaths_ts.csv")
ggplot(deaths_ts, aes(x = as.numeric(factor(year)), y = Median, group = 1)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), 
              fill = "blue", alpha = 0.2) +
  geom_line(color = "gold", size = 1) +
  facet_wrap(~ state, scales = "free_y", ncol=5) +
  labs(
    y = "Median yearly deaths",
    x = "Year index (1–10)"
  ) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 40, vjust = 0.5),
    legend.position = "top")
