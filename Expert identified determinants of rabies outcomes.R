library(tidyverse)
# Enter the data
determinants <- tribble(
  ~Factor, ~Dog_population, ~Vaccination, ~PEP,
  "Poverty", 3, 9, 8,
  "Religious practices", 6, 2, 2,
  "Dog meat trade", 8, 5, 5,
  "Livestock herding", 6, 6, 6,
  "Hunting", 8, 6, 3,
  "Urbanization", 9, 8, 7,
  "Public awareness", 6, 9, 8,
  "Access to health services", 2, 8, 8,
  "Cultural practices", 8, 8, 7
)
# Change from wide to long format
determinants_long <- determinants |>
pivot_longer(
  cols = -Factor,
  names_to = "Outcome",
  values_to = "Number"
) |>
mutate(
  Outcome = recode(
    Outcome,
    Dog_population = "Dog population size",
    Vaccination = "Dog vaccination coverage",
    PEP = "Probability of receiving PEP"
  ),
  # Order factors by the total number of selections
  Factor = fct_reorder(
    Factor,
    Number,
    .fun = sum
  )
)

ggplot(
  determinants_long,
  aes(
    x = Number,
    y = Factor,
    fill = Outcome
  )
) +
geom_col(
  position = position_dodge(width = 0.8),
  width = 0.7
) +

scale_x_continuous(
  breaks = 0:10,
  limits = c(0, 10.5),
  expand = expansion(mult = c(0, 0.02))
) +
scale_fill_manual(
  values = c(
    "Dog population size" = "#1B9E77",
    "Dog vaccination coverage" = "#D95F02",
    "Probability of receiving PEP" = "#7570B3"
  )
) +
labs(
  title = "Expert-identified determinants of rabies-related outcomes",
  subtitle = "Bars show the number of experts who selected each outcome",
  x = "Number of experts",
  y = NULL,
  fill = NULL
) +
theme_classic(base_size = 12) +
theme(
  plot.title = element_text(
    face = "bold",
    size = 14
  ),
  plot.subtitle = element_text(size = 11),
  legend.position = "bottom",
  legend.text = element_text(size = 9),
  axis.text.y = element_text(size = 10)
)

