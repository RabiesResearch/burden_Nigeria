library(tidyverse)
params <- read_csv("data/burden_rel_param_values.csv")
states <- read_csv("data/state_data.csv")

# Select parameter values
i <- which(params$level == "lowest")
# Calculate state-specific burden parameters
states <- states %>%
mutate(
  pPEP = params$pPEP_L[i] +
  (params$pPEP_U[i] - params$pPEP_L[i]) /
  (1 + exp(
    params$pPEP_int[i] +
    params$pPEP_slope[i] * poverty
  )),
  vc = params$vc_int[i] +
  (1 - params$vc_int[i]) *
  ( 1 -1 / (1 + exp(-params$vc_decline[i] *
        (poverty - params$vc_slope_shift[i])
      )
    )

  ),
  hdr = params$hdr_min[i] +
  params$muslim_max[i] * muslim)
# Calculate averages of the burden-result columns
average_values <- states %>%
summarise(
  across(
    c(pPEP, vc, hdr,poverty,muslim),
    ~ mean(.x, na.rm = TRUE)
  )
)
# Assign the averages to every state while retaining state populations
states_average <- states %>%
mutate(pPEP = average_values$pPEP, vc = average_values$vc,hdr = average_values$hdr,
       poverty = average_values$poverty,muslim = average_values$muslim,
       

  # Each state keeps its own human population
  dogs = humans / hdr
)
# Inspect results
head(states_average)

# Save state-level results
write_csv(
  states_average,
  "data/state_datas_with_average_burden_params.csv"
)
