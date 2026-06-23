

source("./scripts/decision_tree.R")

# Read state parameters
parameters_df <- read.csv("./data/state_data_with_burden_params.csv") %>%
  dplyr::select(state, humans, hdr, pPEP, vax_dogs) %>%
  dplyr::rename(
    pop = humans,
    HDR = hdr
  ) %>%
  drop_na()


# Calc dog vax coverage 
parameters_df <- parameters_df %>%
  dplyr::mutate(
    base_vax_cov_owned = round(vax_dogs/ (pop/HDR),2),
    target_vax_cov_owned = base_vax_cov_owned,
    HDR1 = floor(HDR),
    HDR2 = ceiling(HDR) 
  ) %>% 
  dplyr::select(-vax_dogs)



# Run model
load_rabies_models()  # call once per session

outputs_to_keep <- c(
  "ts_deaths",
  "ts_exposures",
  "ts_exp_PEP",
  "ts_vaccine_vials",
  "ts_rabid_dogs"
)


# function to run per state
run_one_state <- function(row, N = 10, horizon = 5, seed = 123) {
  
  state_name <- row$state
  
  res <- decision_tree(
    N = N,
    pop = row$pop,
    HDR = c(row$HDR1, row$HDR2),
    horizon = horizon,
    mu = 0.38,
    k = 0.72,
    pPEP_exposure = row$pPEP,
    pDeath = 0.17,
    pPrevent = 0.986,
    rabies_inc = c(0.0075, 0.0125),
    base_vax_cov_owned = row$base_vax_cov_owned,
    target_vax_cov_owned = row$target_vax_cov_owned,
    seed = seed
  )
  
  summaries <- map_dfr(outputs_to_keep, function(output_name) {
    summarise_stochasticity(res[[output_name]]) %>%
      mutate(
        state = state_name,
        output = output_name,
        .before = 1
      )
  })
  
  rm(res)
  gc()
  
  summaries
}


# run across states
all_state_summaries <- map_dfr(
  seq_len(nrow(parameters_df)),
  ~ run_one_state(parameters_df[.x, ], N = 10, horizon = 5, seed = 123 + .x)
)


head(all_state_summaries)


