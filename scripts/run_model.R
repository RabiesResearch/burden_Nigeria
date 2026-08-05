

source("./scripts/decision_tree.R")

# Read state parameters
parameters_df <- read.csv("./data/state_data_with_burden_params.csv") %>%
  dplyr::select(state, humans, hdr, pPEP, vc) %>%
  dplyr::rename(
    pop = humans,
    HDR = hdr,
    base_vax_cov_owned = vc
  ) 

parameters_df <- read.csv("./data/state_data_with_burden_params2.csv") %>%
  dplyr::select(state, humans, hdr, pPEP, vc) %>%
  dplyr::rename(
    pop = humans,
    HDR = hdr,
    base_vax_cov_owned = vc
  ) 

# Calc dog vax coverage 
parameters_df <- parameters_df %>%
  dplyr::mutate(
    base_vax_cov_owned = round(base_vax_cov_owned,2),
    target_vax_cov_owned = base_vax_cov_owned,
    HDR1 = floor(HDR),
    HDR2 = ceiling(HDR) 
  ) 



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
run_one_state <- function(row, N = 1000, horizon = 10, seed = 123) {
  
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
  ~ run_one_state(parameters_df[.x, ], N = 1000, horizon = 10, seed = 123 + .x)
)


head(all_state_summaries)
glimpse(all_state_summaries)
names(all_state_summaries)

write.csv(all_state_summaries,
          "output/State_summaries2.csv",
          row.names = FALSE)

State_sum <- read.csv("output/State_summaries.csv")



death_summary <- State_sum %>%
  filter(output == "ts_deaths") %>%
  group_by(state) %>%
  summarise(
    total_Median = sum(Median, na.rm = TRUE),
    total_LL = sum(LL, na.rm = TRUE),
    total_UL = sum(UL, na.rm = TRUE)
  )


write.csv(death_summary,
          "output/Total_deaths.csv",
          row.names = FALSE)


# Step 1: Filter only the variables you want for Maps
filtered_data <- State_sum %>%
  filter(output %in% c("ts_deaths", "ts_exp_PEP", "ts_rabid_dogs"))

# Step 2: Calculate mean (average) across 10 years
mean_results <- filtered_data %>%
  group_by(state, output) %>%
  summarise(
    mean_value = mean(Median, na.rm = TRUE),
    mean_LL = mean(LL, na.rm = TRUE),
    mean_UL = mean(UL, na.rm = TRUE),
    .groups = "drop"
  )
# to turn output into clean columns

#  Rename columns

mean_results_clean <- mean_results %>%
  rename(
    mean = mean_value,
    LL = mean_LL,
    UL = mean_UL
  )

# Pivot using the cleaned dataset
Clean_mean_tables <- mean_results_clean %>%
  pivot_wider(
    names_from = output,
    values_from = c(mean, LL, UL),
    
    names_glue = "{output}_{.value}"
  )

#caculate the incidence per states
deaths_ts<-read.csv("output/deaths_ts.csv")

Rabies_deaths <- deaths_ts %>%
  rename(Total_deaths = Median)

population <-parameters_df%>%
  rename(population = pop)

# Merge
merged <- deaths_ts %>%
  left_join(parameters_df, by = "state")

# Step 1: calculate yearly incidence (deaths per 100,000 per year)
merged_incidence<- merged %>%
  mutate(incidence_100000 = (Median / pop) * 100000)
write.csv(merged_incidence,
          "output/Incidence per year.csv",
          row.names = FALSE)
# Step 2: average across 10 years

mean_incidence <- merged_incidence %>%   
  group_by(state) %>%
  summarise(mean_annual_incidence = mean(incidence_100000, na.rm = TRUE))
#copy model results to output
write.csv(mean_incidence,
          "output/Annual_death_Incidence_Per_100,000_year.csv",
          row.names = FALSE)

