
#=========================================================================
# Probabilistic decision tree framework model
# Used to model the burden of rabies in Nigeria

#purpose

#Import state-specific demographic and rabies-burden influencing parameters.
# - Run the stochastic rabies decision-tree model for each state.
# - Summarise annual model outcomes over a 10-year horizon.
# - Export state-level-data of rabies deaths,number of rabid dogs,PEP Vaccine use,incidence of rabies deaths and total deaths.

# Main assumptions:
# - 1,000 stochastic simulations are run per state.
# - Each simulation covers a 10-year time horizon.
# - State-specific random seeds are used for reproducibility.
# - The reported annual incidence is based on the median estimated deaths.
# =============================================================================
source("./scripts/decision_tree.R") # Load the external script containing decision_tree()
                                    # containing required packages and model functions

# Read in data set

parameters_df <- read.csv("./data/state_data_with_burden_params_new.csv") %>%
  dplyr::select(state, humans, hdr, pPEP, vc) %>% # Retain only variables 
                                                  #required for the model
                                                                            
  dplyr::rename(# Rename variables to match the argument
                #names used by decision_tree()
    pop = humans,
    HDR = hdr,
    base_vax_cov_owned = vc
  ) 

# Calc dog vax coverage 
parameters_df <- parameters_df %>%
  dplyr::mutate(
    base_vax_cov_owned = round(base_vax_cov_owned,2),
    # The target coverage is currently set equal to baseline coverage.
    target_vax_cov_owned = base_vax_cov_owned,
    
    # Create lower and upper integer values for the human-to-dog ratio.
    # These values define the range passed to the stochastic model.
    HDR1 = floor(HDR),
    HDR2 = ceiling(HDR) 
  ) 



# Run model
load_rabies_models()  ## Load model components once per R session.

outputs_to_keep <- c(# These are the annual time-series outcomes that will be summarised for
                      # each state and each year of the model horizon.
  
  
  "ts_deaths",         # Estimated human rabies deaths
  "ts_exposures",      # Estimated human rabies exposures
  "ts_exp_PEP",        # Exposed individuals receiving PEP
  "ts_vaccine_vials",  # Estimated vaccine vials required
  "ts_rabid_dogs"      # Estimated number of rabid dogs
)


# function to run per state
run_one_state <- function(row, N = 1000, horizon = 10, seed = 123) {
  
  state_name <- row$state # Extract the state name for use in the final output.
  
  # Run the stochastic  probabilistic decision-tree model.
  res <- decision_tree(
    N = N,                            # Number of simulations
    pop = row$pop,                    # Human population
    HDR = c(row$HDR1, row$HDR2),      #Human-to-dog ratio range
    horizon = horizon,                #Number of model years
    mu = 0.38,                        #mean number of exposure per rabid dog
    k = 0.72,                         #dispersion parameter of mu
    pPEP_exposure = row$pPEP,         #Probability of PEP after exposure
    pDeath = 0.17,                    #probability of death in absence of PEP 
    pPrevent = 0.986,                   # probability that PEP prevents death
    rabies_inc = c(0.0075, 0.0125),     #rabies incidence
    base_vax_cov_owned = row$base_vax_cov_owned,
    target_vax_cov_owned = row$target_vax_cov_owned,
    seed = seed
  )
  # Summarise stochastic uncertainty for each selected model outcome.
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

# Display the structure and column types.
head(all_state_summaries)
glimpse(all_state_summaries)
names(all_state_summaries)

 #write data set to output
write.csv(all_state_summaries,
          "output/State_summaries3.csv",
          row.names = FALSE)
#read in data
State_sum <- read.csv("output/State_summaries3.csv")

#filter out deaths
death_ts <- State_sum %>%
filter(output == "ts_deaths")

#write to output
write.csv(death_ts,
          "output/deaths_ts.csv",
          row.names = FALSE)

#filter out and sum deaths for 10 years
death_summary <- State_sum %>%
  filter(output == "ts_deaths") %>%
  group_by(state) %>%
  summarise(
    total_Median = sum(Median, na.rm = TRUE),
    total_LL = sum(LL, na.rm = TRUE),
    total_UL = sum(UL, na.rm = TRUE)
  )

#export data to output
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

