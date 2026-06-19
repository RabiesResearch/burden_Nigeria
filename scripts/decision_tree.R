

require(pacman)
pacman::p_load(tidyverse,   # cleaning, wrangling
               brms,      # models
               rlang,
               lubridate, # dates
               matrixStats,
               scales,
               readxl,
               ggrepel,
               cowplot,
               patchwork,
               purrr,
               furrr
)



# Decision tree model that can be applied to create different scenarios 

# source helper functions  
source("./scripts/HelperFun.R")


decision_tree <- function(N = 10, pop = 35e6, HDR = c(16,17), unowned_prop = 0, horizon = 5, 
                          mu = 0.38, k = 0.72, pPEP_exposure = 0.6, bpi = 15.3,
                          pDeath = 0.17, pPrevent = 0.986, 
                          rabies_inc = c(0.0075, 0.0125), mdv_unowned_budget = NULL, 
                          mdv_owned_budget = NULL,vaccinate_owned_dog_cost = c(0, 0), 
                          vaccinate_unowned_dog_cost = c(0, 0), base_vax_cov_owned = 0.2,   
                          target_vax_cov_owned = 0.2, base_vax_cov_unowned = 0, target_vax_cov_unowned = 0,
                          years_to_target = 3, seed = 123, 
                          dog_burnin = 1) {
  
  total_horizon <- horizon + dog_burnin
  
  # ---------------------------------------------------------------------------#
  # 1: Dog population & vaccination   #######
  # ---------------------------------------------------------------------------#
  dog_pop      <- estimate_dog_population(N, pop, HDR, total_horizon)
  unowned_dogs <- matrix(rbinom(N * total_horizon, as.integer(dog_pop), unowned_prop),
                         nrow = N, ncol = total_horizon)
  owned_dogs   <- dog_pop - unowned_dogs
  owned_prop   <- 1 - unowned_prop
  
  vax_cov_owned <- calc_vax_coverage(
    base_vax_cov = base_vax_cov_owned, target_vax_cov = target_vax_cov_owned,
    mdv_campaign_budget = mdv_owned_budget, vaccinate_dog_cost = vaccinate_owned_dog_cost,
    dog_pop = owned_dogs, horizon = total_horizon, discount = discount,
    years_to_target = years_to_target, dog_burnin = dog_burnin)
  
  vax_cov_unowned <- calc_vax_coverage(
    base_vax_cov = base_vax_cov_unowned, target_vax_cov = target_vax_cov_unowned,
    mdv_campaign_budget = mdv_unowned_budget, vaccinate_dog_cost = vaccinate_unowned_dog_cost,
    dog_pop = unowned_dogs, horizon = total_horizon, discount = discount,
    years_to_target = years_to_target, dog_burnin = dog_burnin)
  
  dog_vax_cov <- (vax_cov_unowned * unowned_prop) + (vax_cov_owned * owned_prop)
  
  vax_results        <- calculate_vaccinated_and_susceptible(N, total_horizon, dog_pop, dog_vax_cov)
  vaccinated_unowned <- calculate_vaccinated_and_susceptible(N, total_horizon, unowned_dogs, vax_cov_unowned)$ts_dogs_vaccinated
  vaccinated_owned   <- calculate_vaccinated_and_susceptible(N, total_horizon, owned_dogs,   vax_cov_owned)$ts_dogs_vaccinated
  
  MDV_campaign_cost <-
    calculate_campaign_cost(N, total_horizon, mdv_unowned_budget, vaccinated_unowned, vaccinate_unowned_dog_cost) +
    calculate_campaign_cost(N, total_horizon, mdv_owned_budget,   vaccinated_owned,   vaccinate_owned_dog_cost)
  
  # ---------------------------------------------------------------------------#
  # 2: Dogs and human bites: exposures & healthy
  ## 2A: Dog rabies & human exposures — strip burn-in immediately   #######
  # ---------------------------------------------------------------------------#
  rabies_results_full <- predict_dograbies_split(
    N = N, horizon = total_horizon, vax_cov = dog_vax_cov, dog_pop = dog_pop,
    rabies_inc = rabies_inc, mu = mu, k = k, seed = seed,
    pop_serengeti = 1e5, split_by = "mean")
  
  keep_cols   <- (dog_burnin + 1):total_horizon
  strip_burnin <- function(mat) mat[, keep_cols, drop = FALSE]
  
  # ### OPT: bites_by_dog flat-list stripping (column-major: year = ceiling(idx/N))
  bites_keep_idx <- which(ceiling(seq_along(rabies_results_full$bites_by_dog) / N) > dog_burnin)
  
  rabies_results <- list(
    ts_rabid_dogs        = strip_burnin(rabies_results_full$ts_rabid_dogs),
    ts_exposures         = strip_burnin(rabies_results_full$ts_exposures),
    ts_rabid_biting_dogs = strip_burnin(rabies_results_full$ts_rabid_biting_dogs),
    bites_by_dog         = rabies_results_full$bites_by_dog[bites_keep_idx])
  
  vax_results$ts_dogs_vaccinated <- strip_burnin(vax_results$ts_dogs_vaccinated)
  vax_results$sus_dogs           <- strip_burnin(vax_results$sus_dogs)
  MDV_campaign_cost              <- strip_burnin(MDV_campaign_cost)
  
  # ---------------------------------------------------------------------------#
  ##  2B: Healthy bites (human horizon only)   #######
  # ---------------------------------------------------------------------------#
  ts_total_bite_presentations <- matrix(rbinom(N * horizon, pop, bpi / 1000), N, horizon)
  
  # ---------------------------------------------------------------------------#
  #  3: Hospital ######
  ##  3A: Healthcare seeking #######
  ##  3B: Biologicals start & compliance ######
  # Merged 3A & 3B
  # ---------------------------------------------------------------------------#
  #ts_exp  <- rabies_results$ts_exposures

  ts_exp_PEP   <- matrix(rbinom(N * horizon, as.vector(rabies_results$ts_exposures),   pPEP_exposure), N, horizon)
  
  ts_exp_noPEP <- rabies_results$ts_exposures - ts_exp_PEP
  
 
  # ---------------------------------------------------------------------------#
  # 4: IBCM  ######
  # ---------------------------------------------------------------------------#
  # Skipping this for now
  
  # ---------------------------------------------------------------------------#
  # 5: Outcomes  ########
  ## 5A: deaths ########
  # ---------------------------------------------------------------------------#

  ts_deaths <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_noPEP), pDeath), N, horizon)
  

  ## 5B: Deaths averted  ########
  ### PEP
  # ts_deaths_averted_PEP   <- matrix(
  #   rbinom(N * horizon, as.vector(ts_exp_PEP),   pPrevent   * pDeath), N, horizon)
  

  # ---------------------------------------------------------------------------#
  # MDV counterfactual — ### OPT: reuse dog_pop; only recompute what changes
  # ---------------------------------------------------------------------------#
  
  vax_cov_no_MDV <-
    calc_vax_coverage(0.05, 0.05, NULL, vaccinate_owned_dog_cost, owned_dogs,
                      total_horizon, discount, years_to_target, dog_burnin) * owned_prop +
    calc_vax_coverage(0,    0,    NULL, vaccinate_unowned_dog_cost, unowned_dogs,
                      total_horizon, discount, years_to_target, dog_burnin) * unowned_prop
  
  exposures_no_MDV <- strip_burnin(
    predict_dograbies_split(
      N = N, horizon = total_horizon, vax_cov = vax_cov_no_MDV, dog_pop = dog_pop,
      rabies_inc = rabies_inc, mu = mu, k = k, seed = seed,
      pop_serengeti = 1e5, split_by = "mean")$ts_exposures)
  
  #ts_deaths_averted_MDV  <- (exposures_no_MDV - rabies_results$ts_exposures) * pDeath # deterministic to reduce MCMC noise (may lead to NAs)
  # ts_deaths_averted_MDV2 <- matrix(
  #   rbinom(N * horizon, pmax(0L, as.vector(exposures_no_MDV - rabies_results$ts_exposures)), pDeath),
  #   N, horizon)
  
  expected_deaths_no_intervention <- matrix(
    rbinom(N * horizon, as.vector(exposures_no_MDV), pDeath), N, horizon)
  
  ts_deaths_averted  <- expected_deaths_no_intervention - ts_deaths
  

  # ---------------------------------------------------------------------------#
  #  6: Economics  ########
  # Skipping for now ?
  # ---------------------------------------------------------------------------#
  
  
  # ---------------------------------------------------------------------------#
  # Collate & return  #######
  # ---------------------------------------------------------------------------#
  
  # All `ts_` objects from the local environment
  my_list      <- ls(pattern = "^ts_")
  out_matrices <- mget(my_list)
  
  # ts_ elements from rabies_results (already stripped)
  rabies_ts <- rabies_results[grep("^ts_", names(rabies_results))]
  dogs_ts   <- vax_results[grep("^ts_", names(vax_results))]
  
  out_matrices <- c(out_matrices, rabies_ts, dogs_ts)
  
  return(out_matrices)
}







# ── Single-session example ───────────────────────────────────────────────────

load_rabies_models()   # <-- call once; cached for the whole session

tmp <- decision_tree(
              N = 10, pop = 35e6, HDR = c(16,17), horizon = 5, 
              mu = 0.38, k = 0.72, pPEP_exposure = 0.6, 
              pDeath = 0.17, pPrevent = 0.986, rabies_inc = c(0.0075, 0.0125), base_vax_cov_owned = 0.2,   
              target_vax_cov_owned = 0.2, seed = 123
            )

names(tmp)
tmp$ts_exposures
tmp$ts_rabid_dogs
tmp$ts_deaths
summarise_stochasticity(tmp$ts_deaths)
