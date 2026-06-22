# SET UP BURDEN MODEL 
library(tidyverse)
params <- read_csv("data/burden_rel_param_values.csv")
states <- read_csv("data/state_data.csv")
head(states)
head(params)

# use index to select average values for now:
i = which(params$level == "avg")

# create probability of receiving PEP by state as a function of poverty index
states$pPEP = params$pPEP_L[i] +  
  (params$pPEP_U[i] - params$pPEP_L[i]) / 
  (1 + exp(params$pPEP_int[i] + (params$pPEP_slope[i] * states$poverty)))

# create probability of dog vaccination by state as a function of poverty index
states$vc = params$vc_int[i] + (1-params$vc_int[i]) *
  (1-1/(1 + exp(-params$vc_decline[i]*(states$poverty - params$vc_slope_shift[i]))))

# create HDR by state as a function of muslim population (lets just use current data available)
states$hdr = params$hdr_min[i] + (params$muslim_max[i] * states$muslim) 

# create dog pop by state as a function of estimated hdr
states$dogs = states$humans / states$hdr

write.csv(states, "data/state_data_with_burden_params.csv", row.names = F)

# TO DO
# incorporate urban/ peri-urban/ rural data from Grace
# include a parameter to define if states have dog meat trade or not - expect dog meat to reduce HDR 

# Validate params
mean(states$pPEP, na.rm = T)  # 0.665  - seems ok! 
states$pPEP[which(states$state == "Kaduna")]  # looks a bit high if Zaria is representative of Kaduna!
mean(states$vc, na.rm = T)  # 0.21 - seems ok
sum(states$dogs) # 28 million!
sum(states$humans) # >223 million!
sum(states$humans)/sum(states$dogs) # 7.9 humans per dog on avg - too high?

# sense check
susdogs = (1-mean(states$vc, na.rm = T))  * sum(states$dogs) # 23 million!
incidence = 0.01
avg_bites = .38
exposures = susdogs * incidence * avg_bites # 84k exposures
no_pep <- exposures * (1-mean(states$pPEP, na.rm = TRUE))
pdeath = 0.16
pdeath * no_pep # 4500 deaths - seems ok!

