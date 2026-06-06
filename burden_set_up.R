# SET UP BURDEN MODEL 
library(tidyverse)

# Import burden parameters and state level data
params <- read_csv("data/burden_rel_param_values.csv")
states <- read_csv("data/state_data.csv")

head(states)
head(params)

# use index to select average values for now:
i = which(params$level == "avg")

# create probability of receiving PEP by state as a function of poverty index
states$pPEP = 1 / (1 + exp(-(params$pPEP_int[i] + params$pPEP_slope[i] * states$poverty)))

# create probability of dog vaccination by state as a function of poverty index
states$vc = params$vc_int[i] + (1-params$vc_int[i]) *
  (1-1/(1 + exp(-params$vc_decline[i]*(states$poverty - params$vc_slope_shift[i]))))

# create HDR by state as a function of muslim population (lets just use current data available)
states$hdr = params$hdr_int[i] + (params$muslim_slope[i] * states$muslim) 

# create dog pop by state as a function of estimated hdr
states$dogs = states$humans / states$hdr

write.csv(states, "data/state_data_with_burden_params.csv", row.names = F)

# TO DO
# incorporate urban/ peri-urban/ rural data from Grace
# include a parameter to define if states have dog meat trade or not - expect dog meat to reduce HDR 

# Validate params
mean(states$pPEP, na.rm = T)  # 0.83  - seems too high! 
states$pPEP[which(states$state == "Kaduna")]  # looks v high if Zaria is representative of Kaduna!
mean(states$vc, na.rm = T)  # 0.17 - seems ok
sum(states$dogs) # 28 million!
sum(states$humans) # >223 million!
sum(states$humans)/sum(states$dogs) # 7.9 humans per dog on avg - too high?

# sense check
susdogs = (1-mean(states$vc, na.rm = T))  * sum(states$dogs) # 23 million!
incidence = 0.01
avg_bites = .38
exposures = susdogs * incidence * avg_bites # 89k exposures
no_pep <- exposures * (1-mean(states$pPEP, na.rm = TRUE))
pdeath = 0.16
pdeath * no_pep # 2,300 deaths - seems ok


no_pep <- exposures * (1-0.6)
pdeath * no_pep # increases to 6000 at lower pSeek  - seems like once MDV about right should be fine