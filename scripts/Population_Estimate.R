#The population growth rate was extracted from the website ourworldindata.org
#https://ourworldindata.org/profile/population-demography/nigeria

#State Population, 2006 - Nigeria Data Portal   
#The population of Nigeria by states according to the 2006 population census
#sourced from The National bureau of statistics database.
#https://nigeria.opendataforafrica.org/ifpbxbd/state-population-2006

rm(list = ls())#clearing's brain
pop_2006 <- read.csv("Nigerian_2006_population_census.csv")
pop_growth<- c(2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.7,2.6,
               2.5,2.5,2.4,2.2,2.2,2.1,2.1,2.1,2.1,2.1,
               rep(2.1,15)
              )
rep(2.1,15)
# Create a df of population growth
growth_df <- data.frame(
  year = 2006:2039,
  growth_rate = pop_growth
)

growth_df

# create copies
pop_estimates <- pop_2006

# start with 2006 population
current_pop <- pop_2006$X2006

# loop through years and compound growth
for(i in 1:nrow(growth_df)) {
  
  yr <- growth_df$year[i]
  rate <- growth_df$growth_rate[i] / 100
  
  # next year's population
  next_pop <- current_pop * (1 + rate)
  
  # save as new column
  pop_estimates[[paste0("X", yr + 1)]] <- round(next_pop)
  
  # update for next iteration
  current_pop <- next_pop
}

# view result
head(pop_estimates)


write.csv(pop_estimates,
          "Nigerian_2006_2040_population_estimation.csv",
          row.names = FALSE)


pop_2006_2040 <- read.csv("Nigerian_2006_2040_population_estimation.csv")
head(pop_2006_2040)

