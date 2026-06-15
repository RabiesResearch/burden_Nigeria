# Model planning

# 1. Make a function to take the parameters from the state_data_burden_parameters csv file
# and feed them into the model to generate the outputs of interest (deaths, exposures, etc) for each state.

# 2. Try to make sure this works by running one or two states with your function

# 3. When the model runs, you will get useful outputs that you need to store and present 
# in a way that is useful for the next steps. 
# For example, save the outputs in a csv file with the state name, and the average annual (or 10 year total) deaths & 95% CIs
# Plot the timeseries of deaths with the 95% confidence intervals over the 10 year period for each state.

# 4. Now you have done this for one state, you can run the model for all states and save the outputs in a single file.

# While trying out your code to see that it works, only do a small number of runs! e.g 10 to 100.
# When you are sure your code will produce nice outputs, then you can run the model for a larger 
# number of runs (e.g. 1000) to get more robust estimates of the outputs of interest.

# make sure the outputs that you save are formatted in a way that allows you to easily read and 
# compare the values across different states!
# Ideally make a plot that shows you the timeseries in each state
# OR EVEN BETTER - make a map with the states coloured by the total deaths

