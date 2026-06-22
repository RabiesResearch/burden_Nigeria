# Model planning
#___________________________________________________________________________________
# early Jun 2026
# 1. Make a function to take the parameters from the state_data_burden_parameters csv file
# and feed them into the model to generate the outputs of interest (deaths, exposures, etc) for each state.
# 2. Make sure this works by running one or two states with your function
# 3. When the model runs, store the outputs so you can present them in a way that is useful for the next steps. 
# For example, save the outputs in a csv file with the state name, and the average annual (or 10 year total) deaths & 95% CIs
# Plot the timeseries of deaths with the 95% confidence intervals over the 10 year period for each state.
# 4. Now you have done this for one state, run the model for all states and save the outputs in a single file.
# While trying out your code to see that it works, only do a small number of runs! e.g 10 to 100.
# When you are sure your code will produce nice outputs, then you can run the model for  more runs (e.g. 1000) to get robust estimates of outputs of interest.
# make sure the saved outputs are formatted in a way that allows you to easily read and compare the values across different states!
# Ideally make a plot that shows you the timeseries in each state OR EVEN BETTER - make a map with the states coloured by the total deaths!

#——————————————————————————————————————————————————————————————————————
# late Jun 2026
# 1. Rerun the burden set up file with the revisions to the burden_relationship that I have just pushed to Github

# 2. come up with a variable for the dog meat trade by state (push to Github when done so we can include!)

# 3. Plot a set of maps to compare different things together: 
# - i) poverty index, ii) population, ii) religion, iv) dogs, v) vaccination coverage, vi) Probability of PEP, vii) dog meat rade (tendency or yes/no)
# Push your code and maps to Github so I can take a look too :)

# 4. Now set up the model to save summary files for several key outcomes
# for comparison we are interested in: deaths, vials of post-exposure vaccine used (this may be written as PEP? check with Martha) and rabid dogs

# 5.After running the model with new parameters save a set of maps to be able to compare differnt things together:
# - deaths (avg deaths per year), annual death incidence (deaths per 100,000 per year) PEP use (avg PEP use per year), rabid dogs (avg per year)
# Push your code and maps to Github so I can take a look too :)

# 6. Now compare your maps from step 3 and step 5 with the new parameters - do they look right?
# play with the parameter values to see if they look better or worse under different assumptions 
# (save the maps and code for these different assumptions to allow you to easily compare visually)
# Push your code and maps to Github so I can take a look too :)



