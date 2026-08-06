#   Key relationships to consider when estimating the burden of rabies in Nigeria
#  Estimates of PEP seeking - pPEP likely increases with wealth 
# i.e. a low poverty index means the state is wealthier!

poverty = seq(0,1,0.05)
b0 = -1  # intercept - baseline level (as value decreases into negative numbers higher probability remains near 1)
b1 = 3 # slope - baseline steepness - as increases, drops faster
U = 1 # ceiling parameter - the value that probability approaches as poverty approaches 0 
L = 0.4 # Floor parameter that results in horizontal asymptote (as poverty approaches 1)

# Apply inverse logit manually
pPEP_prob_seq <- L +  (U - L) / (1 + exp(b0 + (b1 * poverty)))

# Plot reasonable set of params:
plot(poverty, pPEP_prob_seq,  type = "l", ylim = c(0, 1),  xlab = "Poverty", ylab = "probability of PEP")

# WORST CASE! 
b0 = -1  # intercept - baseline level (as value decreases into negative numbers higher probability remains near 1)
b1 = 6 # slope - baseline steepness - as increases, drops faster
U = 1 # ceiling parameter - the value that probability approaches as poverty approaches 0 
L = 0.4 # Floor parameter that results in horizontal asymptote (as poverty approaches 1)
pPEP_prob_seq <- L +  (U - L) / (1 + exp(b0 + (b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "red")

# Best CASE! 
b0 = -1  # intercept - baseline level (as value decreases into negative numbers higher probability remains near 1)
b1 = 2 # slope - baseline steepness - as increases, drops faster
U = 1 # ceiling parameter - the value that probability approaches as poverty approaches 0 
L = 0.4 # Floor parameter that results in horizontal asymptote (as poverty approaches 1)
pPEP_prob_seq <- L +  (U - L) / (1 + exp(b0 + (b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "blue")

# even worse
b0 = -1  # intercept - baseline level (as value decreases into negative numbers higher probability remains near 1)
b1 = 8 # slope - baseline steepness - as increases, drops faster
U = 1 # ceiling parameter - the value that probability approaches as poverty approaches 0 
L = 0.4 # Floor parameter that results in horizontal asymptote (as poverty approaches 1)
pPEP_prob_seq <- L +  (U - L) / (1 + exp(b0 + (b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "green")

###############################################################################
# Dog population - HDR increases with urban % and muslim % (more dogs per human in rural areas and non-muslim areas)
humans = seq(2000000,17000000, 1000000)
urban = seq(0.2,0.95,0.05)
muslim = seq(0, 1, length = 16)
hdr = seq(5,20,1)

# increases with % urban (more humans per dog in cities)
cu = 3; bu = 25 # Minimum  and maximum HDR
hdr = cu + urban * bu # Linear relationship whereby dogs decrease within urban settings
plot(urban, hdr,  type = "l",  xlab = "Urban", ylab = "Probability", xlim = c(0,1), ylim = c(0,30))

# decreases with religion (increase % muslim)
cm = 3; bm = 25
hdr = cm + muslim * bm
plot(muslim, hdr,  type = "l",  xlab = "Muslim", ylab = "Probability", xlim = c(0,1))

# Combined relationship - multiplicative
c = 3 # intercept
bm = 25 # muslim max
bu = 25 # urban max
bcomb = -15 # multiplicative interaction
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)
# shows the range assuming urban and muslim combinations scale together!
plot(muslim, hdr, ylim= c(0,50), 
     type = "l",  xlab = "Predictor", ylab = "Predicted probability", 
     xlim = c(0,1))
# shows the range assuming urban and muslim combinations scale opppositely
hdr = c + (bm * muslim) + (bu * rev(urban)) + bcomb*(urban * muslim)
lines(muslim, hdr, lty=2)

# alternative ranges
bcomb = -5 # more extreme max HDR
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)
lines(muslim, hdr,  col = "green")

bcomb = -25 # less extreme HDR
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)
lines(muslim, hdr,  col = "green")

###############################################################################
# dog vaccination coverage - expect coverage to decline as poverty increases
# Inverse logistic (flipped sigmoid)
poverty = seq(0,1,0.05)

# Average parameter range
vc_decline = 4 # determines how steep the decline is
vc_slope_shift = 0.05 # determines where along x-axis decline starts
vc_int = 0.05 # minimum coverage (asymptote as poverty increases)

prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))

plot(poverty, prob, ylim= c(0,1), 
     type = "l",  xlab = "Poverty", ylab = "Vaccination coverage", 
     xlim = c(0,1))

# Better case scenario
vc_decline = 6 # slower decline
vc_slope_shift = .25 # 
vc_int = 0.1 # slight increase
prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))
lines(poverty, prob,  col = "green")

# Worse case scenario
vc_decline = 6 # 
vc_slope_shift = .001 # 
vc_int = 0.05 # 
prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))
lines(poverty, prob,  col = "red")

# Worst case scenario
# vc_decline = 10 # 
# vc_slope_shift = .001 # 
# vc_int = 0.05 # 
# prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))
# lines(poverty, prob,  col = "red")

# different case scenario
vc_int <- 0.01 # your existing floor value — use whatever it actually is
vc_decline <- 6
target0 <- 0.4 # target coverage at poverty = 0
L <- 1 - (target0 - vc_int) / (1 - vc_int) # solve for the vc_slope_shift that gives prob(poverty=0) = target0
vc_slope_shift <- log((1 - L) / L) / vc_decline
prob <- vc_int + (1 - vc_int) * (1 - 1 / (1 + exp(-vc_decline * (poverty - vc_slope_shift))))
lines(poverty, prob, col = "blue")







