#   Key relationships to consider when estimating the burden of rabies in Nigeria
#  Estimates of PEP seeking - pPEP likely increases with wealth i.e. lower poverty index (scales from 0-1)
poverty = seq(0,1,0.05)
prob = 1 / (1 + exp(poverty))

# p = 1 / (1 + e^−(β₀ + β₁x))
b0 <- 0  # intercept
b1 <- 5 # slope

# Apply inverse logit manually
pPEP_prob_seq <- 1 / (1 + exp(-(b0 + b1 * poverty)))

# Plot
plot(poverty, pPEP_prob_seq,  type = "l", ylim = c(0, 1), 
     xlab = "Predictor", ylab = "Predicted probability")

b0 <- -1; b1 <- 5 
pPEP_prob_seq <- 1 / (1 + exp(-(b0 + b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "red")

b0 <- -1; b1 <- 2 
pPEP_prob_seq <- 1 / (1 + exp(-(b0 + b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "blue")

b0 <- 0.5; b1 <- 10 # slope
pPEP_prob_seq <- 1 / (1 + exp(-(b0 + b1 * poverty)))
lines(poverty, pPEP_prob_seq,  col = "green")

###############################################################################
# Dog population - HDR increases with urban % and muslim % (more dogs per human in rural areas and non-muslim areas)
humans = seq(2000000,17000000, 1000000)
urban = seq(0.2,0.95,0.05)
muslim = seq(0, 1, length = 16)
hdr = seq(5,20,1)

# increases with % urban (more humans per dog in cities)
cu = 3
bu = 25
hdr = cu + urban * bu

plot(rural, hdr,  type = "l",  xlab = "Predictor", ylab = "Predicted probability", 
     xlim = c(0,1), ylim = c(0,30))

# decreases with % muslim
cm = 3; bm = 25
hdr = cm + muslim * bm

plot(muslim, hdr,  type = "l",  xlab = "Predictor", ylab = "Predicted probability", 
     xlim = c(0,1))

# Combined relationship - multiplicative
c = 0
bm = 25
bu = 25
bcomb = -15
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)

plot(muslim, hdr, ylim= c(0,50), 
     type = "l",  xlab = "Predictor", ylab = "Predicted probability", 
     xlim = c(0,1))

bcomb = -5
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)
lines(muslim, hdr,  col = "green")

bcomb = -25
hdr = c + (bm * muslim) + (bu * urban) + bcomb*(urban * muslim)
lines(muslim, hdr,  col = "green")


###############################################################################
# dog vaccination coverage - expect coverage to decline as poverty increases
# Inverse logistic (flipped sigmoid)
poverty = seq(0,1,0.05)
vc_decline = 4 # determines how steep the decline is
vc_slope_shift = 0.05 # determines where along x-axis decline starts
vc_int = 0.005 # minimum coverage (asymptote as poverty increases)

prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))

plot(poverty, prob, ylim= c(0,1), 
     type = "l",  xlab = "Predictor", ylab = "Predicted probability", 
     xlim = c(0,1))

vc_decline = 6 # 
vc_slope_shift = .25 # 
vc_int = 0.05 # 

prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))
lines(poverty, prob,  col = "green")

vc_decline = 6 # 
vc_slope_shift = .001 # 
vc_int = 0.0 # 

prob = vc_int + (1-vc_int) * (1-1/(1 + exp(-vc_decline*(poverty - vc_slope_shift))))
lines(poverty, prob,  col = "red")

 




