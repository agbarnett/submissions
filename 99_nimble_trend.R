# 99_nimble_trend.R
# Model of increasing trend and post-pandemic increase in submissions
# August 2022
library(nimble)

## define the model
code = nimbleCode({
  for (k in 1:N){
    numbers[k] ~ dnorm(mu[k], tau)
    mu[k] <- intercept + trend*time[k] + pand*pandemic[k] 
  }
  intercept ~ dnorm(0, sd=10000)
  trend ~ dnorm(0, sd=10000)
  pand ~ dnorm(0, sd=10000)
  tau ~ dgamma(1, 1)
  sd <- 1/sqrt(tau)
})

## data
constants <- list(N = nrow(over.time),
                  time = over.time$yrmon - median(over.time$yrmon), # centred time, in scale of years
                  pandemic = as.numeric(over.time$yrmon > pandemic_yrmon)
                  ) 
data <- list(numbers = over.time$adj_count ) # use adjusted count

## initial values
inits <- list(intercept = 100, trend = 0, pand = 0, tau = 1)

# parameters to store
parms = c('sd', 'intercept', 'trend', 'pand', 'mu')

# models
model <- nimbleModel(code = code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
pilot = FALSE
source('99_mcmc.R')
mcmc <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seed,
                       WAIC = TRUE)

# process the results
results_trend = process_nimble(indata = data$numbers, inmcmc = mcmc, predictions = TRUE)

# save
save(results_trend, file=outfile$file)
