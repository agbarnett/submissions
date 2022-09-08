# 99_nimble_trend_change.R
# Model of increasing trend and post-pandemic increase in submissions
# version with post-pandemic change to allow the covid-19 bubble to subside
# September 2022
library(nimble)

## define the model
code = nimbleCode({
  for (k in 1:N){
    numbers[k] ~ dnorm(mu[k], tau)
    mu[k] <- intercept + trend*time[k] + pand*pandemic[k] - pand*post_pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  trend ~ dnorm(0, sd=10000)
  pand ~ dnorm(0, sd=10000)
  tau ~ dgamma(1, 1)
  sd <- 1/sqrt(tau)
})

# put in year/month format
pandemic_yrmon = 2020 + ((3-1)/12) # March 2020

## data
constants <- list(N = nrow(over.time),
                  time = over.time$yrmon - median(over.time$yrmon), # centred time, in scale of years
                  pandemic = as.numeric(over.time$yrmon > pandemic_yrmon)
                  ) 
data <- list(numbers = over.time$adj_count ) # use adjusted count

## initial values
inits <- list(intercept = 100, trend = 0, pand = 0, tau = 1)

# parameters to store
parms = 'pand'

# Loop through post-pandemic (on scale of year-months); start one month after the pandemic
post_pandemic = seq(pandemic_yrmon + 1/12, pandemic_yrmon + 1 + (4/12), 1/12) # months to examine
WAIC = NULL
for (post in post_pandemic){
  constants$post_pandemic = as.numeric(over.time$yrmon > post) # change the post-pandemic time

# models
model <- nimbleModel(code = code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
pilot = FALSE
source('99_mcmc.R')
n.chains = 1 # quicker run
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

# 
frame = data.frame(post_pandemic = post, 
                   pWAIC = mcmc$WAIC$pWAIC,
                  WAIC = mcmc$WAIC$WAIC)
WAIC = bind_rows(WAIC, frame)

} # end of loop

# find the best model
smallest = arrange(WAIC, WAIC) %>%
  slice(1) %>%
  pull(post_pandemic)
# update the data
constants$post_pandemic = as.numeric(over.time$yrmon > smallest)
# models
parms = c('intercept', 'trend', 'pand','mu')
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
# process the results for the best model
results_trend_change = process_nimble(indata = data$numbers, inmcmc = mcmc, predictions = TRUE)

# save
save(WAIC, smallest, results_trend_change, file=outfile$file)
