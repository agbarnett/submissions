# 99_nimble_weekend.R
# Models of weekend submissions with country specific slope and intercept
# four models:
# - country-specific/fixed pandemic & country-specific/fixed slope
# August 2022
library(season)
library(nimble)

## define the models 
# a) fixed slope; fixed pandemic
code_fixedpandemic_fixedslope <- nimbleCode({
  for (k in 1:N){
    weekend[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- country.intercept[country[k]] + slope*window[k] + 
      change*pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  slope ~ dnorm(0, sd=10000)
  change ~ dnorm(0, sd=10000)
  for (c in 1:M){
    country.intercept[c] ~ dnorm(intercept, tau) # country-specific intercept
  }
  tau ~ dgamma(1,1)
  # predictions and differences
  logit(p.pred1) <- intercept + slope*window.pred
  logit(p.pred2) <- intercept + slope*window.pred + change
  logit(p.pred3) <- intercept + slope*(window.pred+1)
  difference1 <- p.pred2 - p.pred1
  difference2 <- p.pred3 - p.pred1
})
# b) random slope; fixed pandemic
code_fixedpandemic_randomslope <- nimbleCode({
  for (k in 1:N){
    weekend[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- country.intercept[country[k]] + country.slope[country[k]]*window[k] + 
      change*pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  slope ~ dnorm(0, sd=10000)
  change ~ dnorm(0, sd=10000)
  for (c in 1:M){
    country.intercept[c] ~ dnorm(intercept, tau[1]) # country-specific intercept
    country.slope[c] ~ dnorm(slope, tau[2]) # country-specific change over time
  }
  for(k in 1:2){tau[k] ~ dgamma(1,1)}
  # predictions and differences
  logit(p.pred1) <- intercept + slope*window.pred
  logit(p.pred2) <- intercept + slope*window.pred + change
  logit(p.pred3) <- intercept + slope*(window.pred+1)
  difference1 <- p.pred2 - p.pred1
  difference2 <- p.pred3 - p.pred1
})
# c) fixed slope; random pandemic
code_randompandemic_fixedslope <- nimbleCode({
  for (k in 1:N){
    weekend[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- country.intercept[country[k]] + slope*window[k] + 
      country.change[country[k]]*pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  slope ~ dnorm(0, sd=10000)
  change ~ dnorm(0, sd=10000)
  for (c in 1:M){
    country.intercept[c] ~ dnorm(intercept, tau[1]) # country-specific intercept
    country.change[c] ~ dnorm(change, tau[2]) # country-specific change
  }
  for(k in 1:2){tau[k] ~ dgamma(1,1)}
  # predictions and differences
  logit(p.pred1) <- intercept + slope*window.pred
  logit(p.pred2) <- intercept + slope*window.pred + change
  logit(p.pred3) <- intercept + slope*(window.pred+1)
  difference1 <- p.pred2 - p.pred1
  difference2 <- p.pred3 - p.pred1
})
# d) random slope; random pandemic
code_randompandemic_randomslope <- nimbleCode({
  for (k in 1:N){
    weekend[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- country.intercept[country[k]] + country.slope[country[k]]*window[k] + 
      country.change[country[k]]*pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  slope ~ dnorm(0, sd=10000)
  change ~ dnorm(0, sd=10000)
  for (c in 1:M){
    country.intercept[c] ~ dnorm(intercept, tau[1]) # country-specific intercept
    country.slope[c] ~ dnorm(slope, tau[2]) # country-specific intercept
    country.change[c] ~ dnorm(change, tau[3]) # country-specific change
  }
  for(k in 1:3){tau[k] ~ dgamma(1,1)}
  # predictions and differences
  logit(p.pred1) <- intercept + slope*window.pred
  logit(p.pred2) <- intercept + slope*window.pred + change
  logit(p.pred3) <- intercept + slope*(window.pred+1)
  difference1 <- p.pred2 - p.pred1
  difference2 <- p.pred3 - p.pred1
})

## data
# combine small countries together
numbers = group_by(jpc.weekend, country) %>%
  tally() %>%
  ungroup()
for.model = left_join(jpc.weekend, numbers, by='country') %>%
  ungroup() %>%
  mutate(country = ifelse(n < 50, 'small', country), 
         country.num = as.numeric(as.factor(country)), 
         pandemic = as.numeric(window > pandemic_week$window), # pandemic window from 2_analysis.Rmd
         yrfrac = yrfraction(date), # add sine and cosine - no longer using
         cos = cos(yrfrac*2*pi),
         sin = sin(yrfrac*2*pi)) 
constants <- list(N = nrow(for.model),
                  window.pred = 7.5, # just before pandemic, window = 271, then standardised
                  window = (for.model$window - 241) / 4, # scaled to 4 weeks; centered at median
                  pandemic = for.model$pandemic,
                  denom = for.model$denom , 
                  country = for.model$country.num,
                  M = length(unique(for.model$country))) 
data <- list(weekend = for.model$Weekend )
# make list of countries and numbers
country.num = select(for.model, country.num, country) %>%
  unique() %>%
  arrange(country.num)

## initial values
# a)
inits_fixedpandemic_fixedslope <- list(intercept = 0,
                                slope = 0,
                                change = 0,
                                country.intercept = rep(0, constants$M),
                                tau = 1)
# b)
inits_fixedpandemic_randomslope <- list(intercept = 0,
                                       slope = 0,
                                       change = 0,
                                       country.slope = rep(0, constants$M),
                                       country.intercept = rep(0, constants$M),
                                       tau = c(1,1))
# c)
inits_randompandemic_fixedslope <- list(intercept = 0,
                                        slope = 0,
                                        change = 0,
                                        country.change = rep(0, constants$M),
                                        country.intercept = rep(0, constants$M),
                                        tau = c(1,1))
# d)
inits_randompandemic_randomslope <- list(intercept = 0,
                                        slope = 0,
                                        change = 0,
                                        country.slope = rep(0, constants$M),
                                        country.change = rep(0, constants$M),
                                        country.intercept = rep(0, constants$M),
                                        tau = c(1,1,1))


# parameters to store
parms_fixedpandemic_fixedslope = c('tau','slope','intercept','change','country.intercept','p.pred1','p.pred2','p.pred3','difference1','difference2')
parms_fixedpandemic_randomslope = c(parms_fixedpandemic_fixedslope, 'country.slope')
parms_randompandemic_fixedslope = c(parms_fixedpandemic_fixedslope, 'country.change')
parms_randompandemic_randomslope = c(parms_fixedpandemic_fixedslope, 'country.slope', 'country.change')

## models
# a)
model_fixedpandemic_fixedslope <- nimbleModel(code = code_fixedpandemic_fixedslope, 
                     data = data, 
                     inits = inits_fixedpandemic_fixedslope, 
                     constants = constants)
# b)
model_fixedpandemic_randomslope <- nimbleModel(code = code_fixedpandemic_randomslope, 
                    data = data, 
                    inits = inits_fixedpandemic_randomslope, 
                    constants = constants)
# c)
model_randompandemic_fixedslope <- nimbleModel(code = code_randompandemic_fixedslope, 
                    data = data, 
                    inits = inits_randompandemic_fixedslope, 
                    constants = constants)
# d)
model_randompandemic_randomslope <- nimbleModel(code = code_randompandemic_randomslope, 
                                               data = data, 
                                               inits = inits_randompandemic_randomslope, 
                                               constants = constants)

# MCMC samples
pilot = FALSE
source('99_mcmc.R')
# a)
mcmc_fixedpandemic_fixedslope <- nimbleMCMC(model = model_fixedpandemic_fixedslope,
                       inits = inits_fixedpandemic_fixedslope,
                       monitors = parms_fixedpandemic_fixedslope,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seed,
                       WAIC = TRUE)
# b)
mcmc_fixedpandemic_randomslope <- nimbleMCMC(model = model_fixedpandemic_randomslope,
                        inits = inits_fixedpandemic_randomslope,
                        monitors = parms_fixedpandemic_randomslope,
                        niter = MCMC*2*thin, # times 2 for burn-in 
                        thin = thin,
                        nchains = n.chains, 
                        nburnin = MCMC,
                        summary = TRUE, 
                        setSeed = seed,
                        WAIC = TRUE)
# c)
mcmc_randompandemic_fixedslope <- nimbleMCMC(model = model_randompandemic_fixedslope,
                        inits = inits_randompandemic_fixedslope,
                        monitors = parms_randompandemic_fixedslope,
                        niter = MCMC*2*thin, # times 2 for burn-in 
                        thin = thin,
                        nchains = n.chains, 
                        nburnin = MCMC,
                        summary = TRUE, 
                        setSeed = seed,
                        WAIC = TRUE)
# d)
mcmc_randompandemic_randomslope <- nimbleMCMC(model = model_randompandemic_randomslope,
                                             inits = inits_randompandemic_randomslope,
                                             monitors = parms_randompandemic_randomslope,
                                             niter = MCMC*2*thin, # times 2 for burn-in 
                                             thin = thin,
                                             nchains = n.chains, 
                                             nburnin = MCMC,
                                             summary = TRUE, 
                                             setSeed = seed,
                                             WAIC = TRUE)


# process results
results_1 = process_nimble(indata = data$weekend, inmcmc = mcmc_fixedpandemic_fixedslope)
results_2 = process_nimble(indata = data$weekend, inmcmc = mcmc_fixedpandemic_randomslope)
results_3 = process_nimble(indata = data$weekend, inmcmc = mcmc_randompandemic_fixedslope)
results_4 = process_nimble(indata = data$weekend, inmcmc = mcmc_randompandemic_randomslope)

# combined results
table = bind_rows(results_1$table, results_2$table, results_3$table, results_4$table, .id = 'model')
model_fit = bind_rows(results_1$model_fit, results_2$model_fit, results_3$model_fit, results_4$model_fit, .id = 'model')
intercepts = bind_rows(results_1$intercepts, results_2$intercepts, results_3$intercepts, results_4$intercepts, .id = 'model')
changes = bind_rows(results_3$changes, results_4$changes, .id = 'model')
slopes = bind_rows(results_2$slopes, results_4$slopes, .id = 'model')

# save results as a list
results_weekend = list()
results_weekend$model_fit = model_fit
results_weekend$table = table
results_weekend$intercepts = intercepts
results_weekend$changes = changes
results_weekend$slopes = slopes
save(country.num, results_weekend, file = outfile$file)
