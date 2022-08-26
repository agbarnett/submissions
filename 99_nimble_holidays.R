# 99_nimble_holidays.R
# holiday model

## not complete ##

library(nimble)

## define the model
code = nimbleCode({
  for (k in 1:N){
    holiday[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- intercept + trend*window[k] + hol*holiday[k]
  }
  intercept ~ dnorm(0, sd=10000)
  trend ~ dnorm(0, sd=10000)
  hol ~ dnorm(0, sd=10000)
})


## just weeks/country pairs with holidays
hols = filter(jpc, holiday=='Yes') %>% select(window, country) %>% unique()
for.analysis = left_join(hols, jpc, by=c('window','country'))

#### MORE TO DO HERE< need to include results where there were 0 submissions on the holiday

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