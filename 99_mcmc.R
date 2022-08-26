## 99_mcmc.R
# MCMC options for nimble
# August 2022

debug = FALSE
n.chains = 2
thin = 5
MCMC = 10000
seed = 1234

if(pilot==TRUE){
  debug = TRUE
  n.chains = 2
  thin = 1
  MCMC = 1000
  seed = 1234
}
