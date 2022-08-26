# 99_nimble_editorial_outcome.R
# Model if submission time effects the editorial outcome
# ordinal logistic regression using a cumulative logit model with latent cut-points
# August 2022
library(nimble)

## define the model, using continuous latent variable with cut-offs
# does not include country
code = nimbleCode({
  for (i in 1:N){
    outcome[i,1:3] ~ dmulti(pi[i,1:3],1);
    # Cumulative probability of > category k given cut-point
    for (k in 1:2){
      logit(Q[i,k]) <- (intercept + (cosine*cos[i]) + (sine*sin[i])
                       +(week*weekend[i]) + (vers[1]*version[i,1]) + (vers[2]*version[i,2]) + (vers[3]*version[i,3] )) - C[k];
    }
    # Calculate probabilities
    pi[i,1] <- 1 - Q[i,1]; # Pr(cat=1) = 1 - Pr(cat>1);
    pi[i,2] <- Q[i,1] - Q[i,2]; # Pr(cat>k-1) - Pr(cat>k);
    pi[i,3] <- Q[i,2]; # Pr(cat=k) = Pr(cat>k-1);
  }
  # priors
  intercept ~ dnorm(0, sd=10000)
  cosine ~ dnorm(0, sd=10000)
  sine ~ dnorm(0, sd=10000)
  for(j in 1:3){vers[j] ~ dnorm(0, sd=10000)}
  week ~ dnorm(0, sd=10000)
  # ordered cut-offs
  C[1] <- 0; # for identifiability
  C[2] ~ dunif(C[1],10);
  # predicted probabilities over 24 hours, not the weekend and version set to reference
  for(h in 1:24){
    for(k in 1:2){
      logit(Q.pred[h,k]) <- (intercept+(cosine*cos.pred[h])+(sine*cos.pred[h])) - C[k];
    }
    # Calculate probabilities
    pi.pred[h,1] <- 1 - Q.pred[h,1]; # Pr(cat=1) = 1 - Pr(cat>1);
    pi.pred[h,2] <- Q.pred[h,1]-Q.pred[h,2]; # Pr(cat>k-1) - Pr(cat>k);
    pi.pred[h,3] <- Q.pred[h,2]; # Pr(cat=k) = Pr(cat>k-1);
  }
})

## prepare data for model
for_model = filter(jpc, decision !='Awaiting') %>% # remove papers without a decision - can't use ordinal otherwise
  mutate(outcome = case_when(
    decision == 'Reject' ~ 1,
    decision == 'Revise' ~ 2,
    decision == 'Accept' ~ 3
  ),
  weekend = as.numeric(weekend == 'Weekend'),
  version = ifelse(version >= 5, 4, version), # truncate version at 4 (small numbers in 5 and 6)
  versionf = as.factor(version), # model as a factor instead of a linear change
  cos = cos(2*pi*local.hour/24), # for diurnal pattern, local.hour is not rounded
  sin = sin(2*pi*local.hour/24))
# for diurnal predictions
predictions = data.frame(hour=0:23) %>%
  mutate(sin = sin(hour*2*pi/24),
         cos = cos(hour*2*pi/24))

# make version matrix
vmatrix = model.matrix(outcome ~ versionf, data=for_model)
vmatrix = as.matrix(vmatrix[,-1]) # remove intercept

## data
constants <- list(N = nrow(for_model),
                  sin = for_model$sin,
                  cos = for_model$cos,
                  weekend = for_model$weekend,
                  version = vmatrix, # reference is version 1
                  cos.pred = predictions$cos,
                  sin.pred = predictions$sin) 
# outcome needs to be a binary matrix
outcome_matrix = matrix(data=0, ncol=3, nrow=nrow(for_model))
for (k in 1:3){
  outcome_matrix[,k] = as.numeric(for_model$outcome ==k)
}
data <- list(outcome = outcome_matrix ) # needs to be a matrix

## initial values
inits <- list(intercept = 0, cosine = 0, sine = 0, vers = c(0,0,0), week = 0, C = c(NA, 1))

# parameters to store
parms = c('intercept', 'cosine', 'sine', 'vers', 'week', 'C', 'pi.pred')

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
results_editorial = process_nimble(indata = data$outcome, inmcmc = mcmc, predictions = FALSE)

# get predictions of curves - to do
predictions = filter(results_editorial$table, str_detect(parameter, '\\.pred')) %>%
  separate(col='row', into=c('hour','outcome'), sep=',', convert=TRUE) %>%
  mutate(hour = hour - 1,
         outcome = factor(outcome, levels=1:3, labels=c('Reject','Revise','Accept')))

# remove predictions from table
results_editorial$table = filter(results_editorial$table, !str_detect(parameter, '\\.pred'))
results_editorial$table$parameter = str_remove_all(results_editorial$table$parameter, '[^A-Z|a-z|0-9]') # remove square brackets
# plot predictions as a bar
pplot = ggplot(data=predictions, aes(x=hour, y=mean, fill=outcome))+
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual(NULL, values=c('indianred2','darkseagreen2','skyblue'))+
  ylab('Predicted probability')+
  g.theme+
  theme(legend.position = 'top')
results_editorial$pplot = pplot
results_editorial$predictions = predictions

# odds ratio by hour with credible intervals
intercept = mcmc$samples$chain1[,4]
plot(intercept) # quick check
cosine = mcmc$samples$chain1[,3]
sine = mcmc$samples$chain1[,77]
odds = NULL
for (hour in 0:23){
  cos = cos(hour*2*pi/24)
  sin = sin(hour*2*pi/24)
  logit = cos*cosine + sin*sine
  mean = exp(mean(logit))
  lower = exp(quantile(logit, 0.025))
  upper = exp(quantile(logit, 1 - 0.025))
  frame = data.frame(hour = hour, mean = mean, lower = lower, upper = upper)
  odds = bind_rows(odds, frame)
}
row.names(odds) = NULL
results_editorial$odds = odds

# save
save(results_editorial, file=outfile$file)
