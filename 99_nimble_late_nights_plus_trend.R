# 99_nimble_late_nights_plus_trend.R
# not yet complete
# Model difference in time of day post-pandemic
# August 2022
library(nimble)
library(ggrepel)

## define the model, with pre- and post-pandemic hourly pattern
code = nimbleCode({
  for (k in 1:N){
    numbers[k] ~ dbinom(mu[k], denom[k])
    logit(mu[k]) <-  intercept + 
      trend.cos * window[k]*cos[k] + # trend
      trend.sin * window[k]*sin[k] +
      country.cos[country[k]]*cos[k]*(1-pandemic[k]) + 
      country.sin[country[k]]*sin[k]*(1-pandemic[k]) +
      (country.cos[country[k]] + cosine.change)*cos[k]*pandemic[k] + 
      (country.sin[country[k]] + sine.change)*sin[k]*pandemic[k]
  }
  intercept ~ dnorm(0, sd=10000)
  cosine ~ dnorm(0, sd=10000)
  sine ~ dnorm(0, sd=10000)
  cosine.change ~ dnorm(0, sd=10000) # pandemic change in season (common to all countries)
  sine.change ~ dnorm(0, sd=10000)
  for (c in 1:M){
    country.cos[c] ~ dnorm(cosine, tau[1]) # country-specific season
    country.sin[c] ~ dnorm(sine, tau[2]) # country-specific season
  }
  tau[1] ~ dgamma(0.1, 0.1)
  tau[2] ~ dgamma(0.1, 0.1)
  # prediction (pre and post pandemic)
  for (j in 1:24){
    logit(pre.pred[j]) <- intercept + cosine*cos.pred[j] + sine*sin.pred[j]
    logit(post.pred[j]) <- intercept + (cosine+cosine.change)*cos.pred[j] + (sine+sine.change)*sin.pred[j]
  }
    
})

## hours per country
# first combine smaller countries
numbers = group_by(jpc, country) %>%
  tally() %>%
  ungroup() %>%
  rename('total' = 'n')
for_model = left_join(jpc, numbers, by='country') %>%
  ungroup() %>%
  mutate(large_window = 1 + floor((window-1) / 4), # make larger windows for this model
         large_window = (large_window - 65)/5, # standardise
         country = ifelse(total < 50, 'small', country), 
         hour = floor(local.hour)) %>%
  group_by(country, large_window, hour, pandemic) %>%
  tally() %>%
  ungroup()
# add denominator
country_total = group_by(for_model, country, large_window, pandemic) %>%
  summarise(denom = sum(n))
# add hours with zero submissions
countries = unique(for_model$country)
all_combinations = expand.grid(country = countries, hour = 0:23, pandemic=c('Pre-pandemic','Post-pandemic')) %>%
  mutate(pandemic = factor(pandemic))
for_model = full_join(all_combinations, for_model, by=c('country','hour','pandemic')) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(country, pandemic, hour) %>%
  left_join(country_total, by=c('country','pandemic')) %>%
  filter(!is.na(denom)) %>% # remove weeks with no results
  mutate(country.num = as.numeric(as.factor(country)))
#
predictions = data.frame(hour=0:23) %>%
  mutate(sin = sin(hour*2*pi/24),
         cos = cos(hour*2*pi/24))

## data
constants <- list(N = nrow(for_model),
                  country = for_model$country.num,
                  M = length(unique(for_model$country)),
                  denom = for_model$denom,
                  pandemic = as.numeric(for_model$pandemic == 'Post-pandemic'),
                  sin = sin(for_model$hour*2*pi/24),
                  cos = cos(for_model$hour*2*pi/24),
                  cos.pred = predictions$cos,
                  sin.pred = predictions$sin
                  ) 
data <- list(numbers = for_model$n ) # 

## initial values
inits <- list(intercept = 0, cosine = 0, sine=0, cosine.change = 0, sine.change=0, tau=c(1,1),
              cos.trend = 0, sin.trend = 0,
              country.cos = rep(0, constants$M), country.sin = rep(0, constants$M))

# parameters to store
parms = c('intercept', 'cosine', 'sine', 'country.cos', 'country.sin', 'cosine.change', 'sine.change', 'tau', 'mu', 'pre.pred', 'post.pred')

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
results_hours = process_nimble(indata = data$numbers, inmcmc = mcmc, predictions = TRUE)

# get predictions of curves
predictions = filter(results_hours$table, str_detect(parameter, '\\.pred')) %>%
  mutate(pandemic = as.numeric(str_detect(parameter, 'post'))+1,
         pandemic = factor(pandemic, levels=1:2, labels=c('Pre-pandemic','Post-pandemic')),
         hour = as.numeric(row) - 1)
# remove predictions from table
results_hours$table = filter(results_hours$table, !str_detect(parameter, '\\.pred'))
# plot
pplot = ggplot(data=predictions, aes(x=hour, y=mean, ymin=lower, ymax=upper, col=pandemic))+
  geom_ribbon(alpha=0.2)+
  geom_line(size=1.05)+
  scale_color_manual(NULL, values=c('skyblue','indianred2'))+
  ylab('Submission probability')+
  g.theme+
  theme(legend.position = c(0.17,0.8))
results_hours$pplot = pplot

# get country-specific amplitude and phase
country.season = filter(results_hours$table, str_detect(parameter, '^country\\.')) %>%
  mutate(country.num = str_remove_all(parameter, '[A-Z|a-z]|\\[|\\]|\\.'),
         country.num = as.numeric(country.num),
         type = str_detect(parameter, '\\.cos'),
         type = ifelse(type==TRUE, 'cos','sin')) %>%
  select(country.num, type, mean) %>%
  pivot_wider(names_from = type, values_from = mean) %>%
  mutate(amp = sqrt(cos^2 + sin^2),
         phase = NA)
for (k in 1:nrow(country.season)){
  country.season$phase[k] = 23 * phasecalc(country.season$cos[k], country.season$sin[k])/(2*pi) # recaled to hours
}
# add country labels
labels = select(for_model, country.num, country) %>%
  unique() %>%
  ungroup()
country.season = full_join(country.season, labels, by='country.num')
# remove sine/cosine from table
results_hours$table = filter(results_hours$table, !str_detect(parameter, '^country\\.'))
## plot
phase_plot = ggplot(data=country.season, aes(x=phase, y=amp, label=country))+
  geom_point()+
  geom_text_repel()+
  xlab('Peak time (24-hour clock)')+
  ylab('Amplitude')+
  g.theme
results_hours$phase_plot = phase_plot

# save
save(results_hours, file=outfile$file)
