# 99_functions.R
# functions for JPC analyses
# August 2022

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# make file names for results and check if file already exists
make_file = function(infile){
  ret = paste(paste(infile[1:2], collapse='/'), '.RData', sep='', collapse='')
  ret_short = paste(infile[2], '.RData', sep='', collapse='')
  exists = length(dir(infile[1], pattern = ret_short)) > 0
  #
  to_return = list()
  to_return$file = ret
  to_return$exists = exists
  return(to_return)
}

# process results from nimble to get table, residuals and model fit
process_nimble = function(indata, inmcmc, predictions = FALSE){
  
  ## table
  # extract summaries
  table = as.data.frame(inmcmc$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]|\\.')) 
  
  # add posterior p-values
  pos = inmcmc$samples$chain1 > 0
  pos = colMeans(pos)
  pos.dash = 1 - pos
  pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
  table = bind_cols(table, pval)
  names(table) = c('parameter','mean','median','sd','lower','upper','row','post_prob')
  
  ## country-specific intercepts
  intercepts = NULL
  if(any(str_detect(table$parameter, pattern='country.intercept'))){
    #
    intercepts = filter(table, str_detect(parameter, pattern='^country.intercept')) 
    # now remove from table
    table = filter(table, 
                   !str_detect(parameter, pattern='^country.intercept'))
  }
  
  ## country-specific slopes
  slopes = NULL
  if(any(str_detect(table$parameter, pattern='country.slope'))){
    #
    slopes = filter(table, str_detect(parameter, pattern='^country.slope')) 
    # now remove from table
    table = filter(table, 
                   !str_detect(parameter, pattern='^country.slope'))
  }

  ## country-specific changes
  changes = NULL
  if(any(str_detect(table$parameter, pattern='country.change'))){
    #
    changes = filter(table, str_detect(parameter, pattern='^country.change')) 
    # now remove from table
    table = filter(table, 
                   !str_detect(parameter, pattern='^country.change'))
  }

  ## predictions and residuals
  residuals = NULL
  if(predictions == TRUE){
    ## predictions and residuals
    fitted = filter(table, str_detect(parameter, pattern='^mu')) %>% # model predictions
      select(row, mean) %>%
      mutate(row = as.numeric(row)) %>%
      bind_cols(indata)
    names(fitted)[3] = 'observed'
    
    #
    residuals = mutate(fitted, residual = observed - mean)
    
    # now remove fitted values from table
    table = filter(table, 
                   !str_detect(parameter, pattern='^mu'))
  }

  ## model fit
  model_fit = data.frame(WAIC = inmcmc$WAIC$WAIC,
                         pWAIC = inmcmc$WAIC$pWAIC)
  
  
  ##
  to_return = list()
  to_return$table = table
  to_return$intercepts = intercepts
  to_return$slopes = slopes
  to_return$changes = changes
  to_return$residuals = residuals
  to_return$model_fit = model_fit
  return(to_return)
}

# process a table
process_table = function(intable, rows=c('beta'), 
                         exp = FALSE, # exponentiate or not
                         dp = 1){
  rows_search = paste(rows, collapse = '|')
  to_table = filter(intable, 
                    str_detect(parameter, pattern=rows_search)) # do not show intercept
  # arrange according to order in function option
  to_table = mutate(to_table, parameter = factor(parameter, levels=rows, ordered=TRUE)) %>%
    arrange(parameter) %>%
    mutate(parameter = as.character(parameter))
  #  
  to_table = mutate(to_table,
           term = parameter_rename(parameter),
           post_prob = format.pval(post_prob, digits=2, eps=0.0001))
  # 
  if(exp==TRUE){
    to_table = mutate(to_table,
                      mean = exp(mean),
                      lower = exp(lower),
                      upper = exp(upper))
  }
  to_table = mutate(to_table,
           #mean = 100*(exp(mean)- 1), # percent change
           #lower = 100*(exp(lower)- 1),
           #upper = 100*(exp(upper)- 1),
           mean = roundz(mean, dp), # rounding
           lower = roundz(lower, dp),
           upper = roundz(upper, dp),
           ci = paste(lower , ' to ' , upper, sep='')) %>%
    select(term, mean, ci, post_prob)
  ftab = flextable(to_table)%>%
    autofit() %>%
    theme_box()
  return(ftab)
}

## parameter rename for tables of parameter estimates
parameter_rename = function(x, papers=TRUE, monthly=FALSE){
  case_when(
    x == 'intercept' ~ 'Intercept', 
    x == 'sine' ~ 'Sine for 24-hour curve', 
    x == 'cosine' ~ 'Cosine for 24-hour curve', 
    x == 'sine.change' ~ 'Change in Sine post-pandemic', 
    x == 'cosine.change' ~ 'Change in Cosine post-pandemic', 
    x == 'trend' ~ 'Trend per year',
    x == 'change' ~ 'Post-pandemic',
    x == 'slope' ~ 'Change per 4 weeks',
    x == 'pand' ~ 'Post-pandemic',
    x == 'week' ~ 'Weekend',
    x == 'vers1' ~ 'Version 2',
    x == 'vers2' ~ 'Version 3',
    x == 'vers3' ~ 'Version 4+',
    x == 'p.pred1' ~ 'Weekend probability pre-pandemic',
    x == 'p.pred2' ~ 'Weekend probability post-pandemic',
    x == 'p.pred3' ~ 'Weekend probability + 4 weeks',
    x == 'difference1' ~ 'Difference due to pandemic',
    x == 'difference2' ~ 'Difference due to trend + 4 weeks',
    x == 'sd' ~ 'Standard deviation',
    TRUE ~ as.character(x))
}

# inverse logit
inv.logit = function(x){
  exp(x) / (1 + exp(x))
}
