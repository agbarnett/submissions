# 2_plot_intercepts.R
# function to plot intercepts from nimble model (results from 99_nimble_weekend.R)
# August 2022

# reference line for overall intercept and 95% CI
xref = filter(results_weekend$table, model==1, parameter=='intercept') %>%
  select(mean, lower, upper) %>%
  mutate_all( inv.logit) # back transform mean and 95% CI

to.plot = filter(results_weekend$intercepts, model==1) %>%
  mutate(row = as.numeric(row),
         lower = inv.logit(lower),
         mean = inv.logit(mean),
         upper = inv.logit(upper)) %>%
  left_join(country.num, by=c('row' = 'country.num')) %>% # add country labels
  arrange(mean) %>%
  mutate(xaxis = 1:n(), colour=1)
n.countries = length(unique(to.plot$country)) + 1 # plus one for mean

# add overall mean and CI
to.plot.mean = mutate(xref, country='Overall', colour=2, Count=0, xaxis=max(to.plot$xaxis)+1)
to.plot = bind_rows(to.plot, to.plot.mean)

# shading to help identify countries
shading = data.frame(xmin=seq(1, n.countries-1, 2)) %>% # ignore mean so minus 1
  mutate(xmax = xmin+0.1,
         lower = min(to.plot$lower),
         upper = max(to.plot$upper) )

# plot
iplot = ggplot(data=to.plot, aes(x=xaxis, y=mean, ymin=lower, ymax=upper, col=factor(colour))) +
  annotate('rect', xmin=shading$xmin, xmax=shading$xmax, ymin=shading$lower, ymax=shading$upper, col=grey(0.9), size=3) + # 
  geom_point()+
  scale_color_manual(NULL, values=c('black','blue'))+
  geom_errorbar(width=0)+
  scale_x_continuous(breaks=1:n.countries, labels=to.plot$country, limits = c(0, n.countries))+
  xlab('')+
  geom_hline(yintercept = xref$mean, lty=2)+ # Add reference line at overall mean
  ylab('Probability')+
  annotate('text', y=xref$mean, x=0, label='Above average', hjust=-0.05, size=3)+
  annotate('text', y=xref$mean, x=0, label='Below average', hjust=1.05, size=3)+
  g.theme +
  theme(axis.text.x = element_text(size=8), legend.position = 'none',
        panel.grid.major.y = element_blank())+
  coord_flip()
    

# export
export = FALSE
if(export==TRUE){
  jpeg('figures/intercepts.weekend.jpg', width=5, height=4, units='in', res=400, quality=100)
  print(iplot)
  invisible(dev.off())
}