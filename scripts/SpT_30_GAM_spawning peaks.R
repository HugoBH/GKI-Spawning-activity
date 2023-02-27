# adj.yday is soaking up all the variance from SST because they are likely colinear 
# I could have a model that describes the time of the peaks (yday by year)
# and a second model with env..only



## GAM fitting
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(broom)     #for tidy output#
library(emmeans)   #for marginal means etc
library(MuMIn)     #for model selection and AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for simulated residuals
library(performance) #for residual disagnostics
library(see)        # to visualize residual diagnostics
library(lubridate)

# load dat.gam
load("outputs/dat21.spawning_5d_avg.Rdata")
head(dat.gam)

ggplot(dat.gam, aes(y = count, x = adj.yday)) +
  geom_point() +
  facet_wrap(~austral.year, ncol = 1, scale = "free") +
  theme_classic() +
  scale_x_continuous(limits = c(0,366)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank()) +
  geom_smooth(method='gam', formula=y~s(x),
              method.args=list(family='poisson')) 


# check model fit for 1 year
sub <- dat.gam %>% filter(austral.year == "2020-2021") %>% droplevels()

gam1 <- gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
            data = sub,
            family = poisson(link = 'log'),
            method = 'REML')
k.check(gam1)
draw(gam1, residuals = TRUE)
simulateResiduals(gam1, plot = TRUE)
#okay, resids still a bit noisy but good fit. Could detrend?
gam2 <- gam(count ~ s(adj.yday, k = 20, bs = 'cr') +
            s(adj.mnth,bs='cc', k=6),
            knots=list(adj.mnth=seq(1,12,length=6)),
            data = sub,
            family = poisson(link = 'log'),
            method = 'REML')
k.check(gam2)
simulateResiduals(gam2, plot = TRUE)
draw(gam2, residuals = TRUE)
#no effect of month, does not improve the model fit

gam3 <- gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
            data = sub,
            family = tw(link = 'log'),
            method = 'REML')
k.check(gam3)
resids = simulateResiduals(gam3, plot = TRUE)
testZeroInflation(resids)
testDispersion(resids)

draw(gam3, residuals = TRUE)
AIC(gam1, gam2, gam3)
#a Tweedie improves the model fit, better than a neg binom?

gam4 <- gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
            data = sub,
            family = nb(link = 'log'),
            method = 'REML')
(theta <- gam4$family$getTheta(TRUE))
gam4 <- gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
            data = sub,
            family = negbin(link = 'log', theta = theta),
            method = 'REML')
AICc(gam1, gam3, gam4)
#no, a tweedie is best



## Fit a gam to every year
gams.by.year <- dat.gam %>% group_by(austral.year) %>% nest() %>% 
  #GAMs
  mutate(gam = map(data, ~gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
                              data = .,
                              family = tw(link = 'log'),
                              method = 'REML'))) %>% 
  #Diagnostics
  mutate(k.check = map(gam, ~k.check(.)),
         resids = map(gam, ~simulateResiduals(., plot = TRUE)))
#all dharma resids look great

gams.by.year %>% dplyr::select(k.check) %>% unnest(k.check)
#k's are good



#Partial plots
em.gam.year <- gams.by.year %>% group_by(austral.year) %>% 
  #emmeans
  mutate(grid = map(data, ~with(., list(adj.yday = modelr::seq_range(adj.yday, by = 1)))),
         emmeans = map2(.x = gam, .y = grid, ~emmeans(.x, ~adj.yday, at = .y, type = "response") %>% 
                          as.data.frame())) %>% 
  #predict your own data, this will maytch observations in this case
  #These are a called Partial Residuals or Partial Observations
  #"We've added the partial residuals to our prediction"
  mutate(newdata = map(gam, ~data.frame(adj.yday = .$model$adj.yday)),
         presid = map2(.x = gam, .y = newdata, 
                       ~data.frame(Pred = predict(.x, newdata = .y, type = "link"),
                                   Resid = resid(.x, type = "response")))) %>% 
  #add summary table
  mutate(summary = map(gam, ~summary(.)),
         summary.df = map(summary, ~data.frame(dev.expl = .$dev.expl)))# %>% 
  #Add derivatives
  #mutate(derivatives = map(gam, ~derivatives(., term = "adj.yday", order = 1)))



save(em.gam.year, file = "outputs/mod30.gam.spawning.peaks.RData")


#deviance explained
em.gam.year %>% dplyr::select(summary.df) %>% unnest(summary.df)

#all models have evidence of significant wiggliness.
em.gam.year %>% pluck("gam",3) %>% summary()


#peaks
em.peaks = em.gam.year %>% 
  dplyr::select(austral.year, emmeans) %>% 
  unnest(emmeans)

em.peaks = em.peaks %>% group_by(austral.year) %>% 
  arrange(-response) %>% 
  top_n(n=1) %>% 
  bind_rows(
    em.peaks %>% filter(austral.year == "2008-2009", adj.yday >= 50) %>% 
      arrange(-response) %>% top_n(n = 1)) %>% 
  bind_rows(
    em.peaks %>% filter(austral.year == "2011-2012", adj.yday >= 180 & adj.yday <= 200) %>% 
      arrange(-response) %>% top_n(n = 1)) %>% 
  bind_rows(
    em.peaks %>% filter(austral.year == "2011-2012", adj.yday >= 200) %>% 
      arrange(-response) %>% top_n(n = 1)) %>% 
  bind_rows(
    em.peaks %>% filter(austral.year == "2020-2021", adj.yday >= 160) %>% 
      arrange(-response) %>% top_n(n = 1)) %>% 
  arrange(austral.year)  
  
save(em.peaks, file = "outputs/mod30.spawning.peaks.RData")

ggplot(data = em.gam.year %>% unnest(emmeans), aes(y = response, x = adj.yday )) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = .5) +
  geom_line() + facet_wrap(~austral.year, ncol = 1) +
  geom_point(data = em.gam.year %>% unnest(c(newdata, presid)) %>% 
               mutate(Presid = exp(Pred) + Resid), 
             aes(y = Presid, x = adj.yday), alpha = .5) +
  geom_point(data = em.peaks, aes(y = response, x = adj.yday), col = "blue", alpha =.5) +
  #geom_point(data = dat.gam, aes(y = count, x = adj.yday), col = "red", alpha =.5) +
  theme_bw()
#END

#trouble shooting an issue with the partial resids not matching the data..
# residuals are on the response and prediction on the link so we have to take the exponent of Pred but not pred
# could also just get the predictions on the response and not have to worry about it... 
gam <- gam(count ~ s(adj.yday, k = 20, bs = 'cr'),
            data = sub,
            family = tw(link = 'log'),
            method = 'REML')

sub %>% dplyr::select(adj.yday, count) %>% mutate(Pred = predict(gam, type = "response"),
                                           Res = resid(gam, type = "response"),
                                           Resid = Pred + Res) %>% 
  ggplot(aes(x = Resid, y = count)) +
  geom_point(alpha = .3)





