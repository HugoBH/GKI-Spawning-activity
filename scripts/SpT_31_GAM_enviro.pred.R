### GAM fitting
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
emm_options(rg.limit = 20000)

# load dat.gam
load("outputs/dat21.spawning_5d_avg.Rdata")


# check model fit for 1 year
sub <- dat.gam %>% filter(austral.year == "2008-2009") %>% droplevels()
gam.env1 <- gam(count ~ s(sst, k = 10, bs = 'cr') +
                s(adj.mnth, by = austral.year, bs = 'cc', k = 5) +
                s(illum, k = 10, bs = 'cc') + 
                flood +
                rain, 
                knots = list(illum = seq(0,1,length=10), adj.mnth=seq(1,12,length=5)),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env1)
#SST and month are capturing most of the variance, maybe a small effect of flood

gam.env2 <- gam(count ~ s(sst, k = 10, bs = 'cr') +
                s(adj.mnth, by = austral.year, bs = 'cc', k = 5) +
                #s(illum, k = 10, bs = 'cc') + 
                flood,
                #rain,
                knots = list(illum = seq(0,1,length=10), adj.mnth=seq(1,12,length=5)),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env2)
AIC(gam.env1, gam.env2)
#Simple model is best, is there an interaction?

gam.env3 <- gam(count ~  s(sst, adj.mnth, by = austral.year, k = 20),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env3)
AIC(gam.env1, gam.env2, gam.env3)
#interaction is important

gam.env4 <- gam(count ~  s(sst, adj.mnth, by = austral.year, k = 20) + s(flood),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env4)
AIC(gam.env1, gam.env2, gam.env3, gam.env4)

gam.env5 <- gam(count ~  s(sst, adj.mnth, by = austral.year, k = 20) + 
                  s(flood) + rain + s(illum, k = 10, bs = 'cc'),
                knots = list(illum = seq(0,1,length=10)),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env5)
AIC(gam.env1, gam.env2, gam.env3, gam.env4, gam.env5)
#lunar and rain are definitely not important
# go back to gam.env4

my.gam = gam.env4

k.check(my.gam)
resids <- simulateResiduals(my.gam,  plot=TRUE)
testZeroInflation(resids)
testDispersion(resids)
# we have no over dispersion or zero inflation
#draw(my.gam)
concurvity(my.gam)

#flood is correlated, 0.77
gam.env6 <- gam(count ~  s(sst, adj.mnth, by = austral.year, k = 20),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env6)
AIC(gam.env4, gam.env6)
# though the AIC scores are not as good, flood is simply soaking up variance and not informative. Particularly give that dat is sparse
# the interaction explain 70% of the variance
my.gam = gam.env6

#what about marginalising over years
gam.env7 <- gam(count ~  s(sst, adj.mnth, by = austral.year, k = 20) +
                  s(austral.year, bs = "re"),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
summary(gam.env7)
AIC(gam.env4, gam.env6, gam.env7)

gam.env8 <- gam(count ~  s(sst, illum, by = austral.year, k = 20) +
                  s(austral.year, bs = "re"),
                data = dat.gam,
                family = tw(link = 'log'),
                method = 'REML')
AIC(gam.env4, gam.env6, gam.env7, gam.env8)
summary(gam.env8)
#flood is really not important
draw(gam.env8)

#check individual years
gam.sub <- gam(count ~  s(sst, adj.mnth, k = 20),
                data = sub,
                family = tw(link = 'log'),
                method = 'REML')


#plot the interactions
grid = with(dat.gam, list(sst = modelr::seq_range(sst, n = 50),
                          adj.mnth = modelr::seq_range(adj.mnth, n = 50),
                          austral.year = levels(austral.year)))

newdata = emmeans(gam.env7, ~sst|adj.mnth|austral.year, at=grid, type='response') %>%
  as_tibble()

#Response
library(RColorBrewer)
pal = brewer.pal(n = 8, name = "RdBu")
ggplot(newdata, aes(y= sst, x= adj.mnth)) +
  geom_tile(aes(fill=response)) +
  geom_contour(aes(z=response), col = "black", bins = 10) +
  #scale_fill_gradientn(colors=heat.colors(10)) +
  #geom_point(data=dat.gam, aes(fill=count), shape=21, size=2, col = "white") +
  facet_grid(austral.year~.) +
  scale_fill_viridis_c() +
  #scale_fill_gradientn(colours = rev(pal)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1,4,7,10), labels = c("Jul", "Oct", "Jan", "Apr")) +
  scale_y_continuous(expand = c(0,0), breaks = seq(18,28,4)) +
  coord_fixed() +
  labs(y = "Sea surface temperature ºC", x = "", fill = "Spawning activity") +
  theme_classic() +
  theme(strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(fill = NA), 
        legend.position = "bottom",
        #legend.title = element_blank(),
        #plot.margin = margin(0,2,0,4, unit = "pt")
  ) +
  guides(fill = guide_colourbar(title.position = "bottom", barheight = .55, barwidth = 5, label.position = "top")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.title.align = 0.5,
        legend.direction = "horizontal",
        legend.margin = margin(0,.1,0,.5, unit = "mm"),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"))


save(gam.env7, file = "outputs/mo31.gam.enviro_v3.Rdata")
