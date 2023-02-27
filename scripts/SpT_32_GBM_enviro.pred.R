#GBMlibrary(gbm)        
#for gradient boosted models
library(car)
library(dismo)
library(pdp)
library(ggfortify)
library(randomForest)
library(tidyverse)
library(gridExtra)
library(patchwork)
library(gbm)

load("SpawningTimes/outputs/dat21.spawning_5d_avg.Rdata")


# check it
str(dat.gam)
dat.gam %>% distinct(austral.year)
train.gbm = dat.gam %>% dplyr::select(count, sst, illum, rain, flood, austral.year) %>% 
  filter(austral.year %in% c("2007-2008", "2008-2009", "2011-2012")) %>% 
  ungroup() %>% droplevels()
pred.gbm = dat.gam %>% dplyr::select(count, sst, illum, rain, flood, austral.year) %>% 
  filter(austral.year %in% c("2020-2021", "2021-2022")) %>% 
  ungroup() %>% droplevels()

scatterplotMatrix(train.gbm)

# run it
#BOOSTED TREES (randomises rows) good for prediction
gbm1 = gbm(count ~ sst + illum + rain + flood + austral.year,
               data=train.gbm ,
               distribution = "poisson",
               #var.monotone = c(0,0,0,0, 0, 0),
               n.minobsinnode = 2,
               n.trees=10000,
               interaction.depth=3, ##!
               bag.fraction=0.5,
               shrinkage=0.001,
               train.fraction=1,
               cv.folds=3)


(best.iter = gbm.perf(gbm1,method='cv'))
# cross validation suggest that the total number of trees greatly exceeds the number necessary to achieve the optimum balance between bias and precision.
# it might be worth decreasing the learning rate.
# we can probably also reduce the total number of trees (so that it runs a little quicker)


summary(gbm1, n.trees=best.iter)


#devtools::install_github("koalaverse/vip")
terms = attr(gbm1$Terms,"term.labels")
vip::vip(gbm1) +
  geom_hline(aes(yintercept = 100 / length(terms)), lty = "dashed")



#partial effect of sst
gbm1 %>%
  pdp::partial(pred.var=c('sst'),
               n.trees=best.iter, recursive=FALSE, inv.link = exp) %>%
  autoplot()


nms <- attr(gbm1$Terms,"term.labels")
p <- vector('list', length(nms))
names(p) <- nms
for (nm in nms) {
  print(nm)
  p[[nm]] <- gbm1 %>% pdp::partial(pred.var=nm,
                                       n.trees=best.iter,
                                       inv.link=exp,
                                       recursive=FALSE,
                                       type='regression') %>%
    autoplot() +
    labs(y = "Predicted spawning") +
    ylim(0,10) # to ensure all plots on the same scale
}
patchwork::wrap_plots(p) & theme_bw()
#yhat is the predicted fish aboundance (predicted counds)

# you would normally do this on part of the data that you haven't used for training
acc <- pred.gbm[,-1] %>%
  bind_cols(Pred = predict(gbm1,
                           newdata=pred.gbm[,-1],
                           n.tree=best.iter,
                           type='response'))
cor(pred.gbm$count, acc$Pred)^2
#ideally you would do this on part of the data that you haven't included

acc %>%
  ggplot() +
  geom_point(aes(y=Pred,  x=pred.gbm$count))
#not very good

#Interactions
attr(gbm1$Terms,"term.labels")

#SST - year interaction
interact.gbm(gbm1, train.gbm[,-1],c(1,5), n.tree=best.iter)
#is this important? yes...
sst_year = gbm1 %>% pdp::partial(pred.var=c(2,6),  
                      n.trees=best.iter, 
                      recursive=FALSE) 
head(sst_year)
ggplot(sst_year, aes(x = sst, y = yhat, group = austral.year, col = austral.year)) +
  geom_line()







# GBM with bootstrap for confidence intervals

boot.gbm = dat.gam %>% dplyr::select(count, sst, month,  illum, rain, flood, austral.year) %>% 
  ungroup() %>% droplevels()

env <- boot.gbm[,-1]
fish <- boot.gbm[,1] %>% pull(count)

nBoot <- 20
fish.pred <- with(env,
                  expand.grid(sst = modelr::seq_range(sst, n = 100),
                              month = NA,
                              illum = NA,
                              rain = NA,
                              flood = NA,
                              austral.year = NA)
)
fish.list <- vector('list', nBoot) 
fish.list
fish.sum <- vector('list', nBoot) 
for (i in 1:nBoot) {
  print(paste0('Boot number: ', i))
  ## Create random set
  fish.rnd <- env %>%
    sample_n(size = n(), replace=TRUE)
  ## Fit the trees
  fish.gbm = gbm(fish ~ sst + month + illum + rain + flood + austral.year,
                 data=fish.rnd ,
                 distribution = "poisson",
                 #var.monotone = c(0,0,0,0, 0, 0),
                 n.minobsinnode = 2,
                 n.trees=10000,
                 interaction.depth=3, ##!
                 bag.fraction=0.5,
                 shrinkage=0.001,
                 train.fraction=1,
                 cv.folds=3)
  ## Determine the best number of trees
  (best.iter = gbm.perf(fish.gbm,method='cv'))
  ## predict based on shell weight
  fit <- predict(fish.gbm, newdata = fish.pred, n.trees = best.iter) %>% exp()
  fish.list[[i]] <- data.frame(fish.pred, Boot = i, Fit = fit)
  ## relative influence
  fish.sum[[i]] <- summary(fish.gbm, n.trees = best.iter)
}
fish.fit <- do.call('rbind', fish.list)
fish.fit1 <- fish.fit %>%
  group_by(sst) %>%
  ggdist::median_hdci(Fit, .width = .95)  %>% 
  mutate(.width = factor(.width))

g1 <- fish.fit1 %>% ggplot(aes(y=Fit, x=sst, group = .width)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper), alpha=0.3) +
  geom_line() +
  scale_fill_manual(values = "grey30") +
  #scale_fill_viridis_d() +
  #scale_colour_viridis_d() +
  theme_classic()

fish.inf <- do.call('rbind', fish.sum)
fish.inf <- fish.inf %>%
  group_by(var) %>%
  ggdist::median_hdci(rel.inf)       

terms = colnames(env)
g2 <- fish.inf %>% ggplot(aes(y=var, x=rel.inf)) +
  geom_vline(xintercept=100 / length(terms), linetype='dashed') +
  geom_pointrange(aes(xmin=.lower, xmax=.upper)) +
  theme_classic()

g2 + patchwork::inset_element(g1, left=0.5, bottom=0.01, right=1, top=0.7)
g1 + patchwork::inset_element(g2, left=0.5, bottom=0.01, right=1, top=0.5)


save(fish.inf, file = "SpawningTimes/outputs/plot33.GBM effects.RData")





