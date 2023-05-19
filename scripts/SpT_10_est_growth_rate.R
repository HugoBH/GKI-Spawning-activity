## Estimating growth rates for different cohorts to predict spawning time

## packages
source("scripts/SpT_00_packages.R")

load("outputs/dat01.keppels123_ageing.RData")

## we can see that the estimated settlement date suggest discrete cohorts
ageing123 %>% 
  #filter(KI_period == "KI3: 2020-2021") %>% 
  ggplot(aes(x = date_spawn)) +
  geom_histogram(binwidth = 7) +
  facet_grid(. ~ KI_period, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("3 months"), 
               minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()


# We don't want to assume the same growth rates for each of these cohorts. So let's model growth separately 
df = ageing123 %>% 
  mutate(cohort = case_when(
    (date_spawn >= as.Date("2007-09-01")) & (date_spawn <= as.Date("2008-03-31")) ~ "G1",
    (date_spawn >= as.Date("2008-08-01")) & (date_spawn <= as.Date("2009-03-31")) ~ "G2",
    (date_spawn >= as.Date("2011-12-01")) & (date_spawn <= as.Date("2012-07-31")) ~ "G3",
    (date_spawn >= as.Date("2020-10-01")) & (date_spawn <= as.Date("2021-01-31")) ~ "G4",
    (date_spawn >= as.Date("2021-02-01")) & (date_spawn <= as.Date("2021-08-15")) ~ "G5",
    (date_spawn >= as.Date("2021-09-01")) & (date_spawn <= as.Date("2022-05-31")) ~ "G6")) %>% 
  filter(tl <= 250)

## We can use these groupings to backculate the age of fish that were collected from the same periods
## We just need the collection dates of these fish rather than their spawn date.

## Groups used for ageing based on the collection date
ageing.cohorts = df %>% group_by(cohort) %>% 
  summarise(min.date = min(date_collect),
            max.date = max(date_collect)) %>% 
  filter(! is.na(cohort))

save(ageing.cohorts, file = "outputs/dat10.ageing.cohorts.RData")


p1 = ggplot(df, aes(x = date_spawn, col = cohort, fill = cohort)) +
  geom_histogram(binwidth = 7) +
  facet_grid(. ~ KI_period, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("3 months"), 
               minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Number of juveniles", x = "Spawning time") +
  theme_classic() 

p2 = ggplot(df, aes(y = age_post, x = tl, col = cohort)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~cohort) +
  labs(y = "Post-settlement age (days", x = "Total length (mm)") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")


# Some of these appear to be different. And some cohorts only have limited data.
# It would be great if we could use one growth curve for everyone but can we?
df = df %>% filter(! is.na(cohort)) %>% 
  mutate(cohort = factor(cohort))

# Model with varying intercept
m1 <- lmer(age_post ~ tl + (1|cohort), data = df)
df$random.intercpet.preds <- predict(m1)
ggplot(data=df, aes(x=tl, y=random.intercpet.preds, group = cohort, colour = cohort)) +
  geom_point(aes(x=tl, y=age_post), alpha = 0.2) +
  geom_line() 
coef(m1)

# Model with varying slope
m2 <- lmer(age_post ~ tl + (0 + tl|cohort), data=df)
df$random.slope.preds <- predict(m2)
ggplot(data=df, aes(x=tl, y=random.slope.preds, group = cohort, colour = cohort)) +
  geom_point(aes(x=tl, y=age_post), alpha = 0.2) +
  geom_line() 
coef(m2)

# Model with varying slope and intercept
m3 <- lmer(age_post ~ tl + (1 + tl|cohort), data=df)
df$random.slope.int.preds <- predict(m3)
ggplot(data=df, aes(x=tl, y=random.slope.int.preds, group = cohort, colour = cohort)) +
  geom_point(aes(x=tl, y=age_post), alpha = 0.2) +
  geom_line() 
coef(m3)

AIC(m1, m2, m3)
## m3 returned the best results

simulateResiduals(m1) %>% plot
## greater variance in higher age class than in lower tl. overall pretty good. 

## Marginalisation and dispersion factor to help deal with Heteroscedasticity
## Dispersion is associated by tl, and perhaps differently for each cohort
m4 <- glmmTMB(age_post ~ tl + (1|cohort), data = df, dispformula = ~ tl, REML = TRUE)
m5 <- glmmTMB(age_post ~ tl + (0 + tl|cohort), data=df, dispformula = ~ tl, REML = TRUE)
m6 <- glmmTMB(age_post ~ tl + (1 + tl|cohort), data=df, dispformula = ~ tl, REML = TRUE)
AIC(m4, m5, m6)

m7 <- glmmTMB(age_post ~ tl * cohort, data = df, dispformula = ~ tl, REML = TRUE)
m8 <- glmmTMB(age_post ~ tl * cohort, data=df, dispformula = ~ cohort, REML = TRUE)
m9 <- glmmTMB(age_post ~ tl * cohort, data=df, dispformula = ~ tl*cohort, REML = TRUE)

simulateResiduals(m9) %>% plot
AIC(m7, m8, m9)

emtrends(m9, ~cohort, var = "tl")
## m9 is a good model but it doesn't allow us to deal with data from unknown cohorts


## instead it would be better to have cohorts as a random effect so we can use the same model to predict the age of fish not in the other model 
m10 <- glmmTMB(age_post ~ tl + (1|cohort), dispformula = ~tl, data = df, REML = TRUE)
m11 <- glmmTMB(age_post ~ tl + (tl|cohort), dispformula = ~tl, data = df, REML = TRUE)
m12 <- glmmTMB(age_post ~ tl + (tl|cohort), dispformula = ~tl*cohort, data = df, REML = TRUE)
m13 <- glmmTMB(age_post ~ tl + (1|cohort), dispformula = ~tl*cohort, data = df, REML = TRUE)

simulateResiduals(m12) %>% plot
AIC(m9,m10,m11,m12,m13)

## m13 is not as good as m9 and DHARMa residuals still suggests an issue with Heteroscedasticity
hist(predict(m9, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")) /
predict(m13, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")))
## m13 is really overestimating the age of smaller individuals

## and in fact m12 is close to m9
hist(predict(m9, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")) /
       predict(m12, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")))

plot(predict(m9, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")) /
       predict(m12, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")),
     predict(m9, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")) /
       predict(m12, newdata = data.frame(tl = seq(1,200,1), cohort = "G1")))

## Can we improve with a polynomial
m14 <- glmmTMB(age_post ~ poly(tl,3) + (1|cohort), dispformula = ~tl*cohort, data = df, REML = TRUE)
m15 <- glmmTMB(age_post ~ poly(tl,2) + (1|cohort), dispformula = ~poly(tl,3)*cohort, data = df, REML = TRUE)

AIC(m9,m12,m14,m15)


## Growth model
growth.model = glmmTMB(age_post ~ poly(tl,3) + (tl|cohort), dispformula = ~tl*cohort, 
                       data = df, 
                       REML = TRUE)

AIC(m9,m12,m14,m15, growth.model)

resids <- simulateResiduals(growth.model)
plot(resids)
testDispersion(growth.model)
summary(growth.model)
#The residual vs predicted plots are not perfect. I suggest the accuracy of counts decreases with larger ages.
#That's difficult to deal with but the dispersion factor seems to help. 


save(growth.model, file = "outputs/mod10.growth.rates.RData")


#We can visualise the model.
ggpredict(growth.model, terms = "tl [all]") %>% plot (add.data = TRUE) 


grid = with(df, list(tl = seq(min(tl),250,1)))
growth.model %>% emmeans(~tl, at = grid, type ="response") %>% 
  as.data.frame() %>% 
  ggplot(aes(y = emmean, x = tl)) +
  geom_point(data = df , aes(y = age_post, x = tl, color = cohort), alpha = 0.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = .2) +
  geom_line() +
  labs(y = "Post-settlement age (days)", x = "Total length (mm)") +
  theme_light()

#I'm not sure how we can visualise the random effect, but these will be accounted for when predicting the age of juvenile fish.
ranef(growth.model)  


#If you want to see the curve for each cohort, you have to run a separate model
m16 <- glmmTMB(age_post ~ poly(tl,3) + cohort, data=df, dispformula = ~ tl*cohort, REML = F)

AIC(growth.model, m16)

grid = with(df, list(tl = seq(min(tl),250,1),
                     cohort = levels(cohort)))
m16 %>% emmeans(~tl|cohort, at = grid, type ="response") %>% 
  as.data.frame() %>% 
  ggplot(aes(y = emmean, x = tl)) +
  geom_point(data = df , aes(y = age_post, x = tl, color = cohort), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = cohort), alpha = .2) +
  geom_line(aes(col = cohort)) +
  labs(y = "Post-settlement age (days)", "Total length (mm)") +
  theme_light()

# growth model seems to be doing a decent job 
predict(growth.model, newdata = data.frame(tl = 100, cohort = "G2"))
predict(m16, newdata = data.frame(tl = 100, cohort = "G2"))
