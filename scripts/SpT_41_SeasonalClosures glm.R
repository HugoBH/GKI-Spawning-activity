source("scripts/SpT_00_packages.R")
load(file = "outputs/mod30.gam.spawning.peaks.RData")
load(file = "outputs/mod30.spawning.peaks.RData")

#deviance explained
em.gam.year %>% dplyr::select(summary.df) %>% unnest(summary.df)


closures.1 = read.csv(file = "data/Closure_dates.csv", header = T) %>% 
  mutate(DATE = as.Date(DATE, format = "%d/%m/%y"),
         year = year(DATE),
         month = month(DATE),
         yday = yday(DATE),
         austral.year = ifelse(month >= 7, 
                               paste(year, year + 1,sep = "-"), 
                               paste(year -1, year,sep = "-")),
         adj.yday = if_else(yday >=183, yday -182, yday + 182)) %>% 
  filter(year %in% c(2007, 2008, 2011, 2020, 2021)) %>% 
  dplyr::select(austral.year, month, adj.yday, Status) %>% 
  as_tibble() %>% 
  filter(! (month == 12 &  adj.yday %in% c(183, 184)))

closures <- em.gam.year %>% unnest(emmeans) %>% 
  dplyr::select(austral.year, response, adj.yday) %>% 
  left_join(closures.1, by = c("austral.year", "adj.yday")) 

ggplot(data = em.gam.year %>% unnest(emmeans), aes(y = response, x = adj.yday, label = austral.year)) +
  geom_area(data = closures %>% filter(Status == "closed"), 
            aes(y = 50, x = adj.yday, group = month), fill = "red", alpha = .2) +
  geom_area(data = closures %>% filter(Status == "closed"), 
            aes(y = response, x = adj.yday, group = month), fill = "red", alpha = .6) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) + geom_line() +
  geom_point(data = em.gam.year %>% unnest(c(newdata, presid)) %>% 
               mutate(Presid = exp(Pred) + Resid), 
             aes(y = Presid, x = adj.yday), alpha = .5) +
  geom_point(data = em.peaks, aes(y = response, x = adj.yday), 
             col = "black", pch = 23, fill = "white") +
  geom_text(data = with(em.gam.year, data.frame(austral.year = levels(austral.year), 
                                                adj.yday = 320, response = 35)), hjust = 0, size = 3) +
  scale_y_continuous(limits = c(NA,50), breaks = c(0,20,40)) +
  scale_x_continuous(breaks = c(1, 92,184,277), labels = c("Jul", "Oct", "Jan", "Apr")) +
  labs(y = "Successful spawning", x = "") +
  facet_wrap(~austral.year, ncol = 1) +
  theme_classic() + theme(strip.background = element_blank(),
                          strip.text = element_blank(),
                          axis.line = element_blank(),
                          panel.border = element_rect(fill = NA))



#Would 9-day closures be better than 5 day closures
dat.1 <- closures %>% filter(austral.year %in% c("2011-2012", "2020-2021", "2021-2022")) %>% 
  mutate(closure = ifelse(Status == "closed", 1, NA))

dat.1$closure[rep(which(dat.1$closure == 1), each=4L) + -2:2] <- 1


closures %>% filter(austral.year %in% c("2011-2012", "2020-2021", "2021-2022")) %>% 
  filter(month %in% c(10,11,12)) %>% 
  group_by(Status, austral.year) %>% 
  dplyr::summarise(sum = sum(response)) %>% 
  spread(key = Status, value = sum) %>% 
  mutate(percent = round(closed/open,3)*100)

dat.1 %>% 
  filter(month %in% c(10,11,12)) %>% 
  mutate(closure = ifelse(is.na(closure), "open", "closed")) %>% 
  group_by(closure, austral.year) %>% 
  dplyr::summarise(sum = sum(response)) %>% 
  spread(key = closure, value = sum) %>% 
  mutate(percent = round(closed/open,3)*100)
#seems like marginal gains and no more than expected


###############
#Compare the effect of 5 vs 9 day closures
df = closures %>% 
  filter(month %in% c(10,11,12)) %>% 
  group_by(austral.year, month, Status) %>% 
  mutate(n = ifelse(Status == "closed", 1:n(), 0))

#what would 5 day closures be?
df.1 <- df %>% filter(austral.year %in% c("2007-2008", "2008-2009")) %>% 
  mutate(legis = "Before",
         five.day = ifelse(n == 5, 1, NA))
df.1$five.day[rep(which(df.1$five.day == 1), each=5L) + -2:2] <- 1
df.1 <- df.1 %>% mutate(nine.day = ifelse(n >= 1, 1, NA))

#what would 9 day closures be?
df.2 = df %>% filter(austral.year %in% c("2011-2012", "2020-2021", "2021-2022")) %>% 
  mutate(legis = "After",
         five.day = ifelse(n >= 1, 1, NA),
         nine.day = ifelse(n == 3, 1, NA))
df.2$nine.day[rep(which(df.2$nine.day == 1), each=9L) + -4:4] <- 1

trout <- bind_rows(df.1, df.2) %>% ungroup() %>% droplevels() %>% 
  mutate(closure = ifelse(nine.day ==1, paste(austral.year, month, sep = "_"), NA)) %>% 
  filter(!is.na(closure)) %>% 
  mutate(five.day = ifelse(five.day ==1, response, NA),
         nine.day = ifelse(nine.day ==1, response, NA)) %>% 
  group_by(austral.year, closure, legis) %>% 
  summarise(sum5 = sum(five.day, na.rm = T),
            sum9 = sum(nine.day, na.rm = T)) %>% 
  gather(key = duration, value = value, -c(austral.year, closure, legis)) %>% 
  mutate(days = ifelse(duration == "sum5", 5, 9)) %>% 
  ungroup() %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(legis = factor(legis, levels = c("Before", "After")),
         s.value = round(value,0))
  
ggplot(trout, aes(y = value, x = austral.year, fill = duration)) +
  geom_boxplot() +
  facet_wrap(~legis, scale = "free_x")

ggplot(trout, aes(y = value, x = duration, group = closure, col = austral.year)) +
  geom_line() + geom_point() +
  facet_wrap(~legis, scale = "free_x")

#random slope for each closure, random intercept
#control = glmmTMBControl(optimizer = "optim",optArgs = "BFGS"))
#+ offset(days)
library(glmmTMB)
glm.g1 <- glmmTMB(value ~ duration + (1|closure), data = trout, REML = TRUE, family = "gaussian",
                  control = glmmTMBControl(optimizer = "optim",optArgs = "BFGS"))
glm.g2 <- glmmTMB(value ~ duration + legis + (1|closure), data = trout, REML = TRUE, family = "gaussian")
glm.g3 <- glmmTMB(value ~ duration * legis + (1|closure), data = trout, REML = TRUE, family = "gaussian")
glm.g4 <- glmmTMB(value ~ duration + austral.year + (1|closure), data = trout, REML = TRUE, family = "gaussian")
glm.g5 <- glmmTMB(value ~ duration * austral.year + (1|closure), data = trout, REML = TRUE, family = "gaussian")
AIC(glm.g1, glm.g2, glm.g3, glm.g4, glm.g5)

glm.p0 <- glmmTMB(s.value ~ duration + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p1 <- glmmTMB(s.value ~ duration + legis + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p2 <- glmmTMB(s.value ~ duration * legis + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p3 <- glmmTMB(s.value ~ duration + austral.year + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p4 <- glmmTMB(s.value ~ duration * austral.year + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
AIC(glm.p0, glm.p1, glm.p2, glm.p3, glm.p4)

glm.p5 <- glmmTMB(s.value ~ duration + (1|closure) + offset(log(days)), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p6 <- glmmTMB(s.value ~ 1 + offset(log(days)) + (1|closure), data = trout, REML = TRUE, family = poisson(link = "log"))
glm.p7 <- glmmTMB(s.value ~ 1 + offset(log(days)) + (1|austral.year/closure), data = trout, REML = TRUE, family = poisson(link = "log"),
                  control = glmmTMBControl(optimizer = "optim",optArgs = "BFGS"))
AIC(glm.p5, glm.p6, glm.p7)

my.glm = glm.p0
summary(my.glm)

#Model validation
resid <- my.glm %>% simulateResiduals(plot = TRUE)

#Partial plots
my.glm %>% ggpredict() %>%
  plot(add.data = TRUE)

my.glm %>% emmeans(revpairwise~duration, type = "response") 
#glm.p0 currently in use.



#What about the number, do we get better chance of landing on peak if we have more closures
df = closures %>% 
  filter(month %in% c(10,11,12)) %>% 
  group_by(austral.year, month, Status) %>% 
  mutate(n = ifelse(Status == "closed", 1:n(), 0))

#what would 5 day closures be?
df.1 <- df %>% filter(austral.year %in% c("2007-2008", "2008-2009")) %>% 
  mutate(legis = "Before",
         five.day = ifelse(n == 5, 1, NA))
df.1$five.day[rep(which(df.1$five.day == 1), each=5L) + -2:2] <- 1
df.1 <- df.1 %>% mutate(nine.day = ifelse(n >= 1, 1, NA))

#what would 9 day closures be?
df.2 = df %>% filter(austral.year %in% c("2011-2012", "2020-2021", "2021-2022")) %>% 
  mutate(legis = "After",
         five.day = ifelse(n == 3, 1, NA),
         nine.day = ifelse(n == 3, 1, NA)) %>% 
  #add a closure
  mutate(five.day = ifelse(austral.year == "2011-2012" & adj.yday == 175, 1, five.day),
         five.day = ifelse(austral.year == "2020-2021" & adj.yday == 166, 1, five.day),
         five.day = ifelse(austral.year == "2021-2022" & adj.yday == 155, 1, five.day)) %>% 
  mutate(nine.day = ifelse(austral.year == "2011-2012" & adj.yday == 175, 1, nine.day),
         nine.day = ifelse(austral.year == "2020-2021" & adj.yday == 166, 1, nine.day),
         nine.day = ifelse(austral.year == "2021-2022" & adj.yday == 155, 1, nine.day))

df.2$five.day[rep(which(df.2$five.day == 1), each=5L) + -2:2] <- 1
df.2$nine.day[rep(which(df.2$nine.day == 1), each=9L) + -4:4] <- 1

p1 = bind_rows(df.1, df.2) %>% ungroup() %>% droplevels() %>% 
  mutate(five.day = ifelse(five.day ==1, response, NA),
         nine.day = ifelse(nine.day ==1, response, NA)) %>% 
  ggplot() +
    geom_line(aes(y = response, x = adj.yday), size = 2) +
    geom_line(aes(y = nine.day, x = adj.yday), col = "pink", size = 2.1) +
    geom_line(aes(y = five.day, x = adj.yday), col = "red", size = 2.2) +
    facet_wrap(~austral.year, ncol = 1) + 
    scale_y_continuous(limits = c(NA,40), breaks = c(0,20,40)) +
    scale_x_continuous(breaks = c(92,123, 153, 184), labels = c("Oct", "Nov", "Dec", "Jan")) +
    labs(y = "Successful spawning", x = "") +
    facet_wrap(~austral.year, ncol = 1) +
    theme_classic() + theme(strip.background = element_blank(),
                            strip.text = element_blank(),
                            axis.line = element_blank(),
                            panel.border = element_rect(fill = NA))


trout <- bind_rows(df.1, df.2) %>% ungroup() %>% droplevels() %>% 
  mutate(closure = ifelse(nine.day ==1, paste(austral.year, month, sep = "_"), NA)) %>% 
  filter(!is.na(closure)) %>% 
  mutate(five.day = ifelse(five.day ==1, response, NA),
         nine.day = ifelse(nine.day ==1, response, NA)) %>% 
  group_by(austral.year, closure, legis) %>% 
  summarise(sum5 = sum(five.day, na.rm = T),
            sum9 = sum(nine.day, na.rm = T)) %>% 
  gather(key = duration, value = value, -c(austral.year, closure, legis)) %>% 
  mutate(days = ifelse(duration == "sum5", 5, 9)) %>% 
  ungroup() %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(legis = factor(legis, levels = c("Before", "After")),
         s.value = round(value,0))



##############
sum.5cv <- trout %>% group_by(austral.year) %>% 
  filter(duration == "sum5") %>% 
  mutate(n = 1:n()) %>% 
  select(austral.year, n, value) %>% 
  spread(key = n, value = value) %>% 
  mutate(var2 = var(c(`1`,`2`)),
         mean2 = mean(c(`1`,`2`)),
         se2 = sd(c(`1`,`2`))/ sqrt(2),
         sum2 = sum(c(`1`,`2`)),
         sum5.cv2 = sd(c(`1`,`2`))/ abs(mean(c(`1`,`2`)))) %>% 
  mutate(var3 = var(c(`1`,`2`,`3`)),
         mean3 = mean(c(`1`,`2`,`3`)),
         se3 = sd(c(`1`,`2`,`3`))/ sqrt(3),
         sum3 = sum(c(`1`,`2`,`3`)),
         sum5.cv3 = sd(c(`1`,`2`,`3`))/ abs(mean(c(`1`,`2`,`3`)))) 

glm1.dat<- sum.5cv %>% select(austral.year, sum2, sum3) %>% 
  gather(key = key, value = value, -austral.year)
glm1 <- glmmTMB(value ~ key + (1|austral.year), data = glm1.dat, REML = TRUE, family = "gaussian")
glm2 <- glmmTMB(round(value,0) ~ key + (1|austral.year), data = glm1.dat, REML = TRUE, family = poisson())
AIC(glm1, glm2)
resid <- glm1 %>% simulateResiduals(plot = TRUE)
glm1 %>% ggpredict() %>%plot(add.data = TRUE)
glm1 %>% emmeans(revpairwise~key, type = "response") 
#the third closure increases the protection of 

sum.9cv <- trout %>% group_by(austral.year) %>% 
  filter(duration == "sum9") %>% 
  mutate(n = 1:n()) %>% 
  select(austral.year, n, value) %>% 
  spread(key = n, value = value) %>% 
  mutate(var2 = var(c(`1`,`2`)),
         mean2 = mean(c(`1`,`2`)),
         sum2 = sum(c(`1`,`2`)),
         sum9.cv2 = sd(c(`1`,`2`))/ abs(mean(c(`1`,`2`)))) %>% 
  mutate(var3 = var(c(`1`,`2`,`3`)),
         mean3 = mean(c(`1`,`2`,`3`)),
         sum3 = sum(c(`1`,`2`,`3`)),
         sum9.cv3 = sd(c(`1`,`2`,`3`))/ abs(mean(c(`1`,`2`,`3`)))) 

glm1.dat<- sum.5cv %>% select(austral.year, sum2, sum3) %>% 
  gather(key = key, value = value, -austral.year)
glm1 <- glmmTMB(value ~ key + (1|austral.year), data = glm1.dat, REML = TRUE, family = gaussian)
resid <- glm1 %>% simulateResiduals(plot = TRUE)
#Partial plots
glm1 %>% ggpredict() %>%plot(add.data = TRUE)
glm1 %>% emmeans(revpairwise~key, type = "response") 

library(ggdist) #median hdci
sum.5cv %>% select(austral.year, sum5.cv2, sum5.cv3) %>% 
  left_join(sum.9cv %>% select(austral.year, sum9.cv2, sum9.cv3)) %>% 
  ungroup() %>% 
  select(-austral.year) %>% 
  gather() %>% group_by(key) %>% 
  summarise_all(median_hdci) %>% 
  separate(key, into = c("days", "n"), sep = "\\.") %>% 
  
  ggplot(aes(y = value$y, x = n, ymin= value$ymin, ymax = value$ymax))+
  geom_pointrange() +
  facet_wrap(~days) +
  labs(x = "",  y = "Volatility (meanCV ± 1SD)") +
  theme(legend.position = "none", plot.margin = unit(c(2,1,0,6), "mm"))

#mean and sd
sum.5cv %>% select(austral.year, sum5.cv2, sum5.cv3) %>% 
  left_join(sum.9cv %>% select(austral.year, sum9.cv2, sum9.cv3)) %>% 
  ungroup() %>% 
  select(-austral.year) %>% 
  gather() %>% group_by(key) %>% 
  group_by(key) %>% 
  summarise_all(list(mean = mean, sd = sd)) %>% 
  
  ggplot(aes(y = mean, x = key, ymin= mean -sd, ymax = mean+sd))+
  geom_pointrange()

sum.5cv %>% select(austral.year, var2, mean2, var3, mean3) %>% 
  ggplot() +
  geom_point(aes(y = var2, x = mean2), col = "red") +
  geom_point(aes(y = var3, x = mean3)) +
  facet_wrap(~austral.year)





# this is the bit that goes in the paper
dat.glm1 = trout %>% ungroup() %>% 
  group_by(duration, austral.year) %>% 
  mutate(n = 1:n(),
         count.dec = cumsum(value),
         count = round(cumsum(value),0),
         n = factor(n))
glm1 <- glmmTMB(count.dec ~ duration + n + (1|austral.year), data = dat.glm1, REML = TRUE, family = "gaussian")
glm2 <- glmmTMB(count.dec ~ duration * n + (1|austral.year), data = dat.glm1, REML = TRUE, family =  "gaussian")
glm3 <- glmmTMB(count ~ duration + n + (1|austral.year/n), data = dat.glm1, REML = TRUE, family = poisson())
glm4 <- glmmTMB(count ~ duration * n + (1|austral.year/n), data = dat.glm1, REML = TRUE, family = poisson())
AIC(glm1, glm2, glm3, glm4)

resid <- glm3 %>% simulateResiduals(plot = TRUE)
#Partial plots
glm3 %>% ggpredict(~duration|n) %>% plot(add.data = TRUE)
glm3 %>% emmeans(~duration|n, type = "response")  

glm3 %>% ggemmeans(~n|duration)  %>% plot
glm3 %>% emmeans(revpairwise~n|duration, type = "response")

glm3 %>% emmeans(revpairwise~duration, type = "response")


#Table
glm3 %>% sjPlot::tab_model(show.se = TRUE, show.aic = TRUE)

#plot
grid <- with(dat.glm1, list(duration = levels(duration),
                            n = levels(n)))
newdata = glm3 %>% emmeans(~n|duration, at = grid, type = "response") %>% 
  as.data.frame() %>% 
  mutate(duration = ifelse(duration == "sum5", "5-day closure", "9-day closure"))

p2 = ggplot(data = newdata, aes(y = rate, x = n)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL, col = duration), position = position_dodge(width = 0.4)) +
  geom_point(data = dat.glm1 %>% 
               mutate(duration = ifelse(duration == "sum5", "5-day closure", "9-day closure")), 
             aes(y = count.dec, x = n, col = duration), position = position_jitterdodge(dodge.width = 0.4, jitter.width = 0.1), alpha = .5) +
  scale_color_manual(values = c("red", "pink")) +
  labs(y = "Cumulative spawning activity", x= "Number of closures") +
  theme_classic() +
  theme(legend.position = c(.15,.85),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")) +
  theme(axis.line = element_blank(),
      panel.border = element_rect(fill = NA))

closure.glm = list(glm3, p1,p2, dat.glm1)
save(closure.glm, file = "outputs/mod42_closure.glm.RData")

#total spawning average
closures %>% 
  filter(month %in% c(10,11,12)) %>% 
  group_by(austral.year) %>% 
  summarise(sum = sum(response)) %>% 
  summarise(mean = mean(sum))





