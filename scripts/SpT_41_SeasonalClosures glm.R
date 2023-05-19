source("scripts/SpT_00_packages.R")
load(file = "outputs/mod30.gam.spawning.peaks.RData")
load(file = "outputs/mod30.spawning.peaks.RData")

#deviance explained
em.gam.year %>% dplyr::select(summary.df) %>% unnest(summary.df)
#The explanory power of the GAMs if good

#We can visualise the spawning activities across years...
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
#...and see that the closures don't align well with the peak. 


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


## Are longer closures better?
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
#Longer closures are better but not more than what you would expect...




##What about the number, do we get better chance of landing on peak if we have more closures
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
    labs(y = "Spawning activity", x = "") +
    facet_wrap(~austral.year, ncol = 1) +
    theme_classic() + theme(strip.background = element_blank(),
                            strip.text = element_blank(),
                            axis.line = element_blank(),
                            panel.border = element_rect(fill = NA))
p1
#Seems to be working... 


## Integrating duration and frequency of closures into a model
#Getting the observations into percent, so we can calculate the proportion of spawning activity protected by each closures
#Adding an identifier for 5 and 9 day closures around the new moon in each year.
trout <- bind_rows(df.1, df.2) %>% ungroup() %>% droplevels() %>% 
  mutate(closure = ifelse(nine.day ==1, paste(austral.year, month, sep = "_"), NA)) %>% 
  #turn into percentage
  group_by(austral.year) %>% 
  mutate(season.sum = sum(response)) %>% 
  mutate(response = response / season.sum) %>% 
  #remove no closure days
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
         s.value = round(value,3))



dat.glm1 = trout %>% ungroup() %>% 
  group_by(duration, austral.year) %>% 
  mutate(n = 1:n(),
         count.dec = cumsum(value),
         #rounding for poisson...
         count = round(cumsum(value)*100,0),
         n = factor(n))

glm1 <- glmmTMB(count.dec ~ duration + n + (1|austral.year), data = dat.glm1, REML = TRUE, family = "gaussian")
glm2 <- glmmTMB(count.dec ~ duration * n + (1|austral.year), data = dat.glm1, REML = TRUE, family =  "gaussian")
glm3 <- glmmTMB(count ~ duration + n + (1|austral.year/n), data = dat.glm1, REML = TRUE, family = poisson())
glm4 <- glmmTMB(count ~ duration * n + (1|austral.year/n), data = dat.glm1, REML = TRUE, family = poisson())
glm5 <- glmmTMB(count.dec ~ duration + n + (1|austral.year), data = dat.glm1, REML = TRUE, family = beta_family())
glm6 <- glmmTMB(count.dec ~ duration * n + (1|austral.year), data = dat.glm1, REML = TRUE, family =  beta_family())
AIC(glm1, glm2, glm3, glm4, glm5, glm6)
summary(glm6) #interactions not important.

resid <- glm5 %>% simulateResiduals(plot = TRUE)
#Looks great

#Partial plots
glm5 %>% ggpredict(~duration|n) %>% plot(add.data = TRUE)
glm5 %>% emmeans(~duration|n, type = "response")  

glm5 %>% ggemmeans(~n|duration)  %>% plot
glm5 %>% emmeans(revpairwise~n|duration, type = "response")

glm5 %>% emmeans(revpairwise~duration, type = "response")


#Table
glm5 %>% sjPlot::tab_model(show.se = TRUE, show.aic = TRUE)

#plot
grid <- with(dat.glm1, list(duration = levels(duration),
                            n = levels(n)))
newdata = glm5 %>% emmeans(~n|duration, at = grid, type = "response") %>% 
  as.data.frame() %>% 
  mutate(duration = ifelse(duration == "sum5", "5-day closure", "9-day closure"))

p2 = ggplot(data = newdata, aes(y = response, x = n)) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL, col = duration), position = position_dodge(width = 0.4)) +
  geom_point(data = dat.glm1 %>% 
               mutate(duration = ifelse(duration == "sum5", "5-day closure", "9-day closure")), 
             aes(y = count.dec, x = n, col = duration), position = position_jitterdodge(dodge.width = 0.4, jitter.width = 0.1), alpha = .5) +
  scale_color_manual(values = c("red", "pink")) +
  scale_y_continuous(label = scales::percent) +
  labs(y = "Percent spawning\nactivity captured", x= "Number of seasonal closures") +
  theme_classic() +
  theme(legend.position = c(.18,.88),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")) +
  theme(axis.line = element_blank(),
        panel.border = element_rect(fill = NA))

p2

glm5 %>% emmeans(revpairwise~duration, type = "response")
glm5 %>% emmeans(revpairwise~duration|n, type = "response")
glm5 %>% emmeans(revpairwise~n, type = "response")


closure.glm = list(glm5, p1, p2, dat.glm1)
save(closure.glm, file = "outputs/mod41_closure.glm.RData")

Fig3 <- ggarrange(closure.glm[[2]], closure.glm[[3]], widths =c(1,2), labels = c("a)", "b)"), font.label = list(face = "plain", size = 11))
Fig3