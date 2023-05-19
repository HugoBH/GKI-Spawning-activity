## Merging measured age and estimated ages of juvenile fishes 

## packages
source("scripts/SpT_00_packages.R")

## load data and model 
load("outputs/dat01.keppels123_collection.RData")
load("outputs/dat01.keppels123_ageing.RData")
load("outputs/mod10.growth.rates.RData")
load("outputs/dat01.keppels123_PLD.RData")
load("outputs/dat10.ageing.cohorts.RData")
(average.PLD = mean(PLD$PLD, na.rm = TRUE))

## full merge data
full.merge = DB.KI %>% dplyr::select(id, year, date_collect,  KI_period, region, reef, fl, tl) %>% 
  full_join(ageing123 %>% dplyr::select(-species), 
            by = c("id", "date_collect", "KI_period", "region", "fl", "tl"))
## 13 individuals are above the 250 mm limit and should not be merged. We are not predicting above 250mm
ageing123 %>% filter(! id %in% DB.KI$id)

pred.df = DB.KI %>% dplyr::select(id, year, date_collect,  KI_period, region, reef, fl, tl) %>% 
  left_join(ageing123 %>% dplyr::select(-species) %>% mutate(ageing = "aged"), 
            by = c("id", "date_collect", "KI_period", "region", "fl", "tl")) %>% 
  mutate(year = as.factor(year),
         cohort = NA,
         ageing = if_else(is.na(ageing), "predicted", ageing),
         ageing = factor(ageing, levels = c("predicted", "aged"))) %>% 
  ## Growth rates were different for each cohort so for any unaged juveniles we can estimate their age based on aged individuals that were collected at the same time. 
  mutate(cohort = case_when(
    (date_collect >= ageing.cohorts$min.date[1]) & (date_collect <= ageing.cohorts$max.date[1]) ~ "G1",
    (date_collect >= ageing.cohorts$min.date[2]) & (date_collect <= ageing.cohorts$max.date[2]) ~ "G2",
    (date_collect >= ageing.cohorts$min.date[3]) & (date_collect <= ageing.cohorts$max.date[3]) ~ "G3",
    (date_collect >= ageing.cohorts$min.date[4]) & (date_collect <= ageing.cohorts$max.date[4]) ~ "G4",
    (date_collect >= ageing.cohorts$min.date[5]) & (date_collect <= ageing.cohorts$max.date[5]) ~ "G5",
    (date_collect >= ageing.cohorts$min.date[6]) & (date_collect <= ageing.cohorts$max.date[6]) ~ "G6"))
    
## We've aged 43% of the data
nrow(ageing123) / nrow(pred.df)
## thus predicting 65% of the data

## How well do we predict our own aged fish?  ie. predict 'age_post'
## First, assuring it's predicting different size for the different cohorts
predict(growth.model, newdata = data.frame(tl = 100, cohort = "G1"))
predict(growth.model, newdata = data.frame(tl = 100, cohort = "G2"))
predict(growth.model, newdata = data.frame(tl = 100, cohort = NA), re.form = ~0)

## Second, assuring cohort groupings have been assigned correctly
ggplot(pred.df, aes(x = date_collect, fill = cohort)) +
  geom_histogram(binwidth = 7) +
  facet_grid(ageing ~ KI_period, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("3 months"), minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

## Third, that predict will use cohort in pred.df
test = data.frame(tl = 100, cohort = c("G1", "G2", NA))
test$pred.c <- predict(growth.model, newdata = test)
test$pred.na <- predict(growth.model, newdata = test, re.form = ~0)

test %>% mutate(age_post.pred = if_else(is.na(cohort), pred.na, pred.c))


## Predict aged of unaged individuals
pred.df$pred.c <- predict(growth.model, newdata = pred.df)
pred.df$pred.na <- predict(growth.model, newdata = pred.df, re.form = ~0)

pred.df <- pred.df %>% mutate(age_post.pred = if_else(is.na(cohort), pred.na, pred.c)) %>% 
  dplyr::select(-c(pred.c, pred.na))

## it does a good job of predicting the age of our aged juveniles, which you would expect. 
## Those that it does not do a good job for at the ones we removed from the ageing analysis
## that's fine, we're going to use the measured age anyway.
ggplot(pred.df, aes(y = age_post, x = age_post.pred, col = cohort)) +
  geom_point(alpha = .3) +
  theme_classic()



keppels.spawn.time = pred.df %>% 
  # if we haven't measured the age, get the prediction
  mutate(age_post = if_else(is.na(age_post), age_post.pred, age_post),
         date_post = date_collect - age_post,
         age_tot = age_post + average.PLD,
         date_spawn = date_collect - age_tot,
         year_spawn = lubridate::year(date_spawn),
         date_spawn = ymd(date_spawn)) %>%
  mutate(month = as.numeric(format(date_spawn, "%m")),
         season = factor(case_when(month %in% c(12,1,2) ~ "Summer",
                                   month %in%  3:5  ~ "Autumn",
                                   month %in%  6:8  ~ "Winter",
                                   TRUE ~ "Spring")),
         season = factor(season, levels= c("Spring","Summer","Autumn","Winter",
                                           ordered = TRUE))) %>% 
  mutate(trip = factor(case_when(
    (date_collect >= as.Date("2008-04-30")) & (date_collect <= as.Date("2008-05-12")) ~ "Apr-2008",
    (date_collect >= as.Date("2009-02-19")) & (date_collect <= as.Date("2009-03-04")) ~ "Feb-2009",
    (date_collect >= as.Date("2012-04-26")) & (date_collect <= as.Date("2012-06-03")) ~ "Apr-2012",
    (date_collect >= as.Date("2013-05-21")) & (date_collect <= as.Date("2013-05-30")) ~ "May-2013",
    (date_collect >= as.Date("2013-08-07")) & (date_collect <= as.Date("2013-08-08")) ~ "Aug-2013",
    (date_collect >= as.Date("2021-03-26")) & (date_collect <= as.Date("2021-04-16")) ~ "Mar-2021",
    (date_collect >= as.Date("2021-10-09")) & (date_collect <= as.Date("2021-10-15	")) ~ "Oct-2021",
    (date_collect >= as.Date("2022-04-28")) & (date_collect <= as.Date("2022-05-06")) ~ "May-2022")))

save(keppels.spawn.time, file = "outputs/dat20.spawning.times.RData")

## aged vs predicted by date_spawn v1
ggplot(keppels.spawn.time, aes(x = date_spawn, fill = ageing)) +
  geom_histogram(binwidth = 7) +
  facet_grid(. ~ KI_period, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("1 months"), 
               minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = expression(paste('Spawning date of' , italic (' P. maculatus'))), y ="Number of juveniles") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank()) 

## aged vs predicted by date_spawn v2
ggplot(keppels.spawn.time, aes(x = date_spawn, fill = cohort)) +
  geom_histogram(binwidth = 7) +
  geom_point(data = keppels.spawn.time, aes(y = 40, x = date_collect), col = "black", shape = 15, show.legend = F) +
  facet_grid(ageing ~ KI_period, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("4 months"), 
               minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = expression(paste('Spawning date of' , italic (' P. maculatus'))), y ="Number of juveniles") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

