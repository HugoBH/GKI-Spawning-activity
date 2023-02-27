## Analysis of spawning during the year

## Define Austral Seasons
# Spring is Sept, Oct and Nov 
# Summer is Dec, Jan and Feb
# Autumn is March, April and May
# Winter is June, July and August

source("scripts/SpT_00_packages.R")
load("outputs/dat20.spawning.times.RData")

keppels.spawn.time = keppels.spawn.time %>% 
  mutate(austral.year = ifelse(month >= 7, 
                               paste(year_spawn, year_spawn + 1,sep = "-"), 
                               paste(year_spawn -1, year_spawn,sep = "-")),
         day = yday(date_spawn),
         week = week(date_spawn),
         mday = format(date_spawn, format="%m-%d")) %>% 
  mutate(day = factor(day, levels = c(seq(yday(as.Date("31-06-2000")),366,1),
                                      seq(1,yday(as.Date("31-06-2000"))-1,1))),
         ymday = as.Date(mday, format="%m-%d"), #this to be allow us to plot.
         month = factor(month, levels = c(7,8,9,10,11,12,1,2,3,4,5,6)))

## Let's filter out some of the austral years where there is no data
keppels.spawn.time %>% group_by(austral.year) %>% 
  summarise(n = n()) %>% 
  filter(n <= 5)

keppels.spawn.time <- keppels.spawn.time %>% 
  filter(! austral.year %in% c("2006-2007", "2010-2011", "2012-2013","2019-2020")) %>% 
  droplevels()

ggplot(keppels.spawn.time, aes(x = date_spawn, fill = as.factor(month))) +
  geom_histogram(binwidth = 4) +
  geom_point(data = keppels.spawn.time, aes(y = 0, x = date_collect), col = "black", shape = 15, show.legend = F) +
  facet_grid( ~ austral.year, scales = "free_x", space = "free_x")+
  scale_x_date(breaks= date_breaks("4 months"), 
               minor_breaks = date_breaks("months"), labels=date_format("%b %y"))+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = expression(paste('Spawning date of' , italic (' P. maculatus'))), y ="Number of juveniles") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none")


## Prepare data for GAM fitting
## There's insufficient data to model on a daily basis but we may be able to sum across days to start with
## First, we need to include 0s for our detection point, not the whole year, only the period where we're able to detect recruits
## To do this, we will first get the count data per day and then sum across days as is necessary. 
## This way we can explore how to best group the data

## summarise counts by day
dat.gam1 <- keppels.spawn.time %>% ungroup() %>% droplevels() %>% 
  group_by(austral.year, date_spawn) %>% 
  summarise(count = n())

## find the periods where we're able to detect spawning
x <- dat.gam1 %>% 
  group_by(austral.year) %>% 
  summarise(count = sum(count),
            min = min(date_spawn),
            max = max(date_spawn))

## Create a full list of those days and merge with counts
dat.gam2 <- rbind(data.frame(austral.year = x$austral.year[1], date_spawn = seq(as.Date(x$min[1]),as.Date(x$max[1]),1)),
                 data.frame(austral.year = x$austral.year[2], date_spawn = seq(as.Date(x$min[2]),as.Date(x$max[2]),1)),
                 data.frame(austral.year = x$austral.year[3], date_spawn = seq(as.Date(x$min[3]),as.Date(x$max[3]),1)),
                 data.frame(austral.year = x$austral.year[4], date_spawn = seq(as.Date(x$min[4]),as.Date(x$max[4]),1)),
                 data.frame(austral.year = x$austral.year[5], date_spawn = seq(as.Date(x$min[5]),as.Date(x$max[5]),1))) %>% 
  left_join(dat.gam1, by = c("austral.year", "date_spawn")) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  as_tibble()

## not necessary but might be usefule later
## Date levels (inc. a leap year)
date.seq=c(seq(as.Date("2019-07-01"), as.Date("2019-12-31"),1), seq(as.Date("2020-01-01"), as.Date("2020-06-30"),1)) %>% 
  format(format="%m-%d")

## group data into xx day chunks
data.averaging = 5 #days
library(lunar)
load("outputs/dat21.flood.RData")
load("outputs/dat21.rain.RData")
dat.gam3 <- dat.gam2 %>% 
  arrange(austral.year, date_spawn) %>% 
  rename(date = date_spawn) %>% 
  left_join(flood %>% dplyr::select(date, flood), by = "date") %>% 
  left_join(rain, by = "date") %>% 
  group_by(austral.year) %>% 
  mutate(grouping = as.numeric(date-min(date)) %/% data.averaging) %>% 
  group_by(austral.year, grouping) %>% 
  summarise(date = min(date, na.rm = TRUE),
            count = sum(count, na.rm = TRUE),
            flood = mean(flood, na.rm = TRUE),
            rain = sum(rainfall, na.rm = TRUE)) %>% 
  mutate(illum = lunar.illumination.mean(date, shift = 10, towards = data.averaging), 
         phase = lunar.phase(date, shift = 10, name = 4)) 

## add SST data, flood and rainfall
load("outputs/dat21.sst.RData")
dat.gam <- dat.gam3 %>% 
  left_join(sst, by = "date") %>% 
  mutate(yday = yday(date),
         month = month(date),
         year = year(date),
         austral.year = factor(austral.year)) %>% 
  mutate(adj.yday = if_else(yday >=183, yday -182, yday + 182),
         num.year = as.numeric(austral.year),
         adj.mnth = if_else(month >= 7, month -6, month + 6)) %>% 
  ungroup() %>% droplevels() %>% 
  mutate(Dt.num=decimal_date(date)) %>% 
  mutate(sSST = scale(sst)) 

## Check we haven't lost anyone along the way
nrow(keppels.spawn.time)
sum(x$count)
sum(dat.gam$count)


ggplot(dat.gam, aes(y = count, x = date, col = flood)) +
  geom_point() +
  facet_wrap(~austral.year, ncol = 1, scale = "free") +
  scale_x_date(date_labels = "%b %y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  geom_smooth(method='gam', formula=y~s(x),
              method.args=list(family='poisson'))

save(dat.gam, file = "outputs/dat21.spawning_5d_avg.Rdata")




## floods

flood %>% filter(date >= as.Date("2011-07-01") & date <= as.Date("2012-07-01")) %>% 
  ggplot(aes(y = flood, x = date)) +
  geom_point()

## Lunar cycles

library(lunar)
lunar.phase(as.Date("2013-05-09"), shift = 10, name = 8)
lune = dat.gam2 %>% 
  mutate(illum = lunar.illumination.mean(date_spawn, shift = 10, towards = 5), 
         phase = lunar.phase(date_spawn, shift = 10, name = 4)) 

lunar.illumination.mean(as.Date("2013-05-09"), shift = 10,towards = 5)

lune %>%
  ggplot(aes(date_spawn, illum, col = phase)) + geom_point() +facet_wrap(~austral.year, scales = "free", ncol = 1)

#The number of hours by which to shift the calculation of lunar phase. By default lunar phase is calculated at 12 noon UT.
#Where radians are returned:
#• 0 refers to the new moon
#• π/2 refers to the first quarter 
#• π refers to the full moon
#• 3π/2 refers to the last quarter


#Solution to try to align these dates:
#https://stackoverflow.com/questions/69658051/plot-time-series-of-different-years-together
#https://stackoverflow.com/questions/63980973/align-time-series-ggplot

# ## Quick plot, with all dates aligned.
# dat.gam.plot = dat.gam %>%  
#   mutate(mday = format(date, format="%m-%d")) 
#   
# date.seq1 = date.seq[date.seq %in% dat.gam.plot$mday]
# 
# dat.gam.plot = dat.gam.plot %>% 
#   mutate(mday = factor(mday, levels = c(date.seq1))) %>% 
#   mutate(mday = as.Date(mday, format="%m-%d"))
# 
# head(dat.gam.plot) %>% 
#   mutate(mday = as.Date(mday, format="%m-%d"))
# 
# ggplot(dat.gam.plot, aes(y = count, x = mday)) +
#   geom_point() +
#   facet_wrap(~austral.year, ncol = 1) +
#   #scale_x_discrete(breaks = c("08-01", "09-01","10-01","11-01","12-01","01-01", "02-01", "03-01", "04-01", "05-01", "06-01")) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


