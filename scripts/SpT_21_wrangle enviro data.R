## SSST data
library(tidyverse)
library(lubridate)

sst.1 <- read.csv("data/g4.areaAvgTimeSeries.MODISA_L3m_NSST_8d_4km_R2019_0_sst.20020704-20220601.150E_23S_150E_23S.csv", skip = 8)

sst.2 <- sst.1 %>% as.tibble() %>% 
  rename(sst = mean_MODISA_L3m_NSST_8d_4km_R2019_0_sst) %>% 
  mutate(date = ymd(as.Date(time))) %>% 
  filter(sst > 1,
         date > as.Date("2007-01-01")) %>% 
  dplyr::select(-time)


min(sst.2$date)
max(sst.2$date)
min(sst.2$sst)
max(sst.2$sst)

sst = tibble(date = seq(min(sst.2$date), max(sst.2$date), by = 1)) %>% 
  left_join(sst.2, by = "date") %>% 
  fill(sst)

save(sst, file = "outputs/dat21.sst.RData")




# Fitzroy river gauge
flood <- read.csv("data/csv.w00198.20220815123041.128/csv.w00198.20220815123041.128.csv", skip = 9)

flood <- flood %>% filter(!is.na(Value)) %>% 
  mutate(X.Timestamp = as.Date(X.Timestamp)) %>% 
  rename(date = X.Timestamp) %>% 
  filter(Value >= 1) %>% 
  rename(flood = Value)

head(flood)
ggplot(flood, aes(x = date, y = flood)) +
  geom_point()

save(flood, file = "outputs/dat21.flood.RData")


# Keppel Rainfall
rain <- read.csv("data/IDCJAC0009_033260_1800/IDCJAC0009_033260_1800_Data.csv")

rain = rain %>% filter(Year >= 2007) %>% 
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  rename(rainfall = Rainfall.amount..millimetres.) %>% 
  dplyr::select(date, rainfall)

head(rain)
ggplot(rain, aes(x = date, y = rainfall)) +
  geom_point()

save(rain, file = "outputs/dat21.rain.RData")

