library(readxl) #for data import
library(tidyverse) #for tidying and organizing data
library(lubridate) #for working with dates (ie.graphing spawning times with date axis)

## Collection data
keppels12 <- read.table(file = "data/Plectropomus database_Final_GKI-Drane.txt", sep = "\t", header = TRUE) %>% 
  filter(REGION == "Keppel Islands") %>% 
  dplyr::select(ID, INDIVIDUAL_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE, "%d/%m/%y"),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor) %>% 
  rename(INDIVID_ID = INDIVIDUAL_ID)

## Keppels 2021-2022
keppels3.Ad <-  read_xlsx('data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 1) %>% 
    dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL)*10,  #converting to mm
         TL = as.numeric(TL)*10) %>%  
  mutate_if(is.character, as.factor)

keppels3.Juv <-  read_xlsx('data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 2) %>% 
  dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)

keppels123 <- keppels12 %>% bind_rows(keppels3.Ad) %>% bind_rows(keppels3.Juv) %>% 
  ungroup() %>% droplevels()



#Establish relationship between FL and TL for juvenile fish (<250)
ggplot(keppels123  %>% filter(TL <=250), aes(TL, FL)) +
  geom_smooth(method = "lm",lty = "dashed") +
  geom_point(alpha = .1)
  
fltl.lm = lm(FL ~ TL, keppels123 %>% filter(TL <=250))
coef(fltl.lm) #FL = 1.40 + 0.95TL
tlfl.lm = lm(TL ~ FL, keppels123 %>% filter(TL <=250))
coef(tlfl.lm) #TL = -1.47 + 1.06FL



DB.KI <- keppels123 %>% 
  filter(TL <= 250, DATE >= as.Date("2007-06-01")) %>% 
  mutate(FL = coalesce(FL, (coef(fltl.lm)[1]) + TL*coef(fltl.lm)[2]),
         TL = coalesce(TL, (coef(tlfl.lm)[1]) + FL*coef(tlfl.lm)[2])) %>% 
  filter(LIFE_STAGE == "Juvenile" | LIFE_STAGE == "" & TL <= 250) %>%   
  rename(date_collect = DATE) %>% 
  mutate(year = year(date_collect),
         KI_period = factor(case_when(
          (date_collect >= as.Date("2005-01-01")) & (date_collect <= as.Date("2009-12-31")) ~ "KI1: 2007-2009",
          (date_collect >= as.Date("2010-01-01")) & (date_collect <= as.Date("2014-12-31")) ~ "KI2: 2011-2013",
          (date_collect >= as.Date("2019-01-01")) & (date_collect <= as.Date("2022-12-31")) ~ "KI3: 2020-2022"))) %>% 
  rename(id = INDIVID_ID,
         region = REGION,
         reef = REEF,
         fl = FL,
         tl = TL)






## Ageing data

## Keppels 2006-13 
juv_data <-  read_xlsx('data/Plect_2006-13_COMBINED DATA_HBH_updated.xlsx') # Keppels 1 & 2

keppels_hist<- juv_data %>% 
  dplyr::select(INDIVIDUAL_ID, Age_days, REGION.y, DATE, FL.y, TL.y, STR_SPECIES) %>%
  mutate(id = factor(INDIVIDUAL_ID),
         species = factor(STR_SPECIES), .keep = 'none',
         region = factor(REGION.y),
         date = date(as.Date(DATE, origin = "1899-12-30")),
         fl = coalesce(FL.y, (coef(fltl.lm)[1]) + TL.y*coef(fltl.lm)[2]),
         tl = coalesce(TL.y, (coef(tlfl.lm)[1]) + FL.y*coef(tlfl.lm)[2]),
         age_post = Age_days) %>%           # Age post settlement
  filter(region == 'Keppel Islands') %>%
  filter(species %in% c("P. maculatus")) %>% # Filters out 8 x non- Pmac entries
  filter(!is.na(age_post)) # Gets rid of NA values from age (we need ages for this part of the study)

## Keppels 2021-2022
keppels3.1 <-  read_xlsx('data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 2) # Keppels 3

keppels_2021<- keppels3.1 %>% 
  dplyr::select(INDIVID_ID, Mean_Count, REGION, DATE, FL, TL, SPECIES) %>%
  mutate(id = factor(INDIVID_ID), 
         species = factor(SPECIES), .keep='none',
         region = factor(REGION),
         date = date(DATE),
         fl = coalesce(FL, (coef(fltl.lm)[1]) + TL*coef(fltl.lm)[2]),
         tl = coalesce(TL, (coef(tlfl.lm)[1]) + FL*coef(tlfl.lm)[2]),
         age_post = Mean_Count) %>%         # Age post settlement
  filter(!is.na(age_post))


#PLD data
## PLD by Lachlan were honed in so that PLD3 is the best count. PLD1 were the first ever reads and are likely to be wrong. Do not average.
BMT_pld = read_excel("data/GBR Plectropomus age samples_BMT.xlsx")

PLD = bind_rows(
  
  BMT_pld %>% filter(REGION == "Keppels") %>% 
    filter(SPECIES == "Pmac") %>% 
    filter(! is.na(PLD)) %>% 
    dplyr::select(REGION, PLD),
    
  keppels3.1 %>% 
    filter(! is.na(`PLD 3`)) %>% 
    dplyr::select(REGION, `PLD 3`) %>% 
    rename(PLD = `PLD 3`))

(average.PLD = mean(PLD$PLD, na.rm = TRUE))
sd(PLD$PLD, na.rm = TRUE)

## Combine together and estimate settlement and spawn date based on age.
ageing123 <- bind_rows(keppels_hist, keppels_2021) %>% 
  rename(date_collect = date) %>% 
  filter(!is.na(fl),
         ! tl > 250) %>%                                                      # Get rid of any remaining NA values in fork length (there were 2 in there)
  ## Add in a column for sampling period
  mutate(KI_period = factor(case_when(
    (date_collect >= as.Date("2005-01-01")) & (date_collect <= as.Date("2009-12-31")) ~ "KI1: 2007-2009",
    (date_collect >= as.Date("2010-01-01")) & (date_collect <= as.Date("2014-12-31")) ~ "KI2: 2011-2013",
    (date_collect >= as.Date("2019-01-01")) & (date_collect <= as.Date("2022-12-31")) ~ "KI3: 2020-2022"))) %>% 
  ## Remove data from pre-Keppels
  filter(! date_collect < as.Date("2008-01-01")) %>% 
  mutate(date_post = date_collect - age_post,
         age_tot = age_post + average.PLD,
         date_spawn = date_collect - age_tot,
         year_spawn = lubridate::year(date_spawn))   # Extract spawning year from spawning data (for binning later)
  
  

## Save for other analyses (GLM etc)
save(ageing123, file = "outputs/dat01.keppels123_ageing.RData")
save(PLD, file = "outputs/dat01.keppels123_PLD.RData")
save(DB.KI, file = "outputs/dat01.keppels123_collection.RData")
