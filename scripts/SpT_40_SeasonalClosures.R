#Spawning closures
load(file = "outputs/dat20.spawning.times.RData") # Load the estimated spawn date df -> may need to run the diw.Rmd again 

closures = read.csv(file = "data/Closure_dates.csv", header = T) %>% 
  mutate(DATE = as.Date(DATE, format = "%d/%m/%y")) %>% rename(set_date = DATE) %>% 
  mutate(n.date = as.numeric(set_date))

#Merge data with Spawning Closure Status
closure.1 = keppels.spawn.time %>%
  #filter(!cohort == "C0") %>% 
  mutate(n.date = floor(as.numeric(date_spawn))) %>%
  left_join(closures, by = c("n.date")) %>% 
  dplyr::select(- set_date, -n.date) %>%
  #To plot closures
  mutate(DayMonth = format(date_spawn, format="%m-%d"),
         DayMonth = as.Date(DayMonth, format="%m-%d")) %>% 
  arrange(year_spawn, DayMonth)

#Overall, 6.7% of fish spawned inside spawning closures. However, we did not sample every cohort every year so this is may be misleading. All closures occur within a 3 month window, what proportion of fish spawned inside the spawning closures in each year?
  
Seas.Closures = closure.1 %>% filter(year_spawn %in% c(2007, 2008, 2011, 2020))
segments = closures %>%  filter(Status == "closed") %>%
  mutate(year_spawn = year(set_date),
         DayMonth = format(set_date, format="%m-%d"),
         DayMonth = as.Date(DayMonth, format="%m-%d")) %>% 
  filter(year_spawn %in% c(2007, 2008, 2011, 2020))


