library(readxl) #for data import
library(lubridate) #for working with dates (ie.graphing spawning times with date axis)
library(lme4) 
library(glmmTMB)
library(ggpubr)

library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects) #for plotting marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(DHARMa)    #for residual diagnostics plots
library(patchwork) #grid of plots
library(scales)    #for more scales

select = dplyr::select
summarise = dplyr::summarise
