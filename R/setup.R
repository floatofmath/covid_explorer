## libraries
knitr::opts_chunk$set(echo = FALSE,message=FALSE,warning=FALSE,fig.width = 12,fig.height=8)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggmap)
library(sf)
library(raster)
library(dplyr)
library(spData)
#library(spDataLarge)
library(tmap)
library(ggflags)
source('R/functions.R')
library(survival)
library(cmprsk)
library(gridExtra)

## data import
if(dir.exists('./data/CSSE_COVID-19/')){#I locally clone the CSSE repository; if that's not available fetch the full files
csse_confirmed <- read_csv('data/CSSE_COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')#Stuff/covid/data/time_series_19-covid-Confirmed.csv")
csse_deaths <- read_csv('data/CSSE_COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
csse_recovered <- read_csv('data/CSSE_COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')
} else {
read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))#~/Projekte/Science 
read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))#~/Projekte/Science Stuff/covid/data/time_series_19-covid-Deaths.csv")
read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))#~/Projekte/Science Stuff/covid/data/time_series_19-covid-Recovered.csv")
}

## preprocessing
## make long
csse_confirmed %>% gather(Date,-(1:4),value = 'Confirmed') %>% mutate(Date = mdy(Date)) -> confirmed_long
csse_deaths %>% gather(Date,-(1:4),value = 'Deaths') %>% mutate(Date = mdy(Date)) -> deaths_long
csse_recovered %>% gather(Date,-(1:4),value = 'Recovered') %>% mutate(Date = mdy(Date)) -> recovered_long


## rename 
data_long <- left_join(left_join(confirmed_long,deaths_long),recovered_long)
## impute potentially missing values
data_long %<>% group_by(`Country/Region`,`Province/State`) %>%  arrange(Date) %>% tidyr::fill(Confirmed,Deaths,Recovered,.direction = 'down') %>% ungroup()

data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'^UK$','United Kingdom'))
data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'Czechia','Czech Republic'))
data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'North Macedonia','Macedonia'))
data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'North Macedonia','Macedonia'))
data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'Korea, South','Republic of Korea'))
data_long %<>% mutate(`Country/Region` = str_replace(`Country/Region`,'Russia','Russian Federation'))





data_long %<>% 
  collapseCountry('China',40.2,116) %>% 
  collapseCountry('US',38.9,-71.4,'United States') %>% 
  collapseCountry('Australia',-33.9,151) %>% 
  collapseCountry('France',46.2,2.21) %>% 
  collapseCountry('Canada',52.9,-73.5) %>% 
  collapseCountry('United Kingdom',55.4,-3.44)


## Estimators
data_long %<>% mutate(mlCFR = ifelse(Deaths+Recovered>0,Deaths/(Deaths+Recovered),NA),
                      naiveCFR = ifelse(Deaths>0,Deaths/Confirmed,NA))

data_long %<>% group_by(`Province/State`,`Country/Region`) %>% arrange(Date) %>% 
  mutate(Infections = c(0,diff(Confirmed)),
         Rate = ifelse(lead(Confirmed>0)&Confirmed>0,lead(Confirmed)/Confirmed,NA),
         Active = pmax(0,Confirmed - Deaths - Recovered),
         rnk = cumsum((Confirmed>0)),
         rnkd = cumsum((Deaths>0)),
         rnkl = cumsum((Active>0)),
         #ADT = ifelse(Confirmed>0,rnk/log2(pmax(2,Confirmed)/ifelse(rnk==1,Confirmed,1)),Inf),
         ADT = adt(rnk,Confirmed,7),
         ADTD = adt(rnkd,Deaths,5),
         ADTL = adt(rnkl,Active,5),
         DDeaths = c(0,diff(Deaths)),
         doy = as.numeric(Date - ymd('2020-01-01'))) %>% ungroup

