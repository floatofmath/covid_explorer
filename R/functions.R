## Helper Functions

## A ton of helper functions
filterEurope <- function(.data,...) filter(.data,Long > -10,Long <25,Lat >40,Lat<75)

filterCore <- function(.data,...) {
  filter(.data,`Country/Region` %in% c('Austria','Italy','Germany','France','Spain','United Kingdom','Switzerland','United States')) 
  } 

filterRecent <- function(.data,start = '2020-02-24',...) filter(.data,Date >= ymd(start))

filterAffected <- function(.data,...) {
  filter(.data,Confirmed > 0) %>% 
  group_by(`Country/Region`) %>% 
  mutate(length=n()) %>% 
  filter(length>1) %>% 
  dplyr::select(-length) %>% 
  ungroup
  }

filterLatest <- function(.data,lag=0) {
  group_by(.data,`Country/Region`,`Province/State`) %>% arrange(desc(Date)) %>% slice((1+lag):(1+lag))
}

removeChina <- function(.data,...) filter(.data,!str_detect(`Country/Region`,'China'))

collapseCountry <- function(.data,Country,lat,lon,new_name=Country,...) {
  filter(.data,`Country/Region`==Country) %>% 
  group_by(Date) %>% summarise(`Province/State`=NA,
                               `Country/Region`=new_name,
                               Lat = -33.9,
                               Long = 151,
                               Confirmed = sum(Confirmed,na.rm=T),
                               Recovered= sum(Recovered,na.rm=T),
                               Deaths = sum(Deaths,na.rm=T)) %>% ungroup %>% {bind_rows(.,filter(.data,`Country/Region`!=Country))}}

plot_world <- function(.data,variable,...){
  left_join(world,.data,by=c(name_long='Country/Region')) %>% 
  {tm_shape(.) + tm_polygons(col=variable,...)+
      tm_layout(legend.position=c('left','bottom'))}
}

plot_europe <- function(.data,variable,...){
  left_join(europe,.data,by=c(name_long='Country/Region')) %>% 
    {tm_shape(.,bbox = eu_bb) +tm_polygons(col=variable,...) + tm_layout(legend.position=c('left','top'))}
}

adt <- function(.rnk,.confirmed,period=5){
  ifelse(.rnk>period,period/log2(.confirmed/lag(.confirmed,period)),NA)
}

shift_country <- function(.data,country,days){
  .data %>% filter(`Country/Region`==country) %>% mutate(Date = Date - days) -> .replace
  .data[.data$`Country/Region`==country,] <- .replace
  .data#ifelse(`Country/Region` == country,Date - days,Date))
}

synchronize_confirmed <- function(.data,country,cut=50){
  first <- .data %>% group_by(`Country/Region`) %>% filter(Confirmed > cut) %>% summarize(first = min(doy))
  fc <- filter(first,`Country/Region`==country)$first
  first <- mutate(first,first=first - fc)
  for(i in 1:nrow(first)){
    .data <- shift_country(.data,first$`Country/Region`[i],first$`first`[i])
  }
  return(.data)
}

synchronize_deaths <- function(.data,country,cut=10){
  first <- .data %>% group_by(`Country/Region`) %>% filter(Deaths > cut) %>% summarize(first = min(doy))
  fc <- filter(first,`Country/Region`==country)$first
  first <- mutate(first,first=first - fc)
  for(i in 1:nrow(first)){
    .data <- shift_country(.data,first$`Country/Region`[i],first$`first`[i])
  }
  return(.data)
}

days_since <- function(.data,cut,variable){
  .variable <- enquo(variable)
  .data %>% group_by(`Country/Region`) %>% filter(!!.variable>=cut) %>% 
    mutate(days_since = doy - min(doy)) %>% ungroup()
}

country_lines <- function(.data,what,index = Date){
  index_ <- enquo(index)
  what_ <- enquo(what)
  ggplot(aes(!!index_,!!what_,group=`Country/Region`),data=.data) + geom_path(aes(col=`Country/Region`)) +
  geom_flag(aes(x=!!index_,y=!!what_,country=str_to_lower(iso_a2),hjust=0.5,vjust = 0),data=filterLatest(.data)) + theme_minimal()
}

# data_long %>% filterCore() %>% 
#   filterRecent() %>% country_lines(ADTD) + ylab('Average doubling time deaths')

