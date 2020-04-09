## Helper Functions

## A ton of helper functions

est_rec <- function(.data)
  .data %>%  group_by(Event) %>% arrange(time) %>% slice(n()-1) %>% 
  filter(Event == '1 Recovered') %$% round(est*100)

est_cfr <- function(.data)
  .data %>%  group_by(Event) %>% arrange(time) %>% slice(n()-1) %>% 
  filter(Event == '1 Deceased') %$% round(est*100)


## transform aggregate case and event data to individual times
to_time <- function(.data){
  .data %<>%  mutate(Recoveries = pmax(0,c(0,diff(Recovered))))
  ultimo <- .data %>% filterLatest() %$% Date
  mlCFR <- .data %>% filterLatest() %$% mlCFR
  naiveCFR <- .data %>% filterLatest() %$% naiveCFR
  naiveReR <- .data %>% filterLatest() %$% {Recovered/Confirmed}
  start <- .data %>% filter(Infections>0) %$% rep(Date,Infections)
  deaths <- .data %>% filter(Infections>0) %$% rep(Date,DDeaths)
  recoveries <- .data %>% filter(Infections>0) %$% rep(Date,Recoveries)
  ## Number of unresolved cases
  unresolved <- length(start)-length(deaths)-length(recoveries)
  ## Vector of possible status
  status <- rep(c('Deceased','Recovered','Unresolved'),c(length(deaths),
                                                         length(recoveries),
                                                         unresolved))
  return(tibble(start,
                end = c(deaths,recoveries,rep(ultimo,unresolved)),
                status,
                mlCFR,
                naiveCFR,
                naiveReR) %>% arrange(end))
}

exp_weights <- function(days,rate=.3){
  ## we add a little bit to deal allow censoring on the same day
  1-exp(-rate*days) + .01
}
#plot(1:20,exp_weights(1:20,rate=.3),type='l')
## resample eventtimes times consistence with incidence
## weights is a function of days in the past
resample_times <- function(start,end,weights=NULL,...){
  nx <- numeric()
  for(i in sort(unique(end))){
#    if(length(setdiff(xend[start <= i],nx))<sum(end==i)) browser()
    ix <- setdiff(which(start <= i),nx)
    if(!is.null(weights)){
      ws = weights(i-as.numeric(start[ix]),...)
      ws = ws/sum(ws)
      nnx <- sample(ix,sum(end==i),prob = ws)
    } else {  
      nnx <- sample(ix,sum(end==i))#,prob = weights)
    }
    nx <- c(nx,nnx)
#    if(any(end[xend]-start[nx]<0)) browser()
  }
  return(nx)
}


## ggplot for cuminc data
plot_cuminc <- function(.data){
  ggplot(.data,aes(time,est,col=Event))+geom_path()+
  # geom_hline(yintercept = CFR,col='red') +
  # geom_hline(yintercept = ReR,col='lightblue') +
  # annotate('text',x=60,y=ReR,label='Naive Recovery Rate',col='lightblue',vjust=1,hjust=1)+
  # annotate('text',x=60,y=CFR,label='Naive CFR',col='red',hjust=1,vjust=1)+
  # geom_hline(yintercept = mlCFR,col='red',lty=2) +
  # geom_hline(yintercept = 1-mlCFR,col='lightblue',lty=2) +
  # annotate('text',x=60,y=1-mlCFR,label='ML Recovery Rate',col='lightblue',vjust=1,hjust=1)+
  # annotate('text',x=60,y=mlCFR,label='ML CFR',col='red',hjust=1,vjust=1)+
  ylim(0,1)+xlim(0,60) + theme_minimal() + ylab('Cumulative Incidence') + xlab('Days since reported as case')
}

estimate_surv <- function(.data,country,cut_date=NULL,...){
  if(is.null(cut_date)) 
    cut_date <- .data %>% filter(`Country/Region`==country) %$% max(Date)
  eventtime <- .data %>% 
    filter(`Country/Region` == country) %>% 
    filter(Date <= cut_date) %>% 
    to_time
  ## somehow there are 5 negative recoveries on one day
  eventtime %>% mutate(res=NA) %>% arrange(end) %>% 
  mutate(res=resample_times(start,end,...),ix = order(end)) %$% 
#  ggplot(aes(end[ix]-start[res]))+geom_histogram() + xlim(0,30)
##   Surv(end[ix]-start[res],status[ix] !='Unresolved') %>% plot()
    cuminc(end-start[res],status,cencode='Unresolved') %>% cuminc2tibble
}


##transform cuminc output to tibble  
cuminc2tibble <- function(cuminc) {
  lapply(cuminc,as_tibble) %>% bind_rows(.id='Event')
}


#' Apply mutation only to rows matching condition
#'
#' Applies a mutation only to rows matching condition, returns a dataframe of same size as input dataframe. IMPORTANT: mutate_cond works only with existing columns, but is unable to add new columns.
#' @param .data dataframe
#' @param condition condition matching rows in .data
#' @param ... mutation expressions
#' @param envir
#'
#' @return dataframe of same size as .data
#' @export
#'
#' @examples
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
 
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

