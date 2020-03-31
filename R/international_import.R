#' Import ecdc case data
#' @return dataframe with columns `Country`, `shortCountry`, `countryCode`, `Population 2018`, `Date`,
#' `Confirmed`, `Deaths`, `Cases`, where `Cases` is daily confirmed case count increase and
#' `Confirmed` and `Deaths` are cumulative
#' @export
get_ecdc_data <- function(){
  today=Sys.Date()
  tmp=tempfile()
  tryCatch({
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(today, "%Y-%m-%d"), ".xlsx", sep = "")
    download.file(url,tmp)
  }, error=function(cond){
    today=today %m+% days(-1)
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(today, "%Y-%m-%d"), ".xlsx", sep = "")
    download.file(url,tmp)
  })
  readxl::read_excel(tmp) %>%
    mutate(Date=as.Date(dateRep)) %>%
    rename(Country=countriesAndTerritories) %>%
    mutate(Country=recode(Country,"CANADA"="Canada")) %>%
    mutate(Country=gsub("_"," ",Country))  %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Confirmed=cumsum(cases),CumulativeDeaths=cumsum(deaths)) %>%
    ungroup %>%
    mutate(Country=recode(Country,
                          "United States of America"="USA",
                          "United Kingdom"="UK",
                          "Czech Republic"="Czechia")) %>%
    select(Country=Country,shortCountry=geoId,countryCode=countryterritoryCode,`Population 2018`=popData2018,
           Date,Confirmed,Deaths=CumulativeDeaths,Cases=cases)
}
#' Import jhs case data
#' @export
get_jhs_data <- function(){
  cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                    col_types = cols("Province/State"="c","Country/Region"="c",.default = "d")) %>%
    rename(Country=`Country/Region`,Province=`Province/State`) %>%
    pivot_longer(-one_of("Province","Country",   "Lat",   "Long"),
                 names_to = "Date",values_to = "Confirmed") %>%
    mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
    mutate(Country=ifelse(Province %in% c("Diamond Princess","Grand Princess"),Province,Country)) %>%
    mutate(Country=recode(Country, !!!jhs_country_recodes))
  deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                     col_types = cols("Province/State"="c","Country/Region"="c",.default = "d")) %>%
    rename(Country=`Country/Region`,Province=`Province/State`) %>%
    pivot_longer(-one_of("Province","Country",   "Lat",   "Long"),
                 names_to = "Date",values_to = "Deaths") %>%
    mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
    mutate(Country=ifelse(Province %in% c("Diamond Princess","Grand Princess"),Province,Country)) %>%
    mutate(Country=recode(Country, !!!jhs_country_recodes)) %>%
    select(Country,Province,Date,Deaths)
  recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                        col_types = cols("Province/State"="c","Country/Region"="c",.default = "d")) %>%
    rename(Country=`Country/Region`,Province=`Province/State`) %>%
    pivot_longer(-one_of("Province","Country",   "Lat",   "Long"),
                 names_to = "Date",values_to = "Recovered") %>%
    mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
    mutate(Country=ifelse(Province %in% c("Diamond Princess","Grand Princess"),Province,Country)) %>%
    mutate(Country=recode(Country, !!!jhs_country_recodes))  %>%
    select(Country,Province,Date,Recovered)

  jhs_data <- full_join(cases,deaths,by=c("Province","Country","Date")) %>%
    full_join(recovered,by=c("Province","Country","Date")) %>%
    mutate(Date=as.Date(Date)) %>%
    group_by(Province,Country,Date) %>%
    summarize_all(sum) %>%
    fill(Confirmed, .direction = "down") %>%
    fill(Deaths, .direction = "down") %>%
    fill(Recovered, .direction = "down") %>%
    mutate_at(c("Confirmed","Deaths","Recovered"),function(d) coalesce(d,0))
}


#' Import country level data, merge ecdc and jhs timelines
#' @return dataframe with columns `Country`, `Date`,
#' `Deaths`, `Recovered`, `Active`, `Cases`, Cases is daily new cases, others are cumulative
#' @export
get_country_timeline_ecdc_jhs_data <- function(){
  ecdc_data <- simpleCache({
    get_ecdc_data()
  },"static_ecdc_data_snapshot",path=tempdir(),refresh=FALSE)


  jhs_data <- get_jhs_data() %>%
    group_by(Country,Date) %>%
    summarize_at(c("Confirmed","Deaths","Recovered"),sum) %>%
    ungroup

  bind_rows(ecdc_data %>%
                 filter(Date<=min(jhs_data$Date)) %>%
                 mutate(Recovered=0) %>%
                 select(names(jhs_data)),
               jhs_data) %>%
    mutate(Active=Confirmed-Deaths-Recovered) %>%
    group_by(Country) %>%
    mutate(Cases=Confirmed-lag(Confirmed,order_by = Date,default = 0)) %>%
    ungroup
}

#' @import dplyr
#' @import tidyr
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom rlang .data
#' @importFrom utils download.file
#' @importFrom lubridate %m+%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


