#' data from the covid-19 data working group
#' @return dataframe with columns `province`, `health_region`, `Date`, `type`, `count`
#' `type` is "Cases", "Deaths", "Recovered". All are new daily cases
#' and Recovered is cumulative recovered cases.
#' @export
get_canada_covid_working_group_data <- function(){
    tmp=tempfile()
    download.file("https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=xlsx",tmp)
    cases<-readxl::read_xlsx(tmp,"Cases",skip=3) %>%
      mutate(Date=as.Date(date_report),count=1) %>%
      select(health_region,province,Date,count)
    deaths<-readxl::read_xlsx(tmp,"Mortality",skip=3) %>%
      mutate(Date=as.Date(date_death_report),count=1) %>%
      select(health_region,province,Date,count)
    recovered<-readxl::read_xlsx(tmp,"Recovered",skip=3) %>%
      filter(!is.na(cumulative_recovered)) %>%
      mutate(Date=as.Date(date_recovered),
             count=as.integer(cumulative_recovered)) %>%
      mutate(count=count-lag(count,order_by = Date,default = 0)) %>%
      select(province,Date,count)

    bind_rows(cases %>% mutate(type="Cases"),
              deaths %>% mutate(type="Deaths"),
              recovered %>% mutate(type="Recovered"))
}


provincial_recodes <- cansim:::short_prov.en
reverse_provincial_recodes <- setNames(names(provincial_recodes),as.vector(provincial_recodes))



#' data from the covid-19 data working group
#' @return dataframe with columns `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Deaths`, `Recovered`, `Active`, where all counts are cumulative,
#' @export
get_canada_covid_working_group_provincial_data <- function(){
  get_canada_covid_working_group_data() %>%
    mutate(count=coalesce(count,0)) %>%
    group_by(province,Date,type) %>%
    summarize(count=sum(count)) %>%
    pivot_wider(id_cols = c("province","Date"),names_from = type,values_from = count) %>%
    group_by(province) %>%
    arrange(Date) %>%
    fill(Recovered,.direction = "down") %>%
    mutate(Recovered=coalesce(Recovered,0),
           Cases=coalesce(Cases,0),
           Deaths=coalesce(Deaths,0)) %>%
    ungroup %>%
    bind_rows((.) %>%
                group_by(Date) %>%
                summarize_at(c("Deaths","Cases","Recovered"),sum) %>%
                mutate(province="Canada")) %>%
    group_by(province) %>%
    arrange(Date) %>%
    mutate(Deaths=cumsum(Deaths),Confirmed=cumsum(Cases),Recovered=cumsum(Recovered)) %>%
    ungroup() %>%
    mutate(Active=Confirmed-Deaths-Recovered) %>%
    mutate(Province=recode(province,!!!reverse_provincial_recodes),
           shortProvince=recode(province,!!!provincial_recodes)) %>%
    select(-province)
}


#' data from the covid-19 data working group
#' @return dataframe with columns `Health Region`, `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Deaths`, where all counts are cumulative,
#' @export
get_canada_covid_working_group_health_region_data <- function(start_cutoff,cache_key,refresh=FALSE){
  data <- get_canada_covid_working_group_data() %>%
    filter(type %in% c('Cases','Deaths')) %>%
    group_by(province,health_region,Date,type) %>%
    summarize(count=n()) %>%
    arrange(Date) %>%
    pivot_wider(id_cols = c("province",'health_region',"Date"),
                names_from = type, values_from = count) %>%
    ungroup() %>%
    fill(Cases,.direction = "down") %>%
    fill(Deaths,.direction = "down") %>%
    mutate(Cases=coalesce(Cases,0L), Deaths=coalesce(Deaths,0L)) %>%
    mutate(health_region=ifelse(health_region=="Not Reported",
                                paste0("Not Reported, ",province), health_region)) %>%
    group_by(health_region) %>%
    arrange(Date) %>%
    mutate(Deaths=cumsum(Deaths), Confirmed=cumsum(Cases)) %>%
    ungroup() %>%
    mutate(Province=recode(province,!!!reverse_provincial_recodes),
           shortProvince=recode(province,!!!provincial_recodes)) %>%
    select(`Health Region`=health_region,Province,shortProvince,Date,Confirmed,Deaths)
}

#' data from <a href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html">Canada.ca</a>
#' @return dataframe with columns `PR_UID`, `prname`, `prnameFR`, `shortProvince`, `Date`,
#' `Confirmed`, `Offical confirmed`, `Probable`, `Deaths`, `Cases`, `Tested` where `Cases` is new daily cases and
#' all other counts are cumulative, `Confirmed` is `Offical confirmed` plus `Probable` (postive tests which have not been confirmed at an official national or provincial lab).
#' Tested is currently not populated, but hopefully will return data soon.
#' @export
get_canada_official_provincial_data <- function(){
  simpleCache(readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"),
              "canada_official_data",tempdir()) %>%
    mutate(Date=as.Date(date,format="%d-%m-%Y")) %>%
    mutate(shortProvince=recode(prname,!!!provincial_recodes)) %>%
    select(PR_UID=pruid,prname,prnameFR,shortProvince,Date,
           Confirmed=numtotal,`Offical confirmed`=numconf,Probable=numprob,Deaths=numdeaths,Cases=numtoday,Tested=numtested)
}

#' data from UofS
#' @return dataframe with columns `Province`, `shortProvince`, `Health Region`, `Date`, `Age`,
#' `Travel history`, `Confirmed state`
#' @export
get_canada_UofS_case_data <- function() {
    r<-httr::GET("https://covid19tracker.ca/dist/api/controller/cases.php")
    data <- httr::content(r)$individualCases %>% purrr::map_df(as_tibble) %>%
    mutate(Date=as.Date(date)) %>%
    mutate(shortProvince=recode(province,!!!provincial_recodes)) %>%
    mutate(province=recode(province,"Repatriated Canadians"="Repatriated travellers")) %>%
    select(id,Date,Province=province,shortProvince,`Health region`=city,Age=age,
           `Travel history`=travel_history,`Confirmed state`=confirmed_presumptive)
}

#' data from UofS
#' @return dataframe with columns `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Offical confirmed`, `Probable`, `Cases`, where `Cases` is new daily cases and
#' all other counts are cumulative. `Confirmed` is `Offical confirmed` plus `Probable` (postive tests which have not been confirmed at an official national or provincial lab)
#' @export
get_canada_UofS_provincial_data <- function(){
  get_canada_UofS_case_data() %>%
    group_by(Date,Province,shortProvince,`Confirmed state`) %>%
    summarize(Cases=n()) %>%
    pivot_wider(id_cols=c("Date","Province","shortProvince"), names_from = `Confirmed state`, values_from = Cases) %>%
    mutate_at(c("CONFIRMED", "PRESUMPTIVE"),function(d)coalesce(d,0L)) %>%
    group_by(Province,shortProvince) %>%
    arrange(Date) %>%
    mutate(`Official confirmed`=cumsum(CONFIRMED),Probable=cumsum(PRESUMPTIVE)) %>%
    bind_rows((.) %>% group_by(Date) %>%
                summarize_at(c("Official confirmed","Probable"),sum) %>%
                mutate(Province="Canada",shortProvince="CAN")) %>%
    mutate(Confirmed=`Official confirmed`+Probable) %>%
    group_by(Province) %>%
    mutate(Cases=Confirmed-lag(Confirmed,order_by = Date,default = 0)) %>%
    ungroup %>%
    selectr(Province, shortProvince,Date,Confirmed,`Official confirmed`, Probable,Cases)
}

#' data from <a href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html">Canada.ca</a>
#' @return dataframe with columns `PR_UID`, `prname`, `prnameFR`, `shortProvince`, `Date`,
#' `Confirmed`, `Probable`, `Total`, `Deaths`, `Cases`, `Tested` where `Cases` is new daily cases and
#' all other counts are cumulative, `Total` is `Confirmed` pluis `Probable` (postive tests which have not been confirmed at an official national or provincial lab)
#' @export
get_canada_UofS_health_region_data <- function(){
  get_canada_UofS_case_data() %>%
    group_by(Date,health_region) %>%
    summarize(Cases=n()) %>%
    group_by(health_region) %>%
    arrange(Date) %>%
    mutate(Confirmed=cumsum(Cases))
}
