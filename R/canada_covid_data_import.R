get_canada_covid_working_group_cases <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/cases.csv") %>%
    mutate(Date=as.Date(date_report,format="%d-%m-%Y"),count=1) %>%
    select(health_region,province,Date,count)
}
get_canada_covid_working_group_deaths <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/mortality.csv") %>%
    mutate(Date=as.Date(date_death_report,format="%d-%m-%Y"),count=1) %>%
    select(health_region,province,Date,count)
}
get_canada_covid_working_group_recovered <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/recovered_cumulative.csv") %>%
    filter(!is.na(cumulative_recovered)) %>%
    mutate(Date=as.Date(date_recovered,format="%d-%m-%Y"),
           cr=as.integer(cumulative_recovered)) %>%
    group_by(province) %>%
    mutate(Recovered=cr-lag(cr,order_by = Date,default = 0)) %>%
    select(province,Date,count=Recovered) %>%
    ungroup
}

#' data from the covid-19 data working group (https://github.com/ishaberry/Covid19Canada)
#' @return dataframe with columns `province`, `health_region`, `Date`, `type`, `count`
#' `type` is "Cases", "Deaths", "Recovered". All are new daily cases
#' and Recovered is cumulative recovered cases.
#' @export
get_canada_covid_working_group_data <- function(){
    bind_rows(get_canada_covid_working_group_cases() %>% mutate(type="Cases"),
              get_canada_covid_working_group_deaths() %>% mutate(type="Deaths"),
              get_canada_covid_working_group_recovered() %>% mutate(type="Recovered"))
}


provincial_recodes <- c(cansim:::short_prov.en,
                        "Prince Edward Island"="PEI", # add non-standard province codes for Working Group data
                        "Northwest Territories"="NWT",
                        "Repatriated"="Repatriated")
reverse_provincial_recodes <- setNames(names(provincial_recodes),as.vector(provincial_recodes))



#' data from the covid-19 data working group (https://github.com/ishaberry/Covid19Canada)
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
           shortProvince=recode(Province,!!!provincial_recodes)) %>%
    select(-province)
}


#' data from the covid-19 data working group (https://github.com/ishaberry/Covid19Canada)
#' @return dataframe with columns `Health Region`, `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Deaths`, where all counts are cumulative,
#' @export
get_canada_covid_working_group_health_region_data <- function(start_cutoff,cache_key,refresh=FALSE){
  data <- get_canada_covid_working_group_data() %>%
    mutate(count=coalesce(count,0)) %>%
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
    select(`Health Region`=health_region,Province,shortProvince,Date,Confirmed,Deaths,Cases)
}

#' data from <a href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html">Canada.ca</a>
#' @return dataframe with columns `PR_UID`, `prname`, `prnameFR`, `shortProvince`, `Date`,
#' `Confirmed`, `Offical confirmed`, `Probable`, `Deaths`, `Cases`, `Tested` where `Cases` is new daily cases and
#' all other counts are cumulative, `Confirmed` is `Offical confirmed` plus `Probable` (postive tests which have not been confirmed at an official national or provincial lab).
#' Tested is currently not populated, but hopefully will return data soon.
#' @export
get_canada_official_provincial_data <- function(){
  readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
    mutate(Date=as.Date(date,format="%d-%m-%Y")) %>%
    mutate(prname=recode(prname,"Repatriated Travellers"="Repatriated",
                         "Repatriated travellers"="Repatriated")) %>%
    mutate(shortProvince=recode(prname,!!!provincial_recodes)) %>%
    select(PR_UID=pruid,prname,prnameFR,shortProvince,Date,
           Confirmed=numtotal,`Offical confirmed`=numconf,Probable=numprob,Deaths=numdeaths,Cases=numtoday,Tested=numtested)
}

#' data from UofS (https://covid19tracker.ca/)
#' @return dataframe with columns `Province`, `shortProvince`, `Health Region`, `Date`, `Age`,
#' `Travel history`, `Confirmed state`
#' @export
get_canada_UofS_case_data <- function() {
  r<-httr::GET("https://covid19tracker.ca/dist/api/controller/cases.php")
  data <- httr::content(r)$individualCases %>% purrr::map_df(as_tibble) %>%
    mutate(Date=as.Date(date)) %>%
    mutate(province=recode(province,"Repatriated Canadians"="Repatriated")) %>%
    mutate(shortProvince=recode(province,!!!provincial_recodes)) %>%
    select(id,Date,Province=province,shortProvince,`Health region`=city,Age=age,
           `Travel history`=travel_history,`Confirmed state`=confirmed_presumptive)
}

#' data from UofS (https://covid19tracker.ca/)
#' @return dataframe with columns `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Offical confirmed`, `Probable`, `Cases`, where `Cases` is new daily cases and
#' all other counts are cumulative. `Confirmed` is `Offical confirmed` plus `Probable` (postive tests which have not been confirmed at an official national or provincial lab)
#' @export
get_canada_UofS_provincial_data <- function(){
  get_canada_UofS_case_data() %>%
    group_by(Date,Province,shortProvince,`Confirmed state`) %>%
    summarize(Cases=n()) %>%
    bind_rows(group_by(.,Date,`Confirmed state`) %>%summarise(Cases=sum(Cases)) %>% mutate(Province="Canada",shortProvince="CAN")) %>%
    ungroup %>%
    pivot_wider(id_cols=c("Date","Province","shortProvince"), names_from = `Confirmed state`, values_from = Cases) %>%
    mutate_at(c("CONFIRMED", "PRESUMPTIVE"),function(d)coalesce(d,0L)) %>%
    group_by(Province,shortProvince) %>%
    arrange(Date) %>%
    mutate(`Official confirmed`=cumsum(CONFIRMED),Probable=cumsum(PRESUMPTIVE)) %>%
    # bind_rows((.) %>% group_by(Date) %>%
    #             summarize_at(c("Official confirmed","Probable"),sum) %>%
    #             mutate(Province="Canada",shortProvince="CAN")) %>%
    mutate(Confirmed=`Official confirmed`+Probable) %>%
    group_by(Province) %>%
    mutate(Cases=Confirmed-lag(Confirmed,order_by = Date,default = 0)) %>%
    ungroup %>%
    select(Province, shortProvince,Date,Confirmed,`Official confirmed`, Probable,Cases)
}

#' data from (https://covid19tracker.ca/)
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


#' combined data from UofS, Canada.ca and Canada Covid-19 working group.
#' @return dataframe with columns `shortProvince`, `Province`, `Date`,
#' `Confirmed`, `Deaths`, `Cases`, `Recovered`, `Active`
#' @export
get_canada_combined_provincial_data <- function(){
  od <- get_canada_official_provincial_data()
  #sd <- get_canada_UofS_provincial_data()
  wd <- get_canada_covid_working_group_provincial_data()

  # sd %>%
  #   select(shortProvince,Date,Confirmed) %>%
  #   full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
  #   select(shortProvince,Date,Confirmed) %>%
  #   full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
  #   full_join(wd %>% select(shortProvince,Date,Confirmed_w=Confirmed,Deaths_w=Deaths,Recovered), by=c("Date","shortProvince")) %>%
  wd %>%
    select(shortProvince,Date,Confirmed,Recovered,Deaths_w=Deaths) %>%
    mutate(Confirmed_w=Confirmed) %>%
    full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
    #full_join(wd %>% select(shortProvince,Date,Confirmed_w=Confirmed,Deaths_w=Deaths,Recovered), by=c("Date","shortProvince")) %>%
    group_by(shortProvince) %>%
    arrange(Date) %>%
    fill(Confirmed,.direction = "down") %>%
    fill(Confirmed_w,.direction = "down") %>%
    fill(Confirmed_o,.direction = "down") %>%
    fill(Deaths,.direction = "down") %>%
    fill(Deaths_w,.direction = "down") %>%
    fill(Recovered,.direction = "down") %>%
    mutate_at(c("Confirmed","Deaths","Deaths_w","Recovered","Confirmed_o","Confirmed_w"),
              function(d)coalesce(as.integer(d),0L)) %>%
    mutate(Cases=Confirmed-lag(Confirmed,order_by = Date,default = 0),
           Active=Confirmed-Deaths-Recovered) %>%
    mutate(Province=reverse_provincial_recodes[shortProvince]) %>%
    ungroup() %>%
    filter(Cases!=0)
}
