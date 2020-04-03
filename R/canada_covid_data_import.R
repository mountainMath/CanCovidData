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
  r<-httr::GET("https://covid19tracker.ca/api/controller/cases.php")
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
  sd <- get_canada_UofS_provincial_data()
  wd <- get_canada_covid_working_group_provincial_data()

  sd %>%
    select(shortProvince,Date,Confirmed) %>%
    full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
    select(shortProvince,Date,Confirmed) %>%
    full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
    full_join(wd %>% select(shortProvince,Date,Confirmed_w=Confirmed,Deaths_w=Deaths,Recovered), by=c("Date","shortProvince")) %>%
  # wd %>%
  #   select(shortProvince,Date,Confirmed,Recovered,Deaths_w=Deaths) %>%
  #   mutate(Confirmed_w=Confirmed) %>%
  #   full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
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


#' import and recode canadian case data from StatCan table 13-10-0767
#' @return a wide format data frame with one row per case with gender, age group,
#' hospitilization and ICU status, transmission pathway, mortality, and onset of symptoms data
#' Data is limited to case information forwarded by provinces to Canada Health
#' @export
get_cansim_case_data <- function(){
  data <- get_cansim("13-10-0767") %>%
    normalize_cansim_values(factors = TRUE)

  notes <- get_cansim_table_notes("13-10-0767")

  covid_theme <- list(
    theme_bw(),
    labs(caption="MountainMath, StatCan table 13-10-0767")
  )

  hospitilization_recodes <- c(
    "1"="Yes",
    "2"="No",
    "7"="Unknown",
    "9"="Not Stated"
  )

  transmission_recodes <- c(
    "1"= "Travel exposure",
    "2" = "Community exposure",
    "3" = "Pending"
  )

  status_recodes <- c(
    "1" = "Deceased",
    "9" = "Not stated"
  )

  age_recodes <- c(
    "1" = "0 to 19 years",
    "2" = "20 to 39 years",
    "3" = "40 to 49 years",
    "4" = "50 to 59 years",
    "5" = "60 to 69 years",
    "6" = "70 to 79 years",
    "7" = "80 years or older",
    "9" = "Not stated"
  )

  gender_recodes <- c(
    "1" = "Male",
    "2" = "Female",
    "3" = "Other",
    "7" = "Unknown",
    "9" = "Not stated"
  )

  recode_field <- function(data,field,recodes){
    data %>%
      mutate(!!field:=recode(!!as.name(field),!!!recodes)) %>%
      mutate(!!field:=factor(!!as.name(field),levels=recodes))
  }


  data %>%
    select(REF_DATE,`Case identifier number`,`Case information`,VALUE) %>%
    pivot_wider(id_cols = c("Case identifier number","REF_DATE"),
                names_from = `Case information`,values_from = VALUE) %>%
    mutate(Date=as.Date(paste0(REF_DATE,"-",`Episode date - month`,"-",`Episode date - day`))) %>%
    mutate(Date2=as.Date(paste0(REF_DATE,"-",`Date case was last updated - month`,"-",`Date case was last updated - day`))) %>%
    recode_field("Hospitalization",hospitilization_recodes) %>%
    recode_field("Intensive care unit",hospitilization_recodes) %>%
    recode_field("Transmission",transmission_recodes) %>%
    recode_field("Status",status_recodes) %>%
    recode_field("Age group",age_recodes) %>%
    recode_field("Gender",gender_recodes)
}

#' import and recode ontario case data from Ontario Open Data. Tends to have a day lag
#' @return a wide format data frame with one row per case with Health Region, gender, age group,
#' transmission pathway, status, and onset of symptoms data
#' Data is limited to case information forwarded by provinces to Canada Health
#' @export
get_ontario_case_data <- function(){
  path="https://data.ontario.ca/datastore/dump/455fd63b-603d-4608-8216-7d8647f43350?format=csv"
  ontario_data <- read_csv(path) %>% #, col_types = cols(.default="c")) %>%
    st_as_sf(coords = c("Reporting_PHU_Longitude", "Reporting_PHU_Latitude"), crs = 4326, agr = "constant") %>%
    mutate(Date=as.Date(ACCURATE_EPISODE_DATE))
}

#' get Health Region geographies
#' @return a simple feature collection with 2018 Health Region data
#' This misses at least one 2020 change to Ontario Health regoins
#' @export
get_health_region_geographies_2018 <- function(){
  tmp=tempfile()
  download.file("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip?st=mOIAprjV",tmp)
  f<-unzip(tmp,exdir = tempdir())
  read_sf(f[grepl("\\.shp$",f)])
}

#' get Health Region level census data
#' @return a long form dataframe with census data for health regions
#' The health region geography used for this extract is different from the 2018 health regions
#' as it misses at least one health region amalgamation in 2018 (and the one in 2020) in Ontario.
#' @export
get_health_region_census_2016_data <- function(refresh=FALSE){
  path <- file.path(getOption("cache_path"),"health_regions_2016.csv")
  if (refresh |!file.exists(path)){
    tmp=tempfile()
    download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=058",tmp)
    f<-unzip(tmp,exdir = tempdir())
    file.copy(f[grepl("_data\\.csv$",f)],path)
  }
  read_csv(path,col_types = cols(.default = "c"))
}

