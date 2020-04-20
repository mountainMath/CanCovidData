#' Get confirmed case data from the covid-19 data working group
#' @return tibble with case data
#' @export
get_canada_covid_working_group_cases <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/cases.csv") %>%
    mutate(Date=as.Date(date_report,format="%d-%m-%Y"),count=1)
}

#' Get deaths data from the covid-19 data working group
#' @return tibble with deaths
#' @export
get_canada_covid_working_group_deaths <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/mortality.csv") %>%
    mutate(Date=as.Date(date_death_report,format="%d-%m-%Y"),count=1)
}

#' Get recovered case data from the covid-19 data working group
#' @return tibble with recovered case data
#' @export
get_canada_covid_working_group_recovered <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/recovered_cumulative.csv") %>%
    filter(!is.na(cumulative_recovered)) %>%
    mutate(Date=as.Date(date_recovered,format="%d-%m-%Y"),
           CumulativeRecovered=as.integer(cumulative_recovered)) %>%
    group_by(province) %>%
    mutate(count=CumulativeRecovered-lag(CumulativeRecovered,order_by = Date,default = 0)) %>%
    ungroup
}

#' Get data on testing in Canada from the covid-19 data working group
#' @return tibble with number of tests by province and data
#' @export
get_canada_covid_working_group_tests <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/testing_cumulative.csv") %>%
    filter(!is.na(cumulative_testing)) %>%
    mutate(cumulative_testing=gsub("[^0-9.]","",cumulative_testing)) %>%
    mutate(Date=as.Date(date_testing,format="%d-%m-%Y"),
           Tests=as.integer(cumulative_testing)) %>%
    group_by(province) %>%
    arrange(Date) %>%
    filter(!duplicated(Tests)) %>%
    mutate(Increase=Tests-lag(Tests,order_by = Date,default = 0)) %>%
    select(province,Date,Tests,Increase) %>%
    ungroup %>%
    mutate(Province=recode(province,!!!reverse_provincial_recodes),
           shortProvince=recode(Province,!!!provincial_recodes)) %>%
    select(-province)
}

#' data from the covid-19 data working group (https://github.com/ishaberry/Covid19Canada)
#' @return dataframe with columns `province`, `health_region`, `Date`, `type`, `count`
#' `type` is "Cases", "Deaths", "Recovered". All are new daily cases
#' and Recovered is cumulative recovered cases.
#' @export
get_canada_covid_working_group_data <- function(){
    bind_rows(get_canada_covid_working_group_cases() %>% mutate(type="Cases"),
              get_canada_covid_working_group_deaths() %>% mutate(type="Deaths"),
              get_canada_covid_working_group_recovered() %>% mutate(type="Recovered")) %>%
    select(health_region,province,Date,type,count)
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
get_canada_covid_working_group_health_region_data <- function(){
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
    select(`Health Region`=health_region,Province,shortProvince,Date,Confirmed,Deaths,Cases) %>%
    mutate(PR_UID = as.character(province_uid_lookup[Province])) %>%
    left_join(health_region_uid_join,by=c("PR_UID","Health Region"))
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
           Confirmed=numtotal,`Offical confirmed`=numconf,Probable=numprob,Deaths=numdeaths,`Official cases`=numtoday,Tested=numtested) %>%
    group_by(PR_UID) %>%
    mutate(Cases=Confirmed-lag(Confirmed,order_by = Date,default = 0)) %>%
    ungroup
}

#' data from UofS (https://covid19tracker.ca/)
#' @return dataframe with columns `Province`, `shortProvince`, `Health Region`, `Date`, `Age`,
#' `Travel history`, `Confirmed state`
#' @export
get_canada_UofS_case_data <- function() {
  tmp=tempfile("UofS_cases.xlsx")
  download.file("https://docs.google.com/spreadsheets/d/1e0QhkGT3XzJJh7l7EfUkK3be-81TawOEqO3JHMD8Y8s/export?format=xlsx&id=1e0QhkGT3XzJJh7l7EfUkK3be-81TawOEqO3JHMD8Y8s",tmp)
  data <- readxl::read_excel(tmp,"Case Data") %>%
  #r<-httr::GET("https://covid19tracker.ca/api/controller/cases.php")
  #data <- httr::content(r)$individualCases %>% purrr::map_df(as_tibble) %>%
    mutate(Date=as.Date(date)) %>%
    mutate(province=recode(province,"Repatriated Canadians"="Repatriated")) %>%
    mutate(shortProvince=recode(province,!!!provincial_recodes)) %>%
    select(id,Date,Province=province,shortProvince,`Health Region`=city,Age=age,
           `Travel history`=travel_history,`Confirmed state`=confirmed_presumptive)
}

#' data from UofS (https://covid19tracker.ca/)
#' @return dataframe with columns `Province`, `shortProvince`, `Date`,
#' `Confirmed`, `Offical confirmed`, `Probable`, `Cases`, where `Cases` is new daily cases and
#' all other counts are cumulative. `Confirmed` is `Offical confirmed` plus `Probable` (postive tests which have not been confirmed at an official national or provincial lab)
#' @export
get_canada_UofS_provincial_data <- function(){
  d<-get_canada_UofS_case_data() %>%
    group_by(Date,Province,shortProvince,`Confirmed state`) %>%
    summarize(Cases=n()) %>%
    bind_rows(group_by(.,Date,`Confirmed state`) %>%summarise(Cases=sum(Cases)) %>% mutate(Province="Canada",shortProvince="CAN")) %>%
    ungroup %>%
    pivot_wider(id_cols=c("Date","Province","shortProvince"), names_from = `Confirmed state`, values_from = Cases)

  if (!("PRESUMPTIVE" %in% names(d))) d <- d %>% mutate(PRESUMPTIVE=0L)

  d %>% mutate_at(c("CONFIRMED", "PRESUMPTIVE"),function(d)coalesce(d,0L)) %>%
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
    group_by(Date,`Health Region`) %>%
    summarize(Cases=n()) %>%
    group_by(`Health Region`) %>%
    arrange(Date) %>%
    mutate(Confirmed=cumsum(Cases))
}


#' combined data from UofS, Canada.ca and Canada Covid-19 working group.
#' @param use_UofS logical, whether or not to use UofS case data.
#' @return dataframe with columns `shortProvince`, `Province`, `Date`,
#' `Confirmed`, `Deaths`, `Cases`, `Recovered`, `Active`
#' @export
get_canada_combined_provincial_data <- function(use_UofS=TRUE){
  od <- get_canada_official_provincial_data()
  sd <- get_canada_UofS_provincial_data()
  wd <- get_canada_covid_working_group_provincial_data()

# try to mix and match, too messy
  # d<-od %>%
  #   filter(!(Cases==0 & is.na(`Official cases`))) %>%
  #   select(shortProvince,Date,Confirmed_od=Confirmed) %>%
  #   full_join(wd %>% select(shortProvince,Date,Confirmed_wd=Confirmed), by=c("Date","shortProvince")) %>%
  #   full_join(sd %>% select(shortProvince,Date,Confirmed_sd=Confirmed), by=c("Date","shortProvince")) %>%
  #   mutate(Confirmed=coalesce(Confirmed_od,Confirmed_wd)) %>%
  #   mutate(Confirmed=coalesce(Confirmed,as.numeric(Confirmed_sd)))




  if (use_UofS) {
  d <- sd %>%
    select(shortProvince,Date,Confirmed) %>%
    full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
    select(shortProvince,Date,Confirmed) %>%
    full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
    full_join(wd %>% select(shortProvince,Date,Confirmed_w=Confirmed,Deaths_w=Deaths,Recovered), by=c("Date","shortProvince"))
  }else {
    d <-   wd %>%
      select(shortProvince,Date,Confirmed,Recovered,Deaths_w=Deaths) %>%
      mutate(Confirmed_w=Confirmed) %>%
      full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince"))
  }
  d %>%
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

#' import and recode canadian case data from StatCan table 13-10-0766
#' @return a wide format data frame with one row per case with gender, age group,
#' hospitilization and ICU status, transmission pathway, mortality, and onset of symptoms data
#' Data is limited to case information forwarded by provinces to Canada Health
#' @export
get_cansim_case_data <- function(){
  data <- suppressWarnings(get_cansim("13-10-0766")) %>%
    mutate(GEO=gsub("^Canada .+$","Canada",GEO)) %>%
    normalize_cansim_values()

  hospitilization_recodes <- c(
    "1"="Yes",
    "2"="No",
    "9"="Not Stated"
  )

  transmission_recodes <- c(
    "1"= "Travel exposure",
    "2" = "Community exposure",
    "3" = "Pending"
  )

  death_recodes <- c(
    "1" = "Yes",
    "2" = "No",
    "9" = "Not stated"
  )

  age_recodes <- c(
    "1" = "0 to 19 years",
    "2" = "20 to 29 years",
    "3" = "30 to 39 years",
    "4" = "40 to 49 years",
    "5" = "50 to 59 years",
    "6" = "60 to 69 years",
    "7" = "70 to 79 years",
    "8" = "80 years or older",
    "99" = "Not stated"
  )

  sex_recodes <- c(
    "1" = "Male",
    "2" = "Female",
    "3" = "Non-binary",
    "9" = "Not stated"
  )

  recode_field <- function(data,field,recodes){
    data %>%
      mutate(!!field:=recode(!!as.name(field),!!!recodes)) %>%
      mutate(!!field:=factor(!!as.name(field),levels=recodes))
  }

  paste_date <- function(year,month,day) {
    ifelse(day==99 | month==99,NA,
           paste0(year,
                  "-",
                  str_pad(month,width=2,side="left",pad=0),
                  "-",
                  str_pad(day,width=2,side="left",pad=0))) %>%
      as.Date()
  }

  data %>%
    select(REF_DATE,`Case identifier number`,`Case information`,VALUE) %>%
    pivot_wider(id_cols = c("Case identifier number","REF_DATE"),
                names_from = `Case information`,values_from = VALUE) %>%
    mutate(Date=paste_date(REF_DATE,`Episode date - month`,`Episode date - day`)) %>%
    mutate(Date2=paste_date(REF_DATE,`Date case was last updated - month`,`Date case was last updated - day`)) %>%
    recode_field("Hospitalization",hospitilization_recodes) %>%
    recode_field("Intensive care unit",hospitilization_recodes) %>%
    recode_field("Transmission",transmission_recodes) %>%
    recode_field("Death",death_recodes) %>%
    recode_field("Age group",age_recodes) %>%
    recode_field("Gender",sex_recodes)
}


#' import and recode canadian case data from StatCan table 13-10-0767, use `get_cansim_case_data`
#' for getting data from the more up-to-date table 13-10-0766
#' @return a wide format data frame with one row per case with gender, age group,
#' hospitilization and ICU status, transmission pathway, mortality, and onset of symptoms data
#' Data is limited to case information forwarded by provinces to Canada Health
#' @export
get_cansim_old_case_data <- function(){
  data <- get_cansim("13-10-0767") %>%
    normalize_cansim_values()

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
#' @export
get_ontario_case_data <- function(){
  path="https://data.ontario.ca/datastore/dump/455fd63b-603d-4608-8216-7d8647f43350?format=csv"
  ontario_data <- read_csv(path) %>% #, col_types = cols(.default="c")) %>%
    st_as_sf(coords = c("Reporting_PHU_Longitude", "Reporting_PHU_Latitude"), crs = 4326, agr = "constant") %>%
    mutate(Date=as.Date(ACCURATE_EPISODE_DATE))
}

#' import and recode ontario case data from Alberta Open Data. Tends to have a day lag
#' @return a wide format data frame with one row per case with Health Region, gender, age group,
#' status, case type
#' @export
get_alberta_case_data <- function(){
  path="https://covid19stats.alberta.ca/"
  r <- xml2::read_html(path)
  scripts <- rvest::html_nodes(r, css='script[type="application/json"]') %>%
    lapply(rvest::html_text) %>% lapply(jsonlite::fromJSON)

  data <- NULL
  for (i in seq(1,length(scripts))) {
    if (!is.null(data)) next
    s <- scripts[[i]]
    if (!is.null(s$x) & !is.null(s$x$data) & !is.null(s$x$container)) {
      header <- xml2::read_html(s$x$container) %>% rvest::html_nodes("th") %>% rvest::html_text()
      header[1] <- "Case number"
      data <- s$x$data %>%
        as_tibble() %>%
        t() %>%
        as_tibble() %>%
        setNames(header)
    }
  }

  data
}

#' import and recode case data from British Columbia CDC. Tends to have a day lag
#' @return a wide format data frame with one row per case with Health Authority, gender, age group
#' and report date
#' @export
get_british_columbia_case_data <- function(){
  path="http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv"
  read_csv(path,col_types=cols(.default="c")) %>%
    rename(`Reported Date`=Reported_Date,`Health Authority`=HA,`Age group`=Age_Group) %>%
    mutate(`Reported Date`=as.Date(`Reported Date`))
}

#' import and recode test data from British Columbia CDC. Tends to have a day lag
#' @return a long format data frame with Date, Health Authority, Metric of tpye of test and Count
#' @export
get_british_columbia_test_data <- function(){
  path="http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Lab_Information.csv"
  read_csv(path,col_types=cols(.default="c")) %>%
    mutate(Date=as.Date(Date)) %>%
    rename(`Health Authority`=Region) %>%
    pivot_longer(-one_of("Date","Health Authority"),names_to = "Metric",values_to = "Count") %>%
    mutate(Count=as.integer(Count))
}







#' get Health Region geographies
#' @return a simple feature collection with 2018 Health Region data
#' This misses at least one 2020 change to Ontario Health regoins
#' @export
get_health_region_geographies_2018 <- function(){
  tmp=tempfile()
  download.file("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip?st=mOIAprjV",tmp)
  f<-unzip(tmp,exdir = tempdir())
  read_sf(f[grepl("\\.shp$",f)]) %>%
    mutate(Name=ENGNAME, GeoUID = HR_UID) %>%
    mutate(PR_UID=substr(HR_UID,1,2)) %>%
    mutate(PR_NAME=province_name_lookup[PR_UID])
}



#' get Health Region level census data
#' @param refresh set to `TRUE` to refresh locally cached data
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


bc_ha_name_lookup <- c(
  "591" = "Interior",
  "592" = "Fraser",
  "593" = "Vancouver Coastal",
  "594" = "Vancouver Island",
  "595" = "Northern"
)

#' aggregate health regions to health autorities in BC
#' @param data dataframe with fields `GeoUID` = HR_UID and `Name`
#' @return a dataframe with GeoUID and Name adjusted to BC Helath Authorities instead of Health Regions
replace_BC_health_region_geocodes <- function(data) {
  data %>%
    mutate(GeoUID = ifelse(grepl("^59",GeoUID),substr(GeoUID,1,3),GeoUID)) %>%
    mutate(Name=ifelse(GeoUID %in% names(bc_ha_name_lookup),recode(GeoUID,!!!bc_ha_name_lookup),Name))
}

ontario_hr_name_lookup <- c(
  "3554" = "Southwestern Public Health",
  "35xx" = "Huron Perth District Health Unit"
)



#' aggregate health regions to health autorities in ON
#' @param data dataframe with fields `GeoUID` = HR_UID and `Name`
#' @return a dataframe with GeoUID and Name adjusted to current names
replace_ON_health_region_geocodes <- function(data){
  data %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("3531","3552"), "3575",GeoUID)) %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("3554","3539"), "35xx",GeoUID)) %>%
    mutate(Name=ifelse(GeoUID %in% names(ontario_hr_name_lookup),recode(GeoUID,!!!ontario_hr_name_lookup),Name))
}


saskatoon_hr_name_lookup <- c(
  "47x1" = "South",
  "47x2" = "Central",
  "47x3" = "North",
  "47x4" = "Far North",
  "4704" = "Regina",
  "4706" = "Saskatoon"
)

#' aggregate health regions to health autorities in SK
#' @param data dataframe with fields `GeoUID` = HR_UID and `Name`
#' @return a dataframe with GeoUID and Name adjusted to current names
replace_SK_health_region_geocodes <- function(data){
  data %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("4701","4702","4703"), "47x1",GeoUID)) %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("4705","4707"), "47x2",GeoUID)) %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("4708","4709","4710"), "47x3",GeoUID)) %>%
    mutate(GeoUID=ifelse(GeoUID %in% c("4711","4712","4713"), "47x4",GeoUID)) %>%
    mutate(Name=ifelse(GeoUID %in% names(saskatoon_hr_name_lookup),recode(GeoUID,!!!saskatoon_hr_name_lookup),Name))
}


#' aggregate health regions to health autorities BC, SK, ON
#' @param data dataframe with fields `GeoUID` = HR_UID and `Name`
#' @return a dataframe with GeoUID and Name adjusted to current names
#' @export
replace_all_health_region_geocodes <- function(data) {
  data %>%
    replace_BC_health_region_geocodes() %>%
    replace_ON_health_region_geocodes() %>%
    replace_SK_health_region_geocodes()
}
