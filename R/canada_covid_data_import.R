clean_cases <- function(data){
  data %>% mutate(Date=as.Date(date_report,format="%d-%m-%Y"),count=1)
}


#' Get confirmed case data from the covid-19 data working group
#' @return tibble with case data
#' @export
get_canada_covid_working_group_cases <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/cases.csv") %>%
    clean_cases()
}

clean_deaths <- function(data){
  data %>% mutate(Date=as.Date(date_death_report,format="%d-%m-%Y"),count=1)
}

#' Get deaths data from the covid-19 data working group
#' @return tibble with deaths
#' @export
get_canada_covid_working_group_deaths <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/mortality.csv") %>%
    clean_deaths()
}

clean_recovered <- function(data){
  data %>%
    select(date_recovered, province, cumulative_recovered) %>%
    filter(!is.na(cumulative_recovered)) %>%
    mutate(Date=as.Date(date_recovered,format="%d-%m-%Y"),
           CumulativeRecovered=as.integer(cumulative_recovered)) %>%
    group_by(province) %>%
    mutate(count=CumulativeRecovered-lag(CumulativeRecovered,order_by = Date,default = 0)) %>%
    ungroup
}

#' Get recovered case data from the covid-19 data working group
#' @return tibble with recovered case data
#' @export
get_canada_covid_working_group_recovered <- function(){
  read_csv("https://github.com/ishaberry/Covid19Canada/raw/master/recovered_cumulative.csv") %>%
    clean_recovered()
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
#' @param use_csv use convenience csv download instead of cases
#' @return dataframe with columns `province`, `health_region`, `Date`, `type`, `count`
#' `type` is "Cases", "Deaths", "Recovered". All are new daily cases
#' and Recovered is cumulative recovered cases.
#' @export
get_canada_covid_working_group_data <- function(use_csv=TRUE){
  if (use_csv) {
    result <- bind_rows(get_canada_covid_working_group_cases() %>% mutate(type="Cases"),
                        get_canada_covid_working_group_deaths() %>% mutate(type="Deaths"),
                        get_canada_covid_working_group_recovered() %>% mutate(type="Recovered"))

  } else {
    tmp=tempfile("cases.xlsx")
    tmp2=tempfile("mortality.xlsx")
    case_path <- "https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=xlsx"
    mortality_path <- "https://docs.google.com/spreadsheets/d/1GrX5kiDA-rj3L9K0Q2IY1H-D7unAd7lfGAF7CMjhdVw/export?format=xlsx&id=1GrX5kiDA-rj3L9K0Q2IY1H-D7unAd7lfGAF7CMjhdVw"
    #download.file("https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=xlsx&id=1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo",tmp)
    download.file(case_path,tmp)
    download.file(mortality_path,tmp2)
    cases <- readxl::read_xlsx(tmp,"Cases",skip=3) %>% clean_cases()

    deaths <- readxl::read_xlsx(tmp2,"Mortality",skip=3) %>% clean_deaths()
    recovered <- readxl::read_xlsx(tmp2,"Recovered",skip=3) %>% clean_recovered()

    result <- bind_rows(cases %>% mutate(type="Cases"),
                        deaths %>% mutate(type="Deaths"),
                        recovered %>% mutate(type="Recovered"))
  }


  result %>%
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
    summarize(count=sum(count),.groups="drop") %>%
    pivot_wider(id_cols = c("province","Date"),names_from = type,values_from = count) %>%
    group_by(province) %>%
    arrange(Date) %>%
    #fill(Recovered,.direction = "down") %>%
    mutate(#Recovered=coalesce(Recovered,0),
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
    #fill(Cases,.direction = "down") %>%
    #fill(Deaths,.direction = "down") %>%
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
    mutate(shortProvince=recode(prname,!!!provincial_recodes),
           `Official cases`=numtoday) %>%
    rename(PR_UID=pruid,
           Confirmed=numtotal,`Offical confirmed`=numconf,Probable=numprob,Deaths=numdeaths,Tested=numtested,
           Cases=numtoday)
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
get_canada_combined_provincial_data <- function(use_UofS=FALSE){
  od <- get_canada_official_provincial_data()
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
    sd <- get_canada_UofS_provincial_data()
    d <- sd %>%
      select(shortProvince,Date,Confirmed) %>%
      full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths_o=Deaths), by=c("Date","shortProvince")) %>%
      select(shortProvince,Date,Confirmed) %>%
      full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths), by=c("Date","shortProvince")) %>%
      full_join(wd %>% select(shortProvince,Date,Confirmed_w=Confirmed,Deaths,Recovered), by=c("Date","shortProvince"))
  }else {
    d <-   wd %>%
      select(shortProvince,Date,Confirmed,Recovered,Deaths) %>%
      mutate(Confirmed_w=Confirmed) %>%
      full_join(od %>% select(shortProvince,Date,Confirmed_o=Confirmed,Deaths_o=Deaths), by=c("Date","shortProvince"))
  }
  d %>%
    group_by(shortProvince) %>%
    arrange(Date) %>%
    fill(Confirmed,.direction = "down") %>%
    fill(Confirmed_w,.direction = "down") %>%
    fill(Confirmed_o,.direction = "down") %>%
    fill(Deaths,.direction = "down") %>%
    fill(Deaths_o,.direction = "down") %>%
    fill(Recovered,.direction = "down") %>%
    mutate_at(c("Confirmed","Deaths","Deaths_o","Recovered","Confirmed_o","Confirmed_w"),
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
    mutate(Date=as.Date(Accurate_Episode_Date))
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

#' import and recode case data from Saskatchewan.
#' @param date for which to download the data. Defauly is NULL and downloads the newest available
#' @return a wide format data frame
#' @export
get_manitoba_daily_case_data <- function(date=NULL){
  tmp <- tempfile()
  url="https://services.arcgis.com/mMUesHYPkXjaFGfS/arcgis/rest/services/mb_covid_cases_by_demographics_rha_all/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&groupByFieldsForStatistics=Age_Group%2CGender&orderByFields=Age_Group%20desc"
  if (!is.null(date)){
    url=paste0("https://raw.githubusercontent.com/jacmarcx/covid_mb_downloader/master/json/demographics-rha/",date,"-demographics-rha.json")
  }
  download.file(url,tmp)
  d<-jsonlite::read_json(tmp)$features %>%
    lapply(function(r)as_tibble(r$attributes)) %>%
    bind_rows
}


#' import and recode case data from caanada covid tracker
#' @param province two letter provincial abbreviation code
#' @return a wide format data frame
#' @export
get_can_covid_tracker_data <- function(province){
  if (province=="CA") {
    url="https://api.covid19tracker.ca/reports"
  } else {
    url=paste0("https://api.covid19tracker.ca/reports/province/",province)
  }
  r<-jsonlite::read_json(url)$data
  d<-r %>%
    lapply(function(l)l %>% unlist %>% as.data.frame() %>% t() %>% as_tibble()) %>%
    bind_rows() %>%
    mutate(Date=as.Date(date))
}

#' import and recode case data from Saskatchewan.
#' @return a wide format data frame
#' @export
get_saskatchewan_case_data <- function(){
  d<-xml2::read_html("https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases")
  e<-rvest::html_node(d,".indicator-export") %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href")
  readr::read_csv(paste0("https://dashboard.saskatchewan.ca",e))
}

#' import and recode case data from Quevec
#' @return a wide format data frame
#' @export
get_quebec_case_data <- function(){
  readr::read_csv("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv")
}

get_alberta_case_data2 <- function(){
  url <- "https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"
  r<-xml2::read_html(url)
  n<-r %>% rvest::html_node("#htmlwidget-6045272bf9c7462e8b33")
  widget_data<-r %>% rvest::html_node(xpath="//script[@data-for='htmlwidget-6045272bf9c7462e8b33']") %>%
    rvest::html_text() %>%
    jsonlite::parse_json()

  d<-widget_data$x$data %>%
    lapply(unlist) %>%
    bind_cols() %>%
    as_tibble() %>%
    setNames(c("Case number","Date reported","Alberta Health Service Zone","Gender","Age group","Case status","Case type")) %>%
    mutate(Date=as.Date(`Date reported`)) %>%
    arrange(Date)
}

#' import and recode case data from British Columbia CDC. Tends to have a day lag
#' @return a wide format data frame with one row per case with Health Authority, gender, age group
#' and report date
#' @export
get_british_columbia_case_data <- function(){
  path="http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv"
  read_csv(path,col_types=cols(.default="c")) %>%
    rename(`Reported Date`=Reported_Date,`Health Authority`=HA,`Age group`=Age_Group) %>%
    mutate(`Age group`=recode(`Age group`,"19-Oct"="10-19")) %>%
    mutate(`Reported Date`=as.Date(`Reported Date`,tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
}

#' import and recode Health Region (Health Service Delivery Area) case data from British Columbia CDC. Tends to have a day lag
#' @return a wide format data frame with one row per Date, Health Authority, Health Region,
#' with Cases and Cases Smoothed
#' @export
get_british_columbia_hr_case_data <- function(){
  path="http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"
  read_csv(path,col_types=cols(.default="c")) %>%
    rename(`Health Authority`=HA,`Health Region`=HSDA,Cases=Cases_Reported,`Cases Smoothed` =Cases_Reported_Smoothed ) %>%
    mutate(Date=as.Date(Date,tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))) %>%
    mutate_at(c("Cases","Cases Smoothed"),as.numeric)
}

#' Get Open Canada Working Group timeseries data via API
#'
#' @param type type of data, one of cases, mortality, recovered, testing, active, avaccine, dvaccine or cvaccine
#' @param location of data, either level of geography canada, prov, hr, or a specific region code, two-letter province code or a health region code
#' @return a data frame with the data
#' @export
get_canada_covid_working_group_timeseries <- function(type="cases",location="prov") {
  url=paste0("https://api.opencovid.ca/timeseries?stat=",type,"&loc=",location)
  response <- httr::GET(url)
  result <- httr::content(response)[[type]] %>%
    purrr::map_df(function(d)as_tibble(d,.name_repair = "minimal"))

  dates <- names(result)[grepl("date",names(result))]
  if (length(dates>0)) result <- result %>% mutate_at(dates,function(d)as.Date(d,format="%d-%m-%Y"))

  if ("province" %in% names(result)) {
    result <- result %>%
      mutate(Province=recode(province,!!!reverse_provincial_recodes),
             shortProvince=recode(Province,!!!provincial_recodes))
  }

  result
}

#' import open table data from \link{https://www.opentable.com/state-of-industry}
#' @param type, type of data, one of "fullbook", "reopening", or "occupancy"
#' @return open table booking, opening or occupancy data
#'
#' @export
get_open_table_data <- function(type=c("fullbook",  "reopening", "occupancy")){
  type <- type[1]
  root <- xml2::read_html("https://www.opentable.com/state-of-industry")
  s<-root %>% rvest::html_nodes("script")
  r<-s[s %>% lapply(function(ss)grepl("covidDataCenter",rvest::html_text(ss))) %>% unlist]
  json_text <- rvest::html_text(r)[[1]] %>%
    gsub('^.+"covidDataCenter":\\{','{',.) %>%
    gsub(',"authentication".+$','',.)

  extract_data <- function(l,header){
    data <- as_tibble(l)
    v <- data %>% select_if(is.list) %>% names
    data %>% cbind(data %>%
                     pull(v) %>%
                     lapply(function(r){
                       n <- length(header)-length(r)
                       if (n>0) r<-c(r,rep(NA,n))
                       r
                     }) %>%
                     as.data.frame(row.names = header) %>%
                     t() %>%
                     as_tibble(.name_repair = "minimal")) %>%
      select(-one_of(v))
  }

  json <- jsonlite::fromJSON(json_text)[[type]]
  header=json$headers
  months <- header %>% strsplit("/") %>% lapply(first) %>% unlist
  old_year <- which(months==12) %>% last
  years=c(rep("2020",old_year),rep("2021",length(months)-old_year))
  headers <- seq(1,length(header)) %>% lapply(function(i) paste0(header[i],"/",years[i])) %>% unlist
  data <- names(json) %>%
    setdiff(c("headers","lastModified")) %>%
    lapply(function(n){
      extract_data(json[[n]],headers) %>%
        mutate(level=n)
    }) %>%
    bind_rows() %>%
    as_tibble() %>%
    pivot_longer(-one_of(c("name","id","size","level","country", "state" )),names_to="date",values_to="value") %>%
    mutate_at(c("size","value"),as.numeric) %>%
    mutate(Date=as.Date(date,format = "%m/%d/%Y"))
}

#' import and recode test data from British Columbia CDC. Tends to have a day lag
#' @return a long format data frame with Date, Health Authority, Metric of tpye of test and Count
#' @export
get_british_columbia_test_data <- function(){
  path="http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Lab_Information.csv"
  read_csv(path,col_types=cols(.default="c")) %>%
    mutate(Date=as.Date(Date,tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))) %>%
    rename(`Health Authority`=Region) %>%
    pivot_longer(-one_of("Date","Health Authority"),names_to = "Metric",values_to = "Count") %>%
    mutate(Count=as.numeric(Count),Metric=gsub("_"," ",Metric))
}


#' get Toronto case data old
#' @return a data frame with Toronto neighbourhood level covid-19 counts
get_toronto_neighbourhood_cases_old <- function(){
  tmp <- tempfile(fileext = ".xlsx")
  utils::download.file("https://docs.google.com/spreadsheets/d/1euhrML0rkV_hHF1thiA0G5vSSeZCqxHY/export?format=xlsx&id=1euhrML0rkV_hHF1thiA0G5vSSeZCqxHY",tmp)
  sheets <- readxl::excel_sheets(tmp)
  d <- readxl::read_xlsx(tmp,"Cases of COVID-19 by Neighbourh") %>%
    mutate(Name=gsub(" \\(\\d+\\)$","",`Neighbourhood Name`),
           Code=gsub(".* \\((\\d+)\\)$", "\\1", `Neighbourhood Name`) %>%
             as.integer)
}

#' get Toronto case data
#' @return a data frame with Toronto neighbourhood level covid-19 counts
#' @export
get_toronto_neighbourhood_cases <- function(){
  readr::read_csv("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv")
}



#' get Toronto neihgbourhood geographies
#' @param refresh data is cached for the duration of the session, set to `TRUE` to refresh the cache
#' @return a simple feature collection with Toronto neighbrouhoods
#' @export
get_toronto_neighbourhood_geos <- function(refresh=FALSE){
  path<-file.path(tempdir(),"toronto_nbhd_geos")
  if (refresh | !dir.exists(path)) {
    dir.create(path)
    tmp=tempfile()
    utils::download.file("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/a083c865-6d60-4d1d-b6c6-b0c8a85f9c15?format=shp&projection=4326",tmp)
    utils::unzip(tmp,exdir = path)
    unlink(tmp)
  }
  file_name <- "Neighbourhoods.shp"
  geo <- sf::read_sf(file.path(path,file_name))
  geo %>%
    select(Code=FIELD_5,Name=FIELD_7,ID=FIELD_1) %>%
    mutate(Code=as.integer(Code),ID=as.character(ID))
}

#' get Toronto neihgbourhood census data 2016
#' @param refresh data is cached for the duration of the session, set to `TRUE` to refresh the cache
#' @return a data frame with toronto neighbourhood census data
#' @export
get_toronto_neighbourhood_census_data <- function(refresh=FALSE){
  tmp<-file.path(tempdir(),"toronto_hoods_census.csv")
  if (!file.exists(tmp)) {
    utils::download.file("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv",tmp)
  }
  header <- readr::read_csv(tmp,col_names=FALSE,n_max=3)
  h<-header[1,] %>% as.character()
  h[1]="Member ID"
  hoods <- header[1,seq(6,ncol(header))] %>% as.character()
  codes <- header[2,seq(6,ncol(header))] %>% as.character()
  codes[1]="3520005"
  code_lookup <- rlang::set_names(codes,hoods)
  cd <- readr::read_csv(tmp,col_names=h,skip=3,col_types = readr::cols(.default="c")) %>%
    tidyr::pivot_longer(hoods,names_to = "Name",values_to = "Value") %>%
    mutate(Code=code_lookup[Name] %>% as.integer) %>%
    mutate(Value=as.numeric(gsub(",","",Value)))

  cd
}


#' get Health Region geographies
#' @param refresh data is cached for the duration of the session, set to `TRUE` to refresh the cache
#' @return a simple feature collection with 2018 Health Region data
#' This misses at least one 2020 change to Ontario Health regoins
#' @export
get_health_region_geographies_2018 <- function(refresh=FALSE){
  path=file.path(tempfile(),"health_region_geos")
  if (refresh | !dir.exists(path)) {
    if (dir.exists(path)) file.remove(path,recursive+TRUE)
    tmp=tempfile()
    download.file("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip?st=9qprvBH_",tmp)
    #download.file("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/Cart2018_ArcGIS-eng.zip?st=W67zNX6P",tmp)
    #download.file("https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip?st=mOIAprjV",tmp)
    unzip(tmp,exdir = path)
  }
  ff=dir(path)
  f=dir(file.path(path,ff))
  file=f[grepl("\\.shp$",f)]
  read_sf(file.path(path,ff,file)) %>%
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


get_bc_covid_speak_survey_path <- function(){
  tmp <- tempdir()
  path <- file.path(tempdir(),"bc_covid_speak_survey.xlsx")
  if (!file.exists(path))
    download.file("http://www.bccdc.ca/Health-Info-Site/Documents/BC_COVID_19_Survey_downloadable_data.xlsx?Web=1",path)
  path
}

get_bc_covid_speak_survey_header <- function(file,sheet,size=2){
  h<-readxl::read_xlsx(file,sheet,col_names = FALSE,n_max=size) %>%
    t() %>%
    as_tibble() %>%
    fill(V1) %>%
    mutate(V3=case_when(is.na(V1) ~ V2,
                        is.na(V2) ~ V1,
                        TRUE ~ paste0(V1," - ",V2))) %>%
    pull(V3)
}



#' get BC Covid SPEAK survey data
#' @return a dataframe with data
#' @export
get_bc_covid_speak_survey_data <- function(){
  tmp <- get_bc_covid_speak_survey_path()
  sd <- readxl::read_xlsx(tmp,"Survey Data",
                          col_names = get_bc_covid_speak_survey_header(tmp,"Survey Data",2),
                          skip=2) %>%
    fill(Domain) %>%
    mutate_at(vars(matches(" - ")),function(d)na_if(d,"Suppressed") %>% as.numeric) %>%
    pivot_longer(cols=matches(" - ")) %>%
    mutate(level=gsub(" - .+$","",name),
           region=gsub("^.+ - ","",name) %>% gsub(" \\(%\\)$","",.)) %>%
    select(-name) %>%
    mutate(value=value/100)

}

#' get BC Covid SPEAK population group survey data
#' @return a dataframe with data
#' @export
get_bc_covid_speak_survey_pop_group_data <- function(){
  tmp <- get_bc_covid_speak_survey_path()
  ad <- readxl::read_xlsx(tmp,"Survey Data by Population Group",
                          col_names = get_bc_covid_speak_survey_header(tmp,"Survey Data by Population Group",2),
                          skip=2) %>%
    rename("Population Group"="Population Group - Population Group","Category"="Population Group Category - Population Group Category") %>%
    fill(Domain,Indicator,`Population Group`,Category) %>%
    mutate_at(vars(matches(" - ")),function(d)na_if(d,"Suppressed") %>% as.numeric) %>%
    pivot_longer(cols=matches(" - ")) %>%
    slice(-1) %>%
    mutate(level=gsub(" - .+$","",name),
           region=gsub("^.+ - ","",name) %>% gsub("\r\n"," ",.) %>% gsub(" \\(%\\)$","",.)) %>%
    select(-name) %>%
    mutate(value=value/100)
}
