#' Simple key-value cache function accepting closures
#' @param object closure with return expression to be cached
#' @param key cache key
#' @param path path to cache the data
#' @param refresh bool option to force refresh of cache, default FALSE
#' @export
simpleCache <- function(object,key,path=getOption("custom_data_path"),refresh=FALSE){
  cache_path=file.path(path,key)
  if(!refresh & file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    data=object
    saveRDS(data,file=cache_path)
    data
  }
}

#' Helper function
#' @param data data frame with `region`, `Date` and `total` columns
#' @param start_cutoff
#' @return data fram with extra column `d` with days since `total` reached `start_cutoff`
#' @export
add_days_since <- function(data,start_cutoff){
  data %>%
    left_join(filter(.,total>=start_cutoff) %>%
                group_by(region) %>%
                summarise(firstDate=min(Date)),
              by="region") %>%
    mutate(d=difftime(Date,firstDate,units="days") %>% as.integer)
}



#' @keywords internal
empty_plot = function()ggplot()+geom_text(aes(x=1,y=1),label="No data\nTry adjusting the start cutoff or metric.")+theme_void()



#' @keywords internal
jhs_country_recodes <- c(
  "US"="USA",
  "United States"="USA",
  "United Kingdom"="UK",
  "Korea, South"="South Korea",
  "Taiwan*"="Taiwan"
)

#' @keywords internal
ecdc_country_recodes <- c(
  "United States of America"="USA",
  "United Kingdom"="UK",
  "Czech Republic"="Czechia"
)



