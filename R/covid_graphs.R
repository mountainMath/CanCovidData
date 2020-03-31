#' Generate grawth rate graph
#' @param data data fram with colum `region` for each group, `Date` with dates and `total`, for fitting the growth rates
#' @return a `ggplot` object
#' @export
growth_rate_graph <- function(data,window_width){


  rr<-data %>%
    compute_rolling_growth_rates(window_width)  %>%
    filter(!is.na(slope))


  growth_lines <- tibble(doubling=c(2,3,4,5,7,14)) %>%
    mutate(label=paste0("doubles in ",doubling," days"),
           slope=log(2)/doubling) %>%
    mutate(d=2)
    #mutate(Date=sort(rr$Date)[2])

  ggplot(rr,aes(x=d,y=slope,color=region)) +
    geom_hline(data=growth_lines,
               aes(yintercept=slope),linetype="dashed",color="grey") +
    geom_text(data=growth_lines,
              aes(label=label), #hjust=1,
              size=3,
              color="black",
              hjust=0,
              vjust=0,
              alpha=0.6) +
    # geom_ribbon(aes(ymax=high,ymin=low,fill=Countr),
    #             alpha=0.1,size=0) +
    geom_line(size=1) +
    geom_point(size=2,shape=21,alpha=0.5) +
    # geom_text(data=~filter(group_by(.,label),Date==min(Date)),
    #           aes(label=label),
    #           size=4,
    #           hjust=-1,guide=FALSE) +
    theme_bw() +
    #ggsci::scale_color_lancet() +
    #scale_color_brewer(palette = "Dark2") +
    #scale_color_manual(values=manual_colours,guide=FALSE) +
    #scale_fill_manual(values=manual_colours,guide=FALSE) +
    labs(title=paste0("COVID-19 growth rate"),
         x=paste0("End date of rolling ",window_width," day window"),
         y=paste0("Growth rate (",window_width," day trailing fit)"),
         color=NULL) +
    scale_y_continuous(labels=scales::percent,limits=c(0,NA)) #+
    # scale_x_date(minor_breaks = function(x)seq.Date(from = min(x), to = max(x), by = "1 days"),
    #              breaks=function(x) seq.Date(from = min(x), to = max(x), by = "1 week"),
    #              #guide = guide_axis(n.dodge = 2),
    #              labels=function(d)strftime(d,format="%b %d")) #+
  #theme(axis.text.x = element_text(angle = 60,hjust=1))
}


#' Generate grawth rate graph
#' @param data data fram with colum `region` for each group, `Date` with dates and `total`, for fitting the growth rates
#' @return a `ggplot` object
#' @export
total_graph <- function(data) {

  if (nrow(data)==0) return(empty_plot())

  factor <- 10
  major_grid <- 10 * factor**(seq(0,3))
  minor_grid <- lapply(major_grid,function(d)seq(d,factor*d,d)) %>% unlist %>% unique
  plot_labels <- rep("",length(minor_grid))
  plot_labels[which(minor_grid %in% major_grid)]=scales::comma(major_grid)
  ybreaks <- minor_grid <- lapply(major_grid,function(d)seq(d,factor*d,2*d)) %>% unlist %>% unique



  manual_colours <- country_base_colours

  maxpd = max(data$d) %>% as.integer()
  maxT=max(data$Confirmed) %>% as.integer()

  intercept = filter(data,d==min(d))$total %>% as.integer() %>% mean

  full_growth_lines <-  tibble(d=seq(0,maxpd),id=1) %>%
    full_join(tibble(rate=c(2,3,4,5,7),id=1),by="id") %>%
    mutate(total=intercept*2**(d/rate)) %>%
    filter(total<=maxT)

  growth_lines <- full_growth_lines %>%
    group_by(rate) %>%
    filter(d %in% c(0,max(d))) %>%
    ungroup



  growth_labels <- full_growth_lines %>%
    group_by(rate) %>%
    filter(d==max(d)-1) %>%
    mutate(label=paste0("doubles in\n",rate," days"),
           color="black")




  g<-ggplot(data,aes(x=as.integer(d),y=total,color=region)) +
    scale_y_continuous(trans="log10",breaks=minor_grid) +
    scale_x_continuous(breaks=seq(0,maxpd)) +
    geom_line(data=growth_lines,linetype="dashed",color="grey",aes(group=rate)) +
    geom_line(size=1) +
    geom_point(size=2,shape=21,alpha=0.5) +
    geom_text(data=~filter(group_by(.,region),d==max(d)),
              aes(label=region),
              size=4,
              vjust=1) +
    geom_text(data=growth_labels,
              aes(label=label), hjust=1,
              size=3,
              color="black",
              vjust=0.5,
              alpha=0.6) +
    # labs(title=paste0("COVID-19 ",case_text," in select countries"),
    #      x=paste0("Days after reporting at least ",start_cutoff," ",case_text),
    #      y=paste0("Number of ",case_text," (log scale)"),
    #      caption="MountainMath, Data: ECDC, JHS") +
    theme_bw() +
    theme(legend.position = "bottom")
}

#' @import ggplot2

NULL

