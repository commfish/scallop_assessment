# mapping ----

f_xmap <- function(x){
  # function to create a base background map - called in other functions
  ggplot()+
    geom_polygon(data=ak, aes(long, lat, group=group), fill=8, color='black') +
    xlab(expression(paste(Longitude^o,~'W'))) +
    ylab(expression(paste(Latitude^o,~'W'))) + 
    coord_map(xlim=c(min(x$set_lon,na.rm=T) - 0.2, max(x$set_lon,na.rm=T) + 0.2),
              ylim=c(min(x$set_lat,na.rm=T) - 0.2, max(x$set_lat,na.rm=T) + 0.2)) +
    theme_sleek()
}

f_global_map <- function(data, YEAR){
  # organize 
  data %>% 
    mutate(District = factor(District, 
                             levels = c('YAK', 'EKI', 'WKI', 'KSH', 
                                        'KNE', 'KSE', 'KSW', 'C', 'UB', 
                                        'KSEM', 'O', 'Q'))) -> x
  
  f_xmap(x) + 
    geom_point(data=x, aes(set_lon, set_lat, color=District), alpha=.2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) -> globe
  
  data %>% 
    filter(year==(YEAR-1)) %>% 
    mutate(District = factor(District, 
                             levels = c('YAK', 'EKI', 'WKI', 'KSH',
                                        'KNE', 'KSE', 'KSW', 'C', 
                                        'UB', 'KSEM', 'O', 'Q'))) -> x1
  f_xmap(x) + 
    geom_point(data=x1, aes(set_lon, set_lat, color=District), alpha=.2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) -> x1

  x = arrangeGrob(globe, x1, ncol=1)
  
  ggsave(paste0('figs/', YEAR, '/global_map.png'), plot = x, height = 5, 
         width=6.5, units='in')
}

f_global_map_year <- function(x, YEAR){
  
  x %>% 
    mutate(District = factor(District, 
                             levels = c('YAK', 'EKI', 'WKI', 'KSH', 
                                        'KNE', 'KSE', 'KSW', 'C', 'UB', 'KSEM', 
                                        'O', 'Q'))) %>% 
    filter(year %in% c(max(year), max(year) - 1, max(year) - 2)) -> x
  
  f_xmap(x) + 
    geom_point(data=x, aes(set_lon, set_lat, color=District), alpha=.2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    facet_wrap(~FY, dir = 'v') + 
    theme_sleek() + theme(legend.position = 'top') -> x
 
  ggsave(paste0('figs/', YEAR, '/global_year_map.png'), plot = x, 
         height = 8.5, width=6.5, units='in')
}

f_district_map <- function(data, breaks = 2, YEAR){
  # map of all fishing areas from 2009 - current assessment
  # may need to remove some years?
  y = deparse(substitute(data))
  xmap <- f_xmap(data)
  
  if("bed" %in% colnames(data)){
    xmap + geom_point(data=data, aes(set_lon, set_lat, color=Bed), alpha=.2) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~FY, dir = 'v') + 
      scale_x_continuous(breaks = seq(-170,-130, breaks)) +
      theme_sleek() + 
      theme(legend.position = 'top') -> x
  } else{
    xmap + geom_point(data=data, aes(set_lon, set_lat), alpha=.2) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~FY, dir = 'v') + 
      scale_x_continuous(breaks = seq(-170,-130, breaks)) -> x
  }
  
  ggsave(paste0('figs/', YEAR, '/district_', y, '_map.png'), plot = x, height = 8.5, width=6.5, units='in')
}

f_raw_cpue_tbl <- function(data, ghl, YEAR){
  
  y = deparse(substitute(data))
  
  data %>% 
    group_by(FY) %>% 
    summarise(m=sum(meat_weight, na.rm = T), 
              r = sum(round_weight, na.rm = T), 
              h = sum(dredge_hrs, na.rm = T), 
              n = length(dredge_hrs), 
              c1 = m/h, 
              c2 = r/h)  %>% 
    bind_cols(ghl) %>% 
    dplyr::select(FY, ghl, m, r, h, n, c1, c2) %T>%
    write_csv(paste0('tables/', YEAR, '/', y, '_cpue_tbl.csv'))
}

f_gam_mw <- function(data){
  
  adj <- mean(data$mw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    
    fit <- bam(mw_cpue + adj ~ s(depth, k=4, by = Bed) + 
                 te(set_lon, set_lat, by = Bed) + Year +
                 Bed + s(Vessel, bs='re', by=dum), 
               data=data, gamma=1.4, family=Gamma(link=log))
  } else {
    
    fit <- bam(mw_cpue + adj ~ s(depth, k=4) + 
                 te(set_lon, set_lat) + Year + 
                 s(Vessel, bs='re', by=dum), 
               data=data, gamma=1.4, family=Gamma(link=log))
  }
  return(fit)
}

f_gam_rw <- function(data){
  
  adj <- mean(data$rw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    fit <- bam(rw_cpue + adj ~ s(depth, k=4, by = Bed) + 
                 te(set_lon, set_lat, by = Bed) + Year + 
                 Bed + Vessel, data=data, gamma=1.4, family=Gamma(link=log))
  } else {
    fit <- bam(rw_cpue + adj ~ s(depth, k=4) + 
                 te(set_lon, set_lat) + Year + 
                 Vessel, data=data, gamma=1.4, family=Gamma(link=log))
  }
  return(fit)
}

f_std_cpue_tbl <- function(data, data_fit, ghl){
  
  y = deparse(substitute(data))
  adj <- mean(data$rw_cpue, na.rm = T) * 0.10
  
  data_fit %>% 
    group_by(FY) %>% 
    summarise('std cpue' = mean(fit_rw - adj, na.rm = T),
              #m = sum(meat_weight), 
              r = sum(round_weight, na.rm = T), 
              h = sum(dredge_hrs, na.rm = T), 
              n=length(dredge_hrs), 
              #c1 = m/h, 
              c2 = r/h)  %>% 
    bind_cols(ghl) %>% 
    dplyr::select(FY, ghl, r, h, n, c2, `std cpue`) %T>%
    write_csv(paste0('tables/', YEAR, '/', y, '_std_cpue_tbl.csv'))
  
}

pred_data <- function(data, dum = 0){
  
  y = deparse(substitute(data))
  
  if('Bed' %in% colnames(data) && y=='ki'){
          expand.grid(Vessel = unique(data$Vessel), 
                  year = unique(data$year), 
                  bed = c('ek1', 'wk1')) %>% 
        mutate(depth = case_when(bed=='ek1' ~ mode(data$depth[data$bed=='ek1']),
                                  bed=='wk1' ~ mode(data$depth[data$bed=='wk1'])),
              set_lon = case_when(bed=='ek1' ~ mode(data$set_lon[data$bed=='ek1']),
                                  bed=='wk1' ~ mode(data$set_lon[data$bed=='wk1'])),
              set_lat = case_when(bed=='ek1' ~ mode(data$set_lat[data$bed=='ek1']),
                                  bed=='wk1' ~ mode(data$set_lat[data$bed=='wk1'])),
              Year = factor(year)) %>% 
      drop_na %>% 
        mutate(Bed = factor(bed), dum = dum)
     } else if('Bed' %in% colnames(data) && y=='ksh'){
       expand.grid(Vessel = unique(data$Vessel), 
                   year = unique(data$year), bed = 1:2) %>% 
         mutate(depth = case_when(bed==1 ~ mode(data$depth[data$bed==1]),
                                  bed==2 ~ mode(data$depth[data$bed==2])),
                set_lon = case_when(bed==1 ~ mode(data$set_lon[data$bed==1]),
                                    bed==2 ~ mode(data$set_lon[data$bed==2])),
                set_lat = case_when(bed==1 ~ mode(data$set_lat[data$bed==1]),
                                    bed==2 ~ mode(data$set_lat[data$bed==2])),
                Year = factor(year)) %>% 
         drop_na %>% 
         mutate( Bed = case_when(bed==1 ~ '1',
                                 bed==2 ~ '2-7'),
                 dum = dum)
       } else if('Bed' %in% colnames(data) && y=='yak'){ 
       
  expand.grid(Vessel = unique(data$Vessel), 
              year = unique(data$year), bed = 0:6) %>% 
    mutate(depth = case_when(bed==0 ~ FNGr::mode(data$depth[data$bed==0]),
                                bed==1 ~ FNGr::mode(data$depth[data$bed==1]),
                                bed==2 ~ FNGr::mode(data$depth[data$bed==2]),
                                bed==3 ~ FNGr::mode(data$depth[data$bed==3]),
                                bed==4 ~ FNGr::mode(data$depth[data$bed==4]),
                                bed==5 ~ FNGr::mode(data$depth[data$bed==5]),
                                bed==6 ~ FNGr::mode(data$depth[data$bed==6])),
              set_lon = case_when(bed==0 ~ FNGr::mode(data$set_lon[data$bed==0]),
                                  bed==1 ~ FNGr::mode(data$set_lon[data$bed==1]),
                                  bed==2 ~ FNGr::mode(data$set_lon[data$bed==2]),
                                  bed==3 ~ FNGr::mode(data$set_lon[data$bed==3]),
                                  bed==4 ~ FNGr::mode(data$set_lon[data$bed==4]),
                                  bed==5 ~ FNGr::mode(data$set_lon[data$bed==5]),
                                  bed==6 ~ FNGr::mode(data$set_lon[data$bed==6])),
              set_lat = case_when(bed==0 ~ FNGr::mode(data$set_lat[data$bed==0]),
                                  bed==1 ~ FNGr::mode(data$set_lat[data$bed==1]),
                                  bed==2 ~ FNGr::mode(data$set_lat[data$bed==2]),
                                  bed==3 ~ FNGr::mode(data$set_lat[data$bed==3]),
                                  bed==4 ~ FNGr::mode(data$set_lat[data$bed==4]),
                                  bed==5 ~ FNGr::mode(data$set_lat[data$bed==5]),
                                  bed==6 ~ FNGr::mode(data$set_lat[data$bed==6])),
              Year = factor(year)) %>% 
    drop_na %>% 
    mutate( Bed = case_when(bed==0 ~ 'B',
                            bed==1 ~ '1',
                            bed==2 ~ '2',
                            bed==3 ~ '3',
                            bed==4 ~ '4',
                            bed==5 ~ '5',
                            bed==6 ~ '6'),
            dum = dum)
  
   } else if('Bed' %in% colnames(data)){
     expand.grid(Vessel = unique(data$Vessel), 
                 year = unique(data$year), bed = 0:6) %>% 
       mutate(depth = case_when(bed==0 ~ mode(data$depth[data$bed==0]),
                                bed==1 ~ mode(data$depth[data$bed==1]),
                                bed==2 ~ mode(data$depth[data$bed==2]),
                                bed==3 ~ mode(data$depth[data$bed==3]),
                                bed==4 ~ mode(data$depth[data$bed==4]),
                                bed==5 ~ mode(data$depth[data$bed==5]),
                                bed==6 ~ mode(data$depth[data$bed==6])),
              set_lon = case_when(bed==0 ~ mode(data$set_lon[data$bed==0]),
                                  bed==1 ~ mode(data$set_lon[data$bed==1]),
                                  bed==2 ~ mode(data$set_lon[data$bed==2]),
                                  bed==3 ~ mode(data$set_lon[data$bed==3]),
                                  bed==4 ~ mode(data$set_lon[data$bed==4]),
                                  bed==5 ~ mode(data$set_lon[data$bed==5]),
                                  bed==6 ~ mode(data$set_lon[data$bed==6])),
              set_lat = case_when(bed==0 ~ mode(data$set_lat[data$bed==0]),
                                  bed==1 ~ mode(data$set_lat[data$bed==1]),
                                  bed==2 ~ mode(data$set_lat[data$bed==2]),
                                  bed==3 ~ mode(data$set_lat[data$bed==3]),
                                  bed==4 ~ mode(data$set_lat[data$bed==4]),
                                  bed==5 ~ mode(data$set_lat[data$bed==5]),
                                  bed==6 ~ mode(data$set_lat[data$bed==6])),
              Year = factor(year)) %>% 
       drop_na %>% 
       mutate( Bed = factor(bed),
               dum = dum)
     } else {
    expand.grid(Vessel = unique(data$Vessel), 
                dum = dum, 
                year = unique(data$year)) %>% 
      mutate(   depth = mode(data$depth),
                set_lon = mode(data$set_lon),
                set_lat = mode(data$set_lat),
                Year = factor(year)) %>% 
      drop_na  
}
}

fig_mw_cpue <- function(data){
  
  y = deparse(substitute(data))
  
  filter(data, year>(YEAR-6)) %>% 
    droplevels(.) -> data
  
  if(substr(y, nchar(y)-3, nchar(y)) != '_fit'){
    
    data %>% 
      ggplot(aes(mw_cpue, fill=Year, color=Year)) + 
      geom_density() + 
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
      theme(legend.key = element_blank(),
            legend.position = c(0.8, 0.85),
            legend.background = element_rect(fill = "#ffffffaa", colour = NA),
            legend.key.height=unit(.6,'line')) +
      xlab('meat wt cpue') -> a
    
    data %>% 
      ggplot(aes(mw_cpue, fill=Year, color=Year)) + 
      geom_histogram(bins = 50) +
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(fill=FALSE, color=FALSE) + 
      theme(legend.key = element_blank()) +
      xlab('meat wt cpue') -> b
    
    if('Bed' %in% colnames(data) & length(unique(data$Bed))>1){
      
      a + 
        facet_grid(Bed~., scales='free_y') -> a
      b + 
        facet_grid(Bed~., scales='free_y') -> b
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_mw_cpue.png'), plot = x, height = 8.5, 
             width=6.5, units='in')
      
    } else {
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_mw_cpue.png'), plot = x, height = 4.5, 
             width=6.5, units='in')
      
    }
  } else {
    
    data %>% 
      ggplot(aes(fit_mw, fill=Year, color=Year)) + 
      geom_density() + 
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
      theme(legend.key = element_blank(),
            legend.position = c(0.8, 0.85),
            legend.background = element_rect(fill = "#ffffffaa", colour = NA),
            legend.key.height=unit(.6,'line')) +
      xlab('standardized meat wt cpue')  -> a
    
    data %>% 
      ggplot(aes(fit_mw, fill=Year, color=Year)) + 
      geom_histogram(bins = 50) +
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(fill=FALSE, color=FALSE) + 
      theme(legend.key = element_blank()) +
      xlab('standardized meat wt cpue') -> b
    
    if('Bed' %in% colnames(data) & length(unique(data$Bed))>1){
      
      a + 
         facet_grid(Bed~., scales='free_y') -> a
      b + 
        facet_grid(Bed~., scales='free_y') -> b
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_mw.png'), plot = x, height = 8.5, 
             width=6.5, units='in')
      
    } else {
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_mw.png'), plot = x, height = 4.5, 
             width=6.5, units='in')
      
    } 
  }
  x
}
      
fig_rw_cpue <- function(data){
  
  y = deparse(substitute(data))
  
  filter(data, year>(YEAR-6)) %>% 
    droplevels(.) -> data
  
  if(substr(y, nchar(y)-3, nchar(y)) != '_fit'){
  
    data %>% 
      ggplot(aes(rw_cpue, fill=Year, color=Year)) + 
      geom_density() + 
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
      theme(legend.key = element_blank(),
            legend.position = c(0.8, 0.85),
            legend.background = element_rect(fill = "#ffffffaa", colour = NA),
            legend.key.height=unit(.6,'line')) +
      xlab('round wt cpue') -> a
    
    data %>% 
      ggplot(aes(rw_cpue, fill=Year, color=Year)) + 
      geom_histogram(bins = 50) +
      scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
      scale_color_manual(values=values, breaks=levels(data$Year), name='') +
      guides(fill=FALSE, color=FALSE) + 
      theme(legend.key = element_blank()) +
      xlab('round wt cpue') -> b
  
      if('Bed' %in% colnames(data) & length(unique(data$Bed))>1){
        
        a + 
          facet_grid(Bed~., scales='free_y') -> a
        b + 
          facet_grid(Bed~., scales='free_y') -> b
        
        x <- grid.arrange(a, b, ncol=2)
        
        ggsave(paste0('figs/', YEAR, '/', y, '_rw_cpue.png'), plot = x, height = 8.5, 
               width=6.5, units='in')
        
      } else {
        
        x <- grid.arrange(a, b, ncol=2)
        
        ggsave(paste0('figs/', YEAR, '/', y, '_rw_cpue.png'), plot = x, height = 4.5, 
               width=6.5, units='in')
        
      }
    } else {
      
      data %>% 
        ggplot(aes(fit_rw, fill=Year, color=Year)) + 
        geom_density() + 
        scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
        scale_color_manual(values=values, breaks=levels(data$Year), name='') +
        guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
        theme(legend.key = element_blank(),
              legend.position = c(0.8, 0.85),
              legend.background = element_rect(fill = "#ffffffaa", colour = NA),
              legend.key.height=unit(.6,'line')) +
        xlab('standardized round wt cpue') -> a
      
      data %>% 
        ggplot(aes(fit_rw, fill=Year, color=Year)) + 
        geom_histogram(bins = 50) +
        scale_fill_manual(values=values, breaks=levels(data$Year), name='') +
        scale_color_manual(values=values, breaks=levels(data$Year), name='') +
        guides(fill=FALSE, color=FALSE) + 
        theme(legend.key = element_blank()) +
        xlab('standardized round wt cpue') -> b
      
    if('Bed' %in% colnames(data) & length(unique(data$Bed))>1){
      
      a + 
        facet_grid(Bed~., scales='free_y') -> a
      b + 
        facet_grid(Bed~., scales='free_y') -> b
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_rw.png'), plot = x, height = 8.5, 
             width=6.5, units='in')
      
    } else {
      
      x <- grid.arrange(a, b, ncol=2)
      
      ggsave(paste0('figs/', YEAR, '/', y, '_rw.png'), plot = x, height = 4.5, 
             width=6.5, units='in')
      
    } 
  }
  x
}

fig_roll_rw <- function(fit){
  
  y = deparse(substitute(fit))
  
  adj <- mean(fit$rw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(fit)){

  fit %>%
    mutate(mean = mean(fit_rw - adj, na.rm = T),
           last5 = mean(.$fit_rw[.$year>(YEAR-5)] - adj, na.rm=T)) %>% 
    group_by(year, Bed) %>%
    summarise(rw = mean(fit_rw - adj, na.rm = T),
              mean = mean(mean),
              last5 = mean(last5, na.rm = T)) %>% 
    mutate(last5 = ifelse(year>=YEAR-6, last5, NA)) %>%
    ggplot(aes(year, rw, color = Bed)) + 
    geom_line() +
    geom_point() +
    geom_line(aes(y = mean), lty = 2, color = 1) +
    geom_line(aes(y = last5), lty = 3, color = 1) +
    xlab('Year') +
    ylab('Standardized round wt CPUE') -> x
    
  } else {
    
    fit %>%
      mutate(mean = mean(fit_rw - adj, na.rm = T),
             last5 = mean(.$fit_rw[.$year>(YEAR-5)] - adj, na.rm=T)) %>% 
      group_by(year) %>%
      summarise(rw = mean(fit_rw - adj, na.rm = T),
                mean = mean(mean),
                last5 = mean(last5, na.rm = T)) %>% 
      mutate(last5 = ifelse(year>=YEAR-6, last5, NA)) %>%
      ggplot(aes(year, rw)) + 
      geom_line() +
      geom_line(aes(y = mean), lty = 2, color = 1) +
      geom_line(aes(y = last5), lty = 3, color = 1) +
      xlab('Year') +
      ylab('Standardized round wt CPUE') -> x
  }
  
  ggsave(paste0('figs/', YEAR, '/', y, '_roll_rw.png'), plot=x, height=4.5,
         width=6.5, units='in')
  x
}

fig_rw_vessel <- function(data){
  
    y = deparse(substitute(data))
  
  ggplot(data, aes(Year, fit_rw, fill=Vessel)) + 
    geom_boxplot(width=.35) + 
    ylab('Standardized CPUE') -> x

  ggsave(paste0('figs/', YEAR, '/', y,'_vessel.png'), plot = x, 
         height = 4.5, width=6.5, units='in')
  x
}

fig_sh <- function(data){
  
    y = deparse(substitute(data))
  
  data.sh <- merge(data, sh, by=c('Fishery', 'District','Haul_ID','ADFG')) 
  
  data.sh %>% 
    filter(year>(YEAR-6)) %>% 
    droplevels(.) %>% 
    ggplot(aes(Shell_Height, fill=Year, color=Year)) + 
    geom_density() + 
    scale_fill_manual(values=values, breaks=levels(data.sh$Year), name='Year') +
    scale_color_manual(values=values, breaks=levels(data.sh$Year), name='Year') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
    theme( legend.position = c(0.2, 0.15)) +
    xlab('Shell height (mm)') + 
    ylab('Density') -> s1
  
  data.sh %>% 
    filter(year>(YEAR-6)) %>% 
    ggplot(aes(Shell_Height, fill=Year, color=Year)) +
    geom_histogram(bins=50) + 
    scale_fill_manual(values=values, breaks=levels(data.sh$Year), name='Year') +
    scale_color_manual(values=values, breaks=levels(data.sh$Year), name='Year') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    guides( fill=FALSE, color=FALSE) + 
    theme(legend.key = element_blank())+
    xlab('Shell height (mm)') + 
    ylab('Count') -> s2
  
  if('Bed' %in% colnames(data) & length(unique(data$Bed))>1 | !('wk1' %in% data$Bed)){
    s1 + 
      facet_grid(Bed~.) -> s1
    s2 + 
      facet_grid(Bed~.) -> s2
    
    x = arrangeGrob(s1, s2, ncol=2)
    
      ggsave(paste0('figs/', YEAR, '/', y,'_sh.png'), plot = x, 
             height = 8, width=6.5, units='in')
  } else {
  
  x = arrangeGrob(s1, s2, ncol=2)

      ggsave(paste0('figs/', YEAR, '/', y,'_sh.png'), plot = x, 
             height = 5, width=6.5, units='in')
  }
  
  x
}

fig_sh_year <- function(data){
  
    y = deparse(substitute(data))
  
  data.sh <- full_join(data, sh, by=c('Fishery', 'District','Haul_ID','ADFG'))
  
  data.sh %>% 
    filter(year==(YEAR-1)) %>% 
    droplevels(.) %>% 
    ggplot(aes(Shell_Height, fill=Rtnd_Disc, color=Rtnd_Disc)) +
    scale_color_manual( values=c('D'='#e41a1c', 'R'='#377eb8'), 
                        labels=c('discard', 'retain'), name='') +
    scale_fill_manual( values=c('D'='#e41a1c', 'R'='#377eb8'), 
                       labels=c('discard', 'retain'), name='') -> fig 
  
  fig + 
    geom_density(alpha=.2) + 
    theme(legend.key = element_blank(), 
          legend.position = c(0.2, 0.75), 
          legend.background = element_rect(fill = "#ffffffaa", colour = NA),
          legend.key.height=unit(.6,'line')) -> s1
  
  fig +
    geom_histogram(alpha=.2, bins = 50) + 
    guides( fill=FALSE, color=FALSE) -> s2
  
   
  if('Bed' %in% colnames(data) & length(unique(data$Bed))>1){
    
  s1 + 
    facet_grid(Bed~., scales = 'free_y') -> s1
    
    
  s2 +
    facet_grid(Bed~., scales = 'free_y') -> s2

  x = grid.arrange(s1, s2, ncol=2)
  
  ggsave(paste0('figs/', YEAR, '/', y,'_sh_year.png'), plot = x, 
         height = 8, width=6.5, units='in')

  } else {

    x = grid.arrange(s1, s2, ncol=2)
    
    ggsave(paste0('figs/', YEAR, '/', y,'_sh_year.png'), plot = x, 
           height = 4.5, width=6.5, units='in')
  }
  x
}

fig_rd <- function(data){
  
  y = deparse(substitute(data))
  
  data.sh <- merge(data, sh, by=c('Fishery', 'District','Haul_ID','ADFG'))
  data.sh %>% 
    ggplot(aes(Shell_Height, fill=Rtnd_Disc,color=Rtnd_Disc)) + 
     facet_grid(FY~.) +
    scale_color_manual(values=c('D'='#e41a1c', 'R'='#377eb8'), 
                        labels=c('discard', 'retain'), name='') +
    scale_fill_manual(values=c('D'='#e41a1c', 'R'='#377eb8'), 
                       labels=c('discard', 'retain'), name='') + 
    theme(legend.key = element_blank(),
          legend.position = c(0.2, 0.18),
          legend.background = element_rect(fill = "#ffffffaa", colour = NA),
          legend.key.height = unit(.6,'line')) -> fig 
  
  fig + 
    geom_density(alpha=.2) +
    xlab('Shell height (mm)') + 
    ylab('Density') -> s1
  
  fig + 
    geom_histogram(alpha=.2, bins = 50) +
    xlab('Shell height (mm)') + 
    ylab('Count') +
    guides( fill=FALSE, color=FALSE) -> s2
  
  x = grid.arrange(s1, s2, ncol=2)
  
  ggsave(paste0('figs/', YEAR, '/', y, '_rd.png'), plot = x, 
         height = 8, width=6.5, units='in')
  x
  
}

fig_bycatch <- function(data){
  
    y = deparse(substitute(data))
  
  data %>% 
    left_join(by) %>% 
    group_by(year) %>% 
    summarise(Halibut_Count = sum(Halibut_Count),
              Tanner_Count = sum(Tanner_Count),
              Opilio_Count = sum(Opilio_Count),
              King_Count = sum(King_Count),
              Sample_Hrs = sum(Sample_Hrs),
              dredge_hrs = sum(dredge_hrs)) %>% 
    mutate(hal = Halibut_Count/Sample_Hrs * dredge_hrs,
           tan = Tanner_Count/Sample_Hrs * dredge_hrs,
           opi = Opilio_Count/Sample_Hrs * dredge_hrs,
           king = King_Count/Sample_Hrs * dredge_hrs) %>% 
    dplyr::select(year, hal, tan, opi, king) %>% 
    gather(variable, value,-year) %>% 
    ggplot(aes(year, value, color=variable, group=variable)) + 
    geom_line() + 
    geom_point(size=2) +
    scale_y_continuous(labels = comma, name = "Numbers") +
    scale_x_continuous(breaks = 2009:YEAR) +
    scale_color_discrete(name="species",
                         breaks=c("tan", "hal", "opi", 'king'),
                         labels=c("Tanner", "Halibut", "Opilio", "King")) -> x

    ggsave(paste0('figs/', YEAR, '/', y,'_by.png'), plot = x, height = 5, 
         width=6.5, units='in')

    x
}

fig_discard <- function(data){
  
  y = deparse(substitute(data))
  
  by %>% 
    mutate(District = case_when(District %in% c('EKI', 'WKI') ~ "KI",
                                District=="UB" ~ "M",
                                TRUE ~ District)) %>% 
    filter(District==toupper(y)) %>% 
    group_by(year) %>% 
    summarise(sample_hrs = sum(Sample_Hrs), 
              disc = sum(Discard_Weight ), 
              broken = sum(Broken_Weight),
              rem_disc = sum(Rem_Disc_Wt), 
              wt = sum(disc, broken, rem_disc) ) -> a
  data %>% 
    group_by(year) %>% 
    summarise(dredge_hrs = sum(dredge_hrs), 
              round_wt = sum(round_weight)) -> b
  
  left_join(a, b) %>% 
    mutate(disc = wt / sample_hrs * dredge_hrs, 
           per = disc / round_wt * 100)  %>% 
    ggplot(aes(year, disc)) + 
    geom_line() + 
    geom_point(size=2) + 
    ylab('Scallop discard weight (lbs)') +
    scale_y_continuous(labels = comma) + 
    expand_limits(y = 0) +
    scale_x_continuous(breaks = 2009:YEAR) -> x
  
  ggsave(paste0('figs/', YEAR, '/', y,'_dis.png'), plot = x, height = 5, 
         width=6.5, units='in')
  
  x
}

fig_discard_rat <- function(data){
  
  y = deparse(substitute(data))
  
  by %>% 
    mutate(District = case_when(District %in% c('EKI', 'WKI') ~ "KI",
                                District=="UB" ~ "M",
                                TRUE ~ District)) %>% 
    filter(District==toupper(y)) %>% 
    group_by(year) %>% 
    summarise(sample_hrs = sum(Sample_Hrs, na.rm=T), 
              disc = sum(Discard_Weight, na.rm=T ), 
              broken = sum(Broken_Weight, na.rm=T),
              rem_disc = sum(Rem_Disc_Wt, na.rm=T), 
              wt = sum(disc, broken, rem_disc)) -> a
  data %>% 
    group_by(year) %>% 
    summarise(dredge_hrs = sum(dredge_hrs, na.rm=T), 
              round_wt = sum(round_weight, na.rm=T)) -> b
  
  left_join(a, b) %>% 
    mutate(disc = wt / sample_hrs * dredge_hrs, 
           per = disc / round_wt * 100)  %>% 
    ggplot(aes(year, per)) + 
    geom_line() + 
    geom_point(size=2) + 
    ylab('Scallop discard ratio') +
    scale_y_continuous(labels = comma) + 
    expand_limits(y = 0) +
    scale_x_continuous(breaks = 2009:YEAR) -> x
  
  ggsave(paste0('figs/', YEAR, '/', y,'_disrat.png'), plot = x, height = 5, 
         width=6.5, units='in')
  
  x
}

fig_bycatch_rat <- function(data){
 
   y = deparse(substitute(data))
 
  by %>% 
    mutate(District = case_when(District %in% c('EKI', 'WKI') ~ "KI",
                                District=='UB' ~ "M",
                                TRUE ~ District)) %>% 
    filter(District==toupper(y)) %>% 
    group_by(year) %>% 
    summarise(Halibut_Count = sum(Halibut_Count),
              Tanner_Count = sum(Tanner_Count),
              Opilio_Count = sum(Opilio_Count),
              King_Count = sum(King_Count),
              sample_hrs = sum(Sample_Hrs, na.rm=T), 
              disc = sum(Discard_Weight, na.rm=T ), 
              broken = sum(Broken_Weight, na.rm=T),
              rem_disc = sum(Rem_Disc_Wt, na.rm=T), 
              wt = sum(disc, broken, rem_disc) ) -> a
  data %>% 
    group_by(year) %>% 
    summarise(dredge_hrs = sum(dredge_hrs, na.rm=T), 
              round_wt = sum(round_weight, na.rm=T)) -> b
  
  left_join(a, b) %>% 
    mutate(hal = Halibut_Count/ dredge_hrs * 100,
           tan = Tanner_Count/dredge_hrs * 100,
           opi = Opilio_Count/dredge_hrs * 100,
           king = King_Count/dredge_hrs * 100) %>% 
    dplyr::select(year, hal, tan, opi, king) %>% 
    gather(variable, value,-year) %>% 
    ggplot(aes(year, value, color=variable, group=variable)) + 
    geom_line() +
    geom_point(size=3) +
    ylab('Bycatch discard ratio - bycatch / dredge_hrs') +
    scale_y_continuous(labels = comma) + 
    expand_limits(y = 0) +
    scale_x_continuous(breaks = 2009:YEAR) -> x
  
  ggsave(paste0('figs/', YEAR, '/', y,'_byrat.png'), plot = x, height = 5, 
         width=6.5, units='in')
  x
}

fig_clap <- function(data){
  
    y = deparse(substitute(data))
  
  by %>% 
    mutate(District = case_when(District %in% c('EKI', 'WKI') ~ "KI",
                                District=='UB' ~ "M",
                                TRUE ~ District)) %>% 
    filter(District==toupper(y)) %>% 
    group_by(year) %>% 
    summarise(sample_hrs = sum(Sample_Hrs, na.rm=T), 
              disc = sum(Clapper_Count, na.rm=T ), 
              broken = sum(Broken_Weight, na.rm=T),
              rem_disc = sum(Rem_Disc_Wt, na.rm=T), 
              wt = sum(disc, broken, rem_disc) ) -> a
  data %>% 
    group_by(year) %>% 
    summarise(dredge_hrs = sum(dredge_hrs, na.rm=T), 
              round_wt = sum(round_weight, na.rm=T)) -> b
  
  left_join(a, b) %>% 
    mutate( per = disc / round_wt * 100)  %>% 
    ggplot(aes(year, per)) + 
    geom_line() + 
    geom_point(size=3) + 
    ylab('Clapper ratio - clapper/round wt') +
    scale_y_continuous(labels = comma) + expand_limits(y = 0) +
    scale_x_continuous(breaks = 2009:YEAR) -> x
    
  ggsave(paste0('figs/', YEAR, '/', y,'_clap.png'), plot = x, height = 4.5, 
         width=6.5, units='in')
  x
}

fig_shmw <- function(data){
  
  y = deparse(substitute(data)) %>% 
    toupper
  
  shmw %>% 
    mutate(District = case_when(District %in% c('EKI', 'WKI') ~ "KI",
                                District=='UB' ~ "M",
                                TRUE ~ District)) %>% 
    filter(District==y) %>% 
    mutate(Year = factor(Year),
           Sex = factor(sex)) %>% 
    ggplot(aes(shell_height, meat_weight_g)) + 
      geom_point(color = 'lightgray') +
    stat_smooth(aes(color = Sex, fill = Sex), method = 'gam', formula = y~s(x, k=4), alpha = 0.2) +
    xlab('\nShell height (mm)') +
    ylab('Meat weight (g)\n') +
    theme(legend.key = element_blank(), 
          legend.position = c(0.2, 0.85), 
          legend.background = element_rect(fill = "#ffffffaa", colour = NA),
          legend.key.height=unit(.6,'line')) +
    expand_limits(y = 0) -> x
    
  
  ggsave(paste0('figs/', YEAR, '/', y,'_shmw.png'), plot = x, height = 4.5, 
         width=6.5, units='in')
  x
    
}

fig_shmw_all <- function(data){
  
  shmw %>% 
    mutate(Year = factor(Year),
           Sex = factor(sex)) %>% 
    ggplot(aes(shell_height, meat_weight_g)) + 
    geom_point(color = 'lightgray') +
    stat_smooth(aes(color = District, fill = District), method = 'gam', 
                formula = y~s(x, k=4), alpha = 0.2) +
    xlab('\nShell height (mm)') +
    ylab('Meat weight (g)\n') +
    theme(legend.key = element_blank(), 
          legend.position = c(0.2, 0.80), 
          legend.background = element_rect(fill = "#ffffffaa", colour = NA),
          legend.key.height=unit(.6,'line')) +
    expand_limits(y = 0) -> x
  
  
  ggsave(paste0('figs/', YEAR, '/', 'all_shmw.png'), plot = x, height = 4.5, 
         width=6.5, units='in')
  x
  
}

fig_shmw_date <- function(data){
  
  shmw %>% 
    mutate(Year = factor(Year),
           Sex = factor(sex)) %>% 
    ggplot(aes(date, meat_weight_g, color = District, fill = District)) + 
    geom_point() +
    stat_smooth(aes(), method = 'gam', 
                formula = y~s(x, k=4), alpha = 0.2) +
    xlab('\nShell height (mm)') +
    ylab('Meat weight (g)\n') +
    theme(legend.key = element_blank(), 
          legend.position = c(0.2, 0.80), 
          legend.background = element_rect(fill = "#ffffffaa", colour = NA),
          legend.key.height=unit(.6,'line')) +
    expand_limits(y = 0) -> x
  
  
  ggsave(paste0('figs/', YEAR, '/', 'date_shmw.png'), plot = x, height = 4.5, 
         width=6.5, units='in')
  x
  
}
