
split_hourwise_approach2 <- function(datas,dayfrequency,daygroups){
  #http://stackoverflow.com/a/37145960/3317829
  # dayfrequency= no. of observations per day
  # daygroups = no. of equal splits required
  stopifnot(dayfrequency%%daygroups==0)
  datas2 = split(datas,rep(1:daygroups,each = dayfrequency/daygroups))
  return(datas2)
}

show_usage_stats <- function(df_xts,dayslots) {
  df_xts2<-subset(df_xts,select=-use)#removing agg. usage
  df_day <- split.xts(df_xts2,f="days",k=1)
  
  day_usg <- lapply(df_day,function(x) {
    hour_split <- split_hourwise_approach2(x,dayfrequency = 144, daygroups= dayslots)
    group_names <- LETTERS[1:length(hour_split)]
    gp_ans <- list()
    for(gp in 1:length(group_names)){
      gp_ans[[gp]] <- apply(as.matrix(coredata(hour_split[[gp]])),2,function(x) { 
        #  browser()
        if(any(x > 10)) 
          return(group_names[gp])
        else
          return(NA)
      }
      )
    }
    gp_day <- do.call(rbind,gp_ans)
    return(gp_day)
  })
  
  usg_stat <- as.data.frame(do.call(rbind, day_usg))
  appliances <- colnames(usg_stat)
  home_details <- lapply(appliances,function(x){
    temp <- plyr::count(usg_stat,x)
    colnames(temp) <- c("slot",colnames(temp)[1])
    return(temp)
  })
  
  bind_days <- do.call(plyr::rbind.fill,home_details)
  res <- plyr::ddply(bind_days,"slot",plyr::numcolwise(sum,na.rm=TRUE))
  res <- res[!is.na(res$slot),]
  #browser()
  res$slot <- factor(res$slot,levels = LETTERS[1:NROW(res)] )
  res <- res[order(res$slot),]
  row.names(res)<- c()
  return(res)
}

