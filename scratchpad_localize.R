library(xts)
library(data.table)
library(ggplot2)
library(gtools)
#library(fasttime)
rm(list=ls())

file1 <- "115.csv"
#path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
path2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/default/" 
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Localize/context_support.R")
df <- fread(paste0(path2,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)


# VISUALIZE ALL APPLAINCES ONCE
df_sub <- df_xts["2014-06-01/2014-08-30"]
appliances <- colnames(df_sub)[!colnames(df_sub)%in%c("use")] # removing aggregate usage from further analysis
appliance_stat <- lapply(appliances,function(x) compute_appliance_histogram(df_sub[,x]))
df_long <- data.frame(appliance=character(),duration=numeric())
for (i in 1:length(appliance_stat)) {
  temp <- data.frame(appliance=appliances[i],duration=appliance_stat[[i]]$duration)
  df_long <- rbind(df_long,temp)
}
#ggplot(df_long,aes(duration)) + geom_density() + facet_wrap(~appliance,scales="free")
ggplot(df_long,aes(duration)) + geom_histogram(binwidth=5) + facet_wrap(~appliance,scales="free")

#VISUALIZE APPLIANCE OVER A ROLLING WINDOW
df_sub <- df_xts["2014-06-01/2014-08-30"]
appliances # display available applainces
dat_app <- df_sub$refrigerator1
windowsize <- 30 # no. of days
show_single_appliance_histogram(dat_app,windowsize)


# visualise usage of single applaince daywise in particualar time window
df_sub <- df_xts["2014-06-01/2014-06-20"]
appliances # display available applainces
dat_app <- df_sub$refrigerator1
hour_start = 18
hour_end =  6
show_appliance_consumption_trace(dat_app,hour_start,hour_end)

show_appliance_consumption_trace <- function(dat,hour_start,hour_end) {
#dat <- df_sub$refrigerator1
names(dat) <- "Power"
sel_dat <- dat[.indexhour(dat) >= hour_start | .indexhour(dat) < hour_end,]
sel_dat$day <- lubridate::day(index(sel_dat))
day_dat <- split.xts(sel_dat,"days",k=1)
day_dat <- lapply(day_dat,function(x) { #keep same time stamp  for all days
  index(x) <- index(day_dat[[1]]) 
  return(x)})
df_long <- do.call(rbind,day_dat)
ggplot(fortify(df_long),aes(Index,Power)) + geom_line() + facet_wrap(~day) +
 scale_x_datetime(labels=date_format("%H",tz="Asia/Kolkata"))

}




compute_appliance_histogram <- function(dat) {
  appliance_name <- colnames(dat)
  colnames(dat) <- c("consumption_val")
  dat_day <- split.xts(dat,f="days",k=1)
  
  #dat <- dat_day
  result <- list()
  for (i in 1:length(dat_day)) { # run for each day separately
    dat <- dat_day[[i]] # temperoray dat variable for each new day
    dat$consumption_binary <- ifelse(dat$consumption_val > 5, 1, 0)
    dat$group_id <- rleid(dat$consumption_binary)
    dat_temp <- dat[dat$consumption_binary%in%c(1),] # only consumption instances and filtering out non-usage timings
    
    diff_slots <- unique(dat_temp$group_id) # number of unique groups
    temp <- lapply(diff_slots,function(x) dat_temp[dat_temp$group_id%in%c(x),]) # subset on basis of groups
    
    result[[i]] <- do.call(rbind,lapply(temp,function(x) return (data.frame(duration=NROW(x),start_time=index(xts::first(x)),end_time = index(xts::last(x)))) ))
    
  }
  final_result <- do.call(rbind,result)
  # names(final_result) <- appliance_name
  return(final_result)
}
show_single_appliance_histogram <- function(dat_app,windowsize) {
day_dat <- split.xts(dat_app,"days",k=windowsize)
windowname <- c(paste0("window:",1:length(day_dat)))
appliance_stat <- lapply(day_dat,function(x) compute_appliance_histogram(x))
df_long <- data.frame(window=character(),duration=numeric())
for (i in 1:length(appliance_stat)) {
  if(is.null(appliance_stat[[i]]$duration)){
    temp <- data.frame(window = windowname[i],duration = NA )
  }else{
    temp <- data.frame(window = windowname[i],duration = appliance_stat[[i]]$duration)
  }
  df_long <- rbind(df_long,temp)
}
#ggplot(df_long,aes(duration)) + geom_density() + facet_wrap(~window,scales="free")
ggplot(df_long,aes(duration)) + geom_histogram(binwidth=5) + facet_wrap(~window,scales="free")
}