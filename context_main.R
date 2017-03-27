
# this scipt does following
# 1: it summarizes appliance usage in tabular fomat. which shows the frequency of appliance usage in different day slots across days
library(xts)
library(data.table)
library(ggplot2)
library(gtools)
#library(fasttime)
rm(list=ls())



file1 <- "101.csv"
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Localize/context_support.R")
df <- fread(paste0(path1,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)

df_sub <- df_xts["2014-06-01/2014-06-30"]
show_usage_stats(df_sub,dayslots=8)

df_sub <- df_xts["2014-07-01/2014-07-30"]
show_usage_stats(df_sub,dayslots=8)

df_sub <- df_xts["2014-08-01/2014-08-30"]
show_usage_stats(df_sub,dayslots=8)

weekend_data <- df_sub[lubridate::wday(df_sub) %in% c(1,7)]
weekday_data <- df_sub[!lubridate::wday(df_sub) %in% c(1,7)]
show_usage_stats(weekday_data,dayslots=8)
show_usage_stats(weekend_data,dayslots=8)
#%%%%%%%%%%%%%%%%%%%%%%%
df <- fread(paste0(path1,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)
df_sub <- df_xts["2014-06-01/2014-06-30"]
dat <- df_sub$range1
appliance_name <- colnames(dat)
colnames(dat) <- c("consumption_val")
dat_day <- split.xts(dat,f="days",k=1)

#dat <- dat_day
result <- list()
for (i in 1:length(dat_day)) { # run for each day separately
  dat <- dat_day[[i]] # temperoray dat variable for each new day
  dat$consumption_binary <- ifelse(dat$consumption_val > 10, 1, 0)
  dat$group_id <- rleid(dat$consumption_binary)
  dat_temp <- dat[dat$consumption_binary%in%c(1),] # only consumption instances and filtering out non-usage timings
  
  diff_slots <- unique(dat_temp$group_id) # number of unique groups
  temp <- lapply(diff_slots,function(x) dat_temp[dat_temp$group_id%in%c(x),]) # subset on basis of groups
  
  result[[i]] <- do.call(rbind,lapply(temp,function(x) return (data.frame(duration=NROW(x),start_time=index(xts::first(x)),end_time = index(xts::last(x)))) ))
  
}
final_result <- do.call(rbind,result)
hist(final_result$duration)


ggplot(final_result,aes(duration))+ geom_histogram(binwidth = 10) +
  scale_x_continuous(breaks=seq(min(final_result$duration),max(final_result$duration),by=10))


