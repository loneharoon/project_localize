path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/commercial/EnerNOC/csv-only/csv/"
filen <- list.files(path,pattern = "*.csv")


for(i in 1:length(filen)){
  
  df <- fread(paste0(path,filen[i]))
  df <- df[,c(2,3)]
  colnames(df) <- c("localminute","power")
  temp <- df
  temp$localminute <- as.POSIXct(strptime(temp$localminute,format="%Y-%m-%d %H:%M:%S"))
  temp_xts <- xts(temp$power,temp$localminute)
  temp_hourly <- resample_data(temp_xts,60)
  dframe <- data.frame(localminute=index(temp_hourly),use=round(coredata(temp_hourly),2))
  write.csv(dframe,file = paste0(path,"hourly/",filen[i]),row.names = FALSE)
}


temp_hourly <- resample_data(temp_xts,60)
temp_xts <- temp_hourly
dframe <- data.frame(localminute=index(temp_xts),use=coredata(temp_xts))
write.csv(dframe,file = paste0(path,"lbnl74_cleaned_hourly.csv"),row.names = FALSE)

resample_data <- function(xts_datap,xminutes) {
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60-3600*0.5) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
}