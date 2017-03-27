library(xts)
library(data.table)
library(ggplot2)
library(gtools)

################# DATAPORT FORECASTING PERFORMANCE########
dataport_prediction_part <- function() {
  # this function does two tasks [currently first one is in hidden mode]
  # 1: stores only prediction accuracy results of all homes using both methods in a csv
  # 2: stores prediction along with actual data in files corresponding to each home 
setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
fls <- mixedsort(list.files(path1,pattern = ".csv"))
file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")

day_pred_result <- list()

for (i in 2:length(fls)) {
  file1 <- fls[i]
  data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
  #appliance_features <- get_appliance_features(path1, file1)
  print("DATA RANGES ARE:")
  print(paste0("Power data,","start: ",index(first(data_ob$power_data))," end: ",index(last(data_ob$power_data))))
  print(paste0("Weather data,","start: ",index(first(data_ob$weather_data))," end: ",index(last(data_ob$weather_data))))
  merge_start_date <- as.POSIXct(strptime('2014-06-05',format = "%Y-%m-%d"))
  merge_end_date   <- as.POSIXct(strptime('2014-08-31',format = "%Y-%m-%d"))
  confirm_validity(data_ob, merge_start_date, merge_end_date)
  my_range <- paste0(merge_start_date,'/',merge_end_date)
  sampled_ob <- combine_energy_weather(data_ob,my_range)
  train_data <- sampled_ob['2014-06-05/2014-06-21']
  test_data <- sampled_ob['2014-06-22/2014-08-30']
  #####without weather variables: next 4 lines
  sampled_ob2 <- subset(sampled_ob,select=power)
  train_data2 <- sampled_ob2['2014-01-01/2014-01-15']
  test_data2 <- sampled_ob2['2014-01-16/2014-03-30']
  neural_result2 <- neuralnetwork_procedure_withoutweather (train_data2,test_data2,hourwindow = 6, daywindow = 15)
  ##########################
  #regression_days <- sampled_ob['2014-06-26/2014-07-30']
  #find_regrassivedays_with_BIC(regression_days) # USED TO FIND NO. OF BEST HISTORICAL DAYS FOR REGRESSION
  #reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
  #print("Regression done")
 
  neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
  #print("N-network done")
  comb_df <- cbind(test_data$power,neural_result)
  comb_temp <- data.frame(timestamp = index(comb_df),round(coredata(comb_df),2))
  write.csv(comb_temp,paste0("dataport_prediction/",file1),row.names=FALSE)
  
  # NEXT THREE LINES ARE USED TO STORE THE PREDICTION RESULT OF REGRESSION AND NERUAL NETWORKS IN A LIST
 #l <- as.data.frame(plot_table(test_data,reg_result$fit,neural_result$fit,intent = "return"))
 #l$home <- strsplit(file1,'[.]')[[1]][1]
 # day_pred_result[[i]] <- l
 # #################
  #INCREMENTAL LOF APPROACH
  lof_ans <- incremental_lof_procedure(train_data,test_data,hourwindow = 1, daywindow = 10)
  pathy <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  anom_readings <- lof_ans[lof_ans > 0.99]
  write.csv(x = fortify(anom_readings),file = paste0(pathy,"Lof_",file1),row.names = FALSE)
}
#agg_result <- do.call(rbind,day_pred_result)
#write.csv(agg_result,file="prediction_result.csv")
}

dataport_anomaly_detection_part <- function() {
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  pathxx <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_prediction/"
  pathy <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  fls <- mixedsort(list.files(pathxx,pattern = ".csv"))
  for (i in 2:length(fls)) {
    file1 <- fls[i]
    data_ob <- fread(paste0(pathxx,file1))
    data_xts <- xts(data_ob[,c("power","fit","lwr","upr")],as.POSIXct(strptime(data_ob$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01"))
    test_data_orig <- data_xts$power
    neu_result  <- subset(data_xts,select = -power)
    #anomaly_status <- find_anomalous_status(test_data=test_data_orig,result=neu_result,anomaly_window = 1,anomalythreshold_len = 4)
    anomaly_status <- find_anomalous_status_neg_anomalies(test_data=test_data_orig,result=neu_result,anomaly_window = 1,anomalythreshold_len = 4)
    anom_readings_online <- anomaly_status[anomaly_status == TRUE]
    write.csv(x = fortify(anom_readings_online),file = paste0(pathy,"online_",file1),row.names = FALSE)
    }
}

dataport_anomaly_localization <- function() {
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  # this will need three inputs
  # 1. ANomaly detection results file [folder dataport_AD]
  # 2. Prediction file [folder dataport dataport prediction]
  # 3 appliance specifiction/ratings file
  AD_detection_results <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  fls <- mixedsort(list.files(AD_detection_results,pattern="online_*"))
  predict_data <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_prediction/"
  write_path <-  "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AL/"
  appliance_feature_file <- fread("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_applianceRatings_corrected.csv")
  
  for (i in 1:length(fls)) {
    file1 <- fls[i]
    dfile <- strsplit(file1,'[_]')[[1]][2]
    df <- fread(paste0(predict_data,dfile))
    df_xts <- xts(subset(df,select=-timestamp),as.POSIXct(strptime(df$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    
    df_AD <- read.csv(paste0(AD_detection_results,file1))
    df_AD_xts <- xts(df_AD[,2],as.POSIXct(strptime(df_AD[,1],format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    

    appliance_features <- appliance_feature_file[V1==dfile][,2:dim(appliance_feature_file)[2]]
    
    #test_data, result, anom_status, appliance_features, window_minutes
    anom_location <- find_anomaly_appliance_with_stored_Results(pred_results = df_xts, anom_status = df_AD_xts,appliance_features,window_minutes = 5)
    write.csv(x = fortify(anom_location),file = paste0(write_path,dfile),row.names = FALSE)
    
  }
  
}

dataport_anomaly_verification_with_visualization <- function() {
 # this function plots detailed energy of anomalous days
   AD_detection_results <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  data_directory  <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  
  dfile <- "114.csv"
  res_file <- paste0("online_",dfile)
  df <- fread(paste0(data_directory,dfile))
  df_xts <- xts(subset(df,select=-localminute),as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01"))   
  
  df_AD <- read.csv(paste0(AD_detection_results,res_file))
  df_AD_xts <- xts(df_AD[,2],as.POSIXct(strptime(df_AD[,1],format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  
  daywise_anomaly_inspection(df_xts,df_AD_xts)
  
  
  
}



################# AMPDS FORECASTING PERFORMANCE########
ampds <- function(){
  # all files are generated with this script for ampds
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  file1 <- "ten_minutes_data.csv"
  df <- fread(paste0(path1,file1),header=TRUE, sep=",") # both power and weather data
  df_sub <- xts(data.frame(df$WHE,df$temperature,df$humidity),as.POSIXct(strptime(df$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  colnames(df_sub) <- c("power","temperature","humidity")
  
  #appliance_features <- get_appliance_features(path1, file1)
  print(paste0("Data ","start: ",index(first(df_sub))," end: ",index(last(df_sub))))
  # Data available from April 2012 and 31 March 2014
  train_data <- df_sub['2014-01-01/2014-01-15']
  test_data <- df_sub['2014-01-16/2014-03-30']
  #regression_days <- sampled_ob['2014-06-26/2014-07-30']
  reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
  neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
  
  ## STORE NEURAL RESULT IN A FILE
  comb_df <- cbind(test_data$power,neural_result)
  comb_temp <- data.frame(timestamp = index(comb_df),round(coredata(comb_df),2))
  #write.csv(comb_temp,paste0("ampds/","neural_prediction.csv"),row.names=FALSE)
  
  # NEXT LINES is USED TO STORE THE PREDICTION RESULT OF REGRESSION AND NERUAL NETWORKS IN A file
  #l <- as.data.frame(plot_table(test_data,reg_result$fit,neural_result$fit,intent = "return"))
  
  # Anomaly detection step using online detection approach
  test_data_orig <- comb_df$power
  neu_result  <- subset(comb_df,select = -power)
  anomaly_status <- find_anomalous_status(test_data = test_data_orig, result = neu_result, anomaly_window = 1,anomalythreshold_len = 4)
  anom_readings_online <- anomaly_status[anomaly_status == TRUE]
  write.csv(x = fortify(anom_readings_online),file = paste0(pathy,"online_ampds.csv",file1),row.names = FALSE)
  
  # Appliance localization
  
  
  # ##########
  #INCREMENTAL LOF APPROACH
  lof_ans <- incremental_lof_procedure(train_data,test_data,hourwindow = 1, daywindow = 10)
  pathy <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/ampds/"
  anom_readings <- lof_ans[lof_ans > 0.99]
  write.csv(x = fortify(anom_readings),file = paste0(pathy,"Lof_ampds.csv"),row.names = FALSE)
  #agg_result <- do.call(rbind,day_pred_result)
  #write.csv(agg_result,file="prediction_result.csv")
  #
  
  #AMPDS APPLAINCE RATINGS PROGRAM
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/"
  file1 <- "ten_minutes_data.csv"
  df <- fread(paste0(path1,file1))
  df_xts <- xts(df[,2:dim(df)[2]],as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  appliances <- subset(df_xts,select=-WHE)
  app_rating <- sort(apply(appliances,2,max),decreasing = TRUE)
  home_appliances_ratings <- t(as.data.frame(app_rating))
  #write.csv(home_appliances_ratings,file="ampds_applianceRatings.csv")
  # plot applaince rating with VI
  df_visualize <- df_xts["2014-01-01/2014-03-30"]
  colnames(df_visualize)
  plot(df_visualize$...) # for each column name
  
}
################# ECO_dataset FORECASTING PERFORMANCE########
ECO_prediction_without_occupancy <- function() {
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  fls <- list.files(path1,pattern = "*.csv")
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ZurichWeather/weather_Zurich_complete.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  day_pred_result <- list()
  
  for (i in 4:length(fls)) {
    file1 <- fls[1]
    data_ob <- create_weather_power_object_from_ECO_dataset(path1,file1,file2)
    #appliance_features <- get_appliance_features(path1, file1)
    print("DATA RANGES ARE:")
    print(paste0("Power data,","start: ",index(first(data_ob$power_data))," end: ",index(last(data_ob$power_data))))
    print(paste0("Weather data,","start: ",index(first(data_ob$weather_data))," end: ",index(last(data_ob$weather_data))))
    # USED DATA FROM 2012-08-01' to 2012-10-30
    merge_start_date <- as.POSIXct(strptime('2012-07-15',format = "%Y-%m-%d"))
    merge_end_date   <- as.POSIXct(strptime('2012-08-26',format = "%Y-%m-%d"))
    confirm_validity(data_ob, merge_start_date, merge_end_date)
    my_range <- paste0(merge_start_date,'/',merge_end_date)
    
    sampled_ob <- combine_energy_weather_ECOdata(data_ob,my_range)
    train_data <- sampled_ob['2012-07-15/2012-07-25']
    test_data <- sampled_ob['2012-07-26/2012-08-05']
    #regression_days <- sampled_ob['2014-06-26/2014-07-30']
   # reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
  #  print("Regression done")

    neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
    print("N-network done")
    print_single_prediction_method_table(test_data, method_result = neural_result$fit)
    plot_singlepredictor_graph(train_data,test_data,neural_result$fit)

    l <- as.data.frame(plot_table(test_data,reg_result$fit,neural_result$fit,intent = "return"))
    l$home <- strsplit(file1,'[.]')[[1]][1]
    day_pred_result[[i]] <- l
  }
  agg_result <- do.call(rbind,day_pred_result)
  # write.csv(agg_result,file="prediction_result_eco_dataset.csv")
}

create_appliance_rating_file_ECO <- function(){
  # this stores appliance ratings of dataprot houses in a single file. Next step: Please plot each home separtely and edit this  file acc. to VI 
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  fls <- mixedsort(list.files(path1,pattern = ".csv"))
  #file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  
  home_appliances <- list()
  for (i in 1:length(fls)) {
    file1 <- fls[i]
    df <- fread(paste0(path1,file1))
    df_xts <- xts(df[,2:dim(df)[2]],as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    appliances <- subset(df_xts,select = -total)
    app_rating <- sort(apply(appliances,2,max,na.rm=TRUE),decreasing = TRUE)
    home_appliances[[file1]] <- t(as.data.frame(app_rating))
  }
  
  home_applinace_df <- do.call(gtools::smartbind,home_appliances)
  write.csv(home_applinace_df,file="ECO_applianceRatings.csv")
  
}

################# REFIT_dataset FORECASTING PERFORMANCE########
REFTI <- function() {
  library(gtools)
  
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/dataset_10mins/"
  fls <- mixedsort(list.files(path1,pattern = "*.csv"))
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/weather_Midlands_complete.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  day_pred_result <- list()
  
  for (i in 1:length(fls)) {
    # House 11 has data from 3 june
    file1 <- fls[i]
    data_ob <- create_weather_power_object_from_REFIT_dataset(path1,file1,file2)
  
    #appliance_features <- get_appliance_features(path1, file1)
    print("DATA RANGES ARE:")
    print(paste0("Power data,","start: ",index(first(data_ob$power_data))," end: ",index(last(data_ob$power_data))))
    print(paste0("Weather data,","start: ",index(first(data_ob$weather_data))," end: ",index(last(data_ob$weather_data))))
    # USED DATA FROM 2012-08-01' to 2012-10-30
    merge_start_date <- as.POSIXct(strptime('2014-05-25',format = "%Y-%m-%d"))
    merge_end_date   <- as.POSIXct(strptime('2014-08-30',format = "%Y-%m-%d"))
    confirm_validity(data_ob, merge_start_date, merge_end_date)
    my_range <- paste0(merge_start_date,'/',merge_end_date)
    
    sampled_ob <- combine_energy_weather_ECOdata(data_ob,my_range)
    train_data <- sampled_ob['2014-06-01/2014-06-20']
    test_data <- sampled_ob['2014-06-21/2014-08-29']
    # regression_days <- sampled_ob['2014-07-10/2014-08-29']
    #find_regrassivedays_with_BIC(regression_days) # USED TO FIND NO. OF BEST HISTORICAL DAYS FOR REGRESSION
    
    #regression_days <- sampled_ob['2014-06-26/2014-07-30']
    reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
    print("Regression done")
    
    neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
    print("N-network done")
    
    l <- as.data.frame(plot_table(test_data,reg_result$fit,neural_result$fit,intent = "return"))
    l$home <- strsplit(file1,'[.]')[[1]][1]
    day_pred_result[[i]] <- l
  }
  
  agg_result <- do.call(rbind,day_pred_result)
   write.csv(agg_result,file="prediction_result_REFIT_dataset.csv")
}

create_appliance_rating_file_REFIT <- function(){
  # this stores appliance ratings of dataprot houses in a single file. Next step: Please plot each home separtely and edit this  file acc. to VI 
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/dataset_10mins/"
  fls <- mixedsort(list.files(path1,pattern = ".csv"))
  #file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  
  home_appliances <- list()
  
  #for (i in 1:length(fls)) {
  for (i in 1:length(fls)) {
    file1 <- fls[i]
    #data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
    #appliance_features <- get_appliance_features(path1, file1)
    df <- fread(paste0(path1,file1))
    df_xts <- xts(df[,2:dim(df)[2]],as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    appliances <- subset(df_xts,select = -Aggregate)
    app_rating <- sort(apply(appliances,2,max),decreasing = TRUE)
    home_appliances[[file1]] <- t(as.data.frame(app_rating))
    
  }
  
  home_applinace_df <- do.call(gtools::smartbind,home_appliances)
  write.csv(home_applinace_df,file="REFIT_applianceRatings.csv")
  
}

