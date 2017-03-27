# in this file I used rnn library to forecast. While varying lots of parameters I found that this implementation is not going to work.
library(rnn)
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/use_column/"
path_hourly <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/hourly/"
file <- "101.csv"
df <- read.csv(paste0(path_hourly,file))
#colnames(df) <- c("timestamp","power","temperature","humidity")
df$timestamp <- fasttime::fastPOSIXct(df$timestamp)-19800
df_xts <- xts(df$power,df$timestamp)
trainX <- df_xts["2014-06-12 00:00:00/2014-06-30 23:59:00"]
trainY <-  df_xts["2014-06-25 00:00:00/2014-06-25 23:59:00"]
trainX <- coredata(create_day_matrix(trainX))
trainX <- normalize_data(trainX)
trainY <- coredata(trainY)
tx1 <-  array(trainX,dim=c(NROW(trainX),NCOL(trainX),1))
ty1 <-  array(trainY,dim=c(NROW(trainY),NCOL(trainY),1))

#lrate <- seq(0.1,0.9,by=0.01)
#for (i in 1:length(lrate)){
model <- trainr(X=round(tx1,3),Y=ty1,learningrate = 0.05, hidden_dim = 10,seq_to_seq_unsync = TRUE,network_type = "lstm") 
predictr(model, tx1)

testX <- df_xts["2014-06-06 00:00:00/2014-06-16 23:59:00"]
testY <-  df_xts["2014-06-17 00:00:00/2014-06-17 23:59:00"]
testX <- coredata(create_day_matrix(testX$power))
testX <- normalize_data(testX)
testY <- coredata(testY$power)
tx2 <-  array(testX,dim=c(NROW(testX),NCOL(testX),1))
#ty2 <-  array(testY,dim=c(NROW(testY),NCOL(testY),1))
predictr(model, tx2)



create_day_matrix <- function(tdata) {
  #browser()
  tdata <- split.xts(tdata,f="days",k=1)
  #sapply(daydata)
  matdata <- do.call(cbind,lapply(tdata,function(x) coredata(x)))
  mat_xts <- xts(matdata,index(tdata[[1]]))
  return(mat_xts)
}
normalize_data <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}