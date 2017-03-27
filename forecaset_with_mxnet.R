# in this file I used  mxnet library to forecast. All things are set but the library results in some unknown errors 
library(xts)
library(data.table)
library(mxnet)
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/use_column/"
path_hourly <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/hourly/"
file <- "101.csv"
df <- read.csv(paste0(path1,file))
#colnames(df) <- c("timestamp","power","temperature","humidity")
df$timestamp <- fasttime::fastPOSIXct(df$timestamp)-19800
df_xts <- xts(df$power,df$timestamp)
trainX <- df_xts["2014-06-12 00:00:00/2014-06-30 23:59:00"]
trainY <-  df_xts["2014-06-25 00:00:00/2014-06-25 23:59:00"]
trainX <- coredata(create_day_matrix(trainX))

batch.size = 32
seq.len = 32
num.hidden = 16
num.embed = 16
num.lstm.layer = 1
num.round = 1
learning.rate= 0.1
wd=0.00001
clip_gradient=1
update.period = 1

colnames(trainX) <- paste0("D",1:dim(trainX)[2])
row.names(trainX) <- paste0("R",1:dim(trainX)[1])
train.list <-  list(data = array(trainX,dim=c(1,19,144)), label = array(coredata(trainY), dim=c(1,1,144)) )

model <- mx.lstm(train.list,
                 ctx=mx.cpu(),
                 num.round=num.round,
                 update.period=update.period,
                 num.lstm.layer=num.lstm.layer,
                 seq.len=seq.len,
                 num.hidden=num.hidden,
                 num.embed=num.embed,
                 num.label=20,
                 batch.size=batch.size,
                 input.size=20,
                 initializer=mx.init.uniform(0.1),
                 learning.rate=learning.rate,
                 wd=wd,
                 clip_gradient=clip_gradient)




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