
normalize_data <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}

#read test and train data
traindat <- read.csv(file = "train.csv")
testdat <- read.csv(file = "test.csv")
# column "power" is response variable and remaining are predictors
# predictors in  traindata
trainX <- traindat[,1:dim(traindat)[2]-1]
# response of train data
trainY <- traindat$power
# arrange data acc. to RNN as [samples,time steps, features]
tx <- array(as.matrix(trainX), dim=c(NROW(trainX), 1, NCOL(trainX)))
tx <- normalize_data(tx) # normalize data in range of [0,1]
ty <- array(trainY, dim=c(NROW(trainY), 1, NCOL(trainY))) # arrange response acc. to predictors
# train model
model <- trainr(X = tx, Y = ty, learningrate = 0.08, hidden_dim = 6, numepochs = 400)

# predictors in test data
testX <- testdat[,1:dim(testdat)[2]-1]
testX <- normalize_data(testX) # normalize data in range of [0,1]
#testY <- testdat$power
# arrange data acc. to RNN as [samples,time steps, features]
tx2 <- array(as.matrix(testX), dim=c(NROW(testX), 1, NCOL(testX))) # predict
pred <- predictr(model,tx2)
pred

