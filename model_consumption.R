setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Localize/")




#appliance 1
max = 1000
day = matrix(NA,nrow = 10,ncol = 1140)
for(i in 1:10)
day[i,] = c(rnorm(60*9,10,1),rnorm(60,1000,1),rnorm(60*9,10,1))



library(TSclust) # for diss
library(xts)
library(Rlof) # FOR LOF
library(HighDimOut) # to normalize output scores
outlierfactor_improved(day)


day2 = c(rnorm(60*9,10,1),rnorm(60,1000,1),rnorm(60*9,10,1))
plot(day1,t="l")