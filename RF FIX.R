mydata1 <-read.csv("E:/Proyek Akhir Data Mining Baru/data_untuk_olah.csv") 
mydata<-subset(mydata1, select=-c(KABUPATEN..KOTA, TAHUN))

#normalisasi data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
nn_norm <- as.data.frame(lapply(mydata, normalize))

library(caTools)
set.seed(123)
sample=sample.split(nn_norm,SplitRatio = 0.8)
trainset=subset(nn_norm, sample==TRUE)
testset=subset(nn_norm, sample==FALSE)

# for reproduciblity
set.seed(123)

# default RF model
library(randomForest)
m1 <- randomForest(y1~.,data=trainset, ntree=500)
plot(m1)
print(m1)

# number of trees with lowest MSE
which.min(m1$mse)
# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])


# Make prediction
predictions1 <- predict(m1, testset[,-17])
result1<-testset[,-17]
result1['y1']<-testset[,17]
result1['prediction']<-predictions1
print(result1)


#select mytry value with minimum OOB error
mtry <- tuneRF(trainset[-1],trainset$y1, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(71)
rf <-randomForest(y1~.,data=trainset, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

# Make prediction
predictions2 <- predict(rf, testset[,-17])
result2<-testset[,-17]
result2['y1']<-testset[,17]
result2['prediction']<-predictions2
print(result2)

# number of trees with lowest MSE
which.min(rf$mse)

# RMSE of this optimal random forest
sqrt(rf$mse[which.min(rf$mse)])
plot(rf)

actual<-result2$y1
predicted<-result2$prediction



