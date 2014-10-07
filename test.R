library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


x=training[c(1,58:69)]
preProc=preProcess(x[2:13],method='pca',thresh = 0.8)
trainPC <- predict(preProc,x[2:13])
modelFit <- train(x$diagnosis ~.,method="glm",data=trainPC)
testPC <- predict(preProc,testing[58:69])
confusionMatrix(testing$diagnosis,predict(modelFit,testPC))


#Non-PCA Accuracy: 0.65 
#PCA Accuracy: 0.72
x=training[c(1,58:69)]
modelFit <- train(x$diagnosis ~.,method="glm",data=x)
confusionMatrix(testing$diagnosis,predict(modelFit,testing[58:69]))