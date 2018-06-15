library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

fit <- train(y ~  ., data = vowel.train, method = "rf")
fit2 <- train(y ~ ., data = vowel.train, method = "gbm", verbose = F)

pred1 <- predict(fit, newdata = vowel.test)
pred2 <- predict(fit2, newdata = vowel.test)

confusionMatrix(pred1, vowel.test$y)$overall[1]
confusionMatrix(pred2, vowel.test$y)$overall[1]
confusionMatrix(pred1, pred2)$overall[1]


#########################################

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

fit <- train(diagnosis ~ ., data = training,  method = "rf")
fit2 <- train(diagnosis ~ ., data = training,  method = "gbm", verbose = FALSE)
fit3 <- train(diagnosis ~ ., data = training,  method = "lda")

test1 <- predict(fit, testing)
test2 <- predict(fit2, testing)
test3 <- predict(fit3, testing)


predDF <- data.frame(fit1 = test1, fit2 = test2, fit3 = test3, diagnosis = testing$diagnosis)

combFit <- train(diagnosis ~ ., data = predDF, method = "rf")

combTest <- predict(combFit, predDF)

confusionMatrix(combTest, predDF$diagnosis)$overall[1]









############################################

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

fit <- train(CompressiveStrength ~ ., data = training, method="lasso")
library(elasticnet)
plot.enet(fit$finalModel, xvar = "penalty", use.color = TRUE)




###############################################


library(lubridate) # For year() function below

dat = read.csv("~/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)
fit <- bats(tstrain, use.parallel = TRUE, num.cores = 4)
fcast <- forecast(fit, level = 95, h = dim(testing)[1])
plot(fcast); lines(testing, col="red")

sum(fcast$lower < testing$visitsTumblr &
          testing$visitsTumblr < fcast$upper)/dim(testing)[1]


#############################################

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
svmModel <- svm(CompressiveStrength ~ ., data = training)
svmPred <- predict(svmModel, testing)
err <- testing$CompressiveStrength - svmPred
RMSE(svmPred, testing$CompressiveStrength)

plot(svmPred, testing$CompressiveStrength, 
     pch=20, cex=1, 
     col=testing$Age, 
     main= "Relationship between the svm forecast and actual values")
