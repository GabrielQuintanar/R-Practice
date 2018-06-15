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
