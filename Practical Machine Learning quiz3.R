library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)

head(segmentationOriginal)
partition <- createDataPartition(segmentationOriginal$Case, p = 0.6, list = FALSE)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]

set.seed(125)

fit <- train(Class ~ ., data= training, method = "rpart")

library(rattle)
fancyRpartPlot(fit$finalModel)


predict(fit, newdata = testing)


#####################################


library(pgmm)
data("olive")
olive = olive[, -1]

partition <- createDataPartition(olive$Area, p = 0.6, list = FALSE)

training <- olive[partition,]
testing <- olive[-partition,]

fit <- train(Area ~ ., data = training, method = "rpart")

prediction <- predict(fit, newdata = testing)

newdata <- as.data.frame(t(colMeans(olive)))
predict(fit, newdata = newdata)

fancyRpartPlot(fit$finalModel)



#####################################


library(ElemStatLearn)
data("SAheart")
set.seed(8484)
train <- sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2, replace = FALSE)
trainSA <- SAheart[train,]
testSA <- SAheart[-train,]

set.seed(13234)

fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,data = trainSA,
             method = "glm",  family = "binomial")
summary(fit)

prediction <- predict(fit, newdata = testing)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(fit, newdata = trainSA))
missClass(testSA$chd, predict(fit, newdata = testSA))
