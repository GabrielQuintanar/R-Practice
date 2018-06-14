library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
library(Hmisc)
library(ggplot2)
cutCS <- cut2(training$CompressiveStrength, g=4)

c1 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = Cement)) +
      geom_jitter() + labs(title = "Cement") + guides(color = "none")
c2 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = BlastFurnaceSlag))+
      geom_jitter() + labs(title = "BlastFurnaceSlag") + guides(color = "none")
c3 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = FlyAsh))+
      geom_jitter()  +labs(title = "FlyAsh") + guides(color = "none")
c4 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = Water))+
      geom_jitter() + labs(title = "Water") + guides(color = "none")
c5 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = Superplasticizer))+
      geom_jitter() + labs(title = "Superplasticizer") + guides(color = "none")
c6 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = CoarseAggregate))+
      geom_jitter() + labs(title = "CoaerseAggregate") + guides(color = "none")
c7 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = FineAggregate))+
      geom_jitter() + labs(title = "FineAggregate") + guides(color = "none")
c8 <- ggplot(data = training,aes(y = training$CompressiveStrength, x = 1:length(training$CompressiveStrength), color = Age))+
      geom_jitter() + labs(title = "Age") + guides(color = "none")
grid.arrange(c1, c2, c3, c4, c5, c6, c7, c8, ncol = 8)


g2 <- ggplot(data = training,aes(x = cutCS, y = CompressiveStrength, fill = cutCS)) + geom_boxplot()

g3 <- ggplot(data = training, aes(x = cutCS, y = CompressiveStrength, color=cutCS)) + geom_jitter()


grid.arrange(g2, g3, ncol = 2)



h1 <- ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram()
h1

#Part 4


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Find all the variables that begin with IL
trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL <- testing[, grep("^IL|diagnosis", names(testing))]

#Perform PCA with a treshold of 90%
procTrain <- preProcess(trainingIL, method = "pca", thresh = 0.9 )
procTrain



model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
predictModel <- predict(model, newdata=testingIL)

matrixModel <- confusionMatrix(predictModel, testingIL$diagnosis)
matrixModel$overall[1]

#Standarized
standarizedTraining <- preProcess(trainingIL, method = c('center', 'scale', 'pca')
                                  , thresh = 0.8)

modelPCA <- train(diagnosis ~ ., data = trainingIL, method = "glm",
                  preProcess = "pca", trControl = trainControl(preProcOptions = 
                                                                     list(thresh = 0.8)))
matrixModelPCA <- confusionMatrix(testingIL$diagnosis, predict(modelPCA, testingIL))
matrixModelPCA
