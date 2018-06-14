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


predDF <- data.frame(fit, fit2, y = vowel.test$y)
