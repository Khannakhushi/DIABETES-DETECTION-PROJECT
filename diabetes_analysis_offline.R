diabetes <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")
head(diabetes)
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
diabetes$HighBP <- as.factor(diabetes$HighBP)
diabetes$HighChol <- as.factor(diabetes$HighChol)
diabetes$CholCheck <- as.factor(diabetes$CholCheck)
diabetes$Smoker <- as.factor(diabetes$Smoker)
diabetes$Stroke <- as.factor(diabetes$Stroke)
diabetes$HeartDisease <- as.factor(diabetes$HeartDisease)
diabetes$PhysActivity <- as.factor(diabetes$PhysActivity)
diabetes$Fruits <- as.factor(diabetes$Fruits)
diabetes$Veggies <- as.factor(diabetes$Veggies)
diabetes$HvyAlcoholConsump <- as.factor(diabetes$HvyAlcoholConsump)
diabetes$AnyHealthcare <- as.factor(diabetes$AnyHealthcare)
diabetes$NoDocbcCost <- as.factor(diabetes$NoDocbcCost)
diabetes$DiffWalk <- as.factor(diabetes$DiffWalk)
diabetes$Sex <- as.factor(diabetes$Sex)

# Running 10 train-test-split
library("randomForest")
error_rates = list(10)
for(i in 1:10) {
  set.seed(i)
  train <- sample(1:nrow(diabetes), (nrow(diabetes) * .8) + .5)
  test <- diabetes[-train,]
  model = randomForest(Diabetes_binary ~ ., data = diabetes, subset = train, ntree = 100, mtry = sqrt(ncol(diabetes) - 1), importance = TRUE)
  dia.pred <- predict(model, newdata = test, type = "class")
  error_rates[i] <- mean(dia.pred != test$Diabetes_binary)
}
summary(model)
print(error_rates)
error_rates <- unlist(error_rates)
mean(error_rates)
varImpPlot(model, sort = TRUE, main = "Variable Importance - Diabetes")

# 10-repeated splits for the logistic regression that uses the same seeds as the random forest
lr_error_rates = list(10)
lr_mse = list(10)
for(i in 1:10) {
  set.seed(i)
  train <- sample(1:nrow(diabetes), (nrow(diabetes) * .8) + .5)
  test <- diabetes[-train,]
  model <- glm(Diabetes_binary~., family="binomial", data=diabetes, subset = train)
  pred <- predict(model, newdata = test, type = "response")
  yHat <- pred > 0.5
  yHat <- as.integer(as.logical(yHat))
  lr_error_rates[i] <- mean(yHat != test$Diabetes_binary)
  lr_mse[i] <- mean((yHat != test$Diabetes_binary))
}
lr_error_rates <- unlist(lr_error_rates)
print(lr_error_rates)
print(mean(lr_error_rates))
summary(model)

model2 <- glm(Diabetes_binary ~ . - Smoker - Fruits - AnyHealthcare - NoDocbcCost, family = "binomial", data = train)
summary(model2)

lr_error_rates_2 = list(10)
lr_mse_2 = list(10)
for(i in 1:10) {
  set.seed(i)
  train <- sample(1:nrow(diabetes), (nrow(diabetes) * .8) + .5)
  test <- diabetes[-train,]
  model2 <- glm(Diabetes_binary ~ . - Smoker - Fruits - AnyHealthcare - NoDocbcCost, family = "binomial", data = diabetes, subset = train)
  pred <- predict(model2, newdata = test, type = "response")
  yHat <- pred > 0.5
  yHat <- as.integer(as.logical(yHat))
  lr_error_rates_2[i] <- mean(yHat != test$Diabetes_binary)
  lr_mse_2[i] <- mean((yHat != test$Diabetes_binary))
}
lr_error_rates_2 <- unlist(lr_error_rates_2)
print(lr_error_rates_2)
print(mean(lr_error_rates_2))
