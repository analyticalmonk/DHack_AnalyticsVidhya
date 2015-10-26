train <- read.csv("train_FBFog7d.csv")
test <- read.csv("Test_L4P23N3.csv")
# head(train)
# str(train)

nrow_train <- nrow(train)

set.seed(21)
library(mice)
col_class_train <- sapply(train, class)
col_class_train <- as.vector(col_class_train)

#Using imputation for all variables
train_for_impute <- train[2:17]

imputed_train <- complete(mice(train_for_impute))
cnames_imputed <- colnames(imputed_train)
train[cnames_imputed] <- imputed_train

test_for_impute <- test[2:17]

imputed_test <- complete(mice(test_for_impute))
test[cnames_imputed] <- imputed_test

## Adding alcohol data
alcohol_data <- read.csv("NewVariable_Alcohol.csv")
test_alcohol <- alcohol_data[1:3387,]
train_alcohol <- alcohol_data[3388:13744,]
train$alcohol <- train_alcohol$Alcohol_Consumption
test$alcohol <- test_alcohol$Alcohol_Consumption
rm(alcohol_data)

library(caret)

train_forest_set <- train[2:19]
nnet.model <- train(Happy ~ alcohol + Engagement_Religion, data = train_forest_set, method = "nnet", tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1)))
predict_train <- predict(nnet.model, newdata = test)

nnet_submit <- data.frame(ID = test$ID, Happy = predict_test)
write.csv(forest_simplemod_submit, "nnet_sub.csv")
