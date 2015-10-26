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

library(randomForest)
train_forest_set <- train[2:19]
# ## Random forest using all variables (train_score: 0.7002703) (test_score: 0.697622585438)
# full_forest_mod <- randomForest(Happy ~ ., data = train_forest_set, ntree = 200, nodesize = 25)
# ## Random forest using Alcohol cons. and Relig. (train_score:0.7060153) (test_score: 0.701263001486)
# full_forest_mod <- randomForest(Happy ~ alcohol + Engagement_Religion, data = train_forest_set, ntree = 200, nodesize = 25)
# # ## Random forest using child variables and alcohol (train_score: 0.7034276)
# full_forest_mod <- randomForest(Happy ~ babies + preteen + teens + alcohol, data = train_forest_set, ntree = 200, nodesize = 25)
## Random forest using Life factors (train_score: 0.6938592)
# full_forest_mod <- randomForest(Happy ~ alcohol + Divorce + Widowed + Education + income + Gender + babies + preteen + teens, data = train_forest_set, ntree = 200, nodesize = 25)
# ## Random forest using Enjoyment factors (train_score: 0.7062277)
# full_forest_mod <- randomForest(Happy ~ TVhours + alcohol, data = train_forest_set, ntree = 200, nodesize = 25)

## Taking out var1 and var2
# train_forest_set_noanon <- train_forest_set
# train_forest_set_noanon$Var1 <- NULL
# train_forest_set_noanon$Var2 <- NULL

# ## Random forest (train_score: 0.7013131)
# full_forest_mod <- randomForest(Happy ~ alcohol + Engagement_Religion + babies + Var2, data = train_forest_set, ntree = 200, nodesize = 25)
# ## Random forest (train_score: 0.7051173)
# full_forest_mod <- randomForest(Happy ~ alcohol + Engagement_Religion + Var2, data = train_forest_set, ntree = 200, nodesize = 25)
# ## Random forest (train_score: 0.7013131)
# full_forest_mod <- randomForest(Happy ~ alcohol + Engagement_Religion + babies + Var2, data = train_forest_set, ntree = 400, nodesize = 10)
## Random forest (train_score:)
full_forest_mod <- randomForest(Happy ~ alcohol + TVhours + Engagement_Religion, data = train_forest_set, ntree = 200, nodesize = 25)

predict_train <- predict(full_forest_mod)
eval_metric(train$Happy, predict_train)

predict_test <- predict(full_forest_mod, newdata = test)

forest_simplemod_submit <- data.frame(ID = test$ID, Happy = predict_test)
write.csv(forest_simplemod_submit, "forest_simplemod_sub.csv")
