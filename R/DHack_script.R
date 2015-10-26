train <- read.csv("train_FBFog7d.csv")
test <- read.csv("Test_L4P23N3.csv")
# head(train)
# str(train)

nrow_train <- nrow(train)
train_bench_result <- rep(x = levels(train$Happy)[2], times = nrow_train)

score_fun <- function(x) {
    if (x == "Very Happy")
        return(15)
    else if (x == "Pretty Happy") 
        return(10)
    else if (x == "Not Happy") 
        return(5)
}

## Adding a response variable
response_class_fun <- function(x) {
    if (x == "Very Happy")
        return(2)
    else if (x == "Pretty Happy") 
        return(1)
    else if (x == "Not Happy") 
        return(0)
}

## Converting from response variable
inverse_response_fun <- function(x) {
    if (x == 1)
        return("Very Happy")
    else if (x == 2) 
        return("Pretty Happy")
    else if (x == 3) 
        return("Not Happy")
}

train_score <- sapply(train$Happy, FUN = score_fun)
# table(train$Happy, train_score)
train_bench_score <- sapply(bench_result, FUN = score_fun)
# table(bench_result, bench_score)

test_bench_result <- rep(x = levels(train$Happy)[2], times = nrow(test))
test_bench_score <- sapply(test_bench_result, FUN = score_fun)

## Imputation on training set for building linear models
library(mice)
col_class_train <- sapply(train, class)
col_class_train <- as.vector(col_class_train)

#Using imputation for all variables
train_for_impute <- train[2:17]
train_for_impute$ID <- NULL

#Using imputation only for integer variables
cnames_train <- colnames(train)[col_class_train == "integer"]
train_for_impute <- train[cnames_train]
train_for_impute$ID <- NULL

#Using imputation only for factor variables
cnames_train <- colnames(train)[col_class_train == "factor"]
train_for_impute <- train[cnames_train]

### Remember to fucking remove ID!!

imputed_train <- complete(mice(train_for_impute))
cnames_imputed <- colnames(imputed_train)

# imputed_train$ID <- train$ID
train[cnames_imputed] <- imputed_train

set.seed(21)
train_forest_set <- train[2:18]
full_forest_mod <- randomForest(Happy ~ ., data = train_forest_set, ntree = 1000, nodesize = 25)

predict_train <- predict(full_forest_mod)
eval_metric(train$Happy, predict_train)

# > eval_metric(train$Happy, predict_train)
# Score is 0.6022593 
# full_forest_mod <- randomForest(Happy ~ ., data = train_forest_set, ntree = 200, nodesize = 25)
# predict_train <- predict(full_forest_mod)
# > eval_metric(train$Happy, predict_train)
# Score is 0.6147437 
# > full_forest_mod <- randomForest(Happy ~ ., data = train_forest_set, ntree = 400, nodesize = 25)
# > predict_train <- predict(full_forest_mod)
# > eval_metric(train$Happy, predict_train)
# Score is 0.6125036 
# > a <- read.csv(file = "bench_phappy_submission.csv")
# > eval_metric(train$Happy, a$Happy)
# Score is 0.6194554 

predict_test <- predict(full_forest_mod, newdata = test)
table(predict_test)

# train$happy_response <- sapply(train$Happy, response_class_fun)