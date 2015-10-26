train <- read.csv("train_FBFog7d.csv")
test <- read.csv("Test_L4P23N3.csv")
# head(train)
# str(train)

nrow_train <- nrow(train)

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

## Imputation on training set for building linear models
library(mice)
set.seed(21)
col_class_train <- sapply(train, class)
col_class_train <- as.vector(col_class_train)

#Using imputation only for integer variables
cnames_train <- colnames(train)[col_class_train == "integer"]
train_for_impute <- train[cnames_train]
train_for_impute$ID <- NULL

### Remember to fucking remove ID!!

imputed_train <- complete(mice(train_for_impute))
cnames_imputed <- colnames(imputed_train)

imputed_train$ID <- train$ID
train[cnames_imputed] <- imputed_train

train$happy_response <- sapply(train$Happy, response_class_fun)

# logit_imputed_int <- glm(happy_response ~ Score + babies + Education + Unemployed10 + TVhours + Var2, data = train)

library(nnet)

predictMNL <- function(model, newdata) {
    
    # Only works for neural network models
    if (is.element("nnet",class(model))) {
        # Calculate the individual and cumulative probabilities
        probs <- predict(model,newdata,"probs")
        cum.probs <- t(apply(probs,1,cumsum))
        
        # Draw random values
        vals <- runif(nrow(newdata))
        
        # Join cumulative probabilities and random draws
        tmp <- cbind(cum.probs,vals)
        
        # For each row, get choice index.
        k <- ncol(probs)
        ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
        
        # Return the values
        return(ids)
    }
}

# nn_imputed_int <- multinom(happy_response ~ Score + babies + Education + Unemployed10 + TVhours + Var2, train)
# a <- predictMNL(nn_imputed_int, train)