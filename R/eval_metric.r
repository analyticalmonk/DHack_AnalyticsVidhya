eval_metric <- function(actual, predicted)
{
  if (all(actual %in% c("Very Happy", "Pretty Happy", "Not Happy")) & all(predicted %in% c("Very Happy", "Pretty Happy", "Not Happy")))
  {
    actual <- ifelse(actual == "Very Happy", 15, ifelse(actual == "Pretty Happy", 10, 5))
    predicted <- ifelse(predicted == "Very Happy", 15, ifelse(predicted == "Pretty Happy", 10, 5))
    
    diff <- actual - predicted
    score <- (length(diff[diff == 0])*50 + length(diff[diff == 5])*10 + length(diff[diff == 10])*5 + length(diff[diff == -5])*-5 + length(diff[diff == -10])*-10)/(50*length(actual))
    
    cat("Score is", score, "\n")
  }else
  {
    cat("Input values are not as expected\n")
  }
}

