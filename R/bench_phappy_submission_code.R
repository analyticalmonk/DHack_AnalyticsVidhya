test <- read.csv("Test_L4P23N3.csv")
# head(train)
# str(train)

# score_fun <- function(x) {
#     if (x == "Very Happy")
#         return(15)
#     else if (x == "Pretty Happy") 
#         return(10)
#     else if (x == "Not Happy") 
#         return(5)
# }

test_bench_result <- rep(x = levels(train$Happy)[2], times = nrow(test))
# test_bench_score <- sapply(test_bench_result, FUN = score_fun)

bench_phappy_submit <- data.frame(ID = test$ID, Happy = test_bench_result)
write.csv(bench_phappy_submit, "bench_phappy_submission.csv")