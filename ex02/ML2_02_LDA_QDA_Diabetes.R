#==============================================================================#
# R code template Classification exercise: Diabetes                            #
#==============================================================================#

# 00: packages -----------------------------------------------------------------
library(pROC)  # install package if necessary
library(glmnet)

# 01: load data ----------------------------------------------------------------
load(Diabetes.Rda)  # ??? stands for "path/Diabetes.Rda"

# 02: Train/Test split ---------------------------------------------------------

# Split the data into a train/test with 2000 observations in the test data set
set.seed(50)
n <- dim(Diabetes)[1]
testidx <- sample(n, 2000)
test <- Diabetes[testidx, ]
train <- Diabetes[-testidx, ]

table(train$YN)