library(ISLR2)
library(Hmisc)
library(MASS)
library(naivebayes)
library(e1071)
library(pROC)
library(modeldata)
library(rsample)

data("Titanic", package = "datasets")
?Titanic

# Convert data to a data frame and inspect it
titanic_df <- as.data.frame(Titanic) # uses the method `as.data.frame.table`
# inspect the data:
# - titanic_df has one row for each combination of Class, Sex, Age and Survived,
# - along with the frequency for this combination
View(titanic_df)
# Expand the number of rows to correspond to the frequencies
# This repeats each combination equal to the frequency of each combination
repeating_sequence <- rep(1:nrow(titanic_df), times = titanic_df$Freq)
titanic_df <- titanic_df[repeating_sequence, names(titanic_df) != "Freq"]
# Check if rows in 'long' data fits to observations in original data
stopifnot(sum(Titanic) == nrow(titanic_df)) # throws an error if not TRUE




data("attrition", package = "modeldata")
?attrition
