#==============================================================================#
# Exercise sheet 04: Support Vector Classifiers                                #
# Code template for exercise 02                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
library(ISLR2)
library(e1071)
library(pROC)
library(naivebayes)

# 01: load data ----------------------------------------------------------------
data("College", package = "ISLR2")

# 02: descriptive stats and data preparation -----------------------------------
dim(College)
table(College$Private)
summary(College)

# From previous exercises we know that some variables were very skewed.
# E.g. Accept
hist(College$Accept)

# It is easier is we define new variables with the log data for four variables
# and append them to the data frame College before we create training and test 
# data sets
College$lAccept <- log(College$Accept)
College$lApps <- log(College$Apps)
College$lF.Undergrad <- log(College$F.Undergrad)
College$lExpend <- log(College$Expend)

# Create training and test data set with 80/20 split
set.seed(2)
train_share <- 0.8
train_idx <- sample(1:nrow(College), size = as.integer(nrow(College) * 0.8))
Colltrain <- College[train_idx, ]
Colltest <- College[-train_idx, ]

# 03: simple 2-variable model --------------------------------------------------
# first SVM model on two variables
svm_01 <- svm(Private ~ lAccept + PhD, data = Colltrain,  
               kernel = "linear", cost = 0.1, scale = FALSE)
# visualisation
plot(???, ???, lAccept ~ PhD)

# model summary
summary(???)

# obtain the confusion matrix (training data)
ypred <- predict(svm_01, newdata = Colltrain)
table(ypred, ???$Private)

# and the proportion of the correctly predicted values (accuracy)
sum(diag(prop.table(table(???, ???$???))))

# make sure you understand why this gives the desired result

# 04: 3-variable model ---------------------------------------------------------
# except an additional variable all other settings remain the same
svm_02 <- ???(Private ~ lAccept + PhD + lExpend, data = ???, ???)
plot(???, ???, lAccept ~ PhD)

# Read the notes in the worksheet about this diagram
mean(Colltrain$lExpend)

# Specify that the underlying value for lExpend should be the approx mean value 
plot(???, ???, lAccept ~ PhD, slice = list(lExpend = 9))

# Recap: Why do we use the value `lExpend = 9`?

# Try out the above command with different "slice values"

# Further model diagnostics
summary(???)
ypred <- ???
table(???)
sum(???)

# better than before?

# 05: full model ---------------------------------------------------------------
# Now try all the variables available used in the GAM exercise
mod_full <- Private ~ lAccept + lApps + lF.Undergrad + Room.Board + lExpend + 
  S.F.Ratio + PhD

svm_03 <- ???(mod_full, ???)

# That warning is not good

# We'll try scaling the data before running (which is the default setting)
svm_04 <- svm(???, scale = TRUE)

# model diagnostics
summary(???)
ypred <- ???
table(???)
sum(???)

# Actually the predictions are very similar whether scaling is used or not, 
# but we should always be wary of a WARNING: reaching max number of iterations

# 05: CV for optimal costs -----------------------------------------------------
# now investigate the best cost value using cross validation
tune_01 <- tune(svm, mod_full, data = Colltrain, kernel = "linear", 
                scale = TRUE,
                ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 50)))
# Further model diagnostics
summary(???)
ypred <- ???
table(???)
sum(???))

best_svm <- tune_01$best.model
summary(best_svm)
ypred <- predict(???, Colltrain)
table(???)
sum(???)

# the accuracy on the training data best model is slightly better


# 06: ROC analysis -------------------------------------------------------------
# We need to re-run the SVM so that the algorithm returns probabilities for how 
# likely a college private is using the 'prob = TRUE' argument
svm_05 <- svm(???, data = Colltrain, prob = TRUE, ???)

# two steps required to extract the probabilities 
ypred <- predict(svm_05, Colltrain, prob = TRUE)  
# since the probs are stored as án attribute, we need the next lineof code
ypredp <- attr(ypred, "probabilities")[, "Yes"]

# create a ROC object and plot it
roc_obj_train  <-  roc(Colltrain$Private, ypredp)
ggroc(roc_obj_train)
auc(roc_obj_train)

# 07: model evaluation on test data --------------------------------------------
ypred <- predict(svm_05, ???)
table(ypred, ???)
sum(diag(prop.table(table(ypred, ???))))

ypred_test <- predict(svm_05, ???, prob = TRUE)
ypred_testp <- attr(ypred_test, "probabilities")[,1]
roc_obj_test  <-  roc(???, ypred_testp)
ggroc(list(train = roc_obj_train, test = roc_obj_test))
auc(roc_obj_train)
auc(roc_obj_test)

# 08: Naive Bayes classification -----------------------------------------------
nb_01 <- naive_bayes(mod_full, data = ???, usekernel = TRUE)

summary(???)
plot(???)

# predictions on test data
ypred_nb_test_class <- predict(nb_01, newdata = ???)

# for ROC curves we need probabilities instead of classes
ypred_nb_test <- predict(nb_01, newdata = ???, type = ???)[, "Yes"]
roc_obj_nb_test  <-  roc(???$Private, ypred_nb_test)
ggroc(list(train = roc_obj_train, 
           test = roc_obj_test,
           test_nb = roc_obj_nb_test))
auc(roc_obj_test)
auc(roc_obj_nb_test)

# accuracy NB:
sum(???(???(table(???, ???$Private))))

# 09: conclusion ---------------------------------------------------------------

# what was it all about?
