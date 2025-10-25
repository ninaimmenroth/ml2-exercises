#==============================================================================#
# Machine Learning II                                                          #
# Workshop 01: Imputing missing values                                         #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: Packages -----------------------------------------------------------------
library(mice)
library(VIM)

# If you get an error at this stage, you need to install the packages first via
# install.packages("mice")
# install.packages("VIM")


# 01: Exercise: tao data -------------------------------------------------------
# 01a: load data ---- 
data(tao)  # in VIM package
?tao
table(tao$Year)

# there are only two years and we do not need the numeric value of Year 
# in the regression model, so convert it to a factor
tao$Year <- as.factor(tao$Year) 

plot(tao$Air.Temp, tao$Humidity, col=tao$Year)

startMar <- par()$mar  # saves the initial values of the plot window margins,
# so that you can return to the default settings later.
par(mar = c(0, 0, 0, 0) + 0.1) # sets appropriate margins for `md.pattern()`
# and `aggr()`
# here come plot function working with specified margins from above
md.pattern(tao, rotate.names = TRUE) 

# when done with plot functions, reset the margins to the initial setting via
par(mar = startMar)

summary(tao)
aggr(tao)


# 01b: Questions regarding the output to the above commands. ----

# - How many observations have a missing value for *humidity*?
# - How many observations have more than one missing value?

# 01c: interpreting a margin plot ----

any.missing <- !complete.cases(tao)
marginplot(tao[, c("Air.Temp", "Humidity")])
marginplot(tao[, c("Air.Temp", "Sea.Surface.Temp")])

# Discussion: 
# - do the missing values appear to be random?
# - what is the problem with univariate imputation?

# 01d: linear model for imputation ----
# You will use the same longish model specification several times in Exercise 2, 
# so let's give the "formula" a short name.
tao.model <- formula(Sea.Surface.Temp ~ Year + Longitude + Latitude + Air.Temp + 
                       Humidity + UWind + VWind)
# fit a linear regression model to the "fully known" observations
lm.knowns <- lm(tao.model, data=tao)
summary(lm.knowns)

# answer the following questions:
# a) what happened to the data with the missing values during the model fit?
# b) How good is your model?


# 02: Exercise: univariate imputation ------------------------------------------

# 02a: Mean imputation ----
mean.replace <- function(x) {
  idx <- is.na(x)  # returns a logical indicator for missing values in x
  known.mean <- mean(x, na.rm = TRUE) 
  x[idx] <- known.mean
  
  return(x)
}   

# check using a test variable
tt <- c(1:10, NA, NA, 99)
tt
mean(tt, na.rm = TRUE)
mean.replace(tt)

hist(tao$Air.Temp)

# impute the Air.Temp using mean replacement
mrep.Air.Temp <- mean.replace(tao$Air.Temp)
hist(mrep.Air.Temp)

# Now it's your turn: do what you learnt above with all variables
# and as always: you have to replace the '???' part in the following expressions
tao.mrep <- tao
tao.mrep$Air.Temp <- mrep.Air.Temp
tao.mrep$Sea.Surface.Temp <- mean.replace(tao$Sea.Surface.Temp)
tao.mrep$Humidity <- mean.replace(tao$Humidity)

with(tao.mrep, plot(Air.Temp, Sea.Surface.Temp, col = Year))


# NB: with(data.frame,command(...)) runs command(...) knowing that variable 
# names are to be found in data.frame

any.missing <- !complete.cases(tao)
with(tao.mrep, plot(Air.Temp, Humidity, col = 1 + any.missing)) #colour the imputed vales red. 

# What can you learn from the obtained graph? Discuss it with your neighbour!


lm.mrep <- lm(tao.model, data = tao.mrep)
summary(lm.mrep)

# Compare the outcome to `summary(lm.knowns)`. What do you observe?

# 02b: mean/variance Simulation ----
mean.sd.replace <- function(x) {
  idx <- is.na(x)
  known.mean <- mean(x, na.rm = TRUE) 
  known.sd <- sd(x, na.rm = TRUE) 
  x[idx] <- rnorm(sum(idx), known.mean, known.sd)
  
  return(x)
}   

# Check your function:
tt <- c(1:10, NA, NA, 95)
tt
mean.sd.replace(tt)
round(mean.sd.replace(tt), 2)

hist(tao$Air.Temp)
# impute the Air.Temp using mean/variance simulation
msdrep.Air.Temp <- mean.sd.replace(tao$Air.Temp)
hist(msdrep.Air.Temp)

# Now work on the complete data set
tao.msdrep <- tao
tao.msdrep$Air.Temp <- msdrep.Air.Temp
tao.msdrep$Sea.Surface.Temp <- mean.sd.replace(tao$Sea.Surface.Temp)
tao.msdrep$Humidity <- mean.sd.replace(tao$Humidity)
with(tao.msdrep, plot(Air.Temp, Humidity, col = Year))
with(tao.msdrep, plot(Air.Temp, Humidity, col = 1 + any.missing))

lm.msdrep <- lm(tao.model, data = tao.msdrep)
summary(lm.msdrep)

# Compare the outcome to `summary(lm.knowns)` and `summary(lm.mrep)`. 
# What do you observe?


# 02c: Direct Random Sampling ----

# Apply what you learnt above to " Direct Random Sampling"  
dir.rand.samp <- function(x) {  
  idx <- is.na(x)
  x[idx] <- sample(x[!idx], size = sum(idx), replace = TRUE)
  
  return(x)
}   

# check your function
tt
dir.rand.samp(tt)
# and again
dir.rand.samp(tt)
# Why?!

tao.drs <- tao
tao.drs$Air.Temp <- dir.rand.samp(tao$Air.Temp)
tao.drs$Sea.Surface.Temp <- dir.rand.samp(tao$Sea.Surface.Temp)
tao.drs$Humidity <- dir.rand.samp(tao$Humidity)
hist(tao.drs$Air.Temp)

with(tao.drs, plot(Air.Temp, Humidity, col = Year))
with(tao.drs, plot(Air.Temp, Humidity, col = 1 + any.missing))

lm.drs <- lm(tao.model, data = tao.drs)
summary(lm.drs)

# Now compare the coefficients from all four univariate methods
cbind(lm.knowns$coefficients,
      lm.mrep$coefficients,
      lm.msdrep$coefficients,
      lm.drs$coefficients)


# 03: Exercise: Multivariate imputation using Gibbs sampling -------------------
# 03a: Gibbs sampling via `mice()` ----
GibbsData  <-  mice(tao, m = 5, maxit = 50, meth = 'pmm', seed = 600)

# 03b: completed data ----
Gibbsdata1 <- complete(GibbsData, 1)

# plot with missing values in red
with(Gibbsdata1, plot(Air.Temp, Sea.Surface.Temp, col = Year))
with(Gibbsdata1, plot(Air.Temp, Sea.Surface.Temp, col = 1 + any.missing))

# visual inspection!
# And? Are you impressed? If not think about what happened!

lm.Gibbs1 <- lm(tao.model, data=Gibbsdata1)
summary(lm.Gibbs1)
round(cbind(lm.knowns$coefficients,
            lm.mrep$coefficients,
            lm.msdrep$coefficients,
            lm.drs$coefficients,
            lm.Gibbs1$coefficients), 4)

# 03c: linear model with all imputed data ----
# run lm on all 5 complete data sets
lm.Gibbs.all <- with(GibbsData, 
                     lm(Sea.Surface.Temp ~ Year + Longitude + Latitude +
                          Air.Temp + Humidity + UWind + VWind))
# the result of each on 
lm.Gibbs.all$analyses 

# summary(lm.obj) for each of the 5 lms.
lapply(lm.Gibbs.all$analyses, summary)
round(sapply(lm.Gibbs.all$analyses, coefficients), 4)
 
# 03d: pooled results ----
summary(pool(lm.Gibbs.all))

# 03e: final model ----
lm.Gibbs.all.final <- with(GibbsData,
                           lm(Sea.Surface.Temp ~ Year + Longitude + Latitude + 
                                Air.Temp + Humidity + VWind))
summary(pool(lm.Gibbs.all.final))

# 04: Exercise: Imputing missing data for the Diabetes data set ----------------
# 04a: data handling ----
library(NHANES)
data("NHANES")
?NHANES
colsOfInterest <- c("Diabetes", "Gender", "Race1", "BMI", "Age", "Pulse", 
                    "BPSysAve", "BPDiaAve", "HealthGen", "DaysPhysHlthBad",
                    "DaysMentHlthBad", "LittleInterest", "Depressed")
Diabetes2 <- NHANES[ , colsOfInterest]
table(Diabetes2$Diabetes)

# convert Diabetes variable to be a binary outcome for logistic regression
Diabetes2$Diabetes <- as.numeric(Diabetes2$Diabetes) - 1

par(mar = c(0, 0, 0, 0) + 0.1)
md.pattern(Diabetes2, rotate.names = TRUE) 
par(mar=startMar)

summary(Diabetes2)
aggr(Diabetes2)

any.missing <- !complete.cases(Diabetes2)

# 04b: Multivariate imputation ----
# this will take a while!
GibbsDiabetes <- mice(???, m = 5, maxit = 10, meth = 'pmm', seed = 700)

# obtain the first complete data set
Gibbs1Diabetes <- ???

# MEOW!
md.pattern(Gibbs1Diabetes) 

# 04c: logistic regression ----
# recap: glm() with family = "binomial" specifies a logistic regression
# first the logistic regression on the original known data, ignoring missings
logreg.known <- glm(Diabetes ~ ., data = ???, family = "binomial")
summary(logreg.known)
logreg.Gibbs1 <- glm(Diabetes ~ ., ???)
summary(???)

logreg.full <- with(GibbsDiabetes,
                    glm(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + 
                          BPSysAve + BPDiaAve + HealthGen + DaysPhysHlthBad + 
                          DaysMentHlthBad + LittleInterest + Depressed,
                        family="binomial"))
summary(pool(???))

# 04d: model refinement ----
# remove the variable Depressed as it seems not to be significant at 5% level  
logreg.final <- with(GibbsDiabetes,
                     glm(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + 
                           BPSysAve + BPDiaAve + HealthGen + DaysPhysHlthBad + 
                           DaysMentHlthBad + LittleInterest,
                         family="binomial"))
summary(pool(???))


# predictions from the logistic regression model
pr.logreg <- predict(logreg.Gibbs1, type = "response") > 0.5
table(pr.logreg, Diabetes2$Diabetes)


# 04e: tree for imputed data (recap) ----
# you might have to install these packages which were used in ML1
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
tree.known <- rpart(Diabetes ~ ., data=Diabetes2)
rpart.plot(tree.known)

tree.Gibbs1 <- rpart(Diabetes ~ ., data=Gibbs1Diabetes)
rpart.plot(tree.Gibbs1)



Gibbs.full <- with(GibbsDiabetes,rpart(Diabetes~Gender+Race1+BMI+Age))

library(rpart)
library(rpart.plot)
tree.known <- rpart(Diabetes ~ ., data = Diabetes2)
rpart.plot(tree.known)

tree.Gibbs1 <- rpart(Diabetes ~ ., data = Gibbs1Diabetes)
rpart.plot(tree.Gibbs1)


Gibbs.full <- with(GibbsDiabetes, rpart(Diabetes ~ Gender + Race1 + BMI + Age))

pool(Gibbs.full) ###Damn!  (produces an error)

for(i in 1:5){
  tree.Gibbsfull <- rpart(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + 
                            BPSysAve + BPDiaAve + HealthGen + DaysPhysHlthBad + 
                            DaysMentHlthBad + LittleInterest + Depressed,
                          data = complete(GibbsDiabetes, i))
  rpart.plot(tree.Gibbsfull, main = paste0("Imputation: ", i), roundint = FALSE)
}

