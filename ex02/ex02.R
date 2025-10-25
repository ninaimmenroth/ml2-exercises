library(ggplot2) # for visualisation
library(MASS) # for LDA nd QDA
library(pROC)
library(ggplot2)

posterior <- function(x){
  py1 <- dnorm(x, mean = 5, sd = 1) / (dnorm(x, mean = 5, sd = 1) + dnorm(x, mean = 4, sd = 1))
  return(py1)
}

curve(posterior, from = 0, to = 8)

posterior <- function(x, mu0 = 4, mu1 = 5, sigma = 1, pi0 = 0.5, pi1 = 1 - p0){
  py1 <- dnorm(x, mean = mu1, sd = sigma) * pi1/ (dnorm(x, mean = mu1, sd = sigma) * pi1 + 
                                               dnorm(x, mean = mu0, sd = sigma) * pi0)
  return(py1)
}
  


ggplot() +
  geom_function(fun = posterior, 
                args = list(mu0 = 4, 
                            mu1 = 5, 
                            sigma = 1)) + 
scale_x_continuous(limits = c(0, 8))

# with base R graphics this can be achieved via
curve(posterior(x, mu0 = 4, mu1 = 5, sigma = 1),
      from = 0, to = 8)







