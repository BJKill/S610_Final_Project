#### Final Project

## First, let's set up a series of p predictor variables: k "real" predictors and p-k "fake" predictors.
## By that, I mean that only k of the predictor variables will be used to generate the response variable.  We want to see if
## either our forward selection or backwards elimination algorithm can settle on the model containing only the k "real" predictors.
## We will have n observations, no intercept, and the linear model will take the form Y ~ 1*X1 + 2*X2 + 3*X3 + ...

library(readr)
library(plyr)
library(dplyr)
library(stringr)

make_model_matrix <- function(n,p) {
        X <- matrix(nrow = n, ncol = p)
        #X[ ,1] <- rep(1,n)
        for (i in 1:p) {
                X[ ,i] <- rnorm(n)
        }
        return(X)
}


make_response_vector <- function(X, k) {
        Y <- vector(length = nrow(X))
        for (i in 1:nrow(X)) {
                Y[i] <- rnorm(1)
                for (j in 1:k) {
                        Y[i] <- Y[i] + X[i,j]*j
                }
        }
        return(Y)
}


make_data_frame <- function(n,p,k) {
      X <- make_model_matrix(n,p)
      Y <- make_response_vector(X,k)
      df <- data.frame(cbind(X,Y))
      return(df)
}


run_BE <- function(n,p,k,alpha) {
        df <- make_data_frame(n,p,k)
        while(summary(lm(Y~.,df))$coefficients[1+which.max(summary(lm(Y~., df))$coefficients[-1,4]),4] > alpha) {
                rem_inx <- which.max(summary(lm(Y~., df))$coefficients[-1,4])
                df <- df[,-rem_inx]
        }
        display <- cbind(summary(lm(Y~.,df))$coefficients,confint(lm(Y~.,df)))
        display <- cbind(display,vector(length = nrow(display)))
        colnames(display)[7] <- "Known Param in CI?"
        display[1,7] <- (0 >= display[1,5]) & (0 <= display[1,6])
        for (i in 2:nrow(display)) {
                        display[i,7] <- (as.numeric(str_sub(rownames(display)[i], 2,-1)) >= display[i,5]) & 
                                        (as.numeric(str_sub(rownames(display)[i], 2,-1)) <= display[i,6])
                                      
                }
        return(display)
}

final <- run_BE(100,50,10,0.05)
final

#debug(run_BE)





