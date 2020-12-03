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
        if (n <= 0 | p <= 0 | is.integer(n) = F | is.integer(p) = F) {return("n and p must be positive integers")}
        X <- matrix(nrow = n, ncol = p)
        for (i in 1:p) {
                X[ ,i] <- rnorm(n)
        }
        return(X)
}


make_response_vector <- function(X, k) {
        if (k > ncol(X) | is.integer(k) = F) {return("k must be a positive integer not exceeding p")}
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
        if (alpha < 0 | alpha > 1) {return("alpha must be in the interval (0,1)")}
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

run_simulation <- function(n,p,k,alpha,m) {
        if (m <= 0 | is.integer(m) = F) {return("m must be a positive integer")}
        CI_freq <- vector(length = p+1)
        full_df <- make_data_frame(n,p,k)
        names(CI_freq) <- rownames(summary(lm(Y~.,full_df))$coefficients)
        for (i in 1:m) {
                display <- run_BE(n,p,k,alpha)
                CI_freq[1] <- CI_freq[1] + display[1,7]
                for (j in 2:nrow(display)) {
                        CI_freq[as.numeric(str_sub(rownames(display)[j], 2,-1))+1] <- CI_freq[as.numeric(str_sub(rownames(display)[j], 2,-1))+1] + display[j,7]
                }
        }
        return(CI_freq / m)
}

run_simulation(100,50,10,0.05,1000)

