#### Final Project

## First, let's set up a series of p predictor variables: k "real" predictors and p-k "fake" predictors.
## By that, I mean that only k of the predictor variables will be used to generate the response variable.  We want to see if
## either our forward selection or backwards elimination algorithm can settle on the model containing only the k "real" predictors.
## We will have n observations, no intercept, and the linear model will take the form Y ~ 1*X1 + 2*X2 + 3*X3 + ...

library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(HyperbolicDist)


## Make a matrix of predictors to put in the model
make_model_matrix <- function(n,p) {
        if (n <= 0 | p <= 0 | is.wholenumber(n) == FALSE | is.wholenumber(p) == FALSE) {return("n and p must be positive integers")}
        if (n < p+2) {return("n must be at least p+2")}
        else {
        X <- matrix(nrow = n, ncol = p)
        for (i in 1:p) {
                X[ ,i] <- rnorm(n)
        }
        return(X)
        }
}

X1 <- make_model_matrix(-5,11); X1
X1 <- make_model_matrix(5,11); X1
X1 <- make_model_matrix(11.2,5); X1
X1 <- make_model_matrix(11,5); X1


## Makes a vector of response values (y's) based on the first k predictor variables (x's)
make_response_vector <- function(pred_mat, k) {
        if (k <= 0 | k > ncol(pred_mat) | is.wholenumber(k) == FALSE) {return("k must be a positive integer not exceeding p")}
        else {
        Y <- vector(length = nrow(pred_mat))
        for (i in 1:nrow(pred_mat)) {
                Y[i] <- rnorm(1)
                for (j in 1:k) {
                        Y[i] <- Y[i] + pred_mat[i,j]*j
                }
        }
        return(Y)
        }
}

Y1 <- make_response_vector(X1, 0.5); Y1
Y1 <- make_response_vector(X1, 20); Y1
Y1 <- make_response_vector(X1, 2); Y1


## creates a data frame of predictors and the response
make_data_frame <- function(n,p,k) {
      if (n <= 0 | p <= 0 | is.wholenumber(n) == FALSE | is.wholenumber(p) == FALSE) {return("n and p must be positive integers")}
      if (n < p+2) {return("n must be at least p+2")}
      if (k <= 0 | k > p | is.wholenumber(k) == FALSE) {return("k must be a positive integer not exceeding p")}
      else {
      X <- make_model_matrix(n,p)
      Y <- make_response_vector(X,k)
      df <- data.frame(cbind(X,Y))
      return(df)
      }
}

## Runs a single round of backwards elimination on the generated data
run_BE <- function(n,p,k,alpha) {
  if (alpha < 0 | alpha > 1) {return("alpha must be in the interval (0,1)")}
  if (n <= 0 | p <= 0 | is.wholenumber(n) == FALSE | is.wholenumber(p) == FALSE) {return("n and p must be positive integers")}
  if (n < p+2) {return("n must be at least p+2")}
  if (k <= 0 | k > p | is.wholenumber(k) == FALSE) {return("k must be a positive integer not exceeding p")}
  else {
    df <- make_data_frame(n,p,k)
    lm1 <- lm(Y~.,df)
    coef_mat <- summary(lm1)$coefficients
    maxp_ind <- which.max(coef_mat[-1,4])
    maxp_val <- coef_mat[1+maxp_ind,4]
    while(maxp_val > alpha) {
      rem_inx <- maxp_ind
      df <- df[,-rem_inx]
      lm1 <- lm(Y~.,df)
      coef_mat <- summary(lm1)$coefficients
      maxp_ind <- which.max(coef_mat[-1,4])
      maxp_val <- coef_mat[1+maxp_ind,4]
    }  ## Saved a bunch of time in the while loop by only making it evaluate the linear model
    display <- cbind(coef_mat,confint(lm1,level = (1-alpha)),vector(length = nrow(coef_mat)))
    colnames(display)[7] <- "Known Param in CI?"
    display[1,7] <- (0 >= display[1,5]) & (0 <= display[1,6])
    for (i in 2:nrow(display)) {
      index <- as.numeric(str_sub(rownames(display)[i], 2,-1))
      display[i,7] <- (index >= display[i,5]) & (index <= display[i,6])
    }
    return(display)
    }
}

BE1 <- run_BE(10,50,10,0.05); BE1
BE1 <- run_BE(100,50,15,1.2); BE1
BE1 <- run_BE(100,50,15,0.10); BE1


## Runs m simulations of backwards elimination and prints the % of the time each predictor variable was
## found to be significant and the % of the time the known parameter was in the confidence interval.
run_simulation <- function(n,p,k,alpha,m) {
  if (m <= 0 | is.wholenumber(m) == FALSE) {return("m must be a positive integer")}
  if (alpha < 0 | alpha > 1) {return("alpha must be in the interval (0,1)")}
  if (n <= 0 | p <= 0 | is.wholenumber(n) == FALSE | is.wholenumber(p) == FALSE) {return("n and p must be positive integers")}
  if (n < p+2) {return("n must be at least p+2")}
  if (k <= 0 | k > p | is.wholenumber(k) == FALSE) {return("k must be a positive integer not exceeding p")}
  else {
    CI_freq <- vector(length = p+1)
    sig_freq <- vector(length = p+1)
    full_df <- make_data_frame(n,p,k)
    var_names <- rownames(summary(lm(Y~.,full_df))$coefficients)
    names(CI_freq) <- var_names
    names(sig_freq) <- var_names
    for (i in 1:m) {
      display <- run_BE(n,p,k,alpha)
      CI_freq[1] <- CI_freq[1] + display[1,7]
      sig_freq[1] <- sig_freq[1] + as.numeric(display[1,4] <= alpha)
      for (j in 2:nrow(display)) {
        index <- as.numeric(str_sub(rownames(display)[j], 2,-1))+1
        CI_freq[index] <- CI_freq[index] + display[j,7]
        sig_freq[index] <- sig_freq[index] + 1
      }
    }
    CI_perc <- CI_freq / m
    sig_perc <- sig_freq / m
    accuracy_mat <- cbind(round(CI_perc*100,2), round((sig_freq / m)*100,2))
    if (p > 1) {
      accuracy_mat <- rbind(accuracy_mat, c(mean(accuracy_mat[2:(k+1),1]), mean(accuracy_mat[c(1,(k+2):(p+1)),2])))
      rownames(accuracy_mat)[p+2] <- "Averages"
    }
    colnames(accuracy_mat) <- c("% Param in CI", "% Param Significant")
    
    return(accuracy_mat)
  }
}

n <- 50
p <- 10
k <- 5
alpha <- 0.08
m <- 10000

BE <- run_BE(n,p,k,alpha)
BE

#output <- run_simulation(n,p,k,alpha,m)
#output

output2 <- run_simulation(100,1,1,0.10,1000); output2


### For the test_that ONLY
run_BE_once <- function(df, alpha) {
  linmod <- lm(Y~.,df)
  coef_mat <- summary(linmod)$coefficients
  maxp_ind <- which.max(coef_mat[-1,4])
  maxp_val <- coef_mat[1+maxp_ind,4]
  if(maxp_val > alpha) {
    rem_inx <- maxp_ind
    df <- df[,-rem_inx]
    lm1 <- lm(Y~.,df)
    coef_mat <- summary(lm1)$coefficients
    maxp_ind <- which.max(coef_mat[-1,4])
    maxp_val <- coef_mat[1+maxp_ind,4]
  }
  return(coef_mat)
}
