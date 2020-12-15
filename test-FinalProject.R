#### Final Project Tests

library(testthat)
library(alr4)

context("Check Dimensions of Simulated Data Frame")
source("FinalProject.R")

test_that("DF dimension are exactly n x (p+1)", {
        expect_equal(dim(make_data_frame(20,10,5)), c(20,11))
        expect_equal(dim(make_data_frame(50,20,10)), c(50,21))
        expect_equal(dim(make_data_frame(3,1,1)), c(3,2))
})

context("Verify That Bad Inputs to run_simulation Return Appropriate Messages")
source("FinalProject.R")

test_that("run_simulation kicks out for appropriate erronious inputs", {
        expect_equal(run_simulation(20,20,10,.10,5), "n must be at least p+2")
        expect_equal(run_simulation(20,-20,10,.10,5), "n and p must be positive integers")
        expect_equal(run_simulation(50,20,10.5,.10,5), "k must be a positive integer not exceeding p")
        expect_equal(run_simulation(50,20,10,1.10,5), "alpha must be in the interval (0,1)")
        expect_equal(run_simulation(50,20,10,0.10,5.5), "m must be a positive integer")
})

context("Check Output of run_BE_once For Removal of Correct Variable (if Any)")
source("FinalProject.R")


## Using known data frames to verify correct variable is being removed in run_BE
df101 <- BigMac2003
df101 <- cbind(df101[,-1],df101[,1])
colnames(df101) <- c("V1", "V2","V3", "V4", "V5", "V6", "V7", "V8", "V9", "Y")
lm101 <- lm(Y~.,df101) 
summary(lm101)
lm101.1 <- lm(Y~.-V4,df101)

df102 <- UN11
df102 <- cbind(df102[, c(-5,-1,-2)], df102[,5])
colnames(df102) <- c("V1", "V2", "V3", "Y")
lm102 <- lm(Y~.,df102)
summary(lm102)
lm102.1 <- lm(Y~.-V3, df102)

df103 <- water
colnames(df103) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "Y")
lm103 <- lm(Y~.,df103)
summary(lm103)
lm103.1 <- lm(Y~.-V5,df103)

test_that("run_BE correctly identifies and eliminates most insignificant variable", {
        expect_equal(run_BE_once(df101,0.10), summary(lm101.1)$coefficients)
        expect_equal(run_BE_once(df102,0.10), summary(lm102)$coefficients)
        expect_equal(run_BE_once(df102,0.01), summary(lm102.1)$coefficients)
        expect_equal(run_BE_once(df103,0.10), summary(lm103.1)$coefficients)
})
