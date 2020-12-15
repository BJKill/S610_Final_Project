#### Final Project Tests

library(testthat)
test_dir(".")

context("Check dimensions of generated data frame")
source("./FinalProject.R")

test_that("DF dimension are exactly n x (p+1)", {
        expect_equal(dim(make_data_frame(20,10,5)), c(20,11))
        expect_equal(dim(make_data_frame(50,20,10)), c(50,21))
        expect_equal(dim(make_data_frame(3,1,1)), c(3,2))
})








