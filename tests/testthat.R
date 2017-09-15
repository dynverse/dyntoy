library(testthat)
library(dynutils)
library(dplyr)

Sys.setenv("R_TESTS" = "")

test_check("dyntoy")

