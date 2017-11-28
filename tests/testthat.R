library(testthat)
library(dynutils)
library(dplyr)
library(tibble)

Sys.setenv("R_TESTS" = "")

test_check("dyntoy")

