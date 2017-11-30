library(testthat)
library(dynutils)
library(dyntoy)
library(dplyr)
library(tibble)

Sys.setenv("R_TESTS" = "")

test_check("dyntoy")

