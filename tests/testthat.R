library(testthat)
library(dynutils)
library(dplyr)
library(ggplot2)

Sys.setenv("R_TESTS" = "")

test_check("dyntoy")

