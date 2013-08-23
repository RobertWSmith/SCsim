library(testthat)

# source('C:/Users/a421356/R-GitHub/SCsim/R/dynamic_data.R')
# test_file('C:/Users/a421356/R-GitHub/SCsim/inst/tests/test-dynamic_data.R')

#  (ON_HAND_INVENTORY, SIMULATION_DAYS)

#### TEST OBJECTS ------
# oh.pass <- 100
# oh.fail <- list(num = 100:101, char = "string", bool = TRUE)
# 
# days.pass <- 250
# days.fail <- list(num = 100:101, char = "string", bool = TRUE)

#### Dynamic Data Class - Loads Correctly ----
context("Dynamic Data Class - Loads Correctly")
test_that("Dynamic Data Class - Loads Correctly",
{ 
  source('C:/Users/a421356/R-GitHub/SCsim/R/dynamic_data.R')
  
  oh.pass <- 100
  days.pass <- 250
  
  expect_that(dynamic_data(oh.pass, days.pass), is_a("dynamic_data"))
})

#### Dynamic Data Class - Fails Correctly ----
context("Dynamic Data Class - Fails Correctly")
test_that("Dynamic Data Class - Fails Correctly",
{ 
  source('C:/Users/a421356/R-GitHub/SCsim/R/dynamic_data.R')
  
  oh.pass <- 100
  oh.fail <- list(num = 100:101, char = "string", bool = TRUE)
  
  days.pass <- 250
  days.fail <- list(num = 100:101, char = "string", bool = TRUE)
  
  for (o in 1:length(oh.fail)) {
    expect_that(dynamic_data(oh.fail[[o]], days.pass), throws_error())
  }
  
  for (d in 1:length(days.fail)) {
    expect_that(dynamic_data(oh.pass, days.fail[[d]]), throws_error())
  }
})

#### Dynamic Data Class - Methods Load & Return Correctly ----
context("Dynamic Data Class - Methods Load & Return Correctly")
test_that("Dynamic Data Class - Methods Load & Return Correctly",
{
  source('C:/Users/a421356/R-GitHub/SCsim/R/dynamic_data.R')
  
  oh.pass <- 100
  days.pass <- 250
  
  dd <- dynamic_data(oh.pass, days.pass)
})