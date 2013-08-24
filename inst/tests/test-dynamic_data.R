library(testthat)

# source(file.path(getwd(), "R", "dynamic_data.R"))
# test_file(file.path(getwd(), "inst", "tests", "test-dynamic_data.R"))

#### TEST OBJECTS ------
oh.pass <- 100
oh.fail <- list(num = 100:101, char = "string", bool = TRUE)

days.pass <- 250
days.fail <- list(num = 100:101, char = "string", bool = TRUE)

#### Dynamic Data Class - Loads Correctly ----
context("Dynamic Data Class - Loads Correctly")
test_that("Dynamic Data Class - Loads Correctly",
{ 
  expect_that(dynamic_data(oh.pass, days.pass), is_a("dynamic_data"))
})

#### Dynamic Data Class - Fails Correctly ----
context("Dynamic Data Class - Fails Correctly")
test_that("Dynamic Data Class - Fails Correctly",
{ 
  for (o in 1:length(oh.fail)) {
    expect_that(dynamic_data(oh.fail[[o]], days.pass), throws_error())
  }
  
  for (d in 1:length(days.fail)) {
    expect_that(dynamic_data(oh.pass, days.fail[[d]]), throws_error())
  }
})

#### Dynamic Data Class - Methods Load & Return Correctly ----
context("Dynamic Data Class - Simple Methods Load & Return Correctly")
test_that("Dynamic Data Class - Methods Load & Return Correctly",
{
  dd <- dynamic_data(oh.pass, days.pass)
  
  expect_that(dd$getOnHandInventory(1), is_equivalent_to(oh.pass))
  expect_that(dd$getPipelineTarget(1), is_equivalent_to(0))
  expect_that(dd$getInTransitOrder(1), is_equivalent_to(0))
  expect_that(dd$getDemandTarget(1), is_equivalent_to(0))
  expect_that(dd$getOrderVolume(1), is_equivalent_to(0))
  expect_that(dd$getReleaseDate(1), is_equivalent_to((days.pass + 1)))
  expect_that(dd$getDelivered(1), is_equivalent_to(FALSE))
  expect_that(dd$getInTransitVolume(1), is_equivalent_to(0))
  
  expect_that(length(dd$getOnHandInventory()), is_equivalent_to(days.pass))
  expect_that(length(dd$getPipelineTarget()), is_equivalent_to(days.pass))
  expect_that(length(dd$getInTransitOrder()), is_equivalent_to(days.pass))
  expect_that(length(dd$getDemandTarget()), is_equivalent_to(days.pass))
  expect_that(length(dd$getOrderVolume()), is_equivalent_to(days.pass))
  expect_that(length(dd$getReleaseDate()), is_equivalent_to(days.pass))
  expect_that(length(dd$getDelivered()), is_equivalent_to(days.pass))
  expect_that(length(dd$getInTransitVolume()), is_equivalent_to(days.pass))
  
  time <- 2
  vol <- 1000
  
  # dd$setOnHandInventory()
})

context("Dynamic Data Class - Complex Methods Load & Return Correctly")

