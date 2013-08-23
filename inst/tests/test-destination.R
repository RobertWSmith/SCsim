library(testthat)

days <- 100
char.pass <- "test"
char.fail <- 2

list.pass <- list(mean = 2, sd = 4)
list.fail <- list(mn = 2, stdev = 4)

numvec.pass <- rep(x = 10, times = days)
numvec.fail <- rep(x = 10, times = (days + 10))

bias.pass <- list(high = 0.25, low = 0.75)
bias.fail.L1 <- list(high = 0.75, low = 0.75)
bias.fail.L2 <- list(high = 0.25, low = 0.3)
bias.fail.V <- c(0.5, 0.5)

schedule.pass <- list(open = 5, ordering = 5)
schedule.fail.N <- 4
schedule.fail.NV <- c(2,4)

context("Destination Data Import")
test_that("Import function returns destination class with appropriate inputs",
{
  # list pass in example
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = list.pass, .expected = list.pass, .bias = bias.pass,
                .schedule = schedule.pass), 
    is_true()
#     is_a("destination")
  )
  # vector pass-in example
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.pass, .expected = numvec.pass, .bias = bias.pass,
                .schedule = schedule.pass), 
    is_true()
#     is_a("destination")
  )
  # minimal example - list input
  expect_that(
    destination(.days = days, .name = NULL, .region = NULL, 
                .actual = list.pass, .expected = list.pass), 
    is_true()
#     is_a("destination")
  )
  # minimal example - numeric vector input
  expect_that(
    destination(.days = days, .name = NULL, .region = NULL, 
                .actual = numvec.pass, .expected = numvec.pass), 
    is_true()
#     is_a("destination")
  )
})

context("Destination Data Throws Errors Appropriately")
test_that("Errors thrown appropriately", 
{
  
  expect_that(
    destination(.days = days, .name = char.fail, .region = char.pass, 
                .actual = list.pass, .expected = list.pass), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.fail, 
                .actual = list.pass, .expected = list.pass), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = list.fail, .expected = list.pass), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = list.pass, .expected = list.fail), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.fail, .expected = list.fail), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.fail, .expected = list.pass), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.fail, .expected = numvec.pass), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.pass, .expected = numvec.fail), 
    throws_error()
  )
  expect_that(
    destination(.days = days, .name = char.pass, .region = char.pass, 
                .actual = numvec.pass, .expected = numvec.fail), 
    throws_error()
  )
})

context("Destination IS Method")
test_that("Is Method evaluates appropriately", {
  expect_that(is.destination(5), is_false())
  expect_that(is.destination("char"), is_false())
  expect_that(is.destination(factor("fact"), is_false()))
  expect_that(
    is.destination(
      destination(.days = days, .name = char.pass, .region = char.pass, 
                  .actual = list.pass, .expected = list.pass, .bias = bias.pass,
                  .schedule = schedule.pass)
    ), 
    is_true()
  )
  # vector pass-in example
  expect_that(
    is.destination(
      destination(.days = days, .name = char.pass, .region = char.pass, 
                  .actual = numvec.pass, .expected = numvec.pass, .bias = bias.pass,
                  .schedule = schedule.pass)              
    ), 
    is_true()
  )
  # minimal example - list input
  expect_that(
    is.destination(
      destination(.days = days, .name = NULL, .region = NULL, 
                  .actual = list.pass, .expected = list.pass)
    ), 
    is_true()
  )
  # minimal example - numeric vector input
  expect_that(
    is.destination(
      destination(.days = days, .name = NULL, .region = NULL, 
                  .actual = numvec.pass, .expected = numvec.pass)
    ), 
    is_true()
  )
})




