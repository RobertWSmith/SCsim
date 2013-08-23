library(testthat)

# source('C:/Users/a421356/R-GitHub/SCsim/R/static_data.R')
# test_file('C:/Users/a421356/R-GitHub/SCsim/inst/tests/test-static_data.R')

#### TEST OBJECTS ------
# act.pass <- sample(1:100, size = 100, replace = TRUE)
# exp.pass <- sample(1:100, size = 100, replace = TRUE)
# operate.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
# order.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
# disrupt.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
# transit.pass <- as.matrix(
#   cbind(
#     sample(1:100, size = 100, replace = TRUE), 
#     sample(1:100, size = 100, replace = TRUE)
#   )
# )
# 
# act.fail <- rep("a", 100)
# exp.fail  <- rep("a", 100)
# operate.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
# order.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
# disrupt.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
# transit.fail <- as.matrix(cbind(
#   sample(c("TRUE", "FALSE"), size = 100, replace = TRUE), 
#   sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)))


#### Static Data Class - Loads Correctly ----
context("Static Data Class - Loads Correctly")
test_that("Static Data Class - Loads Correctly",
{
  source('C:/Users/a421356/R-GitHub/SCsim/R/static_data.R')
  
  act.pass <- sample(1:100, size = 100, replace = TRUE)
  exp.pass <- sample(1:100, size = 100, replace = TRUE)
  operate.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  order.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  disrupt.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  transit.pass <- as.matrix(
    cbind(
      sample(1:100, size = 100, replace = TRUE), 
      sample(1:100, size = 100, replace = TRUE)
    )
  )
  
  
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass),
    is_a("static_data")
  )
})

#### Static Data Class - Fails Correctly ----
context("Static Data Class - Fails Correctly")
test_that("Static Data Class - Fails Correctly", {
  source('C:/Users/a421356/R-GitHub/SCsim/R/static_data.R')
  
  act.pass <- sample(1:100, size = 100, replace = TRUE)
  exp.pass <- sample(1:100, size = 100, replace = TRUE)
  operate.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  order.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  disrupt.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  transit.pass <- as.matrix(
    cbind(
      sample(1:100, size = 100, replace = TRUE), 
      sample(1:100, size = 100, replace = TRUE)
    )
  )
  
  act.fail <- rep("a", 100)
  exp.fail  <- rep("a", 100)
  operate.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
  order.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
  disrupt.fail <- sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)
  transit.fail <- as.matrix(cbind(
    sample(c("TRUE", "FALSE"), size = 100, replace = TRUE), 
    sample(c("TRUE", "FALSE"), size = 100, replace = TRUE)))
  
  expect_that(
    static_data(ACTUAL_DEMAND = act.fail, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass),
    throws_error()
  )
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.fail, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass),
    throws_error()
  )
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.fail, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass),
    throws_error()
  )
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.fail, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass),
    throws_error()
  )
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.fail, TRANSIT_TIME = transit.pass),
    throws_error()
  )
  expect_that(
    static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.fail),
    throws_error()
  )
})

#### Static Data Class - Methods Load & Return Correctly ----
context("Static Data Class - Methods Load & Return Correctly")
test_that("Static Data Class - Methods Load & Return Correctly",
{
  source('C:/Users/a421356/R-GitHub/SCsim/R/static_data.R')
  
  act.pass <- sample(1:100, size = 100, replace = TRUE)
  exp.pass <- sample(1:100, size = 100, replace = TRUE)
  operate.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  order.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  disrupt.pass <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)
  transit.pass <- as.matrix(
    cbind(
      sample(1:100, size = 100, replace = TRUE), 
      sample(1:100, size = 100, replace = TRUE)
    )
  )
  
  TIME <- floor(length(act.pass) / 3)
  RANGE <- 10
  
  sd <- static_data(ACTUAL_DEMAND = act.pass, EXPECTED_DEMAND = exp.pass, 
                    OPERATING_SCHEDULE = operate.pass, ORDERING_SCHEDULE = order.pass, 
                    DISRUPTION = disrupt.pass, TRANSIT_TIME = transit.pass)
  
  #### raw getter testing
  expect_that(sd$getActualDemand(), is_equivalent_to(act.pass))
  expect_that(sd$getExpectedDemand(), is_equivalent_to(exp.pass))
  expect_that(sd$getForecastError(), is_equivalent_to((act.pass - exp.pass)))
  expect_that(sd$getOperatingSchedule(), is_equivalent_to(operate.pass))
  expect_that(sd$getOrderingSchedule(), is_equivalent_to(order.pass))
  expect_that(sd$getDisruption(), is_equivalent_to(disrupt.pass))
  expect_that(sd$getTransitTime(), is_equivalent_to(transit.pass))
  
  #### user getter testing
  expect_that(sd$actualDemand(TIME), is_equivalent_to(act.pass[TIME]))
  expect_that(sd$expectedDemand(TIME), is_equivalent_to(exp.pass[TIME]))
  expect_that(sd$forecastError(TIME), is_equivalent_to((act.pass - exp.pass)[TIME]))
  expect_that(sd$isOperating(TIME), is_equivalent_to(operate.pass[TIME]))
  expect_that(sd$isOrdering(TIME), is_equivalent_to(order.pass[TIME]))
  expect_that(sd$isDisruption(TIME), is_equivalent_to(disrupt.pass[TIME]))
  expect_that(sd$transitTime(TIME), is_equivalent_to(sum(transit.pass[TIME,], na.rm = TRUE)))
  
  #### user getters with options testing
  expect_that(sd$actualDemand(TIME, RANGE), is_equivalent_to(act.pass[RANGE:TIME]))
  expect_that(sd$expectedDemand(TIME, RANGE), is_equivalent_to(exp.pass[TIME:RANGE]))
  expect_that(sd$forecastError(TIME, RANGE), is_equivalent_to((act.pass - exp.pass)[RANGE:TIME]))
  expect_that(sd$isOperating(TIME, RANGE), is_equivalent_to(operate.pass[TIME:RANGE]))
  expect_that(sd$isOrdering(TIME, RANGE), is_equivalent_to(order.pass[TIME:RANGE]))
  expect_that(sd$isDisruption(TIME, RANGE), is_equivalent_to(disrupt.pass[RANGE:TIME]))
  expect_that(sd$transitTime(TIME, RANGE), is_equivalent_to(transit.pass[RANGE:TIME]))
})




