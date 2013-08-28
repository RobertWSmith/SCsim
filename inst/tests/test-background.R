
# source(file.path(getwd(), "R", "background.R"))
# test_file(file.path(getwd(), "inst", "tests", "test-background.R))


#### Background Data Class - Test Objects ----
TOTAL_DAYS = days.pass <- c(100,1000,10000)
TOTAL_DAYS = days.fail <- list(bool = TRUE,  char = "string")

name.pass <- c("fac", "FAC", "Fac")
name.fail <- list(bool = TRUE, dec = 100.00, int = 1000)

region.pass <- c("reg", "REG", "Reg")
region.fail <- list(bool = TRUE, dec = 100.00, int = 1000)

STRATEGY = strategy.pass <- c("8_REASONS", "REPLENISH", "ORDER_POINT", "8_reasons", "replenish", "order_point", "8_Reasons", "Replenish", "Order_Point")
strategy.fail <- list(bool = TRUE, dec = 100.00, int = 1000)

shipment_size.pass <- c(10, 1000, 10000)
shipment_size.fail <- list(bool = TRUE,  char = "string")

orders_per_week.pass <- c(1,3,6)
orders_per_week.fail <- list(num1 = 8, num2 = -1, num3 = 0, bool = TRUE,  char = "string")

#### Background Data Class - Loads Properly ----
context("Background Data Class - Loads Properly")
test_that("Background Data Class - Loads Properly",
{
  for (d in 1:length(days.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[d], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      is_a("background_data")
    )
  }
  
  for (n in 1:length(name.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[n], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1],
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      is_a("background_data")
    )
  }
  
  for (r in 1:length(region.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[r], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      is_a("background_data")
    )
  }
  
  for (s in 1:length(strategy.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[s], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      is_a("background_data")
    )
  }
  
  for (z in 1:length(shipment_size.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[[z]], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      is_a("background_data")
    )
  }
  
  for (o in 1:length(orders_per_week.pass)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[[o]]),
      is_a("background_data")
    )
  }
  
})

#### Background Data Class - Fails Properly ----
context("Background Data Class - Fails Properly")
test_that("Background Data Class - Fails Properly",
{
  for (d in 1:length(days.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.fail[[d]], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      throws_error())
  }
  
  for (n in 1:length(name.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.fail[[n]], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      throws_error())
  }
  
  for (r in 1:length(region.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.fail[[r]], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      throws_error())
  }
  
  for (s in 1:length(strategy.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.fail[[s]], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      throws_error())
  }
  
  for (z in 1:length(shipment_size.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.fail[[z]], 
                       ORDERS_PER_WEEK = orders_per_week.pass[1]),
      throws_error())
  }
  
  for (o in 1:length(orders_per_week.fail)) {
    expect_that(
      background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                       STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                       ORDERS_PER_WEEK = orders_per_week.fail[[o]]),
      throws_error())
  }
})

#### Background Data Class - Methods Load & Return Correctly ----
context("Background Data Class - Methods Load & Return Correctly")
test_that("Background Data Class - Methods Load & Return Correctly",
{  
  bg <- background_class(TOTAL_DAYS = days.pass[1], NAME = name.pass[1], REGION = region.pass[1], 
                         STRATEGY = strategy.pass[1], SHIPMENT_SIZE = shipment_size.pass[1], 
                         ORDERS_PER_WEEK = orders_per_week.pass[1])
  
  expect_that(bg$getTotalDays(), is_equivalent_to(floor(days.pass[1])))
  expect_that(bg$getName(), is_equivalent_to(toupper(name.pass[1])))
  expect_that(bg$getRegion(), is_equivalent_to(toupper(region.pass[1])))
  expect_that(bg$getStrategy(), is_equivalent_to(toupper(strategy.pass[1])))
  expect_that(bg$getShipmentSize(), is_equivalent_to(floor(shipment_size.pass[1])))
  expect_that(bg$getOrdersPerWeek(), is_equivalent_to(floor(orders_per_week.pass[1])))
  
  expect_that(bg$getTotalDays(), is_a("numeric"))
  expect_that(bg$getName(), is_a("character"))
  expect_that(bg$getRegion(), is_a("character"))
  expect_that(bg$getStrategy(), is_a("character"))
  expect_that(bg$getShipmentSize(), is_a("numeric"))
  expect_that(bg$getOrdersPerWeek(), is_a("numeric"))
})




# rm(list=ls())