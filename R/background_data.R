library(testthat)

.bd <- setRefClass(
  "background_data",
  fields = list(
    total_days = "numeric",
    name = "character",
    region = "character",
    strategy = "character",
    shipment_size = "numeric",
    orders_per_week = "numeric",
    bias = "numeric",
    quantile = "numeric"
  ),
  methods = list(
    getTotalDays = function() {return(total_days)},
    getName = function() {return(name)},
    getRegion = function() {return(region)},
    getStrategy = function() {return(strategy)},
    getShipmentSize = function() {return(shipment_size)},
    getOrdersPerWeek = function() {return(orders_per_week)}.
    getQuantile = function() {return(quantile)}
  )
)

background_class <- function(TOTAL_DAYS, NAME, REGION, STRATEGY, SHIPMENT_SIZE, 
                             ORDERS_PER_WEEK, BIAS = c(0.5,0.5), QUANTILE = 0.95) {
  # TYPE CHECKING -- THROWS ERROR FOR INCORRECT TYPE
  stopifnot(is.numeric(TOTAL_DAYS)); stopifnot(is.numeric(SHIPMENT_SIZE))
  stopifnot(is.numeric(ORDERS_PER_WEEK))
  stopifnot(is.character(NAME)); stopifnot(is.character(REGION)); stopifnot(is.character(STRATEGY))
  
  # DETAIL CHECKING
  stopifnot(TOTAL_DAYS > 1); stopifnot(length(TOTAL_DAYS) == 1) 
  stopifnot(toupper(STRATEGY) %in% c("8_REASONS", "REPLENISH", "ORDER_POINT")) 
  stopifnot(SHIPMENT_SIZE > 0); stopifnot(length(SHIPMENT_SIZE) == 1) 
  stopifnot(ORDERS_PER_WEEK > 0); stopifnot(ORDERS_PER_WEEK <= 7) 
  stopifnot(length(ORDERS_PER_WEEK) == 1)
  
  # IMPORTS WITH FORCED CONVERSION -- ENSURES CONSISTENCY
  # ALL CHARACTERS ARE CONVERTED TO UPPER CASE -- ENSURES CONSISTENCY
  # ALL NUMERIC SUBJECT TO FLOOR FUNCTION -- ENSURES CONSISTENCY
  temp <- .bd$new(
    total_days = floor(TOTAL_DAYS), 
    name = toupper(as.character(NAME)), 
    region = toupper(as.character(REGION)), 
    strategy = toupper(as.character(STRATEGY)), 
    shipment_size = floor(SHIPMENT_SIZE), 
    orders_per_week = floor(ORDERS_PER_WEEK),
    bias = BIAS,
    quantile = QUANTILE
  )
  
  return(temp)
}

is.background_data <- function(x) {
  return(inherits(x, "background_data"))
}
