### destination reference class

# @title Destination Reference Class
# 
# @description
# \code{.dest} holds the reference class initializer for the detaintion class.
# 
# @details
# This class holds the detsination data, such as the in-transit, on-hand & 
# various other elements of the discrete event simulation for supply chain / 
# transportation simulation experiements
# 
# @rdName destination
# @export 
.dest <- setRefClass(
  "destination",
  fields = list(
    date = "numeric",
    name = "character",
    region = "character",
    on_hand_inventory = "numeric",
    actual_demand = "numeric",
    expected_demand = "numeric",
    forecast_error = "numeric",
    pipeline_target = "numeric",
    in_transit_order = "numeric",
    demand_target = "numeric",
    operating_schedule = "logical",
    ordering_schedule = "logical",
    orders_per_week = "numeric",
    strategy = "character",
    disruption = "logical",
    order_volume = "numeric",
    release_date = "numeric",
    delivered = "logical",
    in_transit_volume = "numeric",
    shipment_size = "numeric",
    transit_time = "matrix"
  )
)

.dest$methods(
  ##############################################################################
  # ORDERING STRATEGIES
  GORstrategy = function() {
    
  },
  reorderPoint = function() {
    
  },
  used = function() {
    
  },
  ##############################################################################
  # SIMULATION LOGIC
  dailyDemand = function(time) {
    
  },
  receiptLogic = function(time) {
    
  },
  inboundFreight = function(time) {
    
  },
  outboundFreight = function(time, vol) {
    
  },
  ##############################################################################
  # UTILITIES - CALCULATIONS OFTEN REPEATED
  calculateInTransitVolume = function() {
    
  },  
  # out of bounds checking function
  OOBchecker = function(time, range, start) {
    # if start == TRUE then checking for start being out of bounds
    # if start == FALSE then checking for end being out of bounds
    stopifnot(start == TRUE | start == FALSE) 
    
    if (start) {
      return(ifelse((time - range) < 1, 1, (time - range + 1)))
    } else {
      return(ifelse((time + range) >= (length(expected_demand)), (length(expected_demand)), (time + range - 1)))
    }
  },
  ##############################################################################
  # SETTERS - ADVANCED
  operateDestination = function() {
    
  },
  recieveFreight = function() {
    
  },
  placeOrder = function() {
    
  },
  ##############################################################################
  # SETTERS - SIMPLE
  setOnHandInventory = function(time, volume) {
    on_hand_inventory[time] <<- volume
  },
  setDemandTarget = function(time, volume) {
    demand_target[time] <<- volume
  },
  setPipelineTarget = function(time, volume) {
    pipeline_target[time] <<- volume
  },
  setInTransitOrder = function(time, volume) {
    in_transit_order[time] <<- volume
  },
  setOrderVolume = function(time, volume) {
    order_volume[time] <<- volume
  },
  setReleaseDate = function(time, release.date) {
    release_date[time] <<- release.date
  },
  setDelivered = function(time) {
    delivered[time] <<- TRUE
  },
  ##############################################################################
  # GETTERS - ADVANCED
  getActualRange = function(time, range) {
    # logic to contain out of bounds errors
    start <- (OOBchecker(time, range, start = TRUE))
    return(actual_demand[start:time])
  },
  getExpectedRange = function(time, range) {
    end <- (OOBchecker(time, range, start = FALSE))
    return(expected_demand[(time):end])
  },  
  getOperatingRange = function(time, range) {
    end <- (OOBchecker(time, range, start = FALSE))
    return(operating_schedule[time:end])
  },
  getOrderingRange = function(time, range) {
    end <- (OOBchecker(time, range, start = FALSE))
    return(ordering_schedule[time:end])
  },
  getTransitRange = function(time, range) {
    start <- (OOBchecker(time, range, start = TRUE))
    return(transit_time[start:time, ])
  },
  getDemandOrderRange = function() {
    start <- (OOBchecker(time, range, start = TRUE))
    return(demand_order[start:time])
  },
  getPipelineTargetRange = function() {
    start <- (OOBchecker(time, range, start = TRUE))
    return(pipeline_target[start:time])  
  },
  getErrorRange = function() {
    start <- (OOBchecker(time, range, start = TRUE))
    return(forecast_error[start:time])
  },
  ##############################################################################
  # GETTERS - SIMPLE 
  getName = function() {
    return(name)
  },
  getRegion = function() {
    return(region)
  },
  getCurrent = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(current[time])
  },
  getActual = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(actual[time])
  },
  getExpected = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(expected[time])
  },
  getError = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(error[time])
  },
  getPipelineTarget = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(pipeline_target[time])
  },
  getInTransitOrder = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(in_transit_order[time])
  },
  getDemandOrder = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(demand_order[time])
  },
  getOperatingSchedule = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(operating_schedule[time])
  },
  getOrderingSchedule = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(ordering_schedule[time])
  },
  getOrdersPerWeek = function() {
    return(orders_per_week)
  },
  getStrategy = function() {
    return(strategy)
  },
  getDisruption = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(disruption[time])
  },
  getOrderVolume = function() {
    return(order_volume)
  },
  getReleaseDate = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(release_date[time])
  },
  getDelivered = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(delivered[time])
  },
  getInTransitVolume = function(time = NULL) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(in_transit_volume[time])
  },
  getShipmentSize = function() {
    return(shipment_size)
  },
  getTransitTime = function(time) {
    # if no time entered, return whole vector
    if (is.null(time)) time <- 1:length(time)
    return(transit_time[time, ])
  }
)



#### Destination Object --------------------------------------------------------
#' @title Destination Object
#' 
#' @description 
#' \code{destination} initializes the reference class for the destination class.
#' 
#' @details
#' This class holds the detsination data, such as the in-transit, on-hand & 
#' various other elements of the discrete event simulation for supply chain / 
#' transportation simulation experiements
#' 
#' @param .days number of days for the simulation to run
#' @param .name the destination name
#' @param .region the name of the destination's region
#' @param .actual a vector of length equal to the value imported to .days or a named list denoting the mean and standard deviation as 'mean' and 'sd'
#' @param .expected a vector of length equal to the value imported to .days or a named list denoting the mean and standard deviation as 'mean' and 'sd'
#' @param .schedule named list for the operating schedule, 'open' and 'ordering' each with integer inputs; defaults
#' @param .bias the statistical bias of the forecast, a numeric named list, 'high' and 'low', of which the sum must equal 1; defaults to 'high' = .5, 'low' = .5
#' 
#' @rdname destination
#' @export
destination <- function(.days, .actual, .expected, 
                        .bias = list(high = 0.5, low = 0.5),
                        .schedule = list(open = 6, ordering = 6),
                        .name = NULL, .region = NULL, .on_hand_inventory = NULL) {
  
  ###### function to generate schedules
  # nOp the number of open days per week
  # totSim the number of days in the simulation experiment
  gen.sched <- function(nOp, totSim) {
    wrk <- logical(length = totSim)
    for (i in 1:totSim) {
      if (i %% 7 < nOp) wrk[i] <- TRUE
      else  wrk[i] <- FALSE
    }
    return(wrk)
  }
  
  # Error checking
  stopifnot(length(.days) == 1); stopifnot(is.numeric(.days))
  stopifnot(length(.bias) == 2); stopifnot(names(.bias) %in% c('high', 'low')); 
  stopifnot(sum(as.numeric(.bias)) == 1)
  stopifnot(is.list(.schedule)); stopifnot(length(.schedule) == 2); 
  
  if (length (.actual) == 2) { 
    stopifnot(length(.actual) == 2 & is.list(.actual) & names(.actual) %in% c("mean", "sd")) 
    
    actual.demand <- rnorm(.days, mean = 0, sd = as.numeric(.actual$sd))
    actual.demand[1] <- as.numeric(.actual$mean)
    actual.demand <- cumsum(actual.demand)
  } else { 
    stopifnot(length(.actual) == .days & is.numeric(.actual)) 
    
    actual.demand <- .actual
  }
  
  if (length (.expected) == 2) { 
    stopifnot(length(.expected) == 2 & is.list(.expected) & names(.expected) %in% c("mean", "sd")) 
    
    # multiply by 0.25 to decrease error amplitude... hopefully within reason testing to confirm
    expected.demand <- (sqrt((rnorm(.days, mean = 0, sd = as.numeric(.expected$sd)))^2) * 
      0.25 * .bias) 
    expected.demand <- expected.demand + actual.demand
  } else { 
    stopifnot(length(.expected) == .days & is.numeric(.expected)) 
    
    expected.demand <- .expected
  }
  
  if (is.null(.name)) {
    name <- "sample_name"
  } else {
    stopifnot(length(.name) == 1) 
    stopifnot(is.character(.name))
    
    name <- .name
  }
  
  if (is.null(.region)) {
    region <- "sample_region"
  } else {
    stopifnot(length(.region) == 1) 
    stopifnot(is.character(.region))
    
    region <- .region
  }
  
  if (is.null(.on_hand_inventory)) {
    on.hand.inventory <- numeric(length = .days)
  } else {
    stopifnot(length(.on_hand_inventory) == 1) 
    stopifnot(is.numeric(.on_hand_inventory))
    on.hand.inventory <- c(.on_hand_inventory, numeric(length = (.days - 1)))
  }
  
  simulation.days <- 1:.days
  open.schedule <- gen.sched(.days, .schedule$open)
  operation.schedule <- gen.sched(.days, .schedule$ordering)
  
  
  
  
  
  
  return(TRUE)
}


#### is method ------------
#' @title Is an Object from class 'destination'?
#' 
#' @details
#' Function to test inheritance relationship between an object and class \code{destination}.
#' 
#' @param x object to be tested
#' 
#' @example
#' x <- destination(.days = 100, .actual = list(mean = 200, sd = 25), .expected = list(mean = 200, sd = 25))
#' is.destination(x)
#' 
#' @export
is.destination <- function(x) return(inherits(x, "destination"))


