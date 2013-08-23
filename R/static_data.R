.sd <- setRefClass(
  "static_data",
  fields = list(
    actual_demand = "numeric",
    expected_demand = "numeric",
    forecast_error = "numeric",
    operating_schedule = "logical",
    ordering_schedule = "logical",
    disruption = "logical",
    transit_time = "matrix"
  ),
  methods = list(
    #### more complex getters
    actualDemand = function(time, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((time - range < 1)) {
          time <- 1:time
        } else {
          time <- range:time
        }
      }
      
      return(getActualDemand(time))
    },
    expectedDemand = function(time, range = NULL) {
      # plan -- only forward looking & today
      if (!is.null(range)) {
        if ((time + range) > length(expected_demand)) {
          time <- time:(length(expected_demand))
        } else {
          time <- time:range
        }
      }
        

      
      return(getExpectedDemand(time))
    },
    forecastError = function(time, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((time - range < 1)) {
          time <- 1:time
        } else {
          time <- range:time
        }
      }
      
      return(getForecastError(time))
    },
    isOperating = function(time, range = NULL) {
      # plan -- only forward looking & today
      stopifnot(range >= time | is.null(range))
      if (!is.null(range)) time <- time:range
      
      return(getOperatingSchedule(time))
    },
    isOrdering = function(time, range = NULL) {
      # plan -- only forward looking & today
      stopifnot(range >= time | is.null(range))
      if (!is.null(range)) time <- time:range
      
      return(getOrderingSchedule(time))
    },
    isDisruption = function(time, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((time - range < 1)) {
          time <- 1:time
        } else {
          time <- range:time
        }
      }
      
      return(getDisruption(time))
    },
    transitTime = function(time, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((time - range < 1)) {
          time <- 1:time
        } else {
          time <- range:time
        }
      }
      
      return(rowSums(getTransitTime(time), na.rm = TRUE))
    },
    
    
    #### raw getters w/ error & bounds checking
    getActualDemand = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(actual_demand)
      } else {
        # bounds checking
        if (max(time) > length(actual_demand)) {
          time <- (min(time)):length(actual_demand)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(actual_demand[time])
      }
    },
    getExpectedDemand = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(expected_demand)
      } else {
        # bounds checking
        if (max(time) > length(expected_demand)) {
          time <- (min(time)):length(expected_demand)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(expected_demand[time])
      }
    },
    getForecastError = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(forecast_error)
      } else {
        # bounds checking
        if (max(time) > length(forecast_error)) {
          time <- (min(time)):length(forecast_error)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(forecast_error[time])
      }
    },
    getOperatingSchedule = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(operating_schedule)
      } else {
        # bounds checking
        if (max(time) > length(operating_schedule)) {
          time <- (min(time)):length(operating_schedule)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(operating_schedule[time])
      }
    },
    getOrderingSchedule = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(ordering_schedule)
      } else {
        # bounds checking
        if (max(time) > length(ordering_schedule)) {
          time <- (min(time)):length(ordering_schedule)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(ordering_schedule[time])
      }
    },
    getDisruption = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(disruption)
      } else {
        # bounds checking
        if (max(time) > length(disruption)) {
          time <- (min(time)):length(disruption)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(disruption[time])
      }
    },
    getTransitTime = function(time = NULL) {
      # error checking
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(transit_time)
      } else {
        # bounds checking
        if (max(time) > nrow(transit_time)) {
          time <- (min(time)):nrow(transit_time)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(transit_time[time, ])
      }
    }
  )
)

# expects full vectors
static_data <- function(ACTUAL_DEMAND, EXPECTED_DEMAND, OPERATING_SCHEDULE,
                                 ORDERING_SCHEDULE, DISRUPTION, TRANSIT_TIME)  {
  stopifnot(is.numeric(ACTUAL_DEMAND))
  stopifnot(is.numeric(EXPECTED_DEMAND))
  stopifnot(is.logical(OPERATING_SCHEDULE))
  stopifnot(is.logical(ORDERING_SCHEDULE))
  stopifnot(is.logical(DISRUPTION))
  stopifnot(is.numeric(TRANSIT_TIME))
  
  # ACTUAL DEMAND IS THE LITMUS TEST HERE
  stopifnot(length(ACTUAL_DEMAND) == length(EXPECTED_DEMAND)) 
  stopifnot(length(ACTUAL_DEMAND) == length(OPERATING_SCHEDULE))
  stopifnot(length(ACTUAL_DEMAND) == length(ORDERING_SCHEDULE))
  stopifnot(length(ACTUAL_DEMAND) == length(DISRUPTION))
  stopifnot(length(ACTUAL_DEMAND) == nrow(TRANSIT_TIME))
  
  # forcing TRANSIT_TIME to matrix, if easily doable
  if (!is.matrix(TRANSIT_TIME)) {
    if (is.data.frame(TRANSIT_TIME)) TRANSIT_TIME <- as.matrix(TRANSIT_TIME)
    else if (is.vector(TRANSIT_TIME)) TRANSIT_TIME <- as.matrix(TRANSIT_TIME, ncol = 1)
    else stop("Cannot convert Transit Time to matrix.")
  }
  
  temp <- .sd$new(
    actual_demand = ACTUAL_DEMAND,
    expected_demand = EXPECTED_DEMAND,
    forecast_error = (ACTUAL_DEMAND - EXPECTED_DEMAND),
    operating_schedule = OPERATING_SCHEDULE,
    ordering_schedule = ORDERING_SCHEDULE,
    disruption = DISRUPTION,
    transit_time = TRANSIT_TIME)
  
  return(temp)
}

