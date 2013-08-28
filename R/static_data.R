.sd <- setRefClass(
  "static_data",
  fields = list(
    actual_demand = "numeric",
    expected_demand = "numeric",
    forecast_error = "numeric",
    operating_schedule = "logical",
    ordering_schedule = "logical",
    disruption = "logical",
    transit_current_day = "matrix"
  ),
  methods = list(
    #### SIMPLIFIED GETTERS -- FOR USERS
    actualDemand = function(current_day, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((current_day - range < 1)) {
          current_day <- 1:current_day
        } else {
          current_day <- (current_day-range):current_day
        }
      }
      
      return(getActualDemand(current_day))
    },
    expectedDemand = function(current_day, range = NULL) {
      # plan -- only forward looking & today
      if (!is.null(range)) {
        if ((current_day + range) > length(expected_demand)) {
          current_day <- current_day:(length(expected_demand))
        } else {
          current_day <- current_day:(current_day+range)
        }
      }
      
      return(getExpectedDemand(current_day))
    },
    forecastError = function(current_day, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((current_day - range < 1)) {
          current_day <- 1:current_day
        } else {
          current_day <- (current_day-range):current_day
        }
      }
      
      return(getForecastError(current_day))
    },
    isOperating = function(current_day, range = NULL) {
      # plan -- only forward looking & today
      if (!is.null(range)) {
        if ((current_day + range) > length(operating_schedule)) {
          current_day <- current_day:length(operating_schedule)
        } else {
          current_day <- current_day:(current_day+range)
        }
      }
      
      return(getOperatingSchedule(current_day))
    },
    isOrdering = function(current_day, range = NULL) {
      # plan -- only forward looking & today
      if (!is.null(range)) {
        if ((current_day + range) > length(ordering_schedule)) {
          current_day <- current_day:length(ordering_schedule)
        } else {
          current_day <- current_day:(current_day+range)
        }
      }
      
      return(getOrderingSchedule(current_day))
    },
    isDisruption = function(current_day, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((current_day - range < 1)) {
          current_day <- 1:current_day
        } else {
          current_day <- (current_day-range):current_day
        }
      }
      
      return(getDisruption(current_day))
    },
    transitCurrentDay = function(current_day, range = NULL) {
      # history -- only backward looking & today
      if (!is.null(range)) {
        if ((current_day - range < 1)) {
          current_day <- 1:current_day
        } else {
          current_day <- (current_day-range):current_day
        }
      }
      
      return(rowSums(getTransitcurrent_day(current_day), na.rm = TRUE))
    },
    
    #### raw getters w/ error & bounds checking 
    getActualDemand = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(actual_demand)
      } else {
        # bounds checking
        if (max(current_day) > length(actual_demand)) {
          current_day <- (min(current_day)):length(actual_demand)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(actual_demand[current_day])
      }
    },
    getExpectedDemand = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(expected_demand)
      } else {
        # bounds checking
        if (max(current_day) > length(expected_demand)) {
          current_day <- (min(current_day)):length(expected_demand)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(expected_demand[current_day])
      }
    },
    getForecastError = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(forecast_error)
      } else {
        # bounds checking
        if (max(current_day) > length(forecast_error)) {
          current_day <- (min(current_day)):length(forecast_error)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(forecast_error[current_day])
      }
    },
    getOperatingSchedule = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(operating_schedule)
      } else {
        # bounds checking
        if (max(current_day) > length(operating_schedule)) {
          current_day <- (min(current_day)):length(operating_schedule)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(operating_schedule[current_day])
      }
    },
    getOrderingSchedule = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(ordering_schedule)
      } else {
        # bounds checking
        if (max(current_day) > length(ordering_schedule)) {
          current_day <- (min(current_day)):length(ordering_schedule)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(ordering_schedule[current_day])
      }
    },
    getDisruption = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(disruption)
      } else {
        # bounds checking
        if (max(current_day) > length(disruption)) {
          current_day <- (min(current_day)):length(disruption)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(disruption[current_day])
      }
    },
    getTransitCurrentDay = function(current_day = NULL) {
      # error checking
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(transit_current_day)
      } else {
        # bounds checking
        if (max(current_day) > nrow(transit_current_day)) {
          current_day <- (min(current_day)):nrow(transit_current_day)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(as.matrix(transit_current_day[current_day, ], nrow = length(current_day)))
      }
    }
  )
)

# expects full vectors
static_data <- function(ACTUAL_DEMAND, EXPECTED_DEMAND, OPERATING_SCHEDULE,
                        ORDERING_SCHEDULE, DISRUPTION, TRANSIT_current_day)  {
  stopifnot(is.numeric(ACTUAL_DEMAND))
  stopifnot(is.numeric(EXPECTED_DEMAND))
  stopifnot(is.logical(OPERATING_SCHEDULE))
  stopifnot(is.logical(ORDERING_SCHEDULE))
  stopifnot(is.logical(DISRUPTION))
  stopifnot(is.numeric(TRANSIT_current_day))
  
  # ACTUAL DEMAND LENGTH IS THE LITMUS TEST
  stopifnot(length(ACTUAL_DEMAND) == length(EXPECTED_DEMAND)) 
  stopifnot(length(ACTUAL_DEMAND) == length(OPERATING_SCHEDULE))
  stopifnot(length(ACTUAL_DEMAND) == length(ORDERING_SCHEDULE))
  stopifnot(length(ACTUAL_DEMAND) == length(DISRUPTION))
  stopifnot(length(ACTUAL_DEMAND) == nrow(TRANSIT_current_day))
  
  # forcing TRANSIT_current_day to matrix, if easily doable
  if (!is.matrix(TRANSIT_current_day)) {
    if (is.data.frame(TRANSIT_current_day)) {
      
      # CHECKS IF ALL COLUMNS IN DATA FRAME ARE NUMERIC -- IF FALSE THROWS ERROR
      if (sapply(TRANSIT_current_day, is.numeric) == rep(TRUE, length(cols))) {
        TRANSIT_current_day <- as.matrix(TRANSIT_current_day)
      } else {
        stop("Data frames are required to only have numeric columns to be able\n"+
               "to convert to a numeric matrix (required format).")
      }
    }
    else if (is.vector(TRANSIT_current_day)) {
      TRANSIT_current_day <- as.matrix(TRANSIT_current_day, ncol = 1)
    }
    else {
      stop("Cannot convert Transit current_day to numeric matrix (required format).\n" +
             "Please supply either numeric matrix, numeric vector or \n" + 
             "data frame with only numeric columns.")
    }
  }
  
  temp <- .sd$new(
    actual_demand = ACTUAL_DEMAND,
    expected_demand = EXPECTED_DEMAND,
    forecast_error = (ACTUAL_DEMAND - EXPECTED_DEMAND),
    operating_schedule = OPERATING_SCHEDULE,
    ordering_schedule = ORDERING_SCHEDULE,
    disruption = DISRUPTION,
    transit_current_day = TRANSIT_current_day
  )
  
  return(temp)
}

is.static_data <- function(x) {
  return(inherits(x, "static_data"))
}

