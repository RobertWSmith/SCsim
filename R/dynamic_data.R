library(testthat)

.dd <- setRefClass(
  "dynamic_data",
  fields = list(
    on_hand_inventory = "numeric",
    pipeline_target = "numeric",
    in_transit_order = "numeric",
    demand_target = "numeric",
    order_volume = "numeric",
    release_date = "numeric",
    delivered = "logical",
    in_transit_volume = "numeric"
  ),
  methods = list(
    #### logic
    updateOnHandInventory = function(current_day, volume) {
      # removes the amount of inventory passed to volume for "tomorrow"
      setOnHandInventory((current_day + 1), (getOnHandInventory(current_day) - volume))
    },
    updateInTransitInventory = function(current_day) {
      start <- ifelse((current_day - 21) < 1, 1, (current_day - 21))
      
      vol.log <- (in_transit_volume > 0)
      del.log <- !(delivered)
      
      total.in_transit <- 0
      for (i in start:current_day) {
        if (vol.log[i] & del.log[i]) {
          total.in_transit <- total.in_transit + in_transit_volume[i]
        }
      }
      
      return(total.in_transit)
    },
    updatePipelineOrder = function(current_day, quant_transit, expected_demand) {
      
      setPipelineTarget(
        current_day, 
        (quant_transit * expected_demand)
      )
      
    },
    updateInTransitOrder = function(current_day) {
      # in transit order -- calculated as:
      # take pipeline target (calculated above) and subtract currently in transit cargo volume
      setInTransitTarget(current_day, getPipelineTarget(current_day) - getInTransitTarget(current_day))
    },
    updateDemandOrder = function(current_day, dmd_order) {
      setDemandTarget(current_day, dmd_order)
    },
    placeOrder = function(current_day, order, release) {
      setReleaseDate(current_day, release)
      if (order > 0) {
        setInTransitVolume(current_day, order)
      } else {
        setInTransitVolume(current_day, 0)
      }
    },
    readyForDelivery = function(current_day) {
      start <- ifelse((current_day - 50) < 1, 1, (current_day - 50))
      
      # looking for volume greater than zero, release date less than or equal
      # to current simulation current_day & delivered status to be FALSE
      vol.log <- (in_transit_volume > 0) 
      rel.log <- (release_date <= current_day) # eligible for release == TRUE
      del.log <- !(delivered) # not delivered == TRUE
      
      total.released <- 0
      for (i in start:current_day) {
        if (vol.log[i] & rel.log[i] & del.log[i]) { # all TRUE get added together & delivery released
          total.released <- total.released + in_transit_volume[i]
          setDelivered(i)
        }
      }
      
      return(total.released)
    },
    
    #### simple setters w/ error & bounds checking
    setOnHandInventory = function(current_day, volume) {
      if (current_day < length(on_hand_inventory)) {
        on_hand_inventory[current_day] <<- volume
      } 
    },
    setPipelineTarget = function(current_day, volume) {
      if (current_day < length(pipeline_target)) {
        pipeline_target[current_day] <<- volume
      }      
    },
    setInTransitTarget = function(current_day, volume) {
      if (current_day < length(in_transit_order)) {
        in_transit_order[current_day] <<- volume
      }      
    },
    setDemandTarget = function(current_day, volume) {
      if (current_day < length(demand_target)) {
        demand_target[current_day] <<- volume
      }      
    },
    setOrderVolume = function(current_day, volume) {
      if (current_day < length(order_volume)) {
        order_volume[current_day] <<- volume
      }      
    },
    setReleaseDate = function(current_day, date) {
      if (current_day < length(release_date)) {
        release_date[current_day] <<- date
      }      
    },
    setDelivered = function(current_day) {
      if (current_day < length(delivered)) {
        delivered[current_day] <<- TRUE
      }      
    },
    setInTransitVolume = function(current_day, volume) {
      if (current_day < length(in_transit_volume)) {
        in_transit_volume[current_day] <<- volume
      }      
    },
    
    #### simple getters w/ error & bounds checking
    getOnHandInventory = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(on_hand_inventory)
      } else {
        # bounds checking
        if (max(current_day) > length(on_hand_inventory)) {
          current_day <- (min(current_day)):length(on_hand_inventory)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(on_hand_inventory[current_day])
      }
    },
    getPipelineTarget = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(pipeline_target)
      } else {
        # bounds checking
        if (max(current_day) > length(pipeline_target)) {
          current_day <- (min(current_day)):length(pipeline_target)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(pipeline_target[current_day])
      }
    },
    getInTransitTarget = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(in_transit_order)
      } else {
        # bounds checking
        if (max(current_day) > length(in_transit_order)) {
          current_day <- (min(current_day)):length(in_transit_order)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(in_transit_order[current_day])
      }
    },
    getDemandTarget = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(demand_target)
      } else {
        # bounds checking
        if (max(current_day) > length(demand_target)) {
          current_day <- (min(current_day)):length(demand_target)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(demand_target[current_day])
      }
    },
    getOrderVolume = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(order_volume)
      } else {
        # bounds checking
        if (max(current_day) > length(order_volume)) {
          current_day <- (min(current_day)):length(order_volume)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(order_volume[current_day])
      }
    },
    getReleaseDate = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(release_date)
      } else {
        # bounds checking
        if (max(current_day) > length(release_date)) {
          current_day <- (min(current_day)):length(release_date)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(release_date[current_day])
      }
    },
    getDelivered = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(delivered)
      } else {
        # bounds checking
        if (max(current_day) > length(delivered)) {
          current_day <- (min(current_day)):length(delivered)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(delivered[current_day])
      }
    },
    getInTransitVolume = function(current_day = NULL) {
      stopifnot(is.numeric(current_day) | is.null(current_day))
      
      if (is.null(current_day)) {
        return(in_transit_volume)
      } else {
        # bounds checking
        if (max(current_day) > length(in_transit_volume)) {
          current_day <- (min(current_day)):length(in_transit_volume)
        }
        if (min(current_day) < 1) {
          current_day <- 1:(max(current_day))
        }
        
        return(in_transit_volume[current_day])
      }
    }
  )
)

dynamic_data <- function(ON_HAND_INVENTORY, SIMULATION_DAYS) {
  stopifnot(is.numeric(ON_HAND_INVENTORY)); stopifnot(length(ON_HAND_INVENTORY) == 1)
  stopifnot(is.numeric(SIMULATION_DAYS)); stopifnot(length(SIMULATION_DAYS) == 1)
  
  temp <- .dd$new(
    on_hand_inventory = c(ON_HAND_INVENTORY, rep(0, (SIMULATION_DAYS - 1))), 
    pipeline_target = rep(0, SIMULATION_DAYS), 
    in_transit_order = rep(0, SIMULATION_DAYS), 
    demand_target = rep(0, SIMULATION_DAYS), 
    order_volume = rep(0, SIMULATION_DAYS), 
    release_date = rep((SIMULATION_DAYS + 1), SIMULATION_DAYS), 
    delivered = rep(FALSE, SIMULATION_DAYS), 
    in_transit_volume = rep(0, SIMULATION_DAYS) 
  )
  
  return(temp)
}


is.dynamic_data <- function(x) {
  return(inherits(x, "dynamic_data"))  
}

