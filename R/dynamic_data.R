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
    #### simplified setters -- for users
    updateOnHand = function(time, volume) {
      # assumes you're updating tomorrow's inventory 
      # and subtracting the volume supplied
      
      setOnHandInventory((time+1), (getOnHandInventory(time) - volume))
    },
    updateInTransitVolume = function() {
      
    },
    updateTargets = function(time) {
      
    },
    order = function(time) {
      
    },
    recieve = function(time) {
      
    },
    
    #### simplified getters -- for users
    getOnHand = function(time) {
      
    },
    getInTransitVolume = function(time) {
      
    },
    getInTransitShipment = function(time) {
      
    },
    
    #### raw setters w/ error & bounds checking
    setOnHandInventory = function(time, volume) {
      if (time < length(on_hand_inventory)) {
        on_hand_inventory[time] <<- volume
      } 
    },
    setPipelineInventory = function(time, volume) {
      if (time < length(pipeline_target)) {
        pipeline_target[time] <<- volume
      }      
    },
    setInTransitOrder = function(time, volume) {
      if (time < length(in_transit_order)) {
        in_transit_order[time] <<- volume
      }      
    },
    setDemandTarget = function(time, volume) {
      if (time < length(demand_target)) {
        demand_target[time] <<- volume
      }      
    },
    setOrderVolume = function(time, volume) {
      if (time < length(order_volume)) {
        order_volume[time] <<- volume
      }      
    },
    setReleaseDate = function(time, date) {
      if (time < length(release_date)) {
        release_date[time] <<- date
      }      
    },
    setDelivered = function(time) {
      if (time < length(delivered)) {
        delivered[time] <<- TRUE
      }      
    },
    setInTransitVolume = function(time, volume) {
      if (time < length(in_transit_volume)) {
        in_transit_volume[time] <<- volume
      }      
    },
    
    #### raw getters w/ error & bounds checking
    getOnHandInventory = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(on_hand_inventory)
      } else {
        # bounds checking
        if (max(time) > length(on_hand_inventory)) {
          time <- (min(time)):length(on_hand_inventory)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(on_hand_inventory[time])
      }
    },
    getPipelineTarget = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(pipeline_target)
      } else {
        # bounds checking
        if (max(time) > length(pipeline_target)) {
          time <- (min(time)):length(pipeline_target)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(pipeline_target[time])
      }
    },
    getInTransitOrder = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(in_transit_order)
      } else {
        # bounds checking
        if (max(time) > length(in_transit_order)) {
          time <- (min(time)):length(in_transit_order)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(in_transit_order[time])
      }
    },
    getDemandTarget = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(demand_target)
      } else {
        # bounds checking
        if (max(time) > length(demand_target)) {
          time <- (min(time)):length(demand_target)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(demand_target[time])
      }
    },
    getOrderVolume = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(order_volume)
      } else {
        # bounds checking
        if (max(time) > length(order_volume)) {
          time <- (min(time)):length(order_volume)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(order_volume[time])
      }
    },
    getReleaseDate = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(release_date)
      } else {
        # bounds checking
        if (max(time) > length(release_date)) {
          time <- (min(time)):length(release_date)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(release_date[time])
      }
    },
    getDelivered = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        return(delivered)
      } else {
        # bounds checking
        if (max(time) > length(delivered)) {
          time <- (min(time)):length(delivered)
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(delivered[time])
      }
    },
    getInTransitVolume = function(time = NULL) {
      stopifnot(is.numeric(time) | is.null(time))
      
      if (is.null(time)) {
        
        return(in_transit_volume)
        
      } else {
        # bounds checking
        if (max(time) > length(in_transit_volume)) {
          time <- (min(time)):(length(in_transit_volume))
        }
        if (min(time) < 1) {
          time <- 1:(max(time))
        }
        
        return(in_transit_volume[time])
      }
    }
    )
)

dynamic_data <- function(ON_HAND_INVENTORY, SIMULATION_DAYS) {
  stopifnot(is.numeric(ON_HAND_INVENTORY)); stopifnot(length(ON_HAND_INVENTORY) == 1)
  stopifnot(is.numeric(SIMULATION_DAYS)); stopifnot(length(SIMULATION_DAYS) == 1)
  
  temp <- .dd$new(
    on_hand_inventory = c(ON_HAND_INVENTORY, numeric((SIMULATION_DAYS - 1))), #NUM, 0
    pipeline_target = numeric(SIMULATION_DAYS), #0
    in_transit_order = numeric(SIMULATION_DAYS), #0
    in_transit_volume = numeric(SIMULATION_DAYS), #0
    demand_target = numeric(SIMULATION_DAYS), #0
    order_volume = numeric(SIMULATION_DAYS), #0
    release_date = rep((SIMULATION_DAYS + 1), SIMULATION_DAYS), #0
    delivered = logical(SIMULATION_DAYS) #FALSE
    )
  
  return(temp)
}