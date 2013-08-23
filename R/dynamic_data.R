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
    
    #### raw getters w/ error & bounds checking
    
    )
)

dynamic_data <- function(ON_HAND_INVENTORY, SIMULATION_DAYS) {
  stopifnot(is.numeric(ON_HAND_INVENTORY)); stopifnot(length(ON_HAND_INVENTORY) == 1)
  stopifnot(is.numeric(SIMULATION_DAYS)); stopifnot(length(SIMULATION_DAYS) == 1)
  
  temp <- .dd$new(
    on_hand_inventory = c(ON_HAND_INVENTORY, numeric((SIMULATION_DAYS - 1))), #NUM, 0
    pipeline_target = numeric(SIMULATION_DAYS), #0
    in_transit_order = numeric(SIMULATION_DAYS), #0
    demand_target = numeric(SIMULATION_DAYS), #0
    order_volume = numeric(SIMULATION_DAYS), #0
    release_date = numeric(SIMULATION_DAYS), #0
    delivered = logical(SIMULATION_DAYS), #FALSE
    in_transit_volume = numeric(SIMULATION_DAYS) #0
    )
  
  return(temp)
}