library(testthat)

.dest <- setRefClass(
  "destination_data",
  fields = list(
    background = "background_data",
    static = "static_data",
    dynamic = "dynamic_data"
  ),
  methods = list(
    incrementDay = function(current_day) {
      # take out current day's needs from on-hand inventory
      dynamic$updateOnHandInventory(current_day, static$getActualDemand(current_day))
      
      # update the in transit inventory tracking
      dynamic$updateInTransitInventory(current_day)
      
      # add in cargo to be recieved
      dynamic$readyForDelivery(current_day)
      
      # place order --- validity checking within the function
      dynamic$placeOrder(current_day, background$getStrategy())
    },
    placeOrder = function() {
      if (background$getStrategy() == "8_REASONS") {
        dynamic$8_reasons()
      } else if (background$getStrategy() == "REPLENISH") {
        dynamic$replenish()
      } else if (background$getStrategy() == "ORDER_POINT") {
        dynamic$order_point()
      }
    },
    8_reasons = function(current_day) {
      if (static_data$isOperating(current_day)) {
        samples <- 50
        day_range <- 7
        
        # pipeline target -- calculated as:
        # round the quantile of interest to estimate transit time based on the number of samples
        # multiply that by the mean of expected demand from today to the number of samples 
        qnt <- background_data$getQuantile()
        trans_samp <- static_data$transitCurrentDay(current_day, samples)
        exp_dmd <- static$getExpectedDemand(current_day, samples)
        updatePipelineTarget(current_day, round(quantile(transit_sample, probs = quant)), mean(exp_dmd))
        
        dynamic_data$updateInTransitOrder(current_day)
        
        # demand order -- calculated as:
        # sum the expected demand over the day range (7 days) + the forecast error over the last day range
        exp_dmd <- static_data$getExpectedDemand(current_day, day_range)
        fcst_err <- static_data$getForecastError(current_day, day_range)
        
        dynamic_data$updateDemandOrder(current_day, (exp_dmd + fcst_err))
        
        
        # final order -- calculated as:
        # max order <- in transit order + demand order
        # closer order <- max order - on hand inventory
        ### next step effectively 'cielings' to calculate the final order
        # final order <- (closer order / shipment size) * shipment size
        order <- (ceiling(
          (getInTransitOrder(current_day) + 
             getDemandOrder(current_day) - 
             getOnHandInventory(current_day)) / 
            background_data$getShipmentSize() ) *
                    background_data$getShipmentSize()
        )
        rel.date <- static_data$transitCurrentDay(current_day)
        dynamic_data$placeOrder(current_day, order, rel.date)
      }
    }, 
    replenish = function(current_day) { 
      samples <- 50
      
      shp_sz <- background$getShipmentSize()
      
      if (current_day > 2) {
        today_dmd <- static$getActualDemand(current_day) + getDemandOrder((current_day - 1))
        dynamic$updateDemandOrder(current_day, today_dmd)
      } else {
        today_dmd <- 0
      } 
      
      if (today_dmd > shp_sz) {
        order <- (ceiling(today_dmd / shp_sz)) * shp_sz
        rel.date <- static_data$transitCurrentDay(current_day)
        dynamic_data$placeOrder(current_day, order, rel.date)
        
        dynamic$updateDemandOrder(current_day, 0)
      } else {
        order <- 0
      }
      
      rel.date <- static_data$transitCurrentDay(current_day)
      dynamic_data$placeOrder(current_day, order, rel.date)
    },
    order_point = function(current_day) {
      samples <- 50
      
      shp_sz <- static$getShipmentSize()
      
      transit_mean <- round(mean(static$transitCurrentDay(current_day, samples), na.rm = TRUE))
      demand_mean <- round(mean(static$expectedDemand(current_day, samples), na.rm = TRUE))
      
      trans_min <- transit_mean * demand_mean * 0.75
      trans_max <- transit_mean * demand_mean
      
      if (dynamic$getOnHandInventory(current_day) < trans_min) {
        order <- trans_max - getOnHandInventory(current_day)
        order <- (ceiling(order / shp_sz)) * shp_sz
      } else {
        order <- 0
      }
      
      rel.date <- static_data$transitCurrentDay(current_day)
      dynamic_data$placeOrder(current_day, order, rel.date)
    }
  )
)

destination_data <- function(BACKGROUND_DATA, STATIC_DATA, DYNAMIC_DATA) {
  stopifnot(is.background_data(BACKGROUND_DATA))
  stopifnot(is.static_data(STATIC_DATA))
  stopifnot(is.dynamic_data(DYNAMIC_DATA))  
  
  temp <- .dest$new(
    current_day = 1, 
    background = BACKGROUND_DATA, 
    static = STATIC_DATA, 
    dynamic = DYNAMIC_DATA
  )
  
  return(temp)
}

is.destination_data <- function(x) {
  return(inherits(x, "destination_data"))
}

