#' Almond profit model
#' 
#' Computes profit for almond production based on yield anomalies
#' @param yield_anomaly The yield anomaly value (ton/acre)
#' @param baseline_yield The baseline almond yield (ton/acre)
#' @param almond_price_per_ton Price of almonds ($/ton)
#' @param cost_per_acre Base production cost ($/acre)
#' @param negative_anomaly_cost Cost adjustment factor for anomalies
#' @return Profit ($/acre)
almond_profit <- function(yield_anomaly, 
                          baseline_yield = 0.5, 
                          almond_price_per_ton = 5000, 
                          cost_per_acre = 2000,
                          negative_anomaly_cost = 250) {
  
  # Calculate actual yield
  yield <- baseline_yield + yield_anomaly
  
  # Adjust production costs based on negative anomaly 
  cost <- cost_per_acre
  if(yield_anomaly < 0) {
    costs <- cost_per_acre - (negative_anomaly_cost * yield_anomaly)
  }
  
  # Calculate revenue
  revenue <- yield * almond_price_per_ton
  
  # Calculate profit
  profit <- revenue - cost
  
  return(profit)
}
