#' Almond profit model
#' 
#' Computes profit for almond production based on yield anomalies
#' @param yield_anomaly The yield anomaly value (ton/acre)
#' @param baseline_yield The baseline almond yield (ton/acre)
#' @param almond_price Price of almonds ($/ton)
#' @param cost Production cost ($/acre)
#' @author Josephine Cardelle and Eva Newby
#' @return Dataframe with projected profit and year

almond_profit <- function(yield_anomaly, 
                          baseline_yield = 0.5, 
                          almond_price = 3400, 
                          cost = 2000) {
  
  # Calculate actual yield
  yield <- baseline_yield + yield_anomaly
  
  # Calculate revenue
  revenue <- yield * almond_price 
  
  # Calculate profit
  profit <- revenue - cost
  
  return(profit)
}

