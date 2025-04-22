#' Combined almond yield and profit model
#' 
#' Computes profit for almond production based on climate data
#' @param feb_min_temp average minimum temperature in February (ºC)
#' @param jan_precip total precipitation in January (mm)
#' @param baseline_yield The baseline almond yield (ton/acre)
#' @param almond_price Price of almonds ($/ton)
#' @param cost Production cost ($/acre)
#' @param ctemp Regression coefficient for average minimum temperature in February
#' @param ctemp2 Regression coefficient for average minimum temperature squared in February 
#' @param cprecip Regression coefficient for total precipitation in January 
#' @param cprecip2 Regression coefficient for total precipitation squared in January 
#' @param c Regression coefficient intercept
#' @author Josephine Cardelle and Eva Newby
#' @return List containing profit values (min, max, mean) based on yield anomalies
almond_yield_profit <- function(feb_min_temp, jan_precip,
                                       baseline_yield = 0.5,
                                       almond_price = 3400,
                                       cost = 2000,
                                       ctemp = -0.015, ctemp2 = -0.0046,
                                       cprecip = -0.07, cprecip2 = 0.0043,
                                       c = 0.28) {
  
  # Call the almond_yield function to get yield anomalies
  yield_results <- almond_yield(feb_min_temp, jan_precip, 
                                ctemp, ctemp2, cprecip, cprecip2, c)
  
  # Calculate profits for min, max, and mean yield anomalies
  min_profit <- almond_profit(yield_results$min_yield_anomaly, 
                              baseline_yield, almond_price, cost)
  max_profit <- almond_profit(yield_results$max_yield_anomaly, 
                              baseline_yield, almond_price, cost)
  mean_profit <- almond_profit(yield_results$mean_yield_anomaly, 
                               baseline_yield, almond_price, cost)
  
  # Return results
  return(list(
    min_profit = min_profit,
    max_profit = max_profit,
    mean_profit = mean_profit
  ))
}
