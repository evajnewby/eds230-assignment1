#' Combined almond yield and profit model
#' 
#' Computes almond yield anomalies and profit based on climate inputs
#' @param feb_min_temp Average minimum temperature in February (ºC)
#' @param jan_precip Total precipitation in January (mm)
#' @param baseline_yield The baseline almond yield (ton/acre)
#' @param almond_price_per_ton Price of almonds ($/ton)
#' @param cost_per_acre Base production cost ($/acre)
#' @param negative_anomaly_cost Cost adjustment factor for negative anomalies
#' @param ctemp Regression coefficient for average minimum temperature in February
#' @param ctemp2 Regression coefficient for average minimum temperature squared in February 
#' @param cprecip Regression coefficient for total precipitation in January 
#' @param cprecip2 Regression coefficient for total precipitation squared in January 
#' @param c Regression coefficient intercept
#' @return List containing yield anomalies and profit values
almond_yield_profit <-function(feb_min_temp, 
                                jan_precip,
                                baseline_yield = 0.5,
                                almond_price_per_ton = 5000,
                                cost_per_acre = 2000,
                                negative_anomaly_cost = 250,
                                ctemp = -0.015, 
                                ctemp2 = -0.0046, 
                                cprecip = -0.07, 
                                cprecip2 = 0.0043, 
                                c = 0.28) {
  
  # Get yield anomalies using the yield model
  anomalies <- almond_yield(feb_min_temp, jan_precip,
                            ctemp, ctemp2,
                            cprecip, cprecip2,
                            c)
  
  # Calculate profits for min, max, and mean anomalies
  min_profit <- almond_profit(anomalies$min_yield_anomaly, 
                              baseline_yield, 
                              almond_price_per_ton, 
                              cost_per_acre, 
                              negative_anomaly_cost)
  
  max_profit <- almond_profit(anomalies$max_yield_anomaly, 
                              baseline_yield, 
                              almond_price_per_ton, 
                              cost_per_acre, 
                              negative_anomaly_cost)
  
  mean_profit <- almond_profit(anomalies$mean_yield_anomaly, 
                               baseline_yield, 
                               almond_price_per_ton, 
                               cost_per_acre, 
                               negative_anomaly_cost)
  
  # Return combined results
  return(list(
    yield_anomalies = anomalies,
    profits = list(
      min_profit = min_profit,
      max_profit = max_profit,
      mean_profit = mean_profit
    )
  ))
}


