#' Almond yield output
#'
#' Computes min, max, and mean almond yield anomaly given February mean minimum temperature and January precipitation using the Lobell et al.(2006) almond statistical yield model
#' @param feb_min_temp minimum temperature in February (ºC)
#' @param jan_precip total precipitation in January (mm)
#' @author Josephine Cardelle and Eva Newby
#' @return minimum almond yield anomaly, maximum almond yield anomaly and mean almond yield (ton/acre)

almond_yield <- function(feb_min_temp, jan_precip) {
  yield_anomalies <- -0.015 * feb_min_temp - 
    0.0046 * feb_min_temp^2 - 
    0.07 * jan_precip + 
    0.0043 * jan_precip^2 + 
    0.28
  
  # Return results
  return(list(
    min_yield_anomaly = min(yield_anomalies, na.rm = TRUE),
    max_yield_anomaly = max(yield_anomalies, na.rm = TRUE),
    mean_yield_anomaly = mean(yield_anomalies, na.rm = TRUE)
  ))
}
