#' Almond yield output
#'
#' Computes min, max, and mean almond yield anomaly given February mean minimum temperature and January precipitation using the Lobell et al.(2006) almond statistical yield model
#' @param feb_min_temp average minimum temperature in February (ºC)
#' @param jan_precip total precipitation in January (mm)
#' @param ctemp Regression coefficient for average minimum temperature in February
#' @param ctemp2 Regression coefficient for average minimum temperature squared in February 
#' @param cprecip Regression coefficient for total precipitation in January 
#' @param cprecip2 Regression coefficient for total precipitation squared in January 
#' @param c Regression coefficient intercept
#' @author Josephine Cardelle and Eva Newby
#' @return minimum almond yield anomaly, maximum almond yield anomaly and mean almond yield (ton/acre)

almond_yield <- function(feb_min_temp, jan_precip, ctemp = -0.015, ctemp2 = -0.0046, 
                         cprecip = -0.07, cprecip2 = 0.0043, 
                         c = 0.28) {
  yield_anomalies <- ctemp * feb_min_temp + 
    ctemp2 * feb_min_temp^2 + 
    cprecip * jan_precip + 
    cprecip2 * jan_precip^2 + 
    c
  
  # Return results
  return(list(
    min_yield_anomaly = min(yield_anomalies, na.rm = TRUE),
    max_yield_anomaly = max(yield_anomalies, na.rm = TRUE),
    mean_yield_anomaly = mean(yield_anomalies, na.rm = TRUE)
  ))
}
