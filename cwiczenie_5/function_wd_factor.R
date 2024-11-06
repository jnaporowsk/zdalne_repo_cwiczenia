wd_factor <- function(dane) {
  wind_dr <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  wind_dr_deg <- seq(12.25, by=22.5, length.out = 16) 
  wind_dr_deg <- c(0, wind_dr_deg, 360)
  dane$wd_cardinal <- cut(dane$wd, breaks = wind_dr_deg, labels = wind_dr, included.lowest = TRUE)
  dane
}
