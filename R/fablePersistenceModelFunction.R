
# Function carry out a random walk forecast
RW_forecast <- function(site, var, h,
                        bootstrap = FALSE, boot_number = 200, 
                        transformation = 'none', verbose = TRUE,...) {
  # Work out when the forecast should start
  forecast_starts <- targets %>%
    dplyr::filter(!is.na(observed) & site_id == site & variable == var) %>%
    # Start the day after the most recent non-NA value
    dplyr::summarise(start_date = max(time) + 1) %>% # Date
    dplyr::mutate(h = (Sys.Date() - start_date) + h) %>% # Horizon value
    dplyr::ungroup()
  
  if (verbose == T) {
    message(
      site,
      ' ',
      var,
      ' forecast with transformation = ',
      transformation,
      ' and bootstrap = ',
      bootstrap
    )
  }
  
  # filter the targets data set to the site_var pair
  targets_use <- targets %>%
    dplyr::filter(site_id == site,
           variable == var) %>%
    tsibble::as_tsibble(key = c('variable', 'site_id'), index = 'time') %>%
    # add NA values up to today (index)
    tsibble::fill_gaps(.end = Sys.Date()) %>%
    # Remove the NA's put at the end, so that the forecast starts from the last day with an observation,
    # rather than today
    dplyr::filter(time < forecast_starts$start_date)
  
  if (transformation == 'log') {
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(log(observed)))
  }
  if (transformation == 'log1p') {
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(log1p(observed)))
  }
  if (transformation == 'none') {
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(observed))
  }
  
  if (bootstrap == T) {
    forecast <-
      RW_model %>% fabletools::generate(
        h = as.numeric(forecast_starts$h),
        bootstrap = T,
        times = boot_number
      )
  }  else
    forecast <-
    RW_model %>% fabletools::forecast(h = as.numeric(forecast_starts$h))
  return(forecast)
  
}
