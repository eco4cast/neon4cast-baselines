library(tidyverse)
library(tsibble)
library(fable)
source('./R/fablePersistenceModelFunction.R')

# 1.Read in the targets data
targets <- read_csv('https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz') %>%
  mutate(observed = ifelse(observed == 0, 0.00001, observed))

# 2. Make the targets into a tsibble with explicit gaps
targets_ts <- targets %>%
  as_tsibble(key = c('variable', 'site_id'), index = 'time') %>%
  # add NA values up to today (index)
  fill_gaps(.end = Sys.Date())

# 2. Run through each via map
# Requires a dataframe that has each of the variable in the RW_forecast function
site_var_combinations <- expand.grid(site = unique(targets$site_id),
                                     var = unique(targets$variable)) %>%
  # assign the transformation depending on the variable. chla and oxygen get a log(x) transformation
  mutate(transformation = ifelse(var %in% c('chla', 'oxygen'), 'log', 'none')) %>%
  mutate(boot_number = 200,
         h = 35,
         bootstrap = T, 
         verbose = T)

# runs the RW forecast for each combination of variable and site_id
RW_forecasts <- purrr::pmap_dfr(site_var_combinations, RW_daily_forecast) 

# convert the output into EFI standard
RW_forecasts_EFI <- RW_forecasts %>%
  rename(ensemble = .rep,
         predicted = .sim) %>%
  # For the EFI challenge we only want the forecast for future
  filter(time > Sys.Date()) %>%
  group_by(site_id, variable) %>%
  mutate(start_time = min(time) - lubridate::days(1)) %>%
  select(time, start_time, site_id, ensemble, variable, predicted) 

# 4. Write forecast file
forecast_file <- paste("aquatics", min(RW_forecasts_EFI$time), "persistenceRW.csv.gz", sep = "-")

write_csv(RW_forecasts_EFI, forecast_file)

neon4cast::submit(forecast_file = forecast_file,
                  metadata = NULL,
                  ask = FALSE)

unlink(forecast_file)
