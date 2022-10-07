library(tidyverse)
library(tsibble)
library(fable)

source('R/fablePersistenceModelFunction.R')
# 1.Read in the targets data
# We are not doing a peristence for le right now.
targets <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/terrestrial_daily/terrestrial_daily-targets.csv.gz", guess_max = 1e6) %>%
  filter(variable == 'nee')

# 2. Make the targets into a tsibble with explicit gaps
targets_ts <- targets %>%
  as_tsibble(key = c('variable', 'site_id'), index = 'datetime') %>%
  # add NA values up to today (index)
  fill_gaps(.end = Sys.Date())

# 3. Run through each via map
site_var_combinations <- expand.grid(site = unique(targets$site_id),
                                     var = unique(targets$variable)) %>%
  # assign the transformation depending on the variable. le is logged
  mutate(transformation = 'none') %>%
  mutate(boot_number = 200,
         h = 35,
         bootstrap = T, 
         verbose = T)

# Runs the RW forecast for each combination of variable and site_id
RW_forecasts <- purrr::pmap_dfr(site_var_combinations, RW_daily_forecast) 

# convert the output into EFI standard
RW_forecasts_EFI <- RW_forecasts %>%
  rename(parameter = .rep,
         prediction = .sim) %>%
  # For the EFI challenge we only want the forecast for future
  filter(datetime > Sys.Date()) %>%
  group_by(site_id, variable) %>%
  mutate(reference_datetime = min(datetime) - lubridate::days(1),
         family = "ensemble",
         model_id = "persistenceRW") %>%
  select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction) 

#RW_forecasts_EFI |> 
#  filter(site_id %in% unique(RW_forecasts_EFI$site_id)[1:24]) |> 
#  ggplot(aes(x = time, y = prediction, group = ensemble)) +
#  geom_line() +
#  facet_wrap(~site_id)

# 4. Write forecast file
file_date <- RW_forecasts_EFI$reference_datetime[1]

forecast_file <- paste("terrestrial_daily", file_date, "persistenceRW.csv.gz", sep = "-")

write_csv(RW_forecasts_EFI, forecast_file)

neon4cast::submit(forecast_file = forecast_file,
                  metadata = NULL,
                  ask = FALSE)

unlink(forecast_file)