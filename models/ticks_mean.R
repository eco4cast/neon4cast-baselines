
library(fable)
library(distributional)
library(tidyverse)

efi_statistic_format <- function(df){
  ## determine variable name
  var <- attributes(df)$dist
  ## Normal distribution: use distribution mean and variance
  df %>% 
    dplyr::mutate(sigma = sqrt( distributional::variance( .data[[var]] ) ) ) %>%
    dplyr::rename(mu = .mean) %>%
    dplyr::select(time, site_id, .model, mu, sigma) %>%
    tidyr::pivot_longer(c(mu, sigma), names_to = "parameter", values_to = var) %>%
    pivot_longer(tidyselect::all_of(var), names_to="variable", values_to = "predicted") |> 
    mutate(family = "normal")
}

## Get the latest beetle target data.  
download.file("https://data.ecoforecast.org/neon4cast-targets/ticks/ticks-targets.csv.gz",
              "ticks-targets.csv.gz")
targets <-  read_csv("ticks-targets.csv.gz")

curr_iso_week <- ISOweek::ISOweek(Sys.Date())

curr_date <- ISOweek::ISOweek2date(paste0(curr_iso_week, "-1"))

site_list <- unique(targets$site_id)

last_day <- tibble(site_id = site_list,
                   time = rep(curr_date, length(site_list)),
                   variable = "amblyomma_americanum",
                   observed = NA)



targets <- targets |> 
  filter(variable == "amblyomma_americanum") |> 
  bind_rows(last_day) |> 
  select(-variable) |> 
  as_tsibble(index = time, key = site_id)

## a single mean per site... obviously silly
forecast <- targets  %>% 
  model(null = MEAN(log(observed + 1))) %>%
  generate(times = 500, h = "1 year", bootstrap = TRUE) |> 
  dplyr::rename(ensemble = .rep,
                predicted = .sim) |> 
  mutate(variable = "amblyomma_americanum") |> 
  mutate(start_time = lubridate::as_date(min(time)) - lubridate::weeks(1)) |> 
  select(time, start_time, site_id, ensemble, variable, predicted)

## Create the metadata record, see metadata.Rmd
theme_name <- "ticks"
time <- min(forecast$time)
team_name <- "mean"
filename <- paste0(theme_name, "-", time, "-", team_name, ".csv.gz")

## Store the forecast products
readr::write_csv(forecast, filename)

neon4cast::submit(forecast_file = filename, 
                  ask = FALSE)


