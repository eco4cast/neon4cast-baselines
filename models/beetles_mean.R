
library(fable)
library(distributional)
library(tidyverse)


## Get the latest beetle target data.  
download.file("https://data.ecoforecast.org/neon4cast-targets/beetles/beetles-targets.csv.gz",
              "beetles-targets.csv.gz")
targets <-  read_csv("beetles-targets.csv.gz")

curr_iso_week <- ISOweek::ISOweek(Sys.Date())

curr_date <- ISOweek::ISOweek2date(paste0(curr_iso_week, "-1"))

site_list <- unique(targets$site_id)

last_day_richness <- tibble(site_id = site_list,
                   time = rep(curr_date, length(site_list)),
                   variable = "richness",
                   observed = NA)

last_day_abundance <- tibble(site_id = site_list,
                            time = rep(curr_date, length(site_list)),
                            variable = "abundance",
                            observed = NA)

targets_richness <- targets |> 
  filter(variable == "richness") |> 
  bind_rows(last_day_richness) |> 
  rename(richness = observed) |> 
  select(-variable) |> 
  as_tsibble(index = time, key = site_id)

targets_abundance <- targets |> 
  filter(variable == "abundance") |> 
  bind_rows(last_day_abundance) |> 
  rename(abundance = observed) |> 
  select(-variable) |> 
  as_tsibble(index = time, key = site_id)

## a single mean per site... obviously silly
fc_richness <- targets_richness  %>% 
  model(null = MEAN(log(richness + 1))) %>%
  generate(times = 500, h = "1 year", bootstrap = TRUE) |> 
  dplyr::rename(ensemble = .rep,
                predicted = .sim) |> 
  mutate(variable = "richness")

fc_abundance <- targets_abundance  %>%
  model(null = MEAN(log(abundance + 1))) %>%
  generate(times = 500, h = "1 year", bootstrap = TRUE) |> 
  dplyr::rename(ensemble = .rep,
                predicted = .sim) |> 
  mutate(variable = "abundance")

fc_richness |> 
  filter(site_id %in% site_list[1:10], variable == "richness") |> 
  ggplot(aes(x = time, y = predicted, group = ensemble)) +
  geom_line() +
  facet_wrap(~site_id)

targets |> 
  filter(site_id %in% site_list[1:10], variable == "richness") |> 
  ggplot(aes(x = time, y = observed)) +
  geom_point() +
  facet_wrap(~site_id)


forecast <- bind_rows(as_tibble(fc_richness), as_tibble(fc_abundance)) |> 
  mutate(start_time = lubridate::as_date(min(time)) - lubridate::weeks(1)) |> 
  select(time, start_time, site_id, ensemble, variable, predicted)

## Create the metadata record, see metadata.Rmd
theme_name <- "beetles"
time <- min(forecast$time)
team_name <- "mean"
filename <- paste0(theme_name, "-", time, "-", team_name, ".csv.gz")

## Store the forecast products
readr::write_csv(forecast, filename)

neon4cast::submit(forecast_file = filename, 
                  ask = FALSE)

unlink(filename)


