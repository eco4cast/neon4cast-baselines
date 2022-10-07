library(tidyverse)
library(lubridate)

message(paste0("Running null model ", Sys.time()))

download_url <- paste0("https://data.ecoforecast.org/neon4cast-targets/",
                       "phenology", "/", "phenology-targets.csv.gz")

target <- read_csv(download_url)

target_clim <- target %>%  
  mutate(doy = yday(datetime)) %>% 
  group_by(doy, site_id, variable) %>% 
  summarise(mean = mean(observed, na.rm = TRUE),
            sd = sd(observed, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(mean = ifelse(is.nan(mean), NA, mean))

#curr_month <- month(Sys.Date())
curr_month <- month(Sys.Date())
if(curr_month < 10){
  curr_month <- paste0("0", curr_month)
}

curr_year <- year(Sys.Date())
start_date <- Sys.Date() + days(1)

forecast_dates <- seq(start_date, as_date(start_date + days(34)), "1 day")
forecast_doy <- yday(forecast_dates)

forecast <- target_clim %>%
  mutate(doy = as.integer(doy)) %>% 
  filter(doy %in% forecast_doy) %>% 
  mutate(datetime = as_date(ifelse(doy > last(doy),
                               as_date((doy-1), origin = paste(year(Sys.Date())+1, "01", "01", sep = "-")),
                               as_date((doy-1), origin = paste(year(Sys.Date()), "01", "01", sep = "-")))))

subseted_site_names <- unique(forecast$site_id)
site_vector <- NULL
for(i in 1:length(subseted_site_names)){
  site_vector <- c(site_vector, rep(subseted_site_names[i], length(forecast_dates)))
}

forecast_tibble1 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                          site_id = site_vector,
                          variable = "gcc_90")

forecast_tibble2 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                          site_id = site_vector,
                          variable = "rcc_90")

forecast_tibble <- bind_rows(forecast_tibble1, forecast_tibble2)

forecast <- left_join(forecast_tibble, forecast)



combined <- forecast %>% 
  select(datetime, site_id, mean, sd, variable) %>% 
  group_by(site_id, variable) %>% 
  mutate(mu = imputeTS::na_interpolation(mean),
         sigma = median(sd, na.rm = TRUE)) %>%
  pivot_longer(c("mu", "sigma"),names_to = "parameter", values_to = "prediction") |> 
  arrange(site_id, datetime) |> 
  mutate(family = "normal") |> 
  mutate(reference_datetime = lubridate::as_date(min(datetime)) - lubridate::days(1),
         model_id = "climatology") |> 
  select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction) |> 
  ungroup()

combined %>% 
  filter(variable == "gcc_90") |> 
  select(datetime, site_id,parameter, prediction) %>% 
  pivot_wider(names_from = parameter, values_from = prediction) %>% 
  ggplot(aes(x = datetime)) +
  geom_ribbon(aes(ymin=mu - sigma*1.96, ymax=mu + sigma*1.96), alpha = 0.1) + 
  geom_point(aes(y = mu)) +
  facet_wrap(~site_id)

file_date <- combined$reference_datetime[1]

forecast_file <- paste("phenology", file_date, "climatology.csv.gz", sep = "-")

write_csv(combined, file = forecast_file)


neon4cast::submit(forecast_file = forecast_file, 
                  metadata = NULL, 
                  ask = FALSE)

unlink(forecast_file)
#unlink(meta_data_filename)

message(paste0("Completed null model generation ", Sys.time()))



