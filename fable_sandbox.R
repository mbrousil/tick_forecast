
library(targets)

library(tidyverse)
library(fable)
library(fable.prophet)
library(lubridate)

tar_load(fable_fits)
tar_load(reconciled_timeseries)
tar_load(forecast_start_date)

fable_mods <- fable_fits$models

ts_data_list <- reconciled_timeseries %>%
  split(f = .$ensemble) %>%
  map(.x = .,
      .f = ~ as_tsibble(x = .x,
                        key = site_id,
                        index = date) %>%
        filter(date >= forecast_start_date,
               date <= (forecast_start_date + days(90))))

forecast_day1 <- map_df(.x = ts_data_list,
                        .f = ~ forecast(fable_mods,
                                        new_data = .x) %>%
                          as_tibble())

write_csv(x = forecast_day1, file = "data/draft_forecast.csv")
