
library(targets)

library(tidyverse)
library(lubridate)

tar_load(short_term_fable_forecast)

tar_load(forecast_start_date)


clean_and_format_forecasts <- function(short_term_fable_forecast,
                                       forecast_start_date){
  
  full_fable <- short_term_fable_forecast$full_fable_forecast
  
  avg_list <- short_term_fable_forecast$averaged_forecast
  
  fable_forecast_formatted <- full_fable %>%
    # Create extra cols needed
    mutate(family = "ensemble",
           variable = "amblyomma_americanum",
           forecast_start_date = forecast_start_date) %>%
    # Arrange and name as needed
    select(datetime = date, reference_datetime = forecast_start_date, site_id,
           family, parameter = ensemble, variable, predicted = .mean,
           model_id = .model) %>%
    # Replace negative values with 0
    mutate(predicted = if_else(condition = predicted < 0,
                               true = 0,
                               false = predicted))
  
  # Export forecasts
  walk(.x = unique(fable_forecast_formatted$model_id),
       .f = ~ {
         
         # Filename format:
         # theme_name-year-month-day-model_id.csv
         filename <- paste0("data/forecasts/ticks-",
                            forecast_start_date,
                            "-", .x,
                            ".csv")
         
         write_csv(x = fable_forecast_formatted %>%
                     filter(model_id == .x),
                   file = filename)
         
       })
  
  
  avg_forecast_formatted <- map_df(.x = avg_list,
                                   .f = ~ .x) %>%
    mutate(family = "ensemble",
           variable = "amblyomma_americanum",
           model_id = "top_3_weighted_average",
           forecast_start_date = forecast_start_date) %>%
    select(datetime = date, reference_datetime = forecast_start_date, site_id,
           family, parameter = ensemble, variable, predicted = top_3_w_mean_pred,
           model_id) %>%
    mutate(predicted = if_else(condition = predicted < 0,
                               true = 0,
                               false = predicted))
  
  top_3_filename <- paste0("data/forecasts/ticks-",
                           forecast_start_date,
                           "-top_3_weighted_average",
                           ".csv")
  
  write_csv(x = avg_forecast_formatted,
            file = top_3_filename)
  
}