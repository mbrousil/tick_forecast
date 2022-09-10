
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Default packages to load for each target
  packages = c("tidyverse", "lubridate"),
  # Output save format
  format = "rds")

# Load the R script with custom functions:
source(file = "R/functions.R")


# The list of targets (steps in the workflow) to execute, and what to do for
# each one
list(
  
  # 1. Data acquisition -----------------------------------------------------
  
  # Sites of interest
  tar_target(target_sites,
             c("BLAN", "KONZ", "LENO", "ORNL", "OSBS", "SCBI",
               "SERC", "TALL", "UKFS")),
  
  # Where the (manually downloaded) gridMET files are stored
  tar_files(gridmet_files,
            "data/gridmet/"),
  
  
  # 1a. NEON data -----------------------------------------------------------
  
  # EFI-processed tick data:
  tar_target(tick_counts,
             readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/ticks/ticks-targets.csv.gz",
                             guess_max = 1e6) %>%
               mutate(iso_week_num = isoweek(time)) %>%
               pivot_wider(names_from = "variable", values_from = "observed")),
  
  # Note, these steps take an hour or more:
  
  
  # NEON temperatures, raw
  tar_target(downloaded_neon_temperatures,
             download_neon_saat(target_sites = target_sites),
             packages = c("neonstore")),
  
  # Cleaned NEON temps
  tar_target(cleaned_neon_temps,
             clean_neon_temps(neon_path = downloaded_neon_temperatures)),
  
  # Daily aggregation of NEON temps
  tar_target(daily_neon_temps,
             aggregate_neon_temps(cleaned_neon_temps = cleaned_neon_temps)),
  
  
  # NEON RH, raw
  tar_target(downloaded_neon_rh,
             download_neon_rh(target_sites = target_sites),
             packages = c("neonstore")),
  
  # Cleaned NEON RH
  tar_target(cleaned_neon_rh,
             clean_neon_rh(neon_path = downloaded_neon_rh)),
  
  # Daily aggregation of NEON RH
  tar_target(daily_neon_rh,
             aggregate_neon_rh(cleaned_neon_rh = cleaned_neon_rh)),
  
  
  # NEON precip, raw
  tar_target(downloaded_neon_precip,
             download_neon_precip(target_sites = target_sites),
             packages = c("neonstore")),
  
  # Cleaned NEON precip
  tar_target(cleaned_neon_precip,
             clean_neon_precip(neon_path = downloaded_neon_precip)),
  
  # Daily aggregation of NEON precip
  tar_target(daily_neon_precip,
             aggregate_neon_precip(cleaned_neon_precip = cleaned_neon_precip)),
  
  
  # Calculated NEON VPD
  tar_target(neon_vpd,
             derive_neon_vpd(neon_path = downloaded_neon_rh),
             packages = c("tidyverse", "lubridate", "plantecophys")),
  
  # Daily aggregation of NEON-derived VPD
  tar_target(daily_neon_vpd,
             aggregate_neon_vpd(neon_vpd = neon_vpd)),
  
  # A review of the NEON weather data shows that there's a fair amount of gaps.
  # Because of the gaps we'll want some external weather data (gridMET), which
  # we'll pull below
  
  # The sites to use:
  tar_target(site_metadata,
             read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ticks/master/Ticks_NEON_Field_Site_Metadata_20210928.csv")),
  
  # NEON sites with location data and names, isolated from main metadata
  tar_target(neon_sites,
             site_metadata %>%
               select(field_domain_id, field_site_id, field_site_name, field_site_state,
                      field_latitude, field_longitude)),
  
  # NEON sites in a spatial format
  tar_target(neon_sites_sf,
             neon_sites %>%
               select(field_domain_id, field_site_id, field_site_name, field_site_state,
                      field_latitude, field_longitude) %>%
               st_as_sf(x = .,
                        coords = c("field_longitude", "field_latitude"),
                        # WGS84
                        crs = 4326),
             packages = c("tidyverse", "lubridate", "sf")),
  
  
  # 1b. gridMET data --------------------------------------------------------
  
  # Iterate over gridMET files and compile daily estimates for each site's
  # location into a data frame
  
  # RH max
  tar_target(gridmet_rh_max,
             map_df(.x = list.files(path = gridmet_files,
                                    pattern = "rmax_.{4}\\.nc",
                                    full.names = TRUE),
                    .f = ~ extract_from_brick(brick_file = .x,
                                              sites_sf = neon_sites_sf,
                                              origin_date = "1900-01-01",
                                              variable = "rh_max")),
             packages = c("tidyverse", "lubridate", "raster", "assertthat")),
  
  # RH min
  tar_target(gridmet_rh_min,
             map_df(.x = list.files(path = gridmet_files,
                                    pattern = "rmin_.{4}\\.nc",
                                    full.names = TRUE),
                    .f = ~ extract_from_brick(brick_file = .x,
                                              sites_sf = neon_sites_sf,
                                              origin_date = "1900-01-01",
                                              variable = "rh_min")),
             packages = c("tidyverse", "lubridate", "raster", "assertthat")),
  
  # Mean VPD
  tar_target(gridmet_vpd,
             map_df(.x = list.files(path = gridmet_files,
                                    pattern = "vpd_.{4}\\.nc",
                                    full.names = TRUE),
                    .f = ~ extract_from_brick(brick_file = .x,
                                              sites_sf = neon_sites_sf,
                                              origin_date = "1900-01-01",
                                              variable = "vpd")),
             packages = c("tidyverse", "lubridate", "raster", "assertthat")),
  
  # Combine all gridMET RH & VPD variables and then export
  tar_target(gridmet_rh_vpd,
             {
               full_gridmet <- reduce(.x = list(gridmet_rh_min, gridmet_rh_max, gridmet_vpd),
                                      .f = full_join,
                                      by = c("field_site_id", "date"))
               
               return(full_gridmet)
             }),
  
  # Temp max
  tar_target(gridmet_temp_max,
             map_df(.x = list.files(path = gridmet_files,
                                    pattern = "tmmx_.{4}\\.nc",
                                    full.names = TRUE),
                    .f = ~ extract_from_brick(brick_file = .x,
                                              sites_sf = neon_sites_sf,
                                              origin_date = "1900-01-01",
                                              variable = "max_temp_k")) %>%
               mutate(max_temp_c = conv_unit(x = max_temp_k, from = "K", to = "C")) %>%
               dplyr::select(-max_temp_k),
             packages = c("tidyverse", "lubridate", "measurements", "raster", "assertthat")),
  
  # Temp min
  tar_target(gridmet_temp_min,
             map_df(.x = list.files(path = gridmet_files,
                                    pattern = "tmmn_.{4}\\.nc",
                                    full.names = TRUE),
                    .f = ~ extract_from_brick(brick_file = .x,
                                              sites_sf = neon_sites_sf,
                                              origin_date = "1900-01-01",
                                              variable = "min_temp_k")) %>%
               mutate(min_temp_c = conv_unit(x = min_temp_k, from = "K", to = "C")) %>%
               dplyr::select(-min_temp_k),
             packages = c("tidyverse", "lubridate", "measurements", "raster", "assertthat")),
  
  # Combine gridMET temp variables and then export
  tar_target(gridmet_temps,
             {
               full_gridmet_temp <- reduce(.x = list(gridmet_temp_min, gridmet_temp_max),
                                           .f = full_join,
                                           by = c("field_site_id", "date")) %>%
                 ungroup()
               
               return(full_gridmet_temp)
             }),
  
  # Precipitation
  tar_target(gridmet_precip,
             {
               gridmet_precip <- map_df(.x = list.files(path = gridmet_files,
                                                        pattern = "pr_.{4}\\.nc",
                                                        full.names = TRUE),
                                        .f = ~ extract_from_brick(brick_file = .x,
                                                                  sites_sf = neon_sites_sf,
                                                                  origin_date = "1900-01-01",
                                                                  variable = "pr"))
               
               return(gridmet_precip)
               
             },
             packages = c("tidyverse", "lubridate", "raster", "assertthat")),
  
  
  # 2. Data cleaning and management -----------------------------------------
  
  # A data frame containing all potential dates of interest for the purpose of,
  # e.g., degree day calculation:
  tar_target(potential_dates,
             {
               # Go back to the start of the previous calendar year in case we
               # want to have degree days from the previous year (e.g., in the
               # event that we used lagged variables similar to some studies)
               date_summary <- tick_counts %>%
                 group_by(site_id) %>%
                 summarize(min_date = min(time),
                           max_date = max(time)) %>%
                 mutate(min_year = year(min_date),
                        max_year = year(max_date),
                        start_date = ymd(paste0(min_year, "-01-01")) - years(1),
                        end_date = ymd(paste0(max_year, "-12-31")))  
               
               potential_dates <- date_summary %>%
                 distinct(site_id, start_date, end_date) %>%
                 split(sort(as.numeric(rownames(.)))) %>%
                 map_df(.x = .,
                        .f = ~ {
                          
                          # A vector of all the unique dates in the timeseries
                          dates <- seq(from = .x$start_date,
                                       to = .x$end_date,
                                       by = 1)
                          
                          # Output df with site and year included
                          tibble(
                            site_id = .x$site_id,
                            date = dates) %>%
                            mutate(year = year(date))
                        })
               
               return(potential_dates)
               
             }),
  
  # Daily-level dataset of combined min/max temps from NEON and gridMET
  tar_target(combined_temperature_data,
             combine_neon_gridmet_temps(daily_neon_temps = daily_neon_temps,
                                        gridmet_temps = gridmet_temps)),
  
  # Daily-level dataset of combined min/max RH from NEON and gridMET
  tar_target(combined_rh_data,
             combine_neon_gridmet_rh(daily_neon_rh = daily_neon_rh,
                                     gridmet_rh_min = gridmet_rh_min,
                                     gridmet_rh_max = gridmet_rh_max)),
  
  # Daily-level dataset of combined VPD data from NEON and gridMET
  tar_target(combined_vpd_data,
             combine_neon_gridmet_vpd(daily_neon_vpd = daily_neon_vpd,
                                      gridmet_vpd = gridmet_vpd)),
  
  # Daily-level dataset of combined precip data from NEON and gridMET
  tar_target(combined_precip_data,
             combine_neon_gridmet_precip(daily_neon_precip = daily_neon_precip,
                                         gridmet_precip = gridmet_precip)),
  
  # Daily-level dataset of all gap-filled weather (combining those in previous
  # steps)
  tar_target(daily_weather,
             combine_daily_weather(combined_temperature_data = combined_temperature_data,
                                   combined_rh_data = combined_rh_data,
                                   combined_vpd_data = combined_vpd_data,
                                   combined_precip_data = combined_precip_data)),
  
  # Daily data with linear interpolation to fill gaps
  tar_target(interpolated_daily_weather,
             interpolate_daily_weather(daily_weather = daily_weather,
                                       potential_dates = potential_dates),
             packages = c("tidyverse", "lubridate", "tsibble", "imputeTS")),
  
  # Calculate and aggregate degree day info
  tar_target(degree_day_calculations,
             calculate_degree_days(
               interpolated_daily_weather = interpolated_daily_weather,
               # The base temperature (C) to use for DD calculation:
               base_temp = 0),
             packages = c("tidyverse", "lubridate", "zoo")),
  
  # Calculate and aggregate "chill day" info
  tar_target(chill_day_calculations,
             calculate_chill_days(interpolated_daily_weather = interpolated_daily_weather)),
  
  # Add some simple lagged columns to the daily dataset
  tar_target(daily_weather_lagged,
             add_daily_lags(interpolated_daily_weather = interpolated_daily_weather)),
  
  # Add degree day data and create weekly-level dataset
  tar_target(weekly_weather_summary,
             add_dd_and_aggregate(daily_weather_lagged = daily_weather_lagged,
                                  degree_day_calculations = degree_day_calculations,
                                  chill_day_calculations = chill_day_calculations)),
  
  # Ticks joined with the historical weather dataset. Note that this doesn't
  # include any interpolation of tick counts or forecasts of weather
  tar_target(ticks_w_weather,
             join_ticks_with_weather(weekly_weather_summary = weekly_weather_summary,
                                     tick_counts = tick_counts,
                                     degree_day_calculations = degree_day_calculations),
             packages = c("tidyverse", "lubridate", "ISOweek")),
  
  # Carry out linear interpolation of missing tick and some remaining predictor
  # data, and return a tsibble and csv
  tar_target(interpolated_tick_data,
             interpolate_dataset(ticks_w_weather = ticks_w_weather,
                                 tick_counts = tick_counts),
             packages = c("tidyverse", "lubridate", "tsibble", "imputeTS", "zoo")),
  
  # Create training and test datasets (i.e., partial timeseries) for use in 
  # model fitting
  tar_target(training_and_test_sets,
             {
               
               training_set <- interpolated_tick_data$dataset %>%
                 filter(date >= "2014-01-01",
                        date < "2019-03-04")
               
               train_out_path <- "data/tick_training_set.csv"
               
               # Export as csv
               as_tibble(training_set) %>%
                 write_csv(file = train_out_path)
               
               test_set <- interpolated_tick_data$dataset %>%
                 filter(date >= "2019-03-04",
                        date <= "2020-11-04")
               
               test_out_path <- "data/tick_test_set.csv"
               
               as_tibble(test_set) %>%
                 write_csv(file = test_out_path)
               
               return(
                 list(
                   training_tsibble = training_set,
                   training_file = train_out_path,
                   test_tsibble = test_set,
                   test_file = test_out_path
                 ))
             }),
  
  # What date to begin pulling weather forecasts
  tar_target(forecast_start_date,
             {
               
               forecast_week <- Sys.Date() %>%
                 isoweek()
               
               forecast_year <- Sys.Date() %>%
                 year(.) - 1
               
               forecast_start_date <- ISOweek2date(paste0(forecast_year, "-W", forecast_week, "-1"))
               
               return(forecast_start_date)
               
             },
             packages = c("tidyverse", "ISOweek", "lubridate")),
  
  # Get the forecasted weather dataset
  tar_target(noaa_forecast,
             download_noaa_forecast(forecast_start_date = as.character(forecast_start_date),
                                    tick_counts = tick_counts),
             packages = c("tidyverse", "neon4cast", "neon4cast")),
  
  # Aggregate NOAA forecast by day and ensemble member
  tar_target(daily_noaa_forecast,
             aggregate_noaa(start_date = forecast_start_date,
                            end_date = forecast_start_date + days(35),
                            target_sites = target_sites,
                            noaa_forecast = noaa_forecast),
             packages = c("tidyverse", "plantecophys", "measurements",
                          "lubridate")),
  
  # Calculate the basic lags needed to get us to the full dataset
  tar_target(daily_forecast_lagged,
             add_daily_forecast_lags(daily_noaa_forecast = daily_noaa_forecast,
                                     interpolated_daily_weather = interpolated_daily_weather,
                                     forecast_start_date = forecast_start_date)),
  
  # Calculate and aggregate degree day info for forecasted time period
  tar_target(degree_day_forecast,
             forecast_degree_days(
               combined_daily = daily_forecast_lagged$combined_daily,
               # The base temperature (C) to use for DD calculation:
               base_temp = 0,
               forecast_start_date = forecast_start_date),
             packages = c("tidyverse", "lubridate", "zoo")),
  
  # Add degree day data and create weekly-level dataset
  tar_target(weekly_forecast_summary,
             aggregate_forecast(daily_forecast_lagged = daily_forecast_lagged$daily_forecast_lagged,
                                degree_day_forecast = degree_day_forecast,
                                forecast_start_date = forecast_start_date),
             packages = c("tidyverse", "lubridate", "ISOweek")),
  
  # Add more complex lags to the dataset
  tar_target(weekly_forecast_lagged,
             lag_weekly_forecast(weekly_forecast_summary = weekly_forecast_summary, 
                                 forecast_start_date = forecast_start_date, 
                                 interpolated_tick_data = interpolated_tick_data$dataset),
             packages = c("tidyverse", "lubridate", "zoo", "imputeTS")),
  
  # Reconcile forecasted dataset structure with observed dataset structure
  tar_target(reconciled_timeseries,
             reconcile_timeseries(weekly_forecast_lagged = weekly_forecast_lagged,
                                  forecast_start_date = forecast_start_date,
                                  interpolated_tick_data = interpolated_tick_data$dataset,
                                  degree_day_forecast = degree_day_forecast),
             packages = c("tidyverse", "lubridate", "ISOweek", "zoo", "imputeTS")),
  
  
  # 3. Modeling -------------------------------------------------------------
  
  # Model specification with exogenous predictors
  tar_target(model_formula,
             model_formula <- paste0("amam_filled ~ ",
                                     # Exogenous vars
                                     paste("min_temp", "mean_vpd", "sum_precip_mm",
                                           "cume_dd_prev_week", "amam_4wk_rollmean_lag1",
                                           "amam_4wk_rollmean_lag50", "mean_vpd_4wk_rollmean_lag1",
                                           "mean_vpd_4wk_rollmean_lag50", sep = " + ")) %>%
               as.formula()),
  
  tar_target(fable_fits,
             fit_fable(training_tsibble = training_and_test_sets$training_tsibble,
                       test_tsibble = training_and_test_sets$test_tsibble,
                       model_formula = model_formula),
             packages = c("tidyverse", "tsibble", "fable", "fable.prophet")),
  
  tar_target(lightgbm_fits,
             fit_lightgbm(training_tsibble = training_and_test_sets$training_tsibble,
                          test_tsibble = training_and_test_sets$test_tsibble,
                          model_formula = model_formula),
             packages = c("tidyverse", "tsibble", "parsnip", "bonsai", "Metrics",
                          "lightgbm")),
  
  tar_target(model_averaging_test,
             average_models(test_tsibble = training_and_test_sets$test_tsibble,
                            fable_models = fable_fits$models,
                            fable_accuracy = fable_fits$model_accuracy,
                            fable_preds = fable_fits$model_predictions,
                            std_lgb_models = lightgbm_fits$std_models,
                            std_lgb_accuracy = lightgbm_fits$std_model_accuracy,
                            full_lgb_models = lightgbm_fits$full_models,
                            full_lgb_accuracy = lightgbm_fits$full_model_accuracy,
                            std_lgb_preds = lightgbm_fits$standard_lgb_predictions,
                            full_lgb_preds = lightgbm_fits$full_lgb_predictions),
             packages = c("tidyverse", "tsibble", "fable", "parsnip", "bonsai",
                          "Metrics", "lightgbm")),
  
  # Plot the test set observed & predicted values by model
  tar_target(combined_test_set_forecasts,
             plot_test_forecasts(full_predictions = model_averaging_test$full_predictions)),
  
  # tar_target(production_models),
  
  # 4. Forecasting ----------------------------------------------------------
  
  
  
  # 5. Diagnostic plots for the workflow ------------------------------------
  
  # A plot showing each cell's data type and whether it's NA or not
  tar_target(tick_weather_missing_data_plot,
             {
               check_for_ticks <- vis_dat(as_tibble(interpolated_tick_data$dataset)) +
                 theme(plot.background = element_rect(fill = "white"))
               
               out_path <- "figures/workflow_diagnostics/missing_ticks_and_weather_plot.png"
               
               ggsave(filename = out_path, plot = check_for_ticks,
                      width = 10, height = 6, units = "in", device = "png")
               
               return(list(
                 missing_plot = check_for_ticks,
                 out_path = out_path))
               
             }
             ,
             packages = c("tidyverse", "visdat")),
  
  # Plots showing the timeseries of tick counts at each site after interpolation
  tar_target(interpolated_tick_timeseries_plots,
             plot_tick_timeseries_plots(interpolated_tick_data = interpolated_tick_data$dataset)),
  
  # A plot showing how the "chill days" vary over time at each site
  tar_target(chill_day_timeseries_plot,
             {
               chill_timeseries <- ggplot(data = interpolated_tick_data$dataset) +
                 geom_point(aes(x = date, y = cume_cd_prev_winter)) +
                 facet_wrap(vars(site_id)) +
                 ylab("Previous winter's chill day accumulation") +
                 xlab("Date") +
                 theme_bw()
               
               out_path <- "figures/workflow_diagnostics/chill_day_timeseries.png"
               
               ggsave(filename = out_path, plot = chill_timeseries,
                      width = 7.5, height = 5, units = "in", device = "png")
               
               return(list(
                 chill_plot = chill_timeseries,
                 out_path = out_path))
               
             }),
  
  # A plot comparing daily precipitation values by the data source they came from
  tar_target(precipitation_source_comparison_plot,
             {
               
               precip_source_timeseries <- daily_weather_lagged %>%
                 ggplot() +
                 geom_point(aes(x = date, y = sum_precip_mm, fill = precip_source),
                            pch = 21, color = "black", alpha = 0.8) +
                 xlab("Date") +
                 ylab("Total precipitation (mm)") +
                 facet_wrap(vars(site_id), scales = "free") +
                 scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
                 theme_bw() +
                 theme(legend.position = "bottom")
               
               out_path <- "figures/workflow_diagnostics/precip_source_timeseries.png"
               
               ggsave(filename = out_path, plot = precip_source_timeseries,
                      width = 15, height = 8, units = "in", dev = "png")
               
               return(list(
                 precip_plot = precip_source_timeseries,
                 out_path = out_path
               ))
               
             }),
  
  # Plots comparing daily temperature values by the data source they came from
  tar_target(temperature_source_comparison_plots,
             plot_temperatures_by_source(daily_weather_lagged = daily_weather_lagged)),
  
  # Plots comparing daily RH values by the data source they came from
  tar_target(rh_source_comparison_plots,
             plot_rh_by_source(daily_weather_lagged = daily_weather_lagged)),
  
  # Plots comparing daily RH values by the data source they came from
  tar_target(vpd_source_comparison_plot,
             plot_vpd_by_source(daily_weather_lagged = daily_weather_lagged)),
  
  # One-to-one plots comparing daily data for each variable
  tar_target(one_to_one_plots,
             make_one_to_one_plots(daily_neon_temps = daily_neon_temps,
                                   daily_neon_rh = daily_neon_rh,
                                   daily_neon_precip = daily_neon_precip,
                                   daily_neon_vpd = daily_neon_vpd,
                                   
                                   gridmet_temps = gridmet_temps,
                                   gridmet_vpd = gridmet_vpd,
                                   gridmet_rh_min = gridmet_rh_min,
                                   gridmet_rh_max = gridmet_rh_max,
                                   gridmet_precip = gridmet_precip)),
  
  # Plot NOAA weather forecasts to check for gaps
  # tar_target(noaa_temp_plot,
  #            {
  #              out_path <- "figures/noaa_temp_completion.png"
  #              
  #              temp_plot <- noaa_forecast %>%
  #                ggplot() +
  #                geom_point(aes(x = time, y = air_temperature)) +
  #                facet_wrap(vars(site_id)) +
  #                theme_bw()
  #              
  #              ggsave(file = out_path, plot = temp_plot, height = 6, width = 9,
  #                     units = "in", dev = "png")
  #              
  #              return(out_path)
  #            }),
  # 
  # tar_target(noaa_pressure_plot,
  #            {
  #              out_path <- "figures/noaa_pressure_completion.png"
  #              
  #              press_plot <- noaa_forecast %>%
  #                ggplot() +
  #                geom_point(aes(x = time, y = air_pressure)) +
  #                facet_wrap(vars(site_id)) +
  #                theme_bw()
  #              
  #              ggsave(file = out_path, plot = press_plot, height = 6, width = 9,
  #                     units = "in", dev = "png")
  #              
  #              return(out_path)
  #            }),
  # 
  # tar_target(noaa_rh_plot,
  #            {
  #              out_path <- "figures/noaa_rh_completion.png"
  #              
  #              rh_plot <- noaa_forecast %>%
  #                ggplot() +
  #                geom_point(aes(x = time, y = relative_humidity)) +
  #                facet_wrap(vars(site_id)) +
  #                theme_bw()
  #              
  #              ggsave(file = out_path, plot = rh_plot, height = 6, width = 9,
  #                     units = "in", dev = "png")
  #              
  #              return(out_path)
  #            }),
  # 
  # tar_target(noaa_precip_plot,
  #            {
  #              out_path <- "figures/noaa_precip_completion.png"
  #              
  #              precip_plot <- noaa_forecast %>%
  #                ggplot() +
  #                geom_point(aes(x = time, y = precipitation_flux)) +
  #                facet_wrap(vars(site_id)) +
  #                theme_bw()
  #              
  #              ggsave(file = out_path, plot = precip_plot, height = 6, width = 9,
  #                     units = "in", dev = "png")
  #              
  #              return(out_path)
  #            }),
  # 
  
  # 6. Reference documents --------------------------------------------------
  
  # An informational document with info on variables in the dataset and
  # diagnostic plots
  tar_render(dataset_overview,
             path = "dataset_overview.Rmd",
             packages = c( "kableExtra", "tidyverse")),  
  
  # A document compiling all of the warnings produced while building the
  # workflow
  tar_render(workflow_warnings,
             path = "workflow_warnings.Rmd",
             packages = c( "kableExtra", "tidyverse")),  
  
  
  # 7. File tracking --------------------------------------------------------
  
  # Track the outputs of targets in the workflow that are explicitly exported
  # as files (.csv, .png, etc.)
  
  # QC plots
  tar_file(tick_weather_missing_data_plot_path,
           tick_weather_missing_data_plot$out_path),
  
  tar_file(interpolated_tick_timeseries_plot_path,
           interpolated_tick_timeseries_plots$normal_file),
  
  tar_file(color_coded_interpolated_tick_timeseries_plot_path,
           interpolated_tick_timeseries_plots$color_coded_file),
  
  tar_file(chill_day_timeseries_plot_path,
           chill_day_timeseries_plot$out_path),
  
  tar_file(min_temp_source_comparison_plot_path,
           temperature_source_comparison_plots$min_file),
  
  tar_file(max_temp_source_comparison_plot_path,
           temperature_source_comparison_plots$max_file),
  
  tar_file(min_rh_source_comparison_plot_path,
           rh_source_comparison_plots$min_file),
  
  tar_file(max_rh_source_comparison_plot_path,
           rh_source_comparison_plots$max_file),
  
  tar_file(vpd_source_comparison_plot_path,
           vpd_source_comparison_plot$out_path),
  
  tar_file(precipitation_source_comparison_plot_path,
           precipitation_source_comparison_plot$out_path),
  
  # One-to-one plots
  tar_file(min_temp_one_to_one_plot_path,
           one_to_one_plots$min_temp_out_path),
  
  tar_file(min_temp_faceted_one_to_one_plot_path,
           one_to_one_plots$min_temp_facet_out_path),
  
  tar_file(max_temp_one_to_one_plot_path,
           one_to_one_plots$max_temp_out_path),
  
  tar_file(max_temp_faceted_one_to_one_plot_path,
           one_to_one_plots$max_temp_facet_out_path),
  
  tar_file(min_rh_one_to_one_plot_path,
           one_to_one_plots$min_rh_out_path),
  
  tar_file(min_rh_faceted_one_to_one_plot_path,
           one_to_one_plots$min_rh_facet_out_path),
  
  tar_file(max_rh_one_to_one_plot_path,
           one_to_one_plots$max_rh_out_path),
  
  tar_file(max_rh_faceted_one_to_one_plot_path,
           one_to_one_plots$max_rh_facet_out_path),
  
  tar_file(vpd_one_to_one_plot_path,
           one_to_one_plots$vpd_out_path),
  
  tar_file(vpd_faceted_one_to_one_plot_path,
           one_to_one_plots$vpd_facet_out_path),
  
  tar_file(precipitation_one_to_one_plot_path,
           one_to_one_plots$precip_out_path),
  
  tar_file(precipitation_faceted_one_to_one_plot_path,
           one_to_one_plots$precip_facet_out_path),
  
  # Datasets
  tar_file(final_dataset_path,
           interpolated_tick_data$csv_path),
  
  tar_file(training_set_path,
           training_and_test_sets$training_file),
  
  tar_file(test_set_path,
           training_and_test_sets$test_file),
  
  # Model-related
  tar_file(test_set_plots,
           combined_test_set_forecasts),
  
  # Documentation and metadata
  tar_file(dataset_overview_path,
           dataset_overview),
  
  tar_file(workflow_warnings_path,
           workflow_warnings)
  
)




