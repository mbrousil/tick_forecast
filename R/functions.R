# A script that contains functions used in the _targets.R script

# Download data from NEON

# SAAT: Single aspirated air temperature ----------------------------------

download_neon_saat <- function(target_sites){
  
  Sys.setenv("NEONSTORE_HOME" = "/neonstore")
  Sys.setenv("NEONSTORE_DB" = "/neonstore")
  
  neon_download(product = "DP1.00002.001",
                site = target_sites)
  
  # Combine and export
  walk(.x = target_sites,
       .f = ~ {
         
         temp_df <- neon_read(table = "SAAT_30min-basic", site = .x)
         
         write_rds(x = temp_df,
                   file = paste0("data/high_temporal_res/", tolower(.x), "_30min_temps.rds"))
         
       })
  
  return("data/high_temporal_res/")
  
}


# Clean the downloaded SAAT data for use
clean_neon_temps <- function(neon_path){
  
  neon_temps <- map_df(.x = list.files(path = "data/high_temporal_res/",
                                       pattern = "30min_temps.rds",
                                       full.names = TRUE),
                       .f = ~ {
                         
                         # Read in data and filter for the lowest tier of the tower
                         temp_data <- read_rds(.x) %>%
                           select(startDateTime, endDateTime, siteID,
                                  tempSingleMean, tempSingleMaximum, tempSingleMinimum, tempSingleVariance,
                                  finalQF_temp = finalQF,
                                  horizontalPosition_temp = horizontalPosition,
                                  verticalPosition_temp = verticalPosition) %>%
                           filter(verticalPosition_temp == "010") %>%
                           # If flagged as a fail, then make NA
                           mutate(across(.cols = contains("tempSingle"),
                                         .fns = ~if_else(finalQF_temp == 1,
                                                         true = NA_real_,
                                                         false = .x)))
                         
                         # Return to the outer function
                         return(temp_data)
                         
                       }) 
  
  return(neon_temps)
  
}


# Aggregate the SAAT data to daily level
aggregate_neon_temps <- function(cleaned_neon_temps){
  
  daily_neon_temps <- cleaned_neon_temps %>%
    mutate(date = as_date(startDateTime)) %>%
    group_by(siteID, date) %>%
    summarize(min_temp_c = min(tempSingleMinimum, na.rm = TRUE),
              mean_temp_c = mean(tempSingleMean, na.rm = TRUE),
              max_temp_c = max(tempSingleMaximum, na.rm = TRUE),
              mean_var_temp = mean(tempSingleVariance, na.rm = TRUE)) %>%
    mutate(across(.cols = contains("_temp"),
                  .fns = ~ if_else(condition = !is.finite(.x),
                                   true = NA_real_,
                                   false = .x)),
           year = year(date),
           day = yday(date)) %>%
    select(site_id = siteID, year, day, date, everything()) %>%
    ungroup()
  
  return(daily_neon_temps)
  
}


# RH: Relative humidity ---------------------------------------------------

download_neon_rh <- function(target_sites){
  
  Sys.setenv("NEONSTORE_HOME" = "/neonstore")
  Sys.setenv("NEONSTORE_DB" = "/neonstore")
  
  
  # Relative humidity
  neon_download(product = "DP1.00098.001",
                site = target_sites)
  
  # Combine and export
  walk(.x = target_sites,
       .f = ~ {
         
         temp_df <- neon_read(table = "RH_30min-basic", site = .x)
         
         write_rds(x = temp_df,
                   file = paste0("data/high_temporal_res/", tolower(.x), "_30min_rh.rds"))
         
       })
  
  return("data/high_temporal_res/")
}


# Clean the RH data for use
clean_neon_rh <- function(neon_path){
  
  # Compile all RH datasets into one data frame
  neon_rh <- map_df(.x = list.files(path = "data/high_temporal_res/",
                                    pattern = "30min_rh.rds",
                                    full.names = TRUE),
                    .f = ~ {
                      
                      rh_data <- read_rds(.x) %>%
                        select(startDateTime, endDateTime, siteID,
                               RHMean, RHMinimum, RHMaximum, RHVariance,
                               finalQF_rh = RHFinalQF,
                               horizontalPosition_rh = horizontalPosition,
                               verticalPosition_rh = verticalPosition) %>%
                        filter(verticalPosition_rh == "000") %>%
                        mutate(across(.cols = c("RHMean", "RHMinimum", "RHMaximum", "RHVariance"),
                                      .fns = ~if_else(finalQF_rh == 1,
                                                      true = NA_real_,
                                                      false = .x)))
                      
                      # Return to the outer function
                      return(rh_data)
                      
                    }) 
  
  return(neon_rh)
  
}


# Aggregate the RH data to daily level
aggregate_neon_rh <- function(cleaned_neon_rh){
  
  daily_neon_rh <- cleaned_neon_rh %>%
    mutate(date = as_date(startDateTime)) %>%
    group_by(siteID, date) %>%
    summarize(rh_min = min(RHMinimum, na.rm = TRUE),
              rh_mean = mean(RHMean, na.rm = TRUE),
              rh_max = max(RHMaximum, na.rm = TRUE),
              mean_var_rh = mean(RHVariance, na.rm = TRUE)) %>%
    mutate(across(.cols = c(contains("rh_"), "mean_var_rh"),
                  .fns = ~ if_else(condition = !is.finite(.x),
                                   true = NA_real_,
                                   false = .x)),
           year = year(date),
           day = yday(date)) %>%
    select(site_id = siteID, year, day, date, everything()) %>%
    ungroup()
  
  return(daily_neon_rh)
  
}


# PRIPRE: Primary precipitation -------------------------------------------

download_neon_precip <- function(target_sites){
  
  Sys.setenv("NEONSTORE_HOME" = "/neonstore")
  Sys.setenv("NEONSTORE_DB" = "/neonstore")
  
  # Precip
  neon_download(product = "DP1.00006.001",
                site = target_sites)
  
  # Combine and export. Different sites may have different available precipitation
  # types
  walk(.x = target_sites,
       .f = ~ {
         
         # First try retrieving primary precip
         pri_df <- neon_read(table = "PRIPRE_30min-basic", site = .x)
         
         # If primary is NULL, try secondary
         if (is.null(pri_df)) {
           
           sec_df <- neon_read(table = "SECPRE_30min-basic", site = .x)
           
           # If secondary is NULL, try throughfall
           if (is.null(sec_df)) {
             
             # Keep throughfall data and mark it
             precip_df <- neon_read(table = "THRPRE_30min-basic", site = .x) %>%
               mutate(precip_type = "THRPRE")
             
             # If secondary is not NULL, keep it and mark it
           } else {
             
             precip_df <- sec_df %>%
               mutate(precip_type = "SECPRE")
             
           }
           # If primary is not NULL, keep it and mark it
         } else {
           
           precip_df <- pri_df %>%
             mutate(precip_type = "PRIPRE")
           
         }
         
         # Write the resulting data out to rds
         write_rds(x = precip_df,
                   file = paste0("data/high_temporal_res/", tolower(.x), "_30min_precip.rds"))
       })
  
  
  
  return("data/high_temporal_res/")
}


# Clean the precip data for use
clean_neon_precip <- function(neon_path){
  
  neon_precip <- map_df(.x = list.files(path = "data/high_temporal_res/",
                                        pattern = "30min_precip.rds",
                                        full.names = TRUE),
                        .f = ~ {
                          
                          precip_data <- read_rds(.x) %>%
                            # Different measurement column names based on site;
                            # rename so that they can be used agnostically
                            rename(precip_bulk = matches("PrecipBulk"),
                                   # Realistic value range flag
                                   rangeQF_precip = matches("RangeQF"),
                                   # Outcome of scientific review flag
                                   scirevQF_precip = matches("SciRvwQF"),
                                   # For Primary precip data
                                   finalQF_precip = matches("priPrecipFinalQF")) %>%
                            select(startDateTime, endDateTime, siteID,
                                   precip_bulk, contains("QF"), precip_type,
                                   horizontalPosition_precip = horizontalPosition,
                                   verticalPosition_precip = verticalPosition)
                          
                          return(precip_data)
                          
                        }) %>%
    mutate(
      precip_bulk = case_when(
        ((rangeQF_precip %in% c(1, -1)) | (scirevQF_precip %in% c(1, 2))) &
          precip_type == "SECPRE" ~ NA_real_,
        finalQF_precip == 1 & precip_type == "PRIPRE" ~ NA_real_,
        TRUE ~ precip_bulk))
  
  return(neon_precip)
  
}


# Aggregate the precip data to the daily level
aggregate_neon_precip <- function(cleaned_neon_precip){
  
  daily_neon_precip <- cleaned_neon_precip %>%
    mutate(date = as_date(startDateTime)) %>%
    group_by(siteID, date) %>%
    summarize(sum_precip_mm = sum(precip_bulk, na.rm = TRUE),
              precip_type = unique(precip_type)) %>%
    mutate(sum_precip_mm = if_else(condition = !is.finite(sum_precip_mm),
                                   true = NA_real_,
                                   false = sum_precip_mm),
           year = year(date),
           day = yday(date)) %>%
    select(site_id = siteID, year, day, date, everything()) %>%
    ungroup()
  
  return(daily_neon_precip)
}


# Derived NEON VPD --------------------------------------------------------

# Vapor pressure data needs to be calculated using temp & RH data from NEON
derive_neon_vpd <- function(neon_path){
  
  neon_vpd <- map_df(.x = list.files(path = neon_path,
                                     pattern = "30min_rh.rds",
                                     full.names = TRUE),
                     .f = ~ {
                       
                       vpd_data <- read_rds(.x) %>%
                         select(startDateTime, endDateTime, siteID,
                                RHMean, RHMinimum, RHMaximum,
                                finalQF_rh = RHFinalQF,
                                tempRHMean, tempRHMinimum, tempRHMaximum,
                                finalQF_rhtemp = tempRHFinalQF,
                                horizontalPosition_rh = horizontalPosition,
                                verticalPosition_rh = verticalPosition) %>%
                         filter(verticalPosition_rh == "000") %>%
                         mutate(across(.cols = c("RHMean", "RHMinimum", "RHMaximum"),
                                       .fns = ~if_else(finalQF_rh == 1,
                                                       true = NA_real_,
                                                       false = .x)),
                                across(.cols = c("tempRHMean", "tempRHMinimum", "tempRHMaximum"),
                                       .fns = ~if_else(finalQF_rhtemp == 1,
                                                       true = NA_real_,
                                                       false = .x)),
                                min_vpd = RHtoVPD(RH = RHMinimum, TdegC = tempRHMinimum),
                                mean_vpd = RHtoVPD(RH = RHMean, TdegC = tempRHMean),
                                max_vpd = RHtoVPD(RH = RHMaximum, TdegC = tempRHMaximum))
                       
                       # Return to the outer function
                       return(vpd_data)
                       
                     }) 
  
  return(neon_vpd)
  
}

# Aggregate the VPD data to daily level
aggregate_neon_vpd <- function(neon_vpd){
  
  daily_neon_vpd <- neon_vpd %>%
    mutate(date = as_date(startDateTime)) %>%
    group_by(siteID, date) %>%
    summarize(
      min_vpd = min(min_vpd, na.rm = TRUE),
      mean_vpd = mean(mean_vpd, na.rm = TRUE),
      max_vpd = max(max_vpd, na.rm = TRUE)
    ) %>%
    mutate(across(.cols = contains("_vpd"),
                  .fns = ~ if_else(condition = !is.finite(.x),
                                   true = NA_real_,
                                   false = .x)),
           year = year(date),
           day = yday(date)) %>%
    select(site_id = siteID, year, day, date, everything())
  
  return(daily_neon_vpd)
  
}


# gridMET -----------------------------------------------------------------

# A function to be used in mapping over NetCDF files to retrieve and format
# past climate data
extract_from_brick <- function(brick_file, sites_sf, origin_date, variable){
  
  # brick_file = A file to be read in as a raster brick of climate data (gridmet intended)
  # sites_sf = An sf object containing NEON site locations
  # origin_date = The origin date of the .nc files being read in, as
  #               a character string. (e.g., when 'description: days
  #               since 1900-01-01', then the value would be "1900-01-01")
  # variable = A character string containing the name of the variable being
  #            extracted. This is not used to retrieve the variable, just to
  #            label its column in the output data frame 
  
  raster_brick <- brick(x = brick_file)
  
  # Ensure that they are in compatible reference systems
  assert_that(st_crs(raster_brick) == st_crs(sites_sf),
              msg = "CRS of brick_file != CRS of sites_sf")  
  
  extract(raster_brick, sites_sf, method = "simple", df = TRUE) %>%
    pivot_longer(names_to = "raw_dates", values_to = variable, cols = -"ID") %>%
    mutate(date = gsub(pattern = "X", replacement = "", x = raw_dates),
           date = as.numeric(date),
           date = ymd(origin_date) + days(date),
           ID = as.character(ID)) %>%
    left_join(x = .,
              y = as_tibble(sites_sf) %>%
                rownames_to_column() %>%
                dplyr::select(rowname, field_site_id),
              by = c("ID" = "rowname")) %>%
    dplyr::select(field_site_id, date, all_of(variable))
  
}


# Data cleaning and management --------------------------------------------

# Join the NEON and gridMET daily temperature datasets
combine_neon_gridmet_temps <- function(daily_neon_temps, gridmet_temps){
  
  
  temp_join <- full_join(x = ungroup(daily_neon_temps),
                         y = gridmet_temps,
                         by = c("site_id" = "field_site_id",
                                "date"),
                         suffix = c("_neon", "_gridmet")) %>%
    mutate(min_temp = if_else(condition = is.na(min_temp_c_neon),
                              true = min_temp_c_gridmet,
                              false = min_temp_c_neon),
           max_temp = if_else(condition = is.na(max_temp_c_neon),
                              true = max_temp_c_gridmet,
                              false = max_temp_c_neon),
           temp_source = case_when(
             
             (is.na(min_temp_c_neon) & is.na(max_temp_c_neon)) &
               (!is.na(min_temp_c_gridmet) & !is.na(max_temp_c_gridmet)) ~ "gridMET",
             
             (!is.na(min_temp_c_neon) & !is.na(max_temp_c_neon)) ~ "NEON",
             
             TRUE ~ "Check"),
           year = year(date),
           day = yday(date)) %>%
    distinct(site_id, year, day, date, min_temp, max_temp, temp_source)
  
  return(temp_join)
  
}

# Join the NEON and gridMET daily RH datasets
combine_neon_gridmet_rh <- function(daily_neon_rh, gridmet_rh_min, gridmet_rh_max){
  
  rh_join <- reduce(.x = list(daily_neon_rh %>%
                                rename(rh_min_neon = rh_min,
                                       rh_max_neon = rh_max,
                                       rh_mean_neon = rh_mean,
                                       mean_var_rh_neon = mean_var_rh),
                              rename(gridmet_rh_min,
                                     site_id = field_site_id,
                                     rh_min_gridmet = rh_min),
                              rename(gridmet_rh_max,
                                     site_id = field_site_id,
                                     rh_max_gridmet = rh_max)),
                    .f = full_join,
                    by = c("site_id", "date")) %>%
    mutate(rh_min = if_else(condition = is.na(rh_min_neon),
                            true = rh_min_gridmet,
                            false = rh_min_neon),
           rh_max = if_else(condition = is.na(rh_max_neon),
                            true = rh_max_gridmet,
                            false = rh_max_neon),
           rh_source = case_when(
             
             (is.na(rh_min_neon) & is.na(rh_max_neon)) &
               (!is.na(rh_min_gridmet) & !is.na(rh_max_gridmet)) ~ "gridMET",
             
             (!is.na(rh_min_neon) & !is.na(rh_max_neon)) ~ "NEON",
             
             TRUE ~ "Check"),
           year = year(date),
           day = yday(date)) %>%
    distinct(site_id, year, day, date, rh_min, rh_max, rh_source)
  
  return(rh_join)
  
}

# Join the NEON and gridMET daily VPD datasets
combine_neon_gridmet_vpd <- function(daily_neon_vpd, gridmet_vpd){
  
  vpd_join <- full_join(x = ungroup(daily_neon_vpd),
                        y = gridmet_vpd,
                        by = c("site_id" = "field_site_id",
                               "date"),
                        suffix = c("_neon", "_gridmet")) %>%
    mutate(combined_vpd = if_else(condition = is.na(mean_vpd),
                                  true = vpd,
                                  false = mean_vpd),
           vpd_source = case_when(
             
             is.na(mean_vpd) & !is.na(vpd) ~ "gridMET",
             
             !is.na(mean_vpd)  ~ "NEON",
             
             TRUE ~ "Check"),
           year = year(date),
           day = yday(date)) %>%
    distinct(site_id, year, day, date, vpd = combined_vpd, vpd_source)
  
  return(vpd_join)
  
}

# Join the NEON and gridMET daily precip datasets
combine_neon_gridmet_precip <- function(daily_neon_precip, gridmet_precip){
  
  
  precip_join <- full_join(x = ungroup(daily_neon_precip) %>%
                             rename(sum_precip_mm_neon = sum_precip_mm),
                           y = gridmet_precip,
                           by = c("site_id" = "field_site_id",
                                  "date"),
                           suffix = c("_neon", "_gridmet")) %>%
    mutate(sum_precip_mm = if_else(condition = is.na(sum_precip_mm_neon),
                                   true = pr,
                                   false = sum_precip_mm_neon),
           precip_source = case_when(
             
             is.na(sum_precip_mm_neon) & !is.na(pr) ~ "gridMET",
             
             !is.na(sum_precip_mm_neon)  ~ "NEON",
             
             TRUE ~ "Check"),
           year = year(date),
           day = yday(date)) %>%
    distinct(site_id, year, day, date, sum_precip_mm, precip_source)
  
  
  return(precip_join)
}


# Join all daily-level combined datasets together into one data frame
combine_daily_weather <- function(combined_temperature_data,
                                  combined_rh_data,
                                  combined_vpd_data,
                                  combined_precip_data){
  
  daily_weather <- reduce(.x = list(combined_temperature_data,
                                    combined_rh_data,
                                    combined_vpd_data,
                                    combined_precip_data),
                          .f = full_join,
                          by = c("site_id", "year", "day", "date")) 
  
  return(daily_weather)
  
}


# Perform linear interpolation of gaps in weather timeseries
interpolate_daily_weather <- function(daily_weather, potential_dates){
  
  # Make sure that every date we have any interest in including is represented
  extended_daily_weather <- full_join(x = daily_weather,
                                      y = potential_dates %>%
                                        mutate(day = yday(date)),
                                      by = c("site_id", "year", "date", "day"))
  
  
  weather_ts <- tsibble(extended_daily_weather, key = "site_id", index = "date")
  
  # Linear interpolation of the small number of missing values
  interpolated_weather <- weather_ts %>%
    group_by(site_id) %>%
    mutate(across(.cols = c(min_temp, max_temp, rh_min, rh_max, vpd, sum_precip_mm),
                  .fns = ~na_interpolation(., option = "linear"))) %>%
    as_tibble()
  
  return(interpolated_weather)
}


# Calculate degree days needed for modeling
calculate_degree_days <- function(interpolated_daily_weather,
                                  base_temp){
  
  # Daily degree day calculations
  dd_fractions <- interpolated_daily_weather %>%
    arrange(site_id, date) %>%
    group_by(site_id, year) %>%
    rowwise() %>%
    # Using equation 1 from Bouzek et al. 2013
    mutate(dd_mean_temp = mean(c(min_temp, max_temp)),
           dd = if_else(condition = dd_mean_temp < base_temp,
                        true = 0,
                        false = dd_mean_temp - base_temp)) %>%
    ungroup()
  
  # Now make new columns:
  # thirty_day_dd: The 30-day sum of DDs
  # lag_thirty_day_dd: Same as above, but lagged one day
  # lag_thirty_day_dd_*: Indicates 30-day sums, but from the specified num. weeks
  #   previous (i.e., indicative of the previous year)
  # cume_dd: The cumulative sum of DDs since Jan 01
  dd_aggregations <- dd_fractions %>%
    arrange(site_id, date) %>%
    group_by(site_id) %>%
    mutate(thirty_day_dd = rollsum(x = dd,
                                   # 30 days
                                   k = 30,
                                   # Indices with too few data points = NA
                                   fill = NA,
                                   # Move left to right
                                   align = "right"),
           lag_thirty_day_dd = lag(x = thirty_day_dd, n = 1L),
           lag_thirty_day_dd_34wk = lag(x = thirty_day_dd, n = 34L),
           lag_thirty_day_dd_50wk = lag(x = thirty_day_dd, n = 50L),
           lag_thirty_day_dd_42wk = lag(x = thirty_day_dd, n = 42L)) %>%
    group_by(site_id, year) %>%
    mutate(cume_dd = cumsum(dd)) %>%
    ungroup()
  
  # DD info needed for the workflow
  dd_export <- dd_aggregations %>%
    dplyr::select(site_id, date, year, temp_source, contains("dd"))
  
  return(dd_export)
  
}


# Calculate chill days needed for modeling
calculate_chill_days <- function(interpolated_daily_weather){
  
  # Daily chill day calculations
  cd_fractions <- interpolated_daily_weather %>%
    arrange(site_id, date) %>%
    rowwise() %>%
    mutate(cd_mean_temp = mean(c(min_temp, max_temp)),
           cd_fraction = 0 - cd_mean_temp,
           cd_fraction = if_else(condition = cd_fraction < 0,
                                 true = 0,
                                 false = cd_fraction)) %>%
    ungroup()
  
  # Sum within seasons
  cd_seasons <- cd_fractions %>%
    mutate(winter_season = case_when(
      date %within% lubridate::interval("2012-09-01", "2013-03-31") ~ "2012-2013",
      date %within% lubridate::interval("2013-09-01", "2014-03-31") ~ "2013-2014",
      date %within% lubridate::interval("2014-09-01", "2015-03-31") ~ "2014-2015",
      date %within% lubridate::interval("2015-09-01", "2016-03-31") ~ "2015-2016",
      date %within% lubridate::interval("2016-09-01", "2017-03-31") ~ "2016-2017",
      date %within% lubridate::interval("2017-09-01", "2018-03-31") ~ "2017-2018",
      date %within% lubridate::interval("2018-09-01", "2019-03-31") ~ "2018-2019",
      date %within% lubridate::interval("2019-09-01", "2020-03-31") ~ "2019-2020",
      date %within% lubridate::interval("2020-09-01", "2021-03-31") ~ "2020-2021",
      TRUE ~ "Out of season")) %>%
    filter(winter_season != "Out of season") %>%
    group_by(site_id, winter_season) %>%
    summarize(total_cd = sum(cd_fraction, na.rm = TRUE)) %>%
    ungroup()
  
  return(cd_seasons)
  
}


# A function to take the in-progress daily weather dataset and do some simple
# variable lagging
add_daily_lags <- function(interpolated_daily_weather){
  
  # Add single-day lags for a couple weather variables. ALSO check whether
  # the gap between rows is equal to 1 day, or more. If more than one day,
  # then the lags are made NA because they are not truly a lag of a single day
  daily_weather_lagged <- interpolated_daily_weather %>%
    group_by(site_id) %>%
    arrange(date) %>%
    mutate(rh_min_lag = lag(rh_min, 1L),
           vpd_lag = lag(vpd, 1L),
           sum_precip_mm_lag = lag(sum_precip_mm, 1L),
           continuity_check = as.numeric(difftime(time1 = date, time2 = lag(date, 1L))),
           across(.cols = contains("_lag"),
                  .fns = ~if_else(condition = continuity_check > 1,
                                  true = NA_real_,
                                  false = .))) %>%
    ungroup() %>%
    arrange(site_id, date)
  
  return(daily_weather_lagged)
  
}


# Add the degree day (and "chill day") metrics to the dataset and export a 
# weekly-level summary dataset
add_dd_and_aggregate <- function(daily_weather_lagged,
                                 degree_day_calculations,
                                 chill_day_calculations){
  
  daily_weather_w_dd <- left_join(x = daily_weather_lagged,
                                  y = degree_day_calculations,
                                  by = c("site_id", "year", "date", "temp_source")) %>%
    # Fill in the NA mean temps with the average of min and max:
    rowwise() %>%
    mutate(mean_temp = mean(c(min_temp, max_temp))) %>%
    ungroup() %>%
    # Assign seasons to join on CDs
    mutate(winter_season = case_when(
      date %within% lubridate::interval("2013-04-01", "2014-03-31") ~ "2012-2013",
      date %within% lubridate::interval("2014-04-01", "2015-03-31") ~ "2013-2014",
      date %within% lubridate::interval("2015-04-01", "2016-03-31") ~ "2014-2015",
      date %within% lubridate::interval("2016-04-01", "2017-03-31") ~ "2015-2016",
      date %within% lubridate::interval("2017-04-01", "2018-03-31") ~ "2016-2017",
      date %within% lubridate::interval("2018-04-01", "2019-03-31") ~ "2017-2018",
      date %within% lubridate::interval("2019-04-01", "2020-03-31") ~ "2018-2019",
      date %within% lubridate::interval("2020-04-01", "2021-03-31") ~ "2019-2020",
      date %within% lubridate::interval("2021-04-01", "2022-03-31") ~ "2020-2021")) %>%
    # Re-run the year column to make sure no NAs and add the MMWR week
    mutate(year = year(date),
           mmwr_year = epiyear(date),
           mmwr_week = epiweek(date)) %>%
    # Rearrange cols
    dplyr::select(site_id, date, year, mmwr_year, mmwr_week, winter_season,
                  everything())
  
  # A little cleaning needs to be done with the chill days, so we'll do this separately:
  weekly_chill <- daily_weather_w_dd %>%
    #  Combine "chill days" with the daily weather data
    left_join(x = .,
              y = chill_day_calculations,
              by = c("site_id", "winter_season")) %>%
    # Now for each site, year, week we will use the chill data for the first date
    # of that week, to avoid ambiguities over which date's data should be used
    # when the MMWR week falls on the March 31/April 1 threshold
    group_by(site_id, mmwr_year, mmwr_week) %>%
    filter(date == min(date)) %>%
    distinct(site_id, mmwr_year, mmwr_week, total_cd)
  
  # Also going to grab lagged values by week so that we can use the previous week's
  # (end of week) values for two of the DD variables
  dd_weekly <- degree_day_calculations %>%
    group_by(site_id, mmwr_year = epiyear(date), mmwr_week = epiweek(date)) %>%
    filter(date == max(date)) %>%
    dplyr::select(thirty_day_dd, cume_dd) %>%
    ungroup() 
  
  dd_weekly_lag <- dd_weekly %>%
    group_by(site_id) %>%
    # Get the final value of 30-day DD and cume DD from the previous week
    mutate(prev_week_30d_dd = lag(thirty_day_dd),
           prev_week_cume_dd = lag(cume_dd)) %>%
    ungroup() %>%
    dplyr::select(-c(thirty_day_dd, cume_dd))
  
  # Now we need to aggregate the dataset to one value per MMWR week
  weekly_weather <- daily_weather_w_dd %>%
    # Now we aggregate by site*year*week:
    group_by(site_id, mmwr_year, mmwr_week) %>%
    summarize(
      mean_temp = mean(mean_temp, na.rm = TRUE),
      min_temp = min(min_temp, na.rm = TRUE),
      max_temp = max(max_temp, na.rm = TRUE),
      rh_min = min(rh_min, na.rm = TRUE),
      rh_max = max(rh_max, na.rm = TRUE),
      mean_vpd = mean(vpd, na.rm = TRUE),
      mean_precip_mm = mean(sum_precip_mm, na.rm = TRUE),
      sum_precip_mm = sum(sum_precip_mm, na.rm = TRUE),
      dd = mean(dd, na.rm = TRUE),
      thirty_day_dd = mean(thirty_day_dd, na.rm = TRUE),
      lag_thirty_day_dd_34wk = mean(lag_thirty_day_dd_34wk, na.rm = TRUE),
      lag_thirty_day_dd_42wk = mean(lag_thirty_day_dd_42wk, na.rm = TRUE),
      lag_thirty_day_dd_50wk = mean(lag_thirty_day_dd_50wk, na.rm = TRUE)) %>%
    ungroup() %>%
    # Join in the two remaining DD and CD weekly dataframes
    reduce(.x = list(., dd_weekly_lag, weekly_chill),
           .f = left_join,
           by = c("site_id", "mmwr_year", "mmwr_week")) %>%
    rename(prev_winter_cume_cd = total_cd)
  
  return(weekly_weather)
}


# Join ticks to the compiled weather dataset!
join_ticks_with_weather <- function(weekly_weather_summary,
                                    tick_counts){
  
  # Join with the weekly tick data:
  ticks_w_weather <- left_join(x = weekly_weather_summary,
                               y = tick_counts %>%
                                 mutate(mmwr_year = year(time)),
                               by = c("site_id" = "siteID",
                                      "mmwr_year",
                                      "mmwr_week" = "mmwrWeek")) %>%
    dplyr::select(-time) %>%
    # Add column with date of MMWR week start
    mutate(date = MMWRweek2Date(mmwr_year, mmwr_week),
           jd = yday(date)) %>%
    dplyr::select(site_id, date, jd, mmwr_year, mmwr_week, everything())
  
  return(ticks_w_weather)
  
}


# Interpolate the few new missing values in the dataset and then create lags
# for tree-based methods
interpolate_dataset <- function(ticks_w_weather, tick_counts){
  
  # Earliest observations from each site, which will be used to filter the results
  tick_start_dates <- tick_counts %>%
    group_by(site_id = siteID) %>%
    summarize(start_date = min(time)) %>%
    ungroup()
  
  # Create a timeseries object for interpolation
  tick_ts <- ticks_w_weather %>%
    # Filter out observations earlier than the initial sampling date
    left_join(x = .,
              y = tick_start_dates,
              by = "site_id") %>%
    group_by(site_id) %>%
    filter(date >= start_date) %>%
    ungroup() %>%
    dplyr::select(-start_date) %>%
    as_tsibble(index = date, key = site_id)
  
  # Interpolation process:
  # First, fill gaps so that all dates are present
  # Then assign some zeroes to ticks: If NA & mean_temp < 9.6C, or if NA and
  # date is between Oct. 15 and March 1
  tick_interp <- tick_ts %>%
    # This step probably isn't necessary, but present as a backup:
    fill_gaps() %>%
    mutate(amam_filled = if_else(
      (condition = is.na(amblyomma_americanum) & mean_temp < 9.6) | 
        (condition = is.na(amblyomma_americanum) & (yday(date) >= 288 | yday(date) < 60)),
      true = 0, false = amblyomma_americanum)) %>%
    group_by(site_id) %>%
    mutate(
      across(.cols = where(is.numeric), ~if_else(!is.finite(.), true = NA_real_, false = .)),
      amam_filled = na_interpolation(amam_filled, option = "linear"),
      across(.cols = c("mean_temp", "min_temp", "max_temp", "rh_min",
                       "rh_max", "mean_vpd", "mean_precip_mm",
                       "sum_precip_mm", "dd", "thirty_day_dd", "lag_thirty_day_dd_34wk",
                       "lag_thirty_day_dd_42wk", "lag_thirty_day_dd_50wk",
                       "prev_week_30d_dd", "prev_week_cume_dd", "prev_winter_cume_cd"),
             .fns = ~na_interpolation(., option = "linear"))) %>%
    ungroup() %>%
    # Column noting interpolation of tick data
    mutate(tick_interp_flag = if_else(is.na(amblyomma_americanum) & !is.na(amam_filled),
                                      true = "interpolated",
                                      false = "original"))
  
  # Add in the lags Vera uses
  tick_interp_lag <- tick_interp %>%
    group_by(site_id) %>%
    mutate(
      # Initial rolling average
      across(.cols = c(amam_filled, mean_vpd),
             .f = ~ rollmean(x = .x, k = 4, fill = NA, align = "right"),
             .names = "{.col}_4wk_rollmean"),
      # Lag one week
      across(.cols = c(amam_filled_4wk_rollmean, mean_vpd_4wk_rollmean),
             .f = ~ lag(x = .x, n = 1L),
             .names = "{.col}_lag1"),
      # Lag 50 weeks
      across(.cols = c(amam_filled_4wk_rollmean, mean_vpd_4wk_rollmean),
             .f = ~ lag(x = .x, n = 50L),
             .names = "{.col}_lag50"),
      # There's new NAs in these new columns due to the lagging process, so interpolate
      across(.cols = c(contains("rollmean_lag50"), contains("rollmean_lag1")),
             .fns = ~ na_interpolation(., option = "linear"))) %>%
    ungroup() %>%
    rename(
      amam_4wk_rollmean_lag1 = amam_filled_4wk_rollmean_lag1, 
      amam_4wk_rollmean_lag50 = amam_filled_4wk_rollmean_lag50, 
      dd_30d_rollsum_lag34 = lag_thirty_day_dd_34wk, 
      dd_30d_rollsum_lag42 = lag_thirty_day_dd_42wk, 
      dd_30d_rollsum_lag50 = lag_thirty_day_dd_50wk, 
      dd_30d_rollsum_prev_week = prev_week_30d_dd,
      cume_dd_prev_week = prev_week_cume_dd,
      cume_cd_prev_winter = prev_winter_cume_cd) %>%
    select(-c(amam_filled_4wk_rollmean, mean_vpd_4wk_rollmean))
  
  out_path <- "data/final_tick_dataset.csv"
  
  write_csv(x = tick_interp_lag, file = out_path)
  
  return(list(
    dataset = tick_interp_lag,
    csv_path = out_path
  ))
}


# Create plots showing min/max temperatures by data source
plot_temperatures_by_source <- function(daily_weather_lagged){
  
  min_temp_source_timeseries <- daily_weather_lagged %>%
    ggplot() +
    geom_point(aes(x = date, y = min_temp, fill = temp_source),
               pch = 21, color = "black", alpha = 0.8) +
    xlab("Date") +
    ylab("Minimum temperature (C)") +
    facet_wrap(vars(site_id), scales = "free") +
    scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  min_out_path <- "figures/workflow_diagnostics/min_temp_source_timeseries.png"
  
  ggsave(filename = min_out_path, plot = min_temp_source_timeseries,
         width = 15, height = 8, units = "in", dev = "png")
  
  max_temp_source_timeseries <- daily_weather_lagged %>%
    ggplot() +
    geom_point(aes(x = date, y = max_temp, fill = temp_source),
               pch = 21, color = "black", alpha = 0.8) +
    xlab("Date") +
    ylab("Maximum temperature (C)") +
    facet_wrap(vars(site_id), scales = "free") +
    scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  max_out_path <- "figures/workflow_diagnostics/max_temp_source_timeseries.png"
  
  ggsave(filename = max_out_path, plot = max_temp_source_timeseries,
         width = 15, height = 8, units = "in", dev = "png")
  
  return(list(
    min_plot = min_temp_source_timeseries,
    min_file = min_out_path,
    max_plot = max_temp_source_timeseries,
    max_file = max_out_path
  ))
  
}


# Plot the timeseries for each site after interpolation. Produces two versions
# of the plot, one with and another without color-coding for interpolation
plot_tick_timeseries_plots <- function(interpolated_tick_data){
  
  tick_timeseries <- ggplot(data = interpolated_tick_data) +
    geom_line(aes(x = date, y = amam_filled)) +
    facet_wrap(vars(site_id)) +
    ylab("Tick density per 1600m2 (after interpolation)") +
    xlab("Date") +
    theme_bw()
  
  normal_out_path <- "figures/workflow_diagnostics/interpolated_tick_timseries.png"
  
  ggsave(filename = normal_out_path, plot = tick_timeseries,
         width = 7.5, height = 5, units = "in", device = "png")
  
  data_type_tick_timeseries <- interpolated_tick_data %>%
    filter(tick_interp_flag == "original") %>%
    ggplot() +
    geom_line(data = interpolated_tick_data,
              aes(x = date, y = amam_filled, color = "Interpolated data")) +
    geom_point(aes(x = date, y = amam_filled, fill = "Original data"),
               alpha = 0.7, pch = 21, color = "black") +
    facet_wrap(vars(site_id), scales = "free") +
    ylab("Tick density per 1600m2") +
    xlab("Date") +
    scale_fill_manual(NULL, values = "#FDE725FF") +
    scale_color_manual(NULL, values = "gray20") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  type_out_path <- "figures/workflow_diagnostics/interpolated_tick_timseries_colorcoded.png"
  
  ggsave(filename = type_out_path, plot = data_type_tick_timeseries,
         width = 11, height = 6, units = "in", device = "png")
  
  return(list(
    normal_plot = tick_timeseries,
    normal_file = normal_out_path,
    color_coded_plot = data_type_tick_timeseries,
    color_coded_file = type_out_path
  ))  
}


# Create plots showing RH by data source
plot_rh_by_source <- function(daily_weather_lagged){
  
  min_rh_source_timeseries <- daily_weather_lagged %>%
    ggplot() +
    geom_point(aes(x = date, y = rh_min, fill = rh_source),
               pch = 21, color = "black", alpha = 0.8) +
    xlab("Date") +
    ylab("Minimum RH (%)") +
    facet_wrap(vars(site_id), scales = "free") +
    scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  min_out_path <- "figures/workflow_diagnostics/min_rh_source_timeseries.png"
  
  ggsave(filename = min_out_path, plot = min_rh_source_timeseries,
         width = 15, height = 8, units = "in", dev = "png")
  
  max_rh_source_timeseries <- daily_weather_lagged %>%
    ggplot() +
    geom_point(aes(x = date, y = rh_max, fill = rh_source),
               pch = 21, color = "black", alpha = 0.8) +
    xlab("Date") +
    ylab("Maximum RH (%)") +
    facet_wrap(vars(site_id), scales = "free") +
    scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  max_out_path <- "figures/workflow_diagnostics/max_rh_source_timeseries.png"
  
  ggsave(filename = max_out_path, plot = max_rh_source_timeseries,
         width = 15, height = 8, units = "in", dev = "png")
  
  return(list(
    min_plot = min_rh_source_timeseries,
    min_file = min_out_path,
    max_plot = max_rh_source_timeseries,
    max_file = max_out_path
  ))
  
}


# Create plots showing VPD by data source
plot_vpd_by_source <- function(daily_weather_lagged){
  
  vpd_source_timeseries <- daily_weather_lagged %>%
    ggplot() +
    geom_point(aes(x = date, y = vpd, fill = vpd_source),
               pch = 21, color = "black", alpha = 0.8) +
    xlab("Date") +
    ylab("Minimum VPD") +
    facet_wrap(vars(site_id), scales = "free") +
    scale_fill_viridis_d("Data source", option = "viridis", begin = 0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  out_path <- "figures/workflow_diagnostics/vpd_source_timeseries.png"
  
  ggsave(filename = out_path, plot = vpd_source_timeseries,
         width = 15, height = 8, units = "in", dev = "png")
  
  return(list(
    vpd_plot = vpd_source_timeseries,
    out_path = out_path))
  
}


# A function to make 1:1 plots of daily-level weather data to check source agreement
# Makes plots both aggregated across sites and faceted by site
make_one_to_one_plots <- function(daily_neon_temps, daily_neon_rh, 
                                  daily_neon_precip,  daily_neon_vpd,
                                  gridmet_temps, gridmet_vpd,
                                  gridmet_rh_min, gridmet_rh_max,
                                  gridmet_precip){
  
  # Set the locations where plots will be sent
  
  # One to one plots (all sites in one plot)
  min_temp_out_path <- "figures/gridmet_vs_neon_min_temp.png"
  max_temp_out_path <- "figures/gridmet_vs_neon_max_temp.png"
  min_rh_out_path <- "figures/gridmet_vs_neon_min_rh.png"
  max_rh_out_path <- "figures/gridmet_vs_neon_max_rh.png"
  vpd_out_path <- "figures/gridmet_vs_neon_vpd.png"
  precip_out_path <- "figures/gridmet_vs_neon_precip.png"
  
  # Plot version with a facet for each site
  min_temp_facet_out_path <- "figures/gridmet_vs_neon_min_temp_facet.png"
  max_temp_facet_out_path <- "figures/gridmet_vs_neon_max_temp_facet.png"
  min_rh_facet_out_path <- "figures/gridmet_vs_neon_min_rh_facet.png"
  max_rh_facet_out_path <- "figures/gridmet_vs_neon_max_rh_facet.png"
  vpd_facet_out_path <- "figures/gridmet_vs_neon_vpd_facet.png"
  precip_facet_out_path <- "figures/gridmet_vs_neon_precip_facet.png"
  
  
  # Temperature comparisons -------------------------------------------------
  
  # Combine both temperature sources
  test_temp <- full_join(x = ungroup(daily_neon_temps),
                         y = gridmet_temps,
                         by = c("site_id" = "field_site_id",
                                "date"),
                         suffix = c("_neon", "_gridmet")) %>%
    mutate(min_temp = if_else(condition = is.na(min_temp_c_neon),
                              true = min_temp_c_gridmet,
                              false = min_temp_c_neon),
           max_temp = if_else(condition = is.na(max_temp_c_neon),
                              true = max_temp_c_gridmet,
                              false = max_temp_c_neon),
           temp_source = case_when(
             
             (is.na(min_temp_c_neon) & is.na(max_temp_c_neon)) &
               (!is.na(min_temp_c_gridmet) & !is.na(max_temp_c_gridmet)) ~ "gridMET",
             
             (!is.na(min_temp_c_neon) & !is.na(max_temp_c_neon)) ~ "NEON",
             
             TRUE ~ "Check"))
  
  
  # Min temp comparison
  min_temp_plot <- test_temp %>%
    ggplot() +
    geom_hex(aes(x = min_temp_c_neon, y = min_temp_c_gridmet),
             binwidth = 1, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(-30, 30)) +
    ylim(c(-30, 30)) +
    xlab("NEON min. temp, C") +
    ylab("gridMET min. temp, C") +
    scale_fill_viridis_c("Observation count") +
    theme_bw() +
    ggtitle("gridMET minimum temps vs. NEON minimum temps") +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = min_temp_out_path, plot = min_temp_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  min_temp_facet_plot <- test_temp %>%
    ggplot() +
    geom_hex(aes(x = min_temp_c_neon, y = min_temp_c_gridmet),
             binwidth = 1, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(-30, 30)) +
    ylim(c(-30, 30)) +
    xlab("NEON min. temp, C") +
    ylab("gridMET min. temp, C") +
    scale_fill_viridis_c("Observation count") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = min_temp_facet_out_path, plot = min_temp_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  # Max temp comparison
  max_temp_plot <- test_temp %>%
    ggplot() +
    geom_hex(aes(x = max_temp_c_neon, y = max_temp_c_gridmet),
             binwidth = 1, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlab("NEON max. temp, C") +
    ylab("gridMET max. temp, C") +
    scale_fill_viridis_c("Observation count") +
    theme_bw() +
    ggtitle("gridMET maximum temps vs. NEON maximum temps") +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = max_temp_out_path, plot = max_temp_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  max_temp_facet_plot <- test_temp %>%
    ggplot() +
    geom_hex(aes(x = max_temp_c_neon, y = max_temp_c_gridmet),
             binwidth = 1, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlab("NEON max. temp, C") +
    ylab("gridMET max. temp, C") +
    scale_fill_viridis_c("Observation count") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = max_temp_facet_out_path, plot = max_temp_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  # RH comparisons ----------------------------------------------------------
  
  test_rh <- reduce(.x = list(daily_neon_rh %>%
                                rename(rh_min_neon = rh_min,
                                       rh_max_neon = rh_max,
                                       rh_mean_neon = rh_mean,
                                       mean_var_rh_neon = mean_var_rh),
                              rename(gridmet_rh_min,
                                     site_id = field_site_id,
                                     rh_min_gridmet = rh_min),
                              rename(gridmet_rh_max,
                                     site_id = field_site_id,
                                     rh_max_gridmet = rh_max)),
                    .f = full_join,
                    by = c("site_id", "date")) %>%
    mutate(rh_min = if_else(condition = is.na(rh_min_neon),
                            true = rh_min_gridmet,
                            false = rh_min_neon),
           rh_max = if_else(condition = is.na(rh_max_neon),
                            true = rh_max_gridmet,
                            false = rh_max_neon),
           rh_source = case_when(
             
             (is.na(rh_min_neon) & is.na(rh_max_neon)) &
               (!is.na(rh_min_gridmet) & !is.na(rh_max_gridmet)) ~ "gridMET",
             
             (!is.na(rh_min_neon) & !is.na(rh_max_neon)) ~ "NEON",
             
             TRUE ~ "Check"))
  
  
  min_rh_plot <- test_rh %>%
    ggplot() +
    geom_hex(aes(x = rh_min, y = rh_min_gridmet),
             binwidth = 2, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 105)) +
    ylim(c(0, 105)) +
    xlab("NEON RH min, %") +
    ylab("gridMET RH min, %") +
    scale_fill_viridis_c("Observation count") +
    theme_bw() +
    ggtitle(label = "gridMET min RH vs. NEON min RH") +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = min_rh_out_path, plot = min_rh_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  min_rh_facet_plot <- test_rh %>%
    ggplot() +
    geom_hex(aes(x = rh_min, y = rh_min_gridmet),
             binwidth = 2, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 105)) +
    ylim(c(0, 105)) +
    xlab("NEON RH min, %") +
    ylab("gridMET RH min, %") +
    scale_fill_viridis_c("Observation count") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = min_rh_facet_out_path, plot = min_rh_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  max_rh_plot <- test_rh %>%
    ggplot() +
    geom_hex(aes(x = rh_max, y = rh_max_gridmet),
             binwidth = 2, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 110)) +
    ylim(c(0, 110)) +
    xlab("NEON RH max, %") +
    ylab("gridMET RH max, %") +
    scale_fill_viridis_c("Observation count") +
    theme_bw() +
    ggtitle(label = "gridMET max RH vs. NEON max RH") +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = max_rh_out_path, plot = max_rh_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  max_rh_facet_plot <- test_rh %>%
    ggplot() +
    geom_hex(aes(x = rh_max, y = rh_max_gridmet),
             binwidth = 2, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 110)) +
    ylim(c(0, 110)) +
    xlab("NEON RH max, %") +
    ylab("gridMET RH max, %") +
    scale_fill_viridis_c("Observation count") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = max_rh_facet_out_path, plot = max_rh_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  # VPD comparisons ---------------------------------------------------------
  
  test_vpd <- full_join(x = ungroup(daily_neon_vpd),
                        y = gridmet_vpd,
                        by = c("site_id" = "field_site_id",
                               "date"),
                        suffix = c("_neon", "_gridmet")) %>%
    mutate(combined_vpd = if_else(condition = is.na(mean_vpd),
                                  true = vpd,
                                  false = mean_vpd),
           vpd_source = case_when(
             
             is.na(mean_vpd) & !is.na(vpd) ~ "gridMET",
             
             !is.na(mean_vpd)  ~ "NEON",
             
             TRUE ~ "Check"))
  
  vpd_plot <- test_vpd %>%
    ggplot() +
    geom_hex(aes(x = mean_vpd, y = vpd), binwidth = 0.075, color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(-0.25, 3.6)) +
    ylim(c(-0.25, 3.6)) +
    xlab("NEON VPD") +
    ylab("gridMET VPD") +
    scale_fill_viridis_c("Observation count") +
    ggtitle(label = "gridMET VPD vs. NEON VPD") +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = vpd_out_path, plot = vpd_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  vpd_facet_plot <- test_vpd %>%
    ggplot() +
    geom_hex(aes(x = mean_vpd, y = vpd), binwidth = 0.075, color = "gray40") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70", size = 1) +
    scale_fill_viridis_c("Observation count") +
    xlim(c(-0.25, 3.6)) +
    ylim(c(-0.25, 3.6)) +
    xlab("NEON VPD") +
    ylab("gridMET VPD") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = vpd_facet_out_path, plot = vpd_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  # Precip comparisons ------------------------------------------------------
  
  test_precip <- full_join(x = ungroup(daily_neon_precip) %>%
                             rename(sum_precip_mm_neon = sum_precip_mm),
                           y = gridmet_precip,
                           by = c("site_id" = "field_site_id",
                                  "date"),
                           suffix = c("_neon", "_gridmet")) %>%
    mutate(sum_precip_mm = if_else(condition = is.na(sum_precip_mm_neon),
                                   true = pr,
                                   false = sum_precip_mm_neon),
           precip_source = case_when(
             
             is.na(sum_precip_mm_neon) & !is.na(pr) ~ "gridMET",
             
             !is.na(sum_precip_mm_neon)  ~ "NEON",
             
             TRUE ~ "Check"))
  
  
  precip_plot <- test_precip %>%
    ggplot() +
    geom_hex(aes(x = sum_precip_mm_neon, y = pr),
             color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlab("NEON precip, mm") +
    ylab("gridMET precip, mm") +
    scale_fill_viridis_c("Observation count") +
    ggtitle(label = "gridMET precip vs. NEON precip") +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16))
  
  ggsave(filename = "figures/gridmet_vs_neon_precip.png", plot = precip_plot,
         width = 10, height = 8.5, units = "in", device = "png")
  
  precip_facet_plot <- test_precip %>%
    ggplot() +
    geom_hex(aes(x = sum_precip_mm_neon, y = pr),
             color = "gray40") +
    geom_abline(slope = 1, intercept = 0) +
    xlab("NEON precip, mm") +
    ylab("gridMET precip, mm") +
    scale_fill_viridis_c("Observation count") +
    ggtitle(label = "gridMET precip vs. NEON precip") +
    facet_wrap(vars(site_id)) +
    theme_bw() +
    theme(axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          title = element_text(size = 16),
          strip.text = element_text(size = 13))
  
  ggsave(filename = precip_facet_out_path, plot = precip_facet_plot,
         width = 14, height = 14, units = "in", device = "png")
  
  
  # Export the plots both as R objects and as .png files and track both
  return(list(
    # One-to-one plot for min temp
    min_temp_out_path = min_temp_out_path,
    min_temp_plot = min_temp_plot,
    
    # One-to-one plot (faceted by site) for min temp
    min_temp_facet_out_path = min_temp_facet_out_path,
    min_temp_facet_plot = min_temp_facet_plot,
    
    # Max temp
    max_temp_out_path = max_temp_out_path,
    max_temp_plot = max_temp_plot,
    
    max_temp_facet_out_path = max_temp_facet_out_path,
    max_temp_facet_plot = max_temp_facet_plot,
    
    # Min RH
    min_rh_out_path = min_rh_out_path,
    min_rh_plot = min_rh_plot,
    
    min_rh_facet_out_path = min_rh_facet_out_path,
    min_rh_facet_plot = min_rh_facet_plot,
    
    # Max RH
    max_rh_out_path = max_rh_out_path,
    max_rh_plot = max_rh_plot,
    
    max_rh_facet_out_path = max_rh_facet_out_path,
    max_rh_facet_plot = max_rh_facet_plot,
    
    # VPD
    vpd_out_path = vpd_out_path,
    vpd_plot = vpd_plot,
    
    vpd_facet_out_path = vpd_facet_out_path,
    vpd_facet_plot = vpd_facet_plot,
    
    # Precip
    precip_out_path = precip_out_path,
    precip_plot = precip_plot,
    
    precip_facet_out_path = precip_facet_out_path,
    precip_facet_plot = precip_facet_plot))
  
}


# Function to download the NOAA GEFS forecast data and arrange it into a data frame
download_noaa_forecast <- function(tick_counts, start_date, end_date){
  
  # Create sequence of dates to download
  forecast_dates <- seq(from = as_date(start_date),
                        to = as_date(end_date),
                        by = "day")
  
  # Download the NOAA forecast datasets, day-by-day
  walk(.x = forecast_dates,
       .f = ~ download_noaa(siteID = unique(tick_counts$siteID),
                            date = .x,
                            dir = "data/noaa_forecasts/"))
  
  # Read in the six-hour forecasts
  all_forecasts <- map(.x = seq(from = as_date("2021-01-01"),
                                to = as_date("2021-09-30"),
                                by = "day"),
                       .f = ~ stack_noaa(dir = "data/noaa_forecasts/",
                                         forecast_date = .x,
                                         model = "NOAAGEFS_6hr"))
  
  # Compile all forecast data into a single dataframe
  forecast_df <- map_df(.x = all_forecasts,
                        .f = ~ .)
  
  return(forecast_df)
}


# Function to aggregate downloaded NOAA GEFS by day and ensemble member. Code
# adapted from EFI example
aggregate_noaa <- function(start_date, end_date, target_sites, noaa_forecast) {
  
  # A df of all possible dates and locations
  all_dates <- crossing(site_id = target_sites,
                        date = seq(from = as_date(start_date),
                                   to = as_date(end_date),
                                   by = "1 day"))
  
  # Clean up the GEFS data
  # Unit notes based on netCDF file:
  # precipitation_flux units: kgm-2s-1 (kg rain passing through 1mx1m square each second)
  # air_temperature units: K
  noaa_clean <- noaa_forecast %>% 
    mutate(date = as_date(time),
           relative_humidity = relative_humidity * 100,
           air_temperature = conv_unit(x = air_temperature, from = "K", to = "C"),
           vpd = RHtoVPD(RH = relative_humidity, TdegC = air_temperature)) %>%
    rename(site_id = siteID) %>% 
    mutate(ensemble = as.numeric(stringr::str_sub(ensemble, start = 4, end = 6)))
  
  # Major summary stats
  noaa_future_mean <- noaa_clean %>%
    group_by(site_id, date, runStartDate, runEndDate, ensemble) %>% 
    summarize(min_temp = min(air_temperature, na.rm = TRUE),
              mean_temp = mean(air_temperature, na.rm = TRUE),
              max_temp = max(air_temperature, na.rm = TRUE),
              min_rh = min(relative_humidity, na.rm = TRUE),
              max_rh = max(relative_humidity, na.rm = TRUE),
              mean_vpd = mean(vpd, na.rm = TRUE)) %>%
    ungroup()
  
  # Precip
  # https://www.researchgate.net/post/How-to-convert-precipitation-flux-to-mm
  # 1kg of rain over 1m2 = 1mm
  # So multiply by number of seconds in the forecast period (6 hr * 60 seconds = 360)
  # and then add together periods of the day
  precip_sum <- noaa_clean %>%
    mutate(precip_accum = 360 * precipitation_flux) %>%
    group_by(site_id, date, runStartDate, runEndDate, ensemble) %>% 
    summarize(sum_precip_mm = sum(precip_accum, na.rm = TRUE)) %>%
    ungroup()
  
  noaa_daily <- full_join(x = noaa_future_mean,
                          y = precip_sum,
                          by = c("site_id", "date", "runStartDate", "runEndDate", "ensemble"))
  
  # Check if anything is missing:
  noaa_all_dates <- full_join(x = all_dates,
                              y = noaa_daily,
                              by = c("site_id", "date"))
  
  # There should be multiple unique values per date (site_id * ensemble * runStartDate)
  # but we'll just check if each day has data
  date_check <- noaa_all_dates %>%
    filter(if_any(everything(), is.na))
  
  if(nrow(date_check) > 0){
    
    warning("Some dates have NA data. This likely indicates incomplete NOAA forecasts.")
    
  }
  
  return(noaa_all_dates)
  
}




































