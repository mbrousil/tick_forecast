
library(targets)

library(tidyverse)
library(lubridate)

# Plot forecasts
forecasts <- map_df(.x = list.files(path = "data/tick_forecasts/",
                                    pattern = ".csv",
                                    full.names = TRUE),
                    .f = read_csv)

forecasts %>%
  group_by(datetime, site_id, model_id) %>%
  summarize(min_pred = min(predicted),
            max_pred = max(predicted),
            mean_pred = mean(predicted)) %>%
  ggplot() +
  geom_linerange(aes(x = datetime, y = mean_pred, ymin = min_pred, ymax = max_pred,
                     color = model_id)) +
  # geom_point(aes(x = datetime, y = mean_pred, color = model_id)) +
  geom_line(aes(x = datetime, y = mean_pred, color = model_id)) +
  facet_wrap(vars(site_id), scales = "free_y") +
  xlab("Date") +
  ylab("Predicted A. americanum") +
  ggtitle("Tick predictions for 2021-09-27", subtitle = "Ranges are min/max; lines are means") +
  theme_bw()







