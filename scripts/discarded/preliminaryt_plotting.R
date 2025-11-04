library(ggplot2)

source("scripts/read_data_20250521.R")
source("scripts/read_sensor_data.R")

plotting <- APS_number_concentration_20250521candles |>
  full_join(dusttrak_20250521candles, by = "timestamp_rounded_5min" ) |>
  full_join(SMPS_mass_20250521candles, by = ("timestamp_rounded_5min")) |>
  # full_join(dusttrak_20250521candles, by = "timestamp_rounded_5min", suffix = c(".x", ".y")) |>
  select(timestamp_rounded_5min, PM1_SMPS, PM1_SMPS_SPS30, PM2.5_APS_only, PM2.5_APS_valid, PM1_APS_only, PM1_APS_valid, `PM2.5 [mg/m3]`)


plotting |> ggplot(aes(x = timestamp_rounded_5min)) +
  geom_line(aes(y = PM2.5_APS_valid, color = "PM2.5 APS", na.rm = TRUE)) +
  #geom_line(aes(y = PM1_APS_valid,  color = "PM1 APS", na.rm = TRUE)) +
  geom_line(aes(y = PM1_SMPS_SPS30 , color = "PM1 SMPS", na.rm = TRUE)) +
  #geom_point(aes(y = `PM2.5 [mg/m3]` * 1000 , color = "PM2.5 dusttrak", na.rm = TRUE)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


plotting <- plotting |> left_join(all_data_LCS, by = "timestamp_rounded_5min")
plotting |>
  filter(sensor != 80 & sensor != 201) |>
  ggplot(aes(x = timestamp_rounded_5min)) +
  geom_point(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS valid", na.rm = TRUE)) +
  geom_point(aes(y = PM2.5_APS_only * 1000, color = "PM2.5 APS", na.rm = TRUE)) +
  geom_point(aes(y = PM1_APS_only * 1000,  color = "PM1 APS", na.rm = TRUE)) +
  geom_point(aes(y = `PM2.5 [mg/m3]` * 1000 , color = "PM2.5 dusttrak", na.rm = TRUE)) +
  geom_point(aes(y = SPS30_PM2.5, color = "SPS30 PM2.5", na.rm = TRUE)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


source("scripts/read_data_20250523.R")
source("scripts/read_sensor_data.R")

plotting <- APS_number_concentration_20250523cooking |>
  full_join(dusttrak_20250523cooking, by = "timestamp_rounded_5min" ) |>
  # full_join(dusttrak_20250521candles, by = "timestamp_rounded_5min", suffix = c(".x", ".y")) |>
  select(timestamp_rounded_5min, PM2.5_APS_only, PM2.5_APS_valid, PM1_APS_only, PM1_APS_valid, `PM2.5 [mg/m3]`)


plotting |> ggplot(aes(x = timestamp_rounded_5min)) +
  geom_line(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS", na.rm = TRUE)) +
  geom_line(aes(y = PM1_APS_valid * 1000,  color = "PM1 APS", na.rm = TRUE)) +
  geom_point(aes(y = `PM2.5 [mg/m3]` *1000 , color = "PM2.5 dusttrak", na.rm = TRUE)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


plotting <- plotting |> left_join(all_data_LCS, by = "timestamp_rounded_5min")
plotting |>
  filter(sensor != 80 & sensor != 201) |>
  ggplot(aes(x = timestamp_rounded_5min)) +
  geom_point(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS valid", na.rm = TRUE)) +
  geom_point(aes(y = PM2.5_APS_only * 1000, color = "PM2.5 APS", na.rm = TRUE)) +
  geom_point(aes(y = PM1_APS_only * 1000,  color = "PM1 APS", na.rm = TRUE)) +
  geom_point(aes(y = `PM2.5 [mg/m3]` * 1000 , color = "PM2.5 dusttrak", na.rm = TRUE)) +
  geom_point(aes(y = SPS30_PM2.5, color = "SPS30 PM2.5", na.rm = TRUE)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


source("scripts/read_data_20250526.R")
source("scripts/read_sensor_data.R")

plotting <- APS_number_concentration_20250526candles_high_RH |>
  full_join(dusttrak_20250526candles_high_RH, by = "timestamp_rounded_5min" ) |>
  # full_join(dusttrak_20250521candles, by = "timestamp_rounded_5min", suffix = c(".x", ".y")) |>
  select(timestamp_rounded_5min, PM2.5_APS_only, PM2.5_APS_valid, PM1_APS_only, PM1_APS_valid, `PM2.5 [mg/m3]`)


plotting |> ggplot(aes(x = timestamp_rounded_5min)) +
  geom_line(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS", na.rm = TRUE)) +
  geom_line(aes(y = PM1_APS_valid * 1000,  color = "PM1 APS", na.rm = TRUE)) +
  geom_point(aes(y = `PM2.5 [mg/m3]` *1000 , color = "PM2.5 dusttrak", na.rm = TRUE)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


plotting <- plotting |> left_join(all_data_LCS, by = "timestamp_rounded_5min")
plotting |>
  filter(sensor != 80 & sensor != 201) |>
  ggplot(aes(x = timestamp_rounded_5min)) +
  geom_point(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS valid")) +
  geom_point(aes(y = PM2.5_APS_only * 1000, color = "PM2.5 APS")) +
  geom_point(aes(y = PM1_APS_only * 1000,  color = "PM1 APS")) +
  geom_point(aes(y = `PM2.5 [mg/m3]` * 1000 , color = "PM2.5 dusttrak")) +
  geom_point(aes(y = SPS30_PM2.5, color = "SPS30 PM2.5")) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


source("scripts/read_data_20250528.R")
source("scripts/read_sensor_data.R")

plotting <- APS_number_concentration_20250528cooking_high_RH |>
  full_join(dusttrak_20250528cooking_high_RH, by = "timestamp_rounded_5min" ) |>
  # full_join(dusttrak_20250521candles, by = "timestamp_rounded_5min", suffix = c(".x", ".y")) |>
  select(timestamp_rounded_5min, PM2.5_APS_only, PM2.5_APS_valid, PM1_APS_only, PM1_APS_valid, `PM2.5 [mg/m3]`)


plotting |> ggplot(aes(x = timestamp_rounded_5min)) +
  geom_line(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS")) +
  geom_line(aes(y = PM1_APS_valid * 1000,  color = "PM1 APS")) +
  geom_point(aes(y = `PM2.5 [mg/m3]` *1000 , color = "PM2.5 dusttrak")) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")


plotting <- plotting |> left_join(all_data_LCS, by = "timestamp_rounded_5min")
plotting |>
  filter(sensor != 80 & sensor != 201) |>
  ggplot(aes(x = timestamp_rounded_5min)) +
  geom_line(aes(y = PM2.5_APS_valid * 1000, color = "PM2.5 APS valid")) +
  geom_line(aes(y = PM2.5_APS_only * 1000, color = "PM2.5 APS")) +
  geom_line(aes(y = PM1_APS_only * 1000,  color = "PM1 APS")) +
  geom_line(aes(y = `PM2.5 [mg/m3]` * 1000 , color = "PM2.5 dusttrak")) +
  geom_line(aes(y = SPS30_PM2.5, color = sensor)) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  # facet_wrap(~sensor) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")
