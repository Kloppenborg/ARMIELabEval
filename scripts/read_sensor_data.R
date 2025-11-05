# Load libraries
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)
library(labelled)
# Extract ID from file name

folder_path <- "lab_test/data/sensor"

extract_id <- function(file_name) {
  base_name <- basename(file_name)
  strsplit(base_name, "_")[[1]][1]
}

# Read all CSV files and add the "id" column with error handling
data <- list.files(folder_path, full.names = TRUE, pattern = "\\.csv$") |>
  map_dfr(~ {
    id <- extract_id(.x)
    tryCatch(
      {
        read.csv(
          .x,
          sep = "\t",
          dec = ",",
          header = TRUE,
          fill = TRUE,
          check.names = FALSE
        ) |>
          mutate(id = id)
      },
      error = function(e) {
        warning(paste("Error reading file:", .x, "->", e$message))
        NULL
      }
    )
  })

if (nrow(data) > 0) {
  data <- data |>
    mutate(timestamp = dmy_hms(`Time Stamp`) + hours(2),  # Adjust for timezone if necessary
           timestamp_rounded_1min = floor_date(timestamp, unit = "1 minute"),
           timestamp_rounded_5min = floor_date(timestamp, unit = "5 minutes")) |>
    arrange(id, timestamp) |>
    group_by(id) |>
    filter(SPS30_PM2.5_last != 0) |>
    ungroup()

} else {
  warning("No valid data was read from the files.")
}

sensor_information <- data.frame(
  sensor = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  sensor_id = c(202, 196, 180, 93, 199, 203, 176, 145, 170, 144),
  id = c("871d2d30-a27e-11ef-8da9-895b2cf73cdc",
         "4c976ee0-a0cc-11ef-bd03-85089a85334f",
         "2035e790-a0f5-11ef-bd03-85089a85334f",
         "0c8aad90-bac6-11ee-8811-2f44da1755df",
         "519b5b80-a0dc-11ef-bd03-85089a85334f",
         "d7a4d400-a031-11ef-8979-a34d9d7f8ad2",
         "1b6fa670-a275-11ef-8da9-895b2cf73cdc",
         "e8f34110-0167-11ef-aa62-fbe9cd8c1053",
         "2665dc90-030e-11ef-930f-f5126f9179a7",
         "e8f53ce0-0167-11ef-aa62-fbe9cd8c1053")
)

all_data_LCS <- data |>
  left_join(sensor_information) |>
  select(timestamp_rounded_5min, timestamp, SPS30_PM1, SPS30_PM2.5, sensor, id, sensor, everything())
