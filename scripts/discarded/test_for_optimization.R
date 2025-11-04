# Load libraries
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)

# Define date once (YYYYMMDD)
date <- c("20250521", "20250522", "20250523")
exposure <- c("candles", "clean_air", "cooking")

# Define paths using the specified date
folder_path <- paste0("lab_test/data/", date)

###OBS: Update name of reference "clean_air"
# Define paths using the specified date
folder_path <- paste0("lab_test/data/", date)
reference_dusttrak <- paste0(folder_path, "/reference/", date, " - ", exposure, "_dusttrak.csv")
reference_OPS <- paste0(folder_path, "/reference/", date, " - ", exposure, "_OPS.csv")
output_folder <- paste0("clean_data/", date)

# Ensure the output folder exists
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Function to extract ID from the filename
extract_id <- function(file_name) {
  base_name <- basename(file_name)
  strsplit(base_name, "_")[[1]][1]
}

# Read all CSV files and add the "id" column with error handling
data <- list.files(folder_path, full.names = TRUE, pattern = "\\.csv$") %>%
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
        ) %>%
          mutate(id = id)
      },
      error = function(e) {
        warning(paste("Error reading file:", .x, "->", e$message))
        NULL
      }
    )
  })

if (nrow(data) > 0) {
  data <- data %>%
    mutate(date = dmy_hms(`Time Stamp`)) %>% # Ensure correct column name here
    arrange(id, date) %>%
    group_by(id) %>%
    filter(SPS30_PM2.5_last != 0) %>%
    mutate(time_diff = as.numeric(difftime(date, lag(date), units = "mins"))) %>%
    ungroup()

  avg_result <- data %>%
    group_by(id) %>%
    summarize(
      avg_time_diff = mean(time_diff, na.rm = TRUE),
      total_observations = n(),
      .groups = "drop"
    )

  count_result <- data %>%
    filter(!is.na(time_diff) & time_diff > 7) %>%
    group_by(id) %>%
    summarize(
      count_over_7_min = n(),
      .groups = "drop"
    )

  final_result <- avg_result %>%
    left_join(count_result, by = "id")

  print(final_result)
} else {
  warning("No valid data was read from the files.")
}

armie_data <- data

# Save data
assign(paste0("armie_", date), armie_data)
save(list = paste0("armie_", date), file = paste0(output_folder, "/armie_", date, ".rda"))

##################
# DustTrak data #
##################

# Read all lines from the file
raw_lines <- read_lines(reference_file)

# Extract test start time and date from metadata lines
test_start_time <- str_split(raw_lines[7], ",")[[1]][2]
test_start_date <- str_split(raw_lines[8], ",")[[1]][2]

# Combine and convert to POSIXct timestamp
start_datetime <- dmy_hms(paste(test_start_date, test_start_time))

# Read measurement data starting from line 36
DustTrak_data <- read_csv(
  reference_file,
  skip = 35,
  col_types = cols(),
  show_col_types = FALSE
)

# Rename first column
names(DustTrak_data)[1] <- "elapsed_time_sec"

# Add timestamp
DustTrak_data <- DustTrak_data %>%
  mutate(timestamp = start_datetime + seconds(elapsed_time_sec))

# Save DustTrak data
assign(paste0("dusttrak_", date), DustTrak_data)
save(list = paste0("dusttrak_", date), file = paste0(output_folder, "/dusttrak_", date, ".rda"))


######### OPS #####

# Read all lines from the file
raw_lines <- read_lines(reference_OPS)

# Extract test start time and date from metadata lines
test_start_time <- str_split(raw_lines[7], ",")[[1]][2]
test_start_date <- str_split(raw_lines[8], ",")[[1]][2]

# Combine and convert to POSIXct timestamp
start_datetime <- ymd_hms(paste(test_start_date, test_start_time))

# Read measurement data starting from line 36
OPS_data <- read_csv(
  reference_OPS,
  skip = 37,
  col_types = cols(),
  show_col_types = FALSE
)

# Rename first column
names(OPS_data)[1] <- "elapsed_time_sec"

# Add timestamp
OPS_data <- OPS_data %>%
  mutate(timestamp = start_datetime + seconds(elapsed_time_sec))

# Save DustTrak data
assign(paste0("OPS_", date), OPS_data)
save(list = paste0("OPS_", date), file = paste0(output_folder, "/OPS_", date, ".rda"))

