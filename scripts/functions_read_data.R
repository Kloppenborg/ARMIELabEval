# Load libraries
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)
library(labelled)
library(Hmisc)

# Labeling functions for each dataset ######
label_dusttrak <- function(df) {
  var_label(df$elapsed_time_sec) <- "Elapsed time since measurement start [s]"
  var_label(df$timestamp_dusttrak) <- "Measurement timestamp (UTC)"
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"
  #var_label(df$`PM2.5 (µg/m³)`) <- "Dusttrak PM2.5 mass concentration [µg/m³]"
  df
}

label_ops <- function(df) {
  var_label(df$elapsed_time_sec) <- "Elapsed time since measurement start [s]"
  var_label(df$timestamp_OPS) <- "Measurement timestamp (UTC)"
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"
  df
}

label_smps_mass <- function(df) {
  var_label(df$timestamp) <- "Measurement timestamp (UTC)"
  var_label(df$instrument) <- "Instrument name"
  var_label(df$PM1_SMPS) <- "PM1 mass concentration 0. - 0.66 µm [µg/m³]"
  var_label(df$PM1_SMPS_SPS30) <- "PM1 mass concentration 0.3 0.66 µm [µg/m³] corresponds to lower limit of SPS30"
  var_label(df$PM25_SMPS) <- "PM2.5 mass concentration [µg/m³] (NA for SMPS)"
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"
  df
}

label_aps_mass <- function(df) {
  var_label(df$timestamp) <- "Measurement timestamp (UTC)"
  var_label(df$instrument) <- "Instrument name"
  var_label(df$PM1_APS) <- "PM1 mass concentration 0.66–1.0 µm [µg/m³]"
  var_label(df$PM1_APS_only) <- "PM1 mass concentration from APS only [µg/m³]"
  var_label(df$PM25_APS) <- "PM2.5 mass concentration [µg/m³]"
  var_label(df$PM25_APS_only) <- "PM2.5 mass concentration from APS only [µg/m³]"
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"
  df
}

label_smps_number <- function(df) {
  var_label(df$timestamp) <- "Measurement timestamp (UTC)"
  var_label(df$instrument) <- "Instrument name"
  var_label(df$PN05_SMPS) <- "Particle number concentration ≤ 500 nm [#/cm³]"
  var_label(df$PN1_SMPS) <- "Particle number concentration 0. - 0.66  [#/cm³]"
  var_label(df$PN25_SMPS) <- "PN2.5 not available for SMPS"
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"
  df
}

label_aps_number <- function(df) {
  var_label(df$timestamp) <- "Measurement timestamp (UTC)"
  var_label(df$instrument) <- "Instrument name"

  # Particle Number Concentrations (PN)
  var_label(df$PN1_APS) <- "PN1 (APS, >0.66–1.0 µm, #/cm³)"
  var_label(df$PN1_APS_only) <- "PN1 only (APS, ≤1.0 µm, #/cm³, includes unvalidated bins)"
  var_label(df$PN1_APS_valid) <- "PN1 valid (APS, >0.8–1.0 µm, #/cm³, validated only)"

  # Particle Mass Concentrations (PM)
  var_label(df$PM25_APS) <- "PM2.5 (APS, >0.66–2.5 µm, µg/m³)"
  var_label(df$PM25_APS_only) <- "PM2.5 only (APS, ≤2.5 µm, µg/m³, includes unvalidated bins)"
  var_label(df$PM25_APS_valid) <- "PM2.5 valid (APS, >0.8–2.5 mm, µg/m³, validated only)"

  # Environment
  var_label(df$chamber_RH) <- "Chamber relative humidity [%]"
  var_label(df$chamber_temp) <- "Chamber temperature [°C]"

  df
}



########################
### TSI Dusttrak #######
########################

#' Read and process TSI DustTrak data
#'
#' This function reads a DustTrak CSV file, extracts the start date and time from
#' the metadata lines, calculates timestamps for each measurement, and appends
#' chamber environmental data. Variable labels are applied for clarity.
#'
#' @param reference_dusttrak Path to the DustTrak CSV file.
#' @param chamber_RH Numeric value for chamber relative humidity (%).
#' @param chamber_temp Numeric value for chamber temperature (°C).
#'
#' @return A tibble containing DustTrak measurements with calculated timestamps,
#'         chamber conditions, and labeled variables.
#'
#' @details
#' The function expects the DustTrak file to have:
#' \itemize{
#'   \item Metadata lines with test start time at line 8 and test start date at line 9.
#'   \item Measurement data starting after line 35.
#'   \item First column containing elapsed time in seconds.
#' }
#'
#' @examples
#' \dontrun{
#' dusttrak_data <- read_dusttrak_data("path/to/file.csv", 40, 20)
#' }
#'
#' @seealso \code{\link{label_dusttrak}} for variable labeling.
read_dusttrak_data <- function(reference_dusttrak) {
  raw_lines <- read_lines(reference_dusttrak)
  test_start_time <- str_split(raw_lines[7], ",")[[1]][2]
  test_start_date <- str_split(raw_lines[8], ",")[[1]][2]
  start_datetime <- dmy_hms(paste(test_start_date, test_start_time))

  DustTrak_data <- read_csv(reference_dusttrak, skip = 35, col_types = cols(), show_col_types = FALSE)
  names(DustTrak_data)[1] <- "elapsed_time_sec"

  DustTrak_data <- DustTrak_data |>
    mutate(
      timestamp_dusttrak = start_datetime + seconds(elapsed_time_sec),
      timestamp_rounded_1min = floor_date(timestamp_dusttrak, unit = "1 minute"),
      timestamp_rounded_5min = floor_date(timestamp_dusttrak, unit = "5 minutes"),
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp,
      source = source,
      `PM2.5 [mg/m3]` <- ifelse(`PM2.5 [mg/m3]` == 0.0000, 0.001, `PM2.5 [mg/m3]`)
    ) |>
    label_dusttrak()

  return(DustTrak_data)
}

###############
### TSI OPS ###
###############

#' Read and process TSI OPS data
#'
#' This function reads an OPS CSV file, extracts the start date and time from
#' metadata lines, calculates timestamps for each measurement, and appends
#' chamber environmental data. Variable labels are applied for clarity.
#'
#' @param reference_OPS Path to the OPS CSV file.
#' @param chamber_RH Numeric value for chamber relative humidity (%).
#' @param chamber_temp Numeric value for chamber temperature (°C).
#'
#' @return A tibble containing OPS measurements with calculated timestamps,
#'         chamber conditions, and labeled variables.
#'
#' @details
#' The function expects the OPS file to have:
#' \itemize{
#'   \item Metadata lines with test start time at line 8 and test start date at line 9.
#'   \item Measurement data starting after line 37.
#'   \item First column containing elapsed time in seconds.
#' }
#'
#' @examples
#' \dontrun{
#' ops_data <- read_ops_data("path/to/file.csv", 40, 20)
#' }
#'
#' @seealso \code{\link{label_ops}} for variable labeling.
read_ops_data <- function(reference_OPS) {

  # Read all lines from the OPS file
  raw_lines <- read_lines(reference_OPS)

  # Extract start time and date from the header
  test_start_time <- str_split(raw_lines[7], ",")[[1]][2]
  test_start_date <- str_split(raw_lines[8], ",")[[1]][2]
  start_datetime <- ymd_hms(paste(test_start_date, test_start_time))

  # Extract bin cut points (lines 17 to 33 → Bin 1 to Bin 17)
  bin_lines <- raw_lines[12:(12+16)]
  bin_cut_points <- sapply(bin_lines, function(x) {
    as.numeric(str_trim(str_split(x, ",")[[1]][2]))
  })

  # Read OPS data table
  OPS_data <- read_csv(reference_OPS, skip = 37, col_types = cols(), show_col_types = FALSE)
  names(OPS_data)[1] <- "elapsed_time_sec"

  # Rename bin columns to include cut point (e.g., Bin1_0.300um)
  bin_names <- paste0("Bin", 1:16, "_", bin_cut_points, "um")
  names(OPS_data)[2:(1 + length(bin_cut_points))] <- bin_names

  # Store cut points as an attribute
  attr(OPS_data, "bin_cut_points") <- bin_cut_points

  # Add metadata columns and apply labels
  OPS_data <- OPS_data |>
    mutate(
      timestamp_OPS = start_datetime + seconds(elapsed_time_sec),
      timestamp_rounded_1min = floor_date(timestamp_OPS, unit = "1 minute"),
      timestamp_rounded_5min = floor_date(timestamp_OPS, unit = "5 minutes"),
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp,
      source = source
    ) |>
    label_ops()

  return(OPS_data)
}


##################
### SMPS Mass ####
##################

#' Read and process SMPS mass concentration data
#'
#' This function reads raw SMPS (Scanning Mobility Particle Sizer) mass concentration
#' data files, extracts timestamp information, converts particle size bin columns to
#' numeric, and calculates PM1 and PM2.5 mass concentrations. The function also
#' determines whether the PM1 range is truncated (i.e., bins do not cover up to 1000 nm),
#' appends chamber environmental data, and applies descriptive variable labels.
#'
#' @param file_path Character string specifying the path to the SMPS raw data file.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads the file with UTF-8 encoding, falling back to Latin-1 if necessary.
#'   \item Identifies the header line starting with `"Sample #"`.
#'   \item Cleans column names by replacing commas with dots and trimming whitespace.
#'   \item Converts particle size bin columns (matching `"^\\d+\\.\\d+$"`) to numeric.
#'   \item Calculates:
#'     \itemize{
#'       \item \code{PM1_SMPS}: Sum of bins with diameters ≤ 1000 nm, converted to µg/m³.
#'       \item \code{PM2.5_SMPS}: Set to \code{NA} for SMPS (not measured).
#'     }
#'   \item Adds instrument name and chamber environmental conditions.
#'   \item Selects key variables first, followed by all original columns.
#' }
#'
#' @return A tibble containing SMPS mass concentration data with added metadata and labels.
#'
#' @examples
#' \dontrun{
#' smps_mass <- read_smps_data("data/20250521_mass_concentration_SMPS.TXT")
#' }
#'
#' @importFrom readr read_lines read_delim cols locale
#' @importFrom stringr str_detect str_split str_replace_all str_trim
#' @importFrom dplyr mutate across rowwise ungroup select everything
#' @importFrom lubridate dmy_hms
#' @importFrom labelled var_label
read_smps_data <- function(file_path) {
  lines <- tryCatch(read_lines(file_path, locale = locale(encoding = "UTF-8")),
                    error = function(e) read_lines(file_path, locale = locale(encoding = "latin1")))
  header_line <- which(str_detect(lines, "^Sample #"))
  if (length(header_line) == 0) stop("Could not locate header line in SMPS file.")
  col_names <- str_split(lines[header_line], ";")[[1]] |>
    str_replace_all(",", ".") |>
    str_trim()

  smps_data <- read_delim(
    file_path,
    delim = ";",
    skip = header_line,
    col_names = col_names,
    locale = locale(decimal_mark = ".", grouping_mark = ","),
    col_types = cols(.default = "c")
  )

  smps_data <- smps_data |>
    mutate(
      timestamp = dmy_hms(paste(Date, `Start Time`)),
      timestamp_smps = timestamp + minutes(7),  # Adjust for 7 minutes the clock was behind on the SMPS laptop
      timestamp_rounded_1min = floor_date(timestamp, unit = "1 minute"),
      timestamp_rounded_5min = floor_date(timestamp, unit = "5 minutes"),
      across(matches("^\\d+\\.\\d+$"), as.numeric)
    )

  diam_cols <- names(smps_data)[str_detect(names(smps_data), "^\\d+\\.\\d+$")]
  diameters <- as.numeric(diam_cols)

  # Use lead-based log difference (delete last)
  diameter_logdp <- log10(lead(diameters)) - log10(diameters)
  valid_idx <- which(!is.na(diameter_logdp))
  diameter_logdp <- diameter_logdp[valid_idx]
  matched_cols <- diam_cols[valid_idx]

  mass_cols <- purrr::map2(
    smps_data[matched_cols],
    abs(diameter_logdp), # ensure all log-diffs are positive
    ~ .x * .y
  ) |>
    bind_cols()

  diameters_names <- diameters[valid_idx] / 1000  # Convert from nm to µm

  names(mass_cols) <- paste0("bin_", diameters_names, "_mass_conc")

  mass_cols <- mass_cols |>
    tibble() |>
    rowwise() |>
    mutate(
      total_mass_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |> ungroup()

  smps_data <- bind_cols(smps_data, mass_cols)

  # Use mass columns to compute PM1
  mass_pm1_cols <- paste0("bin_", diameters_names[diameters_names <= 1], "_mass_conc")
  mass_pm1_cols <- mass_pm1_cols[mass_pm1_cols %in% names(mass_cols)]
  mass_pm1_cols_SPS30 <- paste0("bin_", diameters_names[diameters_names > 0.3 & diameters_names <= 1], "_mass_conc")
  mass_pm1_cols_SPS30 <- mass_pm1_cols_SPS30[mass_pm1_cols_SPS30 %in% names(mass_cols)]

  smps_data <- smps_data |>
    rowwise() |>
    mutate(
      PM1_SMPS = sum(c_across(all_of(mass_pm1_cols)), na.rm = TRUE),
      PM1_SMPS_SPS30 = sum(c_across(all_of(mass_pm1_cols_SPS30)), na.rm = TRUE),
      PM25_SMPS = NA_real_
    ) |>
    ungroup() |>
    mutate(
      instrument = "SMPS",
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp,
      source = source) |>
    select(
      timestamp_smps, timestamp_rounded_1min, timestamp_rounded_5min,
      instrument, source, PM1_SMPS, total_mass_calculated, PM1_SMPS_SPS30,
      PM25_SMPS, chamber_RH, chamber_temp,
      `Total Conc. (�g/m�)` ,
      everything()
    )|>
    label_smps_mass()

  return(smps_data)
}


################
### APS Mass ###
################

#' Read and process APS mass concentration data
#'
#' This function reads raw APS (Aerodynamic Particle Sizer) mass concentration
#' data files, cleans problematic column names containing micro symbols (`µ`),
#' converts particle size bin columns to numeric, and calculates PM1 and PM2.5
#' mass concentrations using both hybrid (APS + SMPS) and APS-only methods.
#' The function also appends instrument metadata and chamber environmental data.
#'
#' @param file_path Character string specifying the path to the APS raw data file.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads the file with UTF-8 encoding, falling back to Latin-1 if necessary.
#'   \item Locates the header line starting with `"Sample #"` and cleans column names:
#'         \itemize{
#'           \item Trims whitespace.
#'           \item Normalizes Unicode (NFKC) to handle composed/decomposed forms.
#'           \item Replaces both MICRO SIGN (`U+00B5`) and GREEK MU (`U+03BC`) with `(um)`.
#'         }
#'   \item Converts particle size bin columns (matching `"^\\d+\\.\\d+$"`) to numeric.
#'   \item Calculates:
#'         \itemize{
#'           \item \code{PM1_APS}: Sum of APS bins between 0.66 µm and 1.0 µm
#'                 (assuming bins below 0.66 µm are measured by SMPS).
#'           \item \code{PM1_APS_only}: Sum of APS bins ≤ 1.0 µm (APS data only).
#'           \item \code{PM2.5_APS}: Sum of APS bins between 0.66 µm and 2.5 µm
#'                 (excluding SMPS-covered range).
#'           \item \code{PM2.5_APS_only}: Sum of APS bins ≤ 2.5 µm (APS data only).
#'         }
#'   \item Adds instrument identifier, chamber Relative Humidity, and chamber temperature.
#'   \item Reorders columns so metadata appears first.
#' }
#'
#' @return A tibble containing APS mass concentration data with calculated PM metrics
#'         and cleaned column names.
#'
#' @examples
#' \dontrun{
#' aps_mass <- read_aps_data("data/20250521_mass_concentration_APS.TXT")
#' }
#'
#' @importFrom readr read_lines read_delim cols locale
#' @importFrom stringr str_detect str_split str_trim str_replace_all
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr mutate across rowwise ungroup select everything
#' @importFrom lubridate mdy_hms
#' @importFrom labelled var_label
read_aps_data <- function(file_path) {
  enc <- "UTF-8"
  lines <- tryCatch(read_lines(file_path, locale = locale(encoding = "UTF-8")),
                    error = function(e) read_lines(file_path, locale = locale(encoding = "latin1")))

  header_line <- which(str_detect(lines, "^Sample #"))
  if (length(header_line) == 0) stop("Could not locate header line in APS file.")

  col_names <- str_split(lines[header_line], aps_seperator)[[1]] |>
    str_trim() |>
    stringi::stri_trans_general("NFKC") |>
    str_replace_all("\\((?:\u00B5|\u03BC)m\\)", "(um)")

  aps_data <- read_delim(
    file_path,
    delim = aps_seperator,
    skip = header_line,
    col_names = col_names,
    locale = locale(encoding = enc, decimal_mark = ".", grouping_mark = ","),
    col_types = cols(.default = "c")
  )

  aps_data <- aps_data |>
    mutate(
      timestamp = paste(Date, `Start Time`),
      timestamp_aps = mdy_hms(timestamp),
      timestamp_rounded_1min = floor_date(timestamp_aps, unit = "1 minute"),
      timestamp_rounded_5min = floor_date(timestamp_aps, unit = "5 minutes"),
      across(matches("^\\d+\\.\\d+$"), as.numeric),
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp
    )

  diam_cols <- names(aps_data)[str_detect(names(aps_data), "^\\d+\\.\\d+$")]
  diameters <- as.numeric(diam_cols)
  mass_cols <- aps_data[diam_cols] * 1000 # Convert from mg/m³ to µg/m³

  names(mass_cols) <- paste0("bin_", diam_cols, "_mass_conc")

  mass_cols <- mass_cols |>
    tibble() |>
    rowwise() |>
    mutate(
      total_mass_conc_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |> ungroup()

  aps_data <- bind_cols(aps_data, mass_cols)

  pm1_cols <- diam_cols[diameters > 0.66 & diameters <= 1.0]
  pm1_APS_cols <- diam_cols[diameters <= 1.0]
  pm25_cols <- diam_cols[diameters > 0.66 & diameters <= 2.5]
  pm25_APS_cols <- diam_cols[diameters <= 2.5]

  aps_data <- aps_data |>
    rowwise() |>
    mutate(
      PM1_APS      = sum(c_across(all_of(paste0("bin_", pm1_cols,      "_mass_conc"))) * 1000, na.rm = TRUE),
      PM1_APS_only = sum(c_across(all_of(paste0("bin_", pm1_APS_cols,  "_mass_conc"))) * 1000, na.rm = TRUE),
      PM25_APS    = sum(c_across(all_of(paste0("bin_", pm25_cols,     "_mass_conc"))) * 1000, na.rm = TRUE),
      PM25_APS_only = sum(c_across(all_of(paste0("bin_", pm25_APS_cols,"_mass_conc"))) * 1000, na.rm = TRUE),
      source = source
    ) |>
    ungroup() |>
    mutate(instrument = "APS") |>
    select(timestamp_aps, timestamp_rounded_1min, timestamp_rounded_5min, instrument, source, PM1_APS, PM1_APS_only, PM25_APS, PM25_APS_only,
           chamber_RH, chamber_temp, source, everything()) |>
    label_aps_mass()

  return(aps_data)
}




###################################
### SMPS number concentrations ####
###################################

#' Read and process SMPS number concentration data
#'
#' Reads raw SMPS (Scanning Mobility Particle Sizer) number concentration files,
#' converts particle size bin columns to numeric, and calculates integrated particle
#' number concentrations for PN0.5, PN1, and PN2.5 (where applicable).
#' Adds instrument metadata, chamber environmental data, and variable labels for clarity.
#'
#' @param file_path Character string specifying the path to the SMPS number concentration file.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads the file using UTF-8 encoding, falling back to Latin-1 if necessary.
#'   \item Locates the header line starting with `"Sample #"` and cleans column names:
#'         \itemize{
#'           \item Replaces commas with decimal points.
#'           \item Trims whitespace.
#'         }
#'   \item Converts timestamp from Date and Start Time columns to a \code{POSIXct} object.
#'   \item Identifies particle diameter bin columns (matching `"^\\d+\\.\\d+$"`) and converts them to numeric.
#'   \item Calculates:
#'         \itemize{
#'           \item \code{PN0.5_SMPS}: Sum of all bins ≤ 500 nm.
#'           \item \code{PN1_SMPS}: Sum of all bins ≤ 1000 nm.
#'           \item \code{PN2.5_SMPS}: Set to \code{NA_real_} (not measured by SMPS).
#'         }
#'   \item Adds instrument identifier, chamber Relative Humidity, and chamber temperature.
#'   \item Reorders columns so metadata and PN metrics appear first.
#'   \item Applies \code{label_smps_number()} to attach variable labels.
#' }
#'
#' @return A tibble containing SMPS number concentration data with calculated PN metrics,
#'         instrument metadata, and labeled variables.
#'
#' @examples
#' \dontrun{
#' smps_number <- read_smps_number_data("data/20250521_number_concentration_SMPS.TXT")
#' }
#'
#' @importFrom readr read_lines read_delim cols locale
#' @importFrom stringr str_detect str_split str_replace_all str_trim
#' @importFrom dplyr mutate across rowwise ungroup select everything
#' @importFrom lubridate dmy_hms
#' @importFrom labelled var_label
read_smps_number_data <- function(file_path) {
  lines <- tryCatch(
    read_lines(file_path, locale = locale(encoding = "UTF-8")),
    error = function(e) read_lines(file_path, locale = locale(encoding = "latin1"))
  )

  header_line <- which(str_detect(lines, "^Sample #"))
  if (length(header_line) == 0) stop("Could not locate header line in SMPS file.")

  col_names <- str_split(lines[header_line], ";")[[1]] |>
    str_replace_all(",", ".") |>
    str_trim()

  smps_data <- read_delim(
    file_path,
    delim = ";",
    skip = header_line,
    col_names = col_names,
    locale = locale(decimal_mark = ".", grouping_mark = ","),
    col_types = cols(.default = "c")
  )

  smps_data <- smps_data |>
    mutate(
      timestamp = dmy_hms(paste(Date, `Start Time`)),
      timestamp_smps = timestamp + minutes(7),  # Adjust for 7 minutes the clock was behind on the SMPS laptop
      timestamp_rounded_1min = floor_date(timestamp, unit = "1 minute"),
      timestamp_rounded_5min = floor_date(timestamp, unit = "5 minutes"),
      across(matches("^\\d+\\.\\d+$"), as.numeric)
    )

  diam_cols <- names(smps_data)[str_detect(names(smps_data), "^\\d+\\.\\d+$")]
  diameters <- as.numeric(diam_cols)

  # Use lead-based log difference (delete last)
  diameter_logdp <- log10(lead(diameters)) - log10(diameters)
  valid_idx <- which(!is.na(diameter_logdp))
  diameter_logdp <- diameter_logdp[valid_idx]
  matched_cols <- diam_cols[valid_idx]

  number_cols <- purrr::map2(
    smps_data[matched_cols],
    abs(diameter_logdp), # ensure all log-diffs are positive
    ~ .x * .y
  ) |>
    bind_cols()

  diameters_names <- diameters[valid_idx] / 1000  # Convert from nm to µm

  names(number_cols) <- paste0("bin_", diameters_names, "_#_conc")

  number_cols <- number_cols |>
    tibble() |>
    rowwise() |>
    mutate(
      total_number_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |> ungroup()

  smps_data <- bind_cols(smps_data, number_cols)

  max_bin <- max(diameters, na.rm = TRUE)

  # Use number columns to compute PN1
  pn0.5_cols <- paste0("bin_", diameters_names[diameters_names <=500], "_#_conc")
  pn0.5_cols <- pn0.5_cols[pn0.5_cols %in% names(number_cols)]
  pn1_cols <- paste0("bin_", diameters_names[diameters_names <= 1], "_#_conc")
  pn1_cols <- pn1_cols[pn1_cols %in% names(number_cols)]
  pn1_cols_SPS30 <- paste0("bin_", diameters_names[diameters_names > 0.3 & diameters_names <= 1], "_#_conc")
  pn1_cols_SPS30 <- pn1_cols_SPS30[pn1_cols_SPS30 %in% names(number_cols)]

  smps_data <- smps_data |>
    rowwise() |>
    mutate(
      PN05_SMPS = sum(c_across(all_of(pn0.5_cols)), na.rm = TRUE),
      PN1_SMPS = sum(c_across(all_of(pn1_cols)), na.rm = TRUE),
      PN1_SMPS_SPS30 = sum(c_across(all_of(pn1_cols_SPS30)), na.rm = TRUE),
      PN25_SMPS = NA_real_
    ) |>
    ungroup() |>
    mutate(
      instrument = "SMPS",
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp,
      source = source
    ) |>
    select(timestamp_smps, timestamp_rounded_1min, timestamp_rounded_5min, instrument, total_number_calculated, source,
           PN05_SMPS, PN1_SMPS, PN25_SMPS, chamber_RH, chamber_temp, everything(),
           `Total Conc. (#/cm�)`
    )|>
    label_smps_number()

  return(smps_data)
}


#################################
#### APS Number concentration ###
#################################

#' Read and process APS number concentration data
#'
#' Reads raw APS (Aerodynamic Particle Sizer) number concentration files,
#' standardizes column names, converts particle size bin columns to numeric,
#' and calculates integrated particle number concentrations for PN1 and PN2.5 ranges.
#' Returns the processed dataset with instrument metadata, chamber conditions,
#' and labeled variables for clarity.
#'
#' @param file_path Character string specifying the path to the APS number concentration file.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads the file with UTF-8 encoding, falling back to Latin-1 if necessary.
#'   \item Detects the header line starting with \code{"Sample #"}.
#'   \item Cleans and standardizes column names:
#'         \itemize{
#'           \item Trims whitespace.
#'           \item Normalizes Unicode characters to NFKC form.
#'           \item Replaces MICRO SIGN (\u00B5) and GREEK MU (\u03BC) with \code{"um"}.
#'         }
#'   \item Converts timestamp from \code{Date} and \code{Start Time} columns to \code{POSIXct}.
#'   \item Converts bin columns (matching \code{"^\\d+\\.\\d+$"}) to numeric.
#'   \item Identifies bin columns within:
#'         \itemize{
#'           \item \code{pn1_cols}: 0.66–1.0 µm (excluding bins ≤ 0.66 µm, measured by SMPS).
#'           \item \code{pn1_APS_cols}: ≤ 1.0 µm (APS-only).
#'           \item \code{pn2.5_cols}: 0.66–2.5 µm (excluding bins ≤ 0.66 µm).
#'           \item \code{pn2.5_APS_cols}: ≤ 2.5 µm (APS-only).
#'         }
#'   \item Calculates:
#'         \itemize{
#'           \item \code{PN1_APS}: Sum of 0.66–1.0 µm bins.
#'           \item \code{PN1_APS_only}: Sum of all bins ≤ 1.0 µm.
#'           \item \code{PN2.5_APS}: Sum of 0.66–2.5 µm bins.
#'           \item \code{PN2.5_APS_only}: Sum of all bins ≤ 2.5 µm.
#'         }
#'   \item Adds instrument name (\code{"APS"}), chamber relative humidity, and temperature.
#'   \item Reorders columns so PN metrics and metadata appear first.
#'   \item Applies \code{label_aps_number()} to attach variable labels.
#' }
#'
#' @return A tibble containing APS number concentration data with calculated PN metrics,
#'         instrument metadata, and labeled variables.
#'
#' @examples
#' \dontrun{
#' aps_number <- read_aps_number_data("data/20250521_number_concentration_APS.TXT")
#' }
#'
#' @importFrom readr read_lines read_delim cols locale
#' @importFrom stringr str_detect str_split str_replace_all str_trim
#' @importFrom lubridate mdy_hms
#' @importFrom dplyr mutate across rowwise ungroup select everything
#' @importFrom stringi stri_trans_general
#' @importFrom labelled var_label
read_aps_number_data <- function(file_path) {
  enc <- "UTF-8"
  lines <- tryCatch(
    read_lines(file_path, locale = locale(encoding = enc)),
    error = function(e) {
      enc <<- "latin1"
      read_lines(file_path, locale = locale(encoding = enc))
    }
  )

  header_line <- which(str_detect(lines, "^Sample #"))
  if (length(header_line) == 0) stop("Could not locate header line in APS file.")

  col_names <- str_split(lines[header_line], aps_seperator)[[1]] |>
    str_trim() |>
    stringi::stri_trans_general("NFKC") |>
    str_replace_all("\\((?:\u00B5|\u03BC)m\\)", "(um)")

  aps_data <- read_delim(
    file_path,
    delim = aps_seperator,
    skip = header_line,
    col_names = col_names,
    locale = locale(encoding = enc, decimal_mark = ".", grouping_mark = ","),
    col_types = cols(.default = "c")
  )

  aps_data <- aps_data |>
    rename("0.523" = "<0.523") |>
    mutate(
      timestamp = paste(Date, `Start Time`),
      timestamp_aps = lubridate::mdy_hms(timestamp),
      timestamp_aps = timestamp_aps + minutes(5),  # Adjust for 7 minutes the clock was behind on the SMPS laptop
      timestamp_rounded_1min = lubridate::floor_date(timestamp_aps, unit = "1 minute"),
      timestamp_rounded_5min = lubridate::floor_date(timestamp_aps, unit = "5 minutes")
    ) |>
    mutate(across(matches("^\\d+\\.\\d+$"), as.numeric))


  diam_cols <- names(aps_data)[str_detect(names(aps_data), "^\\d+\\.\\d+$")]
  diameters <- as.numeric(diam_cols)

  number_cols <- purrr::map(
    aps_data[diam_cols],
    ~ .x / (1000 / 60 * 10) # Calculates particle count/cm^3
  ) |>
    dplyr::bind_cols()

  names(number_cols) <- paste0("bin_", diam_cols, "_#_conc")

  number_cols <- number_cols |>
    tibble() |>
    rowwise() |>
    mutate(
      total_number_conc_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |> ungroup()

  mass_cols <- purrr::imap_dfc(
    aps_data[diam_cols],
    function(counts, diam) {
      d_um <- if (diam == "0.523") 0.3 else as.numeric(diam)  # µm
      d_m <- d_um * 1e-6                                        # convert to meters
      volume <- (pi * d_m^3) / 6                                # volume of a sphere
      mass <- volume * counts * 1000 * 1000000                  # Calculates the mass of the sphere with a density = 1
      return(mass)
    }
  )

  mass_cols <- purrr::map(
    mass_cols,
    ~ .x / (1000 / 60 * 10) * 1000000 # Calculates particle mass concentration in mg/m³
  ) |>
    dplyr::bind_cols()


  names(mass_cols) <- paste0("bin_", diam_cols, "_mass_conc")

  mass_cols <- mass_cols |>
    dplyr::mutate(across(everything(), as.numeric)) |>
    tibble() |>
    rowwise() |>
    mutate(
      total_mass_conc_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |>
    ungroup()


  aps_data <- bind_cols(aps_data, number_cols, mass_cols)

  max_bin <- max(diameters, na.rm = TRUE)

  pn1_cols <- diam_cols[diameters > 0.66 & diameters <= 1.0]
  pn1_APS_cols <- diam_cols[diameters <= 1.0]
  pn1_aps_cols_valid <- diam_cols[diameters > 0.8 & diameters <= 1.0]
  pn25_cols <- diam_cols[diameters > 0.66 & diameters <= 2.5]
  pn25_APS_cols <- diam_cols[diameters <= 2.5]
  pn25_aps_cols_valid <- diam_cols[diameters > 0.8 & diameters <= 2.5]


  pm1_cols <- diam_cols[diameters > 0.66 & diameters <= 1.0]
  pm1_APS_cols <- diam_cols[diameters <= 1.0]
  pm1_aps_cols_valid <- diam_cols[diameters > 0.8 & diameters <= 1.0]
  pm25_cols <- diam_cols[diameters > 0.66 & diameters <= 2.5]
  pm25_APS_cols <- diam_cols[diameters <= 2.5]
  pm25_aps_cols_valid <- diam_cols[diameters > 0.8 & diameters <= 2.5]


  aps_data <- aps_data |>
    rowwise() |>
    mutate(
      # PN: Particle Number Concentration (#/m³)
      # Includes particles with aerodynamic diameter > 0.66 µm and ≤ 1.0 µm
      PN1_APS = sum(c_across(all_of(paste0("bin_", pn1_cols, "_#_conc"))), na.rm = TRUE),
      # Includes all particles ≤ 1.0 µm (including below APS validation threshold)
      PN1_APS_only = sum(c_across(all_of(paste0("bin_", pn1_APS_cols, "_#_conc"))), na.rm = TRUE),
      # Includes only validated APS bins (particles > 0.8 µm & ≤ 1.0 µm)
      PN1_APS_valid = sum(c_across(all_of(paste0("bin_", pn1_aps_cols_valid, "_#_conc"))), na.rm = TRUE),
      # PN2.5: Particle Number Concentration for particles > 0.66 µm & ≤ 2.5 µm
      PN25_APS = sum(c_across(all_of(paste0("bin_", pn25_cols, "_#_conc"))), na.rm = TRUE),
      # All particles ≤ 2.5 µm, regardless of validation
      PN25_APS_only = sum(c_across(all_of(paste0("bin_", pn25_APS_cols, "_#_conc"))), na.rm = TRUE),
      # Validated only (particles > 0.8 µm & ≤ 2.5 µm)
      PN25_APS_valid = sum(c_across(all_of(paste0("bin_", pn25_aps_cols_valid, "_#_conc"))), na.rm = TRUE),

      # PM: Particle Mass Concentration (mg/m³)
      # PM1 for particles > 0.66 µm & ≤ 1.0 µm
      PM1_APS = sum(c_across(all_of(paste0("bin_", pm1_cols, "_mass_conc"))) * 1000, na.rm = TRUE),
      # All particles ≤ 1.0 µm
      PM1_APS_only = sum(c_across(all_of(paste0("bin_", pm1_APS_cols, "_mass_conc"))) * 1000, na.rm = TRUE),
      # Validated PM1 (particles > 0.8 µm & ≤ 1.0 µm)
      PM1_APS_valid = sum(c_across(all_of(paste0("bin_", pm1_aps_cols_valid, "_mass_conc"))) * 1000, na.rm = TRUE),
      # PM2.5 for particles > 0.66 µm & ≤ 2.5 µm
      PM25_APS = sum(c_across(all_of(paste0("bin_", pm25_cols, "_mass_conc"))) * 1000, na.rm = TRUE),
      # All particles ≤ 2.5 µm
      PM25_APS_only = sum(c_across(all_of(paste0("bin_", pm25_APS_cols, "_mass_conc"))) * 1000, na.rm = TRUE),
      # Validated PM2.5 (particles > 0.8 µm & ≤ 2.5 µm)
      PM25_APS_valid = sum(c_across(all_of(paste0("bin_", pm25_aps_cols_valid, "_mass_conc"))) * 1000, na.rm = TRUE),

      source = source
    ) |>
    ungroup() |>
    mutate(
      instrument = "APS",
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp,
      source = source
    ) |>
    select(
      timestamp_aps, timestamp_rounded_1min, timestamp_rounded_5min, source,
      instrument, PN1_APS, PN1_APS_only, PN1_APS_valid, PN25_APS, PN25_APS_only, PN25_APS_valid,
      chamber_RH, chamber_temp, everything()
    ) |>
    label_aps_number()


  return(aps_data)
}


###############################
### Merge and save all data ###

merge_and_save_all_data <- function(SMPS_data, APS_number_data, SMPS_number_data, DustTrak_data) {
  stopifnot(exists("source"), exists("date"), exists("output_folder"))

  output_name <- paste0(source, date)

  merged_data <- SMPS_data |>
    full_join(APS_number_data, by = "timestamp_rounded_1min", suffix = c("_SMPS_mass", "_APS_#")) |>
    full_join(SMPS_number_data, by = "timestamp_rounded_1min") |>
    left_join(DustTrak_data, by = "timestamp_rounded_1min") |>
    mutate(
      PM25_dusttrak = `PM2.5 [mg/m3]`,
      timestamp_rounded_5min = timestamp_rounded_5min.y,
      source = source.y,
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp
    ) |>
    select(
      timestamp_rounded_1min, timestamp_rounded_5min, source, chamber_RH, chamber_temp,
      PM1_SMPS, PM1_SMPS_SPS30,
      PM1_APS, PM1_APS_only, PM1_APS_valid,
      PM25_APS, PM25_APS_only, PM25_APS_valid,
      PM25_dusttrak,
      PN05_SMPS, PN1_SMPS, PN1_SMPS_SPS30,
      PN1_APS, PN1_APS_only, PN1_APS_valid,
      PN25_APS, PN25_APS_only, PN25_APS_valid,
      contains("_mass_conc"), matches("#_conc")
    )

  assign(output_name, merged_data, envir = .GlobalEnv)

  save(list = output_name, file = here::here("clean_data", paste0(output_name, ".rda")))
}
