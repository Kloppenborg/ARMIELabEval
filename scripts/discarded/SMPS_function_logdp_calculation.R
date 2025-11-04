  lines <- tryCatch(read_lines(reference_SMPS_mass, locale = locale(encoding = "UTF-8")),
                    error = function(e) read_lines(file_path, locale = locale(encoding = "latin1")))
  header_line <- which(str_detect(lines, "^Sample #"))
  if (length(header_line) == 0) stop("Could not locate header line in SMPS file.")
  col_names <- str_split(lines[header_line], ";")[[1]] |>
    str_replace_all(",", ".") |>
    str_trim()

  smps_data <- read_delim(
    reference_SMPS_mass,
    delim = ";",
    skip = header_line,
    col_names = col_names,
    locale = locale(decimal_mark = ".", grouping_mark = ","),
    col_types = cols(.default = "c")
  )

  smps_data <- smps_data |>
    mutate(
      timestamp = dmy_hms(paste(Date, `Start Time`)),
      timestamp = timestamp + minutes(4),  # Adjust for 4 minutes the clock was behind on the SMPS laptop
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

  names(mass_cols) <- paste0("mass_", matched_cols)

  mass_cols <- mass_cols |>
    tibble() |>
    rowwise() |>
    mutate(
      total_mass_calculated = sum(c_across(everything()), na.rm = TRUE)
    ) |> ungroup()

  smps_data <- bind_cols(smps_data, mass_cols)

  # Use mass columns to compute PM1
  mass_pm1_cols <- paste0("mass_", diam_cols[diameters <= 1000]) # Calculated as nanogram/cm^3 ?
  mass_pm1_cols <- mass_pm1_cols[mass_pm1_cols %in% names(mass_cols)]

  smps_data <- smps_data |>
    rowwise() |>
    mutate(
      PM1_SMPS = sum(c_across(all_of(mass_pm1_cols)), na.rm = TRUE),
      PM2.5_SMPS = NA_real_
    ) |>
    ungroup() |>
    mutate(
      instrument = "SMPS",
      chamber_RH = chamber_RH,
      chamber_temp = chamber_temp) |>
    select(
      timestamp, timestamp_rounded_1min, timestamp_rounded_5min,
      instrument, PM1_SMPS, total_mass_calculated, `Total Conc. (�g/m�)` ,PM2.5_SMPS, chamber_RH, chamber_temp,
      everything()
    )|>
    label_smps_mass()

  return(smps_data)

write_csv(SMPS_data, file = "output_SMPS_mass.csv")

