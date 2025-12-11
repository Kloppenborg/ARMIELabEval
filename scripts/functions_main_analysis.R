# sps30_eval_functions.R
# Utilities to construct reference PM2.5, fit SPS30 calibration models,
# and evaluate performance with leave-one-group-out validation.
# Author: <you>
# Date: <today>

suppressPackageStartupMessages({
  library(dplyr)
  library(rlang)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(broom)
})

load_and_bind_rda <- function(filenames, folder = "clean_data") {
  all_data <- lapply(filenames, function(file) {
    file_path <- here(folder, file)

    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
    }

    loaded_name <- load(file_path)
    get(loaded_name)
  }) |>
    bind_rows()

  return(all_data)
}

# -----------------------------
# Reference construction
# -----------------------------

#' Construct reference PM2.5 (µg/m³) from SMPS and APS
#'
#' PM2.5_ref = PM1_SMPS + PM2.5_APS_valid
#' Rationale: SMPS covers submicron up to ~0.6 µm; APS PM1/PM2.5 here refers to bins > 0.8 µm.
#' No overlap to subtract.
#' All inputs must already be in µg/m³.
#'
#' @param df tibble with PM1_SMPS and PM2.5_APS_valid
#' @param ref_col name for the output reference column (default: "PM2.5_ref")
#' @return tibble with new reference column
make_reference_pm25 <- function(df, ref_col = "PM2.5_ref", ref_measurements, reference) {
  df |>
    mutate(
      "{ref_col}" := {{ref_measurements}},
        reference = reference
    )
}

# -----------------------------
# Data preparation / QC
# -----------------------------

#' Prepare dataset for modeling (original scale, no log)
#'
#' - Keeps rows with non-missing SPS30 PM2.5, reference, and RH
#' - Floors negatives at zero
#'
#' @param df tibble with SPS30_PM2.5, ref_col, chamber_RH, sensor, source
#' @param ref_col name of reference column (must exist)
#' @return cleaned tibble
prepare_for_modeling <- function(df, ref_col = "PM2.5_ref") {
  req_cols <- c("SPS30_PM2.5", ref_col, "chamber_RH", "sensor", "source")
  stopifnot(all(req_cols %in% names(df)))

  df %>%
    filter(
      !is.na(.data[[ref_col]]),
      !is.na(SPS30_PM2.5),
      !is.na(chamber_RH),
      !is.na(sensor),
      !is.na(source)
    ) %>%
    mutate(
      "{ref_col}" := pmax(.data[[ref_col]], 0),
      SPS30_PM2.5 = pmax(SPS30_PM2.5, 0)
    )
}

# -----------------------------
# Modeling (no random effects)
# -----------------------------

#' Fit or wrap predictor for calibration
#'
#' Method = "lm": fits PM2.5_ref ~ predictor + RH
#' Method = "raw": bypass model, just return predictor as-is
#'
#' @param train tibble prepared by prepare_for_modeling
#' @param ref_col name of reference column
#' @param predictor_col predictor column (default "SPS30_PM2.5")
#' @param method "lm" (linear regression) or "raw"
#' @return list(model, ref_col, predictor_col, method)
# keep your original fit_base_model as-is if you want
fit_base_model <- function(train, ref_col = "PM2.5_ref",
                           predictor_col = "SPS30_PM2.5") {
  f <- as.formula(paste(ref_col, "~", predictor_col, "* SHTC3_RH"))
  fit <- lm(f, data = train)
  list(
    model = fit,
    ref_col = ref_col,
    predictor_col = predictor_col,
    method = "lm"
  )
}

# new function: flexible base model
fit_flexible_model <- function(train,
                               ref_col = "PM2.5_ref",
                               predictor_col = "SPS30_PM2.5",
                               method = c("lm", "raw")) {
  method <- match.arg(method)

  if (method == "lm") {
    # just call the normal one
    fit_base_model(train, ref_col, predictor_col)
  } else {
    # fake "model" object for raw passthrough
    list(
      model = NULL,
      ref_col = ref_col,
      predictor_col = predictor_col,
      method = "raw"
    )
  }
}

#' Predict calibrated PM2.5 on original scale
#'
#' @param model_obj output of fit_base_model
#' @param newdata tibble prepared by prepare_for_modeling
#' @return numeric vector of predictions (µg/m³)
# predict_calibrated <- function(model_obj, newdata) {
#   pmax(predict(model_obj$model, newdata = newdata), 0)
# }

#' Predict calibrated (or raw) PM2.5
predict_calibrated <- function(model_obj, newdata) {
  if (model_obj$method == "lm") {
    p <- predict(model_obj$model, newdata = newdata)
  } else if (model_obj$method == "raw") {
    p <- newdata[[model_obj$predictor_col]]
  }
  pmax(p, 0)
}


# -----------------------------
# Metrics
# -----------------------------

#' Compute evaluation metrics (original scale only)
#'
#' @param df tibble containing reference and predictions
#' @param ref_col name of reference column
#' @param pred_col name of predictions column
#' @return tibble with metrics
compute_metrics <- function(df, ref_col = "PM2.5_ref", pred_col = "PM2.5_pred") {
  stopifnot(all(c(ref_col, pred_col) %in% names(df)))
  x <- df[[pred_col]]
  y <- df[[ref_col]]
  y_safe <- ifelse(y == 0.0000, 0.001, y) # To avoid division by zero in MPE and MAPE
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]

  err <- x - y_safe   #Error between prediction and observation
  abs_err <- abs(err) #Absolute Error
  # mae <- mean(abs(err)) #Mean Absolute Error
  rmse <- sqrt(mean(err^2)) #Root Mean Squared Error
  err_pct <- (err/y_safe)* 100  #Mean Percent Error
  mean_ref <- mean(y_safe)
  mean_sps30 <- mean(x)
  r <- cor(x, y_safe)    # Pearson correlation
  r2 <- r^2 # Coefficient of determination
  cv <- (sd(err)/mean(err)) # Coeficient of variation

  mean_error <- mean(err)
  loa_low <- mean_error - 1.96 * sd(err) # Lower Limit of Agreement
  loa_high <- mean_error + 1.96 * sd(err) # Upper Limit of Agreement

  summary_table <- tibble::tibble(
    n = length(x),
    R = r,
    R2 = r2,
    MAE = mean(abs_err),
    RMSE = rmse,
    cv,
    MeanError = mean_error,
    MeanErrorPct = mean(err_pct),
    mean_sps30 = mean_sps30,
    mean_ref = mean_ref,
    LoA_Low = loa_low,
    LoA_High = loa_high
  )

 return(summary_table
    )
}


# -----------------------------
# Cross-validation: leave-one-sensor-out
# -----------------------------
loso_by_sensor <- function(df, ref_col = "PM2.5_ref", method = "lm") {
  sensors <- sort(unique(df$sensor))
  results <- vector("list", length(sensors))
  preds_all <- tibble()

  for (hold in sensors) {
    train <- filter(df, sensor != hold)
    test  <- filter(df, sensor == hold)

    mod <- fit_flexible_model(train, ref_col = ref_col,
                              predictor_col = "SPS30_PM2.5",
                              method = method)
    method = mod$method

    test <- test %>%
      mutate(PM2.5_pred = predict_calibrated(mod, .))

    metrics <- compute_metrics(test, ref_col = ref_col,
                               pred_col = "PM2.5_pred") %>%
      mutate(holdout_sensor = hold,
             method = method) |>
      relocate(holdout_sensor)

    results[[hold]] <- metrics
    preds_all <- bind_rows(preds_all, test %>% mutate(holdout_sensor = hold,
                                                      method = method))
  }

  list(
    per_sensor = bind_rows(results),
    predictions = preds_all
  )
}

# -----------------------------
# Cross-validation: leave-one-source-out
# -----------------------------

loso_by_source <- function(df, ref_col = "PM2.5_ref", method = "lm") {
  sources <- sort(unique(df$source))
  results <- vector("list", length(sources))
  preds_all <- tibble()

  for (hold in sources) {
    train <- filter(df, source != hold)
    test  <- filter(df, source == hold)

    mod <- fit_flexible_model(train, ref_col = ref_col,
                              predictor_col = "SPS30_PM2.5",
                              method = method)

    method = mod$method

    test <- test %>%
      mutate(PM2.5_pred = predict_calibrated(mod, .))

    metrics <- compute_metrics(test, ref_col = ref_col, pred_col = "PM2.5_pred") %>%
      mutate(holdout_source = hold,
             method = method) |>
      relocate(holdout_source)

    results[[as.character(hold)]] <- metrics
    preds_all <- bind_rows(preds_all, test %>% mutate(holdout_source = hold,
                                                      method = method))
  }

  list(
    per_source = bind_rows(results),
    predictions = preds_all
  )
}

# -----------------------------
# Visualization helpers (original scale)
# -----------------------------

# Scatter plot on raw scale with 1:1 line
plot_scatter <- function(df, ref_col = "PM2.5_ref", pred_col = "PM2.5_pred",
                         title = "SPS30 vs Reference", facet_factor) {
  ggplot(df, aes(x = .data[[ref_col]], y = .data[[pred_col]])) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "red") +
    labs(
      x = "Reference PM2.5 (µg/m³)",
      y = "Calibrated SPS30 PM2.5 (µg/m³)",
      title = title
    ) +
    facet_wrap(vars(.data[[facet_factor]])) +
    theme_minimal()
}

# Residuals vs humidity
plot_residuals_vs_rh <- function(df, ref_col = "PM2.5_ref", pred_col = "PM2.5_pred",
                                 title = "Residuals vs RH", facet_factor) {
  ggplot(df, aes(x = SHTC3_RH, y = .data[[pred_col]] - .data[[ref_col]])) +
    geom_hline(yintercept = 0, linetype = 2, color = "red") +
    geom_point(alpha = 0.4) +
    labs(
      x = "Relative humidity (%)",
      y = "Prediction error (µg/m³)",
      title = title
    ) +
    facet_wrap(vars(.data[[facet_factor]])) +
    theme_minimal()
}

# Bland–Altman plot on original scale
plot_bland_altman <- function(df, ref_col = "PM2.5_ref", pred_col = "PM2.5_pred",
                              title = "Bland–Altman (original scale)", facet_factor) {
  d <- df %>%
    mutate(
      ref_val =  .data[[ref_col]],
      diff_val = .data[[pred_col]] - .data[[ref_col]]
    )

  m <- mean(d$diff_val, na.rm = TRUE)
  s <- sd(d$diff_val, na.rm = TRUE)

  ggplot(d, aes(x = ref_val, y = diff_val)) +
    geom_hline(yintercept = 0, linetype = 2, color = "red") +
    geom_hline(yintercept = m, linetype = 1) +
    geom_hline(yintercept = m + 1.96 * s, linetype = 3) +
    geom_hline(yintercept = m - 1.96 * s, linetype = 3) +
    geom_point(alpha = 0.4) +
    labs(
      x = "Reference PM2.5 (µg/m³)",
      y = "Prediction − Reference (µg/m³)",
      title = title
    ) +
    facet_wrap(vars(.data[[facet_factor]])) +
    theme_minimal()
}
