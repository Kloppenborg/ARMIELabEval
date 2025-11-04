# --- Setup ---------------------------------------------------------------
library(dplyr)
library(tidyr)

# Assumes `all_data` exists and has:
# PM1_SMPS, `PM2.5_APS_valid`, SPS30_PM2.5, chamber_RH, sensor, source

# --- 1) Build the reference (µg/m³) -------------------------------------
# PM2.5_ref = PM1_SMPS + PM2.5_APS_valid  (no overlap under your definition)
all_data_ref <- all_data %>%
  mutate(`PM2.5_ref` = PM2.5_dusttrak * 1000)

# --- 2) Prepare data (original scale, no logs) ---------------------------
dat <- all_data_ref %>%
  filter(
    !is.na(`PM2.5_ref`),
    !is.na(SPS30_PM2.5),
    !is.na(chamber_RH),
    !is.na(sensor),
    !is.na(source)
  ) %>%
  mutate(
    `PM2.5_ref`   = pmax(`PM2.5_ref`, 0),
    SPS30_PM2.5   = pmax(SPS30_PM2.5, 0)
  )

# Quick sanity check
stopifnot(all(c("PM2.5_ref","SPS30_PM2.5","chamber_RH","sensor","source") %in% names(dat)))

# --- 3) Leave-one-sensor-out (LOSO) -------------------------------------
sensors <- sort(unique(dat$sensor))

per_sensor_metrics <- list()
predictions_all    <- list()

for (s in sensors) {
  # Split
  train <- dat %>% filter(sensor != s)
  test  <- dat %>% filter(sensor == s)

  # Fit on original scale with RH
  fit <- lm(`PM2.5_ref` ~ SPS30_PM2.5 + chamber_RH, data = train)

  # Predict on test and floor at 0
  test <- test %>%
    mutate(`PM2.5_pred` = pmax(predict(fit, newdata = test), 0),
           holdout_sensor = s)

  # --- Metrics (original scale only) ---
  x <- test$`PM2.5_pred`
  y <- test$`PM2.5_ref`

  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]

  err  <- x - y
  n    <- length(x)
  mae  <- mean(abs(err))
  rmse <- sqrt(mean(err^2))

  # Percent metrics: avoid division by zero
  pos  <- y > 0
  mpe  <- if (any(pos)) 100 * mean((x[pos] / y[pos] - 1)) else NA_real_
  mape <- if (any(pos)) 100 * mean(abs((x[pos] - y[pos]) / y[pos])) else NA_real_

  r  <- cor(x, y)  # Pearson correlation
  r2 <- if (is.finite(r)) r^2 else NA_real_

  per_sensor_metrics[[as.character(s)]] <- tibble::tibble(
    holdout_sensor = s,
    n = n,
    MAE = mae,
    RMSE = rmse,
    PercentBias = mpe,
    MAPE = mape,
    R = r,
    R2 = r2
  )

  predictions_all[[as.character(s)]] <- test
}

per_sensor <- dplyr::bind_rows(per_sensor_metrics) %>%
  arrange(holdout_sensor)

predictions <- dplyr::bind_rows(predictions_all)

# Peek at results
per_sensor
head(predictions)

# --- 4) Optional: overall metrics across all LOSO predictions ------------
# This summarizes performance when each sensor acted as the holdout.
x_all <- predictions$`PM2.5_pred`
y_all <- predictions$`PM2.5_ref`

keep_all <- is.finite(x_all) & is.finite(y_all)
x_all <- x_all[keep_all]; y_all <- y_all[keep_all]

err_all  <- x_all - y_all
n_all    <- length(x_all)
mae_all  <- mean(abs(err_all))
rmse_all <- sqrt(mean(err_all^2))

pos_all  <- y_all > 0
mpe_all  <- if (any(pos_all)) 100 * mean((x_all[pos_all] / y_all[pos_all] - 1)) else NA_real_
mape_all <- if (any(pos_all)) 100 * mean(abs((x_all[pos_all] - y_all[pos_all]) / y_all[pos_all])) else NA_real_

r_all  <- suppressWarnings(cor(x_all, y_all))
r2_all <- if (is.finite(r_all)) r_all^2 else NA_real_

overall_summary <- tibble::tibble(
  n = n_all,
  MAE = mae_all,
  RMSE = rmse_all,
  PercentBias = mpe_all,
  MAPE = mape_all,
  R = r_all,
  R2 = r2_all
)

overall_summary
