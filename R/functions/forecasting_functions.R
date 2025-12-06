# FUNÇÕES AUXILIARES COMPARTILHADAS ####

#' Extrair string descritiva de modelo ARIMA
arima_string <- function(fit) {
  sprintf("ARIMA(%d,%d,%d)", fit$arma[1], fit$arma[6], fit$arma[2])
}

#' Validar forecast
validar_forecast <- function(forecast_obj, h = 12) {
  checks <- list(
    length_correct = length(forecast_obj$point) == h,
    no_na = !any(is.na(forecast_obj$point)),
    no_negative = all(forecast_obj$point >= 0, na.rm = TRUE),
    numeric = is.numeric(forecast_obj$point)
  )
  all(unlist(checks))
}

#' Verificar qualidade da série
check_series_quality <- function(ts_obj) {
  list(
    is_constant = sd(ts_obj, na.rm = TRUE) == 0,
    has_enough_obs = length(ts_obj) >= 12,
    prop_zeros = mean(ts_obj == 0, na.rm = TRUE),
    has_variation = max(ts_obj, na.rm = TRUE) > min(ts_obj, na.rm = TRUE),
    all_zeros = all(ts_obj == 0, na.rm = TRUE)
  )
}