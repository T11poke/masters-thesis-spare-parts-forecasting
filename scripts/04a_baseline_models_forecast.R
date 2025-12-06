# 04a - PREVISÃO: MODELOS BASELINE ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Implementação de métodos clássicos e de suavização exponencial
#            para previsão de demanda intermitente (Famílias 1 e 2)
# Data: 2025-12-05
# Versão: 2.0.0

# Família 1: Métodos Clássicos
#   - Naive
#   - Média Simples
#   - Média Móvel (k=36)
#
# Família 2: Suavização Exponencial e Séries Temporais
#   - ARIMA (auto.arima)
#   - ETS
#   - Holt-Winters Aditivo
#   - Holt-Winters Multiplicativo

# ______________________________________
# BLOCO 0: Setup e Configuração ####
# ______________________________________

library(here)
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(forecast)  # Para auto.arima, ets, hw
library(future)
library(furrr)
library(tictoc)
library(writexl)
library(progressr)

source(here("R/utils/load_config.R"))
source(here("R/functions/forecasting_functions.R"))

handlers(handler_cli(clear = FALSE))
handlers(global = TRUE)

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO PREVISÃO - MODELOS BASELINE", "INFO")
log_message("========================================", "INFO")

# Criar estrutura de diretórios
dir.create(here("output/forecasts"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/forecasts/baseline"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/figures/04a_baseline"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/reports/04a_baseline"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/checkpoints"), showWarnings = FALSE, recursive = TRUE)

# Configurar paralelização
if(config$computation$parallel) {
  plan(multisession, workers = config$parameters$forecasting$parallel$n_cores)
  log_message(sprintf("Paralelização ativada: %d cores", 
                      config$parameters$forecasting$parallel$n_cores), "INFO")
}

# Carregar dados processados
log_message("Carregando dados de train/test splits", "INFO")
splits_list <- readRDS(
  here(config$paths$data$processed, "train_test_splits.rds")
  )

# Validação crítica
if(!exists("splits_list") || length(splits_list) == 0) {
  stop("❌ ERRO: train_test_splits.rds não encontrado ou vazio. Execute script 02 primeiro.")
}

cat("\n📊 Dados carregados:\n")
cat(sprintf("   - Número de origens: %d\n", length(splits_list)))
cat(sprintf("   - Horizonte de previsão: %d meses\n", 
            config$parameters$forecasting$horizon))

# ______________________________________________________________________________
# BLOCO 1: Definição e Implementação dos Métodos ####
# ______________________________________________________________________________

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: DEFINIÇÃO DOS MÉTODOS BASELINE\n")
cat(strrep("=", 70), "\n\n")

log_message("Definindo métodos de previsão Família 1 e 2", "INFO")

# ______________________________________________________________________________
# FAMÍLIA 1: MÉTODOS CLÁSSICOS ####
# ______________________________________________________________________________

#' Forecast: Naive
#' 
#' Previsão = última observação
forecast_naive <- function(train_ts, h = 12) {
  
  tryCatch({
    
    last_value <- tail(train_ts, 1)
    
    list(
      point = rep(last_value, h),
      fitted = rep(last_value, length(train_ts)),
      residuals = train_ts - last_value,
      method = "Naive",
      convergence = TRUE,
      error_message = NA_character_
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "Naive",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: Média Simples
#' 
#' Previsão = média histórica
forecast_mean <- function(train_ts, h = 12) {
  
  tryCatch({
    
    mean_value <- mean(train_ts, na.rm = TRUE)
    
    list(
      point = rep(mean_value, h),
      fitted = rep(mean_value, length(train_ts)),
      residuals = train_ts - mean_value,
      method = "Mean",
      convergence = TRUE,
      error_message = NA_character_
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "Mean",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: Média Móvel
#' 
#' Previsão = média das últimas k observações
#' @param k janela de média móvel (default: 36 meses)
forecast_ma <- function(train_ts, h = 12, k = 36) {
  
  tryCatch({
    
    n <- length(train_ts)
    
    # Se série menor que k, usar toda a série
    if(n < k) {
      k <- n
      warning(sprintf("Série tem %d obs < k=%d. Usando k=%d", n, k, k))
    }
    
    # Calcular média móvel das últimas k observações
    ma_value <- mean(tail(train_ts, k), na.rm = TRUE)
    
    # Fitted values: MA centrada ou trailing
    fitted <- rep(NA_real_, n)
    if(n >= k) {
      for(i in k:n) {
        fitted[i] <- mean(train_ts[(i-k+1):i], na.rm = TRUE)
      }
    }
    
    list(
      point = rep(ma_value, h),
      fitted = fitted,
      residuals = train_ts - fitted,
      method = sprintf("MA(%d)", k),
      convergence = TRUE,
      error_message = NA_character_,
      k = k
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = sprintf("MA(%d)", k),
      convergence = FALSE,
      error_message = conditionMessage(e),
      k = k
    )
  })
}

# ===========================================================================
# FAMÍLIA 2: SUAVIZAÇÃO EXPONENCIAL E SÉRIES TEMPORAIS ####
# ===========================================================================

#' Forecast: ARIMA
#' 
#' Seleção automática via auto.arima
forecast_arima <- function(train_ts, h = 12) {
  
  tryCatch({
    
    # Parâmetros do config
    max_p <- config$parameters$forecasting$arima$max_p
    max_d <- config$parameters$forecasting$arima$max_d
    max_q <- config$parameters$forecasting$arima$max_q
    
    # Ajustar modelo
    fit <- forecast::auto.arima(
      train_ts,
      max.p = max_p,
      max.d = max_d,
      max.q = max_q,
      stepwise = config$parameters$forecasting$arima$stepwise,
      approximation = config$parameters$forecasting$arima$approximation,
      trace = FALSE,
      seasonal = FALSE  # Demanda intermitente raramente tem sazonalidade
    )
    
    # Gerar previsões
    fc <- forecast::forecast(fit, h = h)
    
    # Truncar negativos
    fc_point <- pmax(fc$mean, 0)
    
    list(
      point = as.numeric(fc_point),
      fitted = as.numeric(fitted(fit)),
      residuals = as.numeric(residuals(fit)),
      method = "ARIMA",
      model_string = arima_string(fit),
      convergence = TRUE,
      error_message = NA_character_,
      aic = fit$aic,
      model_object = fit
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "ARIMA",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: ETS
#' 
#' Error-Trend-Seasonal via ets()
forecast_ets <- function(train_ts, h = 12) {
  
  tryCatch({
    
    fit <- forecast::ets(
      train_ts,
      model = "ZZN",  # Automático, sem sazonalidade
      damped = NULL    # Testar ambos
    )
    
    fc <- forecast::forecast(fit, h = h)
    
    # Truncar negativos
    fc_point <- pmax(fc$mean, 0)
    
    list(
      point = as.numeric(fc_point),
      fitted = as.numeric(fitted(fit)),
      residuals = as.numeric(residuals(fit)),
      method = "ETS",
      model_string = fit$method,
      convergence = TRUE,
      error_message = NA_character_,
      aic = fit$aic,
      model_object = fit
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "ETS",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: Holt-Winters Aditivo
forecast_hw_add <- function(train_ts, h = 12) {
  
  tryCatch({
    
    # HW requer pelo menos 2 ciclos sazonais completos
    # Para mensal: pelo menos 24 observações
    if(length(train_ts) < 24) {
      stop("Série muito curta para Holt-Winters (< 24 obs)")
    }
    
    fit <- forecast::hw(
      train_ts,
      seasonal = "additive",
      h = h
    )
    
    # Truncar negativos
    fc_point <- pmax(fit$mean, 0)
    
    list(
      point = as.numeric(fc_point),
      fitted = as.numeric(fitted(fit)),
      residuals = as.numeric(residuals(fit)),
      method = "HW_Additive",
      convergence = TRUE,
      error_message = NA_character_,
      model_object = fit
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "HW_Additive",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: Holt-Winters Multiplicativo
forecast_hw_mult <- function(train_ts, h = 12) {
  
  tryCatch({
    
    if(length(train_ts) < 24) {
      stop("Série muito curta para Holt-Winters (< 24 obs)")
    }
    
    # HW Multiplicativo requer valores estritamente positivos
    if(any(train_ts <= 0)) {
      stop("Série contém zeros/negativos - incompatível com HW multiplicativo")
    }
    
    fit <- forecast::hw(
      train_ts,
      seasonal = "multiplicative",
      h = h
    )
    
    fc_point <- pmax(fit$mean, 0)
    
    list(
      point = as.numeric(fc_point),
      fitted = as.numeric(fitted(fit)),
      residuals = as.numeric(residuals(fit)),
      method = "HW_Multiplicative",
      convergence = TRUE,
      error_message = NA_character_,
      model_object = fit
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "HW_Multiplicative",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


#' Forecast: TSLM (Time Series Linear Model)
forecast_tslm <- function(train_ts, h = 12) {
  
  tryCatch({
    
    # Criar variável de tempo
    time_index <- seq_along(train_ts)
    
    fit <- forecast::tslm(train_ts ~ time_index)
    
    # Criar data frame para previsão
    new_data <- data.frame(
      time_index = (length(train_ts) + 1):(length(train_ts) + h)
    )
    
    fc <- forecast::forecast(fit, newdata = new_data, h = h)
    
    # Truncar negativos
    fc_point <- pmax(fc$mean, 0)
    
    list(
      point = as.numeric(fc_point),
      fitted = as.numeric(fitted(fit)),
      residuals = as.numeric(residuals(fit)),
      method = "TSLM",
      convergence = TRUE,
      error_message = NA_character_,
      model_object = fit
    )
    
  }, error = function(e) {
    list(
      point = rep(NA_real_, h),
      fitted = rep(NA_real_, length(train_ts)),
      residuals = rep(NA_real_, length(train_ts)),
      method = "TSLM",
      convergence = FALSE,
      error_message = conditionMessage(e)
    )
  })
}


cat("✅ Funções de previsão definidas:\n")
cat("   - Família 1: Naive, Mean, MA(36)\n")
cat("   - Família 2: ARIMA, ETS, HW_Add, HW_Mult\n\n")





