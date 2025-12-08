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
#   - TSLM

# ===========================================================================
# BLOCO 0: Setup e Configuração ####
# ===========================================================================

library(here)
library(tidyverse)
library(magrittr)
library(tsibble)
library(fable)
library(feasts)
library(forecast)  # Para auto.arima, ets, hw
library(future)
library(furrr)
library(tictoc)
library(writexl)
library(progressr)

Sys.setenv(FORECAST_DEBUG = "TRUE")

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

# ===========================================================================
# BLOCO 1: Definição e Implementação dos Métodos ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: DEFINIÇÃO DOS MÉTODOS BASELINE\n")
cat(strrep("=", 70), "\n\n")

log_message("Definindo métodos de previsão Família 1 e 2", "INFO")

# ===========================================================================
# FAMÍLIA 1: MÉTODOS CLÁSSICOS ####
# ===========================================================================

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
cat("   - Família 2: ARIMA, ETS, HW_Add, HW_Mult, TLSM\n\n")

# ===========================================================================
# BLOCO 2: Pipeline de Forecast por Origem ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando pipeline de forecasting", "INFO")

# ===========================================================================
# 2.1. DEFINIÇÃO DOS MÉTODOS ####
# ===========================================================================

# Lista de métodos baseline a aplicar
metodos_baseline <- list(
  
  # --- FAMÍLIA 1: CLÁSSICOS --- #
  
  naive = function(train_ts, h) {
    tryCatch({
      ultimo_valor <- tail(train_ts[train_ts > 0], 1)
      if(length(ultimo_valor) == 0) ultimo_valor <- 0
      
      list(
        point = rep(ultimo_valor, h),
        fitted = rep(ultimo_valor, length(train_ts)),
        residuals = train_ts - ultimo_valor,
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
  },
  
  mean = function(train_ts, h) {
    tryCatch({
      media <- mean(train_ts, na.rm = TRUE)
      
      list(
        point = rep(media, h),
        fitted = rep(media, length(train_ts)),
        residuals = train_ts - media,
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
  },
  
  ma_36 = function(train_ts, h) {
    tryCatch({
      k <- 36
      n <- length(train_ts)
      if(n < k) k <- n
      
      ma_value <- mean(tail(train_ts, k), na.rm = TRUE)
      
      # Fitted values
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
        method = "MA_36",
        convergence = TRUE,
        error_message = NA_character_
      )
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "MA_36",
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  # --- FAMÍLIA 2: SUAVIZAÇÃO EXPONENCIAL --- #
  
  arima = function(train_ts, h) {
    tryCatch({
      fit <- forecast::auto.arima(
        train_ts,
        max.p = 5, max.d = 2, max.q = 5,
        stepwise = TRUE,
        approximation = FALSE,
        trace = FALSE,
        seasonal = FALSE
      )
      
      fc <- forecast::forecast(fit, h = h)
      fc_point <- pmax(as.numeric(fc$mean), 0)  # Truncar negativos
      
      list(
        point = fc_point,
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        method = "ARIMA",
        model_string = paste0("ARIMA(", 
                              paste(arimaorder(fit), collapse = ","), ")"),
        aic = fit$aic,
        convergence = TRUE,
        error_message = NA_character_
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
  },
  
  ets = function(train_ts, h) {
    tryCatch({
      fit <- forecast::ets(train_ts, model = "ZZN", damped = NULL)
      fc <- forecast::forecast(fit, h = h)
      fc_point <- pmax(as.numeric(fc$mean), 0)
      
      list(
        point = fc_point,
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        method = "ETS",
        model_string = fit$method,
        aic = fit$aic,
        convergence = TRUE,
        error_message = NA_character_
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
  },
  
  hw_add = function(train_ts, h) {
    tryCatch({
      if(length(train_ts) < 24) {
        stop("Série muito curta para Holt-Winters (< 24 obs)")
      }
      
      fit <- forecast::hw(train_ts, seasonal = "additive", h = h)
      fc_point <- pmax(as.numeric(fit$mean), 0)
      
      list(
        point = fc_point,
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        method = "HW_Additive",
        convergence = TRUE,
        error_message = NA_character_
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
  },
  
  hw_mult = function(train_ts, h) {
    tryCatch({
      if(length(train_ts) < 24 || any(train_ts <= 0)) {
        stop("Incompatível com HW multiplicativo (série curta ou com zeros)")
      }
      
      fit <- forecast::hw(train_ts, seasonal = "multiplicative", h = h)
      fc_point <- pmax(as.numeric(fit$mean), 0)
      
      list(
        point = fc_point,
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        method = "HW_Multiplicative",
        convergence = TRUE,
        error_message = NA_character_
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
  },
  
  tslm = function(train_ts, h) {
    tryCatch({
      time_index <- seq_along(train_ts)
      fit <- forecast::tslm(train_ts ~ time_index)
      
      new_data <- data.frame(
        time_index = (length(train_ts) + 1):(length(train_ts) + h)
      )
      
      fc <- forecast::forecast(fit, newdata = new_data, h = h)
      fc_point <- pmax(as.numeric(fc$mean), 0)
      
      list(
        point = fc_point,
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        method = "TSLM",
        convergence = TRUE,
        error_message = NA_character_
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
)

cat("✅ Métodos baseline definidos:\n")
cat(sprintf("   - Total de métodos: %d\n", length(metodos_baseline)))
cat("   - Família 1 (Clássicos): Naive, Mean, MA_36\n")
cat("   - Família 2 (Suavização): ARIMA, ETS, HW_Add, HW_Mult, TSLM\n\n")

# ===========================================================================
# 2.2. CONFIGURAÇÃO DE MODO DEBUG ####
# ===========================================================================

h <- config$parameters$forecasting$horizon

DEBUG_MODE <- Sys.getenv("FORECAST_DEBUG", "FALSE") == "TRUE" ||
  isTRUE(config$parameters$forecasting$debug_mode)

if(DEBUG_MODE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║                    🔧 MODO DEBUG ATIVO 🔧                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("⚠️  Configurações de debug:\n")
  cat(sprintf("   - Processar apenas %d materiais por origem\n", 
              config$parameters$forecasting$debug_n_materials))
  cat(sprintf("   - Usar apenas %d origens\n", 
              config$parameters$forecasting$debug_n_origins))
  cat(sprintf("   - Chunk size: %d\n\n", 
              config$parameters$forecasting$debug_chunk_size))
  
  if(length(splits_list) > config$parameters$forecasting$debug_n_origins) {
    splits_list <- splits_list[1:config$parameters$forecasting$debug_n_origins]
    cat(sprintf("✂️  Limitando análise às primeiras %d origens\n\n", 
                config$parameters$forecasting$debug_n_origins))
  }
}

forecasts_baseline <- list()

# ===========================================================================
# 2.3. LOOP SOBRE ORIGENS ####
# ===========================================================================

origem_nome <- "origem_1"

for(origem_nome in names(splits_list)) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("🔄 PROCESSANDO %s\n", toupper(origem_nome)))
  cat(strrep("=", 70), "\n\n")
  
  tic(sprintf("Tempo total - %s", origem_nome))
  
  origem_split <- splits_list[[origem_nome]]
  origem_id <- origem_split$metadata$origem_id
  
  train_data <- origem_split$train
  test_data <- origem_split$test
  sbc_classification <- origem_split$sbc_classification
  
  cat(sprintf("📊 Informações da origem:\n"))
  cat(sprintf("   - Período treino: %s a %s\n",
              min(train_data$data_competencia),
              max(train_data$data_competencia)))
  cat(sprintf("   - Período teste: %s a %s\n",
              min(test_data$data_competencia),
              max(test_data$data_competencia)))
  cat(sprintf("   - Materiais únicos: %s\n",
              format(n_distinct(train_data$cd_material), big.mark = ",")))
  
  # Identificar materiais elegíveis
  materiais_elegiveis <- train_data %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      n_nonzero = sum(qt_total > 0),
      .groups = 'drop'
    ) %>%
    filter(n_nonzero >= config$parameters$data_cleaning$min_occurrences) %>%
    pull(cd_material)
  
  n_elegiveis_original <- length(materiais_elegiveis)
  
  # ===========================================================================
  # 2.4. APLICAR MODO DEBUG (SUBSET) ####
  # ===========================================================================
  
  if(DEBUG_MODE) {
    
    n_debug <- min(
      config$parameters$forecasting$debug_n_materials,
      n_elegiveis_original
    )
    
    cat(sprintf("\n🔧 MODO DEBUG: Selecionando %d materiais de %d disponíveis\n",
                n_debug, n_elegiveis_original))
    
    # Obter materiais elegíveis com classificação SBC
    sbc_elegiveis <- sbc_classification %>%
      filter(cd_material %in% materiais_elegiveis)
    
  
    # Amostragem estratificada
    cat("   Estratégia: Amostragem estratificada por categoria SBC\n")
    
    # Calcular distribuição por categoria
    categorias_disponiveis <- sbc_elegiveis %>%
      count(categoria_sbc, name = "n_disp")
    
    n_categorias <- nrow(categorias_disponiveis)
    n_por_categoria <- ceiling(n_debug / n_categorias)
    
    cat(sprintf("   - Categorias SBC: %d\n", n_categorias))
    cat(sprintf("   - Alvo por categoria: %d materiais\n\n", n_por_categoria))
    
    set.seed(config$parameters$seed)
    
    materiais_debug <- character(0)
    
    for(cat_atual in categorias_disponiveis$categoria_sbc) {
      
      # Materiais desta categoria
      mats_categoria <- sbc_elegiveis %>%
        filter(categoria_sbc == cat_atual) %>%
        pull(cd_material)
      
      # Amostrar
      n_amostrar <- min(n_por_categoria, length(mats_categoria))
      mats_selecionados <- sample(mats_categoria, size = n_amostrar)
      
      materiais_debug <- c(materiais_debug, mats_selecionados)
    }
    
    # Limitar ao total desejado
    if(length(materiais_debug) > n_debug) {
      materiais_debug <- sample(materiais_debug, size = n_debug)
    }
    
    materiais_elegiveis <- materiais_debug
    
    #Relatório
    cat("   📊 Distribuição da amostra selecionada:\n\n")
    
    distribuicao_debug <- sbc_elegiveis %>%
      mutate(selecionado = cd_material %in% materiais_elegiveis) %>%
      group_by(categoria_sbc) %>%
      summarise(
        n_disponiveis = n(),
        n_selecionados = sum(selecionado),
        percentual = sprintf("%.1f%%", mean(selecionado) * 100),
        .groups = 'drop'
      ) %>%
      arrange(desc(n_selecionados))
    
    print(distribuicao_debug)
    cat(sprintf("\n   ✓ Total selecionado: %d materiais\n", length(materiais_elegiveis)))

  cat("\n")
  }
  
  n_elegiveis <- length(materiais_elegiveis)
  
  cat(sprintf("\n✅ Materiais a processar: %s",
              format(n_elegiveis, big.mark = ",")))
  
  if(DEBUG_MODE) {
    cat(sprintf(" (de %s totais - modo debug)\n",
                format(n_elegiveis_original, big.mark = ",")))
  } else {
    cat("\n")
  }
  
  # ===========================================================================
  # 2.5. FUNÇÃO PARA PROCESSAR UM MATERIAL ####
  # ===========================================================================
  
  processar_material <- function(cd_mat) {
    
    # Extrair série temporal de treino
    serie_train <- train_data %>%
      filter(cd_material == cd_mat) %>%
      arrange(data_competencia) %>%
      pull(qt_total)
    
    # Converter para objeto ts
    train_ts <- ts(serie_train, frequency = 12)
    
    # Obter classificação SBC
    sbc_info <- sbc_classification %>%
      filter(cd_material == cd_mat)
    
    # Aplicar todos os métodos
    forecasts_material <- map(metodos_baseline, function(metodo_func) {
      tic()
      resultado <- metodo_func(train_ts, h = h)
      tempo <- toc(quiet = TRUE)
      resultado$execution_time <- tempo$toc - tempo$tic
      return(resultado)
    })
    
    # Consolidar resultado
    list(
      cd_material = cd_mat,
      origem_id = origem_id,
      categoria_sbc = if(nrow(sbc_info) > 0) sbc_info$categoria_sbc else NA_character_,
      adi = if(nrow(sbc_info) > 0) sbc_info$adi else NA_real_,
      cv2 = if(nrow(sbc_info) > 0) sbc_info$cv2 else NA_real_,
      
      train_stats = list(
        n_periods = length(serie_train),
        n_nonzero = sum(serie_train > 0),
        mean_demand = mean(serie_train[serie_train > 0], na.rm = TRUE),
        sd_demand = sd(serie_train[serie_train > 0], na.rm = TRUE),
        prop_zeros = mean(serie_train == 0)
      ),
      
      forecasts = forecasts_material
    )
  }
  
  # ===========================================================================
  # 2.6. EXECUÇÃO PARALELA COM PROGRESSO ####
  # ===========================================================================
  
  cat("\n🚀 Iniciando forecasting paralelo...\n")
  
  chunk_size <- if(DEBUG_MODE) {
    config$parameters$forecasting$debug_chunk_size
  } else {
    config$parameters$forecasting$parallel$chunk_size
  }
  
  n_chunks <- ceiling(n_elegiveis / chunk_size)
  
  cat(sprintf("   - Dividindo %s materiais em %d chunks de ~%d\n",
              format(n_elegiveis, big.mark = ","),
              n_chunks, chunk_size))
  cat(sprintf("   - Workers paralelos: %d\n", 
              config$parameters$forecasting$parallel$n_cores))
  cat(sprintf("   - Métodos por material: %d\n\n", length(metodos_baseline)))
  
  material_chunks <- split(
    materiais_elegiveis,
    ceiling(seq_along(materiais_elegiveis) / chunk_size)
  )
  
  tic("Forecasting paralelo")
  
  if(config$computation$parallel) {
    
    forecasts_origem <- with_progress({
      
      p <- progressor(
        steps = length(material_chunks),
        message = sprintf("Origem %s", origem_nome)
      )
      
      future_map(
        material_chunks,
        function(chunk) {
          chunk_result <- map(chunk, processar_material)
          p(message = sprintf("✓ Chunk (%d materiais)", length(chunk)))
          return(chunk_result)
        },
        .options = furrr_options(
          seed = config$parameters$seed,
          globals = TRUE
        )
      )
      
    }) %>% flatten()
    
  } else {
    
    cat("   ℹ️  Modo sequencial\n\n")
    
    pb <- progress::progress_bar$new(
      format = "  [:bar] :percent | :current/:total | ETA: :eta",
      total = n_elegiveis,
      clear = FALSE,
      width = 70
    )
    
    forecasts_origem <- map(materiais_elegiveis, function(mat) {
      result <- processar_material(mat)
      pb$tick()
      return(result)
    })
  }
  
  tempo_total <- toc()
  
  # ===========================================================================
  # 2.7. ESTATÍSTICAS DE EXECUÇÃO ####
  # ===========================================================================
  
  cat("\n📊 Estatísticas de execução:\n")
  cat(sprintf("   - Materiais processados: %s\n",
              format(length(forecasts_origem), big.mark = ",")))
  cat(sprintf("   - Tempo total: %.1f seg (%.1f min)\n",
              tempo_total$toc - tempo_total$tic,
              (tempo_total$toc - tempo_total$tic) / 60))
  cat(sprintf("   - Tempo médio/material: %.2f seg\n",
              (tempo_total$toc - tempo_total$tic) / n_elegiveis))
  
  # ===========================================================================
  # 2.8. VALIDAÇÃO E CONVERGÊNCIA ####
  # ===========================================================================
  
  cat("\n🔍 Validando forecasts...\n")
  
  convergence_summary <- map_dfr(forecasts_origem, function(mat_forecast) {
    map_dfr(names(mat_forecast$forecasts), function(metodo_nome) {
      tibble(
        metodo = metodo_nome,
        convergence = mat_forecast$forecasts[[metodo_nome]]$convergence,
        has_na = any(is.na(mat_forecast$forecasts[[metodo_nome]]$point))
      )
    })
  }) %>%
    group_by(metodo) %>%
    summarise(
      n_total = n(),
      n_converged = sum(convergence),
      n_failed = sum(!convergence),
      taxa_sucesso = n_converged / n_total * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(taxa_sucesso))
  
  cat("\n📊 Taxa de convergência por método:\n")
  print(convergence_summary, n = Inf)
  
  # Salvar resumo
  convergence_summary %>%
    mutate(origem = origem_nome) %>%
    write_xlsx(
      here("output/reports/04a_baseline",
           sprintf("convergence_%s.xlsx", origem_nome))
    )
  
  # ===========================================================================
  # 2.9. CHECKPOINT ####
  # ===========================================================================
  
  forecasts_baseline[[origem_nome]] <- list(
    metadata = origem_split$metadata,
    forecasts = forecasts_origem,
    convergence_summary = convergence_summary,
    execution_stats = list(
      n_materiais = n_elegiveis,
      debug_mode = DEBUG_MODE,
      tempo_total_sec = tempo_total$toc - tempo_total$tic,
      timestamp = Sys.time()
    )
  )
  
  checkpoint_file <- if(DEBUG_MODE) {
    sprintf("baseline_%s_DEBUG.rds", origem_nome)
  } else {
    sprintf("baseline_%s.rds", origem_nome)
  }
  
  saveRDS(
    forecasts_baseline[[origem_nome]],
    here("output/checkpoints", checkpoint_file)
  )
  
  cat(sprintf("\n✅ Checkpoint salvo: %s\n", checkpoint_file))
  
  toc()
  
}  # FIM DO LOOP SOBRE ORIGENS

log_message("Pipeline de forecasting baseline concluído", "INFO")

