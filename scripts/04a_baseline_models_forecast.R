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

# Lista de métodos a aplicar
metodos_baseline <- list(
  naive = forecast_naive,
  mean = forecast_mean,
  ma_36 = function(ts, h) forecast_ma(ts, h, k = 36),
  arima = forecast_arima,
  ets = forecast_ets,
  hw_add = forecast_hw_add,
  hw_mult = forecast_hw_mult,
  tslm = forecast_tslm
)

# Horizonte de previsão
h <- config$parameters$forecasting$horizon

# Estrutura para armazenar resultados
forecasts_baseline <- list()

# ===========================================================================
# MODO DEBUG ####
# ===========================================================================

# Detectar modo debug via variável de ambiente ou config
DEBUG_MODE <- Sys.getenv("FORECAST_DEBUG", "FALSE") == "TRUE" ||
  isTRUE(config$parameters$forecasting$debug_mode)

if(DEBUG_MODE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║                    🔧 MODO DEBUG ATIVO 🔧                  ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("⚠️  Configurações de debug:\n")
  cat(sprintf("   - Processar apenas %d materiais por origem\n", 
              config$parameters$forecasting$debug_n_materials))
  cat(sprintf("   - Usar apenas %d origens\n", 
              config$parameters$forecasting$debug_n_origins))
  cat(sprintf("   - Chunk size reduzido: %d\n\n", 
              config$parameters$forecasting$debug_chunk_size))
  
  # Limitar número de origens
  if(length(splits_list) > config$parameters$forecasting$debug_n_origins) {
    splits_list <- splits_list[1:config$parameters$forecasting$debug_n_origins]
    cat(sprintf("✂️  Limitando análise às primeiras %d origens\n\n", 
                config$parameters$forecasting$debug_n_origins))
  }
}

# ===========================================================================
# LOOP SOBRE ORIGENS ####
# ===========================================================================

for(origem_nome in names(splits_list)) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("🔄 PROCESSANDO %s\n", toupper(origem_nome)))
  cat(strrep("=", 70), "\n\n")
  
  tic(sprintf("Tempo total - %s", origem_nome))
  
  origem_split <- splits_list[[origem_nome]]
  origem_id <- origem_split$metadata$origem_id
  
  # Extrair dados de treino
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
  
  # Identificar materiais elegíveis (>= 3 ocorrências de demanda)
  materiais_elegiveis <- train_data %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      n_nonzero = sum(qt_total > 0),
      n_periods = n(),
      .groups = 'drop'
    ) %>%
    filter(n_nonzero >= config$parameters$data_cleaning$min_occurrences) %>%
    pull(cd_material)
  
  n_elegiveis_original <- length(materiais_elegiveis)
  
  if(DEBUG_MODE) {
    n_debug <- min(
      config$parameters$forecasting$debug_n_materials,
      n_elegiveis_original
    )
    
    # Amostragem estratificada por categoria SBC
    materiais_debug <- sbc_classification %>%
      filter(cd_material %in% materiais_elegiveis) %>%
      group_by(categoria_sbc) %>%
      slice_sample(n = ceiling(n_debug / n_distinct(categoria_sbc))) %>%
      ungroup() %>%
      slice_head(n = n_debug) %>%
      pull(cd_material)
    
    materiais_elegiveis <- materiais_debug
    
    cat("\n🔧 DEBUG: Amostra estratificada selecionada:\n")
    sbc_classification %>%
      filter(cd_material %in% materiais_elegiveis) %>%
      count(categoria_sbc) %>%
      print()
    cat("\n")
  }
  
  n_elegiveis <- length(materiais_elegiveis)
  
  cat(sprintf("\n✅ Materiais elegíveis: %s (>= %d ocorrências)\n",
              format(n_elegiveis, big.mark = ","),
              config$parameters$data_cleaning$min_occurrences))
  if(DEBUG_MODE) {
    cat(sprintf(" (de %s totais - modo debug)\n",
                format(n_elegiveis_original, big.mark = ",")))
  } else {
    cat("\n")
  }
  
  # Log de execução
  log_execucao <- tibble()
  
  # ===========================================================================
  ## FUNÇÃO PARA PROCESSAR UM MATERIAL ####
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
    
    # Consolidar em lista estruturada
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
  # EXECUÇÃO PARALELA ####
  # ===========================================================================
  
  cat("\n🚀 Iniciando forecasting paralelo...\n")
  
  tic("Forecasting paralelo")
  
  # Definir chunk size (menor em modo debug)
  chunk_size <- if(DEBUG_MODE) {
    config$parameters$forecasting$debug_chunk_size
  } else {
    config$parameters$forecasting$parallel$chunk_size
  }
  
  n_chunks <- ceiling(n_elegiveis / chunk_size)
  
  cat(sprintf("   - Dividindo %s materiais em %d chunks de ~%d materiais\n",
              format(n_elegiveis, big.mark = ","),
              n_chunks,
              chunk_size))
  cat(sprintf("   - Workers paralelos: %d\n", 
              config$parameters$forecasting$parallel$n_cores))
  cat(sprintf("   - Métodos por material: %d\n\n", 
              length(metodos_baseline)))
  
  material_chunks <- split(
    materiais_elegiveis,
    ceiling(seq_along(materiais_elegiveis) / chunk_size)
  )
  
  tic("Forecasting paralelo")
  
  if(config$computation$parallel) {
    
    # Dividir materiais em chunks
    chunk_size <- config$parameters$forecasting$parallel$chunk_size
    n_chunks <- ceiling(n_elegiveis / chunk_size)
    
    cat(sprintf("   - Dividindo %s materiais em %d chunks de ~%d\n",
                format(n_elegiveis, big.mark = ","),
                n_chunks,
                chunk_size))
    
    material_chunks <- split(
      materiais_elegiveis,
      ceiling(seq_along(materiais_elegiveis) / chunk_size)
    )
    
    # Processar chunks em paralelo
    forecasts_origem <- with_progress({
      # Criar progressor
      p <- progressor(
        steps = length(material_chunks),
        message = sprintf("Origem %s", origem_nome)
      )
      
      future_map(
        material_chunks,
        function(chunk) {
          # Processar todos os materiais do chunk
          chunk_result <- map(chunk, processar_material)
          
          # Atualizar progresso após chunk completo
          p(message = sprintf(
            "✓ Chunk concluído (%d materiais, %d métodos cada)",
            length(chunk),
            length(metodos_baseline)
          ))
          return(chunk_result)
          
        },
        .options = furrr_options(
          seed = config$parameters$seed,
          globals = TRUE
          )
      )
      
    }) %>% flatten()  # Achatar lista de listas
  
    # modo sequencial: - PAREI AQUI!!!! #####
  } else {
    
    # Execução sequencial
    forecasts_origem <- map(
      materiais_elegiveis,
      processar_material,
      .progress = TRUE
    )
  }
  
  toc()
  
  # ===========================================================================
  # VALIDAÇÃO E DIAGNÓSTICO ####
  # ===========================================================================
  
  cat("\n🔍 Validando forecasts...\n")
  
  # Extrair taxas de convergência por método
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
      n_with_na = sum(has_na),
      taxa_sucesso = n_converged / n_total * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(taxa_sucesso))
  
  cat("\n📊 Taxa de convergência por método:\n")
  print(convergence_summary, n = Inf)
  
  # Salvar resumo de convergência
  convergence_summary %>%
    mutate(origem = origem_nome) %>%
    write_xlsx(
      here("output/reports/04a_baseline",
           sprintf("convergence_%s.xlsx", origem_nome))
    )
  
  # ===========================================================================
  # CHECKPOINT: SALVAR RESULTADOS DA ORIGEM ####
  # ===========================================================================
  
  forecasts_baseline[[origem_nome]] <- list(
    metadata = origem_split$metadata,
    forecasts = forecasts_origem,
    convergence_summary = convergence_summary
  )
  
  # Salvar checkpoint
  saveRDS(
    forecasts_baseline[[origem_nome]],
    here("output/checkpoints", sprintf("baseline_%s.rds", origem_nome))
  )
  
  cat(sprintf("\n✅ Checkpoint salvo: baseline_%s.rds\n", origem_nome))
  
  toc()
  
}  # FIM DO LOOP SOBRE ORIGENS

log_message("Pipeline de forecasting concluído para todas as origens", "INFO")



