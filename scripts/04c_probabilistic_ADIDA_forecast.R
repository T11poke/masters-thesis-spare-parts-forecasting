# 04c - FORECASTING: MODELOS PROBABILÍSTICOS E ADIDA ####
#
# Descrição: Implementa métodos probabilísticos (Poisson, Gamma) e
#            agregação temporal (ADIDA)
# Data: 2025-12-08
# Versão: 2.0.0

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(tsibble)
library(furrr)
library(progressr)
library(tictoc)
library(writexl)
library(MASS)  # Para fitdistr (Gamma)

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO FORECASTING - PROBABILÍSTICO E ADIDA", "INFO")
log_message("========================================", "INFO")

# Configurar progresso
if(interactive()) {
  handlers(handler_cli(clear = FALSE))
} else {
  handlers(handler_txtprogressbar())
}
handlers(global = TRUE)

# Criar diretórios
dir.create(here("output/forecasts/probabilistic"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/reports/04c_prob_adida"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/checkpoints"), showWarnings = FALSE, recursive = TRUE)

# Configurar paralelização
parallel::detectCores()
if(config$computation$parallel) {
  plan(multisession, workers = config$computation$n_cores)
  log_message(sprintf("Paralelização ativada: %d cores", 
                      config$computation$n_cores), "INFO")
}

cat("\n📁 Diretórios de output criados\n")

# ===========================================================================
# BLOCO 0: CARREGAR DADOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 0: CARREGAMENTO DE DADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando train/test splits", "INFO")

splits_list <- readRDS(here(config$paths$data$processed, "train_test_splits.rds"))

cat("✅ Dados carregados:\n")
cat(sprintf("   - Número de origens: %d\n", length(splits_list)))
cat(sprintf("   - Horizonte de previsão: %d meses\n", 
            config$parameters$forecasting$horizon))

# ===========================================================================
# BLOCO 1: DEFINIÇÃO DOS MÉTODOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: DEFINIÇÃO DOS MÉTODOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Definindo métodos probabilísticos e ADIDA", "INFO")

# ---------------------------------------------------------------------------
## 1.1. MÉTODOS PROBABILÍSTICOS ####
# ---------------------------------------------------------------------------

metodos_probabilisticos <- list(
  
  poisson = function(train_ts, h, service_level = 0.80) {
    tryCatch({
      
      # Estimar lambda (taxa média)
      lambda <- mean(train_ts, na.rm = TRUE)
      
      if(lambda <= 0) {
        lambda <- 0.01  # Evitar lambda = 0
      }
      
      # Previsão pontual = lambda
      # Para nível de serviço, usar quantil
      fc_point <- rep(qpois(service_level, lambda), h)
      
      # Fitted values = lambda para todos os períodos
      fitted_vals <- rep(lambda, length(train_ts))
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "Poisson",
        lambda = lambda,
        service_level = service_level,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "Poisson",
        lambda = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  gamma = function(train_ts, h, service_level = 0.80) {
    tryCatch({
      
      # Filtrar apenas valores positivos
      positive_vals <- train_ts[train_ts > 0]
      
      if(length(positive_vals) < 3) {
        stop("Menos de 3 valores positivos para estimar Gamma")
      }
      
      # Estimar parâmetros via máxima verossimilhança
      fit_gamma <- MASS::fitdistr(positive_vals, "gamma")
      
      shape <- fit_gamma$estimate["shape"]
      rate <- fit_gamma$estimate["rate"]
      
      # Previsão pontual = média da Gamma = shape/rate
      # Para nível de serviço, usar quantil
      fc_value <- qgamma(service_level, shape = shape, rate = rate)
      fc_point <- rep(fc_value, h)
      
      # Fitted values = média da distribuição
      gamma_mean <- shape / rate
      fitted_vals <- rep(gamma_mean, length(train_ts))
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "Gamma",
        shape = shape,
        rate = rate,
        service_level = service_level,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "Gamma",
        shape = NA_real_,
        rate = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  }
)

# ---------------------------------------------------------------------------
## 1.2. MÉTODOS ADIDA ####
# ---------------------------------------------------------------------------

metodos_adida <- list(
  
  adida_k3_mean = function(train_ts, h) {
    tryCatch({
      
      k <- 3
      n <- length(train_ts)
      
      # Etapa 1: Agregação
      n_blocks <- floor(n / k)
      if(n_blocks == 0) {
        stop("Série muito curta para k=3")
      }
      
      aggregated <- numeric(n_blocks)
      for(i in 1:n_blocks) {
        idx_start <- (i - 1) * k + 1
        idx_end <- i * k
        aggregated[i] <- sum(train_ts[idx_start:idx_end])
      }
      
      # Etapa 2: Forecast na série agregada (usar média)
      fc_aggregated <- mean(aggregated, na.rm = TRUE)
      
      # Etapa 3: Desagregação
      n_blocks_fc <- ceiling(h / k)
      fc_aggregated_vector <- rep(fc_aggregated, n_blocks_fc)
      
      # Desagregar igualmente
      fc_point <- numeric(h)
      for(i in 1:h) {
        block_idx <- ceiling(i / k)
        fc_point[i] <- fc_aggregated_vector[block_idx] / k
      }
      
      # Fitted values (reconstituir da agregação)
      fitted_vals <- numeric(n)
      for(i in 1:n_blocks) {
        idx_start <- (i - 1) * k + 1
        idx_end <- min(i * k, n)
        fitted_vals[idx_start:idx_end] <- aggregated[i] / k
      }
      
      # Preencher últimos valores se série não for múltipla de k
      if(n > n_blocks * k) {
        fitted_vals[(n_blocks * k + 1):n] <- fc_point[1]
      }
      
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "ADIDA_k3_mean",
        k = k,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "ADIDA_k3_mean",
        k = 3,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  adida_k12_mean = function(train_ts, h) {
    tryCatch({
      
      k <- 12
      n <- length(train_ts)
      
      n_blocks <- floor(n / k)
      if(n_blocks == 0) {
        stop("Série muito curta para k=12")
      }
      
      aggregated <- numeric(n_blocks)
      for(i in 1:n_blocks) {
        idx_start <- (i - 1) * k + 1
        idx_end <- i * k
        aggregated[i] <- sum(train_ts[idx_start:idx_end])
      }
      
      fc_aggregated <- mean(aggregated, na.rm = TRUE)
      
      n_blocks_fc <- ceiling(h / k)
      fc_aggregated_vector <- rep(fc_aggregated, n_blocks_fc)
      
      fc_point <- numeric(h)
      for(i in 1:h) {
        block_idx <- ceiling(i / k)
        fc_point[i] <- fc_aggregated_vector[block_idx] / k
      }
      
      fitted_vals <- numeric(n)
      for(i in 1:n_blocks) {
        idx_start <- (i - 1) * k + 1
        idx_end <- min(i * k, n)
        fitted_vals[idx_start:idx_end] <- aggregated[i] / k
      }
      
      if(n > n_blocks * k) {
        fitted_vals[(n_blocks * k + 1):n] <- fc_point[1]
      }
      
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "ADIDA_k12_mean",
        k = k,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "ADIDA_k12_mean",
        k = 12,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  }
)

# Combinar todos os métodos
metodos_prob_adida <- c(metodos_probabilisticos, metodos_adida)

cat("✅ Métodos definidos:\n")
cat(sprintf("   - Probabilísticos: %d (Poisson, Gamma)\n", 
            length(metodos_probabilisticos)))
cat(sprintf("   - ADIDA: %d (k=3 e k=12 com média)\n", 
            length(metodos_adida)))
cat(sprintf("   - Total: %d métodos\n", length(metodos_prob_adida)))
cat(sprintf("   - Service level: %.0f%%\n", 
            config$parameters$forecasting$probabilistic$service_level * 100))

# ===========================================================================
# BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando pipeline de forecasting", "INFO")

h <- config$parameters$forecasting$horizon
service_level <- config$parameters$forecasting$probabilistic$service_level

# Modo debug
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

forecasts_probabilistic <- list()

# ===========================================================================
## 2.1. LOOP SOBRE ORIGENS ####
# ===========================================================================

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
  
  # ---------------------------------------------------------------------------
  ## 2.2. APLICAR MODO DEBUG ####
  # ---------------------------------------------------------------------------
  
  if(DEBUG_MODE) {
    
    n_debug <- min(
      config$parameters$forecasting$debug_n_materials,
      n_elegiveis_original
    )
    
    cat(sprintf("\n🔧 MODO DEBUG: Selecionando %d materiais de %d disponíveis\n",
                n_debug, n_elegiveis_original))
    
    sbc_elegiveis <- sbc_classification %>%
      filter(cd_material %in% materiais_elegiveis)
    
    if(nrow(sbc_elegiveis) == 0) {
      warning("Nenhum material elegível. Usando amostra aleatória.")
      set.seed(config$parameters$seed)
      materiais_elegiveis <- sample(materiais_elegiveis, size = n_debug)
      
    } else {
      cat("   Estratégia: Amostragem estratificada por categoria SBC\n")
      
      categorias_disponiveis <- sbc_elegiveis %>%
        count(categoria_sbc, name = "n_disp")
      
      n_categorias <- nrow(categorias_disponiveis)
      n_por_categoria <- ceiling(n_debug / n_categorias)
      
      cat(sprintf("   - Categorias: %d\n", n_categorias))
      cat(sprintf("   - Alvo por categoria: %d materiais\n\n", n_por_categoria))
      
      set.seed(config$parameters$seed)
      
      materiais_debug <- character(0)
      
      for(cat_atual in categorias_disponiveis$categoria_sbc) {
        mats_categoria <- sbc_elegiveis %>%
          filter(categoria_sbc == cat_atual) %>%
          pull(cd_material)
        
        n_amostrar <- min(n_por_categoria, length(mats_categoria))
        mats_selecionados <- sample(mats_categoria, size = n_amostrar)
        
        materiais_debug <- c(materiais_debug, mats_selecionados)
      }
      
      if(length(materiais_debug) > n_debug) {
        materiais_debug <- sample(materiais_debug, size = n_debug)
      }
      
      materiais_elegiveis <- materiais_debug
      
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
    }
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
  
  # ---------------------------------------------------------------------------
  ## 2.3. FUNÇÃO PARA PROCESSAR UM MATERIAL ####
  # ---------------------------------------------------------------------------
  
  processar_material <- function(cd_mat) {
    
    serie_train <- train_data %>%
      filter(cd_material == cd_mat) %>%
      arrange(data_competencia) %>%
      pull(qt_total)
    
    train_ts <- ts(serie_train, frequency = 12)
    
    sbc_info <- sbc_classification %>%
      filter(cd_material == cd_mat)
    
    forecasts_material <- map(metodos_prob_adida, function(metodo_func) {
      tic()
      
      # Passar service_level se for método probabilístico
      if(grepl("poisson|gamma", metodo_func %>% deparse() %>% paste(collapse = ""))) {
        resultado <- metodo_func(train_ts, h = h, service_level = service_level)
      } else {
        resultado <- metodo_func(train_ts, h = h)
      }
      
      tempo <- toc(quiet = TRUE)
      resultado$execution_time <- tempo$toc - tempo$tic
      return(resultado)
    })
    
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
  
  # ---------------------------------------------------------------------------
  ## 2.4. EXECUÇÃO PARALELA COM PROGRESSO ####
  # ---------------------------------------------------------------------------
  
  cat("\n🚀 Iniciando forecasting paralelo...\n")
  
  chunk_size <- if(DEBUG_MODE) {
    config$parameters$forecasting$debug_chunk_size
  } else {
    config$computation$chunk_size
  }
  
  n_chunks <- ceiling(n_elegiveis / chunk_size)
  
  cat(sprintf("   - Dividindo %s materiais em %d chunks de ~%d\n",
              format(n_elegiveis, big.mark = ","),
              n_chunks, chunk_size))
  cat(sprintf("   - Workers paralelos: %d\n", 
              config$computation$n_cores))
  cat(sprintf("   - Métodos por material: %d\n\n", length(metodos_prob_adida)))
  
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
  
  # ---------------------------------------------------------------------------
  ## 2.5. ESTATÍSTICAS DE EXECUÇÃO ####
  # ---------------------------------------------------------------------------
  
  cat("\n📊 Estatísticas de execução:\n")
  cat(sprintf("   - Materiais processados: %s\n",
              format(length(forecasts_origem), big.mark = ",")))
  cat(sprintf("   - Tempo total: %.1f seg (%.1f min)\n",
              tempo_total$toc - tempo_total$tic,
              (tempo_total$toc - tempo_total$tic) / 60))
  cat(sprintf("   - Tempo médio/material: %.2f seg\n",
              (tempo_total$toc - tempo_total$tic) / n_elegiveis))
  
  # ---------------------------------------------------------------------------
  ## 2.6. VALIDAÇÃO E CONVERGÊNCIA ####
  # ---------------------------------------------------------------------------
  
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
  
  convergence_summary %>%
    mutate(origem = origem_nome) %>%
    write_xlsx(
      here("output/reports/04c_prob_adida",
           sprintf("convergence_%s.xlsx", origem_nome))
    )
  
  # ---------------------------------------------------------------------------
  ## 2.7. CHECKPOINT ####
  # ---------------------------------------------------------------------------
  
  forecasts_probabilistic[[origem_nome]] <- list(
    metadata = origem_split$metadata,
    forecasts = forecasts_origem,
    convergence_summary = convergence_summary,
    execution_stats = list(
      n_materiais = n_elegiveis,
      debug_mode = DEBUG_MODE,
      service_level = service_level,
      tempo_total_sec = tempo_total$toc - tempo_total$tic,
      timestamp = Sys.time()
    )
  )
  
  checkpoint_file <- if(DEBUG_MODE) {
    sprintf("probabilistic_%s_DEBUG.rds", origem_nome)
  } else {
    sprintf("probabilistic_%s.rds", origem_nome)
  }
  
  saveRDS(
    forecasts_probabilistic[[origem_nome]],
    here("output/checkpoints", checkpoint_file)
  )
  
  cat(sprintf("\n✅ Checkpoint salvo: %s\n", checkpoint_file))
  
  toc()
  
}  # FIM DO LOOP SOBRE ORIGENS

# ===========================================================================
# BLOCO 3: CONSOLIDAÇÃO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: CONSOLIDAÇÃO FINAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Consolidando resultados de todas as origens", "INFO")

output_file <- if(DEBUG_MODE) {
  "forecasts_probabilistic_DEBUG.rds"
} else {
  "forecasts_probabilistic.rds"
}

saveRDS(
  forecasts_probabilistic,
  here("output/forecasts/probabilistic", output_file)
)

cat(sprintf("✅ Forecasts salvos: %s\n", output_file))

# ===========================================================================
## RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎉 FORECASTING PROBABILÍSTICO E ADIDA CONCLUÍDO! 🎉\n")
cat(strrep("=", 70), "\n\n")

cat("📋 RESUMO FINAL:\n\n")

total_materiais <- sum(map_int(forecasts_probabilistic, 
                               ~.x$execution_stats$n_materiais))

cat(sprintf("✅ Origens processadas: %d\n", length(forecasts_probabilistic)))
cat(sprintf("📊 Total de materiais: %s\n", 
            format(total_materiais, big.mark = ",")))
cat(sprintf("🎲 Service level: %.0f%%\n", service_level * 100))

cat("\n📁 Arquivos gerados:\n")
cat(sprintf("   - %s\n", output_file))
cat("   - output/reports/04c_prob_adida/convergence_*.xlsx\n")
cat("   - output/checkpoints/probabilistic_*.rds\n")

if(DEBUG_MODE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ⚠️  ATENÇÃO: Resultados em MODO DEBUG                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
}

log_message("========================================", "INFO")
log_message("FORECASTING PROBABILÍSTICO FINALIZADO", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script finalizado em:", format(Sys.time()), "\n")

