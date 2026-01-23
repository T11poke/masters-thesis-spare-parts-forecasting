# 04d - FORECASTING: MODELOS COM DADOS AGREGADOS ANUALMENTE ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Implementa treinamento de modelos com séries temporais agregadas
#            anualmente para comparação com abordagem mensal + agregação
# Data: 2025-01-22
# Versão: 1.0.0
#
# METODOLOGIA:
# - Agrega demanda mensal em séries anuais
# - Treina modelos com essas séries anuais
# - Gera previsão anual (h=1 ano)
# - Calcula métricas: MAE, RMSE, Bias, LinLin, MAD/Mean
# 
# MÉTODOS EXCLUÍDOS:
# - ADIDA (por definição, usa agregação temporal)

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(tsibble)
library(furrr)
library(progressr)
library(tictoc)
library(writexl)
library(forecast)
library(tsintermittent)
library(MASS)

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO FORECASTING - SÉRIES ANUAIS", "INFO")
log_message("========================================", "INFO")

# Configurar progresso
if(interactive()) {
  handlers(handler_cli(clear = FALSE))
} else {
  handlers(handler_txtprogressbar())
}
handlers(global = TRUE)

# Criar diretórios
dir.create(here("output/forecasts/annual"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/reports/04d_annual"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/checkpoints"), showWarnings = FALSE, recursive = TRUE)

# Configurar paralelização
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

# ===========================================================================
# BLOCO 1: DEFINIÇÃO DOS MÉTODOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: DEFINIÇÃO DOS MÉTODOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Definindo métodos para séries anuais", "INFO")

# ---------------------------------------------------------------------------
## 1.1. MÉTODOS BASELINE ####
# ---------------------------------------------------------------------------

metodos_baseline_annual <- list(
  
  naive = function(train_ts, h) {
    tryCatch({
      fc <- forecast::naive(train_ts, h = h)
      list(
        point = as.numeric(fc$mean),
        fitted = as.numeric(fc$fitted),
        residuals = as.numeric(fc$residuals),
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
      fc <- forecast::meanf(train_ts, h = h)
      list(
        point = as.numeric(fc$mean),
        fitted = as.numeric(fc$fitted),
        residuals = as.numeric(fc$residuals),
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
  
  ses = function(train_ts, h) {
    tryCatch({
      fc <- forecast::ses(train_ts, h = h, initial = "optimal")
      list(
        point = as.numeric(fc$mean),
        fitted = as.numeric(fc$fitted),
        residuals = as.numeric(fc$residuals),
        method = "SES",
        alpha = fc$model$par["alpha"],
        convergence = TRUE,
        error_message = NA_character_
      )
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "SES",
        alpha = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  }
)

cat(sprintf("✅ Métodos baseline definidos: %d\n", length(metodos_baseline_annual)))

# ---------------------------------------------------------------------------
## 1.2. MÉTODOS INTERMITENTES ####
# ---------------------------------------------------------------------------

metodos_intermittent_annual <- list(
  
  croston = function(train_ts, h) {
    tryCatch({
      fc <- tsintermittent::crost(train_ts, h = h, type = "croston", 
                                   cost = "mar", init = "mean")
      list(
        point = as.numeric(fc$frc.out),
        fitted = as.numeric(fc$frc.in),
        residuals = train_ts - as.numeric(fc$frc.in),
        method = "Croston",
        alpha_demand = fc$alpha[1],
        alpha_interval = fc$alpha[2],
        convergence = TRUE,
        error_message = NA_character_
      )
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "Croston",
        alpha_demand = NA_real_,
        alpha_interval = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  sba = function(train_ts, h) {
    tryCatch({
      fc <- tsintermittent::crost(train_ts, h = h, type = "sba", 
                                   cost = "mar", init = "mean")
      list(
        point = as.numeric(fc$frc.out),
        fitted = as.numeric(fc$frc.in),
        residuals = train_ts - as.numeric(fc$frc.in),
        method = "SBA",
        alpha_demand = fc$alpha[1],
        alpha_interval = fc$alpha[2],
        convergence = TRUE,
        error_message = NA_character_
      )
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "SBA",
        alpha_demand = NA_real_,
        alpha_interval = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  tsb = function(train_ts, h) {
    tryCatch({
      fc <- tsintermittent::tsb(train_ts, h = h, cost = "mar", init = "mean")
      list(
        point = as.numeric(fc$frc.out),
        fitted = as.numeric(fc$frc.in),
        residuals = train_ts - as.numeric(fc$frc.in),
        method = "TSB",
        alpha_demand = fc$alpha[1],
        alpha_interval = fc$alpha[2],
        alpha_trend = fc$alpha[3],
        beta = fc$beta,
        phi = fc$phi,
        convergence = TRUE,
        error_message = NA_character_
      )
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "TSB",
        alpha_demand = NA_real_,
        alpha_interval = NA_real_,
        alpha_trend = NA_real_,
        beta = NA_real_,
        phi = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  }
)

cat(sprintf("✅ Métodos intermitentes definidos: %d\n", 
            length(metodos_intermittent_annual)))

# ---------------------------------------------------------------------------
## 1.3. MÉTODOS PROBABILÍSTICOS ####
# ---------------------------------------------------------------------------

service_level <- config$parameters$forecasting$probabilistic$service_level

metodos_probabilistic_annual <- list(
  
  poisson = function(train_ts, h) {
    tryCatch({
      lambda_hat <- mean(train_ts, na.rm = TRUE)
      
      if(is.na(lambda_hat) || lambda_hat <= 0) {
        stop("Lambda inválido")
      }
      
      fc_point <- rep(lambda_hat, h)
      fitted_vals <- rep(lambda_hat, length(train_ts))
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "Poisson",
        lambda = lambda_hat,
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
  
  gamma = function(train_ts, h) {
    tryCatch({
      train_nonzero <- train_ts[train_ts > 0]
      
      if(length(train_nonzero) < 2) {
        stop("Dados insuficientes (< 2 valores não-zero)")
      }
      
      gamma_fit <- MASS::fitdistr(train_nonzero, "gamma")
      shape <- gamma_fit$estimate["shape"]
      rate <- gamma_fit$estimate["rate"]
      
      fc_point <- rep(shape / rate, h)
      fitted_vals <- rep(shape / rate, length(train_ts))
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "Gamma",
        shape = shape,
        rate = rate,
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

cat(sprintf("✅ Métodos probabilísticos definidos: %d\n", 
            length(metodos_probabilistic_annual)))

# Consolidar todos os métodos
metodos_annual <- c(
  metodos_baseline_annual,
  metodos_intermittent_annual,
  metodos_probabilistic_annual
)

cat(sprintf("\n📊 Total de métodos para perspectiva anual: %d\n", 
            length(metodos_annual)))
cat("   Excluídos: ADIDA (por definição usa agregação temporal)\n")

# ===========================================================================
# BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando pipeline de forecasting anual", "INFO")

h <- 1  # Horizonte de 1 ano para previsão anual

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

forecasts_annual <- list()

# ===========================================================================
## 2.1. LOOP SOBRE ORIGENS ####
# ===========================================================================

for(origem_nome in names(splits_list)) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("🔄 PROCESSANDO %s - AGREGAÇÃO ANUAL\n", toupper(origem_nome)))
  cat(strrep("=", 70), "\n\n")
  
  tic(sprintf("Tempo total - %s", origem_nome))
  
  origem_split <- splits_list[[origem_nome]]
  origem_id <- origem_split$metadata$origem_id
  
  train_data <- origem_split$train
  test_data <- origem_split$test
  sbc_classification <- origem_split$sbc_classification
  
  # ---------------------------------------------------------------------------
  ## 2.2. AGREGAR DADOS EM SÉRIES ANUAIS ####
  # ---------------------------------------------------------------------------
  
  cat("📊 Agregando dados mensais em séries anuais...\n\n")
  
  # Criar coluna de ano
  train_data_annual <- train_data %>%
    as_tibble() %>%
    mutate(ano = lubridate::year(data_competencia)) %>%
    group_by(cd_material, ano) %>%
    summarise(
      qt_anual = sum(qt_total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(cd_material, ano)
  
  test_data_annual <- test_data %>%
    as_tibble() %>%
    mutate(ano = lubridate::year(data_competencia)) %>%
    group_by(cd_material, ano) %>%
    summarise(
      qt_anual = sum(qt_total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(cd_material, ano)
  
  cat(sprintf("   - Treino: %d anos únicos\n", 
              n_distinct(train_data_annual$ano)))
  cat(sprintf("   - Teste: %d anos únicos\n", 
              n_distinct(test_data_annual$ano)))
  cat(sprintf("   - Materiais no treino: %s\n\n",
              format(n_distinct(train_data_annual$cd_material), big.mark = ",")))
  
  # ---------------------------------------------------------------------------
  ## 2.3. FILTRAR MATERIAIS ELEGÍVEIS ####
  # ---------------------------------------------------------------------------
  
  cat("🔍 Identificando materiais elegíveis...\n")
  
  materiais_elegiveis <- train_data_annual %>%
    group_by(cd_material) %>%
    summarise(
      n_anos = n(),
      n_nonzero = sum(qt_anual > 0),
      .groups = 'drop'
    ) %>%
    # Critério: pelo menos 3 anos de dados e pelo menos 1 ano com demanda
    filter(n_anos >= 3, n_nonzero >= 1) %>%
    pull(cd_material)
  
  n_elegiveis <- length(materiais_elegiveis)
  
  cat(sprintf("   ✅ Materiais elegíveis: %s (de %s no treino)\n",
              format(n_elegiveis, big.mark = ","),
              format(n_distinct(train_data_annual$cd_material), big.mark = ",")))
  
  if(DEBUG_MODE) {
    debug_n <- min(
      n_elegiveis, 
      config$parameters$forecasting$debug_n_materials
    )
    materiais_elegiveis <- sample(materiais_elegiveis, debug_n)
    n_elegiveis <- length(materiais_elegiveis)
    cat(sprintf("   🔧 DEBUG: Limitado a %d materiais\n", n_elegiveis))
  }
  
  cat("\n")
  
  # ---------------------------------------------------------------------------
  ## 2.4. FUNÇÃO DE PROCESSAMENTO POR MATERIAL ####
  # ---------------------------------------------------------------------------
  
  processar_material <- function(mat) {
    
    # Série de treino anual
    train_ts <- train_data_annual %>%
      filter(cd_material == mat) %>%
      arrange(ano) %>%
      pull(qt_anual) %>%
      ts()
    
    # Valores reais de teste (1 ano)
    test_vals <- test_data_annual %>%
      filter(cd_material == mat) %>%
      arrange(ano) %>%
      pull(qt_anual)
    
    # Se não há dados de teste, retornar NULL
    if(length(test_vals) == 0) {
      return(NULL)
    }
    
    # Obter classificação SBC (da origem mensal)
    sbc_class <- sbc_classification %>%
      filter(cd_material == mat) %>%
      pull(categoria_sbc)
    
    if(length(sbc_class) == 0) {
      sbc_class <- NA_character_
    } else {
      sbc_class <- sbc_class[1]
    }
    
    # Aplicar todos os métodos
    forecasts_mat <- map(metodos_annual, ~.x(train_ts, h = h))
    
    # Retornar estrutura consolidada
    list(
      cd_material = mat,
      sbc_classification = sbc_class,
      train_length = length(train_ts),
      test_values = test_vals,
      forecasts = forecasts_mat
    )
  }
  
  # ---------------------------------------------------------------------------
  ## 2.5. EXECUÇÃO PARALELA COM PROGRESSO ####
  # ---------------------------------------------------------------------------
  
  cat("🚀 Iniciando forecasting paralelo...\n")
  
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
  cat(sprintf("   - Métodos por material: %d\n\n", length(metodos_annual)))
  
  material_chunks <- split(
    materiais_elegiveis,
    ceiling(seq_along(materiais_elegiveis) / chunk_size)
  )
  
  tic("Forecasting paralelo - séries anuais")
  
  if(config$computation$parallel) {
    
    forecasts_origem <- with_progress({
      
      p <- progressor(
        steps = length(material_chunks),
        message = sprintf("Origem %s (anual)", origem_nome)
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
  
  # Remover NULLs
  forecasts_origem <- forecasts_origem[!map_lgl(forecasts_origem, is.null)]
  
  tempo_total <- toc()
  
  # ---------------------------------------------------------------------------
  ## 2.6. ESTATÍSTICAS DE EXECUÇÃO ####
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
  ## 2.7. VALIDAÇÃO E CONVERGÊNCIA ####
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
      here("output/reports/04d_annual",
           sprintf("convergence_annual_%s.xlsx", origem_nome))
    )
  
  # ---------------------------------------------------------------------------
  ## 2.8. CHECKPOINT ####
  # ---------------------------------------------------------------------------
  
  forecasts_annual[[origem_nome]] <- list(
    metadata = origem_split$metadata,
    forecasts = forecasts_origem,
    convergence_summary = convergence_summary,
    execution_stats = list(
      n_materiais = n_elegiveis,
      debug_mode = DEBUG_MODE,
      horizonte = h,
      agregacao = "anual",
      tempo_total_sec = tempo_total$toc - tempo_total$tic,
      timestamp = Sys.time()
    )
  )
  
  checkpoint_file <- if(DEBUG_MODE) {
    sprintf("annual_%s_DEBUG.rds", origem_nome)
  } else {
    sprintf("annual_%s.rds", origem_nome)
  }
  
  saveRDS(
    forecasts_annual[[origem_nome]],
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
  "forecasts_annual_DEBUG.rds"
} else {
  "forecasts_annual.rds"
}

saveRDS(
  forecasts_annual,
  here("output/forecasts/annual", output_file)
)

cat(sprintf("✅ Forecasts anuais salvos: %s\n", output_file))

# ===========================================================================
## RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎉 FORECASTING COM SÉRIES ANUAIS CONCLUÍDO! 🎉\n")
cat(strrep("=", 70), "\n\n")

cat("📋 RESUMO FINAL:\n\n")

total_materiais <- sum(map_int(forecasts_annual, 
                               ~.x$execution_stats$n_materiais))

cat(sprintf("✅ Origens processadas: %d\n", length(forecasts_annual)))
cat(sprintf("📊 Total de materiais: %s\n", 
            format(total_materiais, big.mark = ",")))
cat(sprintf("📅 Horizonte de previsão: %d ano\n", h))
cat(sprintf("🔧 Métodos aplicados: %d\n", length(metodos_annual)))

cat("\n📁 Arquivos gerados:\n")
cat(sprintf("   - %s\n", output_file))
cat("   - output/reports/04d_annual/convergence_annual_*.xlsx\n")
cat("   - output/checkpoints/annual_*.rds\n")

if(DEBUG_MODE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ⚠️  ATENÇÃO: Resultados em MODO DEBUG                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
}

log_message("========================================", "INFO")
log_message("FORECASTING ANUAL FINALIZADO", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script finalizado em:", format(Sys.time()), "\n\n")

# Limpar ambiente paralelo
if(config$computation$parallel) {
  plan(sequential)
}