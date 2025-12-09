# 04b - PREVISÃO: MODELOS ESPECIALIZADOS EM DEMANDA INTERMITENTE ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Implementação de métodos especializados para demanda intermitente
#            com otimização de hiperparâmetros (Família 3)
# Data: 2025-12-08
# Versão: 2.0.0

# Família 3: Métodos Especializados
#   - Croston Clássico
#   - SBA (Syntetos-Boylan Approximation)
#   - TSB (Teunter-Syntetos-Babai)
#

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(tsibble)
library(furrr)
library(progressr)
library(tictoc)
library(writexl)
library(tsintermittent)

source(here("R/utils/load_config.R"))
source(here("R/functions/intermittent_functions.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO FORECASTING - MODELOS INTERMITENTES", "INFO")
log_message("========================================", "INFO")

# Configurar progresso
if(interactive()) {
  handlers(handler_cli(clear = FALSE))
} else {
  handlers(handler_txtprogressbar())
}
handlers(global = TRUE)

# Criar diretórios
dir.create(here("output/forecasts/intermittent"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/reports/04b_intermittent"), showWarnings = FALSE, recursive = TRUE)
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
# BLOCO 1: DEFINIÇÃO DOS MÉTODOS INTERMITENTES ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: DEFINIÇÃO DOS MÉTODOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Definindo métodos especializados para demanda intermitente", "INFO")

# ---------------------------------------------------------------------------
## 1.1. FUNÇÃO DE OTIMIZAÇÃO DE ALPHA ####
# ---------------------------------------------------------------------------

# Função transferida para intermittent_functions.R

# ---------------------------------------------------------------------------
## 1.2. MÉTODOS INTERMITENTES ####
# ---------------------------------------------------------------------------

metodos_intermitentes <- list(
  
  croston = function(train_ts, h, optimize_alpha = TRUE) {
    tryCatch({
      
      # Otimizar alpha se solicitado
      if(optimize_alpha) {
        # Garantir que função está disponível no worker paralelo
        if(!exists("otimizar_alpha", mode = "function")) {
          source(here::here("R/functions/intermittent_functions.R"))
        }
        alpha_opt <- otimizar_alpha(train_ts, method = "croston")
      } else {
        alpha_opt <- 0.10
      }
      
      # Ajustar modelo
      fit <- tsintermittent::crost(
        train_ts,
        h = h,
        w = alpha_opt,
        type = "croston",
        init = "mean"
      )
      
      # Extrair previsões
      fc_point <- as.numeric(fit$mean)
      fc_point <- pmax(fc_point, 0)  # Truncar negativos
      
      # Fitted values
      fitted_vals <- as.numeric(fit$fitted)
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "Croston",
        alpha = alpha_opt,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "Croston",
        alpha = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  sba = function(train_ts, h, optimize_alpha = TRUE) {
    tryCatch({
      
      if(optimize_alpha) {
        # Garantir que função está disponível no worker paralelo
        if(!exists("otimizar_alpha", mode = "function")) {
          source(here::here("R/functions/intermittent_functions.R"))
        }
        alpha_opt <- otimizar_alpha(train_ts, method = "sba")
      } else {
        alpha_opt <- 0.10
      }
      
      fit <- tsintermittent::crost(
        train_ts,
        h = h,
        w = alpha_opt,
        type = "sba",
        init = "mean"
      )
      
      fc_point <- pmax(as.numeric(fit$mean), 0)
      fitted_vals <- as.numeric(fit$fitted)
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "SBA",
        alpha = alpha_opt,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "SBA",
        alpha = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  },
  
  tsb = function(train_ts, h, optimize_alpha = TRUE) {
    tryCatch({
      
      if(optimize_alpha) {
        # Garantir que função está disponível no worker paralelo
        if(!exists("otimizar_alpha", mode = "function")) {
          source(here::here("R/functions/intermittent_functions.R"))
        }
        alpha_opt <- otimizar_alpha(train_ts, method = "tsb")
      } else {
        alpha_opt <- 0.10
      }
      
      fit <- tsintermittent::crost(
        train_ts,
        h = h,
        w = alpha_opt,
        type = "tsb",
        init = "mean"
      )
      
      fc_point <- pmax(as.numeric(fit$mean), 0)
      fitted_vals <- as.numeric(fit$fitted)
      residuals_vals <- train_ts - fitted_vals
      
      list(
        point = fc_point,
        fitted = fitted_vals,
        residuals = residuals_vals,
        method = "TSB",
        alpha = alpha_opt,
        convergence = TRUE,
        error_message = NA_character_
      )
      
    }, error = function(e) {
      list(
        point = rep(NA_real_, h),
        fitted = rep(NA_real_, length(train_ts)),
        residuals = rep(NA_real_, length(train_ts)),
        method = "TSB",
        alpha = NA_real_,
        convergence = FALSE,
        error_message = conditionMessage(e)
      )
    })
  }
)

cat("✅ Métodos intermitentes definidos:\n")
cat(sprintf("   - Total de métodos: %d\n", length(metodos_intermitentes)))
cat("   - Métodos: Croston, SBA, TSB\n")
cat(sprintf("   - Otimização de alpha: %s\n", 
            ifelse(config$parameters$forecasting$intermittent$optimize_alpha, 
                   "ATIVADA", "DESATIVADA")))
cat(sprintf("   - Grid de alphas: [%s]\n", 
            paste(config$parameters$forecasting$intermittent$alphas_grid, 
                  collapse = ", ")))

# ===========================================================================
# BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: EXECUÇÃO DE FORECASTS POR ORIGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando pipeline de forecasting intermitente", "INFO")

h <- config$parameters$forecasting$horizon
optimize_alpha <- config$parameters$forecasting$intermittent$optimize_alpha

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

# Estrutura para armazenar forecasts
forecasts_intermittent <- list()

# Lista para consolidar alphas otimizados
alphas_otimizados_todas_origens <- list()

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
  
  # ---------------------------------------------------------------------------
  ## 2.2. FILTRAR MATERIAIS INTERMITENTES/LUMPY ####
  # ---------------------------------------------------------------------------
  
  cat("\n🎯 Filtrando materiais com demanda intermitente/lumpy...\n")
  
  materiais_intermitentes <- sbc_classification %>%
    filter(categoria_sbc %in% c("Intermittent", "Lumpy")) %>%
    pull(cd_material)
  
  # Aplicar filtro de mínimo de ocorrências
  materiais_elegiveis <- train_data %>%
    as_tibble() %>%
    filter(cd_material %in% materiais_intermitentes) %>%
    group_by(cd_material) %>%
    summarise(
      n_nonzero = sum(qt_total > 0),
      .groups = 'drop'
    ) %>%
    filter(n_nonzero >= config$parameters$data_cleaning$min_occurrences) %>%
    pull(cd_material)
  
  n_elegiveis_original <- length(materiais_elegiveis)
  
  cat(sprintf("   - Materiais Intermittent/Lumpy: %s\n",
              format(length(materiais_intermitentes), big.mark = ",")))
  cat(sprintf("   - Após filtro ≥%d ocorrências: %s\n",
              config$parameters$data_cleaning$min_occurrences,
              format(n_elegiveis_original, big.mark = ",")))
  
  # ---------------------------------------------------------------------------
  ## 2.3. APLICAR MODO DEBUG ####
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
      cat("   Estratégia: Amostragem estratificada (Intermittent vs Lumpy)\n")
      
      categorias_disponiveis <- sbc_elegiveis %>%
        count(categoria_sbc, name = "n_disp")
      
      n_categorias <- nrow(categorias_disponiveis)
      n_por_categoria <- ceiling(n_debug / n_categorias)
      
      cat(sprintf("   - Categorias: %d\n", n_categorias))
      cat(sprintf("   - Alvo por categoria: %d materiais\n\n", n_por_categoria))
      
      set.seed(config$parameters$seed)
      
      # Amostragem estratificada manual
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
      
      # Relatório
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
  ## 2.4. FUNÇÃO PARA PROCESSAR UM MATERIAL ####
  # ---------------------------------------------------------------------------
  
  processar_material <- function(cd_mat) {
    
    # Extrair série temporal de treino
    serie_train <- train_data %>%
      filter(cd_material == cd_mat) %>%
      arrange(data_competencia) %>%
      pull(qt_total)
    
    train_ts <- ts(serie_train, frequency = 12)
    
    # Obter classificação SBC
    sbc_info <- sbc_classification %>%
      filter(cd_material == cd_mat)
    
    # Aplicar todos os métodos
    forecasts_material <- map(metodos_intermitentes, function(metodo_func) {
      tic()
      resultado <- metodo_func(train_ts, h = h, optimize_alpha = optimize_alpha)
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
  
  # ---------------------------------------------------------------------------
  ## 2.5. EXECUÇÃO PARALELA COM PROGRESSO ####
  # ---------------------------------------------------------------------------
  
  cat("\n🚀 Iniciando forecasting paralelo...\n")
  
  # Exportar função para workers paralelos
  plan(multisession, workers = config$computation$n_cores)
  
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
  cat(sprintf("   - Métodos por material: %d\n\n", length(metodos_intermitentes)))
  
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
  ## 2.6. EXTRAIR E SALVAR ALPHAS OTIMIZADOS ####
  # ---------------------------------------------------------------------------
  
  if(optimize_alpha) {
    
    cat("\n📊 Extraindo alphas otimizados...\n")
    
    alphas_otimizados <- map_dfr(forecasts_origem, function(mat_forecast) {
      map_dfr(names(mat_forecast$forecasts), function(metodo_nome) {
        tibble(
          cd_material = mat_forecast$cd_material,
          origem_id = mat_forecast$origem_id,
          categoria_sbc = mat_forecast$categoria_sbc,
          adi = mat_forecast$adi,
          cv2 = mat_forecast$cv2,
          metodo = metodo_nome,
          alpha = mat_forecast$forecasts[[metodo_nome]]$alpha
        )
      })
    })
    
    # Salvar para esta origem
    alphas_otimizados %>%
      write_xlsx(
        here("output/reports/04b_intermittent",
             sprintf("alphas_optimized_%s.xlsx", origem_nome))
      )
    
    cat(sprintf("   ✅ Alphas salvos: alphas_optimized_%s.xlsx\n", origem_nome))
    
    # Adicionar à lista consolidada
    alphas_otimizados_todas_origens[[origem_nome]] <- alphas_otimizados
    
    # Estatísticas de alphas
    cat("\n📈 Estatísticas dos alphas otimizados:\n")
    
    alphas_summary <- alphas_otimizados %>%
      group_by(metodo) %>%
      summarise(
        n_materiais = n(),
        alpha_min = min(alpha, na.rm = TRUE),
        alpha_q25 = quantile(alpha, 0.25, na.rm = TRUE),
        alpha_mediana = median(alpha, na.rm = TRUE),
        alpha_q75 = quantile(alpha, 0.75, na.rm = TRUE),
        alpha_max = max(alpha, na.rm = TRUE),
        alpha_media = mean(alpha, na.rm = TRUE),
        .groups = 'drop'
      )
    
    print(alphas_summary)
  }
  
  # ---------------------------------------------------------------------------
  ## 2.7. ESTATÍSTICAS DE EXECUÇÃO ####
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
  ## 2.8. VALIDAÇÃO E CONVERGÊNCIA ####
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
  
  # Salvar resumo
  convergence_summary %>%
    mutate(origem = origem_nome) %>%
    write_xlsx(
      here("output/reports/04b_intermittent",
           sprintf("convergence_%s.xlsx", origem_nome))
    )
  
  # ---------------------------------------------------------------------------
  ## 2.9. CHECKPOINT ####
  # ---------------------------------------------------------------------------
  
  forecasts_intermittent[[origem_nome]] <- list(
    metadata = origem_split$metadata,
    forecasts = forecasts_origem,
    convergence_summary = convergence_summary,
    alphas_otimizados = if(optimize_alpha) alphas_otimizados else NULL,
    execution_stats = list(
      n_materiais = n_elegiveis,
      debug_mode = DEBUG_MODE,
      optimize_alpha = optimize_alpha,
      tempo_total_sec = tempo_total$toc - tempo_total$tic,
      timestamp = Sys.time()
    )
  )
  
  checkpoint_file <- if(DEBUG_MODE) {
    sprintf("intermittent_%s_DEBUG.rds", origem_nome)
  } else {
    sprintf("intermittent_%s.rds", origem_nome)
  }
  
  saveRDS(
    forecasts_intermittent[[origem_nome]],
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

# Salvar forecasts consolidados
output_file <- if(DEBUG_MODE) {
  "forecasts_intermittent_DEBUG.rds"
} else {
  "forecasts_intermittent.rds"
}

saveRDS(
  forecasts_intermittent,
  here("output/forecasts/intermittent", output_file)
)

cat(sprintf("✅ Forecasts salvos: %s\n", output_file))

# Consolidar alphas de todas as origens
if(optimize_alpha && length(alphas_otimizados_todas_origens) > 0) {
  
  alphas_consolidados <- bind_rows(alphas_otimizados_todas_origens, .id = "origem")
  
  alphas_consolidados %>%
    write_xlsx(
      here("output/reports/04b_intermittent", "alphas_all_origins.xlsx")
    )
  
  cat("✅ Alphas consolidados salvos: alphas_all_origins.xlsx\n")
  
  # Estatísticas globais
  cat("\n📊 Estatísticas globais de alphas:\n")
  
  alphas_global_summary <- alphas_consolidados %>%
    group_by(metodo, categoria_sbc) %>%
    summarise(
      n_materiais = n(),
      alpha_mediana = median(alpha, na.rm = TRUE),
      alpha_media = mean(alpha, na.rm = TRUE),
      alpha_sd = sd(alpha, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print(alphas_global_summary)
}

# ===========================================================================
## RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎉 FORECASTING INTERMITENTE CONCLUÍDO! 🎉\n")
cat(strrep("=", 70), "\n\n")

cat("📋 RESUMO FINAL:\n\n")

total_materiais <- sum(map_int(forecasts_intermittent, 
                               ~.x$execution_stats$n_materiais))

cat(sprintf("✅ Origens processadas: %d\n", length(forecasts_intermittent)))
cat(sprintf("📊 Total de materiais: %s\n", 
            format(total_materiais, big.mark = ",")))
cat(sprintf("🔧 Otimização de alpha: %s\n", 
            ifelse(optimize_alpha, "ATIVADA", "DESATIVADA")))

cat("\n📁 Arquivos gerados:\n")
cat(sprintf("   - %s\n", output_file))
cat("   - output/reports/04b_intermittent/alphas_*.xlsx\n")
cat("   - output/reports/04b_intermittent/convergence_*.xlsx\n")
cat("   - output/checkpoints/intermittent_*.rds\n")

if(DEBUG_MODE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ⚠️  ATENÇÃO: Resultados em MODO DEBUG                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
}

log_message("========================================", "INFO")
log_message("FORECASTING INTERMITENTE FINALIZADO", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script finalizado em:", format(Sys.time()), "\n")

