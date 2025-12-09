# 05 - CONSOLIDAÇÃO DE RESULTADOS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Consolida forecasts das 3 famílias, calcula métricas de erro
#            e prepara dados para análise comparativa
# Data: 2025-12-08
# Versão: 1.0.0
#
# OBJETIVOS:
# 1. Consolidar forecasts_baseline + intermittent + probabilistic
# 2. Calcular métricas de erro (MAE, RMSE, Bias, LinLin, MAD/Mean, PER)
# 3. Gerar perspectivas mensal e anual agregada
# 4. Preparar tabelas para análise estatística

# ===========================================================================
# BLOCO 0: SETUP ####
# ===========================================================================

library(here)
library(tidyverse)
library(furrr)
library(progressr)
library(writexl)
library(tictoc)

source(here("R/utils/load_config.R"))
source(here("R/functions/error_metrics.R"))

# Configurar progresso
if(interactive()) {
  handlers(handler_cli(clear = FALSE))
} else {
  handlers(handler_txtprogressbar())
}
handlers(global = TRUE)

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO CONSOLIDAÇÃO DE RESULTADOS", "INFO")
log_message("========================================", "INFO")

# Criar diretórios
dir.create(here("output/forecasts"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/reports"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/tables"), showWarnings = FALSE, recursive = TRUE)

cat("\n📁 Diretórios de output criados\n")

# Configurar paralelização
if(config$computation$parallel) {
  plan(multisession, workers = config$computation$n_cores)
  log_message(sprintf("Paralelização ativada: %d cores", 
                      config$computation$n_cores), "INFO")
} else {
  plan(sequential)
}

# ===========================================================================
# BLOCO 1: CARREGAMENTO DOS FORECASTS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: CARREGAMENTO DE FORECASTS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando forecasts das 3 famílias", "INFO")

# Validar existência dos arquivos
arquivos_necessarios <- c(
  "baseline" = here("output/forecasts/baseline/forecasts_baseline.rds"),
  "intermittent" = here("output/forecasts/intermittent/forecasts_intermittent.rds"),
  "probabilistic" = here("output/forecasts/probabilistic/forecasts_probabilistic.rds")
)

arquivos_faltantes <- arquivos_necessarios[!file.exists(arquivos_necessarios)]

if(length(arquivos_faltantes) > 0) {
  cat("❌ ERRO: Arquivos de forecast não encontrados:\n")
  for(f in arquivos_faltantes) {
    cat(sprintf("   - %s\n", basename(f)))
  }
  stop("Execute os scripts 04a, 04b e 04c antes de consolidar resultados.")
}

# Carregar forecasts por família
tic("Carregamento de forecasts")

forecasts_baseline <- readRDS(arquivos_necessarios["baseline"])
forecasts_intermittent <- readRDS(arquivos_necessarios["intermittent"])
forecasts_probabilistic <- readRDS(arquivos_necessarios["probabilistic"])

toc()

cat("\n✅ Forecasts carregados:\n")
cat(sprintf("   - Baseline: %d origens\n", length(forecasts_baseline)))
cat(sprintf("   - Intermittent: %d origens\n", length(forecasts_intermittent)))
cat(sprintf("   - Probabilistic: %d origens\n", length(forecasts_probabilistic)))

# Validar consistência entre origens
origens_baseline <- names(forecasts_baseline)
origens_intermittent <- names(forecasts_intermittent)
origens_probabilistic <- names(forecasts_probabilistic)

if(!identical(origens_baseline, origens_intermittent) ||
   !identical(origens_baseline, origens_probabilistic)) {
  warning("Origens inconsistentes entre famílias de métodos")
  cat("\n⚠️  ATENÇÃO: Origens inconsistentes detectadas\n")
  cat("   Baseline:", paste(origens_baseline, collapse = ", "), "\n")
  cat("   Intermittent:", paste(origens_intermittent, collapse = ", "), "\n")
  cat("   Probabilistic:", paste(origens_probabilistic, collapse = ", "), "\n\n")
}

# Usar interseção de origens válidas
origens_validas <- intersect(
  origens_baseline,
  intersect(origens_intermittent, origens_probabilistic)
)

cat(sprintf("\n📊 Origens para consolidação: %d\n", length(origens_validas)))
cat(sprintf("   %s\n", paste(origens_validas, collapse = ", ")))

# ===========================================================================
# BLOCO 2: CARREGAMENTO DOS DADOS DE TESTE ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: CARREGAMENTO DOS DADOS DE TESTE\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando dados test para comparação", "INFO")

splits_list <- readRDS(
  here(config$paths$data$processed, "train_test_splits.rds")
)

cat(sprintf("✅ Train/test splits carregados: %d origens\n", 
            length(splits_list)))

# ===========================================================================
# BLOCO 3: CONSOLIDAÇÃO POR ORIGEM ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: CONSOLIDAÇÃO POR ORIGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Consolidando forecasts por origem", "INFO")

tic("Consolidação por origem")

# Estrutura consolidada
forecasts_consolidados <- list()

with_progress({
  
  p <- progressor(steps = length(origens_validas))
  
  for(origem_nome in origens_validas) {
    
    p(sprintf("Consolidando %s", origem_nome))
    
    # Extrair test_data
    test_data <- splits_list[[origem_nome]]$test
    
    # =========================================================================
    # CORREÇÃO CRÍTICA: Acessar estrutura correta dos forecasts
    # =========================================================================
    
    # Os forecasts são salvos como lista de objetos, cada um com:
    # - cd_material
    # - forecasts (lista de métodos)
    
    fc_baseline_list <- forecasts_baseline[[origem_nome]]$forecasts
    fc_intermittent_list <- forecasts_intermittent[[origem_nome]]$forecasts
    fc_probabilistic_list <- forecasts_probabilistic[[origem_nome]]$forecasts
    
    # Criar índices por cd_material (caso as listas não tenham nomes)
    
    # Baseline
    if(is.null(names(fc_baseline_list))) {
      materiais_baseline <- map_chr(fc_baseline_list, ~.x$cd_material)
      names(fc_baseline_list) <- materiais_baseline
    }
    materiais_baseline <- names(fc_baseline_list)
    
    # Intermittent
    if(is.null(names(fc_intermittent_list))) {
      materiais_intermittent <- map_chr(fc_intermittent_list, ~.x$cd_material)
      names(fc_intermittent_list) <- materiais_intermittent
    }
    materiais_intermittent <- names(fc_intermittent_list)
    
    # Probabilistic
    if(is.null(names(fc_probabilistic_list))) {
      materiais_probabilistic <- map_chr(fc_probabilistic_list, ~.x$cd_material)
      names(fc_probabilistic_list) <- materiais_probabilistic
    }
    materiais_probabilistic <- names(fc_probabilistic_list)
    
    # União de todos os materiais
    todos_materiais <- union(
      materiais_baseline,
      union(materiais_intermittent, materiais_probabilistic)
    )
    
    # Consolidar por material
    forecasts_por_material <- map(todos_materiais, function(mat) {
      
      # Valores reais do teste
      valores_reais <- test_data %>%
        filter(cd_material == mat) %>%
        arrange(data_competencia) %>%
        pull(qt_total)
      
      # Validar que temos 12 meses
      if(length(valores_reais) != 12) {
        warning(sprintf("Material %s tem %d meses (esperado: 12)", 
                        mat, length(valores_reais)))
      }
      
      # Consolidar todos os métodos
      metodos_consolidados <- list()
      
      # Baseline - acessar $forecasts dentro do objeto do material
      if(mat %in% names(fc_baseline_list)) {
        metodos_baseline <- fc_baseline_list[[mat]]$forecasts
        # Normalizar nomes dos métodos (lowercase → PascalCase)
        names(metodos_baseline) <- str_to_title(names(metodos_baseline))
        # Ajustes específicos
        names(metodos_baseline) <- str_replace_all(names(metodos_baseline), 
                                                   c("Ma_36" = "MA", 
                                                     "Hw_Add" = "HW_Additive",
                                                     "Hw_Mult" = "HW_Multiplicative",
                                                     "Tslm" = "TSLM"))
        metodos_consolidados <- c(metodos_consolidados, metodos_baseline)
      }
      
      # Intermittent
      if(mat %in% names(fc_intermittent_list)) {
        metodos_intermittent <- fc_intermittent_list[[mat]]$forecasts
        # Normalizar nomes
        names(metodos_intermittent) <- str_to_title(names(metodos_intermittent))
        names(metodos_intermittent) <- str_replace_all(names(metodos_intermittent),
                                                       c("Sba" = "SBA",
                                                         "Tsb" = "TSB"))
        metodos_consolidados <- c(metodos_consolidados, metodos_intermittent)
      }
      
      # Probabilistic
      if(mat %in% names(fc_probabilistic_list)) {
        metodos_probabilistic <- fc_probabilistic_list[[mat]]$forecasts
        # Normalizar nomes
        names(metodos_probabilistic) <- str_to_title(names(metodos_probabilistic))
        names(metodos_probabilistic) <- str_replace_all(names(metodos_probabilistic),
                                                        c("Adida" = "ADIDA"))
        metodos_consolidados <- c(metodos_consolidados, metodos_probabilistic)
      }
      
      list(
        cd_material = mat,
        valores_reais = valores_reais,
        forecasts = metodos_consolidados
      )
    })
    
    names(forecasts_por_material) <- todos_materiais
    
    # Armazenar com metadados
    forecasts_consolidados[[origem_nome]] <- list(
      metadata = splits_list[[origem_nome]]$metadata,
      forecasts = forecasts_por_material,
      n_materiais = length(todos_materiais),
      n_metodos = length(unique(unlist(map(forecasts_por_material, 
                                           ~names(.x$forecasts)))))
    )
  }
})

toc()

cat("\n✅ Consolidação concluída\n")
cat(sprintf("   - Origens consolidadas: %d\n", length(forecasts_consolidados)))
cat(sprintf("   - Total de combinações origem-material: %s\n",
            format(sum(map_int(forecasts_consolidados, ~.x$n_materiais)), 
                   big.mark = ",")))

# Verificar se temos materiais
total_materiais <- sum(map_int(forecasts_consolidados, ~.x$n_materiais))

if(total_materiais == 0) {
  cat("\n❌ ERRO: Nenhum material consolidado!\n")
  cat("   Execute diagnostico_estrutura_forecasts.R para investigar.\n")
  stop("Consolidação falhou: zero materiais processados")
}

cat(sprintf("   - Métodos únicos consolidados: %d\n",
            max(map_int(forecasts_consolidados, ~.x$n_metodos))))

# ===========================================================================
# BLOCO 4: CÁLCULO DE MÉTRICAS DE ERRO (PERSPECTIVA MENSAL) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: CÁLCULO DE MÉTRICAS DE ERRO - PERSPECTIVA MENSAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Calculando métricas mensais", "INFO")

tic("Cálculo de métricas mensais")

# Parâmetro LinLin (penalidade para underestimation)
p_linlin <- 0.85

# Calcular métricas para cada origem-material-método
metricas_mensais <- map_dfr(names(forecasts_consolidados), function(origem_nome) {
  
  origem_data <- forecasts_consolidados[[origem_nome]]
  
  map_dfr(names(origem_data$forecasts), function(mat) {
    
    mat_data <- origem_data$forecasts[[mat]]
    valores_reais <- mat_data$valores_reais
    
    # Classificação SBC do material
    sbc_class <- splits_list[[origem_nome]]$sbc_classification %>%
      filter(cd_material == mat) %>%
      pull(sbc_category)
    
    if(length(sbc_class) == 0) sbc_class <- NA_character_
    
    map_dfr(names(mat_data$forecasts), function(metodo) {
      
      fc <- mat_data$forecasts[[metodo]]
      previsoes <- fc$point
      
      # Validar comprimentos
      validacao <- validar_vetores_metricas(valores_reais, previsoes)
      
      if(!validacao$valido) {
        warning(sprintf("Validação falhou: %s - %s - %s: %s", 
                        origem_nome, mat, metodo,
                        paste(validacao$erros, collapse = "; ")))
        return(NULL)
      }
      
      # Calcular todas as métricas
      metricas <- calculate_all_metrics(valores_reais, previsoes, p = p_linlin)
      
      # Montar tibble de resultado
      tibble(
        origem = origem_nome,
        cd_material = mat,
        sbc_category = sbc_class,
        metodo = metodo,
        familia = categorizar_familia_metodo(metodo),
        convergence = fc$convergence,
        
        # Métricas mensais
        mae_mensal = metricas$mae,
        rmse_mensal = metricas$rmse,
        bias_mensal = metricas$bias,
        linlin_mensal = metricas$linlin,
        mad_mean_ratio = metricas$mad_mean_ratio,
        per = metricas$per,
        
        # Demanda total (para agregação anual)
        demanda_real_total = metricas$demanda_real_total,
        demanda_prevista_total = metricas$demanda_prevista_total,
        
        # Estatísticas descritivas
        n_meses = metricas$n_obs,
        n_zeros_real = metricas$n_zeros_real,
        n_zeros_pred = metricas$n_zeros_pred
      )
    })
  })
})

toc()

cat("\n✅ Métricas mensais calculadas\n")
cat(sprintf("   - Total de linhas: %s\n", 
            format(nrow(metricas_mensais), big.mark = ",")))
cat(sprintf("   - Métodos únicos: %d\n", 
            n_distinct(metricas_mensais$metodo)))
cat(sprintf("   - Materiais únicos: %s\n",
            format(n_distinct(metricas_mensais$cd_material), big.mark = ",")))

# Verificar convergência global
cat("\n📊 Taxa de convergência por família:\n")
metricas_mensais %>%
  group_by(familia) %>%
  summarise(
    n_total = n(),
    n_converged = sum(convergence),
    taxa_sucesso = n_converged / n_total * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(taxa_sucesso)) %>%
  print()

# ===========================================================================
# BLOCO 5: PERSPECTIVA ANUAL AGREGADA ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 5: PERSPECTIVA ANUAL AGREGADA\n")
cat(strrep("=", 70), "\n\n")

log_message("Calculando métricas anuais agregadas", "INFO")

tic("Cálculo de métricas anuais")

metricas_anuais <- metricas_mensais %>%
  group_by(origem, cd_material, sbc_category, metodo, familia, convergence) %>%
  summarise(
    # Agregar demanda em 12 meses
    demanda_real_anual = sum(demanda_real_total, na.rm = TRUE),
    demanda_prevista_anual = sum(demanda_prevista_total, na.rm = TRUE),
    
    # Calcular erro na perspectiva anual
    erro_absoluto_anual = abs(demanda_real_anual - demanda_prevista_anual),
    
    erro_percentual_anual = if_else(
      demanda_real_anual == 0,
      NA_real_,
      (demanda_prevista_anual - demanda_real_anual) / demanda_real_anual * 100
    ),
    
    # Classificar tipo de erro
    tipo_erro_anual = case_when(
      demanda_prevista_anual > demanda_real_anual ~ "Superestimacao",
      demanda_prevista_anual < demanda_real_anual ~ "Subestimacao",
      TRUE ~ "Exato"
    ),
    
    # Métricas médias mensais (para referência)
    mae_mensal_medio = mean(mae_mensal, na.rm = TRUE),
    rmse_mensal_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_mensal_medio = mean(bias_mensal, na.rm = TRUE),
    linlin_mensal_medio = mean(linlin_mensal, na.rm = TRUE),
    
    # Estatísticas descritivas
    n_zeros_real_total = sum(n_zeros_real),
    n_zeros_pred_total = sum(n_zeros_pred),
    
    .groups = 'drop'
  )

toc()

cat("\n✅ Métricas anuais agregadas calculadas\n")
cat(sprintf("   - Total de linhas: %s\n", 
            format(nrow(metricas_anuais), big.mark = ",")))

# Resumo de erros anuais por tipo
cat("\n📊 Distribuição de tipos de erro anual:\n")
metricas_anuais %>%
  count(tipo_erro_anual) %>%
  mutate(prop = n / sum(n) * 100) %>%
  print()

# ===========================================================================
# BLOCO 6: ESTATÍSTICAS DESCRITIVAS GLOBAIS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6: ESTATÍSTICAS DESCRITIVAS GLOBAIS\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando estatísticas descritivas", "INFO")

# Resumo por método
resumo_por_metodo <- metricas_mensais %>%
  group_by(metodo, familia) %>%
  summarise(
    n_previsoes = n(),
    n_convergidas = sum(convergence),
    taxa_convergencia = n_convergidas / n_previsoes * 100,
    
    # Métricas médias
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    mae_mediano = median(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    linlin_medio = mean(linlin_mensal, na.rm = TRUE),
    
    # Desvios padrão
    mae_sd = sd(mae_mensal, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(familia, mae_medio)

cat("\n📊 Top 10 métodos por MAE médio:\n")
resumo_por_metodo %>%
  select(metodo, familia, mae_medio, taxa_convergencia) %>%
  head(10) %>%
  print()

# Resumo por categoria SBC
resumo_por_sbc <- metricas_mensais %>%
  filter(!is.na(sbc_category)) %>%
  group_by(sbc_category, metodo) %>%
  summarise(
    n_previsoes = n(),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n📊 Desempenho por categoria SBC (amostra):\n")
resumo_por_sbc %>%
  group_by(sbc_category) %>%
  slice_min(mae_medio, n = 2) %>%
  select(sbc_category, metodo, mae_medio) %>%
  print()

# ===========================================================================
# BLOCO 7: SALVAR RESULTADOS CONSOLIDADOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 7: SALVAMENTO DE RESULTADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Salvando resultados consolidados", "INFO")

tic("Salvamento de resultados")

# -----------------------------------------------------------------------------
## 7.1. Salvar objeto consolidado (RDS) ####
# -----------------------------------------------------------------------------

consolidado_completo <- list(
  forecasts = forecasts_consolidados,
  metricas_mensais = metricas_mensais,
  metricas_anuais = metricas_anuais,
  resumo_por_metodo = resumo_por_metodo,
  resumo_por_sbc = resumo_por_sbc,
  
  # Metadados
  metadata = list(
    n_origens = length(forecasts_consolidados),
    n_materiais_total = n_distinct(metricas_mensais$cd_material),
    n_metodos = n_distinct(metricas_mensais$metodo),
    p_linlin = p_linlin,
    timestamp = Sys.time(),
    config_version = config$project$version
  )
)

saveRDS(
  consolidado_completo,
  here("output/forecasts/forecasts_consolidated.rds")
)

cat("✅ Objeto consolidado salvo: forecasts_consolidated.rds\n")

# -----------------------------------------------------------------------------
## 7.2. Salvar métricas em Excel ####
# -----------------------------------------------------------------------------

# Preparar sheets para Excel
sheets_excel <- list(
  "Metricas_Mensais" = metricas_mensais %>%
    select(origem, cd_material, sbc_category, metodo, familia,
           mae_mensal, rmse_mensal, bias_mensal, linlin_mensal,
           mad_mean_ratio, per, convergence),
  
  "Metricas_Anuais" = metricas_anuais %>%
    select(origem, cd_material, sbc_category, metodo, familia,
           demanda_real_anual, demanda_prevista_anual,
           erro_absoluto_anual, erro_percentual_anual, 
           tipo_erro_anual, convergence),
  
  "Resumo_Por_Metodo" = resumo_por_metodo,
  
  "Resumo_Por_SBC" = resumo_por_sbc %>%
    group_by(sbc_category) %>%
    slice_min(mae_medio, n = 5) %>%
    ungroup()
)

write_xlsx(
  sheets_excel,
  here("output/reports/05_consolidated_metrics.xlsx")
)

cat("✅ Métricas em Excel: 05_consolidated_metrics.xlsx\n")

# -----------------------------------------------------------------------------
## 7.3. Salvar tabelas em CSV (para análise em Python/outros) ####
# -----------------------------------------------------------------------------

write_csv(
  metricas_mensais,
  here("output/tables/metricas_mensais.csv")
)

write_csv(
  metricas_anuais,
  here("output/tables/metricas_anuais.csv")
)

write_csv(
  resumo_por_metodo,
  here("output/tables/resumo_por_metodo.csv")
)

cat("✅ Tabelas CSV salvas em output/tables/\n")

toc()

# ===========================================================================
# BLOCO 8: RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL DE CONSOLIDAÇÃO\n")
cat(strrep("=", 70), "\n\n")

cat("📋 RESUMO DA CONSOLIDAÇÃO:\n\n")

cat(sprintf("✅ Origens processadas: %d\n", 
            consolidado_completo$metadata$n_origens))
cat(sprintf("📊 Total de materiais: %s\n", 
            format(consolidado_completo$metadata$n_materiais_total, 
                   big.mark = ",")))
cat(sprintf("🔬 Total de métodos: %d\n", 
            consolidado_completo$metadata$n_metodos))
cat(sprintf("📈 Total de previsões: %s\n",
            format(nrow(metricas_mensais), big.mark = ",")))

cat("\n📁 Arquivos gerados:\n")
cat("   - output/forecasts/forecasts_consolidated.rds\n")
cat("   - output/reports/05_consolidated_metrics.xlsx\n")
cat("   - output/tables/metricas_mensais.csv\n")
cat("   - output/tables/metricas_anuais.csv\n")
cat("   - output/tables/resumo_por_metodo.csv\n")

cat("\n🎯 PRÓXIMAS ETAPAS:\n")
cat("   1. Executar script 06_analyze_results.R para análise estatística\n")
cat("   2. Gerar visualizações comparativas\n")
cat("   3. Realizar testes de significância estatística\n")

cat("\n", strrep("=", 70), "\n", sep = "")

log_message("========================================", "INFO")
log_message("CONSOLIDAÇÃO FINALIZADA COM SUCESSO", "INFO")
log_message("========================================", "INFO")

# Desligar paralelização
if(config$computation$parallel) {
  plan(sequential)
}

cat("\n✅ Script 05 finalizado em:", format(Sys.time()), "\n\n")