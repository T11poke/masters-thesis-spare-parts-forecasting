# 05 - CONSOLIDAÇÃO DE RESULTADOS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Consolida forecasts das 3 famílias, calcula métricas de erro
#            e prepara dados para análise comparativa
# Data: 2025-12-08
# Versão: 1.0.3
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
    
    # CORREÇÃO CRÍTICA: Acessar estrutura correta dos forecasts

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
      pull(categoria_sbc)
    
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
        categoria_sbc = sbc_class,
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

## 5.1. Agregar demanda em perspectiva anual ####

cat("📊 5.1. Agregando demanda mensal em perspectiva anual...\n")

demanda_anual_agregada <- metricas_mensais %>%
  group_by(origem, cd_material, categoria_sbc, metodo, familia, convergence) %>%
  summarise(
    # Agregar demanda em 12 meses
    demanda_real_anual = sum(demanda_real_total, na.rm = TRUE),
    demanda_prevista_anual = sum(demanda_prevista_total, na.rm = TRUE),
    
    # Estatísticas descritivas
    n_meses = n(),
    n_zeros_real_total = sum(n_zeros_real),
    n_zeros_pred_total = sum(n_zeros_pred),
    
    .groups = 'drop'
  )

cat(sprintf("✅ Demanda agregada: %s linhas\n", 
            format(nrow(demanda_anual_agregada), big.mark = ",")))

## 5.2. Calcular métricas de erro completas (perspectiva anual) ####

cat("\n📊 5.2. Calculando métricas de erro na perspectiva anual...\n")

# Parâmetro LinLin
p_linlin <- config$parameters$metrics$linlin$p

metricas_anuais <- demanda_anual_agregada %>%
  mutate(
    # Erro absoluto e percentual (mantidos para compatibilidade)
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
    
    # MÉTRICAS COMPLETAS (equivalentes às mensais)

    # MAE anual (igual ao erro absoluto, mas mantemos nomenclatura consistente)
    mae_anual = abs(demanda_real_anual - demanda_prevista_anual),
    
    # RMSE anual (erro ao quadrado)
    rmse_anual = sqrt((demanda_real_anual - demanda_prevista_anual)^2),
    
    # Bias anual (viés sistemático)
    bias_anual = demanda_prevista_anual - demanda_real_anual,
    
    # LinLin anual (função de perda assimétrica)
    linlin_anual = case_when(
      # Subestimação (erro negativo: previsto < real)
      demanda_prevista_anual < demanda_real_anual ~ 
        p_linlin * abs(demanda_real_anual - demanda_prevista_anual),
      
      # Superestimação (erro positivo: previsto > real)
      demanda_prevista_anual >= demanda_real_anual ~ 
        (1 - p_linlin) * abs(demanda_real_anual - demanda_prevista_anual)
    ),
    
    # MAD/Mean anual (erro relativo normalizado)
    mad_mean_anual = if_else(
      demanda_real_anual > 0,
      abs(demanda_real_anual - demanda_prevista_anual) / demanda_real_anual,
      NA_real_
    )
  )

cat("✅ Métricas anuais completas calculadas\n")
cat(sprintf("   - Total de linhas: %s\n", 
            format(nrow(metricas_anuais), big.mark = ",")))
cat("   - Métricas: MAE, RMSE, Bias, LinLin, MAD/Mean (perspectiva anual)\n")

## 5.3. Resumo estatístico das métricas anuais ####


cat("\n📊 5.3. Resumo estatístico das métricas anuais:\n\n")

metricas_anuais %>%
  filter(convergence) %>%
  select(mae_anual, rmse_anual, bias_anual, linlin_anual, mad_mean_anual) %>%
  summary() %>%
  print()

# Distribuição de tipos de erro anual
cat("\n📊 Distribuição de tipos de erro anual:\n")
metricas_anuais %>%
  count(tipo_erro_anual) %>%
  mutate(prop = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  print()

# Comparação de convergência entre perspectivas
cat("\n🔍 Consistência de convergência (mensal vs anual):\n")
cat(sprintf("   - Taxa convergência mensal: %.1f%%\n",
            mean(metricas_mensais$convergence) * 100))
cat(sprintf("   - Taxa convergência anual: %.1f%%\n",
            mean(metricas_anuais$convergence) * 100))
cat("   (Devem ser idênticas - validação de consistência)\n")

toc()

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
  filter(!is.na(categoria_sbc)) %>%
  group_by(categoria_sbc, metodo) %>%
  summarise(
    n_previsoes = n(),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n📊 Desempenho por categoria SBC (amostra):\n")
resumo_por_sbc %>%
  group_by(categoria_sbc) %>%
  slice_min(mae_medio, n = 2) %>%
  select(categoria_sbc, metodo, mae_medio) %>%
  print()
# ===========================================================================
# BLOCO 6.5: ANÁLISE ESTRATIFICADA - FAMÍLIA 3 (ROBUSTEZ CROSS-CATEGORY) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6.5: ANÁLISE DE ROBUSTEZ - FAMÍLIA 3 CROSS-CATEGORY\n")
cat(strrep("=", 70), "\n\n")

log_message("Analisando robustez dos métodos intermitentes em todas as categorias SBC", "INFO")

cat("📊 OBJETIVO: Avaliar performance da Família 3 (Croston, SBA, TSB) quando\n")
cat("   aplicada fora de seu domínio recomendado (Intermittent/Lumpy)\n\n")


## 6.5.1. Performance por Categoria SBC ####


analise_robustez_f3 <- metricas_mensais %>%
  filter(familia == "Familia_3_Intermitentes") %>%
  filter(!is.na(categoria_sbc)) %>%
  group_by(metodo, categoria_sbc) %>%
  summarise(
    n_materiais = n(),
    n_convergido = sum(convergence),
    taxa_convergencia = (n_convergido / n_materiais) * 100,
    
    # Métricas de erro
    mae_mediano = median(mae_mensal, na.rm = TRUE),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    mae_q25 = quantile(mae_mensal, 0.25, na.rm = TRUE),
    mae_q75 = quantile(mae_mensal, 0.75, na.rm = TRUE),
    
    rmse_mediano = median(rmse_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    
    bias_mediano = median(bias_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    
    linlin_mediano = median(linlin_mensal, na.rm = TRUE),
    linlin_medio = mean(linlin_mensal, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(metodo, categoria_sbc)

cat("\n📈 Performance da Família 3 por categoria SBC:\n\n")
print(analise_robustez_f3, n = Inf)


## 6.5.2. Degradação Relativa de Performance ####


cat("\n📉 Calculando degradação relativa fora do domínio nativo...\n\n")

degradacao_f3 <- analise_robustez_f3 %>%
  group_by(metodo) %>%
  mutate(
    # Identificar domínio nativo (Intermittent + Lumpy)
    dominio_nativo = categoria_sbc %in% c("Intermittent", "Lumpy"),
    
    # MAE baseline (média no domínio nativo)
    mae_baseline = mean(mae_mediano[dominio_nativo], na.rm = TRUE),
    
    # Degradação percentual
    degradacao_mae_pct = ((mae_mediano / mae_baseline) - 1) * 100,
    
    # Classificação de performance
    classificacao = case_when(
      dominio_nativo ~ "Domínio Nativo",
      degradacao_mae_pct < 20 ~ "Degradação Aceitável (<20%)",
      degradacao_mae_pct < 50 ~ "Degradação Moderada (20-50%)",
      TRUE ~ "Degradação Severa (>50%)"
    )
  ) %>%
  select(
    metodo, categoria_sbc, 
    dominio_nativo, classificacao,
    n_materiais, taxa_convergencia,
    mae_mediano, mae_baseline, degradacao_mae_pct
  ) %>%
  arrange(metodo, degradacao_mae_pct)

cat("📊 Degradação de performance por categoria:\n\n")
print(degradacao_f3, n = Inf)


## 6.5.3. Comparação com Benchmarks Simples ####


cat("\n🎯 Comparando Família 3 vs. Benchmarks simples por categoria...\n\n")

# Extrair performance dos benchmarks (Naive e Mean)
benchmarks_por_categoria <- metricas_mensais %>%
  filter(metodo %in% c("Naive", "Mean")) %>%
  filter(!is.na(categoria_sbc)) %>%
  group_by(categoria_sbc, metodo) %>%
  summarise(
    mae_mediano = median(mae_mensal, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = metodo,
    values_from = mae_mediano,
    names_prefix = "benchmark_"
  )

cat("📊 Benchmarks por categoria:\n\n")
print(benchmarks_por_categoria)

# Verificar se benchmarks existem
if(nrow(benchmarks_por_categoria) == 0) {
  cat("\n⚠️  ATENÇÃO: Nenhum benchmark encontrado. Pulando comparação.\n")
  
  # Criar objetos vazios para evitar erros
  comparacao_f3_benchmarks <- tibble()
  resumo_competitividade <- tibble()
  
} else {
  
  comparacao_f3_benchmarks <- analise_robustez_f3 %>%
    left_join(benchmarks_por_categoria, by = "categoria_sbc") %>%
    mutate(
      # Verificar se benchmarks estão disponíveis
      benchmark_Naive = ifelse(is.null(benchmark_Naive), NA_real_, benchmark_Naive),
      benchmark_Mean = ifelse(is.null(benchmark_Mean), NA_real_, benchmark_Mean),
      
      # Vantagem sobre Naive
      vantagem_vs_naive_pct = if_else(
        !is.na(benchmark_Naive) & benchmark_Naive > 0,
        ((benchmark_Naive / mae_mediano) - 1) * 100,
        NA_real_
      ),
      supera_naive = if_else(
        !is.na(vantagem_vs_naive_pct),
        vantagem_vs_naive_pct > 0,
        NA
      ),
      
      # Vantagem sobre Mean
      vantagem_vs_mean_pct = if_else(
        !is.na(benchmark_Mean) & benchmark_Mean > 0,
        ((benchmark_Mean / mae_mediano) - 1) * 100,
        NA_real_
      ),
      supera_mean = if_else(
        !is.na(vantagem_vs_mean_pct),
        vantagem_vs_mean_pct > 0,
        NA
      ),
      
      # Classificação de competitividade
      competitividade = case_when(
        is.na(supera_naive) | is.na(supera_mean) ~ "Sem benchmark",
        supera_naive & supera_mean ~ "Superior a ambos",
        supera_naive | supera_mean ~ "Superior a um",
        TRUE ~ "Inferior a ambos"
      )
    ) %>%
    select(
      metodo, categoria_sbc,
      mae_mediano, benchmark_Naive, benchmark_Mean,
      vantagem_vs_naive_pct, vantagem_vs_mean_pct,
      supera_naive, supera_mean,
      competitividade
    )
  
  cat("\n📊 Competitividade da Família 3 vs. Benchmarks:\n\n")
  print(comparacao_f3_benchmarks, n = Inf)
  
  # Resumo executivo
  cat("\n📋 RESUMO EXECUTIVO:\n\n")
  
  resumo_competitividade <- comparacao_f3_benchmarks %>%
    filter(!is.na(supera_naive) | !is.na(supera_mean)) %>%
    group_by(categoria_sbc) %>%
    summarise(
      n_metodos = n(),
      n_supera_naive = sum(supera_naive, na.rm = TRUE),
      n_supera_mean = sum(supera_mean, na.rm = TRUE),
      prop_supera_naive = mean(supera_naive, na.rm = TRUE) * 100,
      prop_supera_mean = mean(supera_mean, na.rm = TRUE) * 100,
      .groups = 'drop'
    )
  
  print(resumo_competitividade)
}


## 6.5.4. Visualização: Heatmap de Performance ####


cat("\n📊 Gerando visualizações...\n")

# Preparar dados para heatmap
dados_heatmap <- analise_robustez_f3 %>%
  mutate(
    categoria_sbc = factor(
      categoria_sbc,
      levels = c("Smooth", "Erratic", "Intermittent", "Lumpy", "Indefinido")
    )
  )

# Heatmap 1: MAE Mediano
p_heatmap_mae <- ggplot(
  dados_heatmap,
  aes(x = metodo, y = categoria_sbc, fill = mae_mediano)
) +
  geom_tile(color = "white", size = 1) +
  geom_text(
    aes(label = sprintf("%.1f", mae_mediano)),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_gradient2(
    low = "#2E8B57",      # Verde (bom)
    mid = "#FF8C00",      # Laranja (médio)
    high = "#DC143C",     # Vermelho (ruim)
    midpoint = median(dados_heatmap$mae_mediano, na.rm = TRUE),
    name = "MAE\nMediano"
  ) +
  labs(
    title = "Robustez da Família 3: Performance Cross-Category",
    subtitle = "MAE mediano por método e categoria SBC\nCategorias Intermittent/Lumpy = Domínio Nativo",
    x = "Método",
    y = "Categoria SBC",
    caption = "Verde = Melhor performance | Vermelho = Pior performance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  here("output/figures/05_robustez_f3_heatmap_mae.png"),
  plot = p_heatmap_mae,
  width = 10, height = 6, dpi = 300
)

# Heatmap 2: Taxa de Convergência
p_heatmap_conv <- ggplot(
  dados_heatmap,
  aes(x = metodo, y = categoria_sbc, fill = taxa_convergencia)
) +
  geom_tile(color = "white", size = 1) +
  geom_text(
    aes(label = sprintf("%.0f%%", taxa_convergencia)),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_gradient2(
    low = "#DC143C",      # Vermelho (baixo)
    mid = "#FF8C00",      # Laranja (médio)
    high = "#2E8B57",     # Verde (alto)
    midpoint = 80,
    name = "Taxa de\nConvergência"
  ) +
  labs(
    title = "Robustez da Família 3: Taxa de Convergência",
    subtitle = "Porcentagem de materiais com convergência bem-sucedida",
    x = "Método",
    y = "Categoria SBC"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  here("output/figures/05_robustez_f3_heatmap_convergencia.png"),
  plot = p_heatmap_conv,
  width = 10, height = 6, dpi = 300
)

# Gráfico de barras: Degradação por categoria
dados_barras <- degradacao_f3 %>%
  filter(!dominio_nativo) %>%  # Apenas categorias fora do domínio
  mutate(
    categoria_sbc = factor(
      categoria_sbc,
      levels = c("Smooth", "Erratic", "Indefinido")
    )
  )

p_degradacao <- ggplot(
  dados_barras,
  aes(x = metodo, y = degradacao_mae_pct, fill = categoria_sbc)
) +
  geom_col(position = "dodge", color = "black", size = 0.3) +
  geom_hline(
    yintercept = c(20, 50),
    linetype = "dashed",
    color = "gray30",
    size = 0.8
  ) +
  geom_text(
    aes(label = sprintf("%.0f%%", degradacao_mae_pct)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Smooth" = "#2E8B57",
      "Erratic" = "#FF6347",
      "Indefinido" = "#808080"
    ),
    name = "Categoria SBC"
  ) +
  labs(
    title = "Degradação de Performance Fora do Domínio Nativo",
    subtitle = "Aumento percentual do MAE em relação ao baseline (Intermittent + Lumpy)",
    x = "Método",
    y = "Degradação do MAE (%)",
    caption = "Linhas tracejadas: 20% (aceitável) e 50% (severa)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(
  here("output/figures/05_robustez_f3_degradacao_barras.png"),
  plot = p_degradacao,
  width = 10, height = 7, dpi = 300
)

cat("✅ Visualizações salvas:\n")
cat("   - 05_robustez_f3_heatmap_mae.png\n")
cat("   - 05_robustez_f3_heatmap_convergencia.png\n")
cat("   - 05_robustez_f3_degradacao_barras.png\n")


## 6.5.5. Testes Estatísticos ####


cat("\n📊 Realizando testes estatísticos...\n\n")

# Teste: MAE médio dentro vs. fora do domínio
dados_teste <- metricas_mensais %>%
  filter(familia == "Familia_3_Intermitentes") %>%
  filter(!is.na(categoria_sbc)) %>%
  mutate(
    dominio_nativo = categoria_sbc %in% c("Intermittent", "Lumpy")
  )

# Teste t pareado por método
testes_por_metodo <- dados_teste %>%
  group_by(metodo) %>%
  summarise(
    mae_dentro = mean(mae_mensal[dominio_nativo], na.rm = TRUE),
    mae_fora = mean(mae_mensal[!dominio_nativo], na.rm = TRUE),
    diferenca = mae_fora - mae_dentro,
    diferenca_pct = (diferenca / mae_dentro) * 100,
    
    n_dentro = sum(dominio_nativo),
    n_fora = sum(!dominio_nativo),
    
    .groups = 'drop'
  )

cat("📊 Diferença de MAE: Dentro vs. Fora do Domínio Nativo\n\n")
print(testes_por_metodo)


## 6.5.6. Salvar Resultados ####


# Adicionar ao objeto consolidado
analise_estratificada_f3 <- list(
  performance_por_categoria = analise_robustez_f3,
  degradacao_relativa = degradacao_f3,
  comparacao_benchmarks = if(nrow(comparacao_f3_benchmarks) > 0) comparacao_f3_benchmarks else NULL,
  resumo_competitividade = if(nrow(resumo_competitividade) > 0) resumo_competitividade else NULL,
  testes_estatisticos = testes_por_metodo
)

# Preparar lista de sheets para Excel (apenas com objetos não-vazios)
sheets_analise <- list(
  "Performance_por_Categoria" = analise_robustez_f3,
  "Degradacao_Relativa" = degradacao_f3,
  "Testes_Estatisticos" = testes_por_metodo
)

# Adicionar sheets opcionais se existirem
if(nrow(comparacao_f3_benchmarks) > 0) {
  sheets_analise[["vs_Benchmarks"]] <- comparacao_f3_benchmarks
}

if(nrow(resumo_competitividade) > 0) {
  sheets_analise[["Resumo_Competitividade"]] <- resumo_competitividade
}

# Salvar em Excel
write_xlsx(
  sheets_analise,
  here("output/reports/05_analise_estratificada_familia3.xlsx")
)

cat("\n✅ Análise estratificada salva:\n")
cat("   - output/reports/05_analise_estratificada_familia3.xlsx\n")
cat(sprintf("   - Sheets incluídas: %d\n", length(sheets_analise)))

cat("\n", strrep("=", 70), "\n\n")

log_message("Análise estratificada da Família 3 concluída", "INFO")

# ===========================================================================
# BLOCO 7: SALVAR RESULTADOS CONSOLIDADOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 7: SALVAMENTO DE RESULTADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Salvando resultados consolidados", "INFO")

tic("Salvamento de resultados")


## 7.1. Salvar objeto consolidado (RDS) ####


consolidado_completo <- list(
  forecasts = forecasts_consolidados,
  metricas_mensais = metricas_mensais,
  metricas_anuais = metricas_anuais,
  resumo_por_metodo = resumo_por_metodo,
  resumo_por_sbc = resumo_por_sbc,
  analise_estratificada_f3 = analise_estratificada_f3,
  
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


## 7.2. Salvar métricas em Excel ####


# Preparar sheets para Excel
sheets_excel <- list(
  "Metricas_Mensais" = metricas_mensais %>%
    select(origem, cd_material, categoria_sbc, metodo, familia,
           mae_mensal, rmse_mensal, bias_mensal, linlin_mensal,
           mad_mean_ratio, per, convergence),
  
  "Metricas_Anuais" = metricas_anuais %>%
    select(origem, cd_material, categoria_sbc, metodo, familia,
           demanda_real_anual, demanda_prevista_anual,
           erro_absoluto_anual, erro_percentual_anual, 
           tipo_erro_anual, convergence),
  
  "Resumo_Por_Metodo" = resumo_por_metodo,
  
  "Resumo_Por_SBC" = resumo_por_sbc %>%
    group_by(categoria_sbc) %>%
    slice_min(mae_medio, n = 5) %>%
    ungroup(),
  
  "F3_Robustez" = analise_robustez_f3,
  "F3_Degradacao" = degradacao_f3
)

write_xlsx(
  sheets_excel,
  here("output/reports/05_consolidated_metrics.xlsx")
)

cat("✅ Métricas em Excel: 05_consolidated_metrics.xlsx\n")


## 7.3. Salvar tabelas em CSV (para análise em Python/outros) ####


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
cat("   - output/reports/05_analise_estratificada_familia3.xlsx") 
cat("   - output/tables/metricas_mensais.csv\n")
cat("   - output/tables/metricas_anuais.csv\n")
cat("   - output/tables/resumo_por_metodo.csv\n")
#### ATEN"CÃO: INCLUIR OS NOVOS OUTPUTS GERADOS COM O 6.5 #########

cat("\n", strrep("=", 70), "\n", sep = "")

log_message("========================================", "INFO")
log_message("CONSOLIDAÇÃO FINALIZADA COM SUCESSO", "INFO")
log_message("========================================", "INFO")

# Desligar paralelização
if(config$computation$parallel) {
  plan(sequential)
}

cat("\n✅ Script 05 finalizado em:", format(Sys.time()), "\n\n")