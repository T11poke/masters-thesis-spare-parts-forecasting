# 06 - ANÁLISE ESTATÍSTICA COMPARATIVA ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Análise estatística comparativa dos métodos de previsão
#            com testes de significância e identificação de domínios de superioridade
# Data: 2025-12-09
# Versão: 1.0.0
#
# OBJETIVOS:
# 1. Benchmarking estatístico global com rankings por métrica
# 2. Testes de significância estatística (Diebold-Mariano, Nemenyi)
# 3. Análise segmentada por categoria SBC
# 4. Análise segmentada por subsistema SISCEAB
# 5. Análise de estabilidade temporal entre origens
# 6. Síntese de resultados e recomendações metodológicas

# ===========================================================================
# BLOCO 0: SETUP ####
# ===========================================================================

library(here)
library(tidyverse)
library(tsutils)      # Teste Nemenyi
library(forecast)     # Teste Diebold-Mariano
library(ggpubr)       # Visualizações estatísticas
library(gt)           # Tabelas formatadas
library(scales)       # Formatação de valores
library(writexl)
library(tictoc)

source(here("R/utils/load_config.R"))

# Configurações gerais
set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO ANÁLISE ESTATÍSTICA COMPARATIVA", "INFO")
log_message("========================================", "INFO")

# Criar diretórios
dir.create(here("output/analysis"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/figures"), showWarnings = FALSE, recursive = TRUE)

cat("\n📁 Diretórios de análise criados\n\n")

# ===========================================================================
# BLOCO 1: CARREGAMENTO E PREPARAÇÃO DOS DADOS ####
# ===========================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: CARREGAMENTO DOS DADOS CONSOLIDADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando dados consolidados do script 05", "INFO")

tic("Carregamento de dados")

# Validar existência do arquivo
arquivo_consolidado <- here("output/forecasts/forecasts_consolidated.rds")

if(!file.exists(arquivo_consolidado)) {
  stop("❌ ERRO: Arquivo consolidado não encontrado. Execute script 05 primeiro.")
}

consolidado <- readRDS(arquivo_consolidado)

# Extrair componentes
metricas_mensais <- consolidado$metricas_mensais
metricas_anuais <- consolidado$metricas_anuais
resumo_por_metodo <- consolidado$resumo_por_metodo
resumo_por_sbc <- consolidado$resumo_por_sbc
metadata <- consolidado$metadata

toc()

cat("\n✅ Dados carregados:\n")
cat(sprintf("   - Métricas mensais: %s linhas\n", 
            format(nrow(metricas_mensais), big.mark = ",")))
cat(sprintf("   - Métricas anuais: %s linhas\n", 
            format(nrow(metricas_anuais), big.mark = ",")))
cat(sprintf("   - Origens: %d\n", metadata$n_origens))
cat(sprintf("   - Materiais: %s\n", 
            format(metadata$n_materiais_total, big.mark = ",")))
cat(sprintf("   - Métodos: %d\n", metadata$n_metodos))

# Preparar dados agregados para análise
cat("\n📊 Preparando dados agregados...\n")

# Agregar por método-origem (média entre materiais)
metricas_metodo_origem_mensal <- metricas_mensais %>%
  filter(convergence) %>%
  group_by(metodo, familia, origem) %>%
  summarise(
    n_materiais = n(),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    linlin_medio = mean(linlin_mensal, na.rm = TRUE),
    mad_mean_medio = mean(mad_mean_ratio, na.rm = TRUE),
    per_medio = mean(per, na.rm = TRUE),
    .groups = 'drop'
  )

metricas_metodo_origem_anual <- metricas_anuais %>%
  filter(convergence) %>%
  group_by(metodo, familia, origem) %>%
  summarise(
    n_materiais = n(),
    erro_abs_medio = mean(erro_absoluto_anual, na.rm = TRUE),
    erro_perc_medio = mean(abs(erro_percentual_anual), na.rm = TRUE),
    prop_super = mean(tipo_erro_anual == "Superestimacao", na.rm = TRUE),
    prop_sub = mean(tipo_erro_anual == "Subestimacao", na.rm = TRUE),
    .groups = 'drop'
  )

# Agregar por método (média entre origens)
metricas_metodo_global_mensal <- metricas_metodo_origem_mensal %>%
  group_by(metodo, familia) %>%
  summarise(
    n_total = sum(n_materiais),
    n_origens = n(),
    mae_medio = mean(mae_medio, na.rm = TRUE),
    mae_sd = if_else(n() > 1, sd(mae_medio, na.rm = TRUE), NA_real_),
    rmse_medio = mean(rmse_medio, na.rm = TRUE),
    rmse_sd = if_else(n() > 1, sd(rmse_medio, na.rm = TRUE), NA_real_),
    bias_medio = mean(bias_medio, na.rm = TRUE),
    bias_sd = if_else(n() > 1, sd(bias_medio, na.rm = TRUE), NA_real_),
    linlin_medio = mean(linlin_medio, na.rm = TRUE),
    linlin_sd = if_else(n() > 1, sd(linlin_medio, na.rm = TRUE), NA_real_),
    mad_mean_medio = mean(mad_mean_medio, na.rm = TRUE),
    per_medio = mean(per_medio, na.rm = TRUE),
    .groups = 'drop'
  )

metricas_metodo_global_anual <- metricas_metodo_origem_anual %>%
  group_by(metodo, familia) %>%
  summarise(
    n_total = sum(n_materiais),
    n_origens = n(),
    erro_abs_medio = mean(erro_abs_medio, na.rm = TRUE),
    erro_abs_sd = if_else(n() > 1, sd(erro_abs_medio, na.rm = TRUE), NA_real_),
    erro_perc_medio = mean(erro_perc_medio, na.rm = TRUE),
    prop_super_media = mean(prop_super, na.rm = TRUE),
    prop_sub_media = mean(prop_sub, na.rm = TRUE),
    .groups = 'drop'
  )

cat("✅ Dados agregados preparados\n")

# Diagnóstico de agregação
cat("\n📊 Diagnóstico de agregação:\n")
n_origens_por_metodo <- metricas_metodo_origem_mensal %>%
  count(metodo) %>%
  pull(n) %>%
  unique()

cat(sprintf("   - Origens por método: %s\n", 
            paste(n_origens_por_metodo, collapse = ", ")))

if(all(n_origens_por_metodo == 1)) {
  cat("   ⚠️  ATENÇÃO: Cada método tem apenas 1 origem.\n")
  cat("      Desvios-padrão (SD) e coeficientes de variação (CV) serão NA.\n")
  cat("      Isso é esperado se houver apenas 1 origem temporal nos dados.\n")
} else {
  cat(sprintf("   ✅ Múltiplas origens detectadas. SDs serão calculados.\n"))
}

# ===========================================================================
# BLOCO 2: ANÁLISE EXPLORATÓRIA INICIAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: ANÁLISE EXPLORATÓRIA INICIAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Estatísticas descritivas gerais", "INFO")

# Distribuição de materiais por categoria SBC
cat("📊 Distribuição de materiais por categoria SBC:\n\n")
dist_sbc <- metricas_mensais %>%
  distinct(cd_material, categoria_sbc) %>%
  count(categoria_sbc, name = "n_materiais") %>%
  mutate(prop = n_materiais / sum(n_materiais) * 100) %>%
  arrange(desc(n_materiais))

print(dist_sbc)

# Taxa de convergência global
cat("\n📊 Taxa de convergência global por método:\n\n")
taxa_convergencia <- metricas_mensais %>%
  group_by(metodo, familia) %>%
  summarise(
    n_total = n(),
    n_convergiu = sum(convergence),
    taxa_convergencia = n_convergiu / n_total * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(taxa_convergencia))

print(taxa_convergencia %>% head(15))

# Estatísticas de erro gerais
cat("\n📊 Estatísticas descritivas das métricas de erro (apenas convergentes):\n\n")

metricas_convergentes <- metricas_mensais %>% filter(convergence)

cat("MAE Mensal:\n")
summary(metricas_convergentes$mae_mensal) %>% print()

cat("\nRMSE Mensal:\n")
summary(metricas_convergentes$rmse_mensal) %>% print()

cat("\nBias Mensal:\n")
summary(metricas_convergentes$bias_mensal) %>% print()

cat("\nLinLin (p=0.85):\n")
summary(metricas_convergentes$linlin_mensal) %>% print()

# ===========================================================================
# BLOCO 3: BENCHMARKING ESTATÍSTICO GLOBAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: BENCHMARKING ESTATÍSTICO GLOBAL\n")
cat(strrep("=", 70), "\n\n")

## 3.1. Rankings por Métrica - Perspectiva Mensal ####

cat("📈 3.1. RANKINGS POR MÉTRICA - PERSPECTIVA MENSAL\n\n")

log_message("Gerando rankings por métrica - perspectiva mensal", "INFO")

# Ranking por MAE
ranking_mae_mensal <- metricas_metodo_global_mensal %>%
  arrange(mae_medio) %>%
  mutate(
    rank_mae = row_number(),
    mae_cv = if_else(!is.na(mae_sd) & mae_medio > 0, 
                     mae_sd / mae_medio, 
                     NA_real_)
  ) %>%
  select(rank_mae, metodo, familia, mae_medio, mae_sd, mae_cv, n_total)

cat("🏆 Top 10 métodos por MAE (mensal):\n\n")
print(ranking_mae_mensal %>% head(10))

# Ranking por RMSE
ranking_rmse_mensal <- metricas_metodo_global_mensal %>%
  arrange(rmse_medio) %>%
  mutate(
    rank_rmse = row_number(),
    rmse_cv = if_else(!is.na(rmse_sd) & rmse_medio > 0, 
                      rmse_sd / rmse_medio, 
                      NA_real_)
  ) %>%
  select(rank_rmse, metodo, familia, rmse_medio, rmse_sd, rmse_cv)

cat("\n🏆 Top 10 métodos por RMSE (mensal):\n\n")
print(ranking_rmse_mensal %>% head(10))

# Ranking por LinLin
ranking_linlin_mensal <- metricas_metodo_global_mensal %>%
  arrange(linlin_medio) %>%
  mutate(
    rank_linlin = row_number(),
    linlin_cv = if_else(!is.na(linlin_sd) & linlin_medio > 0, 
                        linlin_sd / linlin_medio, 
                        NA_real_)
  ) %>%
  select(rank_linlin, metodo, familia, linlin_medio, linlin_sd, linlin_cv)

cat("\n🏆 Top 10 métodos por LinLin (mensal):\n\n")
print(ranking_linlin_mensal %>% head(10))

# Ranking por Bias (menor valor absoluto)
ranking_bias_mensal <- metricas_metodo_global_mensal %>%
  arrange(abs(bias_medio)) %>%
  mutate(
    rank_bias = row_number(),
    bias_abs = abs(bias_medio)
  ) %>%
  select(rank_bias, metodo, familia, bias_medio, bias_abs, bias_sd)

cat("\n🏆 Top 10 métodos por Bias (menor valor absoluto):\n\n")
print(ranking_bias_mensal %>% head(10))

## 3.2. Rankings por Métrica - Perspectiva Anual ####

cat("\n📈 3.2. RANKINGS POR MÉTRICA - PERSPECTIVA ANUAL\n\n")

log_message("Gerando rankings por métrica - perspectiva anual", "INFO")

# Ranking por Erro Absoluto Anual
ranking_anual <- metricas_metodo_global_anual %>%
  arrange(erro_abs_medio) %>%
  mutate(
    rank_erro_anual = row_number(),
    erro_abs_cv = if_else(!is.na(erro_abs_sd) & erro_abs_medio > 0, 
                          erro_abs_sd / erro_abs_medio, 
                          NA_real_)
  ) %>%
  select(rank_erro_anual, metodo, familia, erro_abs_medio, erro_abs_sd, 
         erro_abs_cv, prop_super_media, prop_sub_media)

cat("🏆 Top 10 métodos por Erro Absoluto Anual:\n\n")
print(ranking_anual %>% head(10))

## 3.3. Comparação de Rankings: Mensal vs. Anual ####

cat("\n📊 3.3. COMPARAÇÃO DE RANKINGS: MENSAL VS. ANUAL\n\n")

comparacao_rankings <- ranking_mae_mensal %>%
  select(metodo, rank_mae) %>%
  left_join(
    ranking_anual %>% select(metodo, rank_erro_anual),
    by = "metodo"
  ) %>%
  mutate(
    diferenca_rank = rank_erro_anual - rank_mae,
    tipo_mudanca = case_when(
      diferenca_rank < -2 ~ "Melhora substancial (anual)",
      diferenca_rank > 2 ~ "Piora substancial (anual)",
      TRUE ~ "Estável"
    )
  ) %>%
  arrange(desc(abs(diferenca_rank)))

cat("🔄 Métodos com maior mudança de ranking (mensal → anual):\n\n")
print(comparacao_rankings %>% head(10))

# Correlação de Spearman entre rankings
cor_spearman <- cor(
  ranking_mae_mensal$rank_mae,
  ranking_anual$rank_erro_anual[match(ranking_mae_mensal$metodo, ranking_anual$metodo)],
  method = "spearman",
  use = "complete.obs"
)

cat(sprintf("\n📊 Correlação de Spearman entre rankings mensal e anual: %.3f\n", 
            cor_spearman))

if(cor_spearman > 0.8) {
  cat("   → Rankings altamente consistentes entre perspectivas\n")
} else if(cor_spearman > 0.6) {
  cat("   → Rankings moderadamente consistentes\n")
} else {
  cat("   ⚠️  Rankings apresentam diferenças substanciais entre perspectivas\n")
}

## 3.4. Testes de Significância Estatística ####

cat("\n", strrep("-", 70), "\n", sep = "")
cat("3.4. TESTES DE SIGNIFICÂNCIA ESTATÍSTICA\n")
cat(strrep("-", 70), "\n\n")

log_message("Executando testes de significância estatística", "INFO")

# Função para calcular teste Diebold-Mariano
calculate_dm_test <- function(erros1, erros2, alternative = "two.sided") {
  tryCatch({
    # Remover NAs
    valid_idx <- !is.na(erros1) & !is.na(erros2)
    e1 <- erros1[valid_idx]
    e2 <- erros2[valid_idx]
    
    if(length(e1) < 10) return(list(statistic = NA, p.value = NA))
    
    # Diebold-Mariano test
    dm_result <- dm.test(e1, e2, alternative = alternative, h = 1)
    
    list(
      statistic = as.numeric(dm_result$statistic),
      p.value = as.numeric(dm_result$p.value)
    )
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
}

cat("🔬 Teste Diebold-Mariano: Top 10 vs. Poisson\n\n")

# Identificar se Poisson está presente
metodos_disponiveis <- unique(metricas_mensais$metodo)
poisson_presente <- any(str_detect(tolower(metodos_disponiveis), "poisson"))

if(poisson_presente) {
  metodo_poisson <- metodos_disponiveis[str_detect(tolower(metodos_disponiveis), "poisson")][1]
  
  # Top 10 métodos por MAE
  top10_metodos <- ranking_mae_mensal %>% 
    head(10) %>% 
    pull(metodo)
  
  # Preparar dados para teste DM
  dm_results <- map_dfr(top10_metodos, function(metodo_teste) {
    
    # Erros do método em teste
    erros_teste <- metricas_mensais %>%
      filter(metodo == metodo_teste, convergence) %>%
      pull(mae_mensal)
    
    # Erros do Poisson
    erros_poisson <- metricas_mensais %>%
      filter(metodo == metodo_poisson, convergence) %>%
      pull(mae_mensal)
    
    # Alinhar por cd_material e origem para comparação pareada
    dados_alinhados <- metricas_mensais %>%
      filter(convergence) %>%
      select(cd_material, origem, metodo, mae_mensal) %>%
      pivot_wider(names_from = metodo, values_from = mae_mensal) %>%
      filter(!is.na(!!sym(metodo_teste)), !is.na(!!sym(metodo_poisson)))
    
    if(nrow(dados_alinhados) < 10) {
      return(tibble(
        metodo = metodo_teste,
        dm_statistic = NA,
        dm_pvalue = NA,
        interpretacao = "Dados insuficientes"
      ))
    }
    
    erros_teste_alinhados <- dados_alinhados[[metodo_teste]]
    erros_poisson_alinhados <- dados_alinhados[[metodo_poisson]]
    
    dm_test <- calculate_dm_test(
      erros_teste_alinhados,
      erros_poisson_alinhados,
      alternative = "two.sided"
    )
    
    # Interpretação
    interpretacao <- case_when(
      is.na(dm_test$p.value) ~ "Teste falhou",
      dm_test$p.value < 0.01 ~ "Diferença altamente significativa (p<0.01)",
      dm_test$p.value < 0.05 ~ "Diferença significativa (p<0.05)",
      dm_test$p.value < 0.10 ~ "Diferença marginalmente significativa (p<0.10)",
      TRUE ~ "Sem diferença significativa (p≥0.10)"
    )
    
    tibble(
      metodo = metodo_teste,
      dm_statistic = dm_test$statistic,
      dm_pvalue = dm_test$p.value,
      interpretacao = interpretacao
    )
  })
  
  print(dm_results)
  
} else {
  cat("⚠️  Método Poisson não encontrado nos dados. Pulando teste DM.\n")
  dm_results <- NULL
}

# Teste Nemenyi (post-hoc para ranking global)
cat("\n🔬 Teste Nemenyi: Identificação de grupos estatisticamente equivalentes\n\n")

# Preparar matriz de erros: métodos em colunas, observações em linhas
# Usar apenas materiais e origens com convergência em TODOS os métodos

# Identificar materiais/origens completos
materiais_completos <- metricas_mensais %>%
  filter(convergence) %>%
  group_by(cd_material, origem) %>%
  summarise(n_metodos = n_distinct(metodo), .groups = 'drop') %>%
  filter(n_metodos == metadata$n_metodos) %>%
  select(cd_material, origem)

if(nrow(materiais_completos) < 30) {
  cat(sprintf("⚠️  Apenas %d combinações material-origem têm todos os métodos convergentes.\n",
              nrow(materiais_completos)))
  cat("   Teste Nemenyi requer design balanceado. Usando amostra disponível.\n\n")
}

# Construir matriz para teste
matriz_erros_nemenyi <- metricas_mensais %>%
  semi_join(materiais_completos, by = c("cd_material", "origem")) %>%
  filter(convergence) %>%
  select(cd_material, origem, metodo, mae_mensal) %>%
  unite("obs_id", cd_material, origem, sep = "_") %>%
  pivot_wider(names_from = metodo, values_from = mae_mensal) %>%
  select(-obs_id)

if(ncol(matriz_erros_nemenyi) >= 3 && nrow(matriz_erros_nemenyi) >= 30) {
  
  # Executar teste Nemenyi
  tryCatch({
    nemenyi_result <- nemenyi(matriz_erros_nemenyi, plottype = "none")
    
    cat("✅ Teste Nemenyi executado com sucesso\n")
    cat(sprintf("   - %d métodos comparados\n", ncol(matriz_erros_nemenyi)))
    cat(sprintf("   - %d observações\n", nrow(matriz_erros_nemenyi)))
    cat("\n📊 Grupos estatisticamente equivalentes (α=0.05):\n\n")
    
    # Extrair grupos do resultado
    grupos_nemenyi <- nemenyi_result$means %>%
      as_tibble(rownames = "metodo") %>%
      arrange(value) %>%
      mutate(rank = row_number())
    
    print(grupos_nemenyi)
    
    # Salvar resultado
    saveRDS(nemenyi_result, here("output/analysis/nemenyi_test_result.rds"))
    
  }, error = function(e) {
    cat("❌ Erro ao executar teste Nemenyi:", e$message, "\n")
    nemenyi_result <- NULL
  })
  
} else {
  cat("⚠️  Dados insuficientes para teste Nemenyi (requer ≥3 métodos e ≥30 obs)\n")
  nemenyi_result <- NULL
}

# ===========================================================================
# BLOCO 4: ANÁLISE POR CATEGORIA SBC ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: ANÁLISE SEGMENTADA POR CATEGORIA SBC\n")
cat(strrep("=", 70), "\n\n")

log_message("Análise de desempenho por categoria SBC", "INFO")

## 4.1. Desempenho por Categoria ####

cat("📊 4.1. DESEMPENHO POR CATEGORIA SBC\n\n")

# Agregar por categoria-método
desempenho_por_sbc <- metricas_mensais %>%
  filter(convergence) %>%
  group_by(categoria_sbc, metodo, familia) %>%
  summarise(
    n_materiais = n_distinct(cd_material),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    mae_sd = sd(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    linlin_medio = mean(linlin_mensal, na.rm = TRUE),
    .groups = 'drop'
  )

# Top 5 por categoria
cat("🏆 Top 5 métodos por categoria SBC (ordenado por MAE):\n\n")

categorias <- unique(desempenho_por_sbc$categoria_sbc)

top5_por_categoria <- map_dfr(categorias, function(cat) {
  desempenho_por_sbc %>%
    filter(categoria_sbc == cat) %>%
    arrange(mae_medio) %>%
    head(5) %>%
    mutate(rank = row_number()) %>%
    select(categoria_sbc, rank, metodo, familia, mae_medio, n_materiais)
})

print(top5_por_categoria)

## 4.2. Análise de Inversões de Ranking ####

cat("\n📊 4.2. ANÁLISE DE INVERSÕES DE RANKING ENTRE CATEGORIAS\n\n")

# Calcular ranking de cada método em cada categoria
rankings_por_categoria <- desempenho_por_sbc %>%
  group_by(categoria_sbc) %>%
  arrange(mae_medio) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(categoria_sbc, metodo, rank, mae_medio)

# Detectar inversões significativas
inversoes <- rankings_por_categoria %>%
  select(categoria_sbc, metodo, rank) %>%
  pivot_wider(names_from = categoria_sbc, values_from = rank) %>%
  mutate(
    variacao_ranking = pmax(
      abs(Smooth - Intermittent),
      abs(Smooth - Lumpy),
      abs(Smooth - Erratic),
      abs(Intermittent - Lumpy),
      abs(Intermittent - Erratic),
      abs(Lumpy - Erratic),
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(variacao_ranking))

cat("🔄 Métodos com maior variação de ranking entre categorias:\n\n")
print(inversoes %>% head(10))

# Análise específica: métodos intermitentes em categorias apropriadas
cat("\n🔬 ANÁLISE ESPECÍFICA: Desempenho de métodos intermitentes\n\n")

metodos_intermitentes <- c("Croston", "SBA", "TSB")

desempenho_intermitentes <- desempenho_por_sbc %>%
  filter(metodo %in% metodos_intermitentes) %>%
  select(categoria_sbc, metodo, mae_medio) %>%
  group_by(categoria_sbc) %>%
  mutate(rank = rank(mae_medio)) %>%
  ungroup() %>%
  arrange(categoria_sbc, rank)

cat("📊 Ranking de Croston, SBA, TSB em cada categoria:\n\n")
print(desempenho_intermitentes)

# Comparar com benchmarks
benchmarks <- c("Naive", "Mean", "MA")

comparacao_intermitentes_benchmarks <- desempenho_por_sbc %>%
  filter(
    categoria_sbc %in% c("Intermittent", "Lumpy"),
    metodo %in% c(metodos_intermitentes, benchmarks)
  ) %>%
  group_by(categoria_sbc, metodo) %>%
  summarise(mae_medio = mean(mae_medio), .groups = 'drop') %>%
  mutate(
    tipo_metodo = if_else(metodo %in% metodos_intermitentes, 
                          "Especializado", "Benchmark")
  ) %>%
  arrange(categoria_sbc, mae_medio)

cat("\n🆚 Comparação: Métodos Especializados vs. Benchmarks (Intermittent & Lumpy):\n\n")
print(comparacao_intermitentes_benchmarks)

# ===========================================================================
# BLOCO 5: ANÁLISE POR SUBSISTEMA SISCEAB ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 5: ANÁLISE SEGMENTADA POR SUBSISTEMA SISCEAB\n")
cat(strrep("=", 70), "\n\n")

log_message("Análise de desempenho por subsistema funcional", "INFO")

# Verificar se há informação de subsistema
if("subsistema" %in% names(metricas_mensais)) {
  
  # Agregar por subsistema-método
  desempenho_por_subsistema <- metricas_mensais %>%
    filter(convergence, !is.na(subsistema)) %>%
    group_by(subsistema, metodo, familia) %>%
    summarise(
      n_materiais = n_distinct(cd_material),
      mae_medio = mean(mae_mensal, na.rm = TRUE),
      mae_sd = sd(mae_mensal, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("📊 Top 3 métodos por subsistema:\n\n")
  
  subsistemas <- unique(desempenho_por_subsistema$subsistema)
  
  top3_por_subsistema <- map_dfr(subsistemas, function(sub) {
    desempenho_por_subsistema %>%
      filter(subsistema == sub) %>%
      arrange(mae_medio) %>%
      head(3) %>%
      mutate(rank = row_number()) %>%
      select(subsistema, rank, metodo, familia, mae_medio, n_materiais)
  })
  
  print(top3_por_subsistema)
  
  # Análise de heterogeneidade
  cat("\n📊 Heterogeneidade de desempenho entre subsistemas:\n\n")
  
  heterogeneidade_subsistema <- desempenho_por_subsistema %>%
    group_by(metodo) %>%
    summarise(
      n_subsistemas = n(),
      mae_min = min(mae_medio),
      mae_max = max(mae_medio),
      mae_range = mae_max - mae_min,
      mae_cv = sd(mae_medio) / mean(mae_medio),
      .groups = 'drop'
    ) %>%
    arrange(desc(mae_cv))
  
  cat("🔄 Métodos com maior variabilidade entre subsistemas (CV do MAE):\n\n")
  print(heterogeneidade_subsistema %>% head(10))
  
} else {
  cat("⚠️  Informação de subsistema não disponível nos dados.\n")
  cat("   Para habilitar esta análise, adicione coluna 'subsistema' aos dados.\n")
  desempenho_por_subsistema <- NULL
  top3_por_subsistema <- NULL
}

# ===========================================================================
# BLOCO 6: ANÁLISE DE ESTABILIDADE TEMPORAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6: ANÁLISE DE ESTABILIDADE TEMPORAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Análise de estabilidade entre origens temporais", "INFO")

## 6.1. Variabilidade entre Origens ####

cat("📊 6.1. VARIABILIDADE DE DESEMPENHO ENTRE ORIGENS\n\n")

# Calcular coeficiente de variação do MAE entre origens
estabilidade_temporal <- metricas_metodo_origem_mensal %>%
  group_by(metodo, familia) %>%
  summarise(
    n_origens = n(),
    mae_medio_geral = mean(mae_medio, na.rm = TRUE),
    mae_sd_entre_origens = if_else(n() > 1, sd(mae_medio, na.rm = TRUE), NA_real_),
    mae_cv = if_else(!is.na(mae_sd_entre_origens) & mae_medio_geral > 0,
                     mae_sd_entre_origens / mae_medio_geral, 
                     NA_real_),
    mae_min_origem = min(mae_medio, na.rm = TRUE),
    mae_max_origem = max(mae_medio, na.rm = TRUE),
    mae_amplitude = mae_max_origem - mae_min_origem,
    .groups = 'drop'
  ) %>%
  arrange(mae_cv)

cat("✅ Métodos mais ESTÁVEIS entre origens (menor CV):\n\n")

# Verificar se há múltiplas origens
if(all(is.na(estabilidade_temporal$mae_cv))) {
  cat("⚠️  ATENÇÃO: Análise de estabilidade requer múltiplas origens temporais.\n")
  cat("   Com apenas 1 origem, não é possível calcular variabilidade.\n")
  cat("   Rankings por amplitude (max - min) ainda disponíveis:\n\n")
  print(estabilidade_temporal %>% 
          arrange(mae_amplitude) %>% 
          select(metodo, familia, mae_medio_geral, mae_amplitude, n_origens) %>%
          head(10))
} else {
  print(estabilidade_temporal %>% head(10))
}

cat("\n⚠️  Métodos mais INSTÁVEIS entre origens (maior CV):\n\n")

if(!all(is.na(estabilidade_temporal$mae_cv))) {
  print(estabilidade_temporal %>% arrange(desc(mae_cv)) %>% head(10))
} else {
  cat("   (Análise indisponível com apenas 1 origem temporal)\n")
}

## 6.2. Identificação de Outliers Temporais ####

cat("\n📊 6.2. IDENTIFICAÇÃO DE OUTLIERS TEMPORAIS\n\n")

# Para cada método, identificar origens com desempenho atipicamente bom/ruim
outliers_temporais <- metricas_metodo_origem_mensal %>%
  group_by(metodo) %>%
  mutate(
    mae_z_score = (mae_medio - mean(mae_medio)) / sd(mae_medio),
    tipo_outlier = case_when(
      mae_z_score < -1.5 ~ "Excepcionalmente bom",
      mae_z_score > 1.5 ~ "Excepcionalmente ruim",
      TRUE ~ "Normal"
    )
  ) %>%
  filter(abs(mae_z_score) > 1.5) %>%
  ungroup() %>%
  arrange(mae_z_score) %>%
  select(metodo, familia, origem, mae_medio, mae_z_score, tipo_outlier)

if(nrow(outliers_temporais) > 0) {
  cat("🔍 Outliers temporais detectados:\n\n")
  print(outliers_temporais %>% head(20))
} else {
  cat("✅ Nenhum outlier temporal significativo detectado.\n")
}

## 6.3. Análise de Tendências Temporais ####

cat("\n📊 6.3. ANÁLISE DE TENDÊNCIAS TEMPORAIS\n\n")

# Verificar se há tendência de melhora/piora ao longo das origens
tendencias <- metricas_metodo_origem_mensal %>%
  mutate(origem_num = as.numeric(str_extract(origem, "\\d+"))) %>%
  group_by(metodo) %>%
  summarise(
    correlacao_tempo_mae = cor(origem_num, mae_medio, 
                               method = "spearman"),
    tendencia = case_when(
      correlacao_tempo_mae < -0.5 ~ "Melhora progressiva",
      correlacao_tempo_mae > 0.5 ~ "Piora progressiva",
      TRUE ~ "Estável"
    ),
    .groups = 'drop'
  ) %>%
  arrange(correlacao_tempo_mae)

cat("📈 Tendências temporais identificadas:\n\n")
print(tendencias)

cat("\n🔄 Métodos com maior MELHORA progressiva:\n")
print(tendencias %>% filter(tendencia == "Melhora progressiva") %>% head(5))

cat("\n⚠️  Métodos com PIORA progressiva:\n")
print(tendencias %>% filter(tendencia == "Piora progressiva") %>% head(5))

# ===========================================================================
# BLOCO 7: SÍNTESE DE RESULTADOS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 7: SÍNTESE DE RESULTADOS E RECOMENDAÇÕES\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando síntese de resultados", "INFO")

## 7.1. Ranking Consolidado Multi-Critério ####

cat("🏆 7.1. RANKING CONSOLIDADO MULTI-CRITÉRIO\n\n")

# Combinar rankings de múltiplas métricas
ranking_consolidado <- ranking_mae_mensal %>%
  select(metodo, familia, rank_mae, mae_medio, mae_sd) %>%
  left_join(
    ranking_rmse_mensal %>% select(metodo, rank_rmse, rmse_medio),
    by = "metodo"
  ) %>%
  left_join(
    ranking_linlin_mensal %>% select(metodo, rank_linlin, linlin_medio),
    by = "metodo"
  ) %>%
  left_join(
    ranking_anual %>% select(metodo, rank_erro_anual, erro_abs_medio),
    by = "metodo"
  ) %>%
  left_join(
    taxa_convergencia %>% select(metodo, taxa_convergencia),
    by = "metodo"
  ) %>%
  left_join(
    estabilidade_temporal %>% select(metodo, mae_cv),
    by = "metodo"
  ) %>%
  mutate(
    # Ranking médio (Borda count)
    rank_medio = (rank_mae + rank_rmse + rank_linlin + rank_erro_anual) / 4,
    
    # Classificação de robustez
    robustez = case_when(
      taxa_convergencia >= 95 ~ "Excelente",
      taxa_convergencia >= 90 ~ "Boa",
      taxa_convergencia >= 80 ~ "Moderada",
      TRUE ~ "Baixa"
    ),
    
    # Classificação de estabilidade (considerar NA)
    estabilidade = case_when(
      is.na(mae_cv) ~ "Não aplicável (1 origem)",
      mae_cv < 0.10 ~ "Muito estável",
      mae_cv < 0.20 ~ "Estável",
      mae_cv < 0.30 ~ "Moderadamente variável",
      TRUE ~ "Instável"
    )
  ) %>%
  arrange(rank_medio)

cat("📊 Ranking Multi-Critério (Top 15):\n\n")
print(ranking_consolidado %>% head(15))

## 7.2. Identificação de Métodos Recomendados ####

cat("\n💡 7.2. MÉTODOS RECOMENDADOS POR CONTEXTO\n\n")

# Método campeão global
metodo_campeao <- ranking_consolidado %>% slice(1) %>% pull(metodo)
familia_campeao <- ranking_consolidado %>% slice(1) %>% pull(familia)

cat(sprintf("🥇 MÉTODO CAMPEÃO GLOBAL: %s (%s)\n", metodo_campeao, familia_campeao))
cat(sprintf("   - Rank médio: %.1f\n", 
            ranking_consolidado %>% slice(1) %>% pull(rank_medio)))
cat(sprintf("   - MAE médio: %.2f\n", 
            ranking_consolidado %>% slice(1) %>% pull(mae_medio)))
cat(sprintf("   - Robustez: %s (%.1f%% convergência)\n",
            ranking_consolidado %>% slice(1) %>% pull(robustez),
            ranking_consolidado %>% slice(1) %>% pull(taxa_convergencia)))

mae_cv_campeao <- ranking_consolidado %>% slice(1) %>% pull(mae_cv)
estabilidade_campeao <- ranking_consolidado %>% slice(1) %>% pull(estabilidade)

if(is.na(mae_cv_campeao)) {
  cat(sprintf("   - Estabilidade: %s\n", estabilidade_campeao))
} else {
  cat(sprintf("   - Estabilidade: %s (CV=%.3f)\n", 
              estabilidade_campeao, mae_cv_campeao))
}

# Métodos recomendados por categoria SBC
cat("\n📋 MÉTODOS RECOMENDADOS POR CATEGORIA SBC:\n\n")

recomendacoes_sbc <- top5_por_categoria %>%
  filter(rank == 1) %>%
  select(categoria_sbc, metodo_recomendado = metodo, familia, mae_medio)

print(recomendacoes_sbc)

# Estratégia de portfólio híbrido
cat("\n🎯 ESTRATÉGIA DE PORTFÓLIO HÍBRIDO SUGERIDA:\n\n")
cat("Com base na análise por categoria SBC:\n")
for(i in 1:nrow(recomendacoes_sbc)) {
  cat(sprintf("   - %s: %s (MAE=%.2f)\n",
              recomendacoes_sbc$categoria_sbc[i],
              recomendacoes_sbc$metodo_recomendado[i],
              recomendacoes_sbc$mae_medio[i]))
}

## 7.3. Comparação com Método Atual (Poisson) ####

if(poisson_presente) {
  cat("\n📊 7.3. COMPARAÇÃO COM MÉTODO ATUAL DO DECEA (POISSON)\n\n")
  
  # Extrair desempenho do Poisson
  desempenho_poisson <- ranking_consolidado %>%
    filter(metodo == metodo_poisson) %>%
    select(rank_mae, mae_medio, taxa_convergencia)
  
  # Top 3 alternativos
  top3_alternativos <- ranking_consolidado %>%
    head(3) %>%
    mutate(
      ganho_vs_poisson_pct = (desempenho_poisson$mae_medio - mae_medio) / 
        desempenho_poisson$mae_medio * 100
    ) %>%
    select(rank_mae, metodo, familia, mae_medio, ganho_vs_poisson_pct)
  
  cat(sprintf("📍 Posição atual do Poisson: Rank %d (de %d métodos)\n",
              desempenho_poisson$rank_mae,
              nrow(ranking_consolidado)))
  cat(sprintf("   MAE médio: %.2f\n\n", desempenho_poisson$mae_medio))
  
  cat("🚀 Ganho potencial com métodos alternativos:\n\n")
  print(top3_alternativos)
  
} else {
  cat("\n⚠️  Método Poisson não disponível para comparação.\n")
}

# ===========================================================================
# BLOCO 8: EXPORTAÇÃO DE TABELAS PARA DISSERTAÇÃO ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 8: EXPORTAÇÃO DE TABELAS\n")
cat(strrep("=", 70), "\n\n")

log_message("Exportando tabelas para dissertação", "INFO")

# Lista de tabelas para Excel
tabelas_dissertacao <- list(
  "1_Ranking_Global_Mensal" = ranking_mae_mensal %>% head(20),
  
  "2_Ranking_Global_Anual" = ranking_anual %>% head(20),
  
  "3_Ranking_Consolidado" = ranking_consolidado %>% head(20),
  
  "4_Top5_por_Categoria_SBC" = top5_por_categoria,
  
  "5_Inversoes_Ranking" = inversoes %>% head(20),
  
  "6_Estabilidade_Temporal" = estabilidade_temporal %>% head(20),
  
  "7_Taxa_Convergencia" = taxa_convergencia,
  
  "8_Comparacao_Mensal_Anual" = comparacao_rankings %>% head(20)
)

# Adicionar tabelas condicionais
if(poisson_presente && !is.null(dm_results)) {
  tabelas_dissertacao[["9_Teste_DM_vs_Poisson"]] <- dm_results
}

if(!is.null(desempenho_por_subsistema)) {
  tabelas_dissertacao[["10_Desempenho_Subsistema"]] <- top3_por_subsistema
}

# Salvar Excel
write_xlsx(
  tabelas_dissertacao,
  here("output/tables/06_analise_comparativa.xlsx")
)

cat("✅ Tabelas exportadas: 06_analise_comparativa.xlsx\n")

# Salvar objetos R para análises posteriores
resultados_analise <- list(
  ranking_consolidado = ranking_consolidado,
  desempenho_por_sbc = desempenho_por_sbc,
  estabilidade_temporal = estabilidade_temporal,
  dm_results = if(exists("dm_results")) dm_results else NULL,
  nemenyi_result = if(exists("nemenyi_result")) nemenyi_result else NULL,
  recomendacoes_sbc = recomendacoes_sbc,
  timestamp = Sys.time()
)

saveRDS(
  resultados_analise,
  here("output/analysis/resultados_analise_completa.rds")
)

cat("✅ Resultados salvos: resultados_analise_completa.rds\n")

# ===========================================================================
# RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL - ANÁLISE ESTATÍSTICA COMPARATIVA\n")
cat(strrep("=", 70), "\n\n")

cat("📋 RESUMO DA ANÁLISE:\n\n")

cat(sprintf("✅ Métodos analisados: %d\n", nrow(ranking_consolidado)))
cat(sprintf("📊 Materiais avaliados: %s\n", 
            format(metadata$n_materiais_total, big.mark = ",")))
cat(sprintf("🔬 Origens temporais: %d\n", metadata$n_origens))
cat(sprintf("📈 Total de previsões: %s\n",
            format(nrow(metricas_mensais), big.mark = ",")))

cat("\n🏆 PRINCIPAIS ACHADOS:\n\n")

cat(sprintf("1. Método com melhor desempenho global: %s\n", metodo_campeao))
cat(sprintf("   - MAE médio: %.2f (±%.2f)\n",
            ranking_consolidado$mae_medio[1],
            ranking_consolidado$mae_sd[1]))

cat("\n2. Consistência entre perspectivas mensal e anual:\n")
cat(sprintf("   - Correlação de Spearman: %.3f\n", cor_spearman))

cat("\n3. Métodos especializados em demanda intermitente:\n")
if(any(desempenho_intermitentes$categoria_sbc == "Intermittent" & 
       desempenho_intermitentes$rank <= 3)) {
  cat("   ✅ Apresentam vantagem nas categorias apropriadas\n")
} else {
  cat("   ⚠️  Não demonstram vantagem clara nas categorias apropriadas\n")
}

cat("\n4. Estabilidade temporal:\n")
if(!all(is.na(estabilidade_temporal$mae_cv))) {
  metodos_estaveis <- sum(estabilidade_temporal$mae_cv < 0.20, na.rm = TRUE)
  cat(sprintf("   - %d métodos apresentam desempenho estável (CV<0.20)\n",
              metodos_estaveis))
} else {
  cat("   - Análise de variabilidade indisponível (apenas 1 origem temporal)\n")
  cat("   - Considere executar com múltiplas origens para análise de estabilidade\n")
}

if(poisson_presente) {
  cat("\n5. Comparação com método atual (Poisson):\n")
  melhor_ganho <- max(top3_alternativos$ganho_vs_poisson_pct, na.rm = TRUE)
  cat(sprintf("   - Melhor alternativa oferece ganho de %.1f%% em MAE\n",
              melhor_ganho))
}

cat("\n📁 Arquivos gerados:\n")
cat("   - output/tables/06_analise_comparativa.xlsx\n")
cat("   - output/analysis/resultados_analise_completa.rds\n")
if(!is.null(nemenyi_result)) {
  cat("   - output/analysis/nemenyi_test_result.rds\n")
}

cat("\n🎯 PRÓXIMAS ETAPAS:\n")
cat("   1. Executar script 07_advanced_error_visualization.R\n")
cat("   2. Gerar visualizações para dissertação\n")
cat("   3. Documentar achados no Capítulo 4 (Resultados)\n")

cat("\n", strrep("=", 70), "\n", sep = "")

log_message("========================================", "INFO")
log_message("ANÁLISE ESTATÍSTICA FINALIZADA COM SUCESSO", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script 06 finalizado em:", format(Sys.time()), "\n\n")