# VALIDAÇÃO DE RESULTADOS - SCRIPT 05 ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE  
# Descrição: Verifica consistência e confiabilidade dos resultados consolidados
# Data: 2025-12-08
# Versão: 1.0.0

library(here)
library(tidyverse)

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║     VALIDAÇÃO DE CONSISTÊNCIA - RESULTADOS SCRIPT 05       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ===========================================================================
# 1. CARREGAR DADOS ####
# ===========================================================================

cat("📦 1. CARREGANDO DADOS...\n\n")

consolidado <- readRDS(here("output/forecasts/forecasts_consolidated.rds"))
metricas_mensais <- consolidado$metricas_mensais
metricas_anuais <- consolidado$metricas_anuais

cat(sprintf("✅ Dados carregados:\n"))
cat(sprintf("   - Métricas mensais: %s linhas\n", 
            format(nrow(metricas_mensais), big.mark = ",")))
cat(sprintf("   - Métricas anuais: %s linhas\n", 
            format(nrow(metricas_anuais), big.mark = ",")))

# ===========================================================================
# 2. VALIDAÇÃO DE ESTRUTURA ####
# ===========================================================================

cat("\n📊 2. VALIDAÇÃO DE ESTRUTURA DOS DADOS...\n\n")

testes <- list()

# Teste 2.1: Todas as origens presentes
origens_esperadas <- paste0("origem_", 1:6)
origens_presentes <- unique(metricas_mensais$origem)

testes$origens_completas <- all(origens_esperadas %in% origens_presentes)

cat(sprintf("   2.1. Origens completas: %s\n", 
            ifelse(testes$origens_completas, "✅ PASSOU", "❌ FALHOU")))
if(!testes$origens_completas) {
  cat(sprintf("        Faltando: %s\n", 
              paste(setdiff(origens_esperadas, origens_presentes), collapse = ", ")))
}

# Teste 2.2: Todos os métodos presentes
metodos_esperados <- c(
  # Família 1
  "Naive", "Mean", "MA",
  # Família 2
  "Arima", "Ets", "Hw_add", "Hw_mult", "TSLM",
  # Família 3
  "Croston", "SBA", "TSB"
  # Família 4 e 5 variam
)

metodos_presentes <- unique(metricas_mensais$metodo)

testes$metodos_minimos <- all(metodos_esperados %in% metodos_presentes)

cat(sprintf("   2.2. Métodos mínimos presentes: %s\n", 
            ifelse(testes$metodos_minimos, "✅ PASSOU", "⚠️  VERIFICAR")))
cat(sprintf("        Métodos encontrados: %d\n", length(metodos_presentes)))
cat(sprintf("        Esperados mínimo: %d\n", length(metodos_esperados)))

if(!testes$metodos_minimos) {
  cat("        Faltando:\n")
  for(m in setdiff(metodos_esperados, metodos_presentes)) {
    cat(sprintf("          - %s\n", m))
  }
}

# Teste 2.3: Famílias classificadas
familias_presentes <- unique(metricas_mensais$familia)
testes$familias_validas <- !("Outros" %in% familias_presentes)

cat(sprintf("   2.3. Todas famílias classificadas: %s\n", 
            ifelse(testes$familias_validas, "✅ PASSOU", "⚠️  MÉTODOS NÃO CLASSIFICADOS")))

if(!testes$familias_validas) {
  metodos_outros <- metricas_mensais %>%
    filter(familia == "Outros") %>%
    distinct(metodo) %>%
    pull(metodo)
  
  cat("        Métodos em 'Outros':\n")
  for(m in metodos_outros) {
    cat(sprintf("          - %s\n", m))
  }
}

# ===========================================================================
# 3. VALIDAÇÃO DE VALORES ####
# ===========================================================================

cat("\n📊 3. VALIDAÇÃO DE VALORES DAS MÉTRICAS...\n\n")

# Teste 3.1: Sem NAs em métricas críticas
colunas_criticas <- c("mae_mensal", "rmse_mensal", "bias_mensal", 
                      "demanda_real_total", "demanda_prevista_total")

nas_encontrados <- map_int(colunas_criticas, 
                           ~sum(is.na(metricas_mensais[[.x]])))
names(nas_encontrados) <- colunas_criticas

testes$sem_nas_criticos <- all(nas_encontrados == 0)

cat(sprintf("   3.1. Métricas críticas sem NAs: %s\n", 
            ifelse(testes$sem_nas_criticos, "✅ PASSOU", "⚠️  NAs DETECTADOS")))

if(!testes$sem_nas_criticos) {
  cat("        NAs por coluna:\n")
  for(col in names(nas_encontrados[nas_encontrados > 0])) {
    cat(sprintf("          - %s: %s NAs (%.2f%%)\n", 
                col, 
                format(nas_encontrados[col], big.mark = ","),
                nas_encontrados[col] / nrow(metricas_mensais) * 100))
  }
}

# Teste 3.2: MAE e RMSE não-negativos
valores_negativos <- metricas_mensais %>%
  filter(mae_mensal < 0 | rmse_mensal < 0) %>%
  nrow()

testes$metricas_positivas <- valores_negativos == 0

cat(sprintf("   3.2. MAE/RMSE não-negativos: %s\n", 
            ifelse(testes$metricas_positivas, "✅ PASSOU", "❌ FALHOU")))

if(!testes$metricas_positivas) {
  cat(sprintf("        %d linhas com valores negativos detectadas\n", 
              valores_negativos))
}

# Teste 3.3: RMSE >= MAE (propriedade matemática)
violacoes_rmse_mae <- metricas_mensais %>%
  filter(rmse_mensal < mae_mensal & !is.na(rmse_mensal) & !is.na(mae_mensal)) %>%
  nrow()

testes$rmse_maior_mae <- violacoes_rmse_mae == 0

cat(sprintf("   3.3. RMSE >= MAE: %s\n", 
            ifelse(testes$rmse_maior_mae, "✅ PASSOU", "❌ FALHOU")))

if(!testes$rmse_maior_mae) {
  cat(sprintf("        %d violações detectadas (RMSE < MAE)\n", 
              violacoes_rmse_mae))
}

# Teste 3.4: Demandas não-negativas
demandas_negativas <- metricas_mensais %>%
  filter(demanda_real_total < 0 | demanda_prevista_total < 0) %>%
  nrow()

testes$demandas_positivas <- demandas_negativas == 0

cat(sprintf("   3.4. Demandas não-negativas: %s\n", 
            ifelse(testes$demandas_positivas, "✅ PASSOU", "❌ FALHOU")))

if(!testes$demandas_positivas) {
  cat(sprintf("        %d linhas com demandas negativas\n", demandas_negativas))
}

# ===========================================================================
# 4. VALIDAÇÃO DE CONVERGÊNCIA ####
# ===========================================================================

cat("\n📊 4. ANÁLISE DE CONVERGÊNCIA...\n\n")

resumo_convergencia <- metricas_mensais %>%
  group_by(familia) %>%
  summarise(
    n_total = n(),
    n_convergido = sum(convergence),
    taxa_convergencia = n_convergido / n_total * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(taxa_convergencia))

cat("   Taxas de convergência por família:\n\n")
print(resumo_convergencia, n = Inf)

# Teste 4.1: Taxa mínima de convergência (>80%)
taxa_minima_aceitavel <- 80
familias_baixa_convergencia <- resumo_convergencia %>%
  filter(taxa_convergencia < taxa_minima_aceitavel)

testes$convergencia_adequada <- nrow(familias_baixa_convergencia) == 0

cat(sprintf("\n   4.1. Taxa mínima de convergência (>%d%%): %s\n", 
            taxa_minima_aceitavel,
            ifelse(testes$convergencia_adequada, "✅ PASSOU", "⚠️  ABAIXO DO ESPERADO")))

if(!testes$convergencia_adequada) {
  cat("\n        Famílias com convergência < 80%:\n")
  print(familias_baixa_convergencia)
}

# ===========================================================================
# 5. VALIDAÇÃO DE CONSISTÊNCIA MENSAL-ANUAL ####
# ===========================================================================

cat("\n📊 5. CONSISTÊNCIA ENTRE MÉTRICAS MENSAIS E ANUAIS...\n\n")

# Teste 5.1: Demanda anual = soma das mensais
validacao_demanda_anual <- metricas_mensais %>%
  group_by(origem, cd_material, metodo) %>%
  summarise(
    soma_mensal = sum(demanda_real_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  inner_join(
    metricas_anuais %>% 
      select(origem, cd_material, metodo, demanda_real_anual),
    by = c("origem", "cd_material", "metodo")
  ) %>%
  mutate(
    diferenca_abs = abs(soma_mensal - demanda_real_anual),
    consistente = diferenca_abs < 0.01  # Tolerância numérica
  )

prop_consistente <- mean(validacao_demanda_anual$consistente) * 100

testes$demanda_anual_consistente <- prop_consistente > 99

cat(sprintf("   5.1. Consistência demanda anual: %s\n", 
            ifelse(testes$demanda_anual_consistente, "✅ PASSOU", "⚠️  INCONSISTÊNCIAS")))
cat(sprintf("        %.2f%% das linhas são consistentes\n", prop_consistente))

if(!testes$demanda_anual_consistente) {
  inconsistencias <- validacao_demanda_anual %>%
    filter(!consistente) %>%
    arrange(desc(diferenca_abs)) %>%
    head(10)
  
  cat("\n        Top 10 maiores inconsistências:\n")
  print(inconsistencias)
}

# ===========================================================================
# 6. VALIDAÇÃO DE CATEGORIAS SBC ####
# ===========================================================================

cat("\n📊 6. VALIDAÇÃO DE CATEGORIAS SBC...\n\n")

# Teste 6.1: Proporção de materiais classificados
prop_classificados <- mean(!is.na(metricas_mensais$categoria_sbc)) * 100

testes$sbc_completa <- prop_classificados > 95

cat(sprintf("   6.1. Materiais com classificação SBC: %.2f%%\n", 
            prop_classificados))
cat(sprintf("        Status: %s\n", 
            ifelse(testes$sbc_completa, "✅ PASSOU", "⚠️  MUITOS NAs")))

if(!testes$sbc_completa) {
  n_sem_sbc <- sum(is.na(metricas_mensais$categoria_sbc))
  cat(sprintf("        %s previsões sem categoria SBC\n", 
              format(n_sem_sbc, big.mark = ",")))
}

# Distribuição de categorias
cat("\n   Distribuição de categorias SBC:\n\n")
metricas_mensais %>%
  filter(!is.na(categoria_sbc)) %>%
  count(categoria_sbc) %>%
  mutate(prop = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  print()

# ===========================================================================
# 7. ESTATÍSTICAS DESCRITIVAS ####
# ===========================================================================

cat("\n📊 7. ESTATÍSTICAS DESCRITIVAS DAS MÉTRICAS...\n\n")

cat("   MAE (Mean Absolute Error):\n")
summary(metricas_mensais$mae_mensal) %>% print()

cat("\n   RMSE (Root Mean Squared Error):\n")
summary(metricas_mensais$rmse_mensal) %>% print()

cat("\n   Bias (Mean Error):\n")
summary(metricas_mensais$bias_mensal) %>% print()

cat("\n   LinLin (p=0.85):\n")
summary(metricas_mensais$linlin_mensal) %>% print()

# ===========================================================================
# 8. RESUMO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RESUMO DA VALIDAÇÃO\n")
cat(strrep("=", 70), "\n\n")

n_testes <- length(testes)
n_passou <- sum(unlist(testes))

cat(sprintf("📊 Total de testes: %d\n", n_testes))
cat(sprintf("✅ Testes passados: %d (%.1f%%)\n", 
            n_passou, n_passou / n_testes * 100))
cat(sprintf("⚠️  Testes com ressalvas: %d\n", n_testes - n_passou))

cat("\n📋 Status detalhado:\n\n")
for(teste in names(testes)) {
  status <- ifelse(testes[[teste]], "✅", "⚠️ ")
  cat(sprintf("   %s %s\n", status, teste))
}

if(all(unlist(testes))) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ✅ TODOS OS TESTES PASSARAM - RESULTADOS CONFIÁVEIS      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
} else {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ⚠️  ALGUNS TESTES FALHARAM - REVISAR RESULTADOS         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
}

cat("\n", strrep("=", 70), "\n\n")

# Salvar resultados da validação
validacao_output <- list(
  testes = testes,
  resumo_convergencia = resumo_convergencia,
  timestamp = Sys.time()
)

saveRDS(
  validacao_output,
  here("output/reports/05_validation_results.rds")
)

cat("💾 Resultados da validação salvos: 05_validation_results.rds\n\n")
