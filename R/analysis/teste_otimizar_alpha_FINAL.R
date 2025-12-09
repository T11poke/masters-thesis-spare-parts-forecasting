# TESTE DE DIAGNÓSTICO: Por que otimizar_alpha retorna NA?
# Data: 2025-12-09

library(here)
library(tidyverse)
library(tsintermittent)

# Carregar função
source(here("R/functions/intermittent_functions.R"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║     🔍 DIAGNÓSTICO: Função otimizar_alpha                  ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ===========================================================================
# TESTE 1: Carregar dados reais ####
# ===========================================================================

cat("📊 TESTE 1: Usando dados reais do forecasting\n\n")

# Carregar splits
splits_list <- readRDS(here("data/processed/train_test_splits.rds"))

# Pegar primeiro material intermitente da origem 1
origem_1 <- splits_list$origem_1
sbc <- origem_1$sbc_classification

material_inter <- sbc %>%
  filter(categoria_sbc %in% c("Intermittent", "Lumpy")) %>%
  slice(1) %>%
  pull(cd_material)

cat(sprintf("   Material selecionado: %s\n", material_inter))

# Extrair série
serie <- origem_1$train %>%
  filter(cd_material == material_inter) %>%
  arrange(data_competencia) %>%
  pull(qt_total)

cat(sprintf("   Comprimento da série: %d períodos\n", length(serie)))
cat(sprintf("   Valores não-zero: %d (%.1f%%)\n", 
            sum(serie > 0), mean(serie > 0) * 100))
cat(sprintf("   Demanda média (não-zero): %.2f\n", mean(serie[serie > 0])))

# ===========================================================================
# TESTE 2: Testar função otimizar_alpha ####
# ===========================================================================

cat("\n📊 TESTE 2: Testando otimizar_alpha com série real\n\n")

# Teste 2.1: Croston
cat("   2.1. Testando método Croston...\n")
alpha_croston <- tryCatch({
  otimizar_alpha(serie, method = "croston")
}, error = function(e) {
  cat(sprintf("      ❌ ERRO: %s\n", conditionMessage(e)))
  return(NA)
})
cat(sprintf("      Alpha otimizado: %s\n", 
            ifelse(is.na(alpha_croston), "NA (FALHOU)", 
                   sprintf("%.3f", alpha_croston))))

# Teste 2.2: SBA
cat("\n   2.2. Testando método SBA...\n")
alpha_sba <- tryCatch({
  otimizar_alpha(serie, method = "sba")
}, error = function(e) {
  cat(sprintf("      ❌ ERRO: %s\n", conditionMessage(e)))
  return(NA)
})
cat(sprintf("      Alpha otimizado: %s\n", 
            ifelse(is.na(alpha_sba), "NA (FALHOU)", 
                   sprintf("%.3f", alpha_sba))))

# Teste 2.3: TSB
cat("\n   2.3. Testando método TSB...\n")
alpha_tsb <- tryCatch({
  otimizar_alpha(serie, method = "tsb")
}, error = function(e) {
  cat(sprintf("      ❌ ERRO: %s\n", conditionMessage(e)))
  return(NA)
})
cat(sprintf("      Alpha otimizado: %s\n", 
            ifelse(is.na(alpha_tsb), "NA (FALHOU)", 
                   sprintf("%.3f", alpha_tsb))))

# ===========================================================================
# TESTE 3: Testar manualmente tsintermittent::crost ####
# ===========================================================================

cat("\n📊 TESTE 3: Testando tsintermittent::crost diretamente\n\n")

n <- length(serie)
cv_horizon <- 6
train_cv <- serie[1:(n - cv_horizon)]
valid_cv <- serie[(n - cv_horizon + 1):n]

cat(sprintf("   Treino CV: %d períodos\n", length(train_cv)))
cat(sprintf("   Validação CV: %d períodos\n", length(valid_cv)))

# Testar um alpha específico
alpha_test <- 0.10

for(metodo in c("croston", "sba", "tsb")) {
  cat(sprintf("\n   Testando %s com alpha=%.2f:\n", metodo, alpha_test))
  
  resultado <- tryCatch({
    fit <- tsintermittent::crost(
      train_cv, 
      h = cv_horizon,
      w = alpha_test,
      type = metodo,
      init = "mean",
      outplot = FALSE
    )
    
    fc <- as.numeric(fit$mean)
    mae <- mean(abs(valid_cv - fc), na.rm = TRUE)
    
    list(sucesso = TRUE, mae = mae, previsoes = fc)
    
  }, error = function(e) {
    list(sucesso = FALSE, erro = conditionMessage(e))
  })
  
  if(resultado$sucesso) {
    cat(sprintf("      ✅ SUCESSO - MAE: %.3f\n", resultado$mae))
    cat(sprintf("      Previsões: %s\n", 
                paste(round(resultado$previsoes, 2), collapse = ", ")))
  } else {
    cat(sprintf("      ❌ FALHOU - Erro: %s\n", resultado$erro))
  }
}

# ===========================================================================
# TESTE 4: Testar série sintética ####
# ===========================================================================

cat("\n📊 TESTE 4: Testando com série sintética\n\n")

# Criar série intermitente sintética
set.seed(42)
serie_sintetica <- c(
  0, 0, 5, 0, 0, 0, 3, 0, 0, 2, 0, 0,  # 12 períodos
  0, 4, 0, 0, 0, 6, 0, 0, 3, 0, 0, 0,  # 12 períodos
  5, 0, 0, 0, 0, 2, 0, 0, 4, 0, 0, 0,  # 12 períodos
  0, 3, 0, 0, 5, 0, 0, 0, 0, 2, 0, 0   # 12 períodos (48 total)
)

cat(sprintf("   Série sintética: %d períodos\n", length(serie_sintetica)))
cat(sprintf("   Valores não-zero: %d (%.1f%%)\n", 
            sum(serie_sintetica > 0), mean(serie_sintetica > 0) * 100))

cat("\n   Testando otimizar_alpha com série sintética:\n")

for(metodo in c("croston", "sba", "tsb")) {
  alpha_opt <- tryCatch({
    otimizar_alpha(serie_sintetica, method = metodo)
  }, error = function(e) {
    cat(sprintf("      ❌ %s ERRO: %s\n", toupper(metodo), conditionMessage(e)))
    return(NA)
  })
  
  if(!is.na(alpha_opt)) {
    cat(sprintf("      ✅ %s: alpha = %.3f\n", toupper(metodo), alpha_opt))
  } else {
    cat(sprintf("      ❌ %s: alpha = NA\n", toupper(metodo)))
  }
}

# ===========================================================================
# TESTE 5: Verificar se problema é comprimento da série ####
# ===========================================================================

cat("\n📊 TESTE 5: Testando diferentes comprimentos de série\n\n")

comprimentos <- c(12, 24, 36, 48, 60)

for(n_periodos in comprimentos) {
  
  if(n_periodos <= length(serie)) {
    serie_teste <- serie[1:n_periodos]
  } else {
    serie_teste <- rep(serie, ceiling(n_periodos / length(serie)))[1:n_periodos]
  }
  
  cat(sprintf("   Série com %d períodos:\n", n_periodos))
  
  alpha_opt <- tryCatch({
    otimizar_alpha(serie_teste, method = "croston", cv_horizon = 6)
  }, error = function(e) {
    NA
  })
  
  if(!is.na(alpha_opt)) {
    cat(sprintf("      ✅ Alpha otimizado: %.3f\n", alpha_opt))
  } else {
    cat(sprintf("      ❌ Falhou (série muito curta ou erro)\n"))
  }
}

# ===========================================================================
# DIAGNÓSTICO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("DIAGNÓSTICO FINAL\n")
cat(strrep("=", 70), "\n\n")

if(all(is.na(c(alpha_croston, alpha_sba, alpha_tsb)))) {
  cat("🔴 PROBLEMA CRÍTICO: Todos os métodos falharam\n\n")
  cat("   Possíveis causas:\n")
  cat("   1. Série muito curta (< 18 períodos necessários)\n")
  cat("   2. Série com poucos valores não-zero\n")
  cat("   3. Problema no pacote tsintermittent\n")
  cat("   4. Parâmetros incompatíveis com a série\n\n")
  
  cat("   SOLUÇÃO RECOMENDADA:\n")
  cat("   Desabilitar otimização de alpha temporariamente:\n")
  cat("   config.yaml → optimize_alpha: false\n\n")
  
} else if(any(is.na(c(alpha_croston, alpha_sba, alpha_tsb)))) {
  cat("⚠️  PROBLEMA PARCIAL: Alguns métodos falharam\n\n")
  cat(sprintf("   Croston: %s\n", 
              ifelse(is.na(alpha_croston), "❌ FALHOU", "✅ OK")))
  cat(sprintf("   SBA: %s\n", 
              ifelse(is.na(alpha_sba), "❌ FALHOU", "✅ OK")))
  cat(sprintf("   TSB: %s\n", 
              ifelse(is.na(alpha_tsb), "❌ FALHOU", "✅ OK")))
  
} else {
  cat("✅ TODOS OS MÉTODOS FUNCIONARAM\n\n")
  cat("   O problema pode estar:\n")
  cat("   1. Em como a função é chamada no script 04b\n")
  cat("   2. Na paralelização (workers não encontram a função)\n")
  cat("   3. Em materiais específicos com séries problemáticas\n\n")
}

cat(strrep("=", 70), "\n\n")

