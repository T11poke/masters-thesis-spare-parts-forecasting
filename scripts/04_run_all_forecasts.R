# 04 - ORQUESTRADOR: EXECUTAR TODOS OS FORECASTS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Executa sequencialmente os três pipelines de forecasting
# Data: 2026-01-22
# Versão: 2.0.1

library(here)
library(tictoc)

source(here("R/utils/load_config.R"))
config <- load_config()

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║           PIPELINE COMPLETO DE FORECASTING                 ║\n")
cat("║                                                            ║\n")
cat("║  Este script executa SEQUENCIALMENTE:                      ║\n")
cat("║  1. 04a - Modelos Baseline (perspectiva mensal)            ║\n")
cat("║  2. 04b - Modelos Intermitentes (perspectiva mensal)       ║\n")
cat("║  3. 04c - Modelos Probabilísticos e ADIDA (mensal)         ║\n")
cat("║  4. 04d - Todos os modelos (perspectiva anual)             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Verificar modo debug
DEBUG_MODE <- Sys.getenv("FORECAST_DEBUG", "FALSE") == "TRUE" ||
  isTRUE(config$parameters$forecasting$debug_mode)

if(DEBUG_MODE) {
  cat("⚠️  MODO DEBUG ATIVO\n")
  cat("   Os 4 scripts rodarão em modo debug\n\n")
  
  resposta <- readline(prompt = "Continuar? (s/n): ")
  if(tolower(resposta) != "s") {
    stop("Execução cancelada pelo usuário")
  }
} else {
  cat("🎯 MODO PRODUÇÃO\n")
  cat("   Processamento completo de todos os materiais\n")
  cat("   Tempo estimado: 60-75 minutos\n\n")
  
  resposta <- readline(prompt = "Confirmar execução? (s/n): ")
  if(tolower(resposta) != "s") {
    stop("Execução cancelada pelo usuário")
  }
}

cat("\n")

# Timestamp de início
inicio_geral <- Sys.time()
tempos_execucao <- list()

# ===========================================================================
# SCRIPT 04a: BASELINE MODELS (MENSAL) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ETAPA 1/4: MODELOS BASELINE (PERSPECTIVA MENSAL)\n")
cat(strrep("=", 70), "\n\n")

tic("04a - Baseline Models")

tryCatch({
  source(here("scripts/04a_baseline_models_forecast.R"), encoding = "UTF-8")
  tempo_04a <- toc()
  tempos_execucao$baseline <- tempo_04a$toc - tempo_04a$tic
  
  cat("\n✅ Script 04a concluído com sucesso!\n")
  
}, error = function(e) {
  cat("\n❌ ERRO no script 04a:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline interrompido devido a erro no 04a")
})

# Pausa de 5 segundos para liberar recursos
cat("\n⏸️  Pausa de 5 segundos para liberar recursos...\n")
Sys.sleep(5)

# ===========================================================================
# SCRIPT 04b: INTERMITTENT MODELS (MENSAL) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ETAPA 2/4: MODELOS INTERMITENTES (PERSPECTIVA MENSAL)\n")
cat(strrep("=", 70), "\n\n")

tic("04b - Intermittent Models")

tryCatch({
  source(here("scripts/04b_Intermittent_Demand_Models_Forecast.R"), encoding = "UTF-8")
  tempo_04b <- toc()
  tempos_execucao$intermittent <- tempo_04b$toc - tempo_04b$tic
  
  cat("\n✅ Script 04b concluído com sucesso!\n")
  
}, error = function(e) {
  cat("\n❌ ERRO no script 04b:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline interrompido devido a erro no 04b")
})

# Pausa
cat("\n⏸️  Pausa de 5 segundos para liberar recursos...\n")
Sys.sleep(5)

# ===========================================================================
# SCRIPT 04c: PROBABILISTIC & ADIDA (MENSAL) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ETAPA 3/4: MODELOS PROBABILÍSTICOS E ADIDA (PERSPECTIVA MENSAL)\n")
cat(strrep("=", 70), "\n\n")

tic("04c - Probabilistic & ADIDA")

tryCatch({
  source(here("scripts/04c_probabilistic_ADIDA_forecast.R"), encoding = "UTF-8")
  tempo_04c <- toc()
  tempos_execucao$probabilistic <- tempo_04c$toc - tempo_04c$tic
  
  cat("\n✅ Script 04c concluído com sucesso!\n")
  
}, error = function(e) {
  cat("\n❌ ERRO no script 04c:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline interrompido devido a erro no 04c")
})

# Pausa
cat("\n⏸️  Pausa de 5 segundos para liberar recursos...\n")
Sys.sleep(5)

# ===========================================================================
# SCRIPT 04d: ANNUAL FORECASTS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ETAPA 4/4: MODELOS COM DADOS AGREGADOS ANUALMENTE\n")
cat(strrep("=", 70), "\n\n")

tic("04d - Annual Forecasts")

tryCatch({
  source(here("scripts/04d_annual_forecasts.R"), encoding = "UTF-8")
  tempo_04d <- toc()
  tempos_execucao$annual <- tempo_04d$toc - tempo_04d$tic
  
  cat("\n✅ Script 04d concluído com sucesso!\n")
  
}, error = function(e) {
  cat("\n❌ ERRO no script 04d:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline interrompido devido a erro no 04d")
})

# ===========================================================================
# RELATÓRIO FINAL ####
# ===========================================================================

fim_geral <- Sys.time()
tempo_total <- as.numeric(difftime(fim_geral, inicio_geral, units = "mins"))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎉 PIPELINE COMPLETO CONCLUÍDO! 🎉\n")
cat(strrep("=", 70), "\n\n")

cat("⏱️  RESUMO DE TEMPO DE EXECUÇÃO:\n\n")
cat(sprintf("   1️⃣  Baseline Models:       %.1f min (%.1f%%)\n",
            tempos_execucao$baseline / 60,
            tempos_execucao$baseline / (tempo_total * 60) * 100))
cat(sprintf("   2️⃣  Intermittent Models:   %.1f min (%.1f%%)\n",
            tempos_execucao$intermittent / 60,
            tempos_execucao$intermittent / (tempo_total * 60) * 100))
cat(sprintf("   3️⃣  Probabilistic/ADIDA:   %.1f min (%.1f%%)\n",
            tempos_execucao$probabilistic / 60,
            tempos_execucao$probabilistic / (tempo_total * 60) * 100))
cat(sprintf("   4️⃣  Annual Forecasts:      %.1f min (%.1f%%)\n",
            tempos_execucao$annual / 60,
            tempos_execucao$annual / (tempo_total * 60) * 100))
cat(sprintf("\n   🕐 TEMPO TOTAL:           %.1f min (%.1f horas)\n",
            tempo_total,
            tempo_total / 60))

cat("\n📁 Arquivos gerados:\n")
cat("   PERSPECTIVA MENSAL:\n")
cat("   - output/forecasts/baseline/forecasts_baseline.rds\n")
cat("   - output/forecasts/intermittent/forecasts_intermittent.rds\n")
cat("   - output/forecasts/probabilistic/forecasts_probabilistic.rds\n")
cat("\n   PERSPECTIVA ANUAL:\n")
cat("   - output/forecasts/annual/forecasts_annual.rds\n")
cat("\n   CHECKPOINTS E RELATÓRIOS:\n")
cat("   - output/checkpoints/*.rds\n")
cat("   - output/reports/04*/*.xlsx\n")

cat("\n✅ Próximo passo: Executar script 05_consolidate_results.R\n")
cat("   (O script 05 precisará ser atualizado para incluir dados anuais)\n")

cat("\n", strrep("=", 70), "\n", sep = "")
cat(sprintf("Início:  %s\n", format(inicio_geral, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Término: %s\n", format(fim_geral, "%Y-%m-%d %H:%M:%S")))
cat(strrep("=", 70), "\n\n")

# Salvar log de execução
log_execucao <- list(
  inicio = inicio_geral,
  fim = fim_geral,
  tempo_total_min = tempo_total,
  tempos_por_script = tempos_execucao,
  debug_mode = DEBUG_MODE,
  timestamp = Sys.time(),
  scripts_executados = c("04a", "04b", "04c", "04d")
)

saveRDS(
  log_execucao,
  here("output/reports/execution_log_04_forecasts.rds")
)

cat("💾 Log de execução salvo: execution_log_04_forecasts.rds\n\n")

plan(sequential)