# DIAGNÓSTICO PRÉ-EXECUÇÃO SCRIPT 06 ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Verifica integridade e estrutura dos dados antes do script 06
# Data: 2025-12-10
# Versão: 1.0.0

library(here)
library(tidyverse)

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║     DIAGNÓSTICO DE DADOS PARA SCRIPT 06                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ===========================================================================
# 1. VERIFICAR EXISTÊNCIA DE ARQUIVOS ####
# ===========================================================================

cat("📂 1. VERIFICANDO ARQUIVOS NECESSÁRIOS...\n\n")

arquivos_necessarios <- c(
  consolidado = here("output/forecasts/forecasts_consolidated.rds"),
  splits = here("data/processed/train_test_splits.rds")
)

arquivos_ok <- TRUE

for(tipo in names(arquivos_necessarios)) {
  arquivo <- arquivos_necessarios[tipo]
  existe <- file.exists(arquivo)
  
  if(existe) {
    tamanho <- file.size(arquivo) / 1024^2  # MB
    cat(sprintf("   ✅ %s: %.1f MB\n", tipo, tamanho))
  } else {
    cat(sprintf("   ❌ %s: NÃO ENCONTRADO\n", tipo))
    arquivos_ok <- FALSE
  }
}

if(!arquivos_ok) {
  cat("\n❌ Execute o script 05 antes de continuar.\n\n")
  stop("Arquivos necessários não encontrados.")
}

# ===========================================================================
# 2. CARREGAR E INSPECIONAR DADOS ####
# ===========================================================================

cat("\n📊 2. CARREGANDO DADOS CONSOLIDADOS...\n\n")

consolidado <- readRDS(arquivos_necessarios["consolidado"])

metricas_mensais <- consolidado$metricas_mensais
metricas_anuais <- consolidado$metricas_anuais
metadata <- consolidado$metadata

cat(sprintf("✅ Dados carregados com sucesso\n"))
cat(sprintf("   - Timestamp: %s\n", metadata$timestamp))
cat(sprintf("   - Versão config: %s\n", metadata$config_version))

# ===========================================================================
# 3. VERIFICAR ESTRUTURA DE ORIGENS ####
# ===========================================================================

cat("\n📊 3. ANALISANDO ESTRUTURA DE ORIGENS TEMPORAIS...\n\n")

# Contar origens únicas
origens_unicas <- unique(metricas_mensais$origem)
n_origens <- length(origens_unicas)

cat(sprintf("   Origens detectadas: %d\n", n_origens))
cat(sprintf("   IDs: %s\n\n", paste(origens_unicas, collapse = ", ")))

# Contar materiais e métodos por origem
estrutura_origens <- metricas_mensais %>%
  group_by(origem) %>%
  summarise(
    n_materiais = n_distinct(cd_material),
    n_metodos = n_distinct(metodo),
    n_obs = n(),
    .groups = 'drop'
  )

cat("   Estrutura por origem:\n\n")
print(estrutura_origens)

# Verificar balanceamento
if(n_distinct(estrutura_origens$n_metodos) == 1 && 
   n_distinct(estrutura_origens$n_materiais) == 1) {
  cat("\n   ✅ Estrutura balanceada: mesmos métodos e materiais em todas as origens\n")
} else {
  cat("\n   ⚠️  Estrutura DESBALANCEADA detectada:\n")
  cat("      Diferentes métodos ou materiais entre origens\n")
  cat("      Isso pode afetar agregações e testes estatísticos\n")
}

# ===========================================================================
# 4. VERIFICAR DISPONIBILIDADE DE VARIÁVEIS ####
# ===========================================================================

cat("\n📊 4. VERIFICANDO VARIÁVEIS DISPONÍVEIS...\n\n")

# Variáveis esperadas
vars_esperadas_mensais <- c(
  "origem", "cd_material", "categoria_sbc", "metodo", "familia",
  "mae_mensal", "rmse_mensal", "bias_mensal", "linlin_mensal",
  "mad_mean_ratio", "per", "convergence"
)

vars_presentes <- names(metricas_mensais)
vars_faltantes <- setdiff(vars_esperadas_mensais, vars_presentes)
vars_extras <- setdiff(vars_presentes, vars_esperadas_mensais)

if(length(vars_faltantes) == 0) {
  cat("   ✅ Todas as variáveis mensais esperadas estão presentes\n")
} else {
  cat("   ⚠️  Variáveis mensais FALTANTES:\n")
  for(v in vars_faltantes) {
    cat(sprintf("      - %s\n", v))
  }
}

if(length(vars_extras) > 0) {
  cat("\n   📋 Variáveis adicionais detectadas:\n")
  for(v in vars_extras) {
    cat(sprintf("      - %s\n", v))
  }
  
  # Verificar se subsistema está presente
  if("subsistema" %in% vars_extras) {
    cat("\n   ✅ Variável 'subsistema' detectada:\n")
    cat("      Análise por subsistema SISCEAB será habilitada no script 06\n")
  }
}

# ===========================================================================
# 5. ANALISAR CONVERGÊNCIA ####
# ===========================================================================

cat("\n📊 5. ANALISANDO TAXA DE CONVERGÊNCIA...\n\n")

convergencia_global <- metricas_mensais %>%
  summarise(
    n_total = n(),
    n_convergiu = sum(convergence),
    taxa_convergencia = n_convergiu / n_total * 100
  )

cat(sprintf("   Taxa de convergência global: %.1f%%\n", 
            convergencia_global$taxa_convergencia))
cat(sprintf("   (%d de %s observações)\n\n",
            convergencia_global$n_convergiu,
            format(convergencia_global$n_total, big.mark = ",")))

# Por método
convergencia_por_metodo <- metricas_mensais %>%
  group_by(metodo, familia) %>%
  summarise(
    n_total = n(),
    n_convergiu = sum(convergence),
    taxa_convergencia = n_convergiu / n_total * 100,
    .groups = 'drop'
  ) %>%
  arrange(taxa_convergencia)

cat("   Top 5 métodos com MENOR convergência:\n\n")
print(convergencia_por_metodo %>% head(5))

if(any(convergencia_por_metodo$taxa_convergencia < 80)) {
  cat("\n   ⚠️  ATENÇÃO: Métodos com convergência <80% detectados\n")
  cat("      Estes métodos terão limitações na análise comparativa\n")
}

# ===========================================================================
# 6. VERIFICAR DISTRIBUIÇÃO POR CATEGORIA SBC ####
# ===========================================================================

cat("\n📊 6. VERIFICANDO DISTRIBUIÇÃO POR CATEGORIA SBC...\n\n")

dist_sbc <- metricas_mensais %>%
  distinct(cd_material, categoria_sbc) %>%
  count(categoria_sbc, name = "n_materiais") %>%
  mutate(prop = n_materiais / sum(n_materiais) * 100) %>%
  arrange(desc(n_materiais))

print(dist_sbc)

# Verificar se há categorias com poucos materiais
categorias_pequenas <- dist_sbc %>%
  filter(n_materiais < 10)

if(nrow(categorias_pequenas) > 0) {
  cat("\n   ⚠️  Categorias com menos de 10 materiais:\n")
  for(i in 1:nrow(categorias_pequenas)) {
    cat(sprintf("      - %s: %d materiais\n",
                categorias_pequenas$categoria_sbc[i],
                categorias_pequenas$n_materiais[i]))
  }
  cat("      Análise segmentada pode ter poder estatístico limitado\n")
}

# ===========================================================================
# 7. VERIFICAR PRESENÇA DE NAs ####
# ===========================================================================

cat("\n📊 7. VERIFICANDO PRESENÇA DE VALORES NA...\n\n")

# Contar NAs por coluna (apenas convergentes)
na_counts <- metricas_mensais %>%
  filter(convergence) %>%
  summarise(across(where(is.numeric), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "n_nas") %>%
  filter(n_nas > 0) %>%
  arrange(desc(n_nas))

if(nrow(na_counts) == 0) {
  cat("   ✅ Nenhum valor NA encontrado em observações convergentes\n")
} else {
  cat("   ⚠️  Valores NA detectados:\n\n")
  print(na_counts)
  cat("\n      Estes NAs serão tratados com na.rm=TRUE nas agregações\n")
}

# ===========================================================================
# 8. SIMULAR AGREGAÇÃO (TESTE) ####
# ===========================================================================

cat("\n📊 8. SIMULANDO AGREGAÇÃO MÉTODO-ORIGEM...\n\n")

# Testar agregação como será feita no script 06
teste_agregacao <- metricas_mensais %>%
  filter(convergence) %>%
  group_by(metodo, familia, origem) %>%
  summarise(
    n_materiais = n(),
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    .groups = 'drop'
  )

cat(sprintf("   Linhas geradas: %d\n", nrow(teste_agregacao)))
cat(sprintf("   Métodos únicos: %d\n", n_distinct(teste_agregacao$metodo)))
cat(sprintf("   Origens por método:\n"))

origens_por_metodo <- teste_agregacao %>%
  count(metodo) %>%
  pull(n) %>%
  table()

print(origens_por_metodo)

if(all(names(origens_por_metodo) == "1")) {
  cat("\n   ⚠️  IMPORTANTE: Cada método aparece em apenas 1 origem\n")
  cat("      Desvios-padrão (SD) entre origens serão NA no script 06\n")
  cat("      Coeficientes de variação (CV) serão NA\n")
  cat("      Isto é ESPERADO se houver apenas 1 origem temporal\n\n")
  cat("      📌 RECOMENDAÇÃO:\n")
  cat("         Para análise de estabilidade temporal completa,\n")
  cat("         execute com múltiplas origens (script 02 com origins > 1)\n")
} else {
  cat("\n   ✅ Múltiplas origens por método detectadas\n")
  cat("      Análise de estabilidade temporal estará completa\n")
}

# ===========================================================================
# 9. VERIFICAR MÉTODO POISSON ####
# ===========================================================================

cat("\n📊 9. VERIFICANDO PRESENÇA DO MÉTODO POISSON...\n\n")

metodos_disponiveis <- unique(metricas_mensais$metodo)
poisson_presente <- any(str_detect(tolower(metodos_disponiveis), "poisson"))

if(poisson_presente) {
  metodo_poisson <- metodos_disponiveis[str_detect(tolower(metodos_disponiveis), "poisson")][1]
  
  cat(sprintf("   ✅ Método Poisson encontrado: '%s'\n", metodo_poisson))
  
  # Estatísticas do Poisson
  stats_poisson <- metricas_mensais %>%
    filter(metodo == metodo_poisson, convergence) %>%
    summarise(
      n_obs = n(),
      mae_medio = mean(mae_mensal, na.rm = TRUE),
      taxa_convergencia = mean(convergence) * 100
    )
  
  cat(sprintf("      - Observações: %d\n", stats_poisson$n_obs))
  cat(sprintf("      - MAE médio: %.2f\n", stats_poisson$mae_medio))
  cat(sprintf("      - Taxa convergência: %.1f%%\n", stats_poisson$taxa_convergencia))
  cat("\n   ✅ Comparação com Poisson será habilitada no script 06\n")
  
} else {
  cat("   ⚠️  Método Poisson NÃO encontrado\n")
  cat("      Comparação com método atual do DECEA não estará disponível\n")
  cat("      Para habilitar: incluir Poisson nos scripts 04a-c\n")
}

# ===========================================================================
# 10. RESUMO E RECOMENDAÇÕES ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RESUMO DO DIAGNÓSTICO\n")
cat(strrep("=", 70), "\n\n")

# Compilar status
status <- list(
  arquivos_ok = arquivos_ok,
  origens_multiplas = n_origens > 1,
  estrutura_balanceada = n_distinct(estrutura_origens$n_metodos) == 1,
  convergencia_ok = convergencia_global$taxa_convergencia >= 80,
  categorias_ok = all(dist_sbc$n_materiais >= 10),
  sem_nas = nrow(na_counts) == 0,
  poisson_presente = poisson_presente
)

n_ok <- sum(unlist(status))
n_total <- length(status)

cat(sprintf("Status geral: %d/%d verificações passaram\n\n", n_ok, n_total))

# Detalhamento
cat("✅ VERIFICAÇÕES OK:\n")
if(status$arquivos_ok) cat("   - Arquivos necessários presentes\n")
if(status$convergencia_ok) cat("   - Taxa de convergência adequada (≥80%)\n")
if(status$sem_nas) cat("   - Sem valores NA problemáticos\n")
if(status$estrutura_balanceada) cat("   - Estrutura balanceada entre origens\n")

cat("\n⚠️  ATENÇÕES/LIMITAÇÕES:\n")
if(!status$origens_multiplas) {
  cat("   - Apenas 1 origem temporal detectada\n")
  cat("     → Análise de estabilidade temporal será limitada\n")
  cat("     → SDs e CVs serão NA (comportamento esperado)\n")
}
if(!status$categorias_ok) {
  cat("   - Algumas categorias SBC têm poucos materiais\n")
  cat("     → Poder estatístico limitado em análise segmentada\n")
}
if(!status$poisson_presente) {
  cat("   - Método Poisson não detectado\n")
  cat("     → Comparação com método atual indisponível\n")
}

cat("\n📋 RECOMENDAÇÕES:\n\n")

if(!status$origens_multiplas) {
  cat("1. Para análise temporal completa:\n")
  cat("   - Configure config$parameters$origins > 1 no config.yaml\n")
  cat("   - Re-execute script 02 (train_test_split)\n")
  cat("   - Re-execute scripts 04a-c e 05\n\n")
}

if(!status$poisson_presente) {
  cat("2. Para incluir comparação com Poisson:\n")
  cat("   - Adicione Poisson aos scripts 04a ou 04c\n")
  cat("   - Re-execute consolidação (script 05)\n\n")
}

cat("3. Prosseguir com script 06:\n")
if(n_ok >= 5) {
  cat("   ✅ PRONTO para executar script 06\n")
  cat("      Os resultados serão válidos considerando as limitações acima\n")
} else {
  cat("   ⚠️  Recomenda-se corrigir problemas antes de continuar\n")
  cat("      Script 06 pode falhar ou gerar resultados incompletos\n")
}

cat("\n", strrep("=", 70), "\n\n")

# Salvar diagnóstico
diagnostico_output <- list(
  status = status,
  estrutura_origens = estrutura_origens,
  convergencia_por_metodo = convergencia_por_metodo,
  dist_sbc = dist_sbc,
  timestamp = Sys.time()
)

saveRDS(
  diagnostico_output,
  here("output/analysis/diagnostico_pre_script06.rds")
)

cat("💾 Diagnóstico salvo: diagnostico_pre_script06.rds\n\n")