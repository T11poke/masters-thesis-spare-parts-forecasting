# 02 - DIVISÃO TEMPORAL DOS DADOS - TRAIN/TEST SPLIT ####
#
# Descrição: Implementa estratégia de validação rolling origin com janela
#            expansiva, classificação SBC por origem e validações de integridade
# Data: 2025-12-03
# Versão: 2.1.0

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(tsibble)
library(lubridate)
library(writexl)

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO DIVISÃO TEMPORAL DOS DADOS", "INFO")
log_message("========================================", "INFO")

# Carregar dados processados ####
log_message("Carregando dados processados do script anterior", "INFO")

ts_completa <- readRDS(here(config$paths$data$processed, "ts_completa.rds"))

cat("\n📊 Dados carregados:\n")
cat(sprintf("   - Total de registros: %s\n", format(nrow(ts_completa), big.mark = ",")))
cat(sprintf("   - Materiais únicos: %s\n", format(n_distinct(ts_completa$cd_material), big.mark = ",")))
cat(sprintf("   - Período: %s até %s\n", 
            min(ts_completa$data_competencia),
            max(ts_completa$data_competencia)))

# 1. IDENTIFICAR ÚLTIMA DATA COMPLETA ####

log_message("Identificando última data completa disponível", "INFO")

# Identificar última data disponível
ultima_data_completa <- max(ts_completa$data_competencia)

cat("\n📅 Definição de período:\n")
cat(sprintf("   - Última data completa (análise): %s\n", ultima_data_completa))

log_message(sprintf("Última data completa definida: %s", ultima_data_completa), "INFO")

# 2. DEFINIR ORIGENS TEMPORAIS ####

log_message("Definindo origens temporais para validação rolling origin", "INFO")

# Extrair parâmetros de configuração
n_origins <- config$parameters$validation$n_origins
test_months <- config$parameters$validation$test_months
train_min_months <- config$parameters$validation$train_min_months

cat("\n🎯 Parâmetros de validação:\n")
cat(sprintf("   - Número de origens: %d\n", n_origins))
cat(sprintf("   - Horizonte de teste: %d meses\n", test_months))
cat(sprintf("   - Treino mínimo: %d meses\n", train_min_months))

source(
  here(
    "R/functions/train_test_split_functions.R"
  )
)

# Definir origens
primeira_data <- min(ts_completa$data_competencia)

origens_metadata <- definir_origens_temporais(
  ultima_data = ultima_data_completa,
  n_origins = n_origins,
  test_months = test_months,
  train_min_months = train_min_months,
  primeira_data_disponivel = primeira_data
)

# Exibir tabela de origens
cat("\n📋 Origens temporais definidas:\n\n")
origens_metadata %>%
  mutate(
    across(c(train_start, train_end, test_start, test_end), 
           ~format(.x, "%Y-%m"))
  ) %>%
  print()

log_message("Origens temporais definidas com sucesso", "INFO")

# 3. CRIAR SPLITS PARA CADA ORIGEM ####

log_message("Criando splits de treino/teste para cada origem", "INFO")

# Criar splits para todas as origens ####
cat("\n", strrep("=", 70), "\n", sep = "")
cat("CRIANDO SPLITS PARA TODAS AS ORIGENS\n")
cat("\n", strrep("=", 70), "\n", sep = "")

splits_list <- map2(
  split(origens_metadata, origens_metadata$origem_id),
  origens_metadata$origem_id,
  ~criar_split_origem(
    data = ts_completa,
    origem_info = .x,
    origem_id = .y
  )
)

names(splits_list) <- paste0("origem_", 1:n_origins)

log_message("Splits criados para todas as origens", "INFO")

# 4. ANÁLISE DE TRANSIÇÕES SBC ENTRE ORIGENS ####

log_message("Analisando transições de categorias SBC entre origens", "INFO")

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ANÁLISE DE TRANSIÇÕES SBC ENTRE ORIGENS\n")
cat("\n", strrep("=", 70), "\n", sep = "")

# Consolidar classificações de todas as origens
todas_classificacoes <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$sbc_classification %>%
    select(cd_material, categoria_sbc, adi, cv2, origem_id)
)

# Identificar materiais presentes em múltiplas origens
materiais_multiplas_origens <- todas_classificacoes %>%
  group_by(cd_material) %>%
  summarise(
    n_origens = n_distinct(origem_id),
    categorias = paste(unique(categoria_sbc), collapse = " → "),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_origens))

cat("\n📊 Presença de materiais nas origens:\n")
materiais_multiplas_origens %>%
  count(n_origens, name = "n_materiais") %>%
  mutate(percentual = n_materiais / sum(n_materiais) * 100) %>%
  print()

# Identificar transições (materiais que mudam de categoria)
transicoes <- todas_classificacoes %>%
  arrange(cd_material, origem_id) %>%
  group_by(cd_material) %>%
  mutate(
    categoria_anterior = lag(categoria_sbc),
    transicao = categoria_anterior != categoria_sbc
  ) %>%
  filter(!is.na(transicao) & transicao) %>%
  ungroup()

if (nrow(transicoes) > 0) {
  cat(sprintf("\n🔄 Transições detectadas: %s materiais mudaram de categoria\n",
              format(n_distinct(transicoes$cd_material), big.mark = ",")))
  
  # Resumo das transições mais comuns
  cat("\n📈 Transições mais frequentes:\n")
  transicoes %>%
    mutate(tipo_transicao = paste(categoria_anterior, "→", categoria_sbc)) %>%
    count(tipo_transicao, sort = TRUE) %>%
    head(10) %>%
    print()
  
} else {
  cat("\n✅ Nenhuma transição de categoria detectada\n")
}

# Criar matriz de transições
matriz_transicoes <- transicoes %>%
  count(categoria_anterior, categoria_sbc, name = "n_transicoes") %>%
  pivot_wider(
    names_from = categoria_sbc,
    values_from = n_transicoes,
    values_fill = 0
  )

analise_transicoes <- list(
  materiais_multiplas_origens = materiais_multiplas_origens,
  transicoes_detectadas = transicoes,
  matriz_transicoes = matriz_transicoes,
  todas_classificacoes = todas_classificacoes
)

log_message(sprintf("Análise de transições concluída: %d transições detectadas", 
                    nrow(transicoes)), "INFO")

# 5. VALIDAÇÕES GLOBAIS ####

log_message("Executando validações globais", "INFO")

cat("\n", strrep("=", 70), "\n", sep = "")
cat("VALIDAÇÕES GLOBAIS\n")
cat("\n", strrep("=", 70), "\n", sep = "")

# Validação 1: Número de materiais no teste
cat("\n🔍 Validação 1: Materiais nos conjuntos de teste\n")

n_materiais_teste <- map_dbl(
  splits_list,
  ~n_distinct(.x$test$cd_material)
)

cat("\nMateriais únicos em cada teste:\n")
tibble(
  origem = paste0("Origem ", 1:n_origins),
  n_materiais = n_materiais_teste
) %>% print()

# Validação 2: Cobertura temporal completa
cat("\n🔍 Validação 2: Cobertura temporal dos testes\n")

validacao_temporal <- map_dfr(
  names(splits_list),
  function(origem_nome) {
    origem <- splits_list[[origem_nome]]
    
    datas_esperadas <- seq(
      from = origem$metadata$test_start,
      to = origem$metadata$test_end,
      by = "month"
    )
    
    datas_presentes <- unique(origem$test$data_competencia)
    
    tibble(
      origem = origem_nome,
      datas_esperadas = length(datas_esperadas),
      datas_presentes = length(datas_presentes),
      completo = length(datas_esperadas) == length(datas_presentes)
    )
  }
)

print(validacao_temporal)

if (all(validacao_temporal$completo)) {
  cat("\n✅ Todos os conjuntos de teste têm cobertura temporal completa\n")
} else {
  warning("⚠️  Alguns conjuntos de teste têm datas faltantes!")
}

# Validação 3: Materiais excluídos
cat("\n🔍 Validação 3: Materiais excluídos por origem\n")

materiais_excluidos_consolidado <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$materiais_excluidos
)

if (nrow(materiais_excluidos_consolidado) > 0) {
  resumo_exclusoes <- materiais_excluidos_consolidado %>%
    count(origem_id, motivo) %>%
    arrange(origem_id)
  
  print(resumo_exclusoes)
  
  cat(sprintf("\n⚠️  Total de exclusões: %s\n", 
              format(nrow(materiais_excluidos_consolidado), big.mark = ",")))
} else {
  cat("\n✅ Nenhum material excluído em nenhuma origem\n")
}

validacoes_globais <- list(
  materiais_por_teste = n_materiais_teste,
  cobertura_temporal = validacao_temporal,
  materiais_excluidos = materiais_excluidos_consolidado
)

log_message("Validações globais concluídas", "INFO")

# 6. SALVAMENTO DOS RESULTADOS ####

log_message("Salvando resultados", "INFO")

cat("\n", strrep("=", 70), "\n", sep = "")
cat("SALVAMENTO DOS RESULTADOS\n")
cat("\n", strrep("=", 70), "\n", sep = "")

# Salvar estrutura completa de splits
splits_list %>% saveRDS(here(config$paths$data$processed, "train_test_splits.rds"))
cat(sprintf("\n✅ Splits salvos: %s\n", basename(here(config$paths$data$processed, "train_test_splits.rds"))))
log_message(
  sprintf(
    "Splits salvos em: %s",
    here(config$paths$data$processed, "train_test_splits.rds")
    ),
  "INFO"
  )

# Salvar metadados consolidados
metadados_consolidados <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$metadata %>%
    mutate(
      origem = .x,
      n_materiais_train = n_distinct(splits_list[[.x]]$train$cd_material),
      n_materiais_test = n_distinct(splits_list[[.x]]$test$cd_material),
      n_registros_train = nrow(splits_list[[.x]]$train),
      n_registros_test = nrow(splits_list[[.x]]$test)
    )
)

metadados_consolidados %>% write_xlsx(
  here(config$paths$output$reports, "train_test_metadata.xlsx")
)
cat(sprintf("✅ Metadados salvos: train_test_metadata.xlsx\n"))

# Salvar classificações SBC consolidadas
todas_classificacoes_completas <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$sbc_classification
)

todas_classificacoes_completas %>% write_xlsx(
  here(config$paths$output$reports, "sbc_classifications_all_origins.xlsx")
)
cat(sprintf("✅ Classificações SBC salvas: sbc_classifications_all_origins.xlsx\n"))

# Salvar estatísticas de presença
stats_presenca_consolidadas <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$stats_presenca
)

stats_presenca_consolidadas %>% write_xlsx(
  here(config$paths$output$reports, "estatisticas_presenca_materiais.xlsx")
)
cat(sprintf("✅ Estatísticas de presença salvas: estatisticas_presenca_materiais.xlsx\n"))

# Salvar análise de transições
write_xlsx(
  list(
    presenca_origens = analise_transicoes$materiais_multiplas_origens,
    transicoes = analise_transicoes$transicoes_detectadas,
    matriz_transicoes = analise_transicoes$matriz_transicoes,
    todas_classificacoes = analise_transicoes$todas_classificacoes
  ),
  here(config$paths$output$reports, "analise_transicoes_sbc.xlsx")
)
cat(sprintf("✅ Análise de transições salva: analise_transicoes_sbc.xlsx\n"))

# Salvar validações globais
write_xlsx(
  list(
    cobertura_temporal = validacoes_globais$cobertura_temporal,
    materiais_excluidos = validacoes_globais$materiais_excluidos
  ),
  here(config$paths$output$reports, "validacoes_globais.xlsx")
)
cat(sprintf("✅ Validações globais salvas: validacoes_globais.xlsx\n"))

# Salvar workspace
save.image(here(config$paths$output$models, "02_train_test_split.RData"))
cat(sprintf("✅ Workspace salvo: 02_train_test_split.RData\n"))

# 7. RELATÓRIO FINAL ####

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎉 PROCESSAMENTO CONCLUÍDO! 🎉\n")
cat("\n", strrep("=", 70), "\n", sep = "")

cat("\n📊 RESUMO GERAL:\n\n")

cat(sprintf("🎯 Origens criadas: %d\n", n_origins))
cat(sprintf("📅 Horizonte de teste: %d meses\n", test_months))
cat(sprintf("📚 Período total analisado: %s até %s\n", 
            primeira_data, ultima_data_completa))

cat("\n📈 Estatísticas por origem:\n")
for (i in 1:n_origins) {
  origem <- splits_list[[paste0("origem_", i)]]
  cat(sprintf("\n   Origem %d:\n", i))
  cat(sprintf("      - Treino: %s até %s (%d meses)\n",
              format(origem$metadata$train_start, "%Y-%m"),
              format(origem$metadata$train_end, "%Y-%m"),
              origem$metadata$n_train_months))
  cat(sprintf("      - Teste: %s até %s (%d meses)\n",
              format(origem$metadata$test_start, "%Y-%m"),
              format(origem$metadata$test_end, "%Y-%m"),
              origem$metadata$n_test_months))
  cat(sprintf("      - Materiais no treino: %s\n",
              format(n_distinct(origem$train$cd_material), big.mark = ",")))
  cat(sprintf("      - Materiais no teste: %s\n",
              format(n_distinct(origem$test$cd_material), big.mark = ",")))
}

cat("\n🏷️  Distribuição SBC (média entre origens):\n")
todas_classificacoes_completas %>%
  count(categoria_sbc) %>%
  mutate(
    n_total = sum(n),
    percentual = n / n_total * 100
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    percentual_fmt = sprintf("%.1f%%", percentual)
  ) %>%
  select(categoria_sbc, n, percentual_fmt) %>%
  print()

if (nrow(transicoes) > 0) {
  cat(sprintf("\n🔄 Transições SBC detectadas: %s materiais\n",
              format(n_distinct(transicoes$cd_material), big.mark = ",")))
}

if (nrow(materiais_excluidos_consolidado) > 0) {
  cat(sprintf("\n⚠️  Materiais excluídos (dados insuficientes): %s\n",
              format(nrow(materiais_excluidos_consolidado), big.mark = ",")))
}

cat("\n📁 Arquivos gerados:\n")
cat("   - data/processed/train_test_splits.rds\n")
cat("   - output/reports/train_test_metadata.xlsx\n")
cat("   - output/reports/sbc_classifications_all_origins.xlsx\n")
cat("   - output/reports/estatisticas_presenca_materiais.xlsx\n")
cat("   - output/reports/analise_transicoes_sbc.xlsx\n")
cat("   - output/reports/validacoes_globais.xlsx\n")
cat("   - output/models/02_train_test_split.RData\n")

log_message("========================================", "INFO")
log_message("DIVISÃO TEMPORAL CONCLUÍDA COM SUCESSO", "INFO")
log_message("========================================", "INFO")

# Limpar ambiente (manter apenas objetos essenciais)
rm(list = setdiff(ls(), c(
  "config",
  "splits_list",
  "analise_transicoes",
  "validacoes_globais"
)))
