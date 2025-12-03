# 02 - DIVISÃO TEMPORAL DOS DADOS - TRAIN/TEST SPLIT ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Implementa estratégia de validação rolling origin com janela
#            expansiva, classificação SBC por origem e validações de integridade
# Data: 2025-12-03
# Versão: 1.0.0

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(tsibble)
library(lubridate)
library(writexl)

source(here("R/utils/load_config.R"))

config <- load_config()
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
ultima_data_disponivel <- max(ts_completa$data_competencia)

# Subtrair 1 mês para garantir mês completo (conforme metodologia)
ultima_data_completa <- ultima_data_disponivel - months(1)

cat("\n📅 Definição de período:\n")
cat(sprintf("   - Última data disponível: %s\n", ultima_data_disponivel))
cat(sprintf("   - Última data completa (análise): %s\n", ultima_data_completa))
cat(sprintf("   - Justificativa: Excluir último mês potencialmente incompleto\n"))

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

# Função para definir origens temporais ####
definir_origens_temporais <- function(ultima_data, 
                                      n_origins,
                                      test_months,
                                      train_min_months,
                                      primeira_data_disponivel) {
  
  cat("\n🔍 Calculando datas de corte para cada origem...\n")
  
  # Trabalhar de trás para frente
  # Origem 4 (mais recente): teste termina em ultima_data
  # Origem 3: teste termina 12 meses antes
  # etc.
  
  origens <- tibble(
    origem_id = 1:n_origins
  ) %>%
    mutate(
      # Calcular fim do teste (trabalhar de trás pra frente)
      test_end = ultima_data - months((n_origins - origem_id) * test_months),
      # Início do teste é test_months antes do fim
      test_start = test_end - months(test_months - 1),
      # Fim do treino é 1 mês antes do início do teste
      train_end = test_start - months(1),
      # Início do treino é a primeira data disponível (janela expansiva)
      train_start = primeira_data_disponivel,
      # Calcular duração do treino
      n_train_months = interval(train_start, train_end) %/% months(1) + 1,
      n_test_months = test_months
    )
  
  # Validar se treino mínimo é atendido
  origens_invalidas <- origens %>%
    filter(n_train_months < train_min_months)
  
  if (nrow(origens_invalidas) > 0) {
    cat("\n❌ ERRO: Algumas origens não atendem requisito de treino mínimo!\n")
    print(origens_invalidas)
    stop(sprintf("Treino mínimo de %d meses não atendido para %d origem(ns)", 
                 train_min_months, nrow(origens_invalidas)))
  }
  
  return(origens)
}

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

# Função para criar split de uma origem ####
criar_split_origem <- function(data, origem_info, origem_id) {
  
  cat(sprintf("\n🔄 Processando Origem %d...\n", origem_id))
  
  # Extrair informações da origem
  train_start <- origem_info$train_start
  train_end <- origem_info$train_end
  test_start <- origem_info$test_start
  test_end <- origem_info$test_end
  
  # Separar treino e teste
  data_train <- data %>%
    filter(
      data_competencia >= train_start,
      data_competencia <= train_end
    )
  
  data_test <- data %>%
    filter(
      data_competencia >= test_start,
      data_competencia <= test_end
    )
  
  cat(sprintf("   ✓ Treino: %s registros (%s materiais)\n", 
              format(nrow(data_train), big.mark = ","),
              format(n_distinct(data_train$cd_material), big.mark = ",")))
  cat(sprintf("   ✓ Teste: %s registros (%s materiais)\n", 
              format(nrow(data_test), big.mark = ","),
              format(n_distinct(data_test$cd_material), big.mark = ",")))
  
  # Validar integridade temporal
  validar_integridade_temporal(data_train, data_test, origem_id)
  
  # Calcular estatísticas de presença de materiais
  stats_presenca <- calcular_estatisticas_presenca(data_train, origem_id)
  
  # Filtrar materiais com dados insuficientes no treino
  resultado_filtragem <- filtrar_materiais_insuficientes(
    data_train, 
    min_occurrences = config$parameters$data_cleaning$min_occurrences,
    origem_id
  )
  
  data_train_filtrado <- resultado_filtragem$data_filtrado
  materiais_excluidos <- resultado_filtragem$materiais_excluidos
  
  # Classificar SBC (apenas com dados de treino filtrado!)
  sbc_classification <- classificar_sbc_origem(
    data_train_filtrado,
    adi_threshold = config$parameters$sbc$adi_threshold,
    cv2_threshold = config$parameters$sbc$cv2_threshold,
    origem_id
  )
  
  # Calcular estatísticas descritivas
  stats_descritivas <- calcular_estatisticas_descritivas(
    data_train_filtrado,
    sbc_classification,
    origem_id
  )
  
  # Retornar estrutura hierárquica
  list(
    metadata = origem_info %>%
      mutate(origem_id = origem_id),
    train = data_train_filtrado,
    test = data_test,
    sbc_classification = sbc_classification,
    stats_presenca = stats_presenca,
    materiais_excluidos = materiais_excluidos,
    stats_descritivas = stats_descritivas
  )
}

# Função para validar integridade temporal ####
validar_integridade_temporal <- function(train, test, origem_id) {
  
  cat(sprintf("   🔍 Validando integridade temporal (Origem %d)...\n", origem_id))
  
  # Verificar se conjuntos são disjuntos temporalmente
  max_train <- max(train$data_competencia)
  min_test <- min(test$data_competencia)
  
  if (max_train >= min_test) {
    stop(sprintf("❌ Origem %d: Sobreposição temporal detectada! train_max=%s >= test_min=%s",
                 origem_id, max_train, min_test))
  }
  
  # Verificar continuidade (deve haver exatamente 1 mês de diferença)
  gap_months <- interval(max_train, min_test) %/% months(1)
  
  if (gap_months != 1) {
    warning(sprintf("⚠️  Origem %d: Gap temporal inesperado de %d meses entre treino e teste",
                    origem_id, gap_months))
  }
  
  # Verificar se não há datas faltantes no teste
  datas_teste_esperadas <- seq(min_test, max(test$data_competencia), by = "month")
  datas_teste_presentes <- unique(test$data_competencia)
  
  if (length(datas_teste_esperadas) != length(datas_teste_presentes)) {
    stop(sprintf("❌ Origem %d: Datas faltantes no período de teste!", origem_id))
  }
  
  cat("      ✅ Validação temporal aprovada\n")
  
  invisible(TRUE)
}

# Função para calcular estatísticas de presença ####
calcular_estatisticas_presenca <- function(train, origem_id) {
  
  cat(sprintf("   📊 Calculando estatísticas de presença (Origem %d)...\n", origem_id))
  
  stats <- train %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      n_periodos_total = n(),
      n_periodos_com_demanda = sum(qt_total > 0),
      n_periodos_zero = sum(qt_total == 0),
      proporcao_zeros = mean(qt_total == 0),
      demanda_total = sum(qt_total),
      demanda_media = mean(qt_total[qt_total > 0]),
      demanda_max = max(qt_total),
      .groups = 'drop'
    ) %>%
    mutate(origem_id = origem_id)
  
  cat(sprintf("      ✓ Estatísticas calculadas para %s materiais\n", 
              format(nrow(stats), big.mark = ",")))
  
  return(stats)
}

# Função para filtrar materiais com dados insuficientes ####
filtrar_materiais_insuficientes <- function(train, min_occurrences, origem_id) {
  
  cat(sprintf("   🔧 Filtrando materiais com < %d ocorrências (Origem %d)...\n", 
              min_occurrences, origem_id))
  
  # Identificar materiais com dados insuficientes
  materiais_insuficientes <- train %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      n_ocorrencias = sum(qt_total > 0),
      .groups = 'drop'
    ) %>%
    filter(n_ocorrencias < min_occurrences)
  
  n_excluidos <- nrow(materiais_insuficientes)
  
  if (n_excluidos > 0) {
    cat(sprintf("      ⚠️  %s materiais serão excluídos desta origem\n", 
                format(n_excluidos, big.mark = ",")))
    
    # Filtrar dados
    train_filtrado <- train %>%
      filter(!cd_material %in% materiais_insuficientes$cd_material)
    
  } else {
    cat("      ✅ Todos os materiais atendem critério mínimo\n")
    train_filtrado <- train
  }
  
  list(
    data_filtrado = train_filtrado,
    materiais_excluidos = materiais_insuficientes %>%
      mutate(origem_id = origem_id,
             motivo = sprintf("< %d ocorrências", min_occurrences))
  )
}

# Função para classificar SBC ####
classificar_sbc_origem <- function(train, adi_threshold, cv2_threshold, origem_id) {
  
  cat(sprintf("   🏷️  Classificando padrões SBC (Origem %d)...\n", origem_id))
  
  # Calcular ADI e CV² para cada material (apenas com dados de treino!)
  sbc <- train %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      # Número total de períodos
      n_periodos = n(),
      # Número de períodos com demanda positiva
      n_demandas = sum(qt_total > 0),
      # ADI: Average inter-Demand Interval
      adi = n_periodos / n_demandas,
      # Estatísticas das quantidades positivas
      demanda_media = mean(qt_total[qt_total > 0]),
      demanda_sd = sd(qt_total[qt_total > 0]),
      # CV²: Squared Coefficient of Variation
      cv2 = (demanda_sd / demanda_media)^2,
      .groups = 'drop'
    ) %>%
    # Aplicar classificação SBC
    mutate(
      categoria_sbc = case_when(
        adi <= adi_threshold & cv2 < cv2_threshold ~ "Smooth",
        adi <= adi_threshold & cv2 >= cv2_threshold ~ "Erratic",
        adi > adi_threshold & cv2 < cv2_threshold ~ "Intermittent",
        adi > adi_threshold & cv2 >= cv2_threshold ~ "Lumpy",
        TRUE ~ "Indefinido"
      ),
      origem_id = origem_id
    ) %>%
    # Tratar casos especiais (NaN, Inf)
    mutate(
      cv2 = ifelse(is.nan(cv2) | is.infinite(cv2), NA_real_, cv2),
      categoria_sbc = ifelse(is.na(cv2), "Dados_Insuficientes", categoria_sbc)
    )
  
  # Resumo da classificação
  resumo <- sbc %>%
    count(categoria_sbc, name = "n_materiais") %>%
    mutate(percentual = n_materiais / sum(n_materiais) * 100)
  
  cat("\n      📈 Distribuição de categorias SBC:\n")
  resumo %>%
    mutate(
      percentual_fmt = sprintf("%.1f%%", percentual)
    ) %>%
    select(categoria_sbc, n_materiais, percentual_fmt) %>%
    print()
  
  return(sbc)
}

# Função para calcular estatísticas descritivas ####
calcular_estatisticas_descritivas <- function(train, sbc_classification, origem_id) {
  
  cat(sprintf("   📊 Calculando estatísticas descritivas (Origem %d)...\n", origem_id))
  
  # Estatísticas agregadas por categoria SBC
  stats_por_categoria <- train %>%
    as_tibble() %>%
    left_join(
      sbc_classification %>% select(cd_material, categoria_sbc),
      by = "cd_material"
    ) %>%
    group_by(categoria_sbc) %>%
    summarise(
      n_materiais = n_distinct(cd_material),
      proporcao_zeros_mediana = median(qt_total == 0),
      demanda_media_mediana = median(qt_total[qt_total > 0]),
      demanda_total = sum(qt_total),
      .groups = 'drop'
    ) %>%
    mutate(origem_id = origem_id)
  
  # Estatísticas globais
  stats_globais <- tibble(
    origem_id = origem_id,
    n_materiais_total = n_distinct(train$cd_material),
    n_registros_total = nrow(train),
    proporcao_zeros_geral = mean(train$qt_total == 0),
    demanda_total_geral = sum(train$qt_total)
  )
  
  cat("      ✓ Estatísticas descritivas calculadas\n")
  
  list(
    por_categoria = stats_por_categoria,
    global = stats_globais
  )
}

# Criar splits para todas as origens ####
cat("\n" %+% strrep("=", 70) %+% "\n")
cat("CRIANDO SPLITS PARA TODAS AS ORIGENS\n")
cat(strrep("=", 70) %+% "\n")

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

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("ANÁLISE DE TRANSIÇÕES SBC ENTRE ORIGENS\n")
cat(strrep("=", 70) %+% "\n")

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

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("VALIDAÇÕES GLOBAIS\n")
cat(strrep("=", 70) %+% "\n")

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

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("SALVAMENTO DOS RESULTADOS\n")
cat(strrep("=", 70) %+% "\n")

# Salvar estrutura completa de splits
arquivo_splits <- here(config$paths$data$processed, "train_test_splits.rds")
saveRDS(splits_list, arquivo_splits)
cat(sprintf("\n✅ Splits salvos: %s\n", basename(arquivo_splits)))
log_message(sprintf("Splits salvos em: %s", arquivo_splits), "INFO")

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

write_xlsx(
  metadados_consolidados,
  here(config$paths$output$reports, "train_test_metadata.xlsx")
)
cat(sprintf("✅ Metadados salvos: train_test_metadata.xlsx\n"))

# Salvar classificações SBC consolidadas
todas_classificacoes_completas <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$sbc_classification
)

write_xlsx(
  todas_classificacoes_completas,
  here(config$paths$output$reports, "sbc_classifications_all_origins.xlsx")
)
cat(sprintf("✅ Classificações SBC salvas: sbc_classifications_all_origins.xlsx\n"))

# Salvar estatísticas de presença
stats_presenca_consolidadas <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$stats_presenca
)

write_xlsx(
  stats_presenca_consolidadas,
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

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("🎉 PROCESSAMENTO CONCLUÍDO! 🎉\n")
cat(strrep("=", 70) %+% "\n")

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

cat("\n✅ Próximo passo: Execute 03_exploratory_analysis.R\n")

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
