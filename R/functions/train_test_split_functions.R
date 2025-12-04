# FUNÇÕES AUXILIARES PARA VALIDAÇÃO TRAIN/TEST SPLIT ####
#
# Arquivo: R/functions/train_test_split_functions.R
# Descrição: Funções para criação e validação de splits temporais com rolling origin,
#            classificação SBC e estatísticas descritivas por origem
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Data: 2025-12-03
#
# Dependências: tidyverse, tsibble, lubridate

# DEFINIÇÃO DE ORIGENS TEMPORAIS ####

#' Definir origens temporais para validação rolling origin
#'
#' Cria estrutura de datas de corte para validação com janela expansiva,
#' trabalhando de trás para frente a partir da última data disponível.
#' Implementa estratégia descrita na metodologia (Seção 3.4.1).
#'
#' @param ultima_data Data final do período de análise (Date)
#' @param n_origins Número de origens temporais a criar (integer)
#' @param test_months Horizonte de teste em meses (integer, default=12)
#' @param train_min_months Período mínimo de treino em meses (integer, default=36)
#' @param primeira_data_disponivel Primeira data disponível nos dados (Date)
#'
#' @return tibble com colunas: origem_id, train_start, train_end, test_start,
#'         test_end, n_train_months, n_test_months
#'
#' @details
#' A função implementa estratégia rolling origin com janela expansiva:
#' - Origem mais recente (n_origins) tem teste terminando em ultima_data
#' - Cada origem anterior recua test_months meses
#' - Treino sempre inicia na primeira_data_disponivel (janela expansiva)
#' - Valida automaticamente se train_min_months é atendido
#'
#' @examples
#' origens <- definir_origens_temporais(
#'   ultima_data = as.Date("2024-10-01"),
#'   n_origins = 4,
#'   test_months = 12,
#'   train_min_months = 36,
#'   primeira_data_disponivel = as.Date("2020-01-01")
#' )
#'
#' @export
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
      # Calcular fim do teste
      test_end = ultima_data - months((n_origins - origem_id) * test_months),
      # Início do teste é test_months antes do fim
      test_start = test_end - months(test_months - 1),
      # Fim do treino é 1 mês antes do início do teste
      train_end = test_start - months(1),
      # Início do treino é a primeira data disponível (janela expansiva)
      train_start = primeira_data_disponivel,
      # Calcular duração do treino
      n_train_months = lubridate::interval(train_start,train_end) %/% months(1) + 1,
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

#'
#'
#'
#'
#'

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

#'
#'
#'
#'
#'

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

#'
#'
#'
#'
#'

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

#'
#'
#'
#'
#'

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


#'
#'
#'
#'
#'
#'
#'
#'

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

#'
#'
#'
#'
#'
#'


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












