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
  
  # Metodo 1:
  # Trabalhar de trás para frente
  # Origem 4 (mais recente): teste termina em ultima_data
  # Origem 3: teste termina 12 meses antes
  # etc.
  
  # origens <- tibble::tibble(
  #   origem_id = 1:n_origins
  # ) %>%
  #   dplyr::mutate(
  #     # Calcular fim do teste
  #     test_end = ultima_data - months((n_origins - origem_id) * test_months),
  #     # Início do teste é test_months antes do fim
  #     test_start = test_end - months(test_months - 1),
  #     # Fim do treino é 1 mês antes do início do teste
  #     train_end = test_start - months(1),
  #     # Início do treino é a primeira data disponível (janela expansiva)
  #     train_start = primeira_data_disponivel,
  #     # Calcular duração do treino
  #     n_train_months = lubridate::interval(train_start,train_end) %/% months(1) + 1,
  #     n_test_months = test_months
  #   )
  
  # Metodo 2:
  # Trabalhar com origem fixa.
  # Origem 4 (mais recente): teste termina em ultima_data
  # Origem 3: teste termina na mesma data que origem 3, mas o treino deve ser menor em 12 meses.
  # etc.
  
  origens <- tibble::tibble(
    origem_id = 1:n_origins
  ) %>%
    dplyr::mutate(
      # Calcular fim do teste
      test_end = ultima_data,
      # Início do teste é test_months antes do fim
      test_start = test_end - months(test_months - 1),
      # Fim do treino é 1 mês antes do início do teste
      train_end = test_start - months(1),
      # Início do treino é a primeira data disponível (janela expansiva)
      train_start = primeira_data_disponivel + months((n_origins - origem_id) * test_months),
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

# CRIAÇÃO DE SPLITS ####

#' Criar split de treino/teste para uma origem específica
#'
#' Cria estrutura completa de dados para uma origem temporal, incluindo
#' separação treino/teste, validações, classificação SBC e estatísticas.
#'
#' @param data tsibble completo com séries temporais (tsibble)
#' @param origem_info tibble com uma linha contendo informações temporais da origem
#'        (colunas: train_start, train_end, test_start, test_end)
#' @param origem_id Identificador numérico da origem (integer)
#'
#' @return list com elementos:
#'   \item{metadata}{tibble com informações temporais da origem}
#'   \item{train}{tsibble com dados de treino filtrados}
#'   \item{test}{tsibble com dados de teste}
#'   \item{sbc_classification}{tibble com classificação SBC dos materiais}
#'   \item{stats_presenca}{tibble com estatísticas de presença dos materiais}
#'   \item{materiais_excluidos}{tibble com materiais excluídos e motivo}
#'   \item{stats_descritivas}{list com estatísticas por categoria e globais}
#'
#' @details
#' A função executa sequencialmente:
#' 1. Separação temporal de treino e teste
#' 2. Validação de integridade temporal
#' 3. Cálculo de estatísticas de presença
#' 4. Filtragem de materiais com dados insuficientes
#' 5. Classificação SBC (apenas com treino!)
#' 6. Cálculo de estatísticas descritivas
#'
#' @seealso \code{\link{validar_integridade_temporal}},
#'          \code{\link{classificar_sbc_origem}}
#'
#' @export
criar_split_origem <- function(data, origem_info, origem_id) {
  
  cat(sprintf("\n🔄 Processando Origem %d...\n", origem_id))
  
  # Extrair informações da origem
  train_start <- origem_info$train_start
  train_end <- origem_info$train_end
  test_start <- origem_info$test_start
  test_end <- origem_info$test_end
  
  # Separar treino e teste
  data_train <- data %>%
    dplyr::filter(
      data_competencia >= train_start,
      data_competencia <= train_end
    )
  
  data_test <- data %>%
    dplyr::filter(
      data_competencia >= test_start,
      data_competencia <= test_end
    )
  
  cat(sprintf("   ✓ Treino: %s registros (%s materiais)\n", 
              format(nrow(data_train), big.mark = ","),
              format(dplyr::n_distinct(data_train$cd_material), big.mark = ",")))
  cat(sprintf("   ✓ Teste: %s registros (%s materiais)\n", 
              format(nrow(data_test), big.mark = ","),
              format(dplyr::n_distinct(data_test$cd_material), big.mark = ",")))
  
  # Validar integridade temporal
  validar_integridade_temporal(data_train, data_test, origem_id)
  
  # Calcular estatísticas de presença de materiais
  stats_presenca <- calcular_estatisticas_presenca(data_train, origem_id)
  
  # Filtrar materiais com dados insuficientes no treino
  # NOTA: config deve estar disponível no ambiente global
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
      dplyr::mutate(origem_id = origem_id),
    train = data_train_filtrado,
    test = data_test,
    sbc_classification = sbc_classification,
    stats_presenca = stats_presenca,
    materiais_excluidos = materiais_excluidos,
    stats_descritivas = stats_descritivas
  )
}

# VALIDAÇÕES ####

#' Validar integridade temporal entre treino e teste
#'
#' Executa validações para garantir ausência de data leakage e
#' consistência temporal entre conjuntos de treino e teste.
#'
#' @param train tsibble com dados de treino
#' @param test tsibble com dados de teste
#' @param origem_id Identificador da origem (para mensagens de erro)
#'
#' @return Retorna TRUE invisível se validações passarem, stop() caso contrário
#'
#' @details
#' Validações executadas:
#' \itemize{
#'   \item{Disjunção temporal: max(train) < min(test)}
#'   \item{Continuidade: gap de exatamente 1 mês entre treino e teste}
#'   \item{Completude: ausência de datas faltantes no período de teste}
#' }
#'
#' @examples
#' \dontrun{
#' validar_integridade_temporal(train_data, test_data, origem_id = 1)
#' }
#'
#' @export
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
  # gap_months <- lubridate::interval(max_train, min_test) %/% lubridate::months(1)
  gap_months <- lubridate::time_length(
    lubridate::interval(max_train, min_test), 
    unit = "months"
  )
  
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


# ESTATÍSTICAS DE PRESENÇA ####

#' Calcular estatísticas de presença de materiais no conjunto de treino
#'
#' Computa estatísticas descritivas sobre presença e padrão de demanda
#' de cada material no período de treino.
#'
#' @param train tsibble com dados de treino
#' @param origem_id Identificador da origem
#'
#' @return tibble com colunas:
#'   \item{cd_material}{Código do material}
#'   \item{n_periodos_total}{Total de períodos no treino}
#'   \item{n_periodos_com_demanda}{Períodos com demanda > 0}
#'   \item{n_periodos_zero}{Períodos sem demanda}
#'   \item{proporcao_zeros}{Proporção de períodos zero}
#'   \item{demanda_total}{Demanda acumulada}
#'   \item{demanda_media}{Demanda média (excluindo zeros)}
#'   \item{demanda_max}{Demanda máxima observada}
#'   \item{origem_id}{Identificador da origem}
#'
#' @details
#' Esta função é crítica para rastrear quais materiais estão presentes
#' em cada origem e suas características de demanda, permitindo análise
#' de evolução temporal e identificação de materiais descontinuados.
#'
#' @export
calcular_estatisticas_presenca <- function(train, origem_id) {
  
  cat(sprintf("   📊 Calculando estatísticas de presença (Origem %d)...\n", origem_id))
  
  stats <- train %>%
    tsibble::as_tibble() %>%
    dplyr::group_by(cd_material) %>%
    dplyr::summarise(
      n_periodos_total = dplyr::n(),
      n_periodos_com_demanda = sum(qt_total > 0),
      n_periodos_zero = sum(qt_total == 0),
      proporcao_zeros = mean(qt_total == 0),
      demanda_total = sum(qt_total),
      demanda_media = mean(qt_total[qt_total > 0]),
      demanda_max = max(qt_total),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(origem_id = origem_id)
  
  cat(sprintf("      ✓ Estatísticas calculadas para %s materiais\n", 
              format(nrow(stats), big.mark = ",")))
  
  return(stats)
}


# FILTRAGEM DE MATERIAIS ####

#' Filtrar materiais com dados insuficientes no treino
#'
#' Remove materiais com número de ocorrências de demanda abaixo do mínimo
#' estabelecido, documentando exclusões para rastreabilidade.
#'
#' @param train tsibble com dados de treino
#' @param min_occurrences Número mínimo de ocorrências de demanda (integer, default=3)
#' @param origem_id Identificador da origem
#'
#' @return list com elementos:
#'   \item{data_filtrado}{tsibble com dados após filtragem}
#'   \item{materiais_excluidos}{tibble com materiais excluídos, número de
#'                              ocorrências e motivo da exclusão}
#'
#' @details
#' Materiais com menos de min_occurrences ocorrências de demanda não-zero
#' são considerados insuficientes para estimação estável de parâmetros de
#' previsão e são excluídos desta origem específica (não de todo o estudo).
#'
#' @examples
#' \dontrun{
#' resultado <- filtrar_materiais_insuficientes(train_data, min_occurrences = 3, origem_id = 1)
#' train_limpo <- resultado$data_filtrado
#' excluidos <- resultado$materiais_excluidos
#' }
#'
#' @export
filtrar_materiais_insuficientes <- function(train, min_occurrences = 3, origem_id) {
  
  cat(sprintf("   🔧 Filtrando materiais com < %d ocorrências (Origem %d)...\n", 
              min_occurrences, origem_id))
  
  # Identificar materiais com dados insuficientes
  materiais_insuficientes <- train %>%
    tsibble::as_tibble() %>%
    dplyr::group_by(cd_material) %>%
    dplyr::summarise(
      n_ocorrencias = sum(qt_total > 0),
      .groups = 'drop'
    ) %>%
    dplyr::filter(n_ocorrencias < min_occurrences)
  
  n_insuficientes <- nrow(materiais_insuficientes)
  
  if (n_insuficientes > 0) {
    cat(sprintf("      ⚠️  %s materiais serão excluídos da SBC\n", 
                format(n_insuficientes, big.mark = ",")))
    
    # Filtrar dados
    # train_filtrado <- train %>%
    #   dplyr::filter(!cd_material %in% materiais_insuficientes$cd_material)
    
  } else {
    cat("      ✅ Todos os materiais atendem critério mínimo\n")
    train_filtrado <- train
  }
  
  list(
    data_filtrado = train,  # Retorna dados completos
    # data_filtrado = train_filtrado, # retorna dados filtrados
    materiais_excluidos = materiais_insuficientes %>%
      dplyr::mutate(
        origem_id = origem_id,
        motivo = sprintf("< %d ocorrências", min_occurrences)
      )
  )
}


# CLASSIFICAÇÃO SBC ####

#' Classificar padrões de demanda segundo taxonomia SBC
#'
#' Implementa classificação Syntetos-Boylan-Croston (SBC) baseada em
#' ADI (Average inter-Demand Interval) e CV² (Squared Coefficient of Variation).
#' Referência: Syntetos, Boylan e Croston (2005).
#'
#' @param train tsibble com dados de treino (já filtrado)
#' @param adi_threshold Limiar de ADI para classificação (default=1.32)
#' @param cv2_threshold Limiar de CV² para classificação (default=0.49)
#' @param origem_id Identificador da origem
#'
#' @return tibble com colunas:
#'   \item{cd_material}{Código do material}
#'   \item{n_periodos}{Total de períodos no treino}
#'   \item{n_demandas}{Número de períodos com demanda > 0}
#'   \item{adi}{Average inter-Demand Interval}
#'   \item{demanda_media}{Média das demandas positivas}
#'   \item{demanda_sd}{Desvio-padrão das demandas positivas}
#'   \item{cv2}{Coeficiente de variação quadrado}
#'   \item{categoria_sbc}{Categoria: Smooth, Erratic, Intermittent, Lumpy,
#'                        Dados_Insuficientes ou Indefinido}
#'   \item{origem_id}{Identificador da origem}
#'
#' @details
#' Categorias SBC:
#' \itemize{
#'   \item{Smooth: ADI ≤ 1.32 e CV² < 0.49}
#'   \item{Erratic: ADI ≤ 1.32 e CV² ≥ 0.49}
#'   \item{Intermittent: ADI > 1.32 e CV² < 0.49}
#'   \item{Lumpy: ADI > 1.32 e CV² ≥ 0.49}
#' }
#'
#' CRÍTICO: Esta classificação deve ser calculada APENAS com dados de treino
#' para evitar data leakage.
#'
#' @references
#' Syntetos, A. A., Boylan, J. E., & Croston, J. D. (2005).
#' On the categorization of demand patterns.
#' Journal of the Operational Research Society, 56(5), 495-503.
#'
#' @export
classificar_sbc_origem <- function(train, adi_threshold = 1.32, cv2_threshold = 0.49, origem_id) {
  
  cat(sprintf("   🏷️  Classificando padrões SBC (Origem %d)...\n", origem_id))
  
  # Calcular ADI e CV² para cada material (apenas com dados de treino!)
  sbc <- train %>%
    tsibble::as_tibble() %>%
    dplyr::group_by(cd_material) %>%
    dplyr::summarise(
      # Número total de períodos
      n_periodos = dplyr::n(),
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
    dplyr::mutate(
      categoria_sbc = dplyr::case_when(
        n_demandas < config$parameters$data_cleaning$min_occurrences,
        adi <= adi_threshold & cv2 < cv2_threshold ~ "Smooth",
        adi <= adi_threshold & cv2 >= cv2_threshold ~ "Erratic",
        adi > adi_threshold & cv2 < cv2_threshold ~ "Intermittent",
        adi > adi_threshold & cv2 >= cv2_threshold ~ "Lumpy",
        TRUE ~ "Indefinido"
      ),
      origem_id = origem_id
    ) %>%
    # Tratar casos especiais (NaN, Inf)
    dplyr::mutate(
      cv2 = ifelse(is.nan(cv2) | is.infinite(cv2), NA_real_, cv2),
      categoria_sbc = ifelse(
        is.na(cv2) & n_demandas >= config$parameters$data_cleaning$min_occurrences,
        "Indefinido",
        categoria_sbc
        )
    )
  
  # Resumo da classificação
  resumo <- sbc %>%
    dplyr::count(categoria_sbc, name = "n_materiais") %>%
    dplyr::mutate(percentual = n_materiais / sum(n_materiais) * 100)
  
  cat("\n      📈 Distribuição de categorias SBC:\n")
  resumo %>%
    dplyr::mutate(
      percentual_fmt = sprintf("%.1f%%", percentual)
    ) %>%
    dplyr::select(categoria_sbc, n_materiais, percentual_fmt) %>%
    print()
  
  return(sbc)
}


# ESTATÍSTICAS DESCRITIVAS ####

#' Calcular estatísticas descritivas por categoria SBC e globais
#'
#' Computa estatísticas agregadas para análise exploratória e validação
#' de características de demanda por categoria SBC.
#'
#' @param train tsibble com dados de treino filtrados
#' @param sbc_classification tibble com classificação SBC dos materiais
#' @param origem_id Identificador da origem
#'
#' @return list com elementos:
#'   \item{por_categoria}{tibble com estatísticas agregadas por categoria SBC}
#'   \item{global}{tibble com estatísticas globais da origem}
#'
#' @details
#' Estatísticas por categoria incluem:
#' \itemize{
#'   \item{n_materiais: Número de materiais na categoria}
#'   \item{proporcao_zeros_mediana: Mediana da proporção de zeros}
#'   \item{demanda_media_mediana: Mediana da demanda média}
#'   \item{demanda_total: Demanda acumulada da categoria}
#' }
#'
#' Estatísticas globais incluem contagens totais e proporção geral de zeros.
#'
#' @export
calcular_estatisticas_descritivas <- function(train, sbc_classification, origem_id) {
  
  cat(sprintf("   📊 Calculando estatísticas descritivas (Origem %d)...\n", origem_id))
  
  # Estatísticas agregadas por categoria SBC
  stats_por_categoria <- train %>%
    tsibble::as_tibble() %>%
    dplyr::left_join(
      sbc_classification %>% dplyr::select(cd_material, categoria_sbc),
      by = "cd_material"
    ) %>%
    dplyr::group_by(categoria_sbc) %>%
    dplyr::summarise(
      n_materiais = dplyr::n_distinct(cd_material),
      proporcao_zeros_mediana = median(qt_total == 0),
      demanda_media_mediana = median(qt_total[qt_total > 0]),
      demanda_total = sum(qt_total),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(origem_id = origem_id)
  
  # Estatísticas globais
  stats_globais <- tibble::tibble(
    origem_id = origem_id,
    n_materiais_total = dplyr::n_distinct(train$cd_material),
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