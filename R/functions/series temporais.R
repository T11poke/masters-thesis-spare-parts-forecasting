# FUNÇÕES ADICIONAIS PARA SÉRIES TEMPORAIS ####
#
# Arquivo: R/functions/series_temporais.R
# Descrição: Funções para criar e manipular séries temporais completas
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Data: 2025-11-27

#' Cria séries temporais completas com zeros explícitos
#' 
#' Esta função implementa a transformação descrita na Seção 3.3.2 (Quarta Etapa)
#' da metodologia: "Criou-se série temporal completa preenchendo períodos sem 
#' movimentação com valor zero explícito"
#' 
#' @param data dataframe com dados agregados por material e período
#' @param coluna_material nome da coluna que identifica o material (default: "cd_material")
#' @param coluna_ano nome da coluna com o ano (default: "ano_competencia")
#' @param coluna_mes nome da coluna com o mês (default: "mes_competencia")
#' @param colunas_valores vetor com nomes das colunas de valores a preencher com zero
#' @param inicio_periodo data inicial (Date ou "YYYY-MM-DD"), NULL para usar min dos dados
#' @param fim_periodo data final (Date ou "YYYY-MM-DD"), NULL para usar max dos dados
#' @param criar_variavel_data lógico, se TRUE cria coluna 'data' com primeiro dia do mês
#' @return dataframe com séries temporais completas (retangulares)
#' 
#' @examples
#' # Criar séries completas para todo o período disponível
#' data_completo <- criar_series_completas(
#'   data = data_agregado,
#'   colunas_valores = c("qt_total", "registros_originais", "registros_com_problema")
#' )
#' 
#' # Criar séries para período específico
#' data_completo <- criar_series_completas(
#'   data = data_agregado,
#'   inicio_periodo = "2020-01-01",
#'   fim_periodo = "2024-12-01",
#'   colunas_valores = c("qt_total")
#' )
criar_series_completas <- function(data,
                                   coluna_material = "cd_material",
                                   coluna_ano = "ano_competencia",
                                   coluna_mes = "mes_competencia",
                                   colunas_valores = c("qt_total", "registros_originais", "registros_com_problema"),
                                   inicio_periodo = NULL,
                                   fim_periodo = NULL,
                                   criar_variavel_data = TRUE) {
  
  # Validações
  if(!coluna_material %in% names(data)) {
    stop(sprintf("Coluna '%s' não encontrada no dataframe", coluna_material))
  }
  if(!coluna_ano %in% names(data)) {
    stop(sprintf("Coluna '%s' não encontrada no dataframe", coluna_ano))
  }
  if(!coluna_mes %in% names(data)) {
    stop(sprintf("Coluna '%s' não encontrada no dataframe", coluna_mes))
  }
  
  # Identificar período de análise
  if(is.null(inicio_periodo)) {
    ano_min <- min(data[[coluna_ano]])
    mes_min <- min(data[[coluna_mes]][data[[coluna_ano]] == ano_min])
  } else {
    data_inicio <- as.Date(inicio_periodo)
    ano_min <- lubridate::year(data_inicio)
    mes_min <- lubridate::month(data_inicio)
  }
  
  if(is.null(fim_periodo)) {
    ano_max <- max(data[[coluna_ano]])
    mes_max <- max(data[[coluna_mes]][data[[coluna_ano]] == ano_max])
  } else {
    data_fim <- as.Date(fim_periodo)
    ano_max <- lubridate::year(data_fim)
    mes_max <- lubridate::month(data_fim)
  }
  
  # Criar sequência completa de períodos
  periodos_completos <- expand.grid(
    ano_competencia = ano_min:ano_max,
    mes_competencia = 1:12
  ) %>%
    filter(
      (ano_competencia > ano_min | mes_competencia >= mes_min) &
        (ano_competencia < ano_max | mes_competencia <= mes_max)
    ) %>%
    arrange(ano_competencia, mes_competencia)
  
  cat(sprintf("📅 Período de análise: %02d/%d até %02d/%d\n", 
              mes_min, ano_min, mes_max, ano_max))
  cat(sprintf("📊 Períodos únicos: %d\n", nrow(periodos_completos)))
  
  # Preparar lista de valores para preenchimento
  valores_fill <- setNames(
    rep(list(0), length(colunas_valores)),
    colunas_valores
  )
  
  # Expandir dados para todas as combinações material x período
  data_completo <- data %>%
    complete(
      .data[[coluna_material]],
      nesting(.data[[coluna_ano]], .data[[coluna_mes]]),
      fill = valores_fill
    )
  
  # Renomear de volta para nomes originais se necessário
  if(coluna_ano != "ano_competencia") {
    data_completo <- data_completo %>%
      rename(!!coluna_ano := ano_competencia)
  }
  if(coluna_mes != "mes_competencia") {
    data_completo <- data_completo %>%
      rename(!!coluna_mes := mes_competencia)
  }
  
  # Criar variável de data se solicitado
  if(criar_variavel_data) {
    data_completo <- data_completo %>%
      mutate(
        data = make_date(.data[[coluna_ano]], .data[[coluna_mes]], 1)
      )
  }
  
  # Ordenar por material e data
  data_completo <- data_completo %>%
    arrange(.data[[coluna_material]], .data[[coluna_ano]], .data[[coluna_mes]])
  
  # Estatísticas
  total_materiais <- n_distinct(data_completo[[coluna_material]])
  total_registros <- nrow(data_completo)
  registros_originais <- nrow(data)
  registros_adicionados <- total_registros - registros_originais
  
  # Calcular proporção de zeros para primeira coluna de valores
  primeira_coluna_valor <- colunas_valores[1]
  proporcao_zeros <- sum(data_completo[[primeira_coluna_valor]] == 0) / total_registros
  
  cat(sprintf("✅ Séries completas criadas:\n"))
  cat(sprintf("   - Materiais únicos: %s\n", format(total_materiais, big.mark = ",")))
  cat(sprintf("   - Registros originais: %s\n", format(registros_originais, big.mark = ",")))
  cat(sprintf("   - Registros adicionados: %s\n", format(registros_adicionados, big.mark = ",")))
  cat(sprintf("   - Total de registros: %s\n", format(total_registros, big.mark = ",")))
  cat(sprintf("   - Proporção de zeros (%s): %.1f%%\n", primeira_coluna_valor, proporcao_zeros * 100))
  
  return(data_completo)
}


#' Valida séries temporais completas
#' 
#' Verifica se as séries temporais estão no formato retangular correto,
#' conforme especificado na metodologia
#' 
#' @param data dataframe com séries temporais
#' @param coluna_material nome da coluna que identifica o material
#' @param coluna_data nome da coluna com datas (opcional)
#' @return lista com resultados da validação
#' 
#' @examples
#' validacao <- validar_series_temporais(data_completo)
#' if(validacao$valido) {
#'   cat("✅ Séries válidas\n")
#' } else {
#'   cat("❌ Problemas encontrados:\n")
#'   print(validacao$problemas)
#' }
validar_series_temporais <- function(data,
                                     coluna_material = "cd_material",
                                     coluna_data = "data") {
  
  problemas <- list()
  
  # Teste 1: Todos os materiais têm o mesmo número de períodos?
  materiais_contagem <- data %>%
    count(.data[[coluna_material]], name = "n_periodos")
  
  periodos_unicos <- unique(materiais_contagem$n_periodos)
  
  if(length(periodos_unicos) > 1) {
    problemas$periodos_diferentes <- TRUE
    problemas$distribuicao_periodos <- table(materiais_contagem$n_periodos)
  } else {
    problemas$periodos_diferentes <- FALSE
  }
  
  # Teste 2: Variável de data existe e está correta?
  if(coluna_data %in% names(data)) {
    if(!inherits(data[[coluna_data]], "Date")) {
      problemas$data_tipo_incorreto <- TRUE
      problemas$data_classe <- class(data[[coluna_data]])
    } else {
      problemas$data_tipo_incorreto <- FALSE
    }
    
    # Verificar se datas são sempre primeiro dia do mês
    if(inherits(data[[coluna_data]], "Date")) {
      datas_primeiro_dia <- all(lubridate::day(data[[coluna_data]]) == 1, na.rm = TRUE)
      if(!datas_primeiro_dia) {
        problemas$datas_nao_primeiro_dia <- TRUE
      }
    }
  } else {
    problemas$data_ausente <- TRUE
  }
  
  # Teste 3: Há gaps temporais?
  if(coluna_data %in% names(data) && !problemas$data_tipo_incorreto) {
    gaps_temporais <- data %>%
      group_by(.data[[coluna_material]]) %>%
      arrange(.data[[coluna_data]]) %>%
      mutate(
        gap_meses = as.numeric(difftime(
          lead(.data[[coluna_data]]), 
          .data[[coluna_data]], 
          units = "days"
        )) / 30
      ) %>%
      filter(gap_meses > 1.5, gap_meses < Inf) %>%
      ungroup()
    
    if(nrow(gaps_temporais) > 0) {
      problemas$gaps_temporais <- TRUE
      problemas$n_gaps <- nrow(gaps_temporais)
    } else {
      problemas$gaps_temporais <- FALSE
    }
  }
  
  # Determinar se é válido
  valido <- !any(unlist(problemas[c("periodos_diferentes", "data_tipo_incorreto", 
                                    "data_ausente", "gaps_temporais")]))
  
  # Estatísticas gerais
  stats <- list(
    n_materiais = n_distinct(data[[coluna_material]]),
    n_registros = nrow(data),
    n_periodos = periodos_unicos[1],
    periodo_min = if(coluna_data %in% names(data)) min(data[[coluna_data]]) else NA,
    periodo_max = if(coluna_data %in% names(data)) max(data[[coluna_data]]) else NA
  )
  
  return(list(
    valido = valido,
    problemas = problemas,
    estatisticas = stats
  ))
}


#' Cria relatório de validação de séries temporais
#' 
#' @param validacao resultado da função validar_series_temporais
#' @param nome_dataset nome do dataset para o relatório
criar_relatorio_validacao_series <- function(validacao, nome_dataset = "Dataset") {
  
  cat("\n📊 VALIDAÇÃO DE SÉRIES TEMPORAIS - ", nome_dataset, "\n")
  cat("==========================================\n")
  
  if(validacao$valido) {
    cat("✅ SÉRIES TEMPORAIS VÁLIDAS\n\n")
  } else {
    cat("❌ PROBLEMAS ENCONTRADOS\n\n")
  }
  
  # Estatísticas gerais
  cat("📈 Estatísticas:\n")
  cat(sprintf("   - Materiais únicos: %s\n", 
              format(validacao$estatisticas$n_materiais, big.mark = ",")))
  cat(sprintf("   - Total de registros: %s\n", 
              format(validacao$estatisticas$n_registros, big.mark = ",")))
  cat(sprintf("   - Períodos por material: %d\n", 
              validacao$estatisticas$n_periodos))
  
  if(!is.na(validacao$estatisticas$periodo_min)) {
    cat(sprintf("   - Período: %s até %s\n", 
                validacao$estatisticas$periodo_min,
                validacao$estatisticas$periodo_max))
  }
  
  # Problemas identificados
  if(!validacao$valido) {
    cat("\n⚠️  Problemas:\n")
    
    if(isTRUE(validacao$problemas$periodos_diferentes)) {
      cat("   ❌ Materiais com números diferentes de períodos\n")
      cat("      Distribuição:\n")
      print(validacao$problemas$distribuicao_periodos)
    }
    
    if(isTRUE(validacao$problemas$data_ausente)) {
      cat("   ❌ Variável 'data' ausente\n")
    }
    
    if(isTRUE(validacao$problemas$data_tipo_incorreto)) {
      cat(sprintf("   ❌ Variável 'data' tem tipo incorreto: %s\n",
                  paste(validacao$problemas$data_classe, collapse = ", ")))
    }
    
    if(isTRUE(validacao$problemas$datas_nao_primeiro_dia)) {
      cat("   ⚠️  Algumas datas não são primeiro dia do mês\n")
    }
    
    if(isTRUE(validacao$problemas$gaps_temporais)) {
      cat(sprintf("   ❌ Gaps temporais encontrados: %d ocorrências\n",
                  validacao$problemas$n_gaps))
    }
  }
  
  cat("\n")
}


#' Filtra séries temporais por período
#' 
#' Utilitário para extrair subconjunto de séries para um período específico
#' 
#' @param data dataframe com séries temporais completas
#' @param coluna_data nome da coluna com datas
#' @param inicio data inicial (Date ou "YYYY-MM-DD")
#' @param fim data final (Date ou "YYYY-MM-DD")
#' @return dataframe filtrado
filtrar_periodo <- function(data, 
                            coluna_data = "data",
                            inicio,
                            fim) {
  
  inicio_date <- as.Date(inicio)
  fim_date <- as.Date(fim)
  
  data_filtrado <- data %>%
    filter(
      .data[[coluna_data]] >= inicio_date,
      .data[[coluna_data]] <= fim_date
    )
  
  cat(sprintf("Período filtrado: %s até %s\n", inicio_date, fim_date))
  cat(sprintf("Registros: %s\n", format(nrow(data_filtrado), big.mark = ",")))
  
  return(data_filtrado)
}