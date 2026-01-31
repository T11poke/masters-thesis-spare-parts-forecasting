# MÓDULO: TRATAMENTO DE UNIDADES DE MEDIDA ####
#
# Arquivo: R/functions/analise_unidades_medida.R.R
# Descrição: Funções para análise unidades de medida
# Data: 2025-11-28
#
# PROBLEMA IDENTIFICADO: Materiais alternados podem ter unidades diferentes
# SOLUÇÃO: Converter todas as quantidades para unidade base do material mestre

#' Analisa distribuição de unidades de medida nos dados
#' 
#' Esta função identifica quais materiais têm múltiplas unidades de medida,
#' facilitando a criação de regras de conversão
#' 
#' @param data dataframe com dados de consumo
#' @param coluna_material nome da coluna de material
#' @param coluna_unidade nome da coluna de unidade (default: "sg_medida_port")
#' @return lista com análise completa
#' 
#' @examples
#' analise <- analisar_unidades_medida(data_consumo)
#' View(analise$materiais_multiplas_unidades)
analisar_unidades_medida <- function(data,
                                     coluna_material = "cd_material",
                                     coluna_unidade = "sg_medida_port") {
  
  cat("\n🔍 ANÁLISE DE UNIDADES DE MEDIDA\n")
  cat("==========================================\n")
  
  # Verificar se colunas existem
  if(!coluna_unidade %in% names(data)) {
    stop(sprintf("Coluna '%s' não encontrada no dataframe", coluna_unidade))
  }
  
  # 1. Estatísticas gerais de unidades
  cat("\n📊 Estatísticas Gerais:\n")
  
  unidades_unicas <- data %>%
    distinct(.data[[coluna_unidade]]) %>%
    arrange(.data[[coluna_unidade]])
  
  cat(sprintf("   - Total de unidades únicas: %d\n", nrow(unidades_unicas)))
  
  # Mostrar todas as unidades
  cat("\n📋 Unidades encontradas:\n")
  cat(paste(unidades_unicas[[coluna_unidade]], collapse = ", "))
  cat("\n")
  
  # 2. Distribuição de registros por unidade
  cat("\n📈 Distribuição de registros por unidade:\n")
  
  dist_unidades <- data %>%
    count(.data[[coluna_unidade]], name = "n_registros") %>%
    mutate(percentual = n_registros / sum(n_registros) * 100) %>%
    arrange(desc(n_registros))
  
  print(dist_unidades, n = Inf)
  
  # 3. Identificar materiais com múltiplas unidades
  cat("\n🚨 Materiais com múltiplas unidades de medida:\n")
  
  materiais_mult_unid <- data %>%
    group_by(.data[[coluna_material]]) %>%
    summarise(
      n_unidades = n_distinct(.data[[coluna_unidade]]),
      unidades = paste(unique(.data[[coluna_unidade]]), collapse = " | "),
      .groups = 'drop'
    ) %>%
    filter(n_unidades > 1) %>%
    arrange(desc(n_unidades))
  
  if(nrow(materiais_mult_unid) > 0) {
    cat(sprintf("   ⚠️  ATENÇÃO: %d materiais têm múltiplas unidades!\n", 
                nrow(materiais_mult_unid)))
    cat("\n   Primeiros 10 casos:\n")
    print(head(materiais_mult_unid, 10))
  } else {
    cat("   ✅ Todos os materiais têm unidade única\n")
  }
  
  # 4. Análise detalhada dos casos problemáticos
  casos_problematicos <- list()
  
  if(nrow(materiais_mult_unid) > 0) {
    cat("\n🔎 Análise detalhada dos casos com múltiplas unidades:\n\n")
    
    for(i in 1:min(5, nrow(materiais_mult_unid))) {
      material_id <- materiais_mult_unid[[coluna_material]][i]
      
      cat(sprintf("Caso %d - Material: %s\n", i, material_id))
      
      detalhe <- data %>%
        filter(.data[[coluna_material]] == material_id) %>%
        group_by(.data[[coluna_unidade]]) %>%
        summarise(
          n_ocorrencias = n(),
          qt_min = min(qt_consumo, na.rm = TRUE),
          qt_max = max(qt_consumo, na.rm = TRUE),
          qt_media = mean(qt_consumo, na.rm = TRUE),
          .groups = 'drop'
        )
      
      print(detalhe)
      cat("\n")
      
      casos_problematicos[[as.character(material_id)]] <- detalhe
    }
  }
  
  # 5. Verificar unidades após agregação por mestre
  cat("\n📦 Verificação pós-agregação:\n")
  cat("   Esta análise deve ser refeita após aplicar mapeamento de alternados\n")
  cat("   para verificar se materiais mestres agregam múltiplas unidades.\n")
  
  # Retornar resultados
  resultado <- list(
    unidades_unicas = unidades_unicas,
    distribuicao_unidades = dist_unidades,
    materiais_multiplas_unidades = materiais_mult_unid,
    casos_detalhados = casos_problematicos,
    n_materiais_problema = nrow(materiais_mult_unid),
    n_unidades_unicas = nrow(unidades_unicas)
  )
  
  return(invisible(resultado))
}


#' Analisa unidades após agregação por material mestre
#' 
#' Verifica se a consolidação de alternados criou conflitos de unidades
#' 
#' @param data dataframe com cd_material_final (após mapeamento)
#' @param coluna_material nome da coluna de material final
#' @param coluna_unidade nome da coluna de unidade
#' @return dataframe com materiais que têm conflito de unidades
analisar_unidades_pos_agregacao <- function(data,
                                            coluna_material = "cd_material_final",
                                            coluna_unidade = "sg_medida_port") {
  
  cat("\n🔍 ANÁLISE DE UNIDADES PÓS-AGREGAÇÃO\n")
  cat("==========================================\n")
  
  # Identificar materiais mestres com múltiplas unidades
  conflitos_unidade <- data %>%
    group_by(.data[[coluna_material]]) %>%
    summarise(
      n_unidades = n_distinct(.data[[coluna_unidade]], na.rm = TRUE),
      unidades = paste(unique(.data[[coluna_unidade]]), collapse = " | "),
      n_registros = n(),
      .groups = 'drop'
    ) %>%
    filter(n_unidades > 1) %>%
    arrange(desc(n_unidades), desc(n_registros))
  
  if(nrow(conflitos_unidade) > 0) {
    cat(sprintf("   ⚠️  CRÍTICO: %d materiais mestres têm múltiplas unidades!\n", 
                nrow(conflitos_unidade)))
    cat("   Isso indica que materiais alternados foram consolidados\n")
    cat("   mas têm unidades de medida diferentes.\n\n")
    cat("   🚨 AÇÃO NECESSÁRIA: Implementar conversão de unidades!\n\n")
    
    cat("   Casos mais críticos:\n")
    print(head(conflitos_unidade, 10))
    
    # # Análise detalhada dos 3 piores casos
    # cat("\n📋 Análise detalhada dos 3 casos mais complexos:\n\n")
    # 
    # for(i in 1:min(3, nrow(conflitos_unidade))) {
    #   material_id <- conflitos_unidade[[coluna_material]][i]
    #   
    #   cat(sprintf("\nCaso %d - Material Mestre: %s\n", i, material_id))
    #   cat(sprintf("Unidades envolvidas: %s\n", conflitos_unidade$unidades[i]))
    #   
    #   detalhe <- data %>%
    #     filter(.data[[coluna_material]] == material_id) %>%
    #     group_by(.data[[coluna_unidade]]) %>%
    #     summarise(
    #       n_registros = n(),
    #       qt_total = sum(qt_consumo, na.rm = TRUE),
    #       qt_media = mean(qt_consumo, na.rm = TRUE),
    #       periodo_min = sprintf("%02d/%d", min(mes_competencia), min(ano_competencia)),
    #       periodo_max = sprintf("%02d/%d", max(mes_competencia), max(ano_competencia)),
    #       .groups = 'drop'
    #     ) %>%
    #     mutate(proporcao = n_registros / sum(n_registros) * 100)
    #   
    #   print(detalhe)
    # }
    
  } else {
    cat("   ✅ Perfeito! Todos os materiais mestres têm unidade única.\n")
    cat("   Nenhuma conversão de unidade necessária.\n")
  }
  
  return(conflitos_unidade)
}

#' Valida conversões aplicadas
#' 
#' Verifica se as conversões foram aplicadas corretamente
#' 
#' @param data_original dataframe original
#' @param data_convertido dataframe após conversão
#' @param coluna_material nome da coluna de material
#' @param coluna_unidade nome da coluna de unidade
validar_conversoes <- function(data_original,
                               data_convertido,
                               coluna_material = "cd_material_final",
                               coluna_unidade = "sg_medida_port") {
  
  cat("\n✅ VALIDAÇÃO DE CONVERSÕES\n")
  cat("==========================================\n")
  
  # 1. Verificar se todos os materiais têm agora unidade única
  unidades_por_material <- data_convertido %>%
    group_by(.data[[coluna_material]]) %>%
    summarise(
      n_unidades = n_distinct(unidade_final),
      .groups = 'drop'
    ) %>%
    filter(n_unidades > 1)
  
  if(nrow(unidades_por_material) > 0) {
    cat(sprintf("   ❌ FALHA: %d materiais ainda têm múltiplas unidades!\n",
                nrow(unidades_por_material)))
  } else {
    cat("   ✅ SUCESSO: Todos os materiais têm unidade única!\n")
  }
  
  # 2. Verificar magnitude das conversões
  estatisticas_conversao <- data_convertido %>%
    filter(conversao_aplicada) %>%
    summarise(
      n_registros_convertidos = n(),
      razao_min = min(qt_consumo / qt_original, na.rm = TRUE),
      razao_max = max(qt_consumo / qt_original, na.rm = TRUE),
      razao_media = mean(qt_consumo / qt_original, na.rm = TRUE)
    )
  
  cat("\n   Estatísticas das conversões:\n")
  print(estatisticas_conversao)
  
  # 3. Alertar sobre conversões muito grandes ou pequenas
  conversoes_suspeitas <- data_convertido %>%
    filter(conversao_aplicada) %>%
    mutate(razao = qt_consumo / qt_original) %>%
    filter(razao < 0.001 | razao > 1000)
  
  if(nrow(conversoes_suspeitas) > 0) {
    cat(sprintf("\n   ⚠️  ATENÇÃO: %d conversões com razão suspeita (< 0.001 ou > 1000)\n",
                nrow(conversoes_suspeitas)))
    cat("   Revise estas conversões manualmente.\n")
  }
  
  return(invisible(NULL))
}
