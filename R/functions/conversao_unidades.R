# MÓDULO: TRATAMENTO DE UNIDADES DE MEDIDA ####
#
# Arquivo: R/functions/conversao_unidades.R
# Descrição: Funções para análise e conversão de unidades de medida
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
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
#' @param coluna_descricao nome da coluna com descrição da unidade (default: "ds_medida_port")
#' @return lista com análise completa
#' 
#' @examples
#' analise <- analisar_unidades_medida(data_consumo)
#' View(analise$materiais_multiplas_unidades)
analisar_unidades_medida <- function(data,
                                     coluna_material = "cd_material",
                                     coluna_unidade = "sg_medida_port",
                                     coluna_descricao = "ds_medida_port") {
  
  cat("\n🔍 ANÁLISE DE UNIDADES DE MEDIDA\n")
  cat("==========================================\n")
  
  # Verificar se colunas existem
  if(!coluna_unidade %in% names(data)) {
    stop(sprintf("Coluna '%s' não encontrada no dataframe", coluna_unidade))
  }
  
  # 1. Estatísticas gerais de unidades
  cat("\n📊 Estatísticas Gerais:\n")
  
  unidades_unicas <- data %>%
    distinct(.data[[coluna_unidade]], 
             if(coluna_descricao %in% names(data)) .data[[coluna_descricao]]) %>%
    arrange(.data[[coluna_unidade]])
  
  cat(sprintf("   - Total de unidades únicas: %d\n", nrow(unidades_unicas)))
  
  # Mostrar todas as unidades
  cat("\n📋 Unidades encontradas:\n")
  if(coluna_descricao %in% names(data)) {
    print(unidades_unicas, n = Inf)
  } else {
    cat(paste(unidades_unicas[[coluna_unidade]], collapse = ", "))
    cat("\n")
  }
  
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
    
    # Análise detalhada dos 3 piores casos
    cat("\n📋 Análise detalhada dos 3 casos mais complexos:\n\n")
    
    for(i in 1:min(3, nrow(conflitos_unidade))) {
      material_id <- conflitos_unidade[[coluna_material]][i]
      
      cat(sprintf("\nCaso %d - Material Mestre: %s\n", i, material_id))
      cat(sprintf("Unidades envolvidas: %s\n", conflitos_unidade$unidades[i]))
      
      detalhe <- data %>%
        filter(.data[[coluna_material]] == material_id) %>%
        group_by(.data[[coluna_unidade]]) %>%
        summarise(
          n_registros = n(),
          qt_total = sum(qt_consumo, na.rm = TRUE),
          qt_media = mean(qt_consumo, na.rm = TRUE),
          periodo_min = sprintf("%02d/%d", min(mes_competencia), min(ano_competencia)),
          periodo_max = sprintf("%02d/%d", max(mes_competencia), max(ano_competencia)),
          .groups = 'drop'
        ) %>%
        mutate(proporcao = n_registros / sum(n_registros) * 100)
      
      print(detalhe)
    }
    
  } else {
    cat("   ✅ Perfeito! Todos os materiais mestres têm unidade única.\n")
    cat("   Nenhuma conversão de unidade necessária.\n")
  }
  
  return(conflitos_unidade)
}


#' Cria tabela de conversão de unidades
#' 
#' Esta é uma função template que deve ser customizada com suas regras específicas
#' 
#' @return dataframe com regras de conversão
#' 
#' @details
#' Estrutura da tabela de conversão:
#' - unidade_origem: unidade a ser convertida
#' - unidade_destino: unidade de destino (base)
#' - fator_conversao: multiplicador para converter
#' - formula: descrição da conversão
#' 
#' @examples
#' # Exemplo de uso
#' tabela_conv <- criar_tabela_conversao()
#' 
#' # Ver conversões disponíveis
#' View(tabela_conv)
criar_tabela_conversao <- function() {
  
  # TEMPLATE: Personalize com suas conversões específicas
  # Estas são apenas EXEMPLOS - você deve preencher com regras reais
  
  tabela_conversao <- tribble(
    ~unidade_origem, ~unidade_destino, ~fator_conversao, ~formula, ~categoria,
    
    # COMPRIMENTO
    "M",   "CM",  100,     "1 metro = 100 centímetros",           "comprimento",
    "CM",  "M",   0.01,    "1 centímetro = 0.01 metros",          "comprimento",
    "KM",  "M",   1000,    "1 quilômetro = 1000 metros",          "comprimento",
    
    # MASSA
    "KG",  "G",   1000,    "1 quilograma = 1000 gramas",          "massa",
    "G",   "KG",  0.001,   "1 grama = 0.001 quilogramas",         "massa",
    "TON", "KG",  1000,    "1 tonelada = 1000 quilogramas",       "massa",
    
    # VOLUME
    "L",   "ML",  1000,    "1 litro = 1000 mililitros",           "volume",
    "ML",  "L",   0.001,   "1 mililitro = 0.001 litros",          "volume",
    "M3",  "L",   1000,    "1 metro cúbico = 1000 litros",        "volume",
    
    # ÁREA
    "M2",  "CM2", 10000,   "1 m² = 10000 cm²",                    "area",
    
    # QUANTIDADE (casos especiais)
    "DZ",  "UN",  12,      "1 dúzia = 12 unidades",               "quantidade",
    "CX",  "UN",  NA,      "Caixa - requer info do fabricante",   "quantidade",
    "PC",  "UN",  1,       "Peça = Unidade",                      "quantidade",
    "PAR", "UN",  2,       "1 par = 2 unidades",                  "quantidade",
    "JG",  "UN",  NA,      "Jogo - requer info do fabricante",    "quantidade",
    
    # ELÉTRICA
    "KW",  "W",   1000,    "1 quilowatt = 1000 watts",            "eletrica",
    
    # CONVERSÕES ESPECÍFICAS (adicionar conforme necessário)
    # Adicione aqui conversões específicas do seu domínio
  )
  
  return(tabela_conversao)
}


#' Identifica regras de conversão necessárias
#' 
#' Analisa os conflitos de unidade e sugere quais conversões implementar
#' 
#' @param conflitos_unidade dataframe retornado por analisar_unidades_pos_agregacao
#' @param data dataframe completo para análise detalhada
#' @param coluna_material nome da coluna de material
#' @param coluna_unidade nome da coluna de unidade
#' @return dataframe com regras sugeridas
identificar_conversoes_necessarias <- function(conflitos_unidade,
                                               data,
                                               coluna_material = "cd_material_final",
                                               coluna_unidade = "sg_medida_port") {
  
  cat("\n🔧 IDENTIFICAÇÃO DE CONVERSÕES NECESSÁRIAS\n")
  cat("==========================================\n")
  
  if(nrow(conflitos_unidade) == 0) {
    cat("✅ Nenhuma conversão necessária.\n")
    return(NULL)
  }
  
  # Para cada material com conflito, identificar qual deve ser a unidade base
  # (geralmente a mais frequente)
  
  regras_sugeridas <- conflitos_unidade %>%
    rowwise() %>%
    mutate(
      analise_detalhada = list({
        data %>%
          filter(.data[[coluna_material]] == .data[[coluna_material]]) %>%
          group_by(.data[[coluna_unidade]]) %>%
          summarise(
            n_ocorrencias = n(),
            qt_total = sum(qt_consumo, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(n_ocorrencias)) %>%
          mutate(
            unidade_sugerida = first(.data[[coluna_unidade]]),
            deve_converter = .data[[coluna_unidade]] != unidade_sugerida
          )
      })
    ) %>%
    ungroup()
  
  cat(sprintf("\n📋 Total de materiais com conflito: %d\n", nrow(regras_sugeridas)))
  cat("\nPara cada material, a unidade mais frequente foi selecionada como base.\n")
  cat("As demais unidades deverão ser convertidas.\n\n")
  
  # Extrair todas as conversões únicas necessárias
  conversoes_unicas <- regras_sugeridas %>%
    select(cd_material_final, analise_detalhada) %>%
    unnest(analise_detalhada) %>%
    filter(deve_converter) %>%
    select(
      cd_material_final,
      unidade_origem = sg_medida_port,
      unidade_destino = unidade_sugerida,
      n_ocorrencias,
      qt_total
    ) %>%
    group_by(unidade_origem, unidade_destino) %>%
    summarise(
      n_materiais_afetados = n(),
      n_registros_total = sum(n_ocorrencias),
      qt_total_afetada = sum(qt_total),
      materiais = paste(cd_material_final, collapse = ", "),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_materiais_afetados))
  
  cat("🎯 Conversões únicas necessárias:\n\n")
  print(conversoes_unicas, n = Inf)
  
  # Salvar para referência
  cat("\n💾 Salvando análise detalhada...\n")
  
  return(list(
    regras_por_material = regras_sugeridas,
    conversoes_necessarias = conversoes_unicas
  ))
}


#' Aplica conversão de unidades
#' 
#' Converte quantidades para unidade base do material mestre
#' 
#' @param data dataframe com dados
#' @param tabela_conversao dataframe com regras de conversão
#' @param unidade_base_por_material dataframe com unidade base de cada material
#' @param coluna_material nome da coluna de material
#' @param coluna_unidade nome da coluna de unidade
#' @param coluna_quantidade nome da coluna de quantidade
#' @return dataframe com quantidades convertidas
aplicar_conversao_unidades <- function(data,
                                       tabela_conversao,
                                       unidade_base_por_material,
                                       coluna_material = "cd_material_final",
                                       coluna_unidade = "sg_medida_port",
                                       coluna_quantidade = "qt_consumo") {
  
  cat("\n🔄 APLICANDO CONVERSÃO DE UNIDADES\n")
  cat("==========================================\n")
  
  # Adicionar unidade base para cada material
  data_com_base <- data %>%
    left_join(
      unidade_base_por_material,
      by = coluna_material
    )
  
  # Identificar quais registros precisam de conversão
  registros_converter <- data_com_base %>%
    filter(.data[[coluna_unidade]] != unidade_base)
  
  cat(sprintf("   - Registros que precisam de conversão: %s\n",
              format(nrow(registros_converter), big.mark = ",")))
  
  if(nrow(registros_converter) == 0) {
    cat("   ✅ Nenhuma conversão necessária!\n")
    return(data)
  }
  
  # Aplicar conversões
  data_convertido <- data_com_base %>%
    left_join(
      tabela_conversao %>% 
        select(unidade_origem, unidade_destino, fator_conversao),
      by = c(
        coluna_unidade = "unidade_origem",
        "unidade_base" = "unidade_destino"
      )
    ) %>%
    mutate(
      qt_original = .data[[coluna_quantidade]],
      conversao_aplicada = !is.na(fator_conversao),
      !!coluna_quantidade := ifelse(
        conversao_aplicada,
        .data[[coluna_quantidade]] * fator_conversao,
        .data[[coluna_quantidade]]
      ),
      unidade_final = unidade_base
    )
  
  # Estatísticas
  n_convertidos <- sum(data_convertido$conversao_aplicada, na.rm = TRUE)
  n_sem_regra <- sum(
    data_convertido[[coluna_unidade]] != data_convertido$unidade_base & 
    !data_convertido$conversao_aplicada,
    na.rm = TRUE
  )
  
  cat(sprintf("   - Registros convertidos: %s\n", 
              format(n_convertidos, big.mark = ",")))
  
  if(n_sem_regra > 0) {
    cat(sprintf("   ⚠️  Registros SEM regra de conversão: %s\n",
                format(n_sem_regra, big.mark = ",")))
    cat("   Estes registros serão mantidos na unidade original!\n")
    
    # Mostrar quais conversões faltam
    conversoes_faltantes <- data_convertido %>%
      filter(
        .data[[coluna_unidade]] != unidade_base,
        !conversao_aplicada
      ) %>%
      count(.data[[coluna_unidade]], unidade_base, name = "n_registros") %>%
      arrange(desc(n_registros))
    
    cat("\n   Conversões faltantes:\n")
    print(conversoes_faltantes)
  }
  
  return(data_convertido)
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
