# FUNÇÕES PARA TRATAMENTO DE DADOS ####
#
# Arquivo: R/functions/tratamento_dados.R
# Descrição: Funções utilitárias para limpeza e tratamento de dados
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE

# Função para converter números brasileiros para formato R ####
#' Converte números no formato brasileiro para formato R
#' 
#' @param x vetor com números em formato brasileiro (vírgula como decimal)
#' @return vetor numérico no formato R (ponto como decimal)
#' @examples
#' converter_numero_brasileiro(c("123,45", "1.234,56", "150", ",5", "0,75"))
#' # [1]  123.45 1234.56  150.00    0.50    0.75
converter_numero_brasileiro <- function(x) {
  x_char <- as.character(x)
  
  case_when(
    is.na(x) ~ 0,                              # NA vira 0
    x_char == "" ~ 0,                          # String vazia vira 0
    x_char == " " ~ 0,                         # Espaço vira 0
    str_detect(x_char, "^,[0-9]+$") ~ {        # Formato: ,5 → 0.5
      as.numeric(paste0("0", str_replace(x_char, ",", ".")))
    },
    str_detect(x_char, "^[0-9]+,$") ~ {        # Formato: 123, → 123.0
      as.numeric(str_replace(x_char, ",$", ".0"))
    },
    str_detect(x_char, "^[0-9]+,[0-9]+$") ~ {  # Formato brasileiro: 123,45
      as.numeric(str_replace(x_char, ",", "."))
    },
    str_detect(x_char, "^[0-9]+\\.[0-9]{3},[0-9]+$") ~ { # Formato: 1.234,56
      x_char %>%
        str_replace("\\.", "") %>%             # Remove ponto dos milhares
        str_replace(",", ".") %>%              # Troca vírgula por ponto decimal
        as.numeric()
    },
    str_detect(x_char, "^[0-9]+$") ~ {         # Números inteiros
      as.numeric(x_char)
    },
    str_detect(x_char, "^[0-9]+\\.[0-9]+$") ~ { # Já no formato americano
      as.numeric(x_char)
    },
    TRUE ~ {                                   # Outros casos problemáticos
      suppressWarnings(as.numeric(x_char))
    }
  )
}

# Função para validar dados de consumo ####
#' Valida qualidade dos dados de consumo
#' 
#' @param data dataframe com dados de consumo
#' @param coluna_qt nome da coluna de quantidade (padrão: "qt_consumo")
#' @return lista com estatísticas de validação
validar_dados_consumo <- function(data, coluna_qt = "qt_consumo") {
  
  # Extrair coluna de quantidade
  qt_col <- data[[coluna_qt]]
  
  # Estatísticas básicas
  total_registros <- nrow(data)
  registros_na <- sum(is.na(qt_col))
  registros_vazios <- sum(qt_col == "", na.rm = TRUE)
  registros_zero <- sum(qt_col == 0, na.rm = TRUE)
  
  # Tentar conversão para identificar problemas
  qt_convertido <- suppressWarnings(converter_numero_brasileiro(qt_col))
  registros_problema <- sum(is.na(qt_convertido) & !is.na(qt_col))
  
  # Valores únicos problemáticos
  valores_problema <- data %>%
    mutate(qt_convertido = converter_numero_brasileiro(.data[[coluna_qt]])) %>%
    filter(is.na(qt_convertido) & !is.na(.data[[coluna_qt]])) %>%
    distinct(.data[[coluna_qt]]) %>%
    pull(.data[[coluna_qt]])
  
  # Retornar estatísticas
  list(
    total_registros = total_registros,
    registros_na = registros_na,
    registros_vazios = registros_vazios,
    registros_zero = registros_zero,
    registros_problema = registros_problema,
    valores_problema = valores_problema,
    percentual_problemas = round((registros_problema / total_registros) * 100, 2)
  )
}

# Função para criar relatório de qualidade ####
#' Cria relatório de qualidade dos dados
#' 
#' @param validacao resultado da função validar_dados_consumo
#' @param nome_dataset nome do dataset para o relatório
criar_relatorio_qualidade <- function(validacao, nome_dataset = "Dataset") {
  cat("\n📊 RELATÓRIO DE QUALIDADE - ", nome_dataset, "\n")
  cat("==========================================\n")
  cat(sprintf("📝 Total de registros: %s\n", 
              format(validacao$total_registros, big.mark = ",")))
  cat(sprintf("❌ Registros NA: %s\n", 
              format(validacao$registros_na, big.mark = ",")))
  cat(sprintf("🔤 Registros vazios: %s\n", 
              format(validacao$registros_vazios, big.mark = ",")))
  cat(sprintf("0️⃣  Registros zero: %s\n", 
              format(validacao$registros_zero, big.mark = ",")))
  cat(sprintf("⚠️  Registros problemáticos: %s (%.2f%%)\n", 
              format(validacao$registros_problema, big.mark = ","),
              validacao$percentual_problemas))
  
  if(length(validacao$valores_problema) > 0) {
    cat("\n🔍 Exemplos de valores problemáticos:\n")
    print(head(validacao$valores_problema, 10))
  } else {
    cat("\n✅ Nenhum valor problemático encontrado!\n")
  }
  cat("\n")
}

# Função para limpar e agregar dados por material mestre ####
#' Limpa dados e agrega por material mestre
#' 
#' @param data dataframe com dados de consumo
#' @param coluna_material nome da coluna de material
#' @param coluna_ano nome da coluna de ano
#' @param coluna_mes nome da coluna de mês
#' @param coluna_qt nome da coluna de quantidade
#' @return dataframe agregado e limpo
agregar_por_material_mestre <- function(data, 
                                        coluna_material = "cd_material_final",
                                        coluna_ano = "ano_competencia", 
                                        coluna_mes = "mes_competencia",
                                        coluna_qt = "qt_consumo") {
  
  data %>%
    mutate(
      # Converter quantidade usando função brasileira
      qt_limpo = converter_numero_brasileiro(.data[[coluna_qt]]),
      # Garantir que NAs virem 0
      qt_limpo = ifelse(is.na(qt_limpo), 0, qt_limpo)
    ) %>%
    group_by(
      .data[[coluna_material]],
      .data[[coluna_ano]],
      .data[[coluna_mes]]
    ) %>%
    summarise(
      qt_total = sum(qt_limpo, na.rm = TRUE),
      registros_originais = n(),
      registros_com_problema = sum(is.na(converter_numero_brasileiro(.data[[coluna_qt]]))),
      .groups = 'drop'
    ) %>%
    # Renomear colunas para padrão
    rename(
      cd_material = .data[[coluna_material]],
      ano_competencia = .data[[coluna_ano]],
      mes_competencia = .data[[coluna_mes]]
    )
}