# =============================================================================
# FUNÇÕES DE MÉTRICAS DE ERRO
# =============================================================================
#
# Descrição: Implementa métricas padrão e especializadas para avaliação
#            de forecasts em demanda intermitente
# Data: 2025-12-08
# Versão: 1.0.0
#


# =============================================================================
# MÉTRICAS BÁSICAS ####
# =============================================================================

#' Mean Absolute Error (MAE)
#' 
#' Métrica primária para avaliação de forecasts. Representa o erro médio
#' absoluto entre valores reais e previstos.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Valor numérico do MAE
#' @export
#' 
#' @examples
#' calculate_mae(c(5, 10, 15), c(6, 9, 14))
calculate_mae <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  mean(abs(real - pred), na.rm = TRUE)
}


#' Root Mean Squared Error (RMSE)
#' 
#' Penaliza erros grandes de forma quadrática. Útil para identificar
#' métodos com erros pontuais significativos.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Valor numérico do RMSE
#' @export
calculate_rmse <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  sqrt(mean((real - pred)^2, na.rm = TRUE))
}


#' Bias (Mean Error)
#' 
#' Identifica tendência sistemática de superestimação (positivo) ou
#' subestimação (negativo). Valores próximos de zero indicam previsões
#' não enviesadas.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Valor numérico do Bias
#' @export
calculate_bias <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  mean(pred - real, na.rm = TRUE)
}


#' Mean Absolute Percentage Error (MAPE)
#' 
#' Erro percentual médio. ATENÇÃO: não apropriado para demanda intermitente
#' devido a divisão por zero. Incluído apenas para comparação com literatura.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Valor numérico do MAPE (em percentual)
#' @export
calculate_mape <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  # Remover zeros para evitar divisão por zero
  valid_idx <- real != 0
  
  if(sum(valid_idx) == 0) {
    warning("MAPE indefinido: todos os valores reais são zero")
    return(NA_real_)
  }
  
  mean(abs((real[valid_idx] - pred[valid_idx]) / real[valid_idx]) * 100, 
       na.rm = TRUE)
}

# =============================================================================
# MÉTRICAS ESPECIALIZADAS ####
# =============================================================================

#' LinLin Loss (Linear-Linear Asymmetric Loss)
#' 
#' Função de perda assimétrica que penaliza diferentemente superestimação
#' e subestimação. Apropriada para contextos onde stockouts têm custo
#' diferente de excesso de estoque.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @param p Proporção de penalidade para subestimação (0 < p < 1).
#'          p = 0.85 implica que subestimar custa 85% e superestimar 15%.
#' @return Valor numérico da perda LinLin
#' @export
#' 
#' @references
#' SYNTETOS, A. A.; BOYLAN, J. E. The accuracy of intermittent demand 
#' estimates. International Journal of Forecasting, v. 21, n. 2, 
#' p. 303-314, 2005.
#' 
#' @examples
#' # p = 0.85 penaliza mais fortemente subestimação (stockouts)
#' calculate_linlin(c(10, 5, 8), c(8, 6, 10), p = 0.85)
calculate_linlin <- function(real, pred, p = 0.85) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(p <= 0 || p >= 1) {
    stop("Parâmetro p deve estar entre 0 e 1 (exclusivo)")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  # Calcular erros
  errors <- pred - real
  
  # Aplicar penalidades assimétricas
  loss <- ifelse(
    errors >= 0,
    (1 - p) * abs(errors),  # Overestimation (excesso de estoque)
    p * abs(errors)         # Underestimation (stockout)
  )
  
  mean(loss, na.rm = TRUE)
}


#' MAD/Mean Ratio (Coefficient of Variation of MAD)
#' 
#' Razão entre Mean Absolute Deviation e demanda média. Permite comparação
#' entre SKUs com diferentes escalas de demanda. Valores menores indicam
#' melhor acurácia relativa.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Valor numérico do MAD/Mean ratio
#' @export
calculate_mad_mean_ratio <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  mad <- mean(abs(real - pred), na.rm = TRUE)
  mean_real <- mean(real, na.rm = TRUE)
  
  # Evitar divisão por zero
  if(mean_real == 0) {
    warning("MAD/Mean ratio indefinido: demanda média é zero")
    return(NA_real_)
  }
  
  mad / mean_real
}


#' Period Error Rate (PER)
#' 
#' Proporção de períodos onde o forecast teve direção correta (zero vs não-zero).
#' Métrica específica para demanda intermitente que avalia acerto de padrão.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @return Proporção de acertos (0 a 1)
#' @export
calculate_per <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(NA_real_)
  }
  
  # Classificar como zero ou não-zero
  real_class <- ifelse(real == 0, 0, 1)
  pred_class <- ifelse(pred == 0, 0, 1)
  
  # Calcular acurácia de classificação
  mean(real_class == pred_class, na.rm = TRUE)
}

# =============================================================================
# MÉTRICAS AGREGADAS (PERSPECTIVA ANUAL) ####
# =============================================================================

#' Erro Absoluto em Perspectiva Anual
#' 
#' Calcula erro absoluto considerando demanda agregada em 12 meses.
#' Apropriado para avaliação de métodos para planejamento orçamentário.
#' 
#' @param real Vetor de valores reais observados (12 meses)
#' @param pred Vetor de valores previstos (12 meses)
#' @return Lista com erro absoluto e percentual anual
#' @export
calculate_annual_error <- function(real, pred) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(list(
      erro_absoluto = NA_real_,
      erro_percentual = NA_real_
    ))
  }
  
  # Agregar demanda em 12 meses
  demanda_real_anual <- sum(real, na.rm = TRUE)
  demanda_prevista_anual <- sum(pred, na.rm = TRUE)
  
  # Erro absoluto
  erro_absoluto <- abs(demanda_real_anual - demanda_prevista_anual)
  
  # Erro percentual
  if(demanda_real_anual == 0) {
    erro_percentual <- NA_real_
  } else {
    erro_percentual <- (demanda_prevista_anual - demanda_real_anual) / 
      demanda_real_anual * 100
  }
  
  list(
    demanda_real_anual = demanda_real_anual,
    demanda_prevista_anual = demanda_prevista_anual,
    erro_absoluto = erro_absoluto,
    erro_percentual = erro_percentual
  )
}

# =============================================================================
# FUNÇÕES AUXILIARES ####
# =============================================================================

#' Categorizar Família do Método
#' 
#' Classifica métodos de previsão em suas respectivas famílias conforme
#' taxonomia estabelecida no trabalho.
#' 
#' @param metodo_nome Nome do método de previsão
#' @return String com nome da família
#' @export
categorizar_familia_metodo <- function(metodo_nome) {
  
  # Normalizar para case-insensitive
  metodo_upper <- toupper(metodo_nome)
  
  dplyr::case_when(
    metodo_upper %in% c("NAIVE", "MEAN", "MA", "MA_36") ~ 
      "Familia_1_Classicos",
    
    metodo_upper %in% c("ARIMA", "ETS", "HW_ADDITIVE", "HW_ADD",
                        "HW_MULTIPLICATIVE", "HW_MULT", "TSLM") ~ 
      "Familia_2_Suavizacao",
    
    metodo_upper %in% c("CROSTON", "SBA", "TSB") ~ 
      "Familia_3_Intermitentes",
    
    stringr::str_detect(metodo_upper, "POISSON|GAMMA") ~ 
      "Familia_4_Probabilisticos",
    
    stringr::str_detect(metodo_upper, "ADIDA") ~ 
      "Familia_5_ADIDA",
    
    TRUE ~ "Outros"
  )
}


#' Calcular Todas as Métricas de uma Vez
#' 
#' Função wrapper que calcula todas as métricas disponíveis para um par
#' de vetores real-previsto.
#' 
#' @param real Vetor de valores reais observados
#' @param pred Vetor de valores previstos
#' @param p Parâmetro de penalidade para LinLin (default: 0.85)
#' @return Lista com todas as métricas calculadas
#' @export
calculate_all_metrics <- function(real, pred, p = 0.85) {
  
  # Validações
  if(length(real) != length(pred)) {
    stop("Vetores real e pred devem ter o mesmo comprimento")
  }
  
  if(length(real) == 0) {
    return(list(
      mae = NA_real_,
      rmse = NA_real_,
      bias = NA_real_,
      linlin = NA_real_,
      mad_mean_ratio = NA_real_,
      per = NA_real_,
      n_obs = 0,
      n_zeros_real = 0,
      n_zeros_pred = 0
    ))
  }
  
  # Calcular métricas
  list(
    mae = calculate_mae(real, pred),
    rmse = calculate_rmse(real, pred),
    bias = calculate_bias(real, pred),
    linlin = calculate_linlin(real, pred, p = p),
    mad_mean_ratio = calculate_mad_mean_ratio(real, pred),
    per = calculate_per(real, pred),
    
    # Estatísticas descritivas
    n_obs = length(real),
    n_zeros_real = sum(real == 0),
    n_zeros_pred = sum(pred == 0),
    
    # Demanda total
    demanda_real_total = sum(real, na.rm = TRUE),
    demanda_prevista_total = sum(pred, na.rm = TRUE)
  )
}


#' Validar Vetores de Entrada
#' 
#' Verifica consistência e validade de vetores real e pred antes do
#' cálculo de métricas.
#' 
#' @param real Vetor de valores reais
#' @param pred Vetor de valores previstos
#' @return Lista com status de validação
#' @export
validar_vetores_metricas <- function(real, pred) {
  
  erros <- character()
  avisos <- character()
  
  # Verificar comprimentos
  if(length(real) != length(pred)) {
    erros <- c(erros, "Comprimentos diferentes entre real e pred")
  }
  
  # Verificar se há dados
  if(length(real) == 0) {
    erros <- c(erros, "Vetores vazios")
  }
  
  # Verificar NAs
  if(any(is.na(real))) {
    avisos <- c(avisos, sprintf("%d NAs em valores reais", sum(is.na(real))))
  }
  
  if(any(is.na(pred))) {
    avisos <- c(avisos, sprintf("%d NAs em valores previstos", sum(is.na(pred))))
  }
  
  # Verificar valores negativos
  if(any(real < 0, na.rm = TRUE)) {
    avisos <- c(avisos, "Valores negativos em real")
  }
  
  if(any(pred < 0, na.rm = TRUE)) {
    avisos <- c(avisos, "Valores negativos em pred")
  }
  
  # Status
  valido <- length(erros) == 0
  
  list(
    valido = valido,
    erros = if(length(erros) > 0) erros else NULL,
    avisos = if(length(avisos) > 0) avisos else NULL
  )
}

# =============================================================================
# TESTES UNITÁRIOS (OPCIONAL) ####
# =============================================================================

#' Executar Testes de Validação
#' 
#' Testa as funções de métricas com casos conhecidos para garantir
#' implementação correta.
#' 
#' @return TRUE se todos os testes passaram
#' @export
test_error_metrics <- function() {
  
  cat("\n🧪 Executando testes de validação...\n\n")
  
  # Teste 1: MAE
  real1 <- c(10, 20, 30)
  pred1 <- c(12, 18, 32)
  mae1 <- calculate_mae(real1, pred1)
  expected_mae1 <- (2 + 2 + 2) / 3
  
  if(abs(mae1 - expected_mae1) < 1e-10) {
    cat("✅ Teste MAE: PASSOU\n")
  } else {
    cat("❌ Teste MAE: FALHOU\n")
    return(FALSE)
  }
  
  # Teste 2: RMSE
  rmse1 <- calculate_rmse(real1, pred1)
  expected_rmse1 <- sqrt((4 + 4 + 4) / 3)
  
  if(abs(rmse1 - expected_rmse1) < 1e-10) {
    cat("✅ Teste RMSE: PASSOU\n")
  } else {
    cat("❌ Teste RMSE: FALHOU\n")
    return(FALSE)
  }
  
  # Teste 3: Bias
  bias1 <- calculate_bias(real1, pred1)
  expected_bias1 <- (2 - 2 + 2) / 3
  
  if(abs(bias1 - expected_bias1) < 1e-10) {
    cat("✅ Teste Bias: PASSOU\n")
  } else {
    cat("❌ Teste Bias: FALHOU\n")
    return(FALSE)
  }
  
  # Teste 4: LinLin (p = 0.85)
  real2 <- c(10, 10, 10)
  pred2 <- c(12, 8, 10)  # +2 (over), -2 (under), 0 (exact)
  linlin2 <- calculate_linlin(real2, pred2, p = 0.85)
  expected_linlin2 <- ((1-0.85)*2 + 0.85*2 + 0) / 3
  
  if(abs(linlin2 - expected_linlin2) < 1e-10) {
    cat("✅ Teste LinLin: PASSOU\n")
  } else {
    cat("❌ Teste LinLin: FALHOU\n")
    return(FALSE)
  }
  
  # Teste 5: PER
  real3 <- c(0, 5, 0, 10)
  pred3 <- c(0, 3, 2, 8)  # Acerta: 0=0, 5≠0 vs 3≠0, erra: 0 vs 2≠0, acerta: 10≠0 vs 8≠0
  per3 <- calculate_per(real3, pred3)
  expected_per3 <- 3/4  # 3 acertos em 4
  
  if(abs(per3 - expected_per3) < 1e-10) {
    cat("✅ Teste PER: PASSOU\n")
  } else {
    cat("❌ Teste PER: FALHOU\n")
    return(FALSE)
  }
  
  cat("\n✅ Todos os testes passaram!\n\n")
  return(TRUE)
}