# =============================================================================
# FUNÇÕES AUXILIARES PARA FORECASTING INTERMITENTE
# =============================================================================
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Funções de suporte para métodos intermitentes (Croston, SBA, TSB)
# Data: 2025-12-09
# Versão: 1.0.0

#' Otimizar parâmetro alpha via validação cruzada temporal
#'
#' Função para selecionar o melhor parâmetro de suavização (alpha) para
#' métodos de previsão de demanda intermitente através de validação cruzada
#' temporal. Testa diferentes valores de alpha e seleciona aquele com menor
#' MAE no conjunto de validação.
#'
#' @param train_ts Série temporal de treino (vetor numérico)
#' @param alphas_grid Vetor de alphas candidatos
#' @param cv_horizon Número de períodos para validação
#' @param method Método Croston ('croston', 'sba', 'tsb')
#' 
#' @return Valor numérico do melhor alpha (escalar entre 0 e 1)
#' 
#' @details
#' A função divide a série de treino em:
#' - Treino CV: primeiros (n - cv_horizon) períodos
#' - Validação CV: últimos cv_horizon períodos
#' 
#' Testa cada alpha candidato, calcula o MAE na validação e retorna
#' o alpha com melhor desempenho. Se a série for muito curta ou algum
#' alpha falhar, retorna um valor padrão conservador (0.10).
#' 
#' @examples
#' train_ts <- c(0, 5, 0, 0, 3, 0, 2, 0, 0, 4, 0, 0)
#' best_alpha <- otimizar_alpha(train_ts, method = "croston")
#' 
#' @export
otimizar_alpha <- function(train_ts, 
                           alphas_grid = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30),
                           cv_horizon = 6,
                           method = "croston") {
  
  # Converter para vetor numérico se necessário
  if(inherits(train_ts, "ts")) {
    train_ts <- as.numeric(train_ts)
  }
  
  n <- length(train_ts)
  
  # Verificar se série é longa o suficiente para CV
  if(n < (cv_horizon + 12)) {
    # Série muito curta - retornar alpha conservador
    return(0.10)
  }
  
  # Dividir em treino-validação
  train_cv <- train_ts[1:(n - cv_horizon)]
  valid_cv <- train_ts[(n - cv_horizon + 1):n]
  
  # Testar cada alpha
  maes <- map_dbl(alphas_grid, function(alpha_test) {
    
    tryCatch({
      
      # Ajustar modelo com alpha candidato
      fit <- tsintermittent::crost(
        train_cv, 
        h = cv_horizon,
        w = alpha_test,
        type = method,
        init = "mean",
        outplot = FALSE  # Não gerar gráficos
      )
      
      # Extrair previsões
      fc <- as.numeric(fit$mean)
      
      # Calcular MAE
      mae <- mean(abs(valid_cv - fc), na.rm = TRUE)
      
      return(mae)
      
    }, error = function(e) {
      # Se alpha falhar, penalizar com Inf
      return(Inf)
    })
  }, FUN.VALUE = numeric(1))
  
  # Selecionar melhor alpha (menor MAE)
  if(all(is.infinite(maes))) {
    # Todos os alphas falharam - retornar padrão
    return(0.10)
  }
  
  best_idx <- which.min(maes)
  best_alpha <- alphas_grid[best_idx]
  
  return(best_alpha)
}
