# =============================================================================
# FUNÇÕES AUXILIARES PARA FORECASTING INTERMITENTE
# =============================================================================
#
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
#' @param train_ts Série temporal de treino (vetor numérico ou objeto ts)
#' @param alphas_grid Vetor de alphas candidatos para testar
#' @param cv_horizon Número de períodos reservados para validação
#' @param method Tipo de método Croston: 'croston', 'sba' ou 'tsb'
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
                           method = "croston",
                           debug = FALSE) {
  
  # Converter para vetor numérico se necessário
  if(inherits(train_ts, "ts")) {
    train_ts <- as.numeric(train_ts)
  }
  
  n <- length(train_ts)
  
  # Log de debug
  if(debug) {
    cat(sprintf("\n[DEBUG] otimizar_alpha:\n"))
    cat(sprintf("  - Método: %s\n", method))
    cat(sprintf("  - n = %d períodos\n", n))
    cat(sprintf("  - Valores não-zero: %d (%.1f%%)\n", 
                sum(train_ts > 0), mean(train_ts > 0) * 100))
    cat(sprintf("  - cv_horizon = %d\n", cv_horizon))
    cat(sprintf("  - Mínimo requerido: %d períodos\n", cv_horizon + 12))
  }
  
  # Verificar se série é longa o suficiente para CV
  if(n < (cv_horizon + 12)) {
    if(debug) {
      cat(sprintf("  → Série muito curta, retornando alpha padrão 0.10\n"))
    }
    return(0.10)
  }
  
  # Verificar se série tem valores não-zero suficientes
  n_nonzero <- sum(train_ts > 0)
  if(n_nonzero < 3) {
    if(debug) {
      cat(sprintf("  → Poucos valores não-zero (%d), retornando alpha padrão 0.10\n", 
                  n_nonzero))
    }
    return(0.10)
  }
  
  # Dividir em treino-validação
  train_cv <- train_ts[1:(n - cv_horizon)]
  valid_cv <- train_ts[(n - cv_horizon + 1):n]
  
  if(debug) {
    cat(sprintf("  - Treino CV: %d períodos\n", length(train_cv)))
    cat(sprintf("  - Validação CV: %d períodos\n", length(valid_cv)))
    cat(sprintf("  - Testando %d alphas: %s\n", 
                length(alphas_grid), 
                paste(alphas_grid, collapse = ", ")))
  }
  
  # Testar cada alpha
  maes <- vapply(alphas_grid, function(alpha_test) {
    
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
      
      # ✅ CORREÇÃO: Usar $frc.out ao invés de $mean
      fc <- as.numeric(fit$frc.out)
      
      # Verificar se previsões são válidas
      if(all(is.na(fc)) || all(is.infinite(fc)) || length(fc) == 0) {
        if(debug) cat(sprintf("    ⚠️  Alpha %.2f: previsões inválidas\n", alpha_test))
        return(Inf)
      }
      
      # Calcular MAE
      mae <- mean(abs(valid_cv - fc), na.rm = TRUE)
      
      if(debug) cat(sprintf("    ✓ Alpha %.2f: MAE = %.3f\n", alpha_test, mae))
      
      return(mae)
      
    }, error = function(e) {
      # Se alpha falhar, penalizar com Inf
      if(debug) {
        cat(sprintf("    ✗ Alpha %.2f: ERRO - %s\n", 
                    alpha_test, conditionMessage(e)))
      }
      return(Inf)
    })
  }, FUN.VALUE = numeric(1))
  
  # Selecionar melhor alpha (menor MAE)
  if(all(is.infinite(maes))) {
    # Todos os alphas falharam - retornar padrão
    if(debug) {
      cat(sprintf("  → TODOS os alphas falharam, retornando padrão 0.10\n"))
    }
    return(0.10)
  }
  
  best_idx <- which.min(maes)
  best_alpha <- alphas_grid[best_idx]
  best_mae <- maes[best_idx]
  
  if(debug) {
    cat(sprintf("  → Melhor alpha: %.2f (MAE = %.3f)\n", best_alpha, best_mae))
  }
  
  return(best_alpha)
}