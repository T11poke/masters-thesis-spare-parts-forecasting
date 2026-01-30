# 08 - VISUALIZAÇÃO DE SÉRIES TEMPORAIS COM PREVISÕES ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Visualização de séries temporais incluindo valores previstos 
#            e valores reais observados no período de teste, com amostras
#            representativas de cada categoria SBC
# Data: 2026-01-29
# Versão: 1.0.0
#

# Carregar configurações e bibliotecas ####

library(here)
library(tidyverse)
library(tsibble)
library(lubridate)
library(ggplot2)
library(ggsci)
library(ggthemes)
library(scales)
library(patchwork)
library(parallel)
library(future)


source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

if(config$computation$parallel) {
  plan(multisession, workers = config$computation$n_cores)
  log_message(sprintf("Paralelização ativada: %d cores", 
                      config$computation$n_cores), "INFO")
}

log_message("========================================", "INFO")
log_message("INICIANDO VISUALIZAÇÃO DE FORECASTS", "INFO")
log_message("========================================", "INFO")

# Criar diretórios de output ####
dir.create(here(config$paths$output$figures, "08_forecast_viz"), 
           showWarnings = FALSE, recursive = TRUE)

# Tema padrão para gráficos ####
theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    )
)

# =============================================================================
# BLOCO 1: CARREGAR DADOS ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: CARREGANDO DADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando dados processados", "INFO")

# Carregar splits (treino/teste)
splits_list <- readRDS(here(config$paths$data$processed, "train_test_splits.rds"))

# Carregar forecasts consolidados
forecasts_consolidados <- readRDS(here(config$paths$output$forecasts, 
                                       "forecasts_consolidated.rds"))

# Carregar forecasts anuais
forecasts_anuais <- readRDS(here(config$paths$output$forecasts, 
                                 "annual/forecasts_annual.rds"))

cat("✅ Dados carregados:\n")
cat(sprintf("   - Origens temporais: %d\n", length(splits_list)))
cat(sprintf("   - Forecasts mensais: %d origens\n", 
            length(forecasts_consolidados$forecasts)))
cat(sprintf("   - Forecasts anuais: %d origens\n", 
            length(forecasts_anuais)))

## 1.1. padronizar os nomes dos métodos nos datasets ####


cat("\n", strrep("=", 70), "\n", sep = "")
cat("PADRONIZANDO FORECASTS MENSAIS\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando padronização dos forecasts mensais", "INFO")

n_origens <- length(forecasts_consolidados$forecasts)
n_materiais_total <- 0
n_renomeacoes <- 0

cat(sprintf("📊 Processando %d origens...\n\n", n_origens))

for (origem_nome in names(forecasts_consolidados$forecasts)) {
  
  cat(sprintf("   Origem: %s\n", origem_nome))
  
  origem_data <- forecasts_consolidados$forecasts[[origem_nome]]
  n_materiais <- length(origem_data$forecasts)
  n_materiais_total <- n_materiais_total + n_materiais
  
  cat(sprintf("      Materiais: %d\n", n_materiais))
  
  # Processar cada material
  for (cd_material in names(origem_data$forecasts)) {
    
    mat_forecast <- origem_data$forecasts[[cd_material]]
    
    # Verificar se tem forecasts
    if (!is.null(mat_forecast$forecasts) && length(mat_forecast$forecasts) > 0) {
      
      nomes_originais <- names(mat_forecast$forecasts)
      nomes_novos <- tolower(nomes_originais)
      
      # Contar renomeações
      n_diferentes <- sum(nomes_originais != nomes_novos)
      if (n_diferentes > 0) {
        n_renomeacoes <- n_renomeacoes + n_diferentes
      }
      
      # Renomear
      names(mat_forecast$forecasts) <- nomes_novos
      
      # Atualizar no objeto original
      forecasts_consolidados$forecasts[[origem_nome]]$forecasts[[cd_material]] <- mat_forecast
    }
  }
  
  cat(sprintf("      ✅ Concluído\n"))
}

cat(sprintf("\n✅ Total de métodos renomeados: %s\n", 
            format(n_renomeacoes, big.mark = ",")))
cat(sprintf("✅ Total de materiais processados: %s\n", 
            format(n_materiais_total, big.mark = ",")))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("PADRONIZANDO FORECASTS ANUAIS\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando padronização dos forecasts anuais", "INFO")

n_origens_anual <- length(forecasts_anuais)
n_materiais_anual_total <- 0
n_renomeacoes_anual <- 0

cat(sprintf("📊 Processando %d origens...\n\n", n_origens_anual))

for (origem_nome in names(forecasts_anuais)) {
  
  cat(sprintf("   Origem: %s\n", origem_nome))
  
  origem_data_anual <- forecasts_anuais[[origem_nome]]
  n_materiais_anual <- length(origem_data_anual$forecasts)
  n_materiais_anual_total <- n_materiais_anual_total + n_materiais_anual
  
  cat(sprintf("      Materiais: %d\n", n_materiais_anual))
  
  # Processar cada material
  for (i in seq_along(origem_data_anual$forecasts)) {
    
    mat_forecast_anual <- origem_data_anual$forecasts[[i]]
    
    # Verificar se tem forecasts
    if (!is.null(mat_forecast_anual$forecasts) && length(mat_forecast_anual$forecasts) > 0) {
      
      nomes_originais <- names(mat_forecast_anual$forecasts)
      nomes_novos <- tolower(nomes_originais)
      
      # Contar renomeações
      n_diferentes <- sum(nomes_originais != nomes_novos)
      if (n_diferentes > 0) {
        n_renomeacoes_anual <- n_renomeacoes_anual + n_diferentes
      }
      
      # Renomear
      names(mat_forecast_anual$forecasts) <- nomes_novos
      
      # Atualizar no objeto original
      forecasts_anuais[[origem_nome]]$forecasts[[i]] <- mat_forecast_anual
    }
  }
  
  cat(sprintf("      ✅ Concluído\n"))
}

cat(sprintf("\n✅ Total de métodos renomeados: %s\n", 
            format(n_renomeacoes_anual, big.mark = ",")))
cat(sprintf("✅ Total de materiais processados: %s\n", 
            format(n_materiais_anual_total, big.mark = ",")))

# =============================================================================
# BLOCO 2: SELEÇÃO DE MATERIAIS EXEMPLO ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: SELECIONANDO MATERIAIS EXEMPLO\n")
cat(strrep("=", 70), "\n\n")

log_message("Selecionando 3 materiais por categoria SBC", "INFO")

#' Função para selecionar materiais representativos
#' @param sbc_data Classificação SBC dos materiais
#' @param train_data Dados de treino
#' @param categoria_alvo Categoria SBC alvo
#' @param n_amostras Número de amostras desejadas
#' @param criterio Critério de seleção: "mediano", "diversificado"
selecionar_materiais_exemplo <- function(sbc_data, train_data, categoria_alvo, 
                                         n_amostras = 3, 
                                         criterio = "diversificado") {
  
  materiais_categoria <- sbc_data %>%
    filter(categoria_sbc == categoria_alvo)
  
  if (nrow(materiais_categoria) == 0) {
    return(character(0))
  }
  
  if (nrow(materiais_categoria) <= n_amostras) {
    return(materiais_categoria$cd_material)
  }
  
  if (criterio == "mediano") {
    # Selecionar materiais próximos da mediana da categoria
    adi_med <- median(materiais_categoria$adi, na.rm = TRUE)
    cv2_med <- median(materiais_categoria$cv2, na.rm = TRUE)
    
    materiais_selecionados <- materiais_categoria %>%
      mutate(
        dist_adi = abs(adi - adi_med),
        dist_cv2 = abs(cv2 - cv2_med),
        dist_total = dist_adi + dist_cv2
      ) %>%
      arrange(dist_total) %>%
      slice(1:n_amostras) %>%
      pull(cd_material)
    
  } else if (criterio == "diversificado") {
    # Selecionar materiais que representem diversidade dentro da categoria
    # Dividir em tercis de ADI e CV²
    
    quantis_adi <- quantile(materiais_categoria$adi, 
                            probs = seq(0, 1, length.out = n_amostras + 1),
                            na.rm = TRUE)
    
    materiais_selecionados <- character(0)
    
    for (i in 1:(length(quantis_adi) - 1)) {
      mats_faixa <- materiais_categoria %>%
        filter(adi >= quantis_adi[i], adi < quantis_adi[i + 1])
      
      if (nrow(mats_faixa) > 0) {
        # Selecionar o mediano desta faixa
        mat_selecionado <- mats_faixa %>%
          arrange(abs(adi - median(adi, na.rm = TRUE))) %>%
          slice(1) %>%
          pull(cd_material)
        
        materiais_selecionados <- c(materiais_selecionados, mat_selecionado)
      }
    }
    
    # Garantir exatamente n_amostras
    if (length(materiais_selecionados) < n_amostras) {
      # Complementar com materiais aleatórios
      mats_restantes <- setdiff(materiais_categoria$cd_material, 
                                materiais_selecionados)
      if (length(mats_restantes) > 0) {
        n_faltante <- n_amostras - length(materiais_selecionados)
        mats_extra <- sample(mats_restantes, 
                             min(n_faltante, length(mats_restantes)))
        materiais_selecionados <- c(materiais_selecionados, mats_extra)
      }
    } else if (length(materiais_selecionados) > n_amostras) {
      materiais_selecionados <- materiais_selecionados[1:n_amostras]
    }
  }
  
  return(materiais_selecionados)
}

# Selecionar materiais da origem mais recente (origem_6)
origem_recente <- names(splits_list)[length(splits_list)]
cat(sprintf("📊 Usando %s como referência para seleção\n\n", origem_recente))

sbc_ref <- splits_list[[origem_recente]]$sbc_classification
train_ref <- splits_list[[origem_recente]]$train

# Selecionar 3 materiais por categoria
categorias_principais <- c("Smooth", "Erratic", "Intermittent", "Lumpy")

materiais_exemplo <- tibble(categoria = categorias_principais) %>%
  mutate(
    materiais = map(
      categoria,
      ~selecionar_materiais_exemplo(
        sbc_ref, train_ref, .x, 
        n_amostras = 2, 
        criterio = "mediano"
      )
    )
  ) %>%
  unnest(materiais) %>%
  rename(cd_material = materiais) %>%
  left_join(
    sbc_ref %>% select(cd_material, adi, cv2),
    by = "cd_material"
  )

cat("📋 Materiais selecionados:\n\n")
print(materiais_exemplo %>% 
        group_by(categoria) %>% 
        summarise(n = n(), .groups = 'drop'))

cat("\n")
print(materiais_exemplo, n = Inf)

# =============================================================================
# BLOCO 3: VISUALIZAÇÕES - PERSPECTIVA MENSAL ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: GERANDO VISUALIZAÇÕES - PERSPECTIVA MENSAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Criando gráficos de séries mensais", "INFO")

#' Função para criar gráfico de série temporal com previsões
#' @param cd_mat Código do material
#' @param origem_nome Nome da origem temporal
#' @param categoria Categoria SBC
#' @param metodos_plotar Vetor com nomes dos métodos a plotar
plot_forecast_mensal <- function(cd_mat, origem_nome, categoria, 
                                 metodos_plotar = NULL) {
  
  # Obter dados de treino e teste
  split_origem <- splits_list[[origem_nome]]
  train <- split_origem$train %>% 
    filter(cd_material == cd_mat) %>%
    as_tibble()
  
  test <- split_origem$test %>% 
    filter(cd_material == cd_mat) %>%
    as_tibble()
  
  # Obter forecasts para este material
  # Estrutura: forecasts_consolidados$forecasts$origem_X$forecasts$CD_MATERIAL
  forecasts_origem <- forecasts_consolidados$forecasts[[origem_nome]]
  
  if (is.null(forecasts_origem$forecasts[[cd_mat]])) {
    warning(sprintf("Material %s não encontrado nos forecasts", cd_mat))
    return(NULL)
  }
  
  mat_forecast <- forecasts_origem$forecasts[[cd_mat]]
  
  # Valores reais do teste (já estão no objeto)
  valores_reais_teste <- mat_forecast$valores_reais
  
  # Criar tibble com valores reais do teste
  test_valores <- tibble(
    data_competencia = seq(
      max(train$data_competencia) %m+% months(1),
      by = "month",
      length.out = length(valores_reais_teste)
    ),
    qt_total = valores_reais_teste
  )
  
  # Preparar dados de forecasts para plotagem
  forecast_data <- tibble()
  
  # Definir métodos a plotar
  if (is.null(metodos_plotar)) {
    metodos_plotar <- switch(
      categoria,
      "Smooth" = c("naive", "arima", "hw_add", "ets", "croston"),
      "Erratic" = c("sba", "croston", "tsb", "gamma", "tslm"),
      "Intermittent" = c("sba", "croston", "tsb", "adida_k12_mean", "ma"),
      "Lumpy" = c("adida_k12_mean", "adida_k3_mean", "mean", "sba", "croston"),
      # Default se categoria não reconhecida
      c("sba", "croston", "tsb", "ses", "naive")
    )
  }
  
  # Converter para lowercase para match
  metodos_plotar <- tolower(metodos_plotar)
  
  # Métodos disponíveis nos forecasts
  metodos_disponiveis <- names(mat_forecast$forecasts)
  metodos_usar <- intersect(metodos_plotar, metodos_disponiveis)
  
  if (length(metodos_usar) == 0) {
    warning(sprintf("Material %s: nenhum método disponível para plotar", cd_mat))
  }
  
  for (metodo in metodos_usar) {
    metodo_data <- mat_forecast$forecasts[[metodo]]
    
    # Validar estrutura do método
    if (!is.null(metodo_data) && !is.null(metodo_data$point)) {
      
      # Verificar convergência se disponível
      convergiu <- TRUE
      if (!is.null(metodo_data$convergence)) {
        convergiu <- metodo_data$convergence
      }
      
      if (convergiu) {
        n_forecast <- length(metodo_data$point)
        
        # Validar que temos valores
        if (n_forecast > 0) {
          forecast_df <- tibble(
            data_competencia = seq(
              max(train$data_competencia) %m+% months(1),
              by = "month",
              length.out = n_forecast
            ),
            valor_previsto = metodo_data$point,
            metodo = toupper(metodo)  # Nome em maiúscula para label
          )
          
          forecast_data <- bind_rows(forecast_data, forecast_df)
        }
      }
    }
  }
  
  # Obter informações SBC
  info_sbc <- split_origem$sbc_classification %>%
    filter(cd_material == cd_mat)
  
  if (nrow(info_sbc) == 0) {
    info_sbc <- tibble(adi = NA, cv2 = NA)
  }
  
  # Avisar se não há forecasts para plotar
  if (nrow(forecast_data) == 0) {
    warning(sprintf("Material %s: nenhum forecast convergiu - plotando apenas valores reais", 
                    cd_mat))
  }
  
  # Validar dados antes de plotar
  if (nrow(train) == 0) {
    warning(sprintf("Material %s sem dados de treino", cd_mat))
    return(NULL)
  }
  
  if (nrow(test_valores) == 0) {
    warning(sprintf("Material %s sem dados de teste", cd_mat))
    return(NULL)
  }
  
  # Criar gráfico base
  p <- ggplot() +
    # Série histórica (treino)
    geom_line(
      data = train,
      aes(x = data_competencia, y = qt_total),
      color = "steelblue",
      linewidth = 0.8,
      alpha = 0.7
    ) +
    geom_point(
      data = train %>% filter(qt_total > 0),
      aes(x = data_competencia, y = qt_total),
      color = "steelblue",
      size = 2,
      alpha = 0.5
    ) +
    # Valores reais no teste
    geom_line(
      data = test_valores,
      aes(x = data_competencia, y = qt_total),
      color = "darkred",
      linewidth = 0.8
    ) +
    geom_point(
      data = test_valores %>% filter(qt_total > 0),
      aes(x = data_competencia, y = qt_total),
      color = "darkred",
      size = 2.5
    ) +
    # Linha vertical separando treino/teste
    geom_vline(
      xintercept = as.numeric(max(train$data_competencia)),
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.5
    )
  
  # Adicionar previsões apenas se houver dados
  if (nrow(forecast_data) > 0) {
    
    # Garantir que temos tipos de linha suficientes para até 6 métodos
    n_metodos <- length(unique(forecast_data$metodo))
    tipos_linha <- c("dashed", "dotted", "dotdash", "longdash", "twodash","solid")[1:n_metodos]
    
    p <- p +
      geom_line(
        data = forecast_data,
        aes(x = data_competencia, y = valor_previsto, 
            color = metodo, linetype = metodo),
        linewidth = 0.7
      ) +
      scale_color_nejm() +
      scale_linetype_manual(
        values = tipos_linha
      )
  }
  
  # Adicionar labels
  p <- p +
    labs(
      # title = sprintf("Categoria: %s | Material: %s", categoria, cd_mat),
      title = sprintf("Material: %s", cd_mat),
      
      # subtitle = sprintf(
      #   "%s | ADI: %.2f | CV²: %.2f | Horizonte: 12 meses",
      #   origem_nome,
      #   info_sbc$adi[1],
      #   info_sbc$cv2[1]
      # ),
      subtitle = sprintf(
        "ADI: %.2f | CV²: %.2f",
        info_sbc$adi[1],
        info_sbc$cv2[1]
      ),
      x = "Período",
      y = "Quantidade Demandada",
      color = "Modelo",
      linetype = "Modelo"
    ) +
    theme_tufte() +
    theme(
      plot.subtitle = element_text(color = "gray40", size = 9),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  return(p)
}

# Gerar gráficos para materiais selecionados
cat("📊 Gerando gráficos individuais...\n\n")

# Métodos a plotar (nomes em lowercase conforme estrutura de dados)
# metodos_principais <- c("sba", "adida_k3_mean", "croston", "poisson", "naive")

plots_mensais <- list()

for (i in 1:nrow(materiais_exemplo)) {
  
  mat_info <- materiais_exemplo[i, ]
  
  cat(sprintf("   [%d/%d] Plotando %s (%s)...\n", 
              i, nrow(materiais_exemplo),
              mat_info$cd_material, 
              mat_info$categoria))
  
  tryCatch({
    p <- plot_forecast_mensal(
      cd_mat = mat_info$cd_material,
      origem_nome = origem_recente,
      categoria = mat_info$categoria,
      # metodos_plotar = metodos_principais
    )
    
    if (!is.null(p)) {
      plots_mensais[[i]] <- p
      cat(sprintf("        ✅ OK\n"))
    } else {
      cat(sprintf("        ⚠️  Retornou NULL\n"))
    }
    
  }, error = function(e) {
    cat(sprintf("        ❌ ERRO: %s\n", conditionMessage(e)))
    plots_mensais[[i]] <- NULL
  })
}

# Criar painéis por categoria
cat("\n📊 Criando painéis por categoria...\n\n")

n_graficos_salvos <- 0

for (cat_atual in categorias_principais) {
  
  cat(sprintf("   Categoria: %s\n", cat_atual))
  
  indices_categoria <- which(materiais_exemplo$categoria == cat_atual)
  
  if (length(indices_categoria) > 0) {
    
    plots_categoria <- plots_mensais[indices_categoria]
    plots_categoria <- plots_categoria[!sapply(plots_categoria, is.null)]
    
    cat(sprintf("      - Gráficos válidos: %d de %d\n", 
                length(plots_categoria), length(indices_categoria)))
    
    if (length(plots_categoria) > 0) {
      
      painel <- wrap_plots(plots_categoria, ncol = 1) +
        plot_annotation(
          # title = sprintf(
          #   "Categoria %s",
          #   cat_atual
          # ),
          subtitle = sprintf(
            "Linha azul: histórico | Linha vermelha: observado | Linhas coloridas: previsto"
          ),
          theme = theme(
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 11, color = "gray30")
          )
        )
      
      # Salvar
      filename <- sprintf("01_forecast_mensal_%s.png", tolower(cat_atual))
      
      ggsave(
        here(config$paths$output$figures, "08_forecast_viz", filename),
        plot = painel,
        width = 14,
        height = 4 * length(plots_categoria),
        dpi = 300
      )
      
      cat(sprintf("      ✅ Salvo: %s\n", filename))
      n_graficos_salvos <- n_graficos_salvos + 1
    } else {
      cat(sprintf("      ⚠️  Nenhum gráfico válido - arquivo não criado\n"))
    }
  } else {
    cat(sprintf("      ⚠️  Nenhum material nesta categoria\n"))
  }
  
  cat("\n")
}

cat(sprintf("📊 Total de arquivos mensais salvos: %d de %d categorias\n", 
            n_graficos_salvos, length(categorias_principais)))

# =============================================================================
# BLOCO 4: VISUALIZAÇÕES - PERSPECTIVA ANUAL ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: GERANDO VISUALIZAÇÕES - PERSPECTIVA ANUAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Criando gráficos de séries anuais", "INFO")

#' Função para criar gráfico de série anual com previsões
plot_forecast_anual <- function(cd_mat, origem_nome, categoria) {
  
  # Obter dados de treino e teste
  split_origem <- splits_list[[origem_nome]]
  
  # Agregar treino em anos
  train_anual <- split_origem$train %>%
    filter(cd_material == cd_mat) %>%
    as_tibble() %>%
    mutate(ano = year(data_competencia)) %>%
    group_by(ano) %>%
    summarise(qt_anual = sum(qt_total, na.rm = TRUE), .groups = 'drop')
  
  # Agregar teste em anos (do objeto test de splits)
  test_anual <- split_origem$test %>%
    filter(cd_material == cd_mat) %>%
    as_tibble() %>%
    mutate(ano = year(data_competencia)) %>%
    group_by(ano) %>%
    summarise(qt_anual = sum(qt_total, na.rm = TRUE), .groups = 'drop')
  
  # Obter forecasts anuais
  forecasts_origem_anual <- forecasts_anuais[[origem_nome]]
  
  # Encontrar forecast do material na lista
  mat_forecast <- NULL
  
  if (!is.null(forecasts_origem_anual$forecasts)) {
    for (forecast_item in forecasts_origem_anual$forecasts) {
      if (!is.null(forecast_item$cd_material) && 
          forecast_item$cd_material == cd_mat) {
        mat_forecast <- forecast_item
        break
      }
    }
  }
  
  if (is.null(mat_forecast)) {
    warning(sprintf("Material %s não encontrado nos forecasts anuais", cd_mat))
    return(NULL)
  }
  
  # Preparar dados de forecasts
  forecast_data <- tibble()
  
  metodos_anuais <- c("sba", "croston", "tsb", "ses", "naive")
  metodos_disponiveis <- intersect(metodos_anuais, names(mat_forecast$forecasts))
  
  # Ano da previsão (ano seguinte ao último do treino)
  ano_forecast <- max(train_anual$ano) + 1
  
  if (length(metodos_disponiveis) == 0) {
    warning(sprintf("Material %s: nenhum método anual disponível para plotar", cd_mat))
  }
  
  for (metodo in metodos_disponiveis) {
    metodo_data <- mat_forecast$forecasts[[metodo]]
    
    # Validar estrutura e convergência
    if (!is.null(metodo_data) && 
        !is.null(metodo_data$point) && 
        !is.null(metodo_data$convergence) &&
        metodo_data$convergence) {
      
      # Verificar que temos ao menos 1 valor
      if (length(metodo_data$point) > 0) {
        forecast_df <- tibble(
          ano = ano_forecast,
          valor_previsto = metodo_data$point[1],  # Apenas 1 ano à frente
          metodo = toupper(metodo)
        )
        
        forecast_data <- bind_rows(forecast_data, forecast_df)
      }
    }
  }
  
  # Obter info SBC
  info_sbc <- split_origem$sbc_classification %>%
    filter(cd_material == cd_mat)
  
  if (nrow(info_sbc) == 0) {
    info_sbc <- tibble(adi = NA, cv2 = NA)
  }
  
  # Avisar se não há forecasts para plotar
  if (nrow(forecast_data) == 0) {
    warning(sprintf("Material %s: nenhum forecast anual convergiu - plotando apenas valores reais", 
                    cd_mat))
  }
  
  # Validar dados
  if (nrow(train_anual) == 0) {
    warning(sprintf("Material %s sem dados de treino anual", cd_mat))
    return(NULL)
  }
  
  if (nrow(test_anual) == 0) {
    warning(sprintf("Material %s sem dados de teste anual", cd_mat))
    return(NULL)
  }
  
  # Criar gráfico base
  p <- ggplot() +
    # Série histórica anual (treino)
    geom_line(
      data = train_anual,
      aes(x = ano, y = qt_anual),
      color = "steelblue",
      linewidth = 1
    ) +
    geom_point(
      data = train_anual,
      aes(x = ano, y = qt_anual),
      color = "steelblue",
      size = 3,
      alpha = 0.6
    ) +
    # Valor real no teste
    geom_point(
      data = test_anual,
      aes(x = ano, y = qt_anual),
      color = "darkred",
      size = 4
    ) +
    # Linha vertical separando treino/teste
    geom_vline(
      xintercept = max(train_anual$ano) + 0.5,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.5
    )
  
  # Adicionar previsões apenas se houver dados
  if (nrow(forecast_data) > 0) {
    p <- p +
      geom_point(
        data = forecast_data,
        aes(x = ano, y = valor_previsto, color = metodo, shape = metodo),
        size = 3.5
      ) +
      scale_color_nejm() +
      scale_shape_manual(values = c(15, 16, 17, 18, 19))
  }
  
  # Adicionar scales e labels
  p <- p +
    scale_x_continuous(breaks = seq(min(train_anual$ano), 
                                    max(test_anual$ano), 
                                    by = 1)) +
    labs(
      title = sprintf("Categoria: %s | Material: %s", categoria, cd_mat),
      subtitle = sprintf(
        "%s | ADI: %.2f | CV²: %.2f | Horizonte: 1 ano",
        origem_nome,
        info_sbc$adi[1],
        info_sbc$cv2[1]
      ),
      x = "Ano",
      y = "Quantidade Anual Demandada",
      color = "Método",
      shape = "Método"
    ) +
    theme_tufte() +
    theme(
      plot.subtitle = element_text(color = "gray40", size = 9),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# Gerar gráficos anuais
cat("📊 Gerando gráficos anuais...\n\n")

plots_anuais <- list()

for (i in 1:nrow(materiais_exemplo)) {
  
  mat_info <- materiais_exemplo[i, ]
  
  cat(sprintf("   [%d/%d] Plotando %s (%s)...\n", 
              i, nrow(materiais_exemplo),
              mat_info$cd_material, 
              mat_info$categoria))
  
  tryCatch({
    p <- plot_forecast_anual(
      cd_mat = mat_info$cd_material,
      origem_nome = origem_recente,
      categoria = mat_info$categoria
    )
    
    if (!is.null(p)) {
      plots_anuais[[i]] <- p
      cat(sprintf("        ✅ OK\n"))
    } else {
      cat(sprintf("        ⚠️  Retornou NULL\n"))
    }
    
  }, error = function(e) {
    cat(sprintf("        ❌ ERRO: %s\n", conditionMessage(e)))
    plots_anuais[[i]] <- NULL
  })
}

# Criar painéis por categoria
cat("\n📊 Criando painéis anuais por categoria...\n\n")

n_graficos_anuais_salvos <- 0

for (cat_atual in categorias_principais) {
  
  cat(sprintf("   Categoria: %s\n", cat_atual))
  
  indices_categoria <- which(materiais_exemplo$categoria == cat_atual)
  
  if (length(indices_categoria) > 0) {
    
    plots_categoria <- plots_anuais[indices_categoria]
    plots_categoria <- plots_categoria[!sapply(plots_categoria, is.null)]
    
    cat(sprintf("      - Gráficos válidos: %d de %d\n", 
                length(plots_categoria), length(indices_categoria)))
    
    if (length(plots_categoria) > 0) {
      
      painel <- wrap_plots(plots_categoria, ncol = 1) +
        plot_annotation(
          title = sprintf(
            "Previsões Anuais - Categoria %s (h=1 ano)",
            cat_atual
          ),
          subtitle = sprintf(
            "Linha azul: histórico (treino) | Ponto vermelho: real (teste) | Pontos coloridos: previsões (%s)",
            origem_recente
          ),
          theme = theme(
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 11, color = "gray30")
          )
        )
      
      # Salvar
      filename <- sprintf("02_forecast_anual_%s.png", tolower(cat_atual))
      
      ggsave(
        here(config$paths$output$figures, "08_forecast_viz", filename),
        plot = painel,
        width = 14,
        height = 4 * length(plots_categoria),
        dpi = 300
      )
      
      cat(sprintf("      ✅ Salvo: %s\n", filename))
      n_graficos_anuais_salvos <- n_graficos_anuais_salvos + 1
    } else {
      cat(sprintf("      ⚠️  Nenhum gráfico válido - arquivo não criado\n"))
    }
  } else {
    cat(sprintf("      ⚠️  Nenhum material nesta categoria\n"))
  }
  
  cat("\n")
}

cat(sprintf("📊 Total de arquivos anuais salvos: %d de %d categorias\n", 
            n_graficos_anuais_salvos, length(categorias_principais)))

# =============================================================================
# RELATÓRIO FINAL ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL\n")
cat(strrep("=", 70), "\n\n")

cat("✅ VISUALIZAÇÕES GERADAS:\n\n")

cat("📊 PERSPECTIVA MENSAL (h=12 meses):\n")
for (cat in categorias_principais) {
  filename <- sprintf("01_forecast_mensal_%s.png", tolower(cat))
  cat(sprintf("   - %s\n", filename))
}

cat("\n📊 PERSPECTIVA ANUAL (h=1 ano):\n")
for (cat in categorias_principais) {
  filename <- sprintf("02_forecast_anual_%s.png", tolower(cat))
  cat(sprintf("   - %s\n", filename))
}

cat("\n📁 Localização: output/figures/08_forecast_viz/\n")
cat("📐 Resolução: 300 DPI\n")
cat("📋 Materiais por categoria: 3 amostras representativas\n")

cat("\n", strrep("=", 70), "\n\n")

log_message("========================================", "INFO")
log_message("VISUALIZAÇÃO DE FORECASTS CONCLUÍDA", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script 08 finalizado em:", format(Sys.time()), "\n\n")

if(config$computation$parallel) {
  plan(sequential)
}
