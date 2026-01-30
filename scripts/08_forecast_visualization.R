# 08 - VISUALIZAÇÃO DE SÉRIES TEMPORAIS COM PREVISÕES ####
#
# Descrição: Visualização de séries temporais incluindo valores previstos 
#            e valores reais observados no período de teste, com amostras
#            representativas de cada categoria SBC
# Data: 2026-01-28
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

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

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

# Carregar forecasts mensais consolidados
forecasts_mensais <- readRDS(here(config$paths$output$forecasts, 
                                  "consolidated_forecasts.rds"))

# Carregar forecasts anuais
forecasts_anuais <- readRDS(here(config$paths$output$forecasts, 
                                 "annual/forecasts_annual.rds"))

cat("✅ Dados carregados:\n")
cat(sprintf("   - Origens temporais: %d\n", length(splits_list)))
cat(sprintf("   - Forecasts mensais: %d origens\n", 
            length(forecasts_mensais$monthly_perspective)))
cat(sprintf("   - Forecasts anuais: %d origens\n", 
            length(forecasts_anuais)))

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
        n_amostras = 3, 
        criterio = "diversificado"
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
  forecasts_origem <- forecasts_mensais$monthly_perspective[[origem_nome]]
  
  # Encontrar forecast do material
  mat_forecast <- forecasts_origem %>%
    filter(cd_material == cd_mat)
  
  if (nrow(mat_forecast) == 0) {
    warning(sprintf("Material %s não encontrado nos forecasts", cd_mat))
    return(NULL)
  }
  
  # Preparar dados de forecasts para plotagem
  # A estrutura pode variar, adaptar conforme necessário
  forecast_data <- tibble()
  
  # Tentar extrair forecasts de diferentes métodos
  if (!is.null(metodos_plotar)) {
    metodos_disponiveis <- intersect(metodos_plotar, names(mat_forecast))
  } else {
    # Usar métodos principais por padrão
    metodos_principais <- c("SBA", "Croston", "TSB", "SES", "Naive")
    metodos_disponiveis <- intersect(metodos_principais, names(mat_forecast))
  }
  
  for (metodo in metodos_disponiveis) {
    if (metodo %in% names(mat_forecast)) {
      # Assumindo que previsões estão em coluna 'point'
      # e datas em 'data_competencia' ou similar
      
      # Adaptar conforme estrutura real dos dados
      metodo_data <- mat_forecast[[metodo]]
      
      if (is.list(metodo_data) && "point" %in% names(metodo_data)) {
        n_forecast <- length(metodo_data$point)
        
        forecast_df <- tibble(
          data_competencia = seq(
            max(train$data_competencia) %m+% months(1),
            by = "month",
            length.out = n_forecast
          ),
          valor_previsto = metodo_data$point,
          metodo = metodo
        )
        
        forecast_data <- bind_rows(forecast_data, forecast_df)
      }
    }
  }
  
  # Obter informações SBC
  info_sbc <- split_origem$sbc_classification %>%
    filter(cd_material == cd_mat)
  
  # Criar gráfico
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
      data = test,
      aes(x = data_competencia, y = qt_total),
      color = "darkred",
      linewidth = 0.8
    ) +
    geom_point(
      data = test %>% filter(qt_total > 0),
      aes(x = data_competencia, y = qt_total),
      color = "darkred",
      size = 2.5
    ) +
    # Previsões
    geom_line(
      data = forecast_data,
      aes(x = data_competencia, y = valor_previsto, 
          color = metodo, linetype = metodo),
      linewidth = 0.7
    ) +
    # Linha vertical separando treino/teste
    geom_vline(
      xintercept = as.numeric(max(train$data_competencia)),
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.5
    ) +
    # Escalas e labels
    scale_color_nejm() +
    scale_linetype_manual(
      values = c("solid", "dashed", "dotted", "dotdash", "longdash")
    ) +
    labs(
      title = sprintf("Categoria: %s | Material: %s", categoria, cd_mat),
      subtitle = sprintf(
        "%s | ADI: %.2f | CV²: %.2f | Horizonte: 12 meses",
        origem_nome,
        info_sbc$adi,
        info_sbc$cv2
      ),
      x = "Período",
      y = "Quantidade Demandada",
      color = "Método",
      linetype = "Método"
    ) +
    theme_tufte() +
    theme(
      plot.subtitle = element_text(color = "gray40", size = 9),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  return(p)
}

# Gerar gráficos para materiais selecionados
cat("📊 Gerando gráficos individuais...\n")

# Métodos a plotar (selecionar principais)
metodos_principais <- c("SBA", "Croston", "TSB", "SES", "Naive")

plots_mensais <- list()

for (i in 1:nrow(materiais_exemplo)) {
  
  mat_info <- materiais_exemplo[i, ]
  
  cat(sprintf("   - Plotando %s (%s)...\n", 
              mat_info$cd_material, 
              mat_info$categoria))
  
  p <- plot_forecast_mensal(
    cd_mat = mat_info$cd_material,
    origem_nome = origem_recente,
    categoria = mat_info$categoria,
    metodos_plotar = metodos_principais
  )
  
  if (!is.null(p)) {
    plots_mensais[[i]] <- p
  }
}

# Criar painéis por categoria
cat("\n📊 Criando painéis por categoria...\n")

for (cat_atual in categorias_principais) {
  
  indices_categoria <- which(materiais_exemplo$categoria == cat_atual)
  
  if (length(indices_categoria) > 0) {
    
    plots_categoria <- plots_mensais[indices_categoria]
    plots_categoria <- plots_categoria[!sapply(plots_categoria, is.null)]
    
    if (length(plots_categoria) > 0) {
      
      painel <- wrap_plots(plots_categoria, ncol = 1) +
        plot_annotation(
          title = sprintf(
            "Previsões Mensais - Categoria %s (h=12 meses)",
            cat_atual
          ),
          subtitle = sprintf(
            "Linha azul: histórico (treino) | Linha vermelha: real (teste) | Linhas coloridas: previsões (%s)",
            origem_recente
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
      
      cat(sprintf("   ✅ Salvo: %s\n", filename))
    }
  }
}

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
  
  # Agregar teste em anos
  test_anual <- split_origem$test %>%
    filter(cd_material == cd_mat) %>%
    as_tibble() %>%
    mutate(ano = year(data_competencia)) %>%
    group_by(ano) %>%
    summarise(qt_anual = sum(qt_total, na.rm = TRUE), .groups = 'drop')
  
  # Obter forecasts anuais
  forecasts_origem_anual <- forecasts_anuais[[origem_nome]]
  
  # Encontrar forecast do material
  mat_forecast <- NULL
  
  if (!is.null(forecasts_origem_anual$forecasts)) {
    # Procurar material nos forecasts
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
  
  metodos_anuais <- c("SBA", "Croston", "TSB", "SES", "Naive")
  metodos_disponiveis <- intersect(metodos_anuais, names(mat_forecast$forecasts))
  
  for (metodo in metodos_disponiveis) {
    metodo_data <- mat_forecast$forecasts[[metodo]]
    
    if (!is.null(metodo_data$point)) {
      # Horizonte = 1 ano
      ano_forecast <- max(train_anual$ano) + 1
      
      forecast_df <- tibble(
        ano = ano_forecast,
        valor_previsto = metodo_data$point[1],  # Apenas 1 ano à frente
        metodo = metodo
      )
      
      forecast_data <- bind_rows(forecast_data, forecast_df)
    }
  }
  
  # Obter info SBC
  info_sbc <- split_origem$sbc_classification %>%
    filter(cd_material == cd_mat)
  
  # Criar gráfico
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
    # Previsões
    geom_point(
      data = forecast_data,
      aes(x = ano, y = valor_previsto, color = metodo, shape = metodo),
      size = 3.5
    ) +
    # Linha vertical separando treino/teste
    geom_vline(
      xintercept = max(train_anual$ano) + 0.5,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.5
    ) +
    scale_color_nejm() +
    scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
    scale_x_continuous(breaks = seq(min(train_anual$ano), 
                                    max(test_anual$ano), 
                                    by = 1)) +
    labs(
      title = sprintf("Categoria: %s | Material: %s", categoria, cd_mat),
      subtitle = sprintf(
        "%s | ADI: %.2f | CV²: %.2f | Horizonte: 1 ano",
        origem_nome,
        info_sbc$adi,
        info_sbc$cv2
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
cat("📊 Gerando gráficos anuais...\n")

plots_anuais <- list()

for (i in 1:nrow(materiais_exemplo)) {
  
  mat_info <- materiais_exemplo[i, ]
  
  cat(sprintf("   - Plotando %s (%s)...\n", 
              mat_info$cd_material, 
              mat_info$categoria))
  
  p <- plot_forecast_anual(
    cd_mat = mat_info$cd_material,
    origem_nome = origem_recente,
    categoria = mat_info$categoria
  )
  
  if (!is.null(p)) {
    plots_anuais[[i]] <- p
  }
}

# Criar painéis por categoria
cat("\n📊 Criando painéis anuais por categoria...\n")

for (cat_atual in categorias_principais) {
  
  indices_categoria <- which(materiais_exemplo$categoria == cat_atual)
  
  if (length(indices_categoria) > 0) {
    
    plots_categoria <- plots_anuais[indices_categoria]
    plots_categoria <- plots_categoria[!sapply(plots_categoria, is.null)]
    
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
      
      cat(sprintf("   ✅ Salvo: %s\n", filename))
    }
  }
}

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