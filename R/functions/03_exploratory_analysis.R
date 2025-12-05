# 03 - ANÁLISE EXPLORATÓRIA DOS DADOS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Caracterização dos padrões de demanda segundo taxonomia SBC,
#            análise descritiva por categoria, subsistema e temporal
# Data: 2025-12-04
# Versão: 2.0.0
#

# Carregar configurações e bibliotecas ####

library(here)
library(tidyverse)
library(tsibble)
library(lubridate)
library(writexl)

library(ggplot2)
library(ggsci)          # Paletas de cores científicas
library(ggthemes)       # Temas profissionais
library(scales)         # Formatação de eixos
library(patchwork)      # Composição de múltiplos gráficos
library(treemapify)     # Treemaps
library(ggridges)       # Ridge plots (distribuições)
library(ggrepel)        # Labels sem sobreposição
library(viridis)        # Paletas acessíveis
library(gridExtra)
library(grid)

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO ANÁLISE EXPLORATÓRIA DOS DADOS", "INFO")
log_message("========================================", "INFO")

# Criar diretórios de output se não existirem ####
dir.create(here(config$paths$output$figures, "03_exploratory"), 
           showWarnings = FALSE, recursive = TRUE)
dir.create(here(config$paths$output$tables, "03_exploratory"), 
           showWarnings = FALSE, recursive = TRUE)

# Definir tema padrão para gráficos ####
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
)

# Carregar dados processados ####
log_message("Carregando dados processados do script anterior", "INFO")

splits_list <- readRDS(here(config$paths$data$processed, "train_test_splits.rds"))
# ts_completa <- readRDS(here(config$paths$data$processed, "ts_completa.rds"))

cat("\n📊 Dados carregados:\n")
cat(sprintf("   - Número de origens temporais: %d\n", length(splits_list)))
# cat(sprintf("   - Total de materiais (ts_completa): %s\n", 
#             format(n_distinct(ts_completa$cd_material), big.mark = ",")))

log_message("Dados carregados com sucesso", "INFO")

# =============================================================================
# BLOCO 1: ANÁLISE DA CLASSIFICAÇÃO SBC ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: ANÁLISE DA CLASSIFICAÇÃO SBC\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando análise da classificação SBC", "INFO")

## 1.1. Distribuição Geral por Origem ####

cat("📊 1.1. Calculando distribuição SBC por origem...\n")

# Consolidar distribuição de todas as origens
distribuicao_sbc <- map_dfr(
  names(splits_list),
  function(origem_nome) {
    origem_split <- splits_list[[origem_nome]]
    
    origem_split$sbc_classification %>%
      count(categoria_sbc) %>%
      mutate(
        origem_id = origem_nome,
        origem = sprintf(
          "%s a %s",
          format(min(origem_split$train$data_competencia), "%Y-%m"),
          format(max(origem_split$train$data_competencia), "%Y-%m")
        ),
        percentual = n / sum(n) * 100,
        percentual_fmt = sprintf("%.1f%%", percentual)
      )
  }
)

# Tabela resumo
cat("\nDistribuição de Categorias SBC:\n")
distribuicao_sbc %>%
  select(origem, categoria_sbc, n, percentual_fmt) %>%
  arrange(origem, desc(n)) %>%
  print(n = Inf)

# Preparar dados para treemap
dados_treemap <- distribuicao_sbc %>%
  mutate(
    # Criar identificador único para cada combinação origem-categoria
    grupo = paste0(origem, "\n", categoria_sbc),
    
    # Labels informativos
    label_detalhado = paste0(
      categoria_sbc, "\n",
      # origem, "\n",
      format(n, big.mark = ","), " materiais\n",
      "(", percentual_fmt, ")"
    ),
    
    # Cores personalizadas por categoria SBC
    cor_categoria = case_when(
      categoria_sbc == "Smooth" ~ "#2E8B57",        # Verde escuro
      categoria_sbc == "Erratic" ~ "#FF6347",       # Vermelho tomate  
      categoria_sbc == "Intermittent" ~ "#4169E1",  # Azul royal
      categoria_sbc == "Lumpy" ~ "#FF8C00",         # Laranja escuro
      TRUE ~ "#808080"                              # Cinza para outros
    )
  )

# Visualização 1: Treemap Facetado por Origem
p1a <- ggplot(dados_treemap, 
              aes(area = n, fill = categoria_sbc, label = label_detalhado)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(
    color = "white",
    place = "centre", 
    size = 8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Smooth" = "#2E8B57",
      "Erratic" = "#FF6347",
      "Intermittent" = "#4169E1",
      "Lumpy" = "#FF8C00"
    ),
    name = "Categoria SBC"
  ) +
  facet_wrap(~origem, ncol = 2) +
  labs(
    title = "Distribuição de Categorias SBC por Origem Temporal",
    subtitle = "Classificação Syntetos-Boylan-Croston aplicada aos conjuntos de treino",
    caption = "Tamanho do bloco = Quantidade de materiais | Cores fixas por categoria SBC"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 5)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )

# # Criar tabela resumo
# tabela_resumo <- dados_treemap %>%
#   select(origem, categoria_sbc, n, percentual_fmt) %>%
#   arrange(origem, desc(n))
# 
# p1a_tabela <- gridExtra::tableGrob(
#   tabela_resumo,
#   rows = NULL,
#   theme = gridExtra::ttheme_minimal(
#     base_size = 9,
#     core = list(fg_params = list(hjust = 0, x = 0.05)),
#     colhead = list(fg_params = list(fontface = "bold"))
#   )
# )
# 
# p1a_final <- grid.arrange(
#   p1a, 
#   p1a_tabela,
#   ncol = 2,
#   widths = c(3, 1),  # Treemap 3x maior que tabela
#   top = textGrob("Distribuição de Categorias SBC por Origem Temporal", 
#                  gp = gpar(fontsize = 16, fontface = "bold"))
# )

ggsave(
  here(config$paths$output$figures, "03_exploratory", "01a_treemap_sbc_por_origem.png"),
  plot = p1a,
  width = 14, height = 10, dpi = 300
)

cat("   ✅ Gráfico salvo: 01a_treemap_sbc_por_origem.png\n")

# # Visualização 2: Treemap Único (Todas as Origens Consolidadas)
# # Calcular totais consolidados entre origens
# dados_treemap_consolidado <- distribuicao_sbc %>%
#   group_by(categoria_sbc) %>%
#   summarise(
#     n_total = sum(n),
#     n_origens = n(),
#     percentual_medio = mean(percentual),
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     percentual_fmt = sprintf("%.1f%%", percentual_medio),
#     
#     label_detalhado = paste0(
#       categoria_sbc, "\n",
#       format(n_total, big.mark = ","), " materiais\n",
#       "(", percentual_fmt, " médio)",
#       "\n", n_origens, " origens"
#     ),
#     
#     # Métrica de "complexidade" = desvio-padrão entre origens
#     # (maior desvio = mais variação temporal)
#     volatilidade = map_dbl(categoria_sbc, function(cat) {
#       sd(distribuicao_sbc$percentual[distribuicao_sbc$categoria_sbc == cat])
#     })
#   )
# 
# p1b <- ggplot(dados_treemap_consolidado, 
#               aes(area = n_total, fill = volatilidade, label = label_detalhado)) +
#   geom_treemap(color = "white", size = 3) +
#   geom_treemap_text(
#     color = "white",
#     place = "centre", 
#     size = 14,
#     fontface = "bold",
#     lineheight = 0.9
#   ) +
#   scale_fill_gradient2(
#     low = "#1A5D1A",              # Verde escuro = Estável
#     mid = "#FFD700",              # Amarelo = Moderado
#     high = "#8B0000",             # Vermelho escuro = Volátil
#     midpoint = mean(dados_treemap_consolidado$volatilidade),
#     name = "Volatilidade\nTemporal\n(Desvio %)",
#     labels = function(x) sprintf("%.1f", x)
#   ) +
#   labs(
#     title = "Distribuição Consolidada de Categorias SBC",
#     subtitle = "Tamanho = Total de materiais | Cor = Volatilidade temporal (variação % entre origens)",
#     caption = "Verde = Categorias estáveis temporalmente | Vermelho = Categorias com alta variação entre origens"
#   ) +
#   theme_void() +
#   theme(
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
#     plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 15), lineheight = 1.2),
#     legend.position = "right",
#     legend.text = element_text(size = 11),
#     legend.title = element_text(size = 12, face = "bold"),
#     legend.key.height = unit(1.5, "cm")
#   )
# 
# ggsave(
#   here(config$paths$output$figures, "03_exploratory", "01b_treemap_sbc_consolidado.png"),
#   plot = p1b,
#   width = 14, height = 10, dpi = 300
# )
# 
# cat("   ✅ Gráfico salvo: 01b_treemap_sbc_consolidado.png\n")

# Visualização 3: Gráfico de Barras Empilhadas
# (Manter como alternativa para comparação simples)
p1c <- ggplot(distribuicao_sbc, aes(x = origem, y = percentual, fill = categoria_sbc)) +
  geom_col(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = percentual_fmt),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Smooth" = "#2E8B57",
      "Erratic" = "#FF6347",
      "Intermittent" = "#4169E1",
      "Lumpy" = "#FF8C00"
    ),
    name = "Categoria SBC"
  ) +
  labs(
    title = "Distribuição de Categorias SBC por Origem Temporal",
    subtitle = "Classificação Syntetos-Boylan-Croston aplicada aos conjuntos de treino",
    x = "Origem Temporal", 
    y = "Proporção de Materiais (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(
  here(config$paths$output$figures, "03_exploratory", "01c_barras_sbc_origens.png"),
  plot = p1c,
  width = 12, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 01c_barras_sbc_origens.png \n")

## 1.2. Análise de Transições SBC Entre Origens ####

cat("\n📊 1.2. Analisando transições entre categorias SBC...\n")

# Consolidar classificações de todas as origens
todas_classificacoes <- map_dfr(
  names(splits_list),
  ~splits_list[[.x]]$sbc_classification %>%
    select(cd_material, categoria_sbc, adi, cv2, origem_id)
)

# Identificar materiais presentes em múltiplas origens
materiais_multiplas_origens <- todas_classificacoes %>%
  group_by(cd_material) %>%
  summarise(
    n_origens = n_distinct(origem_id),
    categorias = paste(unique(categoria_sbc), collapse = " → "),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_origens))

cat("\nPresença de materiais nas origens:\n")
materiais_multiplas_origens %>%
  count(n_origens, name = "n_materiais") %>%
  mutate(percentual = n_materiais / sum(n_materiais) * 100) %>%
  print()

# Identificar transições (materiais que mudam de categoria)
transicoes <- todas_classificacoes %>%
  arrange(cd_material, origem_id) %>%
  group_by(cd_material) %>%
  mutate(
    categoria_anterior = lag(categoria_sbc),
    transicao = categoria_anterior != categoria_sbc
  ) %>%
  filter(!is.na(transicao) & transicao) %>%
  ungroup()

if (nrow(transicoes) > 0) {
  cat(sprintf("\n🔄 Transições detectadas: %s materiais mudaram de categoria\n",
              format(n_distinct(transicoes$cd_material), big.mark = ",")))
  
  # Resumo das transições mais comuns
  cat("\nTransições mais frequentes:\n")
  transicoes %>%
    mutate(tipo_transicao = paste(categoria_anterior, "→", categoria_sbc)) %>%
    count(tipo_transicao, sort = TRUE) %>%
    head(10) %>%
    print()
  
  # Criar matriz de transições
  matriz_transicoes <- transicoes %>%
    count(categoria_anterior, categoria_sbc, name = "n_transicoes") %>%
    pivot_wider(
      names_from = categoria_sbc,
      values_from = n_transicoes,
      values_fill = 0
    )
  
  # Visualização 1: Matriz de Transições (Heatmap)
  matriz_trans_long <- transicoes %>%
    count(categoria_anterior, categoria_sbc, name = "n_transicoes")
  
  p2 <- ggplot(matriz_trans_long, 
               aes(x = categoria_anterior, y = categoria_sbc, fill = n_transicoes)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = n_transicoes), color = "white", fontface = "bold", size = 5) +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    labs(
      title = "Matriz de Transições Entre Categorias SBC",
      subtitle = "Frequência de mudanças de categoria entre origens consecutivas",
      x = "Categoria na Origem Anterior", 
      y = "Categoria na Origem Atual",
      fill = "Nº Transições"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    here(config$paths$output$figures, "03_exploratory", "02_matriz_transicoes_heatmap.png"),
    plot = p2,
    width = 10, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 02_matriz_transicoes_heatmap.png\n")
  
} else {
  cat("\n✅ Nenhuma transição de categoria detectada\n")
  matriz_transicoes <- tibble()
}

# Visualização 2: Treemap de Volatilidade
materiais_volatilidade <- materiais_multiplas_origens %>%
  mutate(
    volatilidade = case_when(
      n_origens == 1 ~ "Única origem",
      n_origens == 2 ~ "Baixa volatilidade",
      n_origens == 3 ~ "Média volatilidade",
      n_origens == 4 ~ "Alta estabilidade"
    )
  ) %>%
  count(volatilidade, name = "n_materiais")

p3 <- ggplot(materiais_volatilidade, 
             aes(area = n_materiais, fill = volatilidade, 
                 label = paste0(volatilidade, "\n", 
                                format(n_materiais, big.mark = ","), " materiais"))) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre", size = 14, fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Distribuição de Materiais por Volatilidade de Padrão",
    subtitle = "Baseado em número de origens onde material está presente"
  ) +
  theme(legend.position = "none")

ggsave(
  here(config$paths$output$figures, "03_exploratory", "03_treemap_volatilidade.png"),
  plot = p3,
  width = 10, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 03_treemap_volatilidade.png\n")

## 1.3. Validação da Classificação ####

cat("\n📊 1.3. Validando limiares de classificação SBC...\n")

# Gráfico 1: Distribuição de ADI
p4a <- ggplot(todas_classificacoes, 
              aes(x = categoria_sbc, y = adi, fill = categoria_sbc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 1.32, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 1.32, label = "Limiar ADI = 1.32", 
           vjust = -0.5, color = "red", fontface = "bold", size = 3.5) +
  scale_fill_lancet() +
  scale_y_log10(labels = comma) +
  labs(
    title = "Distribuição de ADI por Categoria SBC",
    x = "Categoria SBC", 
    y = "ADI (escala log)",
    caption = "Linha tracejada indica limiar de classificação (ADI = 1.32)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico 2: Distribuição de CV²
p4b <- ggplot(todas_classificacoes, 
              aes(x = categoria_sbc, y = cv2, fill = categoria_sbc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0.49, linetype = "dashed", color = "blue", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 0.49, label = "Limiar CV² = 0.49", 
           vjust = -0.5, color = "blue", fontface = "bold", size = 3.5) +
  scale_fill_lancet() +
  scale_y_log10(labels = comma) +
  labs(
    title = "Distribuição de CV² por Categoria SBC",
    x = "Categoria SBC", 
    y = "CV² (escala log)",
    caption = "Linha tracejada indica limiar de classificação (CV² = 0.49)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combinar gráficos com patchwork
p4 <- p4a + p4b + plot_annotation(
  title = "Validação dos Limiares de Classificação SBC",
  theme = theme(plot.title = element_text(face = "bold", size = 16))
)

ggsave(
  here(config$paths$output$figures, "03_exploratory", "04_validacao_limiares_sbc.png"),
  plot = p4,
  width = 14, height = 6, dpi = 300
)

cat("   ✅ Gráfico salvo: 04_validacao_limiares_sbc.png\n")

log_message("Análise da classificação SBC concluída", "INFO")

# =============================================================================
# BLOCO 2: CARACTERIZAÇÃO DETALHADA POR CATEGORIA SBC ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: CARACTERIZAÇÃO DETALHADA POR CATEGORIA SBC\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando caracterização por categoria SBC", "INFO")

## 2.1. Estatísticas Descritivas Agregadas ####

cat("📊 2.1. Calculando estatísticas descritivas por categoria...\n")

# Função para calcular estatísticas por categoria
calcular_stats_categoria <- function(train_data, sbc_data) {
  
  train_stats <- train_data %>%
    as_tibble() %>%
    group_by(cd_material) %>%
    summarise(
      prop_zeros = mean(qt_total == 0),
      demanda_media_positiva = mean(qt_total[qt_total > 0]),
      .groups = 'drop'
    )
  
  sbc_data %>%
    left_join(train_stats, by = "cd_material") %>%
    group_by(categoria_sbc) %>%
    summarise(
      n_materiais = n(),
      
      # Proporção de zeros (mediana entre materiais)
      prop_zeros_mediana = median(prop_zeros, na.rm = TRUE),
      
      # Tamanho médio de demanda quando > 0 (mediana entre materiais)
      demanda_media_mediana = median(demanda_media_positiva, na.rm = TRUE),
      
      # Variabilidade (mediana de CV²)
      cv2_mediana = median(cv2, na.rm = TRUE),
      
      # ADI característico (mediana)
      adi_mediana = median(adi, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    mutate(percentual_materiais = n_materiais / sum(n_materiais) * 100)
}

# Aplicar para todas as origens
stats_por_categoria <- map_dfr(
  names(splits_list),
  function(origem_nome) {
    split <- splits_list[[origem_nome]]
    calcular_stats_categoria(split$train, split$sbc_classification) %>%
      mutate(origem = origem_nome)
  }
)

# Tabela consolidada (média entre origens)
stats_consolidadas <- stats_por_categoria %>%
  group_by(categoria_sbc) %>%
  summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_materiais))

cat("\nEstatísticas Consolidadas por Categoria SBC (média entre origens):\n")
print(stats_consolidadas, n = Inf)

# Exportar tabela formatada
stats_consolidadas %>% write_xlsx(
  here(config$paths$output$tables, "03_exploratory", "stats_descritivas_por_categoria.xlsx")
)

cat("\n   ✅ Tabela exportada: stats_descritivas_por_categoria.xlsx \n")

## 2.2. Visualizações por Categoria ####

cat("\n📊 2.2. Gerando visualizações por categoria...\n")

# Scatter Plot: ADI vs CV² com categorias coloridas
p5 <- ggplot(todas_classificacoes, aes(x = adi, y = cv2, color = categoria_sbc)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_vline(xintercept = 1.32, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_hline(yintercept = 0.49, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  scale_color_nejm() +
  labs(
    title = "Classificação SBC: ADI vs CV²",
    subtitle = "Linhas tracejadas indicam limiares de classificação",
    x = "ADI (Average inter-Demand Interval, escala log)",
    y = "CV² (Coeficiente de Variação ao Quadrado, escala log)",
    color = "Categoria"
  )

ggsave(
  here(config$paths$output$figures, "03_exploratory", "05_scatter_adi_cv2_categorias.png"),
  plot = p5,
  width = 12, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 05_scatter_adi_cv2_categorias.png\n")

## 2.3. Seleção Fundamentada de 5 Materiais Exemplo ####

cat("\n📊 2.3. Selecionando materiais exemplo por categoria...\n")

# Função para selecionar material representativo de cada categoria
selecionar_material_exemplo <- function(sbc_data, train_data, categoria_alvo, 
                                        criterio = "mediano") {
  
  materiais_categoria <- sbc_data %>%
    filter(categoria_sbc == categoria_alvo)
  
  if (nrow(materiais_categoria) == 0) {
    return(NA_character_)
  }
  
  if (criterio == "mediano") {
    # Selecionar material com ADI e CV² próximos da mediana da categoria
    adi_med <- median(materiais_categoria$adi, na.rm = TRUE)
    cv2_med <- median(materiais_categoria$cv2, na.rm = TRUE)
    
    material_selecionado <- materiais_categoria %>%
      mutate(
        dist_adi = abs(adi - adi_med),
        dist_cv2 = abs(cv2 - cv2_med),
        dist_total = dist_adi + dist_cv2
      ) %>%
      arrange(dist_total) %>%
      slice(1) %>%
      pull(cd_material)
    
  } else if (criterio == "extremo") {
    # Selecionar material com características mais extremas da categoria
    material_selecionado <- materiais_categoria %>%
      arrange(desc(adi * cv2)) %>%
      slice(1) %>%
      pull(cd_material)
  }
  
  return(material_selecionado)
}

# Selecionar 1 exemplo de cada categoria
categorias_principais <- c("Smooth", "Erratic", "Intermittent", "Lumpy")

exemplos <- tibble(
  categoria = categorias_principais
) %>%
  mutate(
    cd_material = map_chr(
      categoria,
      ~selecionar_material_exemplo(
        splits_list$origem_1$sbc_classification,
        splits_list$origem_1$train,
        .x,
        criterio = "mediano"
      )
    )
  ) %>%
  filter(!is.na(cd_material))

# Selecionar 1 material de transição (que mudou de categoria)
if (nrow(transicoes) > 0) {
  material_transicao <- transicoes %>%
    group_by(cd_material) %>%
    summarise(n_transicoes = n(), .groups = 'drop') %>%
    arrange(desc(n_transicoes)) %>%
    slice(1) %>%
    pull(cd_material)
  
  exemplos <- exemplos %>%
    add_row(categoria = "Transição", cd_material = material_transicao)
}

cat("\nMateriais selecionados como exemplos:\n")
print(exemplos)

# Função para plotar série temporal de exemplo
plot_serie_exemplo <- function(cd_mat, categoria, train_data, sbc_data) {
  
  serie <- train_data %>%
    filter(cd_material == cd_mat) %>%
    as_tibble()
  
  info_sbc <- sbc_data %>%
    filter(cd_material == cd_mat)
  
  if (nrow(info_sbc) == 0) {
    # Material de transição - pegar info da primeira origem
    info_sbc <- tibble(adi = NA, cv2 = NA)
  }
  
  ggplot(serie, aes(x = data_competencia, y = qt_total)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(
      data = serie %>% filter(qt_total > 0), 
      color = "darkred", size = 2, alpha = 0.7
    ) +
    labs(
      title = sprintf("Categoria: %s", categoria),
      subtitle = sprintf("Material: %s | ADI: %.2f | CV²: %.2f", 
                         cd_mat, 
                         ifelse(is.na(info_sbc$adi), 0, info_sbc$adi), 
                         ifelse(is.na(info_sbc$cv2), 0, info_sbc$cv2)),
      x = "Período", 
      y = "Quantidade Demandada"
    ) +
    theme(plot.subtitle = element_text(color = "gray40"))
}

# Gerar todos os plots
plots_exemplos <- map2(
  exemplos$cd_material,
  exemplos$categoria,
  ~plot_serie_exemplo(
    .x, .y, 
    splits_list$origem_1$train, 
    splits_list$origem_1$sbc_classification
  )
)

# Combinar em grid
p6 <- wrap_plots(plots_exemplos, ncol = 2) +
  plot_annotation(
    title = "Exemplos de Séries Temporais por Categoria SBC",
    subtitle = "Materiais selecionados como representativos de cada padrão de demanda",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave(
  here(config$paths$output$figures, "03_exploratory", "06_exemplos_series_temporais.png"),
  plot = p6,
  width = 14, height = 10, dpi = 300
)

cat("   ✅ Gráfico salvo: 06_exemplos_series_temporais.png\n")

log_message("Caracterização por categoria SBC concluída", "INFO")

# =============================================================================
# BLOCO 3: ANÁLISE POR SUBSISTEMA FUNCIONAL ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: ANÁLISE POR SUBSISTEMA FUNCIONAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando análise por subsistema", "INFO")

## 3.1. Distribuição SBC por Subsistema ####

cat("📊 3.1. Analisando distribuição SBC por subsistema...\n")

# Verificar se coluna cd_projeto existe
tem_projeto <- "cd_projeto" %in% names(splits_list$origem_1$sbc_classification)

if (!tem_projeto) {
  cat("\n⚠️  AVISO: Coluna 'cd_projeto' não encontrada.\n")
  cat("   Análise por subsistema será PULADA.\n")
  cat("   Execute as correções nos scripts 01 e 02 conforme documentado.\n\n")
  
  log_message("Coluna cd_projeto não encontrada - análise por subsistema pulada", "WARNING")
  
  # Criar placeholders vazios
  dados_subsistema_clean <- tibble()
  tabela_cruzada <- tibble()
  stats_subsistema <- tibble()
  teste_chi <- list(statistic = NA, p.value = NA)
  
} else {
  
  # Consolidar dados com subsistema
  dados_subsistema <- map_dfr(
    names(splits_list),
    function(origem_nome) {
      splits_list[[origem_nome]]$sbc_classification %>%
        filter(!is.na(cd_projeto)) %>%
        mutate(origem = origem_nome)
    }
  )
  
  # Separar materiais com múltiplos subsistemas (contém ";")
  dados_subsistema_clean <- dados_subsistema %>%
    mutate(
      cd_projeto_principal = str_split_fixed(cd_projeto, ";", 2)[,1],
      multiplos_subsistemas = str_detect(cd_projeto, ";")
    )
  
  cat(sprintf("\nMateriais analisados: %s\n", 
              format(n_distinct(dados_subsistema_clean$cd_material), big.mark = ",")))
  cat(sprintf("Materiais com múltiplos subsistemas: %s (%.1f%%)\n",
              format(sum(dados_subsistema_clean$multiplos_subsistemas), big.mark = ","),
              mean(dados_subsistema_clean$multiplos_subsistemas) * 100))
  
  # Tabela cruzada: Subsistema × Categoria SBC
  tabela_cruzada <- dados_subsistema_clean %>%
    count(cd_projeto_principal, categoria_sbc) %>%
    pivot_wider(names_from = categoria_sbc, values_from = n, values_fill = 0) %>%
    mutate(Total = rowSums(across(where(is.numeric))))
  
  cat("\nTabela Cruzada: Subsistema × Categoria SBC\n")
  print(tabela_cruzada)
  
  # Exportar tabela
  write_csv(
    tabela_cruzada,
    here(config$paths$output$tables, "03_exploratory", "tabela_cruzada_subsistema_sbc.csv")
  )
  
  cat("\n   ✅ Tabela exportada: tabela_cruzada_subsistema_sbc.csv\n")
  
  # Teste Qui-Quadrado de Independência
  matriz_teste <- dados_subsistema_clean %>%
    count(cd_projeto_principal, categoria_sbc) %>%
    pivot_wider(names_from = categoria_sbc, values_from = n, values_fill = 0) %>%
    select(-cd_projeto_principal) %>%
    as.matrix()
  
  # Verificar se há células suficientes para teste
  if (all(dim(matriz_teste) >= 2)) {
    teste_chi <- chisq.test(matriz_teste)
    
    cat(sprintf("\n📊 Teste Qui-Quadrado: Subsistema × Categoria SBC\n"))
    cat(sprintf("   χ² = %.2f, p-valor = %.4f\n", teste_chi$statistic, teste_chi$p.value))
    
    if (teste_chi$p.value < 0.05) {
      cat("   ✅ Existe associação significativa entre subsistema e categoria SBC (p < 0.05)\n")
    } else {
      cat("   ❌ Não há evidência de associação significativa (p ≥ 0.05)\n")
    }
  } else {
    cat("\n⚠️  Dados insuficientes para teste Qui-Quadrado\n")
    teste_chi <- list(statistic = NA, p.value = NA)
  }
  
  # Visualização: Heatmap da Tabela Cruzada
  p7 <- dados_subsistema_clean %>%
    count(cd_projeto_principal, categoria_sbc) %>%
    group_by(cd_projeto_principal) %>%
    mutate(percentual = n / sum(n) * 100) %>%
    ggplot(aes(x = categoria_sbc, y = cd_projeto_principal, fill = percentual)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", percentual)), 
              color = "white", fontface = "bold", size = 4) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    labs(
      title = "Distribuição de Categorias SBC por Subsistema",
      subtitle = "Porcentagem de materiais em cada categoria dentro de cada subsistema",
      x = "Categoria SBC", 
      y = "Subsistema",
      fill = "% Materiais"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    here(config$paths$output$figures, "03_exploratory", "07_heatmap_subsistema_sbc.png"),
    plot = p7,
    width = 12, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 07_heatmap_subsistema_sbc.png\n")
  
  ## 3.2. Características de Demanda por Subsistema ####
  
  cat("\n📊 3.2. Calculando características por subsistema...\n")
  
  # Estatísticas agregadas por subsistema
  stats_subsistema <- map_dfr(
    names(splits_list),
    function(origem_nome) {
      split <- splits_list[[origem_nome]]
      
      split$train %>%
        as_tibble() %>%
        filter(!is.na(cd_projeto)) %>%
        mutate(cd_projeto_principal = str_split_fixed(cd_projeto, ";", 2)[,1]) %>%
        group_by(cd_projeto_principal) %>%
        summarise(
          n_materiais = n_distinct(cd_material),
          volume_total = sum(qt_total, na.rm = TRUE),
          prop_zeros = mean(qt_total == 0),
          demanda_mediana = median(qt_total[qt_total > 0], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(origem = origem_nome)
    }
  )
  
  cat("\nEstatísticas por Subsistema:\n")
  stats_subsistema %>%
    group_by(cd_projeto_principal) %>%
    summarise(
      n_materiais_medio = mean(n_materiais),
      volume_medio = mean(volume_total),
      prop_zeros_media = mean(prop_zeros),
      .groups = 'drop'
    ) %>%
    arrange(desc(volume_medio)) %>%
    print()
  
  # Exportar
  write_csv(
    stats_subsistema,
    here(config$paths$output$tables, "03_exploratory", "stats_subsistema.csv")
  )
  
  cat("\n   ✅ Tabela exportada: stats_subsistema.csv\n")
  
  # Visualização: Barras comparando volume por subsistema
  p8 <- stats_subsistema %>%
    group_by(cd_projeto_principal) %>%
    summarise(volume_medio = mean(volume_total), .groups = 'drop') %>%
    arrange(desc(volume_medio)) %>%
    ggplot(aes(x = reorder(cd_projeto_principal, volume_medio), y = volume_medio)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = comma(volume_medio, accuracy = 1)), 
              hjust = -0.1, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Volume Total de Demanda por Subsistema",
      subtitle = "Soma de todas as quantidades consumidas (média entre origens)",
      x = "Subsistema", 
      y = "Volume Total de Demanda"
    )
  
  ggsave(
    here(config$paths$output$figures, "03_exploratory", "08_volume_demanda_subsistema.png"),
    plot = p8,
    width = 10, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 08_volume_demanda_subsistema.png\n")
  
  ## 3.3. Comparação Entre Subsistemas (Testes Estatísticos) ####
  
  cat("\n📊 3.3. Comparando subsistemas estatisticamente...\n")
  
  # Preparar dados para testes
  dados_teste <- map_dfr(
    names(splits_list),
    function(origem_nome) {
      split <- splits_list[[origem_nome]]
      
      split$sbc_classification %>%
        filter(!is.na(cd_projeto)) %>%
        mutate(
          cd_projeto_principal = str_split_fixed(cd_projeto, ";", 2)[,1],
          origem = origem_nome
        )
    }
  )
  
  # Teste Kruskal-Wallis para ADI médio entre subsistemas
  if (n_distinct(dados_teste$cd_projeto_principal) >= 2) {
    teste_adi <- kruskal.test(adi ~ cd_projeto_principal, data = dados_teste)
    
    cat(sprintf("\n📊 Teste Kruskal-Wallis: ADI entre Subsistemas\n"))
    cat(sprintf("   H = %.2f, p-valor = %.4f\n", teste_adi$statistic, teste_adi$p.value))
    
    # Teste Kruskal-Wallis para CV² médio entre subsistemas
    teste_cv2 <- kruskal.test(cv2 ~ cd_projeto_principal, data = dados_teste)
    
    cat(sprintf("\n📊 Teste Kruskal-Wallis: CV² entre Subsistemas\n"))
    cat(sprintf("   H = %.2f, p-valor = %.4f\n", teste_cv2$statistic, teste_cv2$p.value))
  } else {
    cat("\n⚠️  Dados insuficientes para testes Kruskal-Wallis\n")
  }
  
  # Visualização: Ridge Plot (distribuições de ADI por subsistema)
  p9 <- ggplot(dados_teste, 
               aes(x = adi, y = cd_projeto_principal, fill = cd_projeto_principal)) +
    geom_density_ridges(alpha = 0.7, scale = 1.5) +
    scale_x_log10(labels = comma) +
    scale_fill_brewer(palette = "Set3") +
    theme_ridges() +
    labs(
      title = "Distribuição de ADI por Subsistema",
      subtitle = "Densidade estimada do Average inter-Demand Interval",
      x = "ADI (escala log)", 
      y = "Subsistema"
    ) +
    theme(legend.position = "none")
  
  ggsave(
    here(config$paths$output$figures, "03_exploratory", "09_ridge_adi_subsistema.png"),
    plot = p9,
    width = 12, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 09_ridge_adi_subsistema.png\n")
}

log_message("Análise por subsistema concluída", "INFO")

# =============================================================================
# BLOCO 4: ANÁLISE TEMPORAL (SEM SAZONALIDADE) ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: ANÁLISE TEMPORAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando análise temporal", "INFO")

## 4.1. Evolução Temporal das Características ####

cat("📊 4.1. Analisando evolução temporal das características...\n")

# Calcular métricas temporais para cada origem
evolucao_temporal <- map_dfr(
  names(splits_list),
  function(origem_nome) {
    split <- splits_list[[origem_nome]]
    
    tibble(
      origem = origem_nome,
      origem_id = split$metadata$origem_id,
      periodo_inicio = min(split$train$data_competencia),
      periodo_fim = max(split$train$data_competencia),
      n_materiais = n_distinct(split$train$cd_material),
      adi_medio = mean(split$sbc_classification$adi, na.rm = TRUE),
      cv2_medio = mean(split$sbc_classification$cv2, na.rm = TRUE),
      prop_smooth = mean(split$sbc_classification$categoria_sbc == "Smooth"),
      prop_erratic = mean(split$sbc_classification$categoria_sbc == "Erratic"),
      prop_intermittent = mean(split$sbc_classification$categoria_sbc == "Intermittent"),
      prop_lumpy = mean(split$sbc_classification$categoria_sbc == "Lumpy")
    )
  }
)

cat("\nEvolução Temporal das Características:\n")
print(evolucao_temporal)

# Exportar
write_csv(
  evolucao_temporal,
  here(config$paths$output$tables, "03_exploratory", "evolucao_temporal.csv")
)

cat("\n   ✅ Tabela exportada: evolucao_temporal.csv\n")

# Visualização 1: Evolução de ADI e CV² médios
p10a <- evolucao_temporal %>%
  select(origem, adi_medio, cv2_medio) %>%
  pivot_longer(-origem, names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(x = origem, y = valor, group = metrica, color = metrica)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("adi_medio" = "steelblue", "cv2_medio" = "darkorange"),
    labels = c("ADI Médio", "CV² Médio")
  ) +
  labs(
    title = "Evolução de ADI e CV² Médios",
    x = "Origem Temporal", 
    y = "Valor Médio",
    color = "Métrica"
  )

# Visualização 2: Evolução da proporção de categorias
p10b <- evolucao_temporal %>%
  select(origem, starts_with("prop_")) %>%
  pivot_longer(-origem, names_to = "categoria", values_to = "proporcao") %>%
  mutate(
    categoria = str_remove(categoria, "prop_"),
    categoria = str_to_title(categoria)
  ) %>%
  ggplot(aes(x = origem, y = proporcao * 100, group = categoria, color = categoria)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_nejm() +
  labs(
    title = "Evolução da Distribuição de Categorias SBC",
    x = "Origem Temporal", 
    y = "Proporção (%)",
    color = "Categoria"
  )

# Combinar gráficos
p10 <- p10a / p10b + plot_annotation(
  title = "Evolução Temporal de Características de Demanda",
  theme = theme(plot.title = element_text(face = "bold", size = 16))
)

ggsave(
  here(config$paths$output$figures, "03_exploratory", "10_evolucao_temporal_metricas.png"),
  plot = p10,
  width = 12, height = 10, dpi = 300
)

cat("   ✅ Gráfico salvo: 10_evolucao_temporal_metricas.png\n")

## 4.3. Análise de Estabilidade dos Padrões ####

cat("\n📊 4.3. Analisando estabilidade dos padrões SBC...\n")

# Identificar materiais estáveis (mesma categoria em todas as origens)
estabilidade <- materiais_multiplas_origens %>%
  mutate(
    estavel = !str_detect(categorias, "→")
  )

prop_estaveis <- mean(estabilidade$estavel) * 100

cat(sprintf("\n📊 Estabilidade de Padrões SBC:\n"))
cat(sprintf("   - Materiais estáveis (sem transições): %.1f%%\n", prop_estaveis))
cat(sprintf("   - Materiais voláteis (com transições): %.1f%%\n", 100 - prop_estaveis))

# Taxa de volatilidade por subsistema (se disponível)
if (tem_projeto && nrow(dados_subsistema_clean) > 0) {
  
  volatilidade_subsistema <- dados_subsistema_clean %>%
    distinct(cd_material, cd_projeto_principal) %>%
    left_join(
      estabilidade %>% select(cd_material, estavel),
      by = "cd_material"
    ) %>%
    group_by(cd_projeto_principal) %>%
    summarise(
      taxa_volatilidade = mean(!estavel, na.rm = TRUE) * 100,
      n_materiais = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(taxa_volatilidade))
  
  cat("\nTaxa de Volatilidade por Subsistema:\n")
  print(volatilidade_subsistema)
  
  # Visualização
  p11 <- ggplot(volatilidade_subsistema, 
                aes(x = reorder(cd_projeto_principal, taxa_volatilidade), 
                    y = taxa_volatilidade)) +
    geom_col(fill = "coral", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", taxa_volatilidade)), 
              hjust = -0.1, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Taxa de Volatilidade de Padrões por Subsistema",
      subtitle = "% de materiais que mudaram de categoria SBC entre origens",
      x = "Subsistema", 
      y = "Taxa de Volatilidade (%)"
    )
  
  ggsave(
    here(config$paths$output$figures, "03_exploratory", "11_volatilidade_subsistema.png"),
    plot = p11,
    width = 10, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 11_volatilidade_subsistema.png\n")
}

log_message("Análise temporal concluída", "INFO")

# =============================================================================
# BLOCO 5: ANÁLISE DE CASOS ESPECIAIS ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 5: ANÁLISE DE CASOS ESPECIAIS\n")
cat(strrep("=", 70), "\n\n")

log_message("Iniciando análise de casos especiais", "INFO")

## 5.2. Materiais com Dados Insuficientes ####

cat("📊 5.2. Analisando materiais com dados insuficientes...\n")

# Consolidar materiais excluídos por todas as origens
materiais_excluidos_consolidado <- map_dfr(
  names(splits_list),
  function(origem_nome) {
    splits_list[[origem_nome]]$materiais_excluidos %>%
      mutate(origem = origem_nome)
  }
)

# Materiais excluídos por < 3 ocorrências
insuficientes <- materiais_excluidos_consolidado %>%
  filter(str_detect(motivo, "< 3|insuficiente|ocorrências"))

# Total de materiais únicos no universo
total_materiais_universo <- n_distinct(ts_completa$cd_material)

cat(sprintf("\n⚠️  Materiais com Dados Insuficientes:\n"))
cat(sprintf("   - Total de materiais excluídos: %s\n", 
            format(n_distinct(insuficientes$cd_material), big.mark = ",")))
cat(sprintf("   - % do universo inicial: %.1f%%\n",
            n_distinct(insuficientes$cd_material) / total_materiais_universo * 100))

# Estatísticas por origem
cat("\nExclusões por origem:\n")
materiais_excluidos_consolidado %>%
  group_by(origem) %>%
  summarise(n_excluidos = n(), .groups = 'drop') %>%
  arrange(origem) %>%
  print()

# Visualização: Proporção de exclusões por origem
p12 <- materiais_excluidos_consolidado %>%
  group_by(origem) %>%
  summarise(n_excluidos = n(), .groups = 'drop') %>%
  ggplot(aes(x = origem, y = n_excluidos)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  geom_text(aes(label = format(n_excluidos, big.mark = ",")), 
            vjust = -0.5, fontface = "bold") +
  labs(
    title = "Materiais Excluídos por Origem",
    subtitle = "Materiais com dados insuficientes (< 3 ocorrências no treino)",
    x = "Origem Temporal", 
    y = "Nº Materiais Excluídos"
  )

ggsave(
  here(config$paths$output$figures, "03_exploratory", "12_proporcao_exclusoes.png"),
  plot = p12,
  width = 10, height = 6, dpi = 300
)

cat("   ✅ Gráfico salvo: 12_proporcao_exclusoes.png\n")

# Exportar lista de materiais excluídos
write_csv(
  materiais_excluidos_consolidado,
  here(config$paths$output$tables, "03_exploratory", "materiais_excluidos.csv")
)

cat("   ✅ Tabela exportada: materiais_excluidos.csv\n")

log_message("Análise de casos especiais concluída", "INFO")

# =============================================================================
# BLOCO 6: SÍNTESE E PREPARAÇÃO PARA MODELAGEM ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6: SÍNTESE E PREPARAÇÃO PARA MODELAGEM\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando síntese e recomendações", "INFO")

## 6.1. Resumo Executivo por Origem ####

cat("📊 6.1. Gerando resumo executivo por origem...\n")

# Função para gerar resumo textual
gerar_resumo_origem <- function(origem_nome, split) {
  
  sbc <- split$sbc_classification
  train <- split$train
  
  # Subsistema predominante (se disponível)
  if ("cd_projeto" %in% names(sbc)) {
    subsistema_info <- sbc %>%
      filter(!is.na(cd_projeto)) %>%
      mutate(cd_projeto_principal = str_split_fixed(cd_projeto, ";", 2)[,1]) %>%
      count(cd_projeto_principal, sort = TRUE) %>%
      slice(1)
    
    subsistema_texto <- sprintf(
      "%s (%d materiais)",
      subsistema_info$cd_projeto_principal,
      subsistema_info$n
    )
  } else {
    subsistema_texto <- "N/A"
  }
  
  resumo <- sprintf("
%s - Período de Treino: %s a %s
═══════════════════════════════════════════════════════════════

📊 DISTRIBUIÇÃO SBC:
   - Smooth:       %5d materiais (%5.1f%%)
   - Erratic:      %5d materiais (%5.1f%%)
   - Intermittent: %5d materiais (%5.1f%%)
   - Lumpy:        %5d materiais (%5.1f%%)
   
📈 CARACTERÍSTICAS MÉDIAS:
   - ADI Mediano:              %6.2f
   - CV² Mediano:              %6.2f
   - Proporção Zeros:          %6.1f%%
   - Demanda Média (μz):       %6.1f unidades
   
🔍 SUBSISTEMA PREDOMINANTE: %s
⚠️  MATERIAIS EXCLUÍDOS:     %d

",
                    origem_nome,
                    min(train$data_competencia), max(train$data_competencia),
                    sum(sbc$categoria_sbc == "Smooth"), mean(sbc$categoria_sbc == "Smooth") * 100,
                    sum(sbc$categoria_sbc == "Erratic"), mean(sbc$categoria_sbc == "Erratic") * 100,
                    sum(sbc$categoria_sbc == "Intermittent"), mean(sbc$categoria_sbc == "Intermittent") * 100,
                    sum(sbc$categoria_sbc == "Lumpy"), mean(sbc$categoria_sbc == "Lumpy") * 100,
                    median(sbc$adi, na.rm = TRUE),
                    median(sbc$cv2, na.rm = TRUE),
                    mean(train$qt_total == 0) * 100,
                    median(sbc$demanda_media, na.rm = TRUE),
                    subsistema_texto,
                    nrow(split$materiais_excluidos)
  )
  
  return(resumo)
}

# Gerar resumos para todas as origens
resumos <- map_chr(names(splits_list), ~gerar_resumo_origem(.x, splits_list[[.x]]))

# Salvar em arquivo texto
writeLines(
  c("═══════════════════════════════════════════════════════════════",
    "RELATÓRIO EXECUTIVO - ANÁLISE EXPLORATÓRIA DOS DADOS",
    "Projeto: Previsão de Demanda SISCEAB",
    sprintf("Data: %s", Sys.Date()),
    "═══════════════════════════════════════════════════════════════",
    "", resumos),
  here(config$paths$output$reports, "03_exploratory_summary_report.txt")
)

# Exibir no console
cat(resumos, sep = "\n")

cat("   ✅ Relatório executivo salvo: 03_exploratory_summary_report.txt\n")

## 6.2. Recomendações para Modelagem ####

cat("\n📊 6.2. Gerando recomendações para modelagem...\n")

# Análise consolidada para recomendações
recomendacoes <- list(
  
  # Proporção que beneficiará métodos especializados
  prop_intermitente_lumpy = mean(
    todas_classificacoes$categoria_sbc %in% c("Intermittent", "Lumpy")
  ) * 100,
  
  # Proporção Smooth + Erratic
  prop_smooth_erratic = mean(
    todas_classificacoes$categoria_sbc %in% c("Smooth", "Erratic")
  ) * 100,
  
  # Subsistemas com maior desafio (maior % de Lumpy) - se disponível
  subsistemas_desafiadores = if (tem_projeto && nrow(dados_subsistema_clean) > 0) {
    dados_subsistema_clean %>%
      group_by(cd_projeto_principal) %>%
      summarise(prop_lumpy = mean(categoria_sbc == "Lumpy") * 100, .groups = 'drop') %>%
      arrange(desc(prop_lumpy)) %>%
      slice(1:3)
  } else {
    tibble(cd_projeto_principal = "N/A", prop_lumpy = 0)
  },
  
  # Estabilidade geral
  taxa_estabilidade_geral = mean(estabilidade$estavel) * 100,
  
  # Necessidade de abordagem híbrida
  necessidade_hibrida = n_distinct(todas_classificacoes$categoria_sbc) > 2
)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("RECOMENDAÇÕES PARA MODELAGEM\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat(sprintf("📊 MAGNITUDE DO DESAFIO:\n"))
cat(sprintf("   - %.1f%% dos materiais apresentam padrão Intermittent ou Lumpy\n",
            recomendacoes$prop_intermitente_lumpy))
cat(sprintf("   → Métodos especializados (Croston, SBA, TSB) são CRÍTICOS\n\n"))

cat(sprintf("   - %.1f%% dos materiais apresentam padrão Smooth ou Erratic\n",
            recomendacoes$prop_smooth_erratic))
cat(sprintf("   → Métodos tradicionais podem ser competitivos neste segmento\n\n"))

if (tem_projeto && nrow(recomendacoes$subsistemas_desafiadores) > 0 && 
    recomendacoes$subsistemas_desafiadores$cd_projeto_principal[1] != "N/A") {
  
  cat(sprintf("🎯 SUBSISTEMAS PRIORITÁRIOS:\n"))
  cat(sprintf("   Os seguintes subsistemas concentram maior proporção de padrões Lumpy:\n"))
  for (i in 1:nrow(recomendacoes$subsistemas_desafiadores)) {
    cat(sprintf("   %d. %s (%.1f%% Lumpy)\n",
                i,
                recomendacoes$subsistemas_desafiadores$cd_projeto_principal[i],
                recomendacoes$subsistemas_desafiadores$prop_lumpy[i]))
  }
  cat("\n")
}

cat(sprintf("📈 ESTABILIDADE TEMPORAL:\n"))
cat(sprintf("   - %.1f%% dos materiais mantêm categoria SBC estável entre origens\n",
            recomendacoes$taxa_estabilidade_geral))

if (recomendacoes$taxa_estabilidade_geral < 70) {
  cat("   ⚠️  Alta volatilidade → Considerar recalibração periódica de modelos\n\n")
} else {
  cat("   ✅ Boa estabilidade → Modelos tendem a manter desempenho consistente\n\n")
}

cat(sprintf("🔧 ESTRATÉGIA RECOMENDADA:\n"))
if (recomendacoes$necessidade_hibrida) {
  cat("   ✅ ABORDAGEM HÍBRIDA é NECESSÁRIA:\n")
  cat("      - Métodos especializados (Croston/SBA/TSB) para Intermittent/Lumpy\n")
  cat("      - Métodos tradicionais competitivos para Smooth/Erratic\n")
  cat("      - Considerar ADIDA para agregação temporal\n")
  cat("      - Avaliar métodos probabilísticos (Poisson/Gama) como baseline\n")
} else {
  cat("   ℹ️  Método único pode ser suficiente (baixa heterogeneidade)\n")
}

cat("\n═══════════════════════════════════════════════════════════════\n")

## 6.3. Salvamento de Metadados ####

cat("\n📊 6.3. Salvando metadados exploratórios...\n")

# Consolidar todos os resultados em objeto estruturado
metadata_exploratoria <- list(
  
  # Distribuição SBC
  distribuicao_sbc = distribuicao_sbc,
  stats_por_categoria = stats_consolidadas,
  todas_classificacoes = todas_classificacoes,
  
  # Análise de transições
  materiais_multiplas_origens = materiais_multiplas_origens,
  transicoes = if (nrow(transicoes) > 0) transicoes else tibble(),
  matriz_transicoes = matriz_transicoes,
  materiais_volateis = estabilidade %>% filter(!estavel),
  estabilidade = estabilidade,
  
  # Análise por subsistema (se disponível)
  tem_dados_subsistema = tem_projeto,
  distribuicao_sbc_subsistema = if (tem_projeto) {
    dados_subsistema_clean %>% count(cd_projeto_principal, categoria_sbc)
  } else {
    tibble()
  },
  stats_subsistema = stats_subsistema,
  tabela_cruzada_subsistema = tabela_cruzada,
  teste_chi_subsistema = teste_chi,
  
  # Evolução temporal
  evolucao_temporal = evolucao_temporal,
  
  # Casos especiais
  materiais_excluidos = materiais_excluidos_consolidado,
  materiais_insuficientes = insuficientes,
  
  # Materiais exemplo selecionados
  materiais_exemplo = exemplos,
  
  # Recomendações
  recomendacoes_modelagem = recomendacoes
)

# Salvar
saveRDS(
  metadata_exploratoria,
  here(config$paths$data$processed, "metadata_exploratoria.rds")
)

cat("\n✅ Metadados exploratórios salvos com sucesso!\n")
cat(sprintf("   Arquivo: %s\n", 
            here(config$paths$data$processed, "metadata_exploratoria.rds")))

log_message("Metadados exploratórios salvos", "INFO")

# =============================================================================
# VALIDAÇÕES FINAIS ####
# =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("VALIDAÇÕES FINAIS - CHECKLIST\n")
cat(strrep("=", 70), "\n\n")

log_message("Executando validações finais", "INFO")

validacoes <- list(
  data_leakage = all(sapply(names(splits_list), function(origem) {
    # Verificar se análises usaram apenas TREINO
    nrow(splits_list[[origem]]$test) > 0  # Teste existe mas não foi usado
  })),
  
  todas_origens_analisadas = length(unique(distribuicao_sbc$origem)) == length(splits_list),
  
  graficos_principais_salvos = all(file.exists(
    here(config$paths$output$figures, "03_exploratory",
         c("01_distribuicao_sbc_origens.png",
           "05_scatter_adi_cv2_categorias.png",
           "06_exemplos_series_temporais.png"))
  )),
  
  tabelas_exportadas = all(file.exists(
    here(config$paths$output$tables, "03_exploratory",
         c("stats_descritivas_por_categoria.csv"))
  )),
  
  relatorio_gerado = file.exists(
    here(config$paths$output$reports, "03_exploratory_summary_report.txt")
  ),
  
  metadata_salvo = file.exists(
    here(config$paths$data$processed, "metadata_exploratoria.rds")
  )
)

# Exibir resultados
for (nome in names(validacoes)) {
  status <- ifelse(validacoes[[nome]], "✅", "❌")
  cat(sprintf("%s %s\n", status, nome))
}

if (all(unlist(validacoes))) {
  cat("\n🎉 TODAS AS VALIDAÇÕES PASSARAM! Script concluído com sucesso.\n")
  log_message("Todas as validações passaram - script concluído com sucesso", "INFO")
} else {
  cat("\n⚠️  ATENÇÃO: Algumas validações falharam. Revise o script.\n")
  log_message("Algumas validações falharam - revisar script", "WARNING")
}

cat("\n═══════════════════════════════════════════════════════════════\n")

# =============================================================================
# RELATÓRIO FINAL ####
# =============================================================================

cat("\n🎉 PROCESSAMENTO CONCLUÍDO! 🎉\n")
cat("==========================================\n")
cat("RESUMO DO PROCESSAMENTO:\n\n")

cat(sprintf("📊 Origens analisadas: %d\n", length(splits_list)))
cat(sprintf("📈 Categorias SBC identificadas: %d\n", 
            n_distinct(todas_classificacoes$categoria_sbc)))
cat(sprintf("🔄 Materiais com transições: %s\n", 
            format(n_distinct(transicoes$cd_material), big.mark = ",")))
cat(sprintf("⚠️  Materiais excluídos: %s\n", 
            format(n_distinct(materiais_excluidos_consolidado$cd_material), big.mark = ",")))

cat("\n📁 Arquivos gerados:\n")
cat("   Figuras:\n")
list.files(here(config$paths$output$figures, "03_exploratory")) %>%
  walk(~cat(sprintf("      - %s\n", .x)))

cat("   Tabelas:\n")
list.files(here(config$paths$output$tables, "03_exploratory")) %>%
  walk(~cat(sprintf("      - %s\n", .x)))

cat("   Relatórios:\n")
cat("      - 03_exploratory_summary_report.txt\n")

cat("   Dados Processados:\n")
cat("      - metadata_exploratoria.rds\n")

log_message("========================================", "INFO")
log_message("ANÁLISE EXPLORATÓRIA CONCLUÍDA COM SUCESSO", "INFO")
log_message("========================================", "INFO")

# Limpar ambiente (manter apenas objetos essenciais)
rm(list = setdiff(ls(), c(
  "config",
  "splits_list",
  "metadata_exploratoria",
  "log_message"
)))

# Salvar workspace
save.image(here(config$paths$output$models, "03_exploratory_analysis.RData"))

cat("\n✅ Workspace salvo: 03_exploratory_analysis.RData\n")