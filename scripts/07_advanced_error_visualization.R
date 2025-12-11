# 07 - VISUALIZAÇÕES AVANÇADAS DE RESULTADOS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Geração de visualizações avançadas para análise de resultados
#            e figuras de alta qualidade para dissertação
# Data: 2025-12-10
# Versão: 1.0.0
#
# OBJETIVOS:
# 1. Rankings visuais comparativos
# 2. Análise de desempenho por categoria SBC
# 3. Visualização de estabilidade temporal
# 4. Gráficos de trade-off acurácia vs. robustez
# 5. Comparação com método Poisson
# 6. Heatmaps de significância estatística
# 7. Figuras prontas para publicação (ABNT)

# ===========================================================================
# BLOCO 0: SETUP ####
# ===========================================================================

library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(ggsci)         # Paletas científicas
library(ggpubr)        # Anotações estatísticas
library(ggrepel)       # Labels sem sobreposição
library(viridis)       # Paletas acessíveis
library(RColorBrewer)  # Paletas ColorBrewer
library(gt)            # Tabelas
library(gtExtras)      # Extensões gt
library(ggthemes)

source(here("R/utils/load_config.R"))

# Configurações gerais
set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO GERAÇÃO DE VISUALIZAÇÕES", "INFO")
log_message("========================================", "INFO")

# Criar diretórios para figuras
dir.create(here("output/figures/07_results"), showWarnings = FALSE, recursive = TRUE)

# Tema customizado para dissertação
theme_dissertacao <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
}

# Paleta de cores por família
cores_familia <- c(
  "Familia_1_Benchmarks" = "#E64B35FF",
  "Familia_2_Suavizacao" = "#4DBBD5FF",
  "Familia_3_Intermitentes" = "#00A087FF",
  "Familia_4_Probabilisticos" = "#3C5488FF",
  "Familia_5_ADIDA" = "#F39B7FFF"
)

cat("\n✅ Setup completo\n")
cat("   - Tema customizado definido\n")
cat("   - Paleta de cores configurada\n\n")

# ===========================================================================
# BLOCO 1: CARREGAMENTO DE DADOS ####
# ===========================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: CARREGAMENTO DE DADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando resultados da análise", "INFO")

# Dados consolidados
consolidado <- readRDS(here("output/forecasts/forecasts_consolidated.rds"))
metricas_mensais <- consolidado$metricas_mensais
metricas_anuais <- consolidado$metricas_anuais

# Resultados da análise estatística
resultados <- readRDS(here("output/analysis/resultados_analise_completa.rds"))
ranking_consolidado <- resultados$ranking_consolidado
desempenho_por_sbc <- resultados$desempenho_por_sbc
estabilidade_temporal <- resultados$estabilidade_temporal
recomendacoes_sbc <- resultados$recomendacoes_sbc

cat("✅ Dados carregados com sucesso\n\n")

# ===========================================================================
# BLOCO 2: RANKINGS COMPARATIVOS ####
# ===========================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: VISUALIZAÇÕES DE RANKINGS\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualizações de rankings", "INFO")

# ---------------------------------------------------------------------------
## 2.1. Ranking Horizontal (Top 10 Métodos) ####
# ---------------------------------------------------------------------------

cat("📊 2.1. Ranking horizontal por MAE...\n")

top10 <- ranking_consolidado %>%
  head(10) %>%
  mutate(
    metodo = fct_reorder(metodo, -rank_mae),
    destaque = if_else(rank_mae <= 3, "Top 3", "Outros")
  )

p_ranking_mae <- ggplot(top10, aes(x = mae_medio, y = metodo, fill = destaque)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", mae_medio)), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Top 3" = "#00A087FF", "Outros" = "#4DBBD5FF")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    # title = "Ranking de Métodos de Previsão por MAE",
    # subtitle = "Top 10 métodos com melhor desempenho (menor erro)",
    x = "MAE Médio (Mensal)",
    y = NULL,
    # caption = "Fonte: Elaborado pelo autor"
  ) +
  theme_dissertacao() +
  theme(legend.position = "none")

ggsave(
  here("output/figures/07_results/01_ranking_mae_top15.png"),
  plot = p_ranking_mae,
  width = 10, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 01_ranking_mae_top15.png\n")

# ---------------------------------------------------------------------------
## 2.2. Comparação Multi-Métrica (Top 10) ####
# ---------------------------------------------------------------------------

cat("\n📊 2.2. Comparação multi-métrica...\n")

top10_multi <- ranking_consolidado %>%
  head(10) %>%
  select(metodo, familia, rank_mae, rank_rmse, rank_linlin, rank_erro_anual) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metrica",
    values_to = "rank",
    names_prefix = "rank_"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "mae" ~ "MAE\n(Mensal)",
      metrica == "rmse" ~ "RMSE\n(Mensal)",
      metrica == "linlin" ~ "LinLin\n(Mensal)",
      metrica == "erro_anual" ~ "Erro Abs.\n(Anual)"
    ),
    metodo = fct_reorder(metodo, rank, .fun = mean)
  )

p_multi_metrica <- ggplot(top10_multi, aes(x = metrica, y = rank, group = metodo)) +
  geom_line(aes(color = familia), linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = familia), size = 3) +
  geom_text_repel(
    data = top10_multi %>% filter(metrica == "MAE\n(Mensal)"),
    aes(label = metodo, color = familia),
    nudge_x = -0.3,
    size = 3,
    fontface = "bold",
    segment.size = 0.3
  ) +
  scale_color_manual(values = cores_familia) +
  scale_y_reverse(breaks = 1:10) +
  labs(
    title = "Consistência de Rankings entre Métricas",
    subtitle = "Top 10 métodos - comparação de posições em diferentes métricas",
    x = NULL,
    y = "Posição no Ranking",
    color = "Família",
    caption = "Linhas mais horizontais indicam maior consistência entre métricas"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07_results/02_consistencia_rankings.png"),
  plot = p_multi_metrica,
  width = 12, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 02_consistencia_rankings.png\n")

# ---------------------------------------------------------------------------
## 2.3. Ranking por Família ####
# ---------------------------------------------------------------------------

cat("\n📊 2.3. Ranking agrupado por família...\n")

ranking_familia <- ranking_consolidado %>%
  head(20) %>%
  mutate(metodo = fct_reorder(metodo, mae_medio))

p_ranking_familia <- ggplot(ranking_familia, 
                            aes(x = mae_medio, y = metodo, fill = familia)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2f", mae_medio)), 
            hjust = -0.2, size = 3, fontface = "bold") +
  scale_fill_manual(values = cores_familia) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Ranking de Métodos por Família Metodológica",
    subtitle = "Top 20 métodos classificados por MAE médio",
    x = "MAE Médio",
    y = NULL,
    fill = "Família",
    caption = "Cores representam diferentes famílias metodológicas"
  ) +
  theme_dissertacao() +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07_results/03_ranking_por_familia.png"),
  plot = p_ranking_familia,
  width = 11, height = 9, dpi = 300
)

cat("   ✅ Gráfico salvo: 03_ranking_por_familia.png\n")

# ===========================================================================
# BLOCO 3: ANÁLISE POR CATEGORIA SBC ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: ANÁLISE POR CATEGORIA SBC\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualizações por categoria SBC", "INFO")

# ---------------------------------------------------------------------------
## 3.1. Heatmap de Rankings por Categoria ####
# ---------------------------------------------------------------------------

cat("📊 3.1. Heatmap de rankings por categoria...\n")

# Top 10 métodos globais
top10_metodos <- ranking_consolidado %>% head(10) %>% pull(metodo)

# Calcular rankings por categoria
rankings_sbc <- desempenho_por_sbc %>%
  filter(metodo %in% top10_metodos) %>%
  group_by(categoria_sbc) %>%
  arrange(mae_medio) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(categoria_sbc, metodo, rank, mae_medio)

p_heatmap_sbc <- ggplot(rankings_sbc, 
                        aes(x = categoria_sbc, y = fct_reorder(metodo, rank, .fun = mean), 
                            fill = rank)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = rank), color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient2(
    low = "#00A087FF", 
    mid = "#F39B7FFF", 
    high = "#E64B35FF",
    midpoint = 5.5,
    breaks = 1:10
  ) +
  labs(
    title = "Rankings de Métodos por Categoria SBC",
    subtitle = "Posição dos Top 10 métodos globais em cada categoria de demanda",
    x = "Categoria SBC",
    y = NULL,
    fill = "Rank",
    caption = "Verde = melhor posição; Vermelho = pior posição"
  ) +
  theme_dissertacao() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  here("output/figures/07_results/04_heatmap_rankings_sbc.png"),
  plot = p_heatmap_sbc,
  width = 10, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 04_heatmap_rankings_sbc.png\n")

# ---------------------------------------------------------------------------
## 3.2. Desempenho de Métodos Especializados ####
# ---------------------------------------------------------------------------

cat("\n📊 3.2. Análise de métodos especializados...\n")

metodos_intermitentes <- c("Croston", "SBA", "TSB")
benchmarks <- c("Naive", "Mean", "MA")

comparacao_especializado <- desempenho_por_sbc %>%
  filter(
    categoria_sbc %in% c("Intermittent", "Lumpy"),
    metodo %in% c(metodos_intermitentes, benchmarks)
  ) %>%
  mutate(
    tipo = if_else(metodo %in% metodos_intermitentes, 
                   "Especializado", "Benchmark")
  )

p_especializado <- ggplot(comparacao_especializado, 
                          aes(x = metodo, y = mae_medio, fill = tipo)) +
  geom_col(position = position_dodge(0.8), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2f", mae_medio)), 
            position = position_dodge(0.8),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  facet_wrap(~categoria_sbc, scales = "free_y") +
  scale_fill_manual(values = c("Especializado" = "#00A087FF", 
                               "Benchmark" = "#4DBBD5FF")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Métodos Especializados vs. Benchmarks",
    subtitle = "Desempenho em categorias Intermittent e Lumpy",
    x = NULL,
    y = "MAE Médio",
    fill = "Tipo",
    caption = "Métodos especializados: Croston, SBA, TSB"
  ) +
  theme_dissertacao() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here("output/figures/07_results/05_especializado_vs_benchmark.png"),
  plot = p_especializado,
  width = 10, height = 6, dpi = 300
)

cat("   ✅ Gráfico salvo: 05_especializado_vs_benchmark.png\n")

# ---------------------------------------------------------------------------
## 3.3. Boxplot de Distribuição por Categoria ####
# ---------------------------------------------------------------------------

cat("\n📊 3.3. Distribuição de erros por categoria...\n")

# Top 5 métodos globais
top5_metodos <- ranking_consolidado %>% head(5) %>% pull(metodo)

erros_por_categoria <- metricas_mensais %>%
  filter(convergence, metodo %in% top5_metodos) %>%
  select(categoria_sbc, metodo, mae_mensal)

p_boxplot_sbc <- ggplot(erros_por_categoria, 
                        aes(x = categoria_sbc, y = mae_mensal, fill = metodo)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = comma) +
  scale_fill_nejm() +
  labs(
    title = "Distribuição de Erros por Categoria SBC",
    subtitle = "Top 5 métodos - MAE mensal (escala logarítmica)",
    x = "Categoria SBC",
    y = "MAE Mensal (log)",
    fill = "Método",
    caption = "Boxplots mostram mediana, quartis e outliers"
  ) +
  theme_dissertacao() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here("output/figures/07_results/06_boxplot_erros_categoria.png"),
  plot = p_boxplot_sbc,
  width = 11, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 06_boxplot_erros_categoria.png\n")

# ===========================================================================
# BLOCO 4: ESTABILIDADE TEMPORAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: ANÁLISE DE ESTABILIDADE TEMPORAL\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualizações de estabilidade temporal", "INFO")

# ---------------------------------------------------------------------------
## 4.1. Evolução de MAE ao Longo das Origens ####
# ---------------------------------------------------------------------------

cat("📊 4.1. Evolução temporal de MAE...\n")

# Preparar dados de evolução temporal (Top 8 métodos)
top8_metodos <- ranking_consolidado %>% head(8) %>% pull(metodo)

evolucao_temporal <- metricas_mensais %>%
  filter(convergence, metodo %in% top8_metodos) %>%
  group_by(origem, metodo, familia) %>%
  summarise(mae_medio = mean(mae_mensal, na.rm = TRUE), .groups = 'drop') %>%
  mutate(origem_num = as.numeric(str_extract(origem, "\\d+")))

p_evolucao <- ggplot(evolucao_temporal, 
                     aes(x = origem_num, y = mae_medio, 
                         color = metodo, group = metodo)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  scale_color_d3() +
  scale_x_continuous(breaks = 1:6, labels = paste0("Origem ", 1:6)) +
  labs(
    title = "Estabilidade Temporal dos Métodos",
    subtitle = "Evolução do MAE médio ao longo das 6 origens temporais",
    x = "Origem Temporal",
    y = "MAE Médio",
    color = "Método",
    caption = "Linhas mais planas indicam maior estabilidade"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07_results/07_evolucao_temporal_mae.png"),
  plot = p_evolucao,
  width = 12, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 07_evolucao_temporal_mae.png\n")

# ---------------------------------------------------------------------------
## 4.2. Coeficiente de Variação entre Origens ####
# ---------------------------------------------------------------------------

cat("\n📊 4.2. Coeficiente de variação temporal...\n")

# Verificar se há CVs calculados
if(!all(is.na(estabilidade_temporal$mae_cv))) {
  
  cv_top15 <- estabilidade_temporal %>%
    filter(!is.na(mae_cv)) %>%
    arrange(mae_cv) %>%
    head(15) %>%
    mutate(
      metodo = fct_reorder(metodo, mae_cv),
      estabilidade_cat = case_when(
        mae_cv < 0.10 ~ "Muito Estável",
        mae_cv < 0.20 ~ "Estável",
        mae_cv < 0.30 ~ "Moderado",
        TRUE ~ "Instável"
      )
    )
  
  p_cv_temporal <- ggplot(cv_top15, aes(x = mae_cv, y = metodo, fill = estabilidade_cat)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = sprintf("%.3f", mae_cv)), 
              hjust = -0.2, size = 3.5, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "Muito Estável" = "#00A087FF",
        "Estável" = "#4DBBD5FF",
        "Moderado" = "#F39B7FFF",
        "Instável" = "#E64B35FF"
      )
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Estabilidade Temporal dos Métodos",
      subtitle = "Coeficiente de Variação (CV) do MAE entre origens - Top 15 mais estáveis",
      x = "Coeficiente de Variação",
      y = NULL,
      fill = "Classificação",
      caption = "CV < 0.10 = Muito Estável; CV < 0.20 = Estável"
    ) +
    theme_dissertacao() +
    theme(legend.position = "right")
  
  ggsave(
    here("output/figures/07_results/08_cv_estabilidade_temporal.png"),
    plot = p_cv_temporal,
    width = 11, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 08_cv_estabilidade_temporal.png\n")
  
} else {
  cat("   ⚠️  CV não disponível (apenas 1 origem). Gráfico não gerado.\n")
}

# ===========================================================================
# BLOCO 5: TRADE-OFF ACURÁCIA VS. ROBUSTEZ ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 5: ANÁLISE DE TRADE-OFF ACURÁCIA-ROBUSTEZ\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualização de trade-off", "INFO")

cat("📊 5.1. Scatter plot acurácia vs. robustez...\n")

# Preparar dados de trade-off
trade_off_data <- ranking_consolidado %>%
  select(metodo, familia, mae_medio, taxa_convergencia, rank_mae) %>%
  mutate(
    destaque = case_when(
      rank_mae <= 5 ~ metodo,
      TRUE ~ NA_character_
    ),
    categoria_tradeoff = case_when(
      mae_medio <= quantile(mae_medio, 0.25) & taxa_convergencia >= 95 ~ 
        "Ótimo (Alta acurácia + Alta robustez)",
      mae_medio <= quantile(mae_medio, 0.50) & taxa_convergencia >= 90 ~ 
        "Bom (Balanceado)",
      mae_medio <= quantile(mae_medio, 0.25) & taxa_convergencia < 85 ~ 
        "Alta acurácia, baixa robustez",
      mae_medio > quantile(mae_medio, 0.75) & taxa_convergencia >= 95 ~ 
        "Alta robustez, baixa acurácia",
      TRUE ~ "Intermediário"
    )
  )

p_tradeoff <- ggplot(trade_off_data, 
                     aes(x = mae_medio, y = taxa_convergencia, color = familia)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text_repel(
    aes(label = destaque),
    size = 3.5,
    fontface = "bold",
    max.overlaps = 15,
    segment.size = 0.3
  ) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = median(trade_off_data$mae_medio), 
             linetype = "dashed", color = "gray50") +
  annotate("text", x = min(trade_off_data$mae_medio), y = 96, 
           label = "Alta Robustez (>95%)", hjust = 0, size = 3, color = "gray40") +
  annotate("text", x = median(trade_off_data$mae_medio) + 0.5, 
           y = min(trade_off_data$taxa_convergencia), 
           label = "Mediana MAE", hjust = 0, size = 3, color = "gray40") +
  scale_color_manual(values = cores_familia) +
  labs(
    title = "Trade-off Acurácia vs. Robustez Operacional",
    subtitle = "Desempenho preditivo (MAE) vs. Taxa de convergência",
    x = "MAE Médio (menor = melhor)",
    y = "Taxa de Convergência (%)",
    color = "Família",
    caption = "Quadrante superior esquerdo = ideal (baixo erro + alta convergência)"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07_results/09_tradeoff_acuracia_robustez.png"),
  plot = p_tradeoff,
  width = 12, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 09_tradeoff_acuracia_robustez.png\n")

# ===========================================================================
# BLOCO 6: COMPARAÇÃO COM POISSON ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6: COMPARAÇÃO COM MÉTODO POISSON\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualizações de comparação com Poisson", "INFO")

# Verificar se Poisson está presente
metodos_disponiveis <- unique(metricas_mensais$metodo)
poisson_presente <- any(str_detect(tolower(metodos_disponiveis), "poisson"))

if(poisson_presente) {
  
  metodo_poisson <- metodos_disponiveis[str_detect(tolower(metodos_disponiveis), 
                                                   "poisson")][1]
  
  # ---------------------------------------------------------------------------
  ## 6.1. Ganho Percentual vs. Poisson ####
  # ---------------------------------------------------------------------------
  
  cat("📊 6.1. Ganho percentual sobre Poisson...\n")
  
  # Extrair MAE do Poisson
  mae_poisson <- ranking_consolidado %>%
    filter(metodo == metodo_poisson) %>%
    pull(mae_medio)
  
  # Calcular ganho para top 15
  ganho_poisson <- ranking_consolidado %>%
    head(15) %>%
    mutate(
      ganho_pct = (mae_poisson - mae_medio) / mae_poisson * 100,
      metodo = fct_reorder(metodo, ganho_pct),
      positivo = ganho_pct > 0
    )
  
  p_ganho_poisson <- ggplot(ganho_poisson, 
                            aes(x = ganho_pct, y = metodo, fill = positivo)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = sprintf("%+.1f%%", ganho_pct)), 
              hjust = if_else(ganho_poisson$ganho_pct > 0, -0.2, 1.2), 
              size = 3.5, fontface = "bold") +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
    scale_fill_manual(values = c("TRUE" = "#00A087FF", "FALSE" = "#E64B35FF")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("Ganho de Desempenho em Relação ao Poisson (MAE = %.2f)", 
                      mae_poisson),
      subtitle = "Top 15 métodos - Redução percentual de erro",
      x = "Ganho (%)",
      y = NULL,
      caption = "Valores positivos = melhoria sobre Poisson; Negativos = piora"
    ) +
    theme_dissertacao() +
    theme(legend.position = "none")
  
  ggsave(
    here("output/figures/07_results/10_ganho_vs_poisson.png"),
    plot = p_ganho_poisson,
    width = 11, height = 8, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 10_ganho_vs_poisson.png\n")
  
  # ---------------------------------------------------------------------------
  ## 6.2. Comparação Direta Top 5 vs. Poisson ####
  # ---------------------------------------------------------------------------
  
  cat("\n📊 6.2. Comparação direta top 5 vs. Poisson...\n")
  
  top5_vs_poisson <- ranking_consolidado %>%
    filter(metodo %in% c(top5_metodos, metodo_poisson)) %>%
    mutate(
      destaque = if_else(metodo == metodo_poisson, "Poisson", "Alternativo"),
      metodo = fct_reorder(metodo, mae_medio)
    )
  
  p_top5_poisson <- ggplot(top5_vs_poisson, 
                           aes(x = mae_medio, y = metodo, fill = destaque)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = sprintf("%.2f", mae_medio)), 
              hjust = -0.2, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Poisson" = "#E64B35FF", "Alternativo" = "#00A087FF")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Comparação: Top 5 Métodos Alternativos vs. Poisson (Método Atual DECEA)",
      subtitle = "MAE médio mensal",
      x = "MAE Médio",
      y = NULL,
      fill = "Tipo",
      caption = "Método Poisson representa a prática atual do DECEA"
    ) +
    theme_dissertacao() +
    theme(legend.position = "right")
  
  ggsave(
    here("output/figures/07_results/11_top5_vs_poisson.png"),
    plot = p_top5_poisson,
    width = 11, height = 7, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 11_top5_vs_poisson.png\n")
  
} else {
  cat("⚠️  Método Poisson não encontrado. Gráficos de comparação não gerados.\n")
}

# ===========================================================================
# BLOCO 7: HEATMAP DE SIGNIFICÂNCIA ESTATÍSTICA ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 7: VISUALIZAÇÃO DE SIGNIFICÂNCIA ESTATÍSTICA\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando heatmap de testes de significância", "INFO")

# Verificar se há resultados de DM test
if(!is.null(resultados$dm_results)) {
  
  cat("📊 7.1. Heatmap de significância (Diebold-Mariano)...\n")
  
  dm_data <- resultados$dm_results %>%
    mutate(
      significancia = case_when(
        is.na(dm_pvalue) ~ "NA",
        dm_pvalue < 0.01 ~ "p < 0.01***",
        dm_pvalue < 0.05 ~ "p < 0.05**",
        dm_pvalue < 0.10 ~ "p < 0.10*",
        TRUE ~ "ns"
      ),
      metodo = fct_reorder(metodo, dm_pvalue, .na_rm = TRUE)
    )
  
  p_dm_heatmap <- ggplot(dm_data, 
                         aes(x = "vs. Poisson", y = metodo, fill = dm_pvalue)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = significancia), 
              color = "white", fontface = "bold", size = 3.5) +
    scale_fill_gradient2(
      low = "#00A087FF",
      mid = "#F39B7FFF",
      high = "#E64B35FF",
      midpoint = 0.05,
      na.value = "gray50",
      limits = c(0, 0.10),
      oob = scales::squish
    ) +
    labs(
      title = "Significância Estatística: Top 10 vs. Poisson",
      subtitle = "Teste Diebold-Mariano (α = 0.05)",
      x = NULL,
      y = NULL,
      fill = "p-valor",
      caption = "*** p<0.01; ** p<0.05; * p<0.10; ns = não significativo"
    ) +
    theme_dissertacao() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0)
    )
  
  ggsave(
    here("output/figures/07_results/12_heatmap_significancia_dm.png"),
    plot = p_dm_heatmap,
    width = 8, height = 10, dpi = 300
  )
  
  cat("   ✅ Gráfico salvo: 12_heatmap_significancia_dm.png\n")
  
} else {
  cat("⚠️  Resultados de teste DM não disponíveis. Heatmap não gerado.\n")
}

# ===========================================================================
# BLOCO 8: RECOMENDAÇÕES POR CATEGORIA ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 8: VISUALIZAÇÃO DE RECOMENDAÇÕES\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando visualização de recomendações por categoria", "INFO")

cat("📊 8.1. Métodos recomendados por categoria SBC...\n")

recomendacoes_plot <- recomendacoes_sbc %>%
  mutate(categoria_sbc = fct_reorder(categoria_sbc, mae_medio))

p_recomendacoes <- ggplot(recomendacoes_plot, 
                          aes(x = mae_medio, y = categoria_sbc, fill = familia)) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = sprintf("%s\nMAE=%.2f", metodo_recomendado, mae_medio)),
    hjust = -0.1, size = 3.5, fontface = "bold", color = "black"
  ) +
  scale_fill_manual(values = cores_familia) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Métodos Recomendados por Categoria de Demanda",
    subtitle = "Melhor método para cada categoria SBC baseado em MAE médio",
    x = "MAE Médio",
    y = "Categoria SBC",
    fill = "Família",
    caption = "Estratégia de portfólio híbrido: método específico por categoria"
  ) +
  theme_dissertacao() +
  theme(legend.position = "right")

ggsave(
  here("output/figures/07_results/13_recomendacoes_por_categoria.png"),
  plot = p_recomendacoes,
  width = 12, height = 6, dpi = 300
)

cat("   ✅ Gráfico salvo: 13_recomendacoes_por_categoria.png\n")

# ===========================================================================
# BLOCO 9: RESUMO EXECUTIVO (INFOGRÁFICO) ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 9: INFOGRÁFICO DE RESUMO EXECUTIVO\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando infográfico de resumo", "INFO")

cat("📊 9.1. Painel de resumo executivo...\n")

# Extrair estatísticas-chave
metodo_campeao <- ranking_consolidado %>% slice(1)
n_metodos <- nrow(ranking_consolidado)
n_materiais <- n_distinct(metricas_mensais$cd_material)
taxa_conv_global <- mean(metricas_mensais$convergence) * 100

# Criar componentes do painel
p1_campeao <- ggplot(metodo_campeao, aes(x = "", y = mae_medio)) +
  geom_col(fill = "#00A087FF", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = sprintf("%s\nMAE=%.2f", metodo, mae_medio)),
            vjust = -0.5, size = 5, fontface = "bold") +
  coord_cartesian(ylim = c(0, max(metodo_campeao$mae_medio) * 1.3)) +
  labs(title = "🏆 MÉTODO CAMPEÃO", x = NULL, y = "MAE") +
  theme_dissertacao() +
  theme(axis.text.x = element_blank())

p2_familia_campeao <- ggplot(ranking_consolidado %>% head(10), 
                             aes(x = familia, fill = familia)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = cores_familia) +
  labs(title = "📊 FAMÍLIAS NO TOP 10", x = NULL, y = "Frequência") +
  theme_dissertacao() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

p3_stats <- data.frame(
  metrica = c("Métodos\nTestados", "Materiais\nAnalisados", 
              "Taxa de\nConvergência", "Origens\nTemporais"),
  valor = c(n_metodos, n_materiais, taxa_conv_global, 6)
) %>%
  ggplot(aes(x = metrica, y = valor)) +
  geom_col(fill = "#4DBBD5FF", alpha = 0.8) +
  geom_text(aes(label = ifelse(metrica == "Taxa de\nConvergência",
                               sprintf("%.1f%%", valor),
                               format(valor, big.mark = ","))),
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "📈 ESTATÍSTICAS GERAIS", x = NULL, y = NULL) +
  theme_dissertacao() +
  theme(axis.text.y = element_blank())

if(poisson_presente) {
  mae_poisson_val <- ranking_consolidado %>%
    filter(metodo == metodo_poisson) %>%
    pull(mae_medio)
  
  ganho_max <- max(ganho_poisson$ganho_pct)
  
  p4_ganho <- data.frame(
    categoria = c("Poisson\n(Atual)", "Melhor\nAlternativo", "Ganho"),
    valor = c(mae_poisson_val, metodo_campeao$mae_medio, ganho_max)
  ) %>%
    ggplot(aes(x = categoria, y = valor)) +
    geom_col(fill = c("#E64B35FF", "#00A087FF", "#F39B7FFF"), alpha = 0.8) +
    geom_text(aes(label = ifelse(categoria == "Ganho",
                                 sprintf("+%.1f%%", valor),
                                 sprintf("%.2f", valor))),
              vjust = -0.5, size = 5, fontface = "bold") +
    labs(title = "💰 GANHO VS. POISSON", x = NULL, y = "MAE / Ganho (%)") +
    theme_dissertacao()
} else {
  p4_ganho <- ggplot() + theme_void() + 
    annotate("text", x = 0.5, y = 0.5, 
             label = "Poisson não disponível", size = 5)
}

# Combinar painéis
painel_resumo <- (p1_campeao | p2_familia_campeao) / (p3_stats | p4_ganho) +
  plot_annotation(
    title = "RESUMO EXECUTIVO - ANÁLISE COMPARATIVA DE MÉTODOS DE PREVISÃO",
    subtitle = sprintf("Sistema de Controle do Espaço Aéreo Brasileiro (SISCEAB) | %d SKUs | %d Métodos | 6 Origens Temporais",
                       n_materiais, n_metodos),
    caption = "Fonte: Elaborado pelo autor com base em dados do DECEA (2020-2024)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1)
    )
  )

ggsave(
  here("output/figures/07_results/14_painel_resumo_executivo.png"),
  plot = painel_resumo,
  width = 14, height = 10, dpi = 300
)

cat("   ✅ Gráfico salvo: 14_painel_resumo_executivo.png\n")

# ===========================================================================
# RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL - VISUALIZAÇÕES GERADAS\n")
cat(strrep("=", 70), "\n\n")

# Listar todas as figuras geradas
figuras_geradas <- list.files(
  here("output/figures/07_results"),
  pattern = "\\.png$",
  full.names = FALSE
)

cat("📊 FIGURAS GERADAS PARA DISSERTAÇÃO:\n\n")

categorias_figuras <- list(
  "Rankings Comparativos" = c("01", "02", "03"),
  "Análise por Categoria SBC" = c("04", "05", "06"),
  "Estabilidade Temporal" = c("07", "08"),
  "Trade-off e Robustez" = c("09"),
  "Comparação com Poisson" = c("10", "11"),
  "Significância Estatística" = c("12"),
  "Recomendações" = c("13"),
  "Resumo Executivo" = c("14")
)

for(categoria in names(categorias_figuras)) {
  cat(sprintf("\n%s:\n", categoria))
  prefixos <- categorias_figuras[[categoria]]
  figs_categoria <- figuras_geradas[str_detect(figuras_geradas, 
                                               paste0("^(", paste(prefixos, collapse = "|"), ")"))]
  for(fig in figs_categoria) {
    cat(sprintf("   ✅ %s\n", fig))
  }
}

cat("\n", strrep("=", 70), "\n")
cat(sprintf("\n✅ Total de figuras geradas: %d\n", length(figuras_geradas)))
cat(sprintf("📁 Localização: output/figures/07_results/\n"))
cat(sprintf("📐 Resolução: 300 DPI (pronto para impressão)\n"))
cat(sprintf("🎨 Paleta: Cores científicas acessíveis\n"))

cat("\n🎯 PRÓXIMOS PASSOS:\n")
cat("   1. Revisar todas as figuras geradas\n")
cat("   2. Selecionar figuras para Capítulo 4 (Resultados)\n")
cat("   3. Adicionar legendas e notas conforme ABNT\n")
cat("   4. Integrar com texto da dissertação\n")

cat("\n", strrep("=", 70), "\n", sep = "")

log_message("========================================", "INFO")
log_message("VISUALIZAÇÕES FINALIZADAS COM SUCESSO", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script 07 finalizado em:", format(Sys.time()), "\n\n")