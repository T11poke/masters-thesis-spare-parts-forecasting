# 07b - ANÁLISE MULTI-MÉTRICA E SENSIBILIDADE ####
#
# Descrição: Análise comparativa usando TODAS as métricas implementadas
#            Verifica consistência de rankings e identifica trade-offs
# Data: 2025-12-10
# Versão: 1.0.0
#
# OBJETIVOS:
# 1. Verificar consistência de rankings entre métricas
# 2. Identificar métodos "especialistas" vs. "generalistas"
# 3. Analisar trade-offs entre diferentes objetivos
# 4. Validar robustez das conclusões
# 5. Análise específica de Bias e PER
# 6. Responder: "As conclusões mudam se usarmos outra métrica?"

# ===========================================================================
# BLOCO 0: SETUP ####
# ===========================================================================

library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(ggsci)
library(ggcorrplot)    # Correlação de rankings
library(GGally)        # Scatter matrix
library(gt)
library(gtExtras)

source(here("R/utils/load_config.R"))

set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO ANÁLISE MULTI-MÉTRICA", "INFO")
log_message("========================================", "INFO")

# Criar diretórios
dir.create(here("output/figures/07b_multimetric"), showWarnings = FALSE, recursive = TRUE)

# Tema customizado
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

# Paleta de cores
cores_familia <- c(
  "Familia_1_Benchmarks" = "#E64B35FF",
  "Familia_2_Suavizacao" = "#4DBBD5FF",
  "Familia_3_Intermitentes" = "#00A087FF",
  "Familia_4_Probabilisticos" = "#3C5488FF",
  "Familia_5_ADIDA" = "#F39B7FFF"
)

cat("\n✅ Setup completo\n\n")

# ===========================================================================
# BLOCO 1: CARREGAMENTO E PREPARAÇÃO DE DADOS ####
# ===========================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("BLOCO 1: CARREGAMENTO DE DADOS\n")
cat(strrep("=", 70), "\n\n")

log_message("Carregando dados consolidados", "INFO")

# Dados consolidados
consolidado <- readRDS(here("output/forecasts/forecasts_consolidated.rds"))
metricas_mensais <- consolidado$metricas_mensais

# Resultados da análise estatística
resultados <- readRDS(here("output/analysis/resultados_analise_completa.rds"))

cat("✅ Dados carregados\n\n")

# ===========================================================================
# BLOCO 2: RANKINGS POR TODAS AS MÉTRICAS ####
# ===========================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("BLOCO 2: CÁLCULO DE RANKINGS POR MÉTRICA\n")
cat(strrep("=", 70), "\n\n")

log_message("Calculando rankings para todas as métricas", "INFO")

# Agregar por método (média entre origens e materiais)
rankings_todas_metricas <- metricas_mensais %>%
  filter(convergence) %>%
  group_by(metodo, familia) %>%
  summarise(
    n_obs = n(),
    
    # Métricas de magnitude de erro
    mae_medio = mean(mae_mensal, na.rm = TRUE),
    rmse_medio = mean(rmse_mensal, na.rm = TRUE),
    linlin_medio = mean(linlin_mensal, na.rm = TRUE),
    mad_mean_medio = mean(mad_mean_ratio, na.rm = TRUE),
    
    # Métrica de viés
    bias_medio = mean(bias_mensal, na.rm = TRUE),
    bias_abs_medio = mean(abs(bias_mensal), na.rm = TRUE),
    
    # Métrica de padrão
    per_medio = mean(per, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(
    # Rankings (ordem crescente = melhor, exceto PER que é decrescente)
    rank_mae = rank(mae_medio),
    rank_rmse = rank(rmse_medio),
    rank_linlin = rank(linlin_medio),
    rank_mad_mean = rank(mad_mean_medio),
    rank_bias = rank(bias_abs_medio),  # Menor valor absoluto é melhor
    rank_per = rank(-per_medio),  # Maior PER é melhor (mais acertos)
    
    # Rank médio geral
    rank_medio_geral = (rank_mae + rank_rmse + rank_linlin + 
                          rank_mad_mean + rank_bias + rank_per) / 6
  ) %>%
  arrange(rank_medio_geral)

cat("✅ Rankings calculados para 6 métricas\n")
cat(sprintf("   - Métodos avaliados: %d\n", nrow(rankings_todas_metricas)))

# Resumo estatístico
cat("\n📊 Estatísticas descritivas das métricas:\n\n")
rankings_todas_metricas %>%
  select(mae_medio, rmse_medio, linlin_medio, bias_medio, per_medio) %>%
  summary() %>%
  print()

# ===========================================================================
# BLOCO 3: MATRIZ DE CORRELAÇÃO DE RANKINGS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 3: CORRELAÇÃO ENTRE RANKINGS\n")
cat(strrep("=", 70), "\n\n")

log_message("Calculando correlações entre rankings", "INFO")

cat("📊 3.1. Matriz de correlação de Spearman...\n")

# Preparar matriz de rankings
matriz_rankings <- rankings_todas_metricas %>%
  select(metodo, rank_mae, rank_rmse, rank_linlin, 
         rank_mad_mean, rank_bias, rank_per) %>%
  column_to_rownames("metodo")

# Calcular correlações de Spearman
cor_matrix <- cor(matriz_rankings, method = "spearman")

# Renomear para melhor visualização
colnames(cor_matrix) <- c("MAE", "RMSE", "LinLin", "MAD/Mean", "Bias", "PER")
rownames(cor_matrix) <- c("MAE", "RMSE", "LinLin", "MAD/Mean", "Bias", "PER")

# Imprimir matriz
cat("\n📊 Correlação de Spearman entre rankings:\n\n")
print(round(cor_matrix, 3))

# Visualização: Heatmap de correlação
p_cor_rankings <- ggcorrplot(
  cor_matrix,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 4,
  colors = c("#E64B35FF", "white", "#00A087FF"),
  title = "Correlação de Rankings entre Métricas",
  legend.title = "Correlação\nde Spearman",
  ggtheme = theme_dissertacao()
) +
  labs(
    subtitle = "Valores próximos a 1.0 indicam rankings consistentes",
    caption = "Correlação de Spearman: 1.0 = rankings idênticos; 0.0 = não correlacionados"
  )

ggsave(
  here("output/figures/07b_multimetric/01_correlacao_rankings.png"),
  plot = p_cor_rankings,
  width = 10, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 01_correlacao_rankings.png\n")

# Interpretar correlações
cat("\n🔍 INTERPRETAÇÃO:\n")
cat(sprintf("   - MAE vs RMSE: ρ = %.3f %s\n", 
            cor_matrix["MAE", "RMSE"],
            ifelse(cor_matrix["MAE", "RMSE"] > 0.9, "(Altamente consistente)", "")))
cat(sprintf("   - MAE vs LinLin: ρ = %.3f %s\n", 
            cor_matrix["MAE", "LinLin"],
            ifelse(cor_matrix["MAE", "LinLin"] > 0.9, "(Altamente consistente)", "")))
cat(sprintf("   - MAE vs Bias: ρ = %.3f %s\n", 
            cor_matrix["MAE", "Bias"],
            ifelse(abs(cor_matrix["MAE", "Bias"]) < 0.5, "(Baixa correlação - dimensões independentes)", "")))
cat(sprintf("   - MAE vs PER: ρ = %.3f %s\n", 
            cor_matrix["MAE", "PER"],
            ifelse(abs(cor_matrix["MAE", "PER"]) < 0.5, "(Baixa correlação - dimensões independentes)", "")))

# ===========================================================================
# BLOCO 4: BUMP CHART - RANKINGS EM TODAS AS MÉTRICAS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 4: VISUALIZAÇÃO DE RANKINGS MULTI-MÉTRICA\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando bump chart expandido", "INFO")

cat("📊 4.1. Bump chart com top 10 em todas as métricas...\n")

# Top 10 por rank médio geral
top10_metodos <- rankings_todas_metricas %>%
  arrange(rank_medio_geral) %>%
  head(10) %>%
  pull(metodo)

# Preparar dados para bump chart
dados_bump <- rankings_todas_metricas %>%
  filter(metodo %in% top10_metodos) %>%
  select(metodo, familia, rank_mae, rank_rmse, rank_linlin, 
         rank_mad_mean, rank_bias, rank_per) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metrica",
    values_to = "rank",
    names_prefix = "rank_"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "mae" ~ "MAE",
      metrica == "rmse" ~ "RMSE",
      metrica == "linlin" ~ "LinLin\n(p=0.85)",
      metrica == "mad_mean" ~ "MAD/Mean",
      metrica == "bias" ~ "Bias\n(|valor|)",
      metrica == "per" ~ "PER"
    ),
    metrica = factor(metrica, levels = c("MAE", "RMSE", "LinLin\n(p=0.85)", 
                                         "MAD/Mean", "Bias\n(|valor|)", "PER"))
  )

p_bump_chart <- ggplot(dados_bump, aes(x = metrica, y = rank, 
                                       group = metodo, color = familia)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.0) +
  geom_text(
    data = dados_bump %>% filter(metrica == "MAE"),
    aes(label = metodo),
    hjust = 1.5,
    size = 3,
    fontface = "bold"
  ) +
  scale_color_manual(values = cores_familia) +
  scale_y_reverse(breaks = 1:15, limits = c(15, 1)) +
  scale_x_discrete(expand = expansion(add = c(0.5, 2))) +
  labs(
    # title = "Consistência de Rankings: Top 10 Métodos em 6 Métricas",
    # subtitle = "Linhas horizontais = desempenho consistente; Variações = trade-offs entre métricas",
    x = NULL,
    y = "Posição no Ranking",
    color = "Família",
    caption = "Rankings calculados sobre média de todas as origens e materiais (apenas convergentes)"
  ) +
  theme_dissertacao() +
  theme(
    panel.grid.major.x = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme(legend.position = "none")

ggsave(
  here("output/figures/07b_multimetric/02_bump_chart_todas_metricas.png"),
  plot = p_bump_chart,
  width = 14, height = 9, dpi = 300
)

cat("   ✅ Gráfico salvo: 02_bump_chart_todas_metricas.png\n")

# ===========================================================================
# BLOCO 5: RANKINGS LADO-A-LADO ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 5: COMPARAÇÃO DE TOP 10 POR MÉTRICA\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando comparação lado-a-lado", "INFO")

cat("📊 5.1. Rankings lado-a-lado (MAE, RMSE, LinLin)...\n")

# Top 10 por cada métrica
top10_mae <- rankings_todas_metricas %>% arrange(rank_mae) %>% head(10) %>%
  mutate(metrica = "MAE", posicao = row_number())

top10_rmse <- rankings_todas_metricas %>% arrange(rank_rmse) %>% head(10) %>%
  mutate(metrica = "RMSE", posicao = row_number())

top10_linlin <- rankings_todas_metricas %>% arrange(rank_linlin) %>% head(10) %>%
  mutate(metrica = "LinLin (p=0.85)", posicao = row_number())

top10_bias <- rankings_todas_metricas %>% arrange(rank_bias) %>% head(10) %>%
  mutate(metrica = "Menor |Bias|", posicao = row_number())

top10_per <- rankings_todas_metricas %>% arrange(rank_per) %>% head(10) %>%
  mutate(metrica = "Maior PER", posicao = row_number())

# Combinar
top10_comparacao <- bind_rows(
  top10_mae %>% select(metrica, posicao, metodo, familia, mae_medio),
  top10_rmse %>% select(metrica, posicao, metodo, familia, rmse_medio),
  top10_linlin %>% select(metrica, posicao, metodo, familia, linlin_medio),
  top10_bias %>% select(metrica, posicao, metodo, familia, bias_abs_medio),
  top10_per %>% select(metrica, posicao, metodo, familia, per_medio)
) %>%
  mutate(
    metrica = factor(metrica, levels = c("MAE", "RMSE", "LinLin (p=0.85)", 
                                         "Menor |Bias|", "Maior PER"))
  )

# Criar tabela visual
p_rankings_lado <- ggplot(top10_comparacao, 
                          aes(x = metrica, y = fct_rev(factor(posicao)), 
                              fill = familia)) +
  geom_tile(color = "white", linewidth = 1, alpha = 0.7) +
  geom_text(aes(label = metodo), fontface = "bold", size = 3) +
  scale_fill_manual(values = cores_familia) +
  scale_y_discrete(labels = 1:10) +
  labs(
    title = "Top 10 Métodos: Comparação por Métrica",
    subtitle = "Métodos em destaque aparecem consistentemente no top 10 de múltiplas métricas",
    x = NULL,
    y = "Posição",
    fill = "Família",
    caption = "Cores representam famílias metodológicas"
  ) +
  theme_dissertacao() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/03_rankings_lado_a_lado.png"),
  plot = p_rankings_lado,
  width = 14, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 03_rankings_lado_a_lado.png\n")

# Análise de sobreposição
cat("\n🔍 ANÁLISE DE SOBREPOSIÇÃO:\n")

metodos_top10_mae <- top10_mae$metodo
metodos_top10_rmse <- top10_rmse$metodo
metodos_top10_linlin <- top10_linlin$metodo

intersecao_mae_rmse <- intersect(metodos_top10_mae, metodos_top10_rmse)
intersecao_mae_linlin <- intersect(metodos_top10_mae, metodos_top10_linlin)
intersecao_todos <- intersect(intersecao_mae_rmse, metodos_top10_linlin)

cat(sprintf("   - MAE ∩ RMSE: %d métodos comuns\n", length(intersecao_mae_rmse)))
cat(sprintf("   - MAE ∩ LinLin: %d métodos comuns\n", length(intersecao_mae_linlin)))
cat(sprintf("   - MAE ∩ RMSE ∩ LinLin: %d métodos comuns\n", length(intersecao_todos)))

if(length(intersecao_todos) > 0) {
  cat("\n   🏆 Métodos no TOP 10 de MAE, RMSE E LinLin:\n")
  for(m in intersecao_todos) {
    cat(sprintf("      - %s\n", m))
  }
}

# ===========================================================================
# BLOCO 6: SCATTER PLOTS - TRADE-OFFS ENTRE MÉTRICAS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 6: ANÁLISE DE TRADE-OFFS ENTRE MÉTRICAS\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando scatter plots de trade-offs", "INFO")

cat("📊 6.1. Trade-off MAE vs. RMSE...\n")

# MAE vs RMSE
p_mae_vs_rmse <- ggplot(rankings_todas_metricas, 
                        aes(x = mae_medio, y = rmse_medio, color = familia)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(
    data = rankings_todas_metricas %>% filter(rank_medio_geral <= 5),
    aes(label = metodo),
    hjust = -0.2,
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", 
              linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = cores_familia) +
  labs(
    title = "Trade-off: MAE vs. RMSE",
    subtitle = "RMSE penaliza erros grandes mais fortemente que MAE",
    x = "MAE Médio",
    y = "RMSE Médio",
    color = "Família",
    caption = sprintf("Correlação de Pearson: r = %.3f", 
                      cor(rankings_todas_metricas$mae_medio, 
                          rankings_todas_metricas$rmse_medio))
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/04_tradeoff_mae_rmse.png"),
  plot = p_mae_vs_rmse,
  width = 10, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 04_tradeoff_mae_rmse.png\n")

# ---------------------------------------------------------------------------
## 6.2. MAE vs. LinLin ####
# ---------------------------------------------------------------------------

cat("\n📊 6.2. Trade-off MAE vs. LinLin...\n")

p_mae_vs_linlin <- ggplot(rankings_todas_metricas, 
                          aes(x = mae_medio, y = linlin_medio, color = familia)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(
    data = rankings_todas_metricas %>% filter(rank_medio_geral <= 5),
    aes(label = metodo),
    hjust = -0.2,
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_abline(slope = 0.85, intercept = 0, linetype = "dashed", 
              color = "gray40", linewidth = 0.8) +
  annotate("text", x = min(rankings_todas_metricas$mae_medio), 
           y = max(rankings_todas_metricas$linlin_medio),
           label = "LinLin = 0.85 × MAE\n(se p=0.85 e MAE=Bias)", 
           hjust = 0, vjust = 1, size = 3, color = "gray40") +
  scale_color_manual(values = cores_familia) +
  labs(
    title = "Trade-off: MAE vs. LinLin (p=0.85)",
    subtitle = "LinLin penaliza subestimação (85%) mais que superestimação (15%)",
    x = "MAE Médio (perda simétrica)",
    y = "LinLin Médio (perda assimétrica)",
    color = "Família",
    caption = "p=0.85 reflete razão de custo stockout:excesso = 5.7:1"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/05_tradeoff_mae_linlin.png"),
  plot = p_mae_vs_linlin,
  width = 10, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 05_tradeoff_mae_linlin.png\n")

# ---------------------------------------------------------------------------
## 6.3. MAE vs. Bias ####
# ---------------------------------------------------------------------------

cat("\n📊 6.3. MAE vs. Bias (detectar viés sistemático)...\n")

p_mae_vs_bias <- ggplot(rankings_todas_metricas, 
                        aes(x = mae_medio, y = bias_medio, color = familia)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(
    data = rankings_todas_metricas %>% 
      filter(rank_medio_geral <= 5 | abs(bias_medio) > 2),
    aes(label = metodo),
    hjust = -0.2,
    vjust = 0,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.5, ymax = 0.5,
           alpha = 0.1, fill = "green") +
  annotate("text", x = max(rankings_todas_metricas$mae_medio), y = 0,
           label = "Zona de baixo viés", hjust = 1, vjust = -0.5, 
           size = 3, color = "darkgreen", fontface = "bold") +
  scale_color_manual(values = cores_familia) +
  labs(
    title = "Acurácia (MAE) vs. Viés Sistemático (Bias)",
    subtitle = "Bias > 0 = subestimação sistemática; Bias < 0 = superestimação sistemática",
    x = "MAE Médio",
    y = "Bias Médio (Erro Médio)",
    color = "Família",
    caption = "Ideal: baixo MAE (eixo X) e baixo |Bias| (próximo a zero no eixo Y)"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/06_tradeoff_mae_bias.png"),
  plot = p_mae_vs_bias,
  width = 11, height = 7, dpi = 300
)

cat("   ✅ Gráfico salvo: 06_tradeoff_mae_bias.png\n")

# ===========================================================================
# BLOCO 7: ANÁLISE ESPECÍFICA DE BIAS ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 7: ANÁLISE DE VIÉS SISTEMÁTICO\n")
cat(strrep("=", 70), "\n\n")

log_message("Análise detalhada de Bias", "INFO")

cat("📊 7.1. Classificação de métodos por tipo de viés...\n")

# Classificar métodos por comportamento de viés
classificacao_bias <- rankings_todas_metricas %>%
  mutate(
    tipo_vies = case_when(
      abs(bias_medio) < 0.5 ~ "Neutro",
      bias_medio >= 0.5 ~ "Subestima",
      bias_medio <= -0.5 ~ "Superestima"
    ),
    magnitude_vies = case_when(
      abs(bias_medio) < 0.5 ~ "Baixo",
      abs(bias_medio) < 1.0 ~ "Moderado",
      abs(bias_medio) < 2.0 ~ "Alto",
      TRUE ~ "Muito Alto"
    )
  )

# Tabela de resumo
resumo_bias <- classificacao_bias %>%
  count(tipo_vies, magnitude_vies) %>%
  pivot_wider(names_from = tipo_vies, values_from = n, values_fill = 0)

cat("\n📋 Resumo de viés por tipo:\n\n")
print(resumo_bias)

# Top 10 com menor viés absoluto
cat("\n🏆 Top 10 métodos com menor viés absoluto:\n\n")
classificacao_bias %>%
  arrange(rank_bias) %>%
  select(metodo, familia, bias_medio, bias_abs_medio, tipo_vies) %>%
  head(10) %>%
  print()

# Visualização: Ranking de viés
p_ranking_bias <- classificacao_bias %>%
  arrange(bias_abs_medio) %>%
  head(15) %>%
  mutate(metodo = fct_reorder(metodo, bias_abs_medio)) %>%
  ggplot(aes(x = bias_abs_medio, y = metodo, fill = tipo_vies)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", bias_medio)), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Neutro" = "#00A087FF", 
               "Subestima" = "#E64B35FF", 
               "Superestima" = "#4DBBD5FF")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Métodos com Menor Viés Sistemático",
    subtitle = "Top 15 métodos ordenados por |Bias| - Valores próximos a zero são desejáveis",
    x = "|Bias Médio| (Valor Absoluto)",
    y = NULL,
    fill = "Tipo de Viés",
    caption = "Valores positivos indicam subestimação; negativos indicam superestimação"
  ) +
  theme_dissertacao()

ggsave(
  here("output/figures/07b_multimetric/07_ranking_menor_bias.png"),
  plot = p_ranking_bias,
  width = 11, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 07_ranking_menor_bias.png\n")

# ===========================================================================
# BLOCO 8: ANÁLISE ESPECÍFICA DE PER ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 8: ANÁLISE DE ACERTO DE PADRÃO (PER)\n")
cat(strrep("=", 70), "\n\n")

log_message("Análise de Period Error Rate", "INFO")

cat("📊 8.1. Métodos com melhor acerto de padrão zero/não-zero...\n")

# Top 15 por PER
top15_per <- rankings_todas_metricas %>%
  arrange(rank_per) %>%
  head(15) %>%
  mutate(
    metodo = fct_reorder(metodo, per_medio),
    per_pct = per_medio * 100
  )

cat("\n🏆 Top 15 métodos por PER (acerto de padrão):\n\n")
print(top15_per %>% select(metodo, familia, per_medio, per_pct, rank_per))

# Visualização
p_ranking_per <- ggplot(top15_per, aes(x = per_pct, y = metodo, fill = familia)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", per_pct)), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = cores_familia) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Métodos com Melhor Acerto de Padrão (PER)",
    subtitle = "Period Error Rate: % de períodos onde método acertou se demanda seria zero ou não-zero",
    x = "PER (%)",
    y = NULL,
    fill = "Família",
    caption = "Métrica específica para demanda intermitente - Avalia acerto de padrão, não magnitude"
  ) +
  theme_dissertacao() +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/08_ranking_per.png"),
  plot = p_ranking_per,
  width = 11, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 08_ranking_per.png\n")

# Comparar PER com MAE
cat("\n🔍 Correlação entre PER e MAE:\n")
cor_per_mae <- cor(rankings_todas_metricas$per_medio, 
                   rankings_todas_metricas$mae_medio,
                   method = "spearman")
cat(sprintf("   - Correlação de Spearman: ρ = %.3f\n", cor_per_mae))

if(abs(cor_per_mae) < 0.5) {
  cat("   ⚠️  Baixa correlação: Acertar o padrão ≠ Ter baixo erro de magnitude\n")
  cat("      Métodos podem acertar se é zero/não-zero mas errar a quantidade!\n")
} else {
  cat("   ✅ Boa correlação: Métodos que acertam padrão também têm baixo MAE\n")
}

# ===========================================================================
# BLOCO 9: SÍNTESE - MÉTODO "GENERALISTA" VS. "ESPECIALISTA" ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 9: IDENTIFICAÇÃO DE GENERALISTAS E ESPECIALISTAS\n")
cat(strrep("=", 70), "\n\n")

log_message("Classificando métodos por consistência multi-métrica", "INFO")

cat("📊 9.1. Identificando métodos generalistas...\n")

# Calcular variabilidade de ranks entre métricas
variabilidade_ranks <- rankings_todas_metricas %>%
  mutate(
    rank_range = pmax(rank_mae, rank_rmse, rank_linlin, 
                      rank_mad_mean, rank_bias, rank_per) -
      pmin(rank_mae, rank_rmse, rank_linlin, 
           rank_mad_mean, rank_bias, rank_per),
    
    rank_sd = sqrt((
      (rank_mae - rank_medio_geral)^2 +
        (rank_rmse - rank_medio_geral)^2 +
        (rank_linlin - rank_medio_geral)^2 +
        (rank_mad_mean - rank_medio_geral)^2 +
        (rank_bias - rank_medio_geral)^2 +
        (rank_per - rank_medio_geral)^2
    ) / 6),
    
    tipo_metodo = case_when(
      rank_sd < 3 ~ "Generalista (consistente)",
      rank_sd < 6 ~ "Moderado",
      TRUE ~ "Especialista (trade-offs)"
    )
  )

# Top 10 generalistas (menor variabilidade)
cat("\n🏆 Top 10 GENERALISTAS (consistente em todas as métricas):\n\n")
variabilidade_ranks %>%
  arrange(rank_sd) %>%
  head(10) %>%
  select(metodo, familia, rank_medio_geral, rank_sd, rank_range, tipo_metodo) %>%
  print()

# Top 10 especialistas (maior variabilidade)
cat("\n⚡ Top 10 ESPECIALISTAS (trade-offs entre métricas):\n\n")
variabilidade_ranks %>%
  arrange(desc(rank_sd)) %>%
  head(10) %>%
  select(metodo, familia, rank_medio_geral, rank_sd, rank_range, tipo_metodo) %>%
  print()

# Visualização: Consistência vs. Desempenho
p_generalista_especialista <- ggplot(variabilidade_ranks, 
                                     aes(x = rank_medio_geral, y = rank_sd, 
                                         color = familia)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(
    data = variabilidade_ranks %>% 
      filter(rank_medio_geral <= 10 | rank_sd > 8),
    aes(label = metodo),
    hjust = -0.2,
    vjust = 0,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "gray50") +
  annotate("rect", xmin = -Inf, xmax = 10, ymin = -Inf, ymax = 3,
           alpha = 0.05, fill = "green") +
  annotate("text", x = 5, y = 1, 
           label = "IDEAL:\nTop 10 + Consistente", 
           hjust = 0.5, size = 3.5, fontface = "bold", color = "darkgreen") +
  scale_color_manual(values = cores_familia) +
  labs(
    title = "Generalistas vs. Especialistas: Desempenho e Consistência",
    subtitle = "Desvio-padrão baixo = consistente em todas as métricas; Alto = trade-offs",
    x = "Ranking Médio Geral (menor = melhor)",
    y = "Desvio-padrão dos Rankings entre Métricas",
    color = "Família",
    caption = "Quadrante inferior esquerdo = ideal (bom desempenho + consistente)"
  ) +
  theme_dissertacao() +
  guides(color = guide_legend(nrow = 2))

ggsave(
  here("output/figures/07b_multimetric/09_generalista_especialista.png"),
  plot = p_generalista_especialista,
  width = 12, height = 8, dpi = 300
)

cat("   ✅ Gráfico salvo: 09_generalista_especialista.png\n")

# ===========================================================================
# BLOCO 10: TABELA DE SÍNTESE MULTI-MÉTRICA ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("BLOCO 10: TABELA DE SÍNTESE PARA DISSERTAÇÃO\n")
cat(strrep("=", 70), "\n\n")

log_message("Gerando tabela de síntese multi-métrica", "INFO")

# Top 15 com todas as métricas
tabela_sintese <- variabilidade_ranks %>%
  arrange(rank_medio_geral) %>%
  head(15) %>%
  select(
    Método = metodo,
    Família = familia,
    `MAE` = mae_medio,
    `RMSE` = rmse_medio,
    `LinLin` = linlin_medio,
    `Bias` = bias_medio,
    `PER (%)` = per_medio,
    `Rank Médio` = rank_medio_geral,
    `DP Rank` = rank_sd
  ) %>%
  mutate(
    `PER (%)` = `PER (%)` * 100,
    across(where(is.numeric), ~round(., 2))
  )

# Salvar em Excel
library(writexl)
write_xlsx(
  list("Sintese_Multi_Metrica" = tabela_sintese),
  here("output/tables/07b_sintese_multimetrica.xlsx")
)

cat("✅ Tabela salva: 07b_sintese_multimetrica.xlsx\n")

# Imprimir no console
cat("\n📋 TOP 15 MÉTODOS - SÍNTESE MULTI-MÉTRICA:\n\n")
print(tabela_sintese)

# ===========================================================================
# RELATÓRIO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL - ANÁLISE MULTI-MÉTRICA\n")
cat(strrep("=", 70), "\n\n")

cat("📊 FIGURAS GERADAS:\n\n")

figuras_geradas <- list.files(
  here("output/figures/07b_multimetric"),
  pattern = "\\.png$",
  full.names = FALSE
)

for(i in seq_along(figuras_geradas)) {
  cat(sprintf("   %d. %s\n", i, figuras_geradas[i]))
}

cat("\n🎯 PRINCIPAIS ACHADOS:\n\n")

# 1. Correlação entre métricas
cat("1. CONSISTÊNCIA ENTRE MÉTRICAS:\n")
cat(sprintf("   - MAE vs RMSE: ρ = %.3f\n", cor_matrix["MAE", "RMSE"]))
cat(sprintf("   - MAE vs LinLin: ρ = %.3f\n", cor_matrix["MAE", "LinLin"]))
cat(sprintf("   - MAE vs PER: ρ = %.3f\n", cor_matrix["MAE", "PER"]))

if(cor_matrix["MAE", "RMSE"] > 0.9 && cor_matrix["MAE", "LinLin"] > 0.9) {
  cat("\n   ✅ CONCLUSÃO: Rankings são altamente consistentes entre MAE, RMSE e LinLin\n")
  cat("      → Escolha de métrica NÃO altera conclusões principais\n")
} else {
  cat("\n   ⚠️  CONCLUSÃO: Rankings divergem entre métricas\n")
  cat("      → Escolha de métrica AFETA conclusões\n")
}

# 2. Métodos no top 10 de todas
if(length(intersecao_todos) > 0) {
  cat(sprintf("\n2. MÉTODOS NO TOP 10 DE MAE, RMSE E LinLin: %d\n", 
              length(intersecao_todos)))
  cat("   Métodos robustos a escolha de métrica:\n")
  for(m in intersecao_todos) {
    cat(sprintf("   - %s\n", m))
  }
}

# 3. Bias
metodos_baixo_bias <- classificacao_bias %>%
  filter(tipo_vies == "Neutro") %>%
  nrow()

cat(sprintf("\n3. VIÉS SISTEMÁTICO: %d métodos são neutros (|Bias| < 0.5)\n", 
            metodos_baixo_bias))

# 4. Generalistas
metodos_generalistas <- variabilidade_ranks %>%
  filter(tipo_metodo == "Generalista (consistente)") %>%
  nrow()

cat(sprintf("\n4. GENERALISTAS: %d métodos têm desempenho consistente em todas as métricas\n", 
            metodos_generalistas))

cat("\n💡 RECOMENDAÇÕES:\n\n")
cat("1. Usar MAE como métrica primária (consenso da literatura)\n")
cat("2. Complementar com LinLin para contexto de custos assimétricos\n")
cat("3. Monitorar Bias para evitar super/subestimação sistemática\n")
cat("4. Avaliar PER em cenários onde padrão zero/não-zero é crítico\n")
cat("5. Priorizar métodos GENERALISTAS para robustez operacional\n")

cat("\n", strrep("=", 70), "\n", sep = "")

log_message("========================================", "INFO")
log_message("ANÁLISE MULTI-MÉTRICA FINALIZADA", "INFO")
log_message("========================================", "INFO")

cat("\n✅ Script 07b finalizado em:", format(Sys.time()), "\n\n")