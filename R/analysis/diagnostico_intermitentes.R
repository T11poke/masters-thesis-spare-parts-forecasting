# DIAGNÓSTICO: Família 3 - Métodos Intermitentes ####
#
# Investigar por que Croston, SBA e TSB têm 0% de convergência

library(here)
library(tidyverse)

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║   🔍 DIAGNÓSTICO: FAMÍLIA 3 - MÉTODOS INTERMITENTES        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ===========================================================================
# 1. CARREGAR DADOS ####
# ===========================================================================

cat("📦 1. CARREGANDO DADOS...\n\n")

consolidado <- readRDS(here("output/forecasts/forecasts_consolidated.rds"))
metricas_mensais <- consolidado$metricas_mensais

# Filtrar apenas métodos intermitentes
metodos_intermitentes <- metricas_mensais %>%
  filter(familia == "Familia_3_Intermitentes")

cat(sprintf("✅ Total de previsões intermitentes: %s\n", 
            format(nrow(metodos_intermitentes), big.mark = ",")))
cat(sprintf("   Métodos encontrados: %s\n", 
            paste(unique(metodos_intermitentes$metodo), collapse = ", ")))

# ===========================================================================
# 2. ANÁLISE DE CONVERGÊNCIA ####
# ===========================================================================

cat("\n📊 2. ANÁLISE DETALHADA DE CONVERGÊNCIA...\n\n")

# Convergência por método
conv_por_metodo <- metodos_intermitentes %>%
  group_by(metodo) %>%
  summarise(
    n_total = n(),
    n_convergido = sum(convergence),
    n_falhou = sum(!convergence),
    taxa_convergencia = n_convergido / n_total * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(taxa_convergencia))

cat("   Convergência por método intermitente:\n\n")
print(conv_por_metodo)

# ===========================================================================
# 3. INVESTIGAR NOMES DOS MÉTODOS ####
# ===========================================================================

cat("\n📊 3. INVESTIGANDO NOMES DOS MÉTODOS...\n\n")

cat("   Nomes únicos de métodos encontrados:\n")
cat(sprintf("   %s\n", paste(unique(metodos_intermitentes$metodo), collapse = ", ")))

# Verificar se há variações de case
cat("\n   Verificando variações de nomenclatura:\n")
todos_metodos <- unique(metricas_mensais$metodo)

metodos_suspeitos <- todos_metodos[str_detect(tolower(todos_metodos), 
                                               "croston|sba|tsb")]

if(length(metodos_suspeitos) > 0) {
  cat("   Possíveis métodos intermitentes com nomenclatura diferente:\n")
  for(m in metodos_suspeitos) {
    familia <- unique(metricas_mensais$familia[metricas_mensais$metodo == m])
    n_obs <- sum(metricas_mensais$metodo == m)
    cat(sprintf("     - '%s' → Família: %s | N: %s\n", 
                m, familia, format(n_obs, big.mark = ",")))
  }
} else {
  cat("   ⚠️  PROBLEMA: Nenhum método intermitente encontrado!\n")
}

# ===========================================================================
# 4. VERIFICAR DADOS ORIGINAIS ####
# ===========================================================================

cat("\n📊 4. VERIFICANDO DADOS ORIGINAIS (forecasts_intermittent.rds)...\n\n")

# Carregar dados originais
forecasts_intermittent <- readRDS(
  here("output/forecasts/intermittent/forecasts_intermittent.rds")
)

cat(sprintf("   Origens no arquivo original: %d\n", 
            length(forecasts_intermittent)))

# Pegar primeira origem
origem_1 <- forecasts_intermittent[[1]]

cat(sprintf("   Materiais na origem 1: %d\n", 
            length(origem_1$forecasts)))

if(length(origem_1$forecasts) > 0) {
  
  # Pegar primeiro material
  primeiro_material <- origem_1$forecasts[[1]]
  
  cat(sprintf("   CD Material: %s\n", primeiro_material$cd_material))
  cat(sprintf("   Métodos no material: %d\n", 
              length(primeiro_material$forecasts)))
  cat(sprintf("   Nomes dos métodos: %s\n",
              paste(names(primeiro_material$forecasts), collapse = ", ")))
  
  # Verificar convergência do primeiro método
  if(length(primeiro_material$forecasts) > 0) {
    primeiro_metodo <- primeiro_material$forecasts[[1]]
    cat(sprintf("\n   Detalhes do primeiro método (%s):\n", 
                names(primeiro_material$forecasts)[1]))
    cat(sprintf("     - Convergence: %s\n", primeiro_metodo$convergence))
    cat(sprintf("     - Point forecast length: %d\n", 
                length(primeiro_metodo$point)))
    cat(sprintf("     - Tem NAs? %s\n", 
                any(is.na(primeiro_metodo$point))))
    
    if(!is.null(primeiro_metodo$error_message) && 
       !is.na(primeiro_metodo$error_message)) {
      cat(sprintf("     - Error message: %s\n", 
                  primeiro_metodo$error_message))
    }
  }
  
} else {
  cat("   ⚠️  PROBLEMA: Nenhum material com forecasts!\n")
}

# ===========================================================================
# 5. VERIFICAR PROCESSO DE NORMALIZAÇÃO ####
# ===========================================================================

cat("\n📊 5. VERIFICANDO PROCESSO DE NORMALIZAÇÃO DE NOMES...\n\n")

# Simular normalização
nomes_originais <- c("croston", "sba", "tsb")

cat("   Normalização esperada:\n")
for(nome in nomes_originais) {
  normalizado <- str_to_title(nome)
  normalizado <- str_replace_all(normalizado, c("Sba" = "SBA", "Tsb" = "TSB"))
  cat(sprintf("     '%s' → '%s'\n", nome, normalizado))
}

# Verificar o que realmente aconteceu
cat("\n   Métodos intermitentes no consolidado:\n")
metodos_inter_consolidado <- metricas_mensais %>%
  filter(str_detect(tolower(metodo), "croston|sba|tsb")) %>%
  distinct(metodo, familia) %>%
  arrange(metodo)

if(nrow(metodos_inter_consolidado) > 0) {
  print(metodos_inter_consolidado)
} else {
  cat("     ⚠️  NENHUM método intermitente encontrado!\n")
}

# ===========================================================================
# 6. DIAGNÓSTICO FINAL ####
# ===========================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("DIAGNÓSTICO FINAL\n")
cat(strrep("=", 70), "\n\n")

# Hipótese 1: Métodos não foram consolidados
n_metodos_inter <- sum(str_detect(tolower(metricas_mensais$metodo), 
                                   "croston|sba|tsb"))

if(n_metodos_inter == 0) {
  cat("🔴 HIPÓTESE 1: Métodos intermitentes NÃO foram consolidados\n")
  cat("   Possível causa: Problema no BLOCO 3 do script 05\n")
  cat("   Solução: Verificar se fc_intermittent_list estava vazio\n\n")
}

# Hipótese 2: Métodos foram consolidados mas com nome errado
metodos_com_familia_errada <- metricas_mensais %>%
  filter(str_detect(tolower(metodo), "croston|sba|tsb"),
         familia != "Familia_3_Intermitentes") %>%
  distinct(metodo, familia)

if(nrow(metodos_com_familia_errada) > 0) {
  cat("🔴 HIPÓTESE 2: Métodos intermitentes classificados na família errada\n")
  cat("   Métodos encontrados:\n")
  print(metodos_com_familia_errada)
  cat("\n   Possível causa: Problema na função categorizar_familia_metodo()\n")
  cat("   Solução: Corrigir normalização de nomes\n\n")
}

# Hipótese 3: Métodos convergiram mas convergence = FALSE
metodos_nao_convergiram <- metodos_intermitentes %>%
  filter(!convergence) %>%
  distinct(metodo) %>%
  pull(metodo)

if(length(metodos_nao_convergiram) > 0 && 
   length(metodos_nao_convergiram) == length(unique(metodos_intermitentes$metodo))) {
  cat("🔴 HIPÓTESE 3: Todos os métodos intermitentes falharam na execução\n")
  cat("   Possível causa: Erro no script 04b\n")
  cat("   Solução: Verificar logs do script 04b\n\n")
}

# Hipótese 4: Materiais elegíveis = 0
if(length(forecasts_intermittent[[1]]$forecasts) == 0) {
  cat("🔴 HIPÓTESE 4: Nenhum material elegível para métodos intermitentes\n")
  cat("   Possível causa: Filtro muito restritivo no script 04b\n")
  cat("   Solução: Revisar filtro de materiais Intermittent/Lumpy\n\n")
}

cat("\n", strrep("=", 70), "\n\n")

cat("📋 AÇÕES RECOMENDADAS:\n\n")
cat("1. Verificar quantos materiais foram processados no 04b\n")
cat("2. Checar se fc_intermittent_list estava vazio no BLOCO 3\n")
cat("3. Revisar normalização de nomes dos métodos\n")
cat("4. Executar script 04b em modo DEBUG para investigar\n\n")
