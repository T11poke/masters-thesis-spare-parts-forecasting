# DIAGNÓSTICO: Estrutura dos Forecasts Salvos ####
#
# Este script investiga a estrutura real dos dados salvos pelos scripts 04a/b/c
# para corrigir o script 05

library(here)
library(tidyverse)

cat("\n🔍 DIAGNÓSTICO DA ESTRUTURA DE FORECASTS\n")
cat(strrep("=", 70), "\n\n")

# Carregar forecasts
forecasts_baseline <- readRDS(here("output/forecasts/baseline/forecasts_baseline.rds"))
forecasts_intermittent <- readRDS(here("output/forecasts/intermittent/forecasts_intermittent.rds"))

cat("1️⃣ ESTRUTURA GERAL:\n\n")

cat("📦 forecasts_baseline:\n")
cat(sprintf("   - Classe: %s\n", class(forecasts_baseline)))
cat(sprintf("   - Length: %d\n", length(forecasts_baseline)))
cat(sprintf("   - Names: %s\n", paste(names(forecasts_baseline), collapse = ", ")))

cat("\n📦 forecasts_intermittent:\n")
cat(sprintf("   - Classe: %s\n", class(forecasts_intermittent)))
cat(sprintf("   - Length: %d\n", length(forecasts_intermittent)))
cat(sprintf("   - Names: %s\n", paste(names(forecasts_intermittent), collapse = ", ")))

# Inspecionar primeira origem
cat("\n\n2️⃣ ESTRUTURA DA PRIMEIRA ORIGEM (origem_1):\n\n")

origem_1_baseline <- forecasts_baseline[["origem_1"]]

cat("📋 Elementos em origem_1 (baseline):\n")
cat(sprintf("   - Names: %s\n", paste(names(origem_1_baseline), collapse = ", ")))

cat("\n📋 Estrutura de 'forecasts':\n")
cat(sprintf("   - Classe: %s\n", class(origem_1_baseline$forecasts)))
cat(sprintf("   - Length: %d materiais\n", length(origem_1_baseline$forecasts)))

if(length(origem_1_baseline$forecasts) > 0) {
  
  # Pegar primeiro material
  primeiro_material <- origem_1_baseline$forecasts[[1]]
  
  cat("\n📦 Primeiro material:\n")
  cat(sprintf("   - cd_material: %s\n", primeiro_material$cd_material))
  cat(sprintf("   - Campos disponíveis: %s\n", 
              paste(names(primeiro_material), collapse = ", ")))
  
  cat("\n📦 Forecasts do primeiro material:\n")
  cat(sprintf("   - Classe: %s\n", class(primeiro_material$forecasts)))
  cat(sprintf("   - Length: %d métodos\n", length(primeiro_material$forecasts)))
  cat(sprintf("   - Métodos: %s\n", 
              paste(names(primeiro_material$forecasts), collapse = ", ")))
  
  # Pegar primeiro método
  if(length(primeiro_material$forecasts) > 0) {
    primeiro_metodo <- primeiro_material$forecasts[[1]]
    
    cat("\n📊 Estrutura do primeiro método:\n")
    cat(sprintf("   - Campos: %s\n", paste(names(primeiro_metodo), collapse = ", ")))
    cat(sprintf("   - point (length): %d\n", length(primeiro_metodo$point)))
    cat(sprintf("   - convergence: %s\n", primeiro_metodo$convergence))
  }
}

cat("\n\n3️⃣ COMO ACESSAR OS DADOS:\n\n")
cat("✅ Estrutura correta:\n")
cat("   forecasts_baseline[[\"origem_1\"]]$forecasts  → lista de materiais\n")
cat("   forecasts_baseline[[\"origem_1\"]]$forecasts[[1]]  → dados do material 1\n")
cat("   forecasts_baseline[[\"origem_1\"]]$forecasts[[1]]$cd_material  → código\n")
cat("   forecasts_baseline[[\"origem_1\"]]$forecasts[[1]]$forecasts  → lista de métodos\n")
cat("   forecasts_baseline[[\"origem_1\"]]$forecasts[[1]]$forecasts[[\"Naive\"]]  → resultado\n")

cat("\n\n4️⃣ NOMES DOS MATERIAIS:\n\n")

# Verificar se a lista de forecasts tem nomes ou é indexada por posição
materiais_com_nome <- !is.null(names(origem_1_baseline$forecasts))

cat(sprintf("   - Lista tem nomes? %s\n", materiais_com_nome))

if(materiais_com_nome) {
  cat(sprintf("   - Primeiros 5 nomes: %s\n", 
              paste(head(names(origem_1_baseline$forecasts), 5), collapse = ", ")))
} else {
  cat("   - Lista indexada por posição (sem nomes)\n")
  cat("   - Códigos dos materiais:\n")
  
  codigos <- map_chr(origem_1_baseline$forecasts[1:min(5, length(origem_1_baseline$forecasts))], 
                     ~.x$cd_material)
  cat(sprintf("     %s\n", paste(codigos, collapse = ", ")))
}

cat("\n\n5️⃣ DIAGNÓSTICO COMPLETO:\n\n")
cat("Execute este código no script 05 para corrigir o acesso:\n\n")

if(materiais_com_nome) {
  cat("# A lista TEM nomes - usar names() diretamente\n")
  cat("materiais_baseline <- names(fc_baseline)\n")
} else {
  cat("# A lista NÃO tem nomes - extrair cd_material de cada elemento\n")
  cat("materiais_baseline <- map_chr(fc_baseline, ~.x$cd_material)\n")
  cat("names(fc_baseline) <- materiais_baseline\n")
}

cat("\n" , strrep("=", 70), "\n\n")

