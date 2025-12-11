# =============================================================================
# SCRIPT DE SETUP - PROJETO PREVISÃO DE DEMANDA SISCEAB
# =============================================================================
# Descrição: Script para inicializar a estrutura completa do projeto
# Autor: Luiz Antonio Rezende
# Data: 2025-11-27
# Versão: 2.1.1
# =============================================================================

# Limpar ambiente
rm(list = ls())
gc()

cat("\n", rep("=", 80), "\n", sep = "")
cat("SETUP DO PROJETO - PREVISÃO DE DEMANDA SISCEAB\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# 1. INSTALAR PACOTES MÍNIMOS NECESSÁRIOS
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("INSTALAR PACOTES ESSENCIAIS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Pacotes MÍNIMOS necessários para o script de setup funcionar
minimal_packages <- c("here", "yaml", "renv")

cat("📦 Instalando pacotes essenciais (biblioteca global)...\n")
for(pkg in minimal_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("   → Instalando %s...\n", pkg))
    install.packages(pkg, quiet = TRUE)
  } else {
    cat(sprintf("   ✓ %s já instalado\n", pkg))
  }
}

# Carregar pacotes essenciais
library(here)
library(yaml)

cat("\n✅ Pacotes essenciais carregados!\n\n")

# =============================================================================
# 2. CRIAR ESTRUTURA DE PASTAS
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("ESTRUTURA DE PASTAS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Definir estrutura de pastas
folders <- c(
  "data/raw",
  "data/processed", 
  "data/interim",
  "data/external",
  "R/functions",
  "R/analysis",
  "R/modeling",
  "R/validation",
  "R/utils",
  "scripts",
  "output/figures",
  "output/tables",
  "output/models",
  "output/reports",
  "docs",
  "config",
  "tests",
  "logs"
)

# Criar pastas
cat("📁 Criando estrutura de diretórios...\n")
for(folder in folders) {
  if(!dir.exists(here(folder))) {
    dir.create(here(folder), recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("  ✓ %s\n", folder))
  } else {
    cat(sprintf("  → %s (já existe)\n", folder))
  }
}

cat("\n✅ Estrutura de pastas criada!\n\n")

# =============================================================================
# 3. CRIAR ARQUIVOS .gitkeep
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("ARQUIVOS .gitkeep\n")
cat(rep("=", 80), "\n\n", sep = "")

# Pastas que precisam de .gitkeep
gitkeep_folders <- c(
  "data/raw",
  "data/interim",
  "data/processed",
  "data/external",
  "R/functions",
  "R/analysis",
  "R/modeling",
  "R/validation",
  "R/utils",
  "output/figures",
  "output/tables",
  "output/models",
  "logs"
)

cat("📌 Criando arquivos .gitkeep...\n")
for(folder in gitkeep_folders) {
  gitkeep_path <- here(folder, ".gitkeep")
  if(!file.exists(gitkeep_path)) {
    file.create(gitkeep_path)
    cat(sprintf("  ✓ %s/.gitkeep\n", folder))
  }
}

cat("\n✅ Arquivos .gitkeep criados!\n\n")

# =============================================================================
# 4. ADICIONAR AO ARQUIVO .gitignore
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("CONFIGURAR .gitignore\n")
cat(rep("=", 80), "\n\n", sep = "")

gitignore_additions <- "
# =============================================================================
# ADIÇÕES PARA PROJETO PREVISÃO DE DEMANDA SISCEAB
# =============================================================================

# === DADOS CONFIDENCIAIS ===
data/raw/*
data/interim/*
data/external/*.csv
data/external/*.xlsx
!data/raw/.gitkeep
!data/interim/.gitkeep

# === DADOS PROCESSADOS GRANDES ===
*.csv
*.xlsx
*.xls
*.rds
*.feather
*.parquet
data/processed/*.csv
data/processed/*.rds

# === OUTPUTS GERADOS ===
output/figures/*.png
output/figures/*.pdf
output/figures/*.jpg
output/tables/*.csv
output/tables/*.xlsx
output/models/*.rds
output/reports/*.html
output/reports/*.pdf
!output/figures/.gitkeep
!output/tables/.gitkeep
!output/models/.gitkeep

# === LOGS ===
logs/*.log
logs/*.txt
*.log

# === CACHE E TEMPORÁRIOS ===
*_cache/
*_files/
*.tmp
*.temp
~$*

# === RENV (adicional) ===
renv/library/
renv/local/
renv/cellar/
renv/lock/
renv/python/
renv/sandbox/
renv/staging/

# === CREDENCIAIS ===
.Renviron
credentials.R
config_local.R
secrets.yaml
*.env

# === DOCUMENTOS TEMPORÁRIOS ===
*.docx
*.pptx
!docs/*.docx
!docs/*.pptx
"

# Adicionar ao .gitignore existente (não substituir)
gitignore_path <- here(".gitignore")

if(file.exists(gitignore_path)) {
  # Ler conteúdo existente
  existing_content <- readLines(gitignore_path, warn = FALSE)
  
  # Verificar se já tem as adições (evitar duplicação)
  if(!any(grepl("PROJETO PREVISÃO DE DEMANDA SISCEAB", existing_content))) {
    cat("📝 .gitignore já existe. Adicionando regras específicas do projeto...\n")
    
    # Fazer backup
    backup_path <- here(".gitignore.backup")
    file.copy(gitignore_path, backup_path, overwrite = TRUE)
    cat(sprintf("   Backup criado: %s\n", basename(backup_path)))
    
    # Adicionar ao final (não substituir)
    cat(gitignore_additions, file = gitignore_path, append = TRUE)
    cat("✅ Regras adicionadas ao .gitignore existente!\n\n")
  } else {
    cat("✅ .gitignore já contém as regras do projeto!\n\n")
  }
} else {
  # Se não existir, criar do zero
  cat("📝 Criando .gitignore...\n")
  
  # Conteúdo base para R
  base_content <- "# === R BASE ===
.Rproj.user
.Rhistory
.RData
.Ruserdata

# === SISTEMA OPERACIONAL ===
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db
desktop.ini
"
  
  writeLines(c(base_content, gitignore_additions), gitignore_path)
  cat("✅ .gitignore criado!\n\n")
}

# =============================================================================
# 5. CRIAR ARQUIVO DE CONFIGURAÇÃO config.yaml
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("ARQUIVO DE CONFIGURAÇÃO\n")
cat(rep("=", 80), "\n\n", sep = "")

config_content <- "# =============================================================================
# ARQUIVO DE CONFIGURAÇÃO - Projeto Previsão de Demanda SISCEAB
# =============================================================================

project:
  name: 'Previsão de Demanda SISCEAB'
  author: 'Luiz Antonio Rezende'
  version: '1.0.0'
  description: 'Análise comparativa de métodos de previsão de demanda intermitente'

paths:
  data:
    raw: 'data/raw'
    processed: 'data/processed'
    interim: 'data/interim'
    external: 'data/external'
  output:
    figures: 'output/figures'
    tables: 'output/tables'
    models: 'output/models'
    reports: 'output/reports'
  logs: 'logs'

parameters:
  seed: 42
  validation:
    n_origins: 4
    test_months: 12
    train_min_months: 36
  forecasting:
    horizon: 12
    methods:
      - 'naive'
      - 'mean'
      - 'moving_average'
      - 'croston'
      - 'sba'
      - 'tsb'
      - 'arima'
      - 'ets'
      - 'poisson'
      - 'gamma'
      - 'adida'
  metrics:
    - 'mae'
    - 'rmse'
    - 'bias'
    - 'linlin'
    - 'mad_mean_ratio'
  sbc:
    adi_threshold: 1.32
    cv2_threshold: 0.49

computation:
  parallel: true
  n_cores: 8

output:
  save_intermediate: true
  figures:
    format: 'png'
    dpi: 300
    width: 10
    height: 6
  tables:
    format: 'csv'
"

writeLines(config_content, here("config", "config.yaml"))
cat("✅ Arquivo config.yaml criado em config/\n\n")

# =============================================================================
# 6. INSTALAR PACOTES DO PROJETO
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("INSTALAR PACOTES DO PROJETO\n")
cat(rep("=", 80), "\n\n", sep = "")

# Lista completa de pacotes do projeto
project_packages <- c(
  # Manipulação e Transformação de Dados:
  "tidyverse", "janitor", "lubridate", "readxl", "writexl",
  
  # Análise Exploratória e Estatísticas Descritivas:
  "skimr", "rstatix", "broom",
  
  # Séries Temporais e Previsão:
  "forecast", "tsintermittent", "smooth", "fable", "fabletools", "tsibble",
  "tsutils",
  
  # Análise de Redes e Grafos:
  "igraph",
  
  # Visualização:
  "ggplot2", "patchwork", "ggrepel", "ggthemes", "ggsci", "viridis", "scales", 
  "corrplot", "treemapify", "plotly", "ggridges", "ggpubr", "gtExtras",
  
  # Tabelas e Relatórios:
  "kableExtra", "DT", "GT",
  
  # Computação Paralela e Monitoramento:
  "future", "furrr", "progressr", "tictoc",
  
  # Utilitários Gerais:
  "magrittr", "usethis", "stats", "mgcv"
)

cat("ℹ️  Os pacotes serão instalados no ambiente isolado do renv.\n")
cat(sprintf("   Total de pacotes a instalar: %d\n\n", length(project_packages)))

cat("📦 Para instalar os pacotes, execute:\n\n")
cat("   install.packages(c(\n")
for(i in 1:length(project_packages)) {
  pkg <- project_packages[i]
  if(i == length(project_packages)) {
    cat(sprintf("     '%s'\n", pkg))
  } else {
    cat(sprintf("     '%s',\n", pkg))
  }
}

# =============================================================================
# 7. RESUMO FINAL
# =============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ SETUP CONCLUÍDO COM SUCESSO!\n")
cat(rep("=", 80), "\n\n", sep = "")

# Limpar ambiente
rm(list = ls())
gc()
