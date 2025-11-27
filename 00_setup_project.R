# =============================================================================
# SCRIPT DE SETUP - PROJETO PREVISÃO DE DEMANDA
# =============================================================================
# Descrição: Script para inicializar a estrutura completa do projeto
# Autor: Luiz Antonio Rezende
# Data: 2025-8-1
# Última atualização: 2025-11-27
# =============================================================================

# Limpar ambiente
rm(list = ls())
gc()

# =============================================================================
# 1. VERIFICAR E INSTALAR PACOTES NECESSÁRIOS
# =============================================================================

# Função para instalar pacotes se não estiverem instalados
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat(sprintf("📦 Instalando %d pacote(s): %s\n", 
                length(new_packages), 
                paste(new_packages, collapse = ", ")))
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("✅ Todos os pacotes já estão instalados!\n")
  }
}

# Lista de pacotes essenciais
required_packages <- c(
  # Manipulação e Transformação de Dados:
  "tidyverse", "janitor", "lubridate", "readxl", "writexl", "here",
  
  # Análise Exploratória e Estatísticas Descritivas:
  "skimr", "rstatix", "broom",
  
  # Séries Temporais e Previsão:
  "forecast", "tsintermittent", "smooth", "fable", "fabletools", "tsibble",
  
  # Análise de Redes e Grafos:
  "igraph",
  
  # Visualização:
  "ggplot2", "patchwork", "ggrepel", "ggthemes", "ggsci", "viridis", "scales", 
  "corrplot", "treemapify", "plotly",
  
  # Tabelas e Relatórios:
  "kableExtra", "DT",
  
  # Computação Paralela e Monitoramento:
  "future", "furrr", "progressr", "tictoc",
  
  # Utilitários Gerais:
  "magrittr", "yaml", "usethis", "stats", "mgcv"
)

# Instalar pacotes
cat("📦 Instalando pacotes necessários...\n")
install_if_missing(required_packages)

# Carregar pacotes principais
library(here)
library(usethis)
library(yaml)
library(tidyverse)
library(magrittr)

# =============================================================================
# 2. CRIAR ESTRUTURA DE PASTAS
# =============================================================================

cat("📁 Criando estrutura de pastas...\n")

# Definir estrutura de pastas
folders <- c(
  "data/raw",
  "data/processed", 
  "data/external",
  "R/functions",
  "R/analysis",
  "R/modeling",
  "R/utils",
  "scripts",
  "output/figures",
  "output/tables",
  "output/models",
  "output/reports",
  "docs",
  "config",
  "tests"
)

# Criar pastas
for(folder in folders) {
  dir.create(here(folder), recursive = TRUE, showWarnings = FALSE)
}

# Limpar ambiente
rm(list = ls())
gc()