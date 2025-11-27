# =============================================================================
# SCRIPT DE SETUP - PROJETO PREVISÃO DE DEMANDA
# =============================================================================
# Descrição: Script para inicializar a estrutura completa do projeto
# Autor: Luiz Antonio Rezende
# Data: 2025-08-12
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
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Lista de pacotes essenciais
required_packages <- c(
  # Gerenciamento de projeto
  "here", "renv", "usethis",
  
  # Tidyverse e manipulação de dados
  "tidyverse", "magrittr", "purrr", "janitor", "skimr", "igraph",
  
  # Leitura e escrita de dados
  "readxl", "writexl",
  
  # Datas e tempo
  "lubridate", "tsibble", "zoo",
  
  # Visualização básica
  "plotly", "corrplot", "scales",
  
  # Visualização avançada e temas
  "ggthemes", "ggsci", "patchwork", "treemap", "RColorBrewer", "treemapify",
  
  # Séries temporais e previsão
  "forecast", "fable", "fabletools", "tseries", "prophet",
  
  # Configuração e utilitários
  "config", "yaml", "DT", "knitr", "rmarkdown",
  
  # Outros a serem classificados
  "furrr", "smooth", "tictoc", "tsintermittent"
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