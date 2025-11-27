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
  # Gerenciamento de ambiente
  "renv",
  
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
suppressPackageStartupMessages({
  library(here)
  library(usethis)
  library(yaml)
  library(tidyverse)
  library(magrittr)
})
cat("✅ Pacotes principais carregados\n\n")

# =============================================================================
# 2. CRIAR ESTRUTURA DE PASTAS
# =============================================================================

cat("📁 Criando estrutura de pastas...\n")

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
cat("📌 Criando arquivos .gitkeep...\n")
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

for(folder in gitkeep_folders) {
  gitkeep_path <- here(folder, ".gitkeep")
  if(!file.exists(gitkeep_path)) {
    file.create(gitkeep_path)
    cat(sprintf("  ✓ %s/.gitkeep\n", folder))
  }
}

cat("\n✅ Arquivos .gitkeep criados!\n\n")

# =============================================================================
# 4. CRIAR ARQUIVO .gitignore PERSONALIZADO
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("CONFIGURAR .gitignore\n")
cat(rep("=", 80), "\n\n", sep = "")

gitignore_content <- "# =============================================================================
# .gitignore - Projeto Previsão de Demanda SISCEAB
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

# === RSTUDIO ===
.Rproj.user
.Rhistory
.RData
.Ruserdata
*.Rproj.user

# === RENV ===
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

# === SISTEMA OPERACIONAL ===
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db
desktop.ini

# === DOCUMENTOS TEMPORÁRIOS ===
*.docx
*.pptx
!docs/*.docx
!docs/*.pptx
"

# Criar ou atualizar .gitignore
gitignore_path <- here(".gitignore")
if(file.exists(gitignore_path)) {
  cat("⚠️  .gitignore já existe. Fazendo backup...\n")
  file.copy(gitignore_path, here(".gitignore.backup"), overwrite = TRUE)
}

writeLines(gitignore_content, gitignore_path)
cat("✅ .gitignore configurado!\n\n")

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
# 6. CRIAR README.md
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("README.md\n")
cat(rep("=", 80), "\n\n", sep = "")

readme_content <- "# Previsão de Demanda para Sobressalentes do SISCEAB

**Dissertação de Mestrado em Logística**

Análise comparativa de métodos de previsão de demanda intermitente aplicados ao contexto de sobressalentes eletrônicos e eletromecânicos do Sistema de Controle do Espaço Aéreo Brasileiro (SISCEAB).

## 👤 Autor

**Luiz Antonio Rezende**  
Mestrando em Logística  
PUC-Rio

## 🎯 Objetivos

### Objetivo Geral
Avaliar comparativamente o desempenho de diferentes métodos de previsão de demanda aplicados a sobressalentes e consumíveis do SISCEAB, visando identificar abordagens que otimizem a disponibilidade operacional dos sistemas críticos de controle de tráfego aéreo.

### Objetivos Específicos
- Caracterizar o padrão de demanda histórica segundo taxonomia SBC
- Implementar e parametrizar 15+ métodos de previsão
- Estabelecer métricas apropriadas para demanda intermitente
- Conduzir análise comparativa com validação out-of-sample
- Propor recomendações metodológicas para o DECEA

## 📊 Estrutura do Projeto

\`\`\`
.
├── data/
│   ├── raw/              # Dados SILOMS (CONFIDENCIAIS - não versionados)
│   ├── processed/        # Dados limpos e consolidados
│   ├── interim/          # Dados intermediários
│   └── external/         # Dados externos complementares
├── R/
│   ├── functions/        # Funções customizadas
│   ├── analysis/         # Scripts de análise exploratória
│   ├── modeling/         # Scripts de modelagem
│   ├── validation/       # Scripts de validação
│   └── utils/            # Funções utilitárias
├── scripts/              # Scripts principais (workflow)
├── output/
│   ├── figures/          # Gráficos e visualizações
│   ├── tables/           # Tabelas de resultados
│   ├── models/           # Modelos salvos (.rds)
│   └── reports/          # Relatórios finais
├── docs/                 # Documentação adicional
├── config/               # Arquivos de configuração
│   └── config.yaml       # Configurações do projeto
└── logs/                 # Logs de execução

\`\`\`

## 🔬 Métodos de Previsão

### Família 1: Métodos Clássicos (Benchmarks)
- Naive
- Média Simples
- Média Móvel (k=36 meses)

### Família 2: Suavização Exponencial e Séries Temporais
- ARIMA (AutoRegressive Integrated Moving Average)
- ETS (Error, Trend, Seasonal)
- Holt-Winters Aditivo e Multiplicativo
- TSLM (Time Series Linear Model)

### Família 3: Métodos Especializados (Demanda Intermitente)
- **Croston Clássico** (1972)
- **SBA** - Syntetos-Boylan Approximation (2005)
- **TSB** - Teunter-Syntetos-Babai (2011)

### Família 4: Métodos Probabilísticos
- **Distribuição de Poisson** (método atual do DECEA)
- **Distribuição Gama**

### Família 5: Agregação Temporal
- **ADIDA** - Aggregate-Disaggregate Intermittent Demand Approach

## 📏 Métricas de Avaliação

- **MAE** (Mean Absolute Error) - métrica primária
- **RMSE** (Root Mean Squared Error)
- **Bias** (Mean Error)
- **LinLin** (Função de Perda Assimétrica, p=0.85)
- **MAD/Mean Ratio**

**Perspectivas de avaliação:**
1. **Mensal**: acurácia mês a mês (12 previsões)
2. **Anual agregada**: demanda total de 12 meses (planejamento orçamentário)

## 🔄 Estratégia de Validação

- **Método**: Rolling Origin com janela expansiva
- **Origens**: 4 pontos temporais
- **Horizonte**: 12 meses (h=12)
- **Treino mínimo**: 36 meses

## 🚀 Como Executar

### 1. Configuração Inicial

\`\`\`r
# Executar setup completo (apenas primeira vez)
source('00_setup_project.R')

# Inicializar renv
renv::init()
\`\`\`

### 2. Restaurar Ambiente (projetos clonados)

\`\`\`r
# Restaurar pacotes
renv::restore()
\`\`\`

### 3. Workflow de Análise

\`\`\`r
# Scripts principais em scripts/
....
....
....
...
...
...
...
...
...
\`\`\`

## 📦 Pacotes Principais

- **Manipulação**: `tidyverse`, `janitor`, `lubridate`
- **Séries Temporais**: `forecast`, `tsintermittent`, `fable`
- **Visualização**: `ggplot2`, `patchwork`, `plotly`
- **Redes**: `igraph` (consolidação de materiais alternados)
- **Relatórios**: `kableExtra`, `DT`

## ⚠️ Confidencialidade

**IMPORTANTE**: Este projeto contém dados operacionais confidenciais do DECEA/SISCEAB.

- Dados em `data/raw/` e `data/interim/` **NÃO** são versionados
- Arquivos `.csv`, `.xlsx`, `.rds` com dados reais estão no `.gitignore`
- Apenas código metodológico e estrutura são compartilhados no Git

## 📚 Referências Principais

- **BOYLAN, J. E.; SYNTETOS, A. A.** Intermittent Demand Forecasting: Context, Methods and Applications. Wiley, 2021.

- **SYNTETOS, A. A.; BOYLAN, J. E.; CROSTON, J. D.** On the categorization of demand patterns. Journal of the Operational Research Society, v. 56, n. 5, p. 495-503, 2005.

- **PETROPOULOS, F. et al.** Forecasting: theory and practice. International Journal of Forecasting, v. 38, n. 3, p. 705-871, 2022.

- **TEUNTER, R. H.; SYNTETOS, A. A.; BABAI, M. Z.** Intermittent demand: Linking forecasting to inventory obsolescence. European Journal of Operational Research, v. 214, n. 3, p. 606-615, 2011.

## 📧 Contato

[Seu email institucional]

---

**Status**: 🚧 Em desenvolvimento  
**Última atualização**: 2025-11-27  
**Versão**: 2.0.0
"

readme_path <- here("README.md")
if(file.exists(readme_path)) {
  cat("⚠️  README.md já existe. Fazendo backup...\n")
  file.copy(readme_path, here("README.backup.md"), overwrite = TRUE)
}

writeLines(readme_content, readme_path)
cat("✅ README.md criado!\n\n")

# =============================================================================
# 7. INICIALIZAR RENV
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("INICIALIZAR RENV\n")
cat(rep("=", 80), "\n\n", sep = "")

if(!require("renv", quietly = TRUE)) {
  install.packages("renv")
}

cat("🔧 Inicializando renv...\n")
cat("   (Isso pode demorar alguns minutos na primeira vez)\n\n")

# Inicializar renv se ainda não estiver
if(!file.exists(here("renv.lock"))) {
  renv::init(bare = TRUE)
  cat("✅ renv inicializado!\n")
  cat("   Execute 'renv::snapshot()' após instalar todos os pacotes\n\n")
} else {
  cat("✅ renv já está inicializado!\n\n")
}

# =============================================================================
# 8. CRIAR FUNÇÕES UTILITÁRIAS
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("FUNÇÕES UTILITÁRIAS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Função para carregar configuração
load_config_content <- "# =============================================================================
# FUNÇÃO: Carregar Configuração
# =============================================================================

#' Carregar arquivo de configuração YAML
#'
#' @param config_file Nome do arquivo de configuração (padrão: config.yaml)
#' @return Lista com configurações do projeto
#' @export
load_config <- function(config_file = 'config.yaml') {
  config_path <- here::here('config', config_file)
  
  if(!file.exists(config_path)) {
    stop(sprintf('Arquivo de configuração não encontrado: %s', config_path))
  }
  
  config <- yaml::read_yaml(config_path)
  return(config)
}

#' Função de logging com timestamp
#'
#' @param msg Mensagem para log
#' @param level Nível do log (INFO, WARNING, ERROR)
#' @export
log_message <- function(msg, level = 'INFO') {
  timestamp <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  log_msg <- sprintf('[%s] [%s] %s', timestamp, level, msg)
  cat(log_msg, '\\n')
  
  # Salvar em arquivo de log se existir pasta logs/
  if(dir.exists(here::here('logs'))) {
    log_file <- here::here('logs', sprintf('log_%s.txt', Sys.Date()))
    cat(log_msg, '\\n', file = log_file, append = TRUE)
  }
}

# Configurar seed global
if(exists('config')) {
  set.seed(config$parameters$seed)
}
"

writeLines(load_config_content, here("R/utils/load_config.R"))
cat("✅ Funções utilitárias criadas em R/utils/\n\n")

# =============================================================================
# 9. RESUMO FINAL
# =============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ SETUP CONCLUÍDO COM SUCESSO!🚀\n")
cat(rep("=", 80), "\n\n", sep = "")

# Limpar ambiente
rm(list = ls())
gc()



