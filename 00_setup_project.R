# =============================================================================
# SCRIPT DE SETUP - PROJETO PREVISÃO DE DEMANDA SISCEAB
# =============================================================================
# Descrição: Script para inicializar a estrutura completa do projeto
# Autor: Luiz Antonio Rezende
# Data: 2025-11-27
# Versão: 2.0
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
# 2. CRIAR ESTRUTURA DE PASTAS (ANTES DO RENV)
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
# 6. CRIAR FUNÇÕES UTILITÁRIAS
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
config <- load_config()
set.seed(config$parameters$seed)

cat('✅ Configuração carregada. Seed definido:', config$parameters$seed, '\\n')
"

writeLines(load_config_content, here("R/utils/load_config.R"))
cat("✅ Funções utilitárias criadas em R/utils/\n\n")

# =============================================================================
# 7. INICIALIZAR RENV (AGORA SIM!)
# =============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("INICIALIZAR AMBIENTE RENV\n")
cat(rep("=", 80), "\n\n", sep = "")

# Verificar se renv já foi inicializado
if(!file.exists(here("renv.lock"))) {
  cat("🔧 Inicializando renv (criando ambiente isolado)...\n")
  cat("   (Isso pode demorar alguns minutos)\n\n")
  
  tryCatch({
    # Inicializar com bare = TRUE para não instalar pacotes ainda
    renv::init(bare = TRUE, restart = FALSE)
    
    cat("\n✅ renv inicializado com sucesso!\n")
    cat("   Ambiente isolado criado em: renv/library/\n\n")
    
  }, error = function(e) {
    cat("\n⚠️  Erro ao inicializar renv:\n")
    cat(sprintf("   %s\n\n", e$message))
    cat("💡 Você pode tentar manualmente:\n")
    cat("   1. Reiniciar R: Session → Restart R\n")
    cat("   2. Executar: renv::init()\n\n")
  })
  
} else {
  cat("✅ renv já está inicializado!\n")
  cat("   Usando ambiente isolado existente.\n\n")
}

# =============================================================================
# 8. INSTALAR PACOTES DO PROJETO (DENTRO DO RENV)
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
cat("   ))\n\n")
cat("   Depois execute: renv::snapshot()\n\n")

cat("⚠️  IMPORTANTE: Não execute install.packages() agora!\n")
cat("   Primeiro faça commit do setup inicial, depois instale os pacotes.\n\n")

# =============================================================================
# 9. CRIAR README.md
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
[Sua Instituição]

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

## 🚀 Como Começar

### 1. Clonar o Repositório

\`\`\`bash
git clone https://github.com/seu-usuario/masters-thesis-spare-parts-forecasting.git
cd masters-thesis-spare-parts-forecasting
\`\`\`

### 2. Abrir Projeto no RStudio

- Abra o arquivo \`.Rproj\`
- O renv será ativado automaticamente

### 3. Restaurar Pacotes

\`\`\`r
# Instalar todos os pacotes do projeto
renv::restore()
\`\`\`

### 4. Adicionar Dados

- Colocar dados do SILOMS em \`data/raw/\`
- Estes arquivos não serão versionados (protegidos pelo .gitignore)

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

## ⚠️ Confidencialidade

**IMPORTANTE**: Este projeto contém dados operacionais confidenciais do DECEA/SISCEAB.

- Dados em \`data/raw/\` e \`data/interim/\` **NÃO** são versionados
- Arquivos \`.csv\`, \`.xlsx\`, \`.rds\` com dados reais estão no \`.gitignore\`
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
**Versão**: 1.0.0
"

readme_path <- here("README.md")
if(file.exists(readme_path)) {
  cat("⚠️  README.md já existe. Fazendo backup...\n")
  file.copy(readme_path, here("README.backup.md"), overwrite = TRUE)
}

writeLines(readme_content, readme_path)
cat("✅ README.md criado!\n\n")

# =============================================================================
# 10. RESUMO FINAL
# =============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ SETUP CONCLUÍDO COM SUCESSO!\n")
cat(rep("=", 80), "\n\n", sep = "")

# Limpar ambiente
rm(list = ls())
gc()
