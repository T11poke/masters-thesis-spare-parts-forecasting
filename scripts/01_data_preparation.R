# 01 - PREPARAÇÃO DOS DADOS - TRATAMENTO DE SKUs ALTERNADOS ####
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Este código agrupa SKUs alternados (mesmo PN) como um único
#            item de consumo para análise de previsão de demanda
# Data: 2025-08-12
# Versão: 2.0.0
# Última atualização: 2025-11-27

# Carregar configurações e bibliotecas ####
library(here)
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(igraph)
library(skimr)
library(janitor)
library(igraph)
library(future)

source(here("R/utils/load_config.R"))
source(here("R/functions/tratamento_dados.R"))

config <- load_config()
print(config$parameters$seed)
set.seed(config$parameters$seed)

log_message("========================================", "INFO")
log_message("INICIANDO PREPARAÇÃO DOS DADOS", "INFO")
log_message("========================================", "INFO")
log_message(sprintf("Seed configurado: %d", config$parameters$seed), "INFO")
log_message(sprintf("Versão do R: %s", R.version.string), "INFO")

# Configuração para paralelização
if(config$computation$parallel) {
  plan(multisession, workers = config$computation$n_cores)
  log_message(sprintf("Paralelização ativada: %d cores", config$computation$n_cores), "INFO")
} else {
  plan(sequential)
  log_message("Modo sequencial ativado", "INFO")
}

# 1. CARREGAMENTO DOS DADOS ####

cat("📁 Carregando dados brutos...\n")
log_message("Carregando dados brutos do SILOMS", "INFO")

# Carregar dados de consumo
arquivo_consumo <- here(config$paths$data$raw, 
                        config$data$files$consumo)
log_message(sprintf("Lendo arquivo: %s", basename(arquivo_consumo)), "INFO")

data_consumo <- read_excel(
  arquivo_consumo,
  sheet = config$data$sheets$consumo
) %>% 
  clean_names()

# Carregar dados de alternados
arquivo_alternados <- here(config$paths$data$raw, 
                           config$data$files$alternados)
log_message(sprintf("Lendo arquivo: %s", basename(arquivo_alternados)), "INFO")

data_alternados <- read_excel(
  arquivo_alternados,
  sheet = config$data$sheets$alternados
) %>%
  clean_names() %>%
  select(-nr_pn, -pn_alternado)  # Remove colunas desnecessárias

cat("✅ Dados carregados:\n")
cat(sprintf("   - Registros de consumo: %s\n", format(nrow(data_consumo), big.mark = ",")))
cat(sprintf("   - Registros de alternados: %s\n", format(nrow(data_alternados), big.mark = ",")))

log_message(sprintf("Registros de consumo: %s", 
                    format(nrow(data_consumo), big.mark = ",")), "INFO")
log_message(sprintf("Registros de alternados: %s", 
                    format(nrow(data_alternados), big.mark = ",")), "INFO")

# 2. ANÁLISE EXPLORATÓRIA INICIAL ####
#' Nota metodológica:
#' Esta análise é sobre os dados brutos, para uma verificação inicial do dataset
#' antes da divisão temporal. Não há risco de data leakage, pois essa análise
#' não será utilizada posteriormente no pipeline.

log_message("Executando análise exploratória inicial", "INFO")
cat("\n📋 Análise exploratória dos dados brutos...\n")

# Visão geral dos dados
cat("Estrutura dos dados de consumo:\n")
glimpse(data_consumo)

cat("\nResumo estatístico do consumo:\n")
skim(data_consumo)

cat("\nEstrutura dos dados de alternados:\n")
glimpse(data_alternados)

# Verificar duplicatas nos alternados
duplicados_alternados <- data_alternados %>%
  duplicated() %>%
  sum()

cat(sprintf("\nDuplicatas em alternados: %d\n", duplicados_alternados))
log_message(sprintf("Duplicatas identificadas: %d", duplicados_alternados), 
            if(duplicados_alternados > 0) "WARNING" else "INFO")

# 3. TRATAMENTO DOS DADOS DE ALTERNADOS ####
#' Metodologia: 3.3.2. Tratamento e limpeza dos dados
#' Primeira etapa: Compilação de alternados
#' Segunda etapa: Identificação e tratamento de outliers
#' Terceira etapa: validação de consistência
#' Quarta etapa: transformações para séries temporais completas

## ETAPA 1/4: Compilação de alternados ####

log_message("Processando consolidação de materiais alternados (Etapa 1 de 4)", "INFO")
cat("\n🔄 Processando mapeamento de SKUs alternados...\n")

# Criar arestas para o grafo (eliminar duplicatas A-B e B-A)
arestas <- data_alternados %>%
  mutate(
    cd_material = as.character(cd_material),
    cd_material_alternado = as.character(cd_material_alternado)
  ) %>%
  transmute(
    mat1 = pmin(cd_material, cd_material_alternado),
    mat2 = pmax(cd_material, cd_material_alternado)
  ) %>% 
  distinct()

cat(sprintf("   - Pares únicos de alternados: %s\n", format(nrow(arestas), big.mark = ",")))
log_message(sprintf("Pares únicos identificados: %s", format(nrow(arestas), big.mark = ",")), "INFO")

# Criar grafo não direcionado e identificar componentes conectados
# METODOLOGIA: "Modelou-se a matriz de alternância como grafo não-direcionado"
log_message("Criando grafo não-direcionado para identificação de componentes", "INFO")
grafo <- arestas %>% 
  graph_from_data_frame(directed = FALSE)

componentes_info <- components(grafo)

cat(sprintf("   - Grupos de materiais alternados: %d\n", componentes_info$no))
log_message(sprintf("Componentes conectados identificados: %d", componentes_info$no), "INFO")

# Criar mapeamento material -> ID do componente
mapa_material_para_id <- data.frame(
  cd_material_original = names(componentes_info$membership),
  id_componente = componentes_info$membership,
  stringsAsFactors = FALSE
)

# Criar mapeamento ID -> Material mestre (menor CD_MATERIAL do grupo)
# METODOLOGIA: "Designou-se como código mestre o menor valor numérico de CD_MATERIAL"
mapa_id_para_grupo <- mapa_material_para_id %>% 
  group_by(id_componente) %>%
  summarise(
    cd_mestre = min(cd_material_original), 
    qtd_alternados = n(),
    .groups = 'drop'
  )

# Criar mapeamento final de-para
mapa_de_para <- mapa_material_para_id %>% 
  left_join(mapa_id_para_grupo, by = "id_componente") %>% 
  select(cd_material_original, cd_mestre, qtd_alternados)

cat(sprintf("   - Materiais com alternados: %s\n", format(nrow(mapa_de_para), big.mark = ",")))
log_message(sprintf("Mapeamento criado para %s materiais", 
                    format(nrow(mapa_de_para), big.mark = ",")), "INFO")

# Estatísticas dos grupos
grupos_stats <- mapa_id_para_grupo %>%
  count(qtd_alternados, name = "grupos") %>%
  arrange(desc(qtd_alternados))

cat("\nDistribuição dos grupos de alternados:\n")
print(grupos_stats)

### APLICAÇÃO DO MAPEAMENTO AOS DADOS DE CONSUMO ####
# METODOLOGIA: "Remapearam-se registros substituindo código original por código mestre"

log_message("Aplicando mapeamento aos dados de consumo", "INFO")
cat("\n🔀 Aplicando mapeamento aos dados de consumo...\n")

# Aplicar mapeamento de alternados
data_com_mestre <- data_consumo %>% 
  mutate(cd_material_char = as.character(cd_material)) %>% 
  left_join(
    mapa_de_para, 
    by = c("cd_material_char" = "cd_material_original")
  ) %>% 
  mutate(
    # Se não tem alternado (CD_MESTRE é NA), o próprio material é o mestre
    cd_material_final = ifelse(
      is.na(cd_mestre), 
      cd_material_char, 
      cd_mestre
    )
  ) %>% 
  select(-cd_material_char, -cd_mestre, -qtd_alternados)

# Verificar materiais afetados pelo mapeamento
materiais_mapeados <- data_com_mestre %>%
  filter(cd_material_final != as.character(cd_material)) %>%
  distinct(cd_material) %>%
  nrow()

cat(sprintf("   - Materiais afetados pelo mapeamento: %s\n", format(materiais_mapeados, big.mark = ",")))
log_message(sprintf("Materiais remapeados: %s", format(materiais_mapeados, big.mark = ",")), "INFO")

### AGREGAÇÃO DOS CONSUMOS POR MATERIAL MESTRE ####
# METODOLOGIA: "agregam-se consumos por período de competência, considerando unidades de medida"

log_message("Agregando consumos por material mestre (Etapa 2 de 4)", "INFO")
cat("\n📊 Agregando consumos por material mestre...\n")

# Validar qualidade dos dados antes da agregação
cat("🔍 Validando qualidade dos dados...\n")
validacao <- validar_dados_consumo(data_com_mestre, "qt_consumo")
criar_relatorio_qualidade(validacao, "Dados com Mestre")

data_agrupado <- agregar_por_material_mestre(
  data = data_com_mestre,
  coluna_material = "cd_material_final",
  coluna_ano = "ano_competencia",
  coluna_mes = "mes_competencia", 
  coluna_qt = "qt_consumo",
  coluna_un = "sg_medida_port"
)

cat(sprintf("   - Registros após agregação: %s\n", format(nrow(data_agrupado), big.mark = ",")))
cat(sprintf("   -  %.1f%%\n", 
            (1 - nrow(data_agrupado)/nrow(data_consumo)) * 100))

log_message(sprintf("Agregação concluída: %s registros.
                                         Redução:  %.1f%%",
                    format(nrow(data_agrupado), big.mark = ","),
                    (1 - nrow(data_agrupado)/nrow(data_consumo)) * 100), "INFO")

# Estatísticas sobre problemas encontrados
total_problemas <- data_agrupado %>%
  summarise(total_registros_problema = sum(registros_com_problema)) %>%
  pull(total_registros_problema)

if(total_problemas > 0) {
  cat(sprintf("⚠️  Total de registros com problemas de conversão: %s\n", 
              format(total_problemas, big.mark = ",")))
  log_message(sprintf("Registros com problemas: %s", 
                      format(total_problemas, big.mark = ",")), "WARNING")
} else {
  cat("✅ Todos os valores foram convertidos com sucesso!\n")
  log_message("Todos os valores convertidos com sucesso", "INFO")
}

### ANÁLISE DAS UNIDADES DE MEDIDAS ####



#### CONVERSÃO DE UNIDADES ####
#### AGREGAÇÃO FINAL ####




## ETAPA 2/4: Identificação e tratamento de outliers ####














# OK Até aqui!! Continuar!!!!! #####

## ETAPA 3/4: validação de consistência ####
### ANÁLISE DE QUALIDADE DOS DADOS ####

# Identificar inconsistências (consumos negativos ou zero)
data_inconsistente <- data_agrupado %>% 
  filter(qt_total <= 0)

cat(sprintf("   - Registros inconsistentes (≤0): %s\n", format(nrow(data_inconsistente), big.mark = ",")))

if(nrow(data_inconsistente) > 0) {
  cat("Salvando registros inconsistentes...\n")
  write_xlsx(
    data_inconsistente, 
    here("data", "processed", "inconsistencias_consumo_negativo.xlsx")
  )
}

# Filtrar apenas consumos positivos
data_final <- data_agrupado %>% 
  filter(qt_total > 0)

cat(sprintf("   - Registros finais válidos: %s\n", format(nrow(data_final), big.mark = ",")))

# Resumo estatístico final
cat("\n📈 Resumo estatístico dos dados finais:\n")
skim(data_final)
data_final %$% skim(qt_total)

# 7. SALVAMENTO DOS RESULTADOS ####

cat("\n💾 Salvando resultados...\n")

# Salvar dados processados
write_rds(data_final, here("data", "processed", "consumo_agrupado_por_mestre.rds"))
write_xlsx(data_final, here("data", "processed", "consumo_agrupado_por_mestre.xlsx"))

# Salvar mapeamento de alternados para referência
write_xlsx(mapa_de_para, here("data", "processed", "mapeamento_alternados.xlsx"))

# Salvar ambiente (se necessário para compatibilidade)
save(
  data_final, 
  file = here("output", "models", "environment_dados_tratados.RData")
)

# 8. RELATÓRIO FINAL ####

cat("\n🎉 PROCESSAMENTO CONCLUÍDO! 🎉\n")
cat("==========================================\n")
cat("RESUMO DO PROCESSAMENTO:\n\n")
cat(sprintf("📥 Registros de entrada: %s\n", format(nrow(data_consumo), big.mark = ",")))
cat(sprintf("📤 Registros finais: %s\n", format(nrow(data_final), big.mark = ",")))
cat(sprintf("🔄 Materiais com alternados: %s\n", format(nrow(mapa_de_para), big.mark = ",")))
cat(sprintf("🎯 Grupos de alternados: %d\n", componentes_info$no))
cat(sprintf("⚠️  Inconsistências removidas: %s\n", format(nrow(data_inconsistente), big.mark = ",")))
cat(sprintf("📉 Redução total: %.1f%%\n", (1 - nrow(data_final)/nrow(data_consumo)) * 100))

cat("\n📁 Arquivos gerados:\n")
cat("   - data/processed/consumo_agrupado_por_mestre.rds\n")
cat("   - data/processed/consumo_agrupado_por_mestre.xlsx\n")
cat("   - data/processed/mapeamento_alternados.xlsx\n")
cat("   - output/models/environment_dados_tratados.RData\n")

if(nrow(data_inconsistente) > 0) {
  cat("   - data/processed/inconsistencias_consumo_negativo.xlsx\n")
}

cat("\n✅ Dados prontos para análise exploratória!\n")
cat("Próximo passo: Execute o script 02_exploratory_analysis.R\n")

# Limpeza do ambiente (opcional)
rm(list = setdiff(ls(), c("data_final")))
