# TESTE RÁPIDO: VERIFICAÇÃO DE UNIDADES DE MEDIDA ####
#
# script para saber se  precisa implementar
# conversão de unidades no seu projeto

library(here)
library(tidyverse)
library(readxl)

source(here("R/utils/load_config.R"))

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("    TESTE RÁPIDO: VERIFICAÇÃO DE UNIDADES DE MEDIDA\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# =============================================================================
# TESTE 1: UNIDADES NOS DADOS BRUTOS
# =============================================================================

cat("📋 TESTE 1: Análise dos dados brutos\n")
cat("───────────────────────────────────────────────────────────\n")

tryCatch({
  
  # Carregar dados
  cat("Carregando dados...\n")
  data_consumo <- read_excel(
    here(config$paths$data$raw,
         config$data$files$consumo),
    sheet = config$data$sheets$consumo
  ) %>% 
    clean_names()
  
  # Verificar se coluna existe
  if(!"sg_medida_port" %in% names(data_consumo)) {
    cat("❌ ERRO: Coluna 'sg_medida_port' não encontrada!\n")
    cat("   Verifique se o arquivo tem esta coluna.\n\n")
  } else {
    
    # 1.1 Quantas unidades únicas?
    unidades_unicas <- data_consumo %>%
      distinct(sg_medida_port) %>%
      filter(!is.na(sg_medida_port)) %>%
      pull(sg_medida_port) %>%
      sort()
    
    cat(sprintf("✅ Unidades únicas encontradas: %d\n", length(unidades_unicas)))
    cat("\nLista de unidades:\n")
    cat(paste("   -", unidades_unicas, collapse = "\n"))
    cat("\n\n")
    
    # 1.2 Materiais com múltiplas unidades?
    materiais_mult_unid <- data_consumo %>%
      group_by(cd_material) %>%
      summarise(
        n_unidades = n_distinct(sg_medida_port, na.rm = TRUE),
        unidades = paste(unique(sg_medida_port), collapse = ", "),
        .groups = 'drop'
      ) %>%
      filter(n_unidades > 1)
    
    if(nrow(materiais_mult_unid) > 0) {
      cat(sprintf("⚠️  ATENÇÃO: %d materiais têm múltiplas unidades nos dados brutos\n", 
                  nrow(materiais_mult_unid)))
      cat("\nPrimeiros 5 casos:\n")
      print(head(materiais_mult_unid, 5))
      cat("\n")
      cat("💡 Isso pode indicar:\n")
      cat("   - Mudança de unidade ao longo do tempo\n")
      cat("   - Erro de cadastro no SILOMS\n")
      cat("   - Necessidade de padronização\n\n")
    } else {
      cat("✅ OK: Cada material tem unidade única nos dados brutos\n\n")
    }
  }
  
}, error = function(e) {
  cat("❌ ERRO ao carregar dados brutos:\n")
  cat("  ", conditionMessage(e), "\n\n")
  cat("Verifique:\n")
  cat("  - Arquivo existe em data/raw/?\n")
  cat("  - Nome do arquivo está correto?\n")
  cat("  - Sheet existe?\n\n")
})

# =============================================================================
# TESTE 2: UNIDADES APÓS MAPEAMENTO DE ALTERNADOS (CRÍTICO!)
# =============================================================================

cat("🔍 TESTE 2: Análise após mapeamento de alternados\n")
cat("───────────────────────────────────────────────────────────\n")

# Verificar se já existe o arquivo com mapeamento aplicado
arquivo_mestre <- here("data/interim/data_com_mestre.rds")

if(!file.exists(arquivo_mestre)) {
  cat("⏭️  PULADO: Arquivo 'data_com_mestre.rds' não encontrado\n")
  cat("   Execute o script 01_data_preparation.R primeiro\n")
  cat("   até a seção de mapeamento de alternados.\n\n")
} else {
  
  tryCatch({
    
    cat("Carregando dados com mapeamento...\n")
    data_com_mestre <- readRDS(arquivo_mestre)
    
    # Verificar estrutura
    if(!"cd_material_final" %in% names(data_com_mestre)) {
      cat("❌ ERRO: Coluna 'cd_material_final' não encontrada!\n")
      cat("   O arquivo pode não ser o correto.\n\n")
    } else if(!"sg_medida_port" %in% names(data_com_mestre)) {
      cat("❌ ERRO: Coluna 'sg_medida_port' não encontrada!\n\n")
    } else {
      
      # TESTE CRÍTICO: Materiais mestres com múltiplas unidades?
      conflitos_criticos <- data_com_mestre %>%
        group_by(cd_material_final) %>%
        summarise(
          n_unidades = n_distinct(sg_medida_port, na.rm = TRUE),
          unidades = paste(unique(sg_medida_port), collapse = " | "),
          n_registros = n(),
          .groups = 'drop'
        ) %>%
        filter(n_unidades > 1) %>%
        arrange(desc(n_unidades), desc(n_registros))
      
      if(nrow(conflitos_criticos) > 0) {
        cat("\n")
        cat("🚨🚨🚨 ALERTA CRÍTICO! 🚨🚨🚨\n")
        cat("───────────────────────────────────────────────────────────\n")
        cat(sprintf("%d MATERIAIS MESTRES TÊM MÚLTIPLAS UNIDADES!\n", 
                    nrow(conflitos_criticos)))
        cat("\nIsso significa que:\n")
        cat("  ❌ Materiais alternados foram consolidados\n")
        cat("  ❌ MAS têm unidades de medida DIFERENTES\n")
        cat("  ❌ Somar sem converter = ERRO GRAVE DE METODOLOGIA\n\n")
        
        cat("Casos mais críticos:\n")
        print(head(conflitos_criticos, 10))
        
        cat("\n")
        cat("╔═══════════════════════════════════════════════════════════╗\n")
        cat("║  AÇÃO OBRIGATÓRIA:                                          ║\n")
        cat("║  Implementar conversão de unidades ANTES de agregar!        ║\n")
        cat("║                                                           ║\n")
        cat("╚═══════════════════════════════════════════════════════════╝\n")
        cat("\n")
        
        # Salvar para análise
        write_csv(
          conflitos_criticos,
          here("output", "reports", "ALERTA_conflitos_unidades.csv")
        )
        
        cat("📄 Conflitos salvos em: output/reports/ALERTA_conflitos_unidades.csv\n\n")
        
      } else {
        cat("\n")
        cat("✅✅✅ EXCELENTE! ✅✅✅\n")
        cat("───────────────────────────────────────────────────────────\n")
        cat("Todos os materiais mestres têm unidade única!\n")
        cat("Nenhuma conversão de unidade é necessária.\n\n")
      }
    }
    
  }, error = function(e) {
    cat("❌ ERRO ao processar data_com_mestre:\n")
    cat("  ", conditionMessage(e), "\n\n")
  })
}

# =============================================================================
# TESTE 3: ANÁLISE ESTATÍSTICA DAS UNIDADES
# =============================================================================

if(exists("data_consumo") && "sg_medida_port" %in% names(data_consumo)) {
  
  cat("📊 TESTE 3: Estatísticas de distribuição\n")
  cat("───────────────────────────────────────────────────────────\n")
  
  distribuicao <- data_consumo %>%
    count(sg_medida_port, name = "n_registros") %>%
    mutate(
      percentual = n_registros / sum(n_registros) * 100
    ) %>%
    arrange(desc(n_registros))
  
  cat("\nDistribuição de registros por unidade:\n\n")
  print(distribuicao, n = Inf)
  
  cat("\nUnidade predominante:", 
      distribuicao$sg_medida_port[1], 
      sprintf("(%.1f%%)", distribuicao$percentual[1]))
  cat("\n\n")
}

# =============================================================================
# RESUMO E RECOMENDAÇÕES
# =============================================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("    RESUMO E RECOMENDAÇÕES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Determinar ação necessária
if(exists("conflitos_criticos") && nrow(conflitos_criticos) > 0) {
  
  cat("⚠️  STATUS: AÇÃO OBRIGATÓRIA NECESSÁRIA\n\n")
  
  cat("PROBLEMA IDENTIFICADO:\n")
  cat(sprintf("  - %d materiais mestres com múltiplas unidades\n", 
              nrow(conflitos_criticos)))
  cat("  - Conversão é OBRIGATÓRIA antes de agregar quantidades\n\n")
  
  cat("PRÓXIMOS PASSOS:\n")
  cat("  1. Abra: output/reports/ALERTA_conflitos_unidades.csv\n")
  cat("  2. Leia: GUIA_CONVERSAO_UNIDADES.md\n")
  cat("  3. Execute: exemplo_analise_unidades.R\n")
  cat("  4. Implemente conversões no script principal\n\n")
  
  cat("ESTIMATIVA DE TEMPO:\n")
  if(nrow(conflitos_criticos) < 10) {
    cat("  📅 2-4 horas (poucos conflitos)\n")
  } else if(nrow(conflitos_criticos) < 50) {
    cat("  📅 4-8 horas (conflitos moderados)\n")
  } else {
    cat("  📅 1-2 dias (muitos conflitos - análise detalhada)\n")
  }
  cat("\n")
  
  cat("RISCO SE NÃO IMPLEMENTAR:\n")
  cat("  🔴 Quantidades agregadas INCORRETAS\n")
  cat("  🔴 Análise exploratória com DADOS FALSOS\n")
  cat("  🔴 Previsões baseadas em ERRO\n")
  cat("  🔴 Resultados da dissertação INVÁLIDOS\n\n")
  
} else if(exists("materiais_mult_unid") && nrow(materiais_mult_unid) > 0) {
  
  cat("🟡 STATUS: VERIFICAÇÃO ADICIONAL RECOMENDADA\n\n")
  
  cat("SITUAÇÃO:\n")
  cat("  - Materiais individuais têm múltiplas unidades\n")
  cat("  - Mas não há conflitos após mapeamento (ainda)\n")
  cat("  - Pode indicar mudança de unidade ao longo do tempo\n\n")
  
  cat("RECOMENDAÇÃO:\n")
  cat("  1. Investigar os casos identificados\n")
  cat("  2. Documentar mudanças de unidade\n")
  cat("  3. Considerar flags temporais se necessário\n\n")
  
} else {
  
  cat("✅ STATUS: TUDO OK - NENHUMA AÇÃO NECESSÁRIA\n\n")
  
  cat("SITUAÇÃO:\n")
  cat("  ✅ Cada material tem unidade única\n")
  cat("  ✅ Nenhum conflito após mapeamento\n")
  cat("  ✅ Conversão de unidades não é necessária\n\n")
  
  cat("PRÓXIMOS PASSOS:\n")
  cat("  → Prosseguir com agregação normalmente\n")
  cat("  → Não precisa implementar conversão de unidades\n\n")
}

cat("═══════════════════════════════════════════════════════════\n")
cat("Teste concluído em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Criar flag para uso em outros scripts
if(exists("conflitos_criticos") && nrow(conflitos_criticos) > 0) {
  necessita_conversao <- TRUE
  n_conflitos <- nrow(conflitos_criticos)
} else {
  necessita_conversao <- FALSE
  n_conflitos <- 0
}

# Salvar resultado do teste
resultado_teste <- list(
  data_teste = Sys.time(),
  necessita_conversao = necessita_conversao,
  n_conflitos = n_conflitos,
  conflitos = if(exists("conflitos_criticos")) conflitos_criticos else NULL
)

saveRDS(
  resultado_teste,
  here("output", "reports", "resultado_teste_unidades.rds")
)

cat("📄 Resultado salvo em: output/reports/resultado_teste_unidades.rds\n")
cat("   Use: readRDS('...') para verificar em outros scripts\n\n")

# Retornar resultado
if(necessita_conversao) {
  cat("🔴 RESULTADO FINAL: CONVERSÃO OBRIGATÓRIA\n\n")
  invisible(list(status = "OBRIGATORIO", n_conflitos = n_conflitos))
} else {
  cat("✅ RESULTADO FINAL: SEM NECESSIDADE DE CONVERSÃO\n\n")
  invisible(list(status = "OK", n_conflitos = 0))
}