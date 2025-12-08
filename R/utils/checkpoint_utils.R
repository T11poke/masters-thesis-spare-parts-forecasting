# =============================================================================
# FUNÇÕES UTILITÁRIAS: CHECKPOINT RECOVERY
# =============================================================================
#
# Autor: LUIZ ANTONIO DOS SANTOS DIAS REZENDE
# Descrição: Funções para gerenciar checkpoints e recovery no pipeline
# Data: 2025-12-08
# Versão: 1.0.0

#' Verificar se checkpoint de um script existe e é válido
#'
#' @param script_name Nome do script (ex: "04a", "04b", "04c")
#' @param checkpoint_dir Diretório de checkpoints
#' @param debug_mode Modo debug ativo?
#' @return Lista com status do checkpoint
#' @export
check_script_checkpoint <- function(script_name, 
                                    checkpoint_dir = here::here("output/checkpoints"),
                                    debug_mode = FALSE) {
  
  # Padrão do arquivo de checkpoint do script completo
  checkpoint_pattern <- if(debug_mode) {
    sprintf("checkpoint_%s_completo_DEBUG.rds", script_name)
  } else {
    sprintf("checkpoint_%s_completo.rds", script_name)
  }
  
  checkpoint_file <- file.path(checkpoint_dir, checkpoint_pattern)
  
  if(!file.exists(checkpoint_file)) {
    return(list(
      exists = FALSE,
      valid = FALSE,
      file = checkpoint_file,
      timestamp = NA,
      n_origins = NA,
      message = "Checkpoint não encontrado"
    ))
  }
  
  # Carregar e validar checkpoint
  tryCatch({
    checkpoint_data <- readRDS(checkpoint_file)
    
    # Validações básicas
    valid <- all(c("script_name", "timestamp", "n_origins", "success") %in% 
                   names(checkpoint_data))
    
    if(valid) {
      valid <- checkpoint_data$success == TRUE
    }
    
    list(
      exists = TRUE,
      valid = valid,
      file = checkpoint_file,
      timestamp = checkpoint_data$timestamp,
      n_origins = checkpoint_data$n_origins,
      tempo_execucao = checkpoint_data$tempo_execucao_sec,
      data = checkpoint_data,
      message = if(valid) "Checkpoint válido" else "Checkpoint inválido"
    )
    
  }, error = function(e) {
    list(
      exists = TRUE,
      valid = FALSE,
      file = checkpoint_file,
      timestamp = NA,
      n_origins = NA,
      message = sprintf("Erro ao ler checkpoint: %s", conditionMessage(e))
    )
  })
}

#' Salvar checkpoint de conclusão de script
#'
#' @param script_name Nome do script (ex: "04a", "04b", "04c")
#' @param n_origins Número de origens processadas
#' @param tempo_execucao_sec Tempo de execução em segundos
#' @param checkpoint_dir Diretório de checkpoints
#' @param debug_mode Modo debug ativo?
#' @param additional_info Lista com informações adicionais (opcional)
#' @export
save_script_checkpoint <- function(script_name,
                                   n_origins,
                                   tempo_execucao_sec,
                                   checkpoint_dir = here::here("output/checkpoints"),
                                   debug_mode = FALSE,
                                   additional_info = list()) {
  
  checkpoint_pattern <- if(debug_mode) {
    sprintf("checkpoint_%s_completo_DEBUG.rds", script_name)
  } else {
    sprintf("checkpoint_%s_completo.rds", script_name)
  }
  
  checkpoint_file <- file.path(checkpoint_dir, checkpoint_pattern)
  
  checkpoint_data <- list(
    script_name = script_name,
    timestamp = Sys.time(),
    n_origins = n_origins,
    tempo_execucao_sec = tempo_execucao_sec,
    debug_mode = debug_mode,
    success = TRUE,
    r_version = R.version.string,
    additional_info = additional_info
  )
  
  saveRDS(checkpoint_data, checkpoint_file)
  
  invisible(checkpoint_file)
}

#' Listar todos os checkpoints disponíveis
#'
#' @param checkpoint_dir Diretório de checkpoints
#' @param debug_mode Filtrar apenas checkpoints debug?
#' @return Tibble com informações dos checkpoints
#' @export
list_checkpoints <- function(checkpoint_dir = here::here("output/checkpoints"),
                             debug_mode = NULL) {
  
  if(!dir.exists(checkpoint_dir)) {
    return(tibble::tibble())
  }
  
  # Listar arquivos de checkpoint completo
  pattern <- if(is.null(debug_mode)) {
    "checkpoint_04[abc]_completo.*\\.rds$"
  } else if(debug_mode) {
    "checkpoint_04[abc]_completo_DEBUG\\.rds$"
  } else {
    "checkpoint_04[abc]_completo\\.rds$"
  }
  
  files <- list.files(checkpoint_dir, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    return(tibble::tibble())
  }
  
  # Extrair informações de cada checkpoint
  purrr::map_dfr(files, function(f) {
    tryCatch({
      data <- readRDS(f)
      tibble::tibble(
        script = data$script_name,
        timestamp = data$timestamp,
        n_origins = data$n_origins,
        tempo_min = data$tempo_execucao_sec / 60,
        debug_mode = data$debug_mode,
        success = data$success,
        arquivo = basename(f)
      )
    }, error = function(e) {
      tibble::tibble()
    })
  }) %>%
    dplyr::arrange(script)
}

#' Limpar checkpoints antigos
#'
#' @param checkpoint_dir Diretório de checkpoints
#' @param keep_last_n Manter apenas os N checkpoints mais recentes de cada script
#' @param older_than_days Remover checkpoints mais antigos que N dias
#' @param dry_run Se TRUE, apenas mostra o que seria deletado
#' @export
clean_old_checkpoints <- function(checkpoint_dir = here::here("output/checkpoints"),
                                  keep_last_n = 2,
                                  older_than_days = 7,
                                  dry_run = TRUE) {
  
  if(!dir.exists(checkpoint_dir)) {
    message("Diretório de checkpoints não existe")
    return(invisible(NULL))
  }
  
  # Listar todos os checkpoints (incluindo por origem)
  all_files <- list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if(length(all_files) == 0) {
    message("Nenhum checkpoint encontrado")
    return(invisible(NULL))
  }
  
  # Informações dos arquivos
  file_info <- tibble::tibble(
    path = all_files,
    filename = basename(all_files),
    mtime = file.mtime(all_files),
    age_days = as.numeric(difftime(Sys.time(), file.mtime(all_files), units = "days"))
  )
  
  # Identificar checkpoints para remoção
  to_remove <- file_info %>%
    dplyr::filter(age_days > older_than_days)
  
  if(nrow(to_remove) > 0) {
    if(dry_run) {
      cat("🔍 MODO DRY RUN - Arquivos que SERIAM removidos:\n\n")
      to_remove %>%
        dplyr::select(filename, age_days) %>%
        dplyr::arrange(desc(age_days)) %>%
        print(n = Inf)
      
      cat(sprintf("\n📊 Total: %d arquivos (%.1f MB)\n",
                  nrow(to_remove),
                  sum(file.size(to_remove$path)) / 1024^2))
      
      cat("\n💡 Para executar a remoção, use: clean_old_checkpoints(dry_run = FALSE)\n")
      
    } else {
      cat("🗑️  Removendo checkpoints antigos...\n\n")
      
      removed_count <- 0
      for(f in to_remove$path) {
        if(file.remove(f)) {
          removed_count <- removed_count + 1
          cat(sprintf("   ✓ %s\n", basename(f)))
        }
      }
      
      cat(sprintf("\n✅ %d arquivos removidos\n", removed_count))
    }
  } else {
    message("Nenhum checkpoint antigo para remover")
  }
  
  invisible(to_remove)
}

#' Exibir resumo de checkpoints disponíveis
#'
#' @param checkpoint_dir Diretório de checkpoints
#' @export
checkpoint_summary <- function(checkpoint_dir = here::here("output/checkpoints")) {
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMO DE CHECKPOINTS DISPONÍVEIS             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  # Checkpoints de produção
  checkpoints_prod <- list_checkpoints(checkpoint_dir, debug_mode = FALSE)
  
  if(nrow(checkpoints_prod) > 0) {
    cat("🎯 CHECKPOINTS DE PRODUÇÃO:\n\n")
    checkpoints_prod %>%
      dplyr::mutate(
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
        tempo_min = round(tempo_min, 1)
      ) %>%
      dplyr::select(script, timestamp, n_origins, tempo_min, success) %>%
      print(n = Inf)
    cat("\n")
  } else {
    cat("⚠️  Nenhum checkpoint de produção encontrado\n\n")
  }
  
  # Checkpoints debug
  checkpoints_debug <- list_checkpoints(checkpoint_dir, debug_mode = TRUE)
  
  if(nrow(checkpoints_debug) > 0) {
    cat("🔧 CHECKPOINTS DEBUG:\n\n")
    checkpoints_debug %>%
      dplyr::mutate(
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
        tempo_min = round(tempo_min, 1)
      ) %>%
      dplyr::select(script, timestamp, n_origins, tempo_min, success) %>%
      print(n = Inf)
    cat("\n")
  }
  
  # Estatísticas gerais
  all_files <- list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if(length(all_files) > 0) {
    total_size_mb <- sum(file.size(all_files)) / 1024^2
    
    cat("📊 ESTATÍSTICAS:\n")
    cat(sprintf("   - Total de arquivos: %d\n", length(all_files)))
    cat(sprintf("   - Espaço ocupado: %.1f MB\n", total_size_mb))
    cat(sprintf("   - Localização: %s\n", checkpoint_dir))
  }
  
  cat("\n")
}