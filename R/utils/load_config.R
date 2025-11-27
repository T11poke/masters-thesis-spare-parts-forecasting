# =============================================================================
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
  cat(log_msg, '\n')
  
  # Salvar em arquivo de log se existir pasta logs/
  if(dir.exists(here::here('logs'))) {
    log_file <- here::here('logs', sprintf('log_%s.txt', Sys.Date()))
    cat(log_msg, '\n', file = log_file, append = TRUE)
  }
}

# Configurar seed global
if(exists('config')) {
  set.seed(config$parameters$seed)
}

