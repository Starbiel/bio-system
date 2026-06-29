# =============================================================================
# 01_preprocessing.R
# Pré-processamento do dataset UCI HAPT
#
# Pipeline:
#   1. Download e extração do dataset
#   2. Leitura dos sinais brutos (acelerômetro + giroscópio)
#   3. Leitura dos rótulos de atividade
#   4. Segmentação das transições posturais
#   5. Aplicação de filtro Butterworth
#   6. Janelamento dos segmentos
#   7. Exportação dos segmentos processados
#
# Atividades de interesse (transições posturais):
#   7  = stand-to-sit
#   8  = sit-to-stand   <-- mais próxima da RCP (movimento oscilatório rítmico)
#   9  = sit-to-lie
#   10 = lie-to-sit
#   11 = stand-to-lie
#   12 = lie-to-stand
# =============================================================================

packages <- c("signal", "dplyr", "readr")

missing <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(missing) > 0){
  install.packages(missing)
}

lapply(packages, library, character.only = TRUE)

library(signal)    # filtro Butterworth
library(dplyr)     # manipulação de dados
library(readr)     # leitura de arquivos
# -----------------------------------------------------------------------------
# 0. Configurações globais
# -----------------------------------------------------------------------------

FS          <- 50        # frequência de amostragem (Hz)
WIN_SEC     <- 2.56      # tamanho da janela em segundos
WIN_SAMPLES <- round(WIN_SEC * FS)   # 128 amostras por janela
OVERLAP     <- 0.7       # 50% de sobreposição entre janelas
STEP        <- round(WIN_SAMPLES * (1 - OVERLAP))  # passo entre janelas

# Atividades de transição postural (rótulos do dataset)
TRANSITION_LABELS <- 7:12
LABEL_NAMES <- c(
  "7"  = "stand-to-sit",
  "8"  = "sit-to-stand",
  "9"  = "sit-to-lie",
  "10" = "lie-to-sit",
  "11" = "stand-to-lie",
  "12" = "lie-to-stand"
)

DATA_DIR   <- "data/HAPT"
OUTPUT_DIR <- "output"

# -----------------------------------------------------------------------------
# 1. Download e extração do dataset
# -----------------------------------------------------------------------------

download_hapt <- function(dest_dir = "data") {
  
  zip_path <- file.path(dest_dir, "hapt.zip")
  url <- paste0(
    "https://archive.ics.uci.edu/static/public/341/",
    "smartphone+based+recognition+of+human+activities",
    "+and+postural+transitions.zip"
  )
  
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  if (!file.exists(zip_path)) {
    message("Baixando dataset HAPT (~25 MB)...")
    download.file(url, destfile = zip_path, mode = "wb")
    message("Download concluído.")
  } else {
    message("Arquivo zip já existe. Pulando download.")
  }
  
  extract_dir <- file.path(dest_dir, "HAPT")
  if (!dir.exists(extract_dir)) {
    message("Extraindo arquivos...")
    unzip(zip_path, exdir = extract_dir)
    message("Extração concluída.")
  } else {
    message("Arquivos já extraídos. Pulando extração.")
  }
  
  return(invisible(extract_dir))
}

# -----------------------------------------------------------------------------
# 2. Leitura dos rótulos (labels.txt)
#
# Formato do arquivo labels.txt:
#   exp_id  user_id  activity_id  start_sample  end_sample
# -----------------------------------------------------------------------------

read_labels <- function(data_dir) {
  
  labels_path <- file.path(data_dir, "RawData", "labels.txt")
  stopifnot(file.exists(labels_path))
  
  labels <- read_table(
    labels_path,
    col_names = c("exp_id", "user_id", "activity_id", "start", "end"),
    col_types = "iiiii"
  )
  
  message(sprintf(
    "Rótulos carregados: %d registros, %d experimentos, %d sujeitos",
    nrow(labels), n_distinct(labels$exp_id), n_distinct(labels$user_id)
  ))
  
  return(labels)
}

# -----------------------------------------------------------------------------
# 3. Leitura do sinal bruto de um experimento
#
# Arquivos: acc_expXX_userYY.txt  (3 colunas: X Y Z)
#           gyro_expXX_userYY.txt (3 colunas: X Y Z)
# -----------------------------------------------------------------------------

read_raw_signal <- function(data_dir, exp_id, user_id) {
  
  exp_str  <- sprintf("%02d", exp_id)
  user_str <- sprintf("%02d", user_id)
  raw_dir  <- file.path(data_dir, "RawData")
  
  acc_file  <- file.path(raw_dir, paste0("acc_exp",  exp_str, "_user", user_str, ".txt"))
  gyro_file <- file.path(raw_dir, paste0("gyro_exp", exp_str, "_user", user_str, ".txt"))
  
  if (!file.exists(acc_file) || !file.exists(gyro_file)) {
    warning(sprintf("Arquivos não encontrados: exp%s_user%s", exp_str, user_str))
    return(NULL)
  }
  
  acc  <- read_table(acc_file,  col_names = c("acc_x",  "acc_y",  "acc_z"),  col_types = "ddd")
  gyro <- read_table(gyro_file, col_names = c("gyro_x", "gyro_y", "gyro_z"), col_types = "ddd")
  
  # Combina acelerômetro + giroscópio em um único data frame
  signal_df <- bind_cols(acc, gyro) |>
    mutate(sample_idx = row_number())
  
  return(signal_df)
}

# -----------------------------------------------------------------------------
# 4. Filtro Butterworth passa-banda
#
# Remove ruído DC (< 0.3 Hz) e ruído de alta frequência (> 20 Hz)
# Faixa de interesse para movimento humano: 0.3 – 20 Hz
# -----------------------------------------------------------------------------

apply_butterworth <- function(signal_vec, fs = FS, low = 0.3, high = 20, order = 4) {
  
  nyq <- fs / 2
  
  # Filtro passa-alta (remove componente gravitacional / DC)
  hp <- butter(order, low / nyq, type = "high")
  
  # Filtro passa-baixa (remove ruído de alta frequência)
  lp <- butter(order, high / nyq, type = "low")
  
  filtered <- filtfilt(hp, signal_vec)
  filtered <- filtfilt(lp, filtered)
  
  return(filtered)
}

filter_all_axes <- function(df, fs = FS) {
  cols <- c("acc_x", "acc_y", "acc_z", "gyro_x", "gyro_y", "gyro_z")
  for (col in cols) {
    df[[col]] <- apply_butterworth(df[[col]], fs = fs)
  }
  return(df)
}

# -----------------------------------------------------------------------------
# 5. Segmentação por atividade + janelamento
#
# Para cada segmento de transição postural:
#   a) Extrai o trecho do sinal correspondente
#   b) Aplica o filtro Butterworth
#   c) Divide em janelas sobrepostas
#   d) Retorna lista de janelas com metadados
# -----------------------------------------------------------------------------

segment_transitions <- function(signal_df, labels_row) {
  
  start <- labels_row$start
  end   <- labels_row$end
  
  # Extrai o trecho do sinal
  segment <- signal_df |>
    dplyr::filter(sample_idx >= start, sample_idx <= end) |>
    dplyr::select(-sample_idx)
  
  if (nrow(segment) < WIN_SAMPLES) {
    return(NULL)  # segmento muito curto para ao menos uma janela
  }
  
  # Aplica filtro
  segment <- filter_all_axes(segment)
  
  # Janelamento com sobreposição
  windows  <- list()
  n_start  <- seq(1, nrow(segment) - WIN_SAMPLES + 1, by = STEP)
  
  for (i in seq_along(n_start)) {
    idx    <- n_start[i]:(n_start[i] + WIN_SAMPLES - 1)
    window <- segment[idx, ]
    window$window_id    <- i
    window$activity_id  <- labels_row$activity_id
    window$activity_name <- LABEL_NAMES[as.character(labels_row$activity_id)]
    window$exp_id       <- labels_row$exp_id
    window$user_id      <- labels_row$user_id
    windows[[i]] <- window
  }
  
  return(windows)
}

# -----------------------------------------------------------------------------
# 6. Pipeline principal
# -----------------------------------------------------------------------------

run_preprocessing <- function(
    data_dir   = DATA_DIR,
    output_dir = OUTPUT_DIR,
    activities = TRANSITION_LABELS,
    max_users  = NULL  # NULL = todos os 30 sujeitos; use ex: 5 para teste rápido
) {
  
  message("\n=== INÍCIO DO PRÉ-PROCESSAMENTO ===\n")
  
  # Lê rótulos
  labels <- read_labels(data_dir)
  
  # Filtra apenas transições posturais de interesse
  labels_filtered <- labels |>
    dplyr::filter(activity_id %in% activities)
  
  message(sprintf(
    "Transições posturais encontradas: %d segmentos",
    nrow(labels_filtered)
  ))
  
  # Limita número de sujeitos se max_users definido (para testes)
  if (!is.null(max_users)) {
    users_sel      <- unique(labels_filtered$user_id)[1:max_users]
    labels_filtered <- labels_filtered |> dplyr::filter(user_id %in% users_sel)
    message(sprintf("Modo de teste: usando %d sujeitos", max_users))
  }
  
  all_windows <- list()
  exp_list    <- unique(labels_filtered[, c("exp_id", "user_id")])
  
  for (i in seq_len(nrow(exp_list))) {
    
    exp_id  <- exp_list$exp_id[i]
    user_id <- exp_list$user_id[i]
    
    message(sprintf("Processando exp%02d_user%02d...", exp_id, user_id))
    
    # Lê sinal bruto do experimento
    signal_df <- read_raw_signal(data_dir, exp_id, user_id)
    if (is.null(signal_df)) next
    
    # Segmentos de transição deste experimento
    exp_labels <- labels_filtered |>
      dplyr::filter(exp_id == !!exp_id, user_id == !!user_id)
    
    for (j in seq_len(nrow(exp_labels))) {
      windows <- segment_transitions(signal_df, exp_labels[j, ])
      if (!is.null(windows)) {
        all_windows <- c(all_windows, windows)
      }
    }
  }
  
  # Consolida em um único data frame
  message("\nConsolidando janelas...")
  windows_df <- bind_rows(all_windows) |>
    mutate(
      # Calcula magnitude do acelerômetro (escalar útil para VG)
      acc_mag  = sqrt(acc_x^2  + acc_y^2  + acc_z^2),
      gyro_mag = sqrt(gyro_x^2 + gyro_y^2 + gyro_z^2)
    )
  
  # Salva resultado
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  out_path <- file.path(output_dir, "windows_preprocessed.rds")
  saveRDS(windows_df, out_path)
  
  message(sprintf(
    "\n=== PRÉ-PROCESSAMENTO CONCLUÍDO ===\n  Total de janelas: %d\n  Arquivo salvo em: %s\n",
    length(unique(paste(windows_df$exp_id, windows_df$user_id, windows_df$window_id))),
    out_path
  ))
  
  return(invisible(windows_df))
}

# -----------------------------------------------------------------------------
# 7. Execução
# -----------------------------------------------------------------------------

# Baixa e extrai o dataset (pula se já existir)
download_hapt(dest_dir = "data")

# Roda o pipeline completo
# Para teste rápido com 3 sujeitos, use: max_users = 3
windows_df <- run_preprocessing(
  data_dir   = DATA_DIR,
  output_dir = OUTPUT_DIR,
  activities = TRANSITION_LABELS,
  max_users  = NULL
)

# Resumo rápido
message("\nDistribuição de janelas por atividade:")
print(
  windows_df |>
    distinct(exp_id, user_id, window_id, activity_name) |>
    count(activity_name, name = "n_janelas") |>
    arrange(desc(n_janelas))
)