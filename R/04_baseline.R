# =============================================================================
# 04_baseline.R
# Extração de features estatísticas clássicas (Baseline)
#
# Pipeline:
#   1. Carregamento das janelas de sinal (windows_preprocessed.rds)
#   2. Definição de funções auxiliares (RMS, Entropia de Shannon no tempo)
#   3. Extração vetorizada via dplyr (Média, SD, Mediana, IQR, Max, Min, RMS, Entropia)
#   4. Exportação das features (RDS e CSV)
#   5. Análise exploratória básica do baseline
#
# Objetivo: 
#   Fornecer um conjunto padrão de features clássicas para treinar modelos
#   tradicionais (ex: Random Forest, SVM) e comparar com o poder preditivo
#   das features topológicas extraídas via Visibility Graph.
# =============================================================================

packages <- c("dplyr", "readr", "tidyr")

missing <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(missing) > 0){
  message("Instalando pacotes faltantes...")
  install.packages(missing)
}

invisible(lapply(packages, library, character.only = TRUE))

DATA_PATH  <- "output/windows_preprocessed.rds"
OUTPUT_DIR <- "output"

# -----------------------------------------------------------------------------
# 1. Funções matemáticas e estatísticas auxiliares
# -----------------------------------------------------------------------------

# Root Mean Square (RMS) - Mede a "energia" do sinal no domínio do tempo
calc_rms <- function(x) {
  sqrt(mean(x^2, na.rm = TRUE))
}

# Entropia de Shannon do sinal contínuo (usando histograma)
# Mede a incerteza/informação contida na distribuição de amplitude do sinal
calc_signal_entropy <- function(x, bins = 10) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  
  # Calcula a frequência dos valores divididos em 'bins'
  h <- hist(x, breaks = bins, plot = FALSE)
  probs <- h$counts / sum(h$counts)
  
  # Remove probabilidades nulas para não dar erro no log2
  probs <- probs[probs > 0]
  
  # H(X) = - sum( p(x) * log2(p(x)) )
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

# Taxa de Cruzamento por Zero (Zero-Crossing Rate - ZCR) com base na média
calc_zcr <- function(x) {
  x_centered <- x - mean(x, na.rm = TRUE)
  sum(diff(x_centered > 0) != 0, na.rm = TRUE) / length(x)
}

# -----------------------------------------------------------------------------
# 2. Pipeline de Extração do Baseline
# -----------------------------------------------------------------------------

extract_baseline_features <- function(
    data_path  = DATA_PATH,
    output_dir = OUTPUT_DIR
) {
  
  message("\n=== EXTRAÇÃO DE FEATURES DE BASELINE (ESTATÍSTICAS) ===\n")
  
  if (!file.exists(data_path)) {
    stop("Arquivo pre-processado não encontrado. Rode o 01_preprocessing.R primeiro.")
  }
  
  windows_df <- readRDS(data_path)
  
  t_start <- proc.time()
  
  # O dplyr agrupa por janela e extrai todas as features rapidamente
  baseline_features <- windows_df |>
    group_by(exp_id, user_id, activity_id, activity_name, window_id) |>
    summarise(
      # --- Features para a Magnitude do Acelerômetro ---
      acc_mean    = mean(acc_mag, na.rm = TRUE),
      acc_sd      = sd(acc_mag, na.rm = TRUE),
      acc_median  = median(acc_mag, na.rm = TRUE),
      acc_iqr     = IQR(acc_mag, na.rm = TRUE),
      acc_max     = max(acc_mag, na.rm = TRUE),
      acc_min     = min(acc_mag, na.rm = TRUE),
      acc_rms     = calc_rms(acc_mag),
      acc_entropy = calc_signal_entropy(acc_mag),
      acc_zcr     = calc_zcr(acc_mag),
      
      # --- Features para a Magnitude do Giroscópio ---
      gyro_mean    = mean(gyro_mag, na.rm = TRUE),
      gyro_sd      = sd(gyro_mag, na.rm = TRUE),
      gyro_rms     = calc_rms(gyro_mag),
      gyro_entropy = calc_signal_entropy(gyro_mag),
      gyro_zcr     = calc_zcr(gyro_mag),
      
      .groups = "drop"
    )
  
  elapsed <- (proc.time() - t_start)["elapsed"]
  
  message(sprintf(
    "Features clássicas extraídas: %d janelas × %d features em %.1fs",
    nrow(baseline_features),
    ncol(baseline_features) - 5, # Desconta as 5 colunas de identificação
    elapsed
  ))
  
  # Salva os resultados
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  rds_path <- file.path(output_dir, "features_baseline.rds")
  csv_path <- file.path(output_dir, "features_baseline.csv")
  
  saveRDS(baseline_features, rds_path)
  write_csv(baseline_features, csv_path)
  
  message(sprintf("Arquivos salvos em:\n  %s\n  %s\n", rds_path, csv_path))
  
  return(invisible(baseline_features))
}

# -----------------------------------------------------------------------------
# 3. Análise exploratória das features (EDA) para o Baseline
# -----------------------------------------------------------------------------

explore_baseline <- function(baseline_df, output_dir = OUTPUT_DIR) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Instale ggplot2 para a EDA: install.packages('ggplot2')")
    return(invisible(NULL))
  }
  
  library(ggplot2)
  
  fig_dir <- file.path(output_dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  
  # Paleta de cores padronizada (a mesma usada no 03_features.R)
  cores <- c(
    "stand-to-sit"  = "#7F77DD",
    "sit-to-stand"  = "#1D9E75",
    "sit-to-lie"    = "#D85A30",
    "lie-to-sit"    = "#BA7517",
    "stand-to-lie"  = "#378ADD",
    "lie-to-stand"  = "#D4537E"
  )
  
  # ---- Figura 4: Energia do Sinal (RMS) por Atividade ----
  p4 <- ggplot(baseline_df, aes(x = acc_rms, fill = activity_name)) +
    geom_density(alpha = 0.6, color = NA) +
    scale_fill_manual(values = cores) +
    labs(
      title    = "Distribuição do RMS do Acelerômetro",
      subtitle = "Baseline: Energia do sinal no domínio do tempo",
      x        = "RMS (Root Mean Square)",
      y        = "Densidade",
      fill     = "Atividade"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(fig_dir, "fig4_baseline_rms.png"), p4, width = 9, height = 5, dpi = 150)
  
  # ---- Figura 5: Desvio Padrão (Acc) vs Desvio Padrão (Gyro) ----
  p5 <- ggplot(baseline_df, aes(x = acc_sd, y = gyro_sd, color = activity_name)) +
    geom_point(alpha = 0.6, size = 1.5) +
    scale_color_manual(values = cores) +
    labs(
      title    = "Dispersão do Acelerômetro vs Giroscópio",
      subtitle = "Baseline: Comparação de desvios padrão (SD)",
      x        = "Desvio Padrão - Acelerômetro",
      y        = "Desvio Padrão - Giroscópio",
      color    = "Atividade"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(fig_dir, "fig5_baseline_scatter.png"), p5, width = 9, height = 5, dpi = 150)
  
  message(sprintf("Figuras do Baseline salvas em: %s", fig_dir))
  return(invisible(list(p4 = p4, p5 = p5)))
}

# -----------------------------------------------------------------------------
# 4. Execução Principal
# -----------------------------------------------------------------------------

baseline_features <- extract_baseline_features(
  data_path  = DATA_PATH,
  output_dir = OUTPUT_DIR
)

# Resumo rápido por atividade
message("\nResumo do Baseline por atividade:")
print(
  baseline_features |>
    group_by(activity_name) |>
    summarise(
      n_janelas     = n(),
      acc_rms_m     = round(mean(acc_rms), 3),
      acc_sd_m      = round(mean(acc_sd), 3),
      acc_entropy_m = round(mean(acc_entropy), 3),
      gyro_rms_m    = round(mean(gyro_rms), 3),
      .groups = "drop"
    ) |>
    arrange(desc(acc_rms_m))
)

# Gera as figuras exploratórias do baseline
explore_baseline(baseline_features, output_dir = OUTPUT_DIR)