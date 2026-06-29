# =============================================================================
# 03_features.R
# Extração de features topológicas dos Visibility Graphs
#
# Para cada grafo (janela), calcula:
#
#   DISTRIBUIÇÃO DE GRAU
#     - mean_degree       : grau médio dos nós
#     - sd_degree         : desvio padrão do grau
#     - max_degree        : grau máximo (nó hub)
#     - degree_exponent   : expoente da lei de potência (indica scale-free)
#
#   CONECTIVIDADE E ESTRUTURA
#     - density           : densidade de arestas (edges / max_edges)
#     - clustering_coef   : coeficiente de aglomeração médio (small-world)
#     - mean_path_length  : comprimento médio do caminho mais curto
#     - diameter          : maior distância entre dois nós
#
#   CENTRALIDADE
#     - mean_betweenness  : betweenness centrality média
#     - max_betweenness   : betweenness do nó mais central
#     - mean_closeness    : closeness centrality média
#
#   ENTROPIA E COMPLEXIDADE
#     - degree_entropy    : entropia de Shannon da distribuição de grau
#     - modularity        : modularidade da melhor partição encontrada
#
# Saída:
#   output/features_topological.rds — data frame (1 linha por janela)
#   output/features_topological.csv — versão legível
# =============================================================================

library(igraph)
library(dplyr)
library(tibble)
library(readr)

GRAPHS_PATH <- "output/graphs_nvg_acc_mag.rds"
OUTPUT_DIR  <- "output"

# -----------------------------------------------------------------------------
# 1. Entropia de Shannon da distribuição de grau
#
# Mede a irregularidade da conectividade:
#   - Alta entropia = graus muito variados (sinal irregular)
#   - Baixa entropia = graus homogêneos (sinal periódico regular)
# -----------------------------------------------------------------------------

degree_entropy <- function(g) {
  deg   <- degree(g)
  freq  <- table(deg) / vcount(g)   # distribuição de probabilidade
  entr  <- -sum(freq * log2(freq + 1e-10))
  return(as.numeric(entr))
}

# -----------------------------------------------------------------------------
# 2. Expoente da lei de potência (degree_exponent)
#
# Redes scale-free têm P(k) ~ k^(-gamma). Estima gamma via regressão
# log-log da distribuição de grau complementar acumulada (CCDF).
# Valor típico para séries caóticas: 2 < gamma < 3.
# Valor próximo de 0 indica distribuição homogênea (sinal periódico).
# -----------------------------------------------------------------------------

power_law_exponent <- function(g) {
  deg  <- degree(g)
  if (length(unique(deg)) < 3) return(NA_real_)
  
  fit <- tryCatch(
    fit_power_law(deg, xmin = 1),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NA_real_)
  return(fit$alpha)
}

# -----------------------------------------------------------------------------
# 3. Comprimento médio do caminho (trata grafos desconexos)
#
# Grafos VG são sempre conexos por construção (vizinhos imediatos
# estão sempre conectados), mas por segurança usa o componente gigante.
# -----------------------------------------------------------------------------

safe_mean_path <- function(g) {
  if (!is_connected(g)) {
    # Usa o componente gigante
    comp <- components(g)
    giant_idx <- which(comp$membership == which.max(comp$csize))
    g <- induced_subgraph(g, giant_idx)
  }
  mean_distance(g, directed = FALSE, unconnected = FALSE)
}

safe_diameter <- function(g) {
  if (!is_connected(g)) {
    comp <- components(g)
    giant_idx <- which(comp$membership == which.max(comp$csize))
    g <- induced_subgraph(g, giant_idx)
  }
  diameter(g, directed = FALSE)
}

# -----------------------------------------------------------------------------
# 4. Modularidade (comunidades pelo método de Louvain)
#
# Mede se o grafo tem estrutura modular clara.
# Alta modularidade em VG de sinal oscilatório indica que os ciclos
# formam comunidades distintas na rede — relevante para detectar
# regularidade nos ciclos de RCP/transição postural.
# -----------------------------------------------------------------------------

safe_modularity <- function(g) {
  if (ecount(g) == 0) return(0)
  tryCatch({
    comm <- cluster_louvain(g)
    modularity(comm)
  }, error = function(e) NA_real_)
}

# -----------------------------------------------------------------------------
# 5. Extração de todas as features de um único grafo
# -----------------------------------------------------------------------------

extract_features <- function(g) {
  
  deg  <- degree(g)
  n    <- vcount(g)
  m    <- ecount(g)
  
  # --- Grau ---
  mean_deg <- mean(deg)
  sd_deg   <- sd(deg)
  max_deg  <- max(deg)
  d_exp    <- power_law_exponent(g)
  
  # --- Conectividade ---
  dens     <- edge_density(g)
  clust    <- transitivity(g, type = "average")   # clustering coef médio
  mpl      <- safe_mean_path(g)
  diam     <- safe_diameter(g)
  
  # --- Centralidade ---
  btw      <- betweenness(g, normalized = TRUE)
  clo      <- closeness(g, normalized = TRUE)
  
  # --- Entropia e complexidade ---
  d_entr   <- degree_entropy(g)
  modul    <- safe_modularity(g)
  
  # --- Small-world index (aproximação) ---
  # sigma > 1 indica small-world: alta clusterização, curtos caminhos
  # Comparado com grafo aleatório equivalente
  clust_rand <- (2 * m) / (n * (n - 1))   # clustering esperado em grafo aleatório
  mpl_rand   <- log(n) / log(mean_deg + 1e-10)  # caminho esperado em grafo aleatório
  sw_index   <- ifelse(
    clust_rand > 0 & mpl_rand > 0,
    (clust / clust_rand) / (mpl / mpl_rand),
    NA_real_
  )
  
  tibble(
    # Metadados
    activity_id   = graph_attr(g, "activity_id"),
    activity_name = graph_attr(g, "activity_name"),
    user_id       = graph_attr(g, "user_id"),
    exp_id        = graph_attr(g, "exp_id"),
    window_id     = graph_attr(g, "window_id"),
    
    # Tamanho do grafo
    n_nodes       = n,
    n_edges       = m,
    
    # Distribuição de grau
    mean_degree   = mean_deg,
    sd_degree     = sd_deg,
    max_degree    = max_deg,
    degree_exp    = d_exp,
    
    # Conectividade
    density       = dens,
    clustering    = clust,
    mean_path     = mpl,
    diameter      = diam,
    
    # Centralidade
    mean_betw     = mean(btw),
    max_betw      = max(btw),
    mean_close    = mean(clo),
    
    # Complexidade
    degree_entropy = d_entr,
    modularity    = modul,
    sw_index      = sw_index
  )
}

# -----------------------------------------------------------------------------
# 6. Pipeline principal
# -----------------------------------------------------------------------------

extract_all_features <- function(
    graphs_path = GRAPHS_PATH,
    output_dir  = OUTPUT_DIR,
    verbose     = TRUE
) {
  
  message("\n=== EXTRAÇÃO DE FEATURES TOPOLÓGICAS ===\n")
  
  graphs    <- readRDS(graphs_path)
  n_graphs  <- length(graphs)
  message(sprintf("Grafos carregados: %d\n", n_graphs))
  
  features_list <- vector("list", n_graphs)
  t_start       <- proc.time()
  
  for (i in seq_len(n_graphs)) {
    
    features_list[[i]] <- tryCatch(
      extract_features(graphs[[i]]),
      error = function(e) {
        warning(sprintf("Erro no grafo %d: %s", i, e$message))
        NULL
      }
    )
    
    if (verbose && i %% 100 == 0) {
      elapsed <- (proc.time() - t_start)["elapsed"]
      message(sprintf(
        "  [%d/%d] %.1fs decorridos",
        i, n_graphs, elapsed
      ))
    }
  }
  
  features_df <- bind_rows(features_list)
  
  elapsed <- (proc.time() - t_start)["elapsed"]
  message(sprintf(
    "\nFeatures extraídas: %d janelas × %d features em %.1fs",
    nrow(features_df),
    ncol(features_df) - 5,   # desconta colunas de metadados
    elapsed
  ))
  
  # Salva
  rds_path <- file.path(output_dir, "features_topological.rds")
  csv_path <- file.path(output_dir, "features_topological.csv")
  saveRDS(features_df, rds_path)
  write_csv(features_df, csv_path)
  
  message(sprintf("Salvo em:\n  %s\n  %s\n", rds_path, csv_path))
  
  return(invisible(features_df))
}

# -----------------------------------------------------------------------------
# 7. Análise exploratória das features (EDA)
# -----------------------------------------------------------------------------

explore_features <- function(features_df, output_dir = OUTPUT_DIR) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Instale ggplot2 para a EDA: install.packages('ggplot2')")
    return(invisible(NULL))
  }
  
  library(ggplot2)
  
  fig_dir <- file.path(output_dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  
  # Paleta por atividade
  cores <- c(
    "stand-to-sit"  = "#7F77DD",
    "sit-to-stand"  = "#1D9E75",
    "sit-to-lie"    = "#D85A30",
    "lie-to-sit"    = "#BA7517",
    "stand-to-lie"  = "#378ADD",
    "lie-to-stand"  = "#D4537E"
  )
  
  # ---- Figura 1: distribuição do grau médio por atividade ----
  p1 <- ggplot(features_df, aes(x = mean_degree, fill = activity_name)) +
    geom_density(alpha = 0.6, color = NA) +
    scale_fill_manual(values = cores) +
    labs(
      title    = "Distribuição do grau médio por atividade",
      subtitle = "Visibility Graph — sinal de magnitude do acelerômetro",
      x        = "Grau médio",
      y        = "Densidade",
      fill     = "Atividade"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(fig_dir, "fig1_mean_degree.png"),
         p1, width = 9, height = 5, dpi = 150)
  
  # ---- Figura 2: clustering vs. comprimento médio (small-world) ----
  p2 <- ggplot(features_df, aes(x = mean_path, y = clustering,
                                color = activity_name)) +
    geom_point(alpha = 0.4, size = 1.5) +
    scale_color_manual(values = cores) +
    labs(
      title    = "Coeficiente de aglomeração vs. comprimento médio",
      subtitle = "Estrutura small-world por atividade",
      x        = "Comprimento médio do caminho",
      y        = "Coeficiente de aglomeração",
      color    = "Atividade"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(fig_dir, "fig2_smallworld.png"),
         p2, width = 9, height = 5, dpi = 150)
  
  # ---- Figura 3: entropia por atividade (boxplot) ----
  p3 <- ggplot(features_df,
               aes(x = reorder(activity_name, degree_entropy, median),
                   y = degree_entropy,
                   fill = activity_name)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
    scale_fill_manual(values = cores) +
    coord_flip() +
    labs(
      title    = "Entropia da distribuição de grau por atividade",
      subtitle = "Maior entropia = sinal mais irregular",
      x        = NULL,
      y        = "Entropia (bits)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  ggsave(file.path(fig_dir, "fig3_entropy.png"),
         p3, width = 9, height = 5, dpi = 150)
  
  message(sprintf("Figuras salvas em: %s", fig_dir))
  return(invisible(list(p1 = p1, p2 = p2, p3 = p3)))
}

# -----------------------------------------------------------------------------
# 8. Resumo estatístico por atividade
# -----------------------------------------------------------------------------

summarize_by_activity <- function(features_df) {
  features_df |>
    group_by(activity_name) |>
    summarise(
      n_janelas      = n(),
      mean_degree_m  = round(mean(mean_degree), 3),
      clustering_m   = round(mean(clustering, na.rm = TRUE), 3),
      mean_path_m    = round(mean(mean_path, na.rm = TRUE), 3),
      mean_betw_m    = round(mean(mean_betw, na.rm = TRUE), 4),
      entropy_m      = round(mean(degree_entropy), 3),
      modularity_m   = round(mean(modularity, na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    arrange(desc(mean_degree_m))
}

# -----------------------------------------------------------------------------
# 9. Execução
# -----------------------------------------------------------------------------

features_df <- extract_all_features(
  graphs_path = GRAPHS_PATH,
  output_dir  = OUTPUT_DIR
)

# Resumo por atividade
message("\nResumo por atividade:")
print(summarize_by_activity(features_df))

# Gera figuras exploratórias
explore_features(features_df, output_dir = OUTPUT_DIR)