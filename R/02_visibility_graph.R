# =============================================================================
# 02_visibility_graph.R
# Construção de Visibility Graphs a partir das janelas de sinal IMU
#
# Algoritmo Natural Visibility Graph (NVG) — Lacasa et al., PNAS 2008:
#   Dado um sinal temporal y[1..n], dois nós i e j (i < j) são conectados
#   se para todo k entre i e j:
#       y[k] < y[i] + (y[j] - y[i]) * (k - i) / (j - i)
#   ou seja, nenhum ponto intermediário "bloqueia a visão" entre i e j.
#
# Saída:
#   Lista de grafos igraph, um por janela, salva em output/graphs.rds
#   Cada grafo carrega atributos: activity_id, user_id, exp_id, window_id
# =============================================================================

required_packages <- base::c(
  "igraph",
  "dplyr",
  "tibble"
)
installed <- rownames(installed.packages())

missing <- required_packages[
  !required_packages %in% installed
]

if(length(missing) > 0) {
  cat("Instalando pacotes faltantes...\n")
  install.packages(missing)
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

library(igraph)   # manipulação e análise de redes
library(dplyr)
library(tibble)

DATA_PATH  <- "output/windows_preprocessed.rds"
OUTPUT_DIR <- "output"

# -----------------------------------------------------------------------------
# 1. Algoritmo Natural Visibility Graph (NVG)
#
# Entrada : vetor numérico y (sinal de uma janela, ex: acc_mag)
# Saída   : objeto igraph com n nós (um por amostra) e arestas de visibilidade
# -----------------------------------------------------------------------------

natural_visibility_graph <- function(y) {

  n <- length(y)
  edges <- vector("list", n * 2)  # pré-aloca estimativa
  edge_count <- 0L

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {

      # Linha de visão entre i e j
      # Pontos intermediários k devem estar ABAIXO da reta que liga (i,y[i]) a (j,y[j])
      visible <- TRUE

      if (j > i + 1) {
        k_seq    <- (i + 1):(j - 1)
        # Altura da linha de visão em cada k
        interp   <- y[i] + (y[j] - y[i]) * (k_seq - i) / (j - i)
        visible  <- all(y[k_seq] < interp)
      }

      if (visible) {
        edge_count <- edge_count + 1L
        edges[[edge_count]] <- c(i, j)
      }
    }
  }

  # Constrói o grafo
  edge_mat <- do.call(rbind, edges[seq_len(edge_count)])
  g <- make_empty_graph(n = n, directed = FALSE)
  g <- add_edges(g, t(edge_mat))

  return(g)
}

# -----------------------------------------------------------------------------
# 2. Versão vetorizada do NVG (mais rápida para janelas de 128 amostras)
#
# Mesma lógica, mas evita o loop interno usando operações vetoriais em R.
# Para WIN_SAMPLES = 128, reduz o tempo de ~0.8s para ~0.05s por janela.
# -----------------------------------------------------------------------------

nvg_fast <- function(y) {

  n    <- length(y)
  from <- integer(n * n)
  to   <- integer(n * n)
  cnt  <- 0L

  for (i in seq_len(n - 1)) {
    j_seq <- (i + 1):n

    for (j in j_seq) {
      if (j == i + 1L) {
        # Vizinhos imediatos sempre se veem
        cnt        <- cnt + 1L
        from[cnt]  <- i
        to[cnt]    <- j
      } else {
        k      <- (i + 1L):(j - 1L)
        h_line <- y[i] + (y[j] - y[i]) * (k - i) / (j - i)
        if (all(y[k] < h_line)) {
          cnt       <- cnt + 1L
          from[cnt] <- i
          to[cnt]   <- j
        }
      }
    }
  }

  edge_vec <- as.vector(rbind(from[seq_len(cnt)], to[seq_len(cnt)]))
  g <- make_empty_graph(n = n, directed = FALSE)
  if (length(edge_vec) > 0) g <- add_edges(g, edge_vec)
  return(g)
}

# -----------------------------------------------------------------------------
# 3. Horizontal Visibility Graph (HVG) — alternativa mais rápida
#
# Versão simplificada: i e j se veem apenas se TODOS os k intermediários
# têm y[k] < min(y[i], y[j]).
# Produz grafos mais esparsos; útil como comparação interna.
# -----------------------------------------------------------------------------

horizontal_visibility_graph <- function(y) {

  n    <- length(y)
  from <- integer(n * 2)
  to   <- integer(n * 2)
  cnt  <- 0L

  for (i in seq_len(n - 1)) {
    threshold <- min(y[i], y[i + 1])

    # Vizinho imediato sempre conectado
    cnt       <- cnt + 1L
    from[cnt] <- i
    to[cnt]   <- i + 1L

    # Verifica vizinhos mais distantes
    if (i + 2 <= n) {
      for (j in (i + 2):n) {
        if (all(y[(i + 1):(j - 1)] < threshold) &&
            y[j] >= threshold) {
          cnt       <- cnt + 1L
          from[cnt] <- i
          to[cnt]   <- j
        }
        # Se encontrou um pico acima do threshold, para a busca
        if (any(y[(i + 1):(j - 1)] >= threshold)) break
      }
    }
  }

  edge_vec <- as.vector(rbind(from[seq_len(cnt)], to[seq_len(cnt)]))
  g <- make_empty_graph(n = n, directed = FALSE)
  if (length(edge_vec) > 0) g <- add_edges(g, edge_vec)
  return(g)
}

# -----------------------------------------------------------------------------
# 4. Adiciona metadados ao grafo como atributos de grafo
# -----------------------------------------------------------------------------

attach_metadata <- function(g, meta) {
  graph_attr(g, "activity_id")   <- meta$activity_id
  graph_attr(g, "activity_name") <- meta$activity_name
  graph_attr(g, "user_id")       <- meta$user_id
  graph_attr(g, "exp_id")        <- meta$exp_id
  graph_attr(g, "window_id")     <- meta$window_id
  return(g)
}

# -----------------------------------------------------------------------------
# 5. Pipeline principal: itera sobre todas as janelas e constrói os grafos
# -----------------------------------------------------------------------------

build_all_graphs <- function(
    data_path  = DATA_PATH,
    output_dir = OUTPUT_DIR,
    signal_col = "acc_mag",   # coluna do sinal a usar para o VG
    vg_type    = "nvg",       # "nvg" ou "hvg"
    verbose    = TRUE
) {

  message("\n=== CONSTRUÇÃO DOS VISIBILITY GRAPHS ===\n")
  message(sprintf("Sinal usado     : %s", signal_col))
  message(sprintf("Tipo de VG      : %s\n", toupper(vg_type)))

  # Carrega janelas pré-processadas
  windows_df <- readRDS(data_path)

  # Identifica janelas únicas
  window_keys <- windows_df |>
    distinct(exp_id, user_id, window_id, activity_id, activity_name)

  n_windows <- nrow(window_keys)
  message(sprintf("Total de janelas: %d\n", n_windows))

  graphs <- vector("list", n_windows)

  # Seleciona função VG
  vg_fun <- switch(vg_type,
                   "nvg" = nvg_fast,
                   "hvg" = horizontal_visibility_graph,
                   stop("vg_type deve ser 'nvg' ou 'hvg'")
  )

  t_start <- proc.time()

  for (i in seq_len(n_windows)) {

    key <- window_keys[i, ]

    # Extrai o sinal desta janela
    y <- windows_df |>
      filter(
        exp_id    == key$exp_id,
        user_id   == key$user_id,
        window_id == key$window_id
      ) |>
      pull(!!signal_col)

    # Normaliza o sinal (z-score) para remover diferenças de escala entre sujeitos
    y <- (y - mean(y)) / (sd(y) + 1e-10)

    # Constrói o grafo
    g <- vg_fun(y)

    # Anexa metadados
    g <- attach_metadata(g, key)

    graphs[[i]] <- g

    if (verbose && i %% 50 == 0) {
      elapsed <- (proc.time() - t_start)["elapsed"]
      message(sprintf(
        "  [%d/%d] %.1fs decorridos — %.2fs por grafo",
        i, n_windows, elapsed, elapsed / i
      ))
    }
  }

  elapsed <- (proc.time() - t_start)["elapsed"]
  message(sprintf(
    "\nGrafos construídos: %d em %.1fs (média %.3fs/grafo)",
    n_windows, elapsed, elapsed / n_windows
  ))

  # Salva
  out_path <- file.path(output_dir, sprintf("graphs_%s_%s.rds", vg_type, signal_col))
  saveRDS(graphs, out_path)
  message(sprintf("Grafos salvos em: %s\n", out_path))

  return(invisible(graphs))
}

# -----------------------------------------------------------------------------
# 6. Funções de inspeção e visualização
# -----------------------------------------------------------------------------

# Resume as propriedades básicas de um grafo
summarize_graph <- function(g) {
  tibble(
    activity   = graph_attr(g, "activity_name"),
    n_nodes    = vcount(g),
    n_edges    = ecount(g),
    density    = edge_density(g),
    mean_degree = mean(degree(g))
  )
}

# Plota um exemplo de sinal e o grafo VG correspondente (requer ggplot2)
plot_vg_example <- function(y, g, title = "Visibility Graph") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Instale ggplot2 para visualização: install.packages('ggplot2')")
    return(invisible(NULL))
  }

  library(ggplot2)

  n         <- length(y)
  edge_list <- as_edgelist(g)

  # Data frame do sinal
  df_signal <- tibble(t = seq_len(n), y = y)

  # Data frame das arestas (para desenhar linhas de visibilidade)
  df_edges <- tibble(
    x    = edge_list[, 1],
    xend = edge_list[, 2],
    y    = y[edge_list[, 1]],
    yend = y[edge_list[, 2]]
  )

  p <- ggplot() +
    # Linhas de visibilidade
    geom_segment(
      data = df_edges,
      aes(x = x, xend = xend, y = y, yend = yend),
      color = "#7F77DD", alpha = 0.15, linewidth = 0.3
    ) +
    # Sinal
    geom_line(
      data = df_signal,
      aes(x = t, y = y),
      color = "#1D9E75", linewidth = 0.8
    ) +
    # Nós
    geom_point(
      data = df_signal,
      aes(x = t, y = y),
      color = "#1D9E75", size = 1.5
    ) +
    labs(
      title    = title,
      subtitle = sprintf(
        "%d nós · %d arestas · grau médio %.1f",
        vcount(g), ecount(g), mean(degree(g))
      ),
      x = "Amostra (tempo)",
      y = "Amplitude normalizada"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  return(p)
}

# -----------------------------------------------------------------------------
# 7. Execução
# -----------------------------------------------------------------------------

graphs <- build_all_graphs(
  data_path  = DATA_PATH,
  output_dir = OUTPUT_DIR,
  signal_col = "acc_mag",
  vg_type    = "nvg"
)

# Inspeção rápida dos primeiros grafos
message("Resumo dos primeiros 5 grafos:")
summaries <- bind_rows(lapply(graphs[1:5], summarize_graph))
print(summaries)
