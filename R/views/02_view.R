# =============================================================================
# PLOT 2: O Grafo de Visibilidade (Visibility Graph)
# =============================================================================
library(dplyr)
library(igraph)
library(ggplot2)

# 1. Carrega os grafos já construídos e os dados do sinal
grafos     <- readRDS("output/graphs_nvg_acc_mag.rds")
windows_df <- readRDS("output/windows_preprocessed.rds")

# 2. Pega o primeiro grafo e seu sinal correspondente (reduzido para 40 amostras para clareza)
g_exemplo  <- grafos[[1]]
sinal_orig <- windows_df |> 
  filter(exp_id == graph_attr(g_exemplo, "exp_id"), 
         window_id == graph_attr(g_exemplo, "window_id")) |> 
  pull(acc_mag) |> scale() |> as.numeric()

amostras <- 40
sinal_curto <- sinal_orig[1:amostras]
g_curto <- induced_subgraph(g_exemplo, 1:amostras) # Recorta a rede

# 3. Prepara os dados de vértices e arestas para o ggplot
df_arestas <- as_edgelist(g_curto) |> 
  as.data.frame() |> 
  setNames(c("x", "xend")) |>
  mutate(y = sinal_curto[x], yend = sinal_curto[xend])

df_sinal <- data.frame(t = 1:amostras, y = sinal_curto)

# 4. Plota o sinal com as conexões de rede mapeadas por cima
ggplot() +
  geom_segment(data = df_arestas, aes(x=x, xend=xend, y=y, yend=yend), color="#7F77DD", alpha=0.4, linewidth=0.5) +
  geom_line(data = df_sinal, aes(x=t, y=y), color="gray50", linewidth=0.8, linetype="dashed") +
  geom_point(data = df_sinal, aes(x=t, y=y), color="#378ADD", size=3) +
  geom_segment(data = df_sinal, aes(x=t, xend=t, y=min(sinal_curto)-0.5, yend=y), color="gray80", linewidth=0.3) +
  labs(
    title = "Etapa 2: Natural Visibility Graph (NVG)",
    subtitle = "Linhas roxas representam as 'linhas de visão' (arestas) entre as amostras",
    x = "Tempo", y = "Amplitude Normalizada"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
