# =============================================================================
# PLOT 1: Visualização da Janela de Sinal (Pré-processamento)
# =============================================================================
library(dplyr)
library(ggplot2)

# 1. Carrega os dados processados
windows_df <- readRDS("output/windows_preprocessed.rds")

# 2. Isola apenas uma janela específica da atividade "sit-to-stand"
exemplo_sinal <- windows_df |>
  dplyr::filter(activity_name == "sit-to-stand") |>
  dplyr::filter(exp_id == min(exp_id), user_id == min(user_id), window_id == min(window_id)) |>
  dplyr::mutate(tempo = dplyr::row_number()) # Cria um eixo de tempo sequencial

# 3. Plota o sinal de aceleração
ggplot(exemplo_sinal, aes(x = tempo, y = acc_mag)) +
  geom_line(color = "#1D9E75", linewidth = 1.2) +
  geom_point(color = "#0A5C40", size = 2, alpha = 0.6) +
  labs(
    title = "Etapa 1: O Movimento Físico no Domínio do Tempo",
    subtitle = "Sinal de aceleração (Magnitude) durante a transição 'Sit-to-Stand'",
    x = "Amostra (Tempo)",
    y = "Aceleração Filtrada (g)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
