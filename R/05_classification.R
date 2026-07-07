# =============================================================================
# 05_classification.R
# Treinamento e Avaliação do Classificador (Random Forest)
#
# Pipeline:
#   1. Carregamento e união das features (Baseline + Topológicas)
#   2. Divisão treino/teste (Subject-Independent)
#   3. Treinamento de 3 modelos Random Forest:
#      - Modelo A: Apenas features do Baseline
#      - Modelo B: Apenas features Topológicas (Visibility Graph)
#      - Modelo C: Combinação (Baseline + Topológicas)
#   4. Avaliação (Matriz de Confusão, Acurácia, Kappa)
#   5. Visualização de Resultados (Comparação e Importância das Variáveis)
# =============================================================================

packages <- c("dplyr", "readr", "randomForest", "caret", "ggplot2", "tidyr")

missing <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(missing) > 0){
  message("Instalando pacotes faltantes...")
  install.packages(missing)
}

invisible(lapply(packages, library, character.only = TRUE))

DIR_OUTPUT <- "output"
FILE_TOPO  <- file.path(DIR_OUTPUT, "features_topological.rds")
FILE_BASE  <- file.path(DIR_OUTPUT, "features_baseline.rds")

# -----------------------------------------------------------------------------
# 1. Preparação dos Dados
# -----------------------------------------------------------------------------

message("\n=== PREPARAÇÃO DOS DADOS PARA CLASSIFICAÇÃO ===\n")

if (!file.exists(FILE_TOPO) || !file.exists(FILE_BASE)) {
  stop("Arquivos de features não encontrados. Rode os scripts 03 e 04 primeiro.")
}

feat_topo <- readRDS(FILE_TOPO)
feat_base <- readRDS(FILE_BASE)

# Chaves para juntar (identificadores únicos de cada janela)
keys <- c("exp_id", "user_id", "window_id", "activity_id", "activity_name")

# Junta os dois conjuntos de dados
# Garante que as linhas comparadas são EXATAMENTE as mesmas
dataset <- inner_join(feat_base, feat_topo, by = keys) |>
  # Converte o alvo para factor
  mutate(activity_name = as.factor(activity_name)) |>
  # Remove linhas com NAs gerados por falha na extração de alguma métrica
  drop_na()

message(sprintf("Dataset final combinado: %d janelas prontas para treino.", nrow(dataset)))

# Define as listas de preditores (features)
cols_baseline <- c(
  "acc_mean", "acc_sd", "acc_median", "acc_iqr", "acc_max", "acc_min",
  "acc_rms", "acc_entropy", "acc_zcr",
  "gyro_mean", "gyro_sd", "gyro_rms", "gyro_entropy", "gyro_zcr"
)

cols_topological <- c(
  "n_nodes", "n_edges", "mean_degree", "sd_degree", "max_degree",
  "degree_exp", "density", "clustering", "mean_path", "diameter",
  "mean_betw", "max_betw", "mean_close", "degree_entropy", "modularity", "sw_index"
)

# -----------------------------------------------------------------------------
# 2. Divisão Treino / Teste (Subject-Independent)
# -----------------------------------------------------------------------------

# Para evitar "Data Leakage", dividimos por ID de usuário e não por janela.
set.seed(42)
all_users   <- unique(dataset$user_id)
train_ratio <- 0.7
train_users <- sample(all_users, size = floor(train_ratio * length(all_users)))
test_users  <- setdiff(all_users, train_users)

train_data <- dataset |> filter(user_id %in% train_users)
test_data  <- dataset |> filter(user_id %in% test_users)

message(sprintf(
  "\nDivisão por sujeitos (Train %.0f%% / Test %.0f%%):",
  train_ratio * 100,
  (1 - train_ratio) * 100
))
message(sprintf("  Sujeitos no Treino : %d (Aprox. %d janelas)", length(train_users), nrow(train_data)))
message(sprintf("  Sujeitos no Teste  : %d (Aprox. %d janelas)", length(test_users), nrow(test_data)))

# -----------------------------------------------------------------------------
# 3. Treinamento dos Modelos Random Forest
# -----------------------------------------------------------------------------

message("\n=== TREINAMENTO DOS MODELOS RANDOM FOREST ===\n")

# Controle comum: fixa a semente para garantir reprodutibilidade
set.seed(123)

# Modelo A: Baseline Clássico
message("Treinando Modelo A: Features Clássicas (Baseline)...")
formula_base <- as.formula(paste("activity_name ~", paste(cols_baseline, collapse = " + ")))
rf_base <- randomForest(
  formula_base,
  data = train_data,
  ntree = 300,
  importance = TRUE
)

# Modelo B: Visibility Graph (Topológicas)
message("Treinando Modelo B: Features Topológicas (Visibility Graph)...")
formula_topo <- as.formula(paste("activity_name ~", paste(cols_topological, collapse = " + ")))
rf_topo <- randomForest(
  formula_topo,
  data = train_data,
  ntree = 300,
  importance = TRUE
)

# Modelo C: Combinado
message("Treinando Modelo C: Combinado (Baseline + Topológicas)...")
cols_all <- c(cols_baseline, cols_topological)
formula_all <- as.formula(paste("activity_name ~", paste(cols_all, collapse = " + ")))
rf_all <- randomForest(
  formula_all,
  data = train_data,
  ntree = 300,
  importance = TRUE
)

# -----------------------------------------------------------------------------
# 4. Avaliação no Conjunto de Teste
# -----------------------------------------------------------------------------

evaluate_model <- function(model, test_data) {
  preds <- predict(model, newdata = test_data)
  cm <- confusionMatrix(preds, test_data$activity_name)

  metricas_por_classe <- as.data.frame(cm$byClass)

  return(list(
    accuracy  = as.numeric(cm$overall["Accuracy"]),
    kappa     = as.numeric(cm$overall["Kappa"]),
    precision = mean(metricas_por_classe$Precision, na.rm = TRUE),
    recall    = mean(metricas_por_classe$Recall, na.rm = TRUE),
    f1_score  = mean(metricas_por_classe$F1, na.rm = TRUE),
    matrix    = cm$table
  ))
}

eval_base <- evaluate_model(rf_base, test_data)
eval_topo <- evaluate_model(rf_topo, test_data)
eval_all  <- evaluate_model(rf_all,  test_data)

message("\n=== RESULTADOS NO CONJUNTO DE TESTE (SUBJECT-INDEPENDENT) ===")

# Tabela consolidada com todas as métricas (acurácia, kappa, precision, recall, F1)
# Precision/Recall/F1 são a média macro entre as 6 classes de transição.
tabela_metricas <- data.frame(
  Modelo    = c("A (Baseline)", "B (Topológicas)", "C (Combinado)"),
  Acuracia  = c(eval_base$accuracy,  eval_topo$accuracy,  eval_all$accuracy)  * 100,
  Kappa     = c(eval_base$kappa,     eval_topo$kappa,     eval_all$kappa),
  Precision = c(eval_base$precision, eval_topo$precision, eval_all$precision),
  Recall    = c(eval_base$recall,    eval_topo$recall,    eval_all$recall),
  F1        = c(eval_base$f1_score,  eval_topo$f1_score,  eval_all$f1_score)
)

print(
  transform(
    tabela_metricas,
    Acuracia  = sprintf("%.2f%%", Acuracia),
    Kappa     = sprintf("%.4f", Kappa),
    Precision = sprintf("%.4f", Precision),
    Recall    = sprintf("%.4f", Recall),
    F1        = sprintf("%.4f", F1)
  ),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 5. Visualizações
# -----------------------------------------------------------------------------

fig_dir <- file.path(DIR_OUTPUT, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ---- Figura 6: Comparação de Acurácia ----
df_metrics <- data.frame(
  Modelo = factor(c("Baseline", "Topológicas (VG)", "Combinado"),
                  levels = c("Baseline", "Topológicas (VG)", "Combinado")),
  Acuracia = c(eval_base$accuracy, eval_topo$accuracy, eval_all$accuracy) * 100
)

p6 <- ggplot(df_metrics, aes(x = Modelo, y = Acuracia, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", Acuracia)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("#7F77DD", "#1D9E75", "#D85A30")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Comparação de Performance (Random Forest)",
    subtitle = "Avaliação Subject-Independent no Conjunto de Teste",
    y = "Acurácia Global (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave(file.path(fig_dir, "fig6_accuracy_comparison.png"), p6, width = 8, height = 5, dpi = 150)

# ---- Figura 7: Importância das Variáveis (Modelo Combinado) ----
var_imp <- as.data.frame(importance(rf_all)) |>
  tibble::rownames_to_column(var = "Feature") |>
  mutate(
    # Classifica a feature como Baseline ou Topológica para colorir o gráfico
    Origem = ifelse(Feature %in% cols_baseline, "Baseline", "Topológica")
  ) |>
  arrange(desc(MeanDecreaseGini)) |>
  slice_head(n = 15) # Pega as Top 15 mais importantes

p7 <- ggplot(var_imp, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini, fill = Origem)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_manual(values = c("Baseline" = "#7F77DD", "Topológica" = "#1D9E75")) +
  coord_flip() +
  labs(
    title = "Top 15 Variáveis Mais Importantes (Mean Decrease Gini)",
    subtitle = "Modelo Combinado (Random Forest)",
    x = "Feature",
    y = "Importância (Gini)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(fig_dir, "fig7_variable_importance.png"), p7, width = 9, height = 6, dpi = 150)

# ---- Figura 8: Matriz de Confusão (Modelo Combinado) ----
# Compara as predições do melhor modelo (C) com as classes reais no teste.
cm_df <- as.data.frame(eval_all$matrix)

p8 <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq),
            color = ifelse(cm_df$Freq > mean(cm_df$Freq), "white", "black"),
            size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#F3F6F8", high = "#1D9E75") +
  labs(
    title = "Matriz de Confusão — Modelo Combinado (Random Forest)",
    subtitle = "Avaliação Subject-Independent no conjunto de teste",
    x = "Classe Real (Ground Truth)",
    y = "Classe Prevista pelo Modelo",
    fill = "Frequência"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(face = "bold"),
    panel.grid  = element_blank()
  )

ggsave(file.path(fig_dir, "fig8_confusion_matrix.png"), p8, width = 8, height = 6, dpi = 150)

message(sprintf("\nGráficos de resultados salvos em: %s", fig_dir))
message("\n=== PIPELINE CONCLUÍDO COM SUCESSO! ===")
