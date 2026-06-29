# =============================================================================
# PLOT 5: Avaliação do Classificador (Matriz de Confusão)
# =============================================================================
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(tidyr)

# 1. Carrega e une os dados
feat_topo <- readRDS("output/features_topological.rds")
feat_base <- readRDS("output/features_baseline.rds")
dataset <- inner_join(feat_base, feat_topo, by = c("exp_id", "user_id", "window_id", "activity_id", "activity_name")) |>
  drop_na() |> mutate(activity_name = as.factor(activity_name))

# 2. Divisão treino/teste rápida e treino do modelo
set.seed(42)
train_users <- sample(unique(dataset$user_id), size = floor(0.7 * length(unique(dataset$user_id))))
train_data  <- dataset |> filter(user_id %in% train_users)
test_data   <- dataset |> filter(!user_id %in% train_users)

cols_pred <- c("acc_rms", "gyro_rms", "acc_entropy", "mean_degree", "degree_entropy", "clustering")
form <- as.formula(paste("activity_name ~", paste(cols_pred, collapse = " + ")))
modelo_rapido <- randomForest(form, data = train_data, ntree = 100)

# 3. Predição e Matriz de Confusão
preds <- predict(modelo_rapido, test_data)
cm <- confusionMatrix(preds, test_data$activity_name)

# 4. Converte a matriz para um formato plotável no ggplot
cm_df <- as.data.frame(cm$table)

# 5. Plota o Heatmap da Matriz de Confusão
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), color = ifelse(cm_df$Freq > mean(cm_df$Freq), "white", "black"), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#F3F6F8", high = "#1D9E75") +
  labs(
    title = "Etapa 5: Matriz de Confusão (Random Forest)",
    subtitle = "Comparando as predições do modelo com a realidade",
    x = "Classe Real (Ground Truth)",
    y = "Classe Prevista pelo Modelo",
    fill = "Frequência"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )