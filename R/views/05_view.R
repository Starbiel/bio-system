# =============================================================================
# Métricas do Classificador
# =============================================================================

# Métricas globais
metricas_gerais <- data.frame(
  Metrica = names(cm$overall),
  Valor = as.numeric(cm$overall)
)

# Métricas por classe
metricas_por_classe <- as.data.frame(cm$byClass)
metricas_por_classe$Classe <- rownames(metricas_por_classe)
metricas_por_classe <- metricas_por_classe |>
  relocate(Classe)

# Matriz de confusão
matriz_confusao <- as.data.frame(cm$table)

# Visualizar
View(metricas_gerais)
View(metricas_por_classe)
View(matriz_confusao)
