# =============================================================================
# PLOT 3: Padrões nas Features de Rede (Topologia)
# =============================================================================
library(dplyr)
library(ggplot2)

# 1. Carrega as features extraídas da rede
feat_topo <- readRDS("output/features_topological.rds")

# 2. Cria um Boxplot comparando a entropia e o grau máximo das diferentes transições
ggplot(feat_topo, aes(x = reorder(activity_name, degree_entropy, median), y = degree_entropy, fill = activity_name)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Etapa 3: Assinatura Topológica das Atividades",
    subtitle = "A Entropia do Grafo separa movimentos regulares de movimentos caóticos",
    x = "Transição Postural",
    y = "Entropia da Distribuição de Grau (Complexidade)",
    fill = "Atividade"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))