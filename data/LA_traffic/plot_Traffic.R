

library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)



Traffic_cluster <- readLines(file.choose()) # GFL cluster result
Traffic_mu <- read.csv(file.choose(), header=F) # GFL estimated mu

node_labels <- rep(0, 207)
for (i in seq_along(Traffic_cluster)) {
  nodes <- as.numeric(strsplit(Traffic_cluster[i], "\\s+")[[1]]) + 1
  node_labels[nodes] <- i
}

colnames(Traffic_mu) <- c("dim1", "dim2", "dim3")
Traffic_mu$label <- as.factor(node_labels) 

cluster_colors <- c(
  '#B2182B',  # Dark red
  '#67A9CF',  # Medium blue
  '#EF8A62',  # Coral
  '#D1E5F0',  # Light blue
  '#2166AC',  # Dark blue
  '#FDD8A9'  # Light peach
)

#################################
# 3D latent space visualization #
#################################


base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  )


p1_xy <- ggplot(Traffic_mu, aes(x = dim1, y = dim2, color = label)) +
  geom_point(alpha = 0.8, size = 1.8) +
  scale_color_manual(values = cluster_colors) +
  base_theme + labs(x = "dim1", y = "dim2")


p1_xz <- ggplot(Traffic_mu, aes(x = dim1, y = dim3, color = label)) +
  geom_point(alpha = 0.8, size = 1.8) +
  scale_color_manual(values = cluster_colors) +
  base_theme + labs(x = "dim1", y = "dim3")


p1_yz <- ggplot(Traffic_mu, aes(x = dim2, y = dim3, color = label)) +
  geom_point(alpha = 0.8, size = 1.8) +
  scale_color_manual(values = cluster_colors) +
  base_theme + labs(x = "dim2", y = "dim3")


(p1_xy | p1_xz | p1_yz)

