library(reticulate)
library(ggplot2)
library(igraph)
library(patchwork)
np <- reticulate::import("numpy")


################
# Estimated mu #
################


adj_mat1 <- "data/data_s1_n196.npz"
adj_mat2 <- "data/data_s2_n210.npz" 
adj_mat3 <- "data/data_s3_n210.npz" 

np <- reticulate::import("numpy")
data1 <- np$load(adj_mat1)
data2 <- np$load(adj_mat2)
data3 <- np$load(adj_mat3)
label1 <- data1['labels'][1,]
label2 <- data2['labels'][1,]
label3 <- data3['labels'][1,]
rm(data1,data2,data3)
community_colors1 <- c("deepskyblue", "green", "coral", "orange")
community_colors2 <- c("deepskyblue", "green", "orange")  
community_colors3 <- c("deepskyblue", "green", "orange")

# LOAD LEARNED MU FROM result FOLDER
mu_data1 <- read.csv(file.choose(), header = FALSE) # choose the file (s1)
mu_data2 <- read.csv(file.choose(), header = FALSE) # choose the file (s2)
mu_data3 <- read.csv(file.choose(), header = FALSE) # choose the file (s3)

mu_data1$label_col1 <- as.factor(community_colors1[label1+1])
mu_data2$label_col2 <- as.factor(community_colors2[label2+1])
mu_data3$label_col3 <- as.factor(community_colors3[label3+1])

colnames(mu_data1) <- colnames(mu_data2) <- colnames(mu_data3) <- c("dim1", "dim2", "dim3", "label_col")



###########################
# Projection with 3 Views #
###########################
base_theme <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(),
                                                    plot.title = element_blank(), legend.position = "none")

# mu from s1
p1_xy <- ggplot(mu_data1, aes(x = dim1, y = dim2, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim2")

p1_xz <- ggplot(mu_data1, aes(x = dim1, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim3")

p1_yz <- ggplot(mu_data1, aes(x = dim2, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim2", y = "dim3")

# mu from s2
p2_xy <- ggplot(mu_data2, aes(x = dim1, y = dim2, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim2")

p2_xz <- ggplot(mu_data2, aes(x = dim1, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim3")

p2_yz <- ggplot(mu_data2, aes(x = dim2, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim2", y = "dim3")

# mu from s3
p3_xy <- ggplot(mu_data3, aes(x = dim1, y = dim2, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim2")

p3_xz <- ggplot(mu_data3, aes(x = dim1, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim1", y = "dim3")

p3_yz <- ggplot(mu_data3, aes(x = dim2, y = dim3, color = label_col)) +
  geom_point(alpha = 0.8, size = 1.8) + scale_color_identity() +
  base_theme + labs(x = "dim2", y = "dim3")

# 9 by 6
(p1_xy | p1_xz | p1_yz) / (p2_xy | p2_xz | p2_yz) / (p3_xy | p3_xz | p3_yz)

