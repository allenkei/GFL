
library(reticulate)
library(dtwclust)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(readr)
library(igraph)

scenario <- "s1"  
num_node <- "144" 
true_K <- 4       


np <- import("numpy")
TS_data <- np$load(paste0("data/data_",scenario,"_n",num_node,".npz"))
data  <- TS_data$f["y"]  # seq by nodes by T
label <- TS_data$f["labels"] # seq by nodes


T_seq <- dim(data)[1] 
output_label <- array(NA, dim = dim(label)) 

for(seq_iter in 1:T_seq){
  
  one_data <- data[seq_iter,,]
  
  result <- tsclust(one_data, 
                    k = true_K,
                    type = "partitional",
                    distance = "dtw",
                    seed = 42)
  
  # selecting optimal K
  # result_Sil <- sapply(result, cvi, type = "Sil")
  # optimal_k <- K_range[which.max(result_Sil)]
  # result[[which.max(result_Sil)]]@cluster
  
  output_label[seq_iter,] <- result@cluster
  
}



dir.create( paste0("result/c2_",scenario,"_n",num_node), showWarnings = FALSE)

write.csv(output_label, 
          paste0("result/c2_",scenario,"_n",num_node,"/output_label.csv"),
          row.names = F)


###############
# CA data map #
###############
true_K = 10
CA_data <- read.csv("data/CA_data/CA_time_series.csv")


set.seed(42) # different seeds, different results
result <- tsclust(CA_data, 
                  k = true_K,
                  type = "partitional",
                  distance = "dtw",
                  seed = 42)
result@cluster


shapefile_path <- "data/CA_data/California_Counties.shp"
ca_map <- st_read(shapefile_path) # ca_map$NAME gives county names
ca_map$NAME <- gsub("(?i) County", "", ca_map$NAME, perl = TRUE)

ca_map$cluster <- result@cluster # add label to map

tableau20 <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
  "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
  "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5")

cluster_levels <- sort(unique(ca_map$cluster))
cluster_colors <- setNames(tableau20[1:length(cluster_levels)], cluster_levels)


p_map <- ggplot(ca_map) +
  geom_sf(aes(fill = as.factor(cluster)), color = "white", size = 0.2) +
  geom_sf_text(aes(label = NAME), size = 1.5, color = "black") +
  scale_fill_manual(name = "Cluster", values = cluster_colors) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),      
    axis.text = element_blank(),       
    axis.ticks = element_blank(),      
    panel.grid = element_blank(),
    legend.position = "none"
  );p_map



##############
# LA Traffic #
##############
true_k = 5
Traffic_data <- read.csv(file.choose(), header=FALSE)


result <- tsclust(Traffic_data, 
                  k = true_k,
                  type = "partitional",
                  distance = "dtw",
                  seed = 42)




labels <- result@cluster


cluster_nodes <- split(
  0:(length(labels) - 1),
  labels
)


writeLines(
  sapply(cluster_nodes, function(nodes) {
    paste(nodes, collapse = " ")
  }),
  "traffic_labels_c2.txt"
)
