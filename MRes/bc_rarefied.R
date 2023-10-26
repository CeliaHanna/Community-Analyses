#### calculating Bray-curtis distances on rarefied datasets ####

library(vegan)

rarefy_function <- function(abundance_matrix, sample_size) {
  rarefied_data <- rrarefy(abundance_matrix, sample_size)
  return(rarefied_data)
}

num_datasets <- 10
sample_size <- 200
rarefied_datasets <- list()

for (i in 1:num_datasets) {
  rarefied_data <- rarefy_function(simper_abundance_matrix, 200 )
  rarefied_datasets[[i]] <- rarefied_data
}

num_datasets <- 100
bray_curtis_matrices <- list()

for (i in 1:num_datasets) {
  diss_matrix <- vegdist(rarefied_datasets[[i]], method = "bray")
  bray_curtis_matrices[[i]] <- diss_matrix
}

average_bray_curtis_matrix <- Reduce(`+`, bray_curtis_matrices) / num_datasets
average_bray_curtis_matrix <- as.matrix(average_bray_curtis_matrix)

# now plot

# Hierarchical clustering
dendrogram <- hclust(as.dist(1 - average_bray_curtis_matrix))
plot(dendrogram)

# Create a heatmap
heatmap(average_bray_curtis_matrix, dendrogram = "row", scale = "none", trace = "none", col = colorRampPalette(c("blue", "white", "red"))(100))

# Add the dendrogram to the heatmap
op <- par(mar = c(5, 4, 2, 10))
heatmapdendrogram(dendrogram, Rowv = as.dendrogram(dendrogram), Colv = NULL, col = "black", lwd = 2)
par(op)
