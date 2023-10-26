### jaccard disimilarity 


# Normalize abundance values by transect number (row-wise)
normalized_matrix <- t(t(distance_abundance_matrix) / transects)
binary_matrix <- ifelse(normalized_matrix> 0, 1, 0)
jaccard_matrix_standardised <- vegdist(binary_matrix, method = "jaccard")
jaccard_matrix<-as.matrix(jaccard_matrix_standardised)
dendrogram <- hclust(as.dist(jaccard_matrix))
plot(dendrogram)
# row wise normalisation to make dissimilarity matrix !

# Assuming 'abundance_matrix' is your data matrix and 'transects' is a vector of sampling effort
# Standardize abundances by transect number
normalized_matrix <- abundance_matrix / transects
# chosen to rarefy transects because this is more approproate than species number when you have variation in species density

# Perform rarefaction to standardize sampling effort
rarefied_matrix <- rrarefy(abundance_matrix, sample = 98)

# Calculate Chao dissimilarity using the rarefied matrix
chao_dissimilarity <- vegdist(rarefied_matrix, method = "chao")
chao_dissimilarity<-as.matrix(chao_dissimilarity)
dendrogramchao <- hclust(as.dist(chao_dissimilarity))
plot(dendrogram)
tabasco(chao_dissimilarity)
# Reset the plot margins to default values
par(mar = c(5.1, 4.1, 4.1, 2.1))


plot(dendrogramchao, xlab = "", sub = "", main = "", lwd = 2, cex = 1.2, bg = "gray")
heatmap(chao_dissimilarity, lwd = 5, col = colorRampPalette(brewer.pal(9, "BuPu"))(50))




