# Specify the number of repetitions and cutoff
n_rep <- 100
cutoff <- 90

# Create an empty list to store rarefied richness data
list_richness <- vector("list", n_rep)

for (i in 1:n_rep) {
  print(i)
  rarefied_richness_temp <- rrarefy(species_abundance_matrix, cutoff)
  diversity_temp <- c(specnumber(rarefied_richness_temp))
  list_richness[[i]] <- diversity_temp
}

# Combine the rarefied richness data into a data frame
df_richness <- do.call("cbind", list_richness)
df_richness <- df_richness %>% as.data.frame() %>%
  mutate(DepthZone = 1:n())

# Put data into long format for WLS
df_richness_long <- pivot_longer(df_richness, cols = 1:n_rep, values_to = "Richness")

# Calculate the weights based on the variance of residuals from the linear model
model <- lm(Richness ~ as.factor(DepthZone), data = df_richness_long)
residuals <- residuals(model)
individual_variances <- residuals^2
weights <- 1 / individual_variances

summary(model)
residuals <- resid(model)

# Fit the WLS model
wls_model <- lm(Richness ~ as.factor(DepthZone), data = df_richness_long, weights = weights)

# Summarize the WLS model
summary(wls_model)

plot(wls_model)
