
combined_dataa <- list(dive8_nineteenth50,
                      dive8_twentieth50,
                      dive9_seventh50,
                      dive9_eighth50,
                      dive9_ninth50,
                      dive8_sixteenth50,
                      dive8_seventeenth50,
                      dive8_eighteenth50,
                      dive9_first50, 
                      dive9_second50,
                      dive9_third50, 
                      dive9_fourth50,
                      dive9_fifth50,
                      dive8_second50,
                      dive8_third50,
                      dive8_fourth50,
                      dive8_fifth50,
                      dive8_sixth50,
                      dive8_seventh50,
                      dive8_eighth50,
                      dive8_ninth50,
                      dive8_tenth50,
                      dive8_eleventh50,
                      dive8_twelfth50,
                      dive7_first50 ,
                      dive7_second50,
                      dive7_third50,
                      dive7_fourth50,
                      dive7_fifth50,
                      dive10_seventh50,
                      dive10_eighth50,
                      dive10_ninth50,
                      dive10_tenth50,
                      dive10_first50,
                      dive10_second50,
                      dive10_third50,
                      dive10_fourth50,
                      dive10_fifth50,
                      dive10_sixth50)



dive7_fifth50  <- dive7_fifth50 %>%
  mutate(color = NA)

# Create a sequence of transect labels from 1 to 39
transect_labels <- as.character(1:39)

# Add the "transect" column to each data frame
combined_dataa <- lapply(seq_along(combined_dataa), function(i) {
  combined_dataa[[i]]$transect <- transect_labels[i]
  return(combined_dataa[[i]])
})

# Combine the list of data frames into a single data frame
combined_dataframe <- bind_rows(combined_dataa)

# filter for VME taxa 
filtered_df_VME <- combined_dataframe%>%
  filter(grepl("Porifera|Cnidaria", label_hierarchy))


VME_abundance_data <- filtered_df_VME %>%
  group_by(transect, label_name, depth_zone) %>%
  summarise(abundance = n())

VME_abundance_data<- VME_abundance_data %>%
  pivot_wider(names_from = label_name, values_from = abundance, values_fill = 0)

VME_abundance_data<- as.data.frame(VME_abundance_data)

VME_abundance_data <- VME_abundance_data %>%
  mutate(Richness = rowSums(select(., -depth_zone, -transect) > 0))

# Calculate mean richness and standard deviation for each depth zone
richness_summary <- VME_abundance_data %>%
  group_by(depth_zone) %>%
  summarise(
    MeanRichness = mean(Richness),
    StdDevRichness = sd(Richness)
  )

# View the summary
print(richness_summary)

# re-order depth zones 

desired_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m", "1000-1200m", "1200-1400m")

# Convert DepthZone to a factor with the desired order
richness_summary$depth_zone <- factor(richness_summary$depth_zone, levels = desired_order)


ggplot(richness_summary, aes(x = depth_zone, y = MeanRichness)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = MeanRichness - StdDevRichness, ymax = MeanRichness + StdDevRichness), width = 0.2) +
  labs(title = "Mean Richness by Depth Zone with Standard Deviations",
       x = "Depth Zone",
       y = "Mean Richness") +
  theme_minimal()


