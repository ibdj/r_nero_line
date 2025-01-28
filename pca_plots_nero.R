# Load required libraries
library(vegan)  # for PCA
library(ggplot2)  # for plotting

# Assuming your data is in a file named 'plant_data.csv'
data_summarized <- data_analysis |> 
  group_by(plot_id, taxon_code, year, veg_type)  |> 
  summarize(presence = as.numeric(max(raunkiaer_value, na.rm = TRUE))) |> 
  mutate(presence = ifelse(presence > 0, 1, 0))
  ungroup()

# Pivot the data wider
wide_data <- data_summarized %>%
  pivot_wider(
    id_cols = c(plot_id, year, veg_type),
    names_from = taxon_code,
    values_from = presence,
    values_fill = 0
  ) |> 
  unite(col = "site_year_type", plot_id, year, veg_type, sep = "_")

pca_data <- wide_data

# Perform PCA
pca_result <- rda(pca_data[, -1])

# Extract site scores and species scores
site_scores <- scores(pca_result, display = "sites")
species_scores <- scores(pca_result, display = "species")

# Create a data frame for plotting sites
plot_data <- data.frame(
  site_year_type = pca_data$site_year_type,
  PC1 = site_scores[, 1],
  PC2 = site_scores[, 2]
)

# Separate plot_id and year for coloring and shaping
plot_data <- plot_data %>%
  separate(site_year_type, into = c("plot_id", "year","veg_type"), sep = "_")

# Create a data frame for species scores
species_df <- as.data.frame(species_scores)
species_df$species <- rownames(species_df)

# Create the PCA plot
ggplot() +
  # Plot sites
  geom_point(data = plot_data, aes(x = PC1, y = PC2, color = plot_id, shape = year), size = 3) +
  
  # Add species vectors
  geom_segment(data = species_df, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.7) +
  
  # Add species labels
  geom_text(data = species_df, aes(x = PC1, y = PC2, label = species),
            color = "red", size = 3, vjust = -0.5) +
  
  theme_minimal() +
  labs(title = "PCA of Plant Species Presence-Absence Data",
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2, 1] * 100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2, 2] * 100, 2), "%)")) +
  theme(legend.position = "none")

#### Plot with out species####
ggplot() +
  # Plot sites
  geom_point(data = plot_data |> filter(veg_type == 'snowbed'), aes(x = PC1, y = PC2, color = veg_type, shape = year), size = 3) + #color = plot_id
  theme_minimal() +
  labs(title = "PCA of Plant Species Presence-Absence Data",
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2, 1] * 100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2, 2] * 100, 2), "%)")) +
  theme(legend.position = "none")

#### Filtered plots with elipses ####

ggplot() +
  # Plot sites
  geom_point(data = plot_data |> filter(veg_type == 'snowbed'), 
             aes(x = PC1, y = PC2, color = year, shape = veg_type), size = 3) +
  # Add circles around year groups
  geom_mark_ellipse(data = plot_data |> filter(veg_type == 'snowbed'), 
                    aes(x = PC1, y = PC2, group = year, fill = year),
                    alpha = 0.1, expand = 0.01) +
  theme_minimal() +
  labs(title = "PCA of Plant Species Presence-Absence Data",
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2, 1] * 100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2, 2] * 100, 2), "%)")) +
  theme(legend.position = "right")

#### With rings around the groups####
library(ggforce)

ggplot() +
  # Plot sites
  geom_point(data = plot_data, aes(x = PC1, y = PC2, color = plot_id, shape = year), size = 3) +
  # Add rings around groups
  geom_mark_hull(data = plot_data, aes(x = PC1, y = PC2, fill = year, group = year), 
                 alpha = 0.1, expand = 0.01, concavity = 0.5) +
  theme_minimal() +
  labs(title = "PCA of Plant Species Presence-Absence Data",
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2, 1] * 100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2, 2] * 100, 2), "%)")) +
  theme(legend.position = "none")

#### Quatifying the elipses ####

# Assuming plot_data is your existing dataframe
snowbed_data <- plot_data |>  filter(veg_type == 'snowbed')

# Calculate ellipse parameters for each year
ellipse_params <- snowbed_data |> 
  group_by(year) |> 
  summarize(
    mean_PC1 = mean(PC1),
    mean_PC2 = mean(PC2),
    cov_matrix = list(cov(cbind(PC1, PC2))),
    n = n()
  ) %>%
  mutate(
    eigen = map(cov_matrix, ~eigen(.x)$values),
    semi_major = sqrt(eigen[[1]][1]),
    semi_minor = sqrt(eigen[[1]][2])
  )




  # Plot sites

veg_type_to_plot <- "heath"
ggplot()+
  geom_point(data = plot_data |> filter(veg_type == veg_type_to_plot), 
             aes(x = PC1, y = PC2, color = year, shape = veg_type), size = 3) +
  # Add convex hulls around year groups
  geom_polygon(data = plot_data |> filter(veg_type == veg_type_to_plot) |>
                 group_by(year) |>
                 slice(chull(PC1, PC2)),
               aes(x = PC1, y = PC2, fill = year, group = year),
               alpha = 0.1, color = NA) +
  theme_minimal() +
  labs(title = paste0("PCA of p-a data for ", veg_type_to_plot),
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2, 1] * 100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2, 2] * 100, 2), "%)")) +
  theme(legend.position = "right")
