
set.seed(1)
calculate_species_frequency <- function(data) {
  data |> 
    # Group by vt_section and taxon_code, count unique plot_ids
    group_by(vt_section, taxon_code, year, veg_type) %>%
    summarise(species_frequency = n_distinct(plot_id), .groups = "drop") |> 
    # Join with total plot counts per vt_section
    left_join(
      data |> 
        group_by(vt_section) %>%
        summarise(total_plots = n_distinct(plot_id), .groups = "drop"),
      by = "vt_section"
    ) |> 
    # Calculate frequency
    mutate(frequency = species_frequency / total_plots*100)
}


results <- calculate_species_frequency(data_analysis) 


# names form result: "vt_section"        "taxon_code"        "year"              "species_frequency" "total_plots"       "frequency"  

frequency_long <- results %>%
  pivot_wider(
    id_cols = c(vt_section, taxon_code, veg_type),
    names_from = year,
    values_from = c(species_frequency, total_plots, frequency),
    names_glue = "{.value}_{year}"
  ) %>%
  # Reorder columns for better readability
  select(vt_section, taxon_code, veg_type,
         starts_with("species_frequency_"),
         starts_with("total_plots_"),
         starts_with("frequency_"))

library(tidyr)
library(dplyr)
library(tibble)

# Assuming your dataframe is named 'df'
results_wide <- results %>%
  mutate(section_id = paste(vt_section, year, veg_type, sep = "_")) %>%
  select(section_id,year, taxon_code, frequency) %>%
  pivot_wider(names_from = taxon_code, values_from = frequency)

print(df_wide)

#### cluster analyse #####

df_cluster <- results_wide |>  
  tibble::column_to_rownames("section_id") |> 
  filter(year == 2007) |> 
  select(-c(year))
  
df_cluster[is.na(df_cluster)] <- 0


# Using Bray-Curtis distance, which is common in ecological studies
library(vegan)
dist_matrix <- vegdist(df_cluster, method = "bray")

library(cluster)
hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc, hang = -1, cex = 0.6)
rect.hclust(hc, k = 6, border = "red")
frect.hclust(hc, h = 50, border = "blue")

library(factoextra)
fviz_nbclust(df_cluster, FUN = hcut, method = "silhouette")
clusters <- cutree(hc, k = 6)
df_cluster$cluster <- clusters

library(vegan)
nmds <- metaMDS(df_cluster, distance = "bray")
plot(nmds, type = "n")
points(nmds, cex = 2, pch = 21, bg = clusters, col = "black")
