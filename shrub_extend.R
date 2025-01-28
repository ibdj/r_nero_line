#### Shrub extend ####

shrub <- data_analysis |> 
  group_by(year,plot_id,func_type) |> 
  summarise(frec = length(taxon_code)) |> 
  filter(func_type == "shrub")

salgla <- data_analysis |> 
  group_by(year,plot_id,func_type) |>
  filter(taxon_code == "salgla")
  summarise(frec = length(taxon_code))

func_type <- data_analysis |> 
  group_by(year,plot_id,vt_section,func_type,veg_type) |> 
  summarise(frec = length(taxon_code))


# Calculate average frequency per plot for each year
avg_frec_per_plot <- shrub %>%
  group_by(year) %>%
  summarise(avg_frec = mean(frec), 
            plot_count = n()
            )

# Create the bar plot
# Create the bar plot with trendline
ggplot(avg_frec_per_plot, aes(x = year, y = avg_frec)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(x = "Year", y = "Average Frequency per Plot", 
       title = "Average Frequency per Plot by Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("n=%d", plot_count)), 
            vjust = -0.5, size = 3.5)

#### Percentage plots with shrub ####

# Calculate the total number of unique plots by year and vegetation type
plots_count <- func_type %>%
  group_by(year, veg_type) %>%
  summarise(total_plots = n_distinct(plot_id), .groups = "drop")

# Calculate the number of plots with 'shrub' as a functional type
shrub_count <- func_type %>%
  filter(func_type == 'shrub') %>%
  group_by(year, veg_type) %>%
  summarise(shrub_plots = n_distinct(plot_id), .groups = "drop")

# Merge counts and calculate percentage
result <- left_join(plots_count, shrub_count, by = c("year", "veg_type")) %>%
  mutate(shrub_plots = coalesce(shrub_plots, 0),
         percentage_shrub = (shrub_plots / total_plots) * 100)

# Display the result
result %>%
  select(year, veg_type, total_plots, shrub_plots, percentage_shrub)

# Display the result
result %>%
  select(year, total_plots, shrub_plots, percentage_shrub)


# Create the grouped bar chart
ggplot(result, aes(x = factor(year), y = percentage_shrub, fill = veg_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_smooth(aes(color = veg_type), method = "lm", se = FALSE, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_shrub)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Year", 
       y = "Percentage of Shrub Plots", 
       title = "Percentage of Shrub Plots by Year and Vegetation Type",
       fill = "Vegetation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

ggplot(result, aes(x = year, y = percentage_shrub)) +
  geom_bar(stat = "identity", fill = "blue", width = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_shrub)), 
            vjust = -0.5,hjust = -0.25, size = 3) +
  #facet_grid(veg_type ~ ., scales = "free_y")+
    facet_wrap(~ veg_type, scales = "free_y", ncol = 2) +
  labs(x = "Year", 
       y = "Percentage of Shrub Plots", 
       title = "Percentage of plots with shrub by year and vegetation ype") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = c(2007, 2012, 2017, 2022))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))

#### Looking at salix glauca ####

# Calculate the number of plots with 'shrub' as a functional type
salgla <- data_analysis %>%
  filter(taxon_code == 'salgla') %>%
  group_by(year, veg_type) %>%
  summarise(salgla_plots = n_distinct(plot_id)) #, .groups = "drop"

# Merge counts and calculate percentage
result_salgla <- left_join(plots_count, salgla, by = c("year", "veg_type")) %>%
  mutate(salgla_plots = coalesce(salgla_plots, 0),
         percentage_salgla = (salgla_plots / total_plots) * 100)

# plotting only Salix glauca #

ggplot(result_salgla, aes(x = year, y = percentage_salgla)) +
  geom_bar(stat = "identity", fill = "blue", width = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_salgla)), 
            vjust = -0.5,hjust = -0.25, size = 3) +
  #facet_grid(veg_type ~ ., scales = "free_y")+
  facet_wrap(~ veg_type, scales = "free_y", ncol = 2) +
  labs(x = "Year", 
       y = "Percentage of Shrub Plots", 
       title = expression("Percentage of plots with " * italic("Salix glauca") * " by year and vegetation type")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = c(2007, 2012, 2017, 2022))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "skyblue", size = 0),
        strip.text = element_text(face = "bold"))

#### Looking at Betula nana ####

# Calculate the number of plots with 'shrub' as a functional type
betnan <- data_analysis %>%
  filter(taxon_code == 'betnan') %>%
  group_by(year, veg_type) %>%
  summarise(betnan_plots = n_distinct(plot_id)) #, .groups = "drop"

# Merge counts and calculate percentage
result_betnan <- left_join(plots_count, betnan, by = c("year", "veg_type")) %>%
  mutate(betnan_plots = coalesce(betnan_plots, 0),
         percentage_betnan = (betnan_plots / total_plots) * 100)

# plotting only Salix glauca #

ggplot(result_betnan, aes(x = year, y = percentage_betnan)) +
  geom_bar(stat = "identity", fill = "blue", width = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_betnan)), 
            vjust = -0.5,hjust = -0.25, size = 3) +
  #facet_grid(veg_type ~ ., scales = "free_y")+
  facet_wrap(~ veg_type, scales = "free_y", ncol = 2) +
  labs(x = "Year", 
       y = "Percentage of Shrub Plots", 
       title = expression("Percentage of plots with " * italic("Betula nana") * " by year and vegetation type")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = c(2007, 2012, 2017, 2022))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "skyblue", size = 0),
        strip.text = element_text(face = "bold"))


#### number of plots with salgla pr section ####

vt_count <- data_analysis %>%
  group_by(year, veg_type, vt_section) %>%
  summarise(total_plots = n_distinct(plot_id), .groups = "drop")

vt_shrub_count <- data_analysis %>%
  filter(taxon_code == 'salgla') %>%
  group_by(year, veg_type, vt_section) %>%
  summarise(shrub_plots = n_distinct(plot_id), .groups = "drop") |> 
  filter(veg_type == "heath")

vt_result <- left_join(vt_count, vt_shrub_count, by = c("year", "veg_type", "vt_section")) %>%
  mutate(shrub_plots = coalesce(shrub_plots, 0),
         percentage_shrub = (shrub_plots / total_plots) * 100) |> 
  filter(veg_type == "heath")

plot(x = vt_result$year, y = vt_result$percentage_shrub)+
  abline(vt_result$percentage_shrub ~ vt_result$year)

summary(lm(vt_result$percentage_shrub ~ vt_result$year,data = vt_result))
