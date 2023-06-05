
#tomorrow, add mass and diet boxplots (integrate into main code for grouping)

perf_stats$species <- gsub("_", " ", perf_stats$species) # remove underscore from species names


setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/habitat_groups/CBI_habitat_groups")
# Loop over habitat area categories
for (h in unique(spec_subset$habitat_area)) {
  cat("\nCreating plots for habitat area:", h)
  # Subset the species data by habitat area
  habitat_subset <- spec_subset %>% filter(habitat_area == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% habitat_subset$species)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AICc", h)
  perf_subset$radiuskm <- as.factor(perf_subset$radiuskm)
  # Create the plot
  CBI_plot<- ggplot(perf_subset, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "radii", y = "Continuous Boyce Index", title= paste("Average CBI for", h, "species")) +
    ylim(.5,1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
    
 AUC_plot <-ggplot(perf_subset, aes(x = radiuskm, y = auc.train, fill = radiuskm)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(x = "Radii", y = "AUC", title=paste("Average AUC for", h, "species"))+
  ylim(.4,1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
# Save both plots to files
filename_AUC <- paste("avg_performance_boxplot_", "AUC", "_", h, ".png", sep = "")
ggsave(filename_AUC, plot = AUC_plot, width = 12, height = 8, dpi = 300, path = getwd())

filename_CBI <- paste("performance_boxplot_", "CBI", "_", h, ".png", sep = "")
ggsave(filename_CBI, plot = CBI_plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#mass
spec_subset <- spec_subset %>% 
  mutate(body_mass_category = cut(body_mass_e, quantile(body_mass_e, probs = seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4")))

spec_subset[19,6] <-c("Q1") #species too low

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/mass_groups/CBI_mass")
for (h in unique(spec_subset$body_mass_category)) {
  cat("\nCreating plots for mass:", h)
  
  # Subset the species data by habitat area
  mass_subset <- spec_subset %>% filter(body_mass_category == h)

  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% mass_subset$species)
  
  mean_perf_avg <- perf_stats%>%
    group_by(radiuskm) %>%
    summarize(mean_performance = mean(auc.train))
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  perf_subset$radiuskm <- as.factor(perf_subset$radiuskm)
  # Create the plot
  CBI_plot<- ggplot(perf_subset, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "radii", y = "Continuous Boyce Index", title= paste("Average CBI for", h, "species")) +
    ylim(.5,1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  
  AUC_plot <-ggplot(perf_subset, aes(x = radiuskm, y = auc.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "Radii", y = "AUC", title=paste("Average AUC for", h, "species"))+
    ylim(.4,1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  # Save both plots to files
  filename_AUC <- paste("avg_performance_boxplot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename_AUC, plot = AUC_plot, width = 12, height = 8, dpi = 300, path = getwd())
  
  filename_CBI <- paste("performance_boxplot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename_CBI, plot = CBI_plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#diet

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/diet_groups/CBI_diet")
for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  
  # Subset the performance data by species with this diet
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  perf_subset$radiuskm <- as.factor(perf_subset$radiuskm)
  # Create the plot
  CBI_plot<- ggplot(perf_subset, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "radii", y = "Continuous Boyce Index", title= paste("Average CBI for", h, "species")) +
    ylim(.5,1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  
  AUC_plot <-ggplot(perf_subset, aes(x = radiuskm, y = auc.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "Radii", y = "AUC", title=paste("Average AUC for", h, "species"))+
    ylim(.4,1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  # Save both plots to files
  filename_AUC <- paste("avg_performance_boxplot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename_AUC, plot = AUC_plot, width = 12, height = 8, dpi = 300, path = getwd())
  
  filename_CBI <- paste("performance_boxplot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename_CBI, plot = CBI_plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#Heatmaps for species groups for permutation importance 
##mass
library(dplyr)

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/mass_groups/variable_counts")

for (h in unique(spec_subset$body_mass_category)) {
  cat("\nCreating plots for mass:", h)
  
  # Subset the species data by habitat area
  mass_subset <- spec_subset %>% filter(body_mass_category == h)

    
    top_vars <- perm_imp %>%
      group_by(species, radiuskm) %>%
      do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 6)) %>%
      ungroup()
    
    
    top_vars$species <- gsub("_", " ", top_vars$species) # remove underscore from species names
    
    # Subset by species group'
    diet_perm_subset <-  top_vars %>% 
      filter(species %in% mass_subset$species)
    
    #How many times does each variable get used in each radii model? 
    #Count the number of times each variable is used across all species for each radii
    variable_counts <- mass_perm_subset %>%
      group_by(radiuskm, variable) %>%
      summarise(n_species = n_distinct(species))
    
    # View the resulting data frame
    variable_counts <- variable_counts %>% 
      mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
      mutate(variable = case_when(
        str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
        TRUE ~ variable))
    
    variable_counts$radiuskm <-as.factor(variable_counts$radiuskm)
    
    plot_title <- paste("Top Variable Count", h)
    
    
    ##heat map showing something similar
    # Create a color scale
    top_counts <- ggplot(data = variable_counts, aes(x = as.factor(radiuskm), y = variable, fill = n_species)) +
      geom_tile() +
      scale_fill_gradientn(colors = colorRampPalette(c("lightblue", "darkblue"))(10), na.value = "white") +
      theme_bw() +
      labs(x = "Radius (km)", y = "Variable", fill = "Count", title=paste("Top Variable count", h, "species"),) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
    
    filename_count <- paste("top_variable_counts_","mass", "_", h, ".png", sep = "")
    ggsave(filename_count, plot = top_counts, width = 12, height = 8, dpi = 300, path = getwd())
    
  }

## Diet
library(dplyr)

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/diet_groups/variable_counts")

for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  top_vars <- perm_imp %>%
    group_by(species, radiuskm) %>%
    do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 6)) %>%
    ungroup()
  
  
  top_vars$species <- gsub("_", " ", top_vars$species) # remove underscore from species names
  
  # Subset by species group'
  diet_perm_subset <-  top_vars %>% 
    filter(species %in% diet_subset$species)
  
  #How many times does each variable get used in each radii model? 
  #Count the number of times each variable is used across all species for each radii
  variable_counts <- diet_perm_subset %>%
    group_by(radiuskm, variable) %>%
    summarise(n_species = n_distinct(species))
  
  # View the resulting data frame
  variable_counts <- variable_counts %>% 
    mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
    mutate(variable = case_when(
      str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
      TRUE ~ variable))
  
  variable_counts$radiuskm <-as.factor(variable_counts$radiuskm)
  
  plot_title <- paste("Top Variable Count", h)
  
  
  ##heat map showing something similar
  # Create a color scale
  top_counts <- ggplot(data = variable_counts, aes(x = as.factor(radiuskm), y = variable, fill = n_species)) +
    geom_tile() +
    scale_fill_gradientn(colors = colorRampPalette(c("lightblue", "darkblue"))(10), na.value = "white") +
    theme_bw() +
    labs(x = "Radius (km)", y = "Variable", fill = "Count", title=paste("Top Variable count", h, "species"),) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  filename_count <- paste("top_variable_counts_","diet", "_", h, ".png", sep = "")
  ggsave(filename_count, plot = top_counts, width = 12, height = 8, dpi = 300, path = getwd())
  
}

#habitat
library(dplyr)

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/habitat_groups/variable_counts")

for (h in unique(spec_subset$habitat_area)) {
  cat("\nCreating plots for habitat:", h)
  
  # Subset the species data by habitat area
  habitat_subset <- spec_subset %>% filter(habitat_area == h)
  
  top_vars <- perm_imp %>%
    group_by(species, radiuskm) %>%
    do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 6)) %>%
    ungroup()
  
  
  top_vars$species <- gsub("_", " ", top_vars$species) # remove underscore from species names
  
  # Subset by species group'
  habitat_perm_subset <-  top_vars %>% 
    filter(species %in% habitat_subset$species)
  
  #How many times does each variable get used in each radii model? 
  #Count the number of times each variable is used across all species for each radii
  variable_counts <- habitat_perm_subset %>%
    group_by(radiuskm, variable) %>%
    summarise(n_species = n_distinct(species))
  
  # View the resulting data frame
  variable_counts <- variable_counts %>% 
    mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
    mutate(variable = case_when(
      str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
      TRUE ~ variable))
  
  variable_counts$radiuskm <-as.factor(variable_counts$radiuskm)
  
  plot_title <- paste("Top Variable Count", h)
  
  
  ##heat map showing something similar
  # Create a color scale
  top_counts <- ggplot(data = variable_counts, aes(x = as.factor(radiuskm), y = variable, fill = n_species)) +
    geom_tile() +
    scale_fill_gradientn(colors = colorRampPalette(c("lightblue", "darkblue"))(10), na.value = "white") +
    theme_bw() +
    labs(x = "Radius (km)", y = "Variable", fill = "Count", title=paste("Top Variable count", h, "species"),) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  filename_count <- paste("top_variable_counts_","habitat", "_", h, ".png", sep = "")
  ggsave(filename_count, plot = top_counts, width = 12, height = 8, dpi = 300, path = getwd())
  
}



#line graph

# Subset the species data by habitat area
mass_subset <- spec_subset %>% filter(body_mass_category == h)


top_vars <- perm_imp %>%
  group_by(species, radiuskm) %>%
  do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 6)) %>%
  ungroup()


top_vars$species <- gsub("_", " ", top_vars$species) # remove underscore from species names

# Subset by species group'
mass_perm_subset <-  top_vars %>% 
  filter(species %in% mass_subset$species)

variable_counts <- mass_perm_subset %>% 
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable))


#How many times does each variable get used in each radii model? 
#Count the number of times each variable is used across all species for each radii
top_variable_counts <- variable_counts %>%
  group_by(radiuskm, variable) %>%
  summarise(n_species = n_distinct(species))


variable_counts_sorted <- top_variable_counts %>%
  arrange(radiuskm, desc(n_species)) %>%filter(radiuskm != 1)

variable_counts$radiuskm <-as.factor(variable_counts$radiuskm)

plot_title <- paste("Top Variable Count", h)

# line graph

# Load ggplot2 library
library(ggplot2)

# Sort variable counts by radius and count within each radius
variable_counts_sorted <- variable_counts %>%
  arrange(radiuskm, desc(n_species)) %>%  filter(radiuskm != 1)


# Plot top variable counts by radius as line graph

theme_set(theme_bw(base_size = 14))  # Increase font size and use black and white theme
plot_title <- paste("Top Variable Counts Across Radii")  # Update plot title
legend_title <- "Variable"  # Update legend title


# Plot top variable counts by radius as line graph
variable_counts_sorted$radiuskm <-as.factor(variable_counts_sorted$radiuskm)
top_counts <- ggplot(data = variable_counts_sorted, aes(x = radiuskm, y = n_species, group = variable)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_discrete(name = legend_title) +
  labs(x = "Radius (km)", y = "Count", title = plot_title) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


