#Trait Groupings

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

#read in trait data

perf_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/stats_mean.csv")


spec_subset <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/chapter_2_trait_subset.csv")
perf_stats$species <- gsub("_", " ", perf_stats$species) # remove underscore from species names
spec_subset$species <- gsub("_", " ", spec_subset$species) # remove underscore from species names

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots")
# Create individual plots for each trait in each habitat_area category
# Loop over unique habitat areas
# Get unique habitat area categories


setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AIC_habitat_groups")
# Loop over habitat area categories
for (h in unique(spec_subset$habitat_area)) {
  cat("\nCreating plots for habitat area:", h)
  # Subset the species data by habitat area
  habitat_subset <- spec_subset %>% filter(habitat_area == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% habitat_subset$species)

  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange((AICc)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm,AICc)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, AICc)

  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AICc", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = AICc, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AICc") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$AICc, color = "red", fill = "red", size=3, shape=1)  +     scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
    
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AICc", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}


setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_habitat_groups")
# AUC
# Loop over habitat area categories
for (h in unique(spec_subset$habitat_area)) {
  cat("\nCreating plots for habitat area:", h)
  
  # Subset the species data by habitat area
  habitat_subset <- spec_subset %>% filter(habitat_area == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% habitat_subset$species)
 
   top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, auc.train)
   
   top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
   
   # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
           
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

# cbi
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/habitat_groups/CBI_habitat_groups")
      
# Loop over habitat area categories
for (h in unique(spec_subset$habitat_area)) {
  cat("\nCreating plots for habitat area:", h)
  
  # Subset the species data by habitat area
  habitat_subset <- spec_subset %>% filter(habitat_area == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% habitat_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("cbi") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#Group by mass
spec_subset <- spec_subset %>% 
  mutate(body_mass_category = cut(body_mass_e, quantile(body_mass_e, probs = seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4")))

spec_subset[19,6] <-c("Q1") #species too low


setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/mass_groups/CBI_mass")
for (h in unique(spec_subset$body_mass_category)) {
  cat("/nCreating plots for mass:", h)
  
  # Subset the species data by habitat area
  mass_subset <- spec_subset %>% filter(body_mass_category == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% mass_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("cbi") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#AUC
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_mass")
for (h in unique(spec_subset$body_mass_category)) {
  cat("\nCreating plots for mass:", h)
  
  # Subset the species data by habitat area
  mass_subset <- spec_subset %>% filter(body_mass_category == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% mass_subset$species)
  
  
  top_peaks <- perf_subset %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    dplyr::select(species, radiuskm, auc.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}



#Group by diet
#CBI

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/diet_groups/CBI_diet")
for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("cbi") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#AUC
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_diet")
for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, auc.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#group by body mass and diet
#CBI

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/CBI_diet")
for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("cbi") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#AUC
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_diet")
for (h in unique(spec_subset$diet_cat_specific)) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, auc.train)
  
  top_peaks_spec <- top_peaks %>% filter (species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

# AUC Diet/Mass categories
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_diet_mass")

for (h in unique(paste0(spec_subset$diet_cat_specific, "_", spec_subset$body_mass_category))) {
  cat("\nCreating plots for diet and mass:", h)
  
  # Subset the species data by diet and mass
  diet_mass_subset <- spec_subset %>% 
    filter(paste0(diet_cat_specific, "_", body_mass_category) == h)
  
  # Subset the performance data by species in this diet and mass category
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_mass_subset$species)
  
  # Find top two peaks for each species
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, auc.train)
  
  top_peaks_spec <- top_peaks %>% 
    filter(species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1)
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#CBI_diet_mass
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/CBI_diet_mass")

for (h in unique(paste0(spec_subset$diet_cat_specific, "_", spec_subset$body_mass_category))) {
  cat("\nCreating plots for diet and mass:", h)
  
  # Subset the species data by diet and mass
  diet_mass_subset <- spec_subset %>% 
    filter(paste0(diet_cat_specific, "_", body_mass_category) == h)
  
  # Subset the performance data by species in this diet and mass category
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_mass_subset$species)
  
  # Find top two peaks for each species
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% 
    filter(species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("CBI") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#AUC diet/diet breadth
# AUC Diet/Mass categories
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/AUC_diet_diet_breadth")

for (h in unique(paste0(spec_subset$diet_cat_specific, "_", spec_subset$diet_breadth))) {
  cat("\nCreating plots for diet and diet breadth:", h)
  
  # Subset the species data by diet and mass
  diet_breadth_subset <- spec_subset %>% 
    filter(paste0(diet_cat_specific, "_", diet_breadth) == h)
  
  # Subset the performance data by species in this diet and mass category
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_breadth_subset$species)
  
  # Find top two peaks for each species
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(auc.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, auc.train)
  
  top_peaks_spec <- top_peaks %>% 
    filter(species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, auc.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for AUC", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = auc.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("AUC") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$auc.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "AUC", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}

#CBI_diet_mass
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/CBI_diet_mass")
spec_subset <- spec_subset %>% 
  mutate(body_mass_category = cut(body_mass_e, quantile(body_mass_e, probs = seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4")))
# Replace spaces with underscores in species column
perf_stats$species <- gsub("_", " ", perf_stats$species)
for (h in unique(paste0(spec_subset$diet_cat_specific, "_", spec_subset$body_mass_category))) {
  cat("\nCreating plots for diet and mass:", h)
  
  # Subset the species data by diet and mass
  diet_mass_subset <- spec_subset %>% 
    filter(paste0(diet_cat_specific, "_", body_mass_category) == h)
  
  # Subset the performance data by species in this diet and mass category
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_mass_subset$species)
  
  # Find top two peaks for each species
  top_peaks <- perf_stats %>%
    filter(radiuskm != 1) %>%
    group_by(species) %>%
    nest() %>%
    mutate(peaks = map(data, ~{
      df <- .x
      df %>%
        arrange(desc(cbi.train)) %>%
        head(2)
    })) %>%
    unnest(peaks) %>%
    ungroup() %>%
    select(species, radiuskm, cbi.train)
  
  top_peaks_spec <- top_peaks %>% 
    filter(species %in% perf_subset$species)
  
  # Create the plot data
  plot_data <- perf_subset %>% 
    select(species, radiuskm, cbi.train)
  
  # Create the plot title
  plot_title <- paste("Performance Metrics for CBI", h)
  
  # Create the plot
  plot <- ggplot(data = plot_data, aes(x = radiuskm, y = cbi.train, col = species)) +
    geom_line() +
    ggtitle(plot_title) +
    ylab("CBI") + xlab("Radius") +
    guides(col = guide_legend("Species")) +
    annotate("point", x = top_peaks_spec$radiuskm, y = top_peaks_spec$cbi.train, color = "red", fill = "red", size=3, shape=1) + scale_x_continuous(breaks = c(1,3,9,15,21,27,33))
  
  # Save the plot to a file
  filename <- paste("performance_plot_", "CBI", "_", h, ".png", sep = "")
  ggsave(filename, plot = plot, width = 12, height = 8, dpi = 300, path = getwd())
}
