#Title: Trait groupings

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code generates boxplots showing model performance for different species groupings based on mass and feeding type (Wilman et al., 2014).

#Data input: CSV files of model performance and species traits.

#Data output: Generates multipanel boxplots of model performance for species grouped by trait.

#Author: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

library(gridExtra)

# read in model performance and trait data

perf_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/stats_mean.csv")

# Species trait subset (equivilent to Table 1 in manuscript)
spec_subset <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/chapter_2_trait_subset.csv")

#remove underscores 
perf_stats$species <- gsub("_", " ", perf_stats$species) # remove underscore from species names

#generate quantiles of body mass
spec_subset <- spec_subset %>% 
  mutate(body_mass_category = cut(body_mass_e, quantile(body_mass_e, probs = seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4")))
spec_subset[19,6] <- c("Q1") #species too low

#Set working directory for where you would like the plot to save
setwd("INSERT FILE PATH")

# Create a list to store the plots
plots <- list()

#Mass boxplots
for (h in unique(spec_subset$body_mass_category)) {
  cat("\nCreating plots for mass:", h)
  
  # Subset the species data by habitat area
  mass_subset <- spec_subset %>% filter(body_mass_category == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% mass_subset$species)
  
  
  # Create the plot title

  perf_subset$radiuskm <- as.factor(perf_subset$radiuskm)
  
  # Create the plot
  CBI_plot <- ggplot(perf_subset, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "grain", y = "Continuous Boyce Index", title = paste("Mass:", h)) +
    ylim(.5, 1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  

  # Store the plots in the list
  plots[[paste("CBI_", h)]] <- CBI_plot
}

# Reverse the order of the plots in the list
plots <- plots[rev(names(plots))]

# Create a multipanel plot using grid.arrange from gridExtra
multiplot_1 <- do.call(gridExtra::grid.arrange, c(plots, ncol = 4))

# Save the multipanel plot to a file
filename_multipanel <- "multipanel_plot_cbi_mass.png"
ggsave(filename_multipanel, plot = multiplot_1, width = 12, height = 8, dpi = 300, path = getwd())

#diet boxplots

setwd("INSERT FILE PATH")

# Create a list to store the plots
plots <- list()
order_categories <- c("frugivore", "fruitnect", "folivore", "omnivore")
for (h in order_categories) {
  cat("\nCreating plots for diet:", h)
  
  # Subset the species data by habitat area
  diet_subset <- spec_subset %>% filter(diet_cat_specific == h)
  
  # Subset the performance data by species in this habitat area
  perf_subset <- perf_stats %>% 
    filter(species %in% diet_subset$species)
  
  
  # Create the plot title
  
  perf_subset$radiuskm <- as.factor(perf_subset$radiuskm)
  
  # Create the plot
  CBI_plot <- ggplot(perf_subset, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
    labs(x = "grain", y = "Continuous Boyce Index", title = paste("Diet:", h)) +
    ylim(.5, 1) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  
  # Store the plots in the list
  plots[[paste("CBI_", h)]] <- CBI_plot
}

# Create a multipanel plot using grid.arrange from gridExtra
multiplot_2 <- do.call(gridExtra::grid.arrange, c(plots, ncol = 4))

# Save the multipanel plot to a file
filename_multipanel <- "multipanel_plot_cbi_diet.png"
ggsave(filename_multipanel, plot = multiplot, width = 12, height = 8, dpi = 300, path = getwd())

# Create the combined 2-panel figure
combined_plot <- grid.arrange(
  multiplot_1,
  multiplot_2,
  nrow = 2
)


