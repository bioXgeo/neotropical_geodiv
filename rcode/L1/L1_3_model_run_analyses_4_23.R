#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code calculates statistics for 10 species model sets testing geodiversity predictors with variability calculated over 3, 9, 15, 21, 27 and 33 km. Outputs are: 
#1) table of permutation importance for each species for each radii-- top 6 predictors
#2) Average permutation importance for each variable category
#3) AUC/BIC/AIC tables and figures for each species
#4) In the end, a summary of average permutation importance for each radii over data poor and data rich species (which are the top performing categories)

#Authors: Beth E. Gerstner

library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(purrr)
library(tidyr)


#load permutation importance dataset across all species
perm_imp <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/imp_mean.csv")
#load performance stats across all species
perf_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/stats_mean.csv")

perf_stats <- perf_stats %>%
  filter(species != "Tapirus_pinchaque" & species != "Saimiri_sciurius")

## Permutation importance statistics


# Find top 7 most important variables for each radii/species
get_top_variables <- function(data, species, radius, n_top) {
  data %>%
    filter(species == {{species}} & radiuskm == {{radius}}) %>%
    arrange(desc(permutation.importance)) %>%
    slice(1:n_top) %>%
    dplyr::select(variable, radiuskm, permutation.importance)
}

# Create data frame of top variables for each species and radius
library(dplyr)
top_vars <- perm_imp %>%
  group_by(species, radiuskm) %>%
  do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 7)) %>%
  ungroup() %>%
  dplyr::select(species,variable, radiuskm, permutation.importance)

# View the resulting data frame
top_vars

#How many times does each variable get used in each radii model? 
#Count the number of times each variable is used across all species for each radii
variable_counts <- top_vars %>%
  group_by(radiuskm, variable) %>%
  summarise(n_species = n_distinct(species))

# View the resulting data frame
variable_counts <- variable_counts %>% 
         mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
         mutate(variable = case_when(
           str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
           TRUE ~ variable))

variable_counts$radiuskm <-as.factor(variable_counts$radiuskm)
#Barplot of counts - test
# Create stacked bar chart of variable counts by radii
variable_count_stacked <-ggplot(variable_counts, aes(x = radiuskm, y = n_species, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Radius (km)", y = "variable count", fill = "Variable") +
  theme_classic()

#group variables different to summarize how often geodiversity variables show up, not necessary the scale they are used

##heat map showing something similar
# Create a color scale
ggplot(data = variable_counts, aes(x = as.factor(radiuskm), y = variable, fill = n_species)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("lightgreen", "darkgreen"))(10), na.value = "white") +
  theme_bw() +
  labs(x = "Radius (km)", y = "Variable", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Average permutation importance calculated over all species per radii (averaged over every species)
avg_imp <- perm_imp %>%
  group_by(variable, radiuskm) %>%
  summarize(avg_importance = mean(permutation.importance, na.rm = TRUE))

#Change the variable names to remove the radii
avg_imp <- perm_imp %>%
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable
  )) %>%
  group_by(variable, radiuskm) %>%
  summarize(avg_importance = mean(permutation.importance, na.rm = TRUE))
avg_imp_arranged <-arrange(avg_imp, radiuskm, avg_importance)

#create a dataframe of average permutation importance and counts per radii 
perm_counts <-merge(avg_imp, variable_counts, by = c("variable", "radiuskm"))
perm_counts <- arrange(perm_counts, variable, radiuskm)

library(tidyr)
perm_counts_no_count <- perm_counts
perm_counts_no_count$n_species <- NULL
transformed_df <- perm_counts_no_count %>%
  spread(key = radiuskm, value = avg_importance)

#round to two decimal places and remove radii 1
transformed_df <- transformed_df %>%
  distinct(variable, .keep_all = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  select(-`1`)

write.csv (transformed_df, "variable_importance_table.csv")

#average permutation importance for geodiversity and non-geodiversity
# Average permutation importance over all radii for variables ending in "_sq"
avg_imp_sq <- perm_imp %>%
  filter(str_detect(variable, "_sq*")) %>%
  summarize(avg_importance = mean(permutation.importance, na.rm = TRUE))

# Average permutation importance over all radii for variables not ending in "_sq"
avg_imp_not_sq <- perm_imp %>%
  filter(!str_detect(variable, "_sq*")) %>%
  summarize(avg_importance = mean(permutation.importance, na.rm = TRUE))


#average permutation importance over all radii
avg_imp_over_radii <- perm_imp %>%
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable
  )) %>%
  group_by(variable) %>%
  summarize(avg_importance = mean(permutation.importance, na.rm = TRUE))
avg_imp_over_radii_arranged <-arrange(avg_imp_over_radii, avg_importance)

avg_imp_df <-as.data.frame(avg_imp)
avg_imp_df$radiuskm <- as.factor(avg_imp_df$radiuskm)

#plot avg PI across species
avg_perm_imp <-ggplot(data= avg_imp_df , aes(x=radiuskm, y=avg_importance, group=variable, col=variable)) + 
  geom_point() +
  geom_line() +
  ggtitle("Average permutation importance: all species")+
  ylab("PI") + xlab("radii (km)") + guides(col=guide_legend("variable"))

#peaks
# group permutation importance by variable and find top two peaks for each radius
top_peaks <- avg_imp_df %>%
  filter(radiuskm != 1) %>%
  group_by(variable) %>%
  nest() %>%
  mutate(peaks = map(data, ~{
    df <- .x
    df %>%
      arrange(desc(avg_importance)) %>%
      head(2)
  })) %>%
  unnest(peaks) %>%
  ungroup() %>%
  select(variable, radiuskm, avg_importance)

# plot avg permutation importance with top two peaks annotated
avg_perm_imp <- ggplot(data = avg_imp_df, aes(x = radiuskm, y = avg_importance, group = variable, col = variable)) + 
  geom_point() +
  geom_line() +
  ggtitle("Average permutation importance: all species") +
  ylab("PI") +
  xlab("radii (km)") +
  guides(col = guide_legend("variable")) +
  annotate("point", x = top_peaks$radiuskm, y = top_peaks$avg_importance, color = "red", fill = "red", size=3, shape=1)

avg_perm_imp

# For how many species did geodiversity variables make it into the top 3 in all the models? 
top_vars_3 <- perm_imp %>%
  group_by(species, radiuskm) %>%
  do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 3)) %>%
  ungroup() %>%
  dplyr::select(species,variable, radiuskm, permutation.importance)

top_vars_3_edit <- top_vars_3 %>% 
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable))


# Filter the original dataframe for species with "_sq" in the variable column
subset_df <- top_vars_3_edit %>%
  filter(str_detect(variable, "_sq")) %>%
  distinct(species, .keep_all = TRUE)

# Print the subset dataframe
print(subset_df)

# Count the number of species
num_species <- nrow(subset_df) #26 species have sq in top 3

# Extract species names from the subset dataframe
species_names <- subset_df$species

# Filter the original dataframe based on species names
filtered_df <- top_vars_3_edit %>%
  filter(species %in% species_names)

# Print the filtered dataframe
print(filtered_df)

# For how many species did geodiversity variables make it into the top 3 in optimal models? 

# Get the unique species names and optimal radiuskm

# optimal models
unique_radiuskm <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/percent_increase_cbi_auto.csv")
# Subset the permutation importance dataframe based on optimal radiuskm
subset_df_optimal <- perm_imp %>%
  inner_join(unique_radiuskm, by = c("species" = "species", "radiuskm" = "optimal_geodiv_grain"))

top_vars_3_optimal <- subset_df_optimal %>%
  group_by(species, radiuskm) %>%
  do(get_top_variables(data = ., species = .$species[1], radius = .$radiuskm[1], n_top = 3)) %>%
  ungroup() %>%
  dplyr::select(species,variable, radiuskm, permutation.importance)

top_vars_3_edit_optimal <- top_vars_3_optimal %>% 
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable))

# Filter the original dataframe for species with "_sq" in the variable column
subset_optimal <- top_vars_3_edit_optimal %>%
  filter(str_detect(variable, "_sq")) %>%
  distinct(species, .keep_all = TRUE) #23 species have geodiversity in top 3

average_permutation_importance_optimal <- top_vars_3_edit_optimal %>%
  filter(str_detect(variable, "_sq$")) %>%
  summarize(average_importance = mean(permutation.importance))



# Permutation importance for each species
# Loop through unique species and radii

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots/permutation_importance_species")

spec_imp <- perm_imp %>%
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable))

# Loop over unique species
for (s in unique(spec_imp$species)) {
  
  # Loop over unique radii
  for (r in unique(spec_imp$radiuskm)) {
    
    # Filter the data for this species and radius
    plot_data <- spec_imp %>% 
      filter(species == s, radiuskm == r)
    
    # Create the plot
    plot_title <- paste("Permutation Importance for", gsub("_", " ", s), "at Radius", r, "km")
    plot <- ggplot(data = plot_data, aes(x = variable, y = permutation.importance, fill = variable)) +
      geom_bar(stat = "identity") +
      ggtitle(plot_title) +
      ylab("PI") + xlab("Variable") +
      guides(fill = guide_legend("Variable"))
    
    # Create pdf file for this species/radius combination
    pdf(paste0(s, "_", r, "_permutation_importance_plot.pdf"), onefile = TRUE)
    
    # Print the plot
    print(plot)
    
    # Close the pdf file
    dev.off()
  }
}

#average permutation importance for geodiversity and non geodiversity variables

# Subset data for variables ending in _sq
perm_imp_sq <- spec_imp %>%
  filter(str_detect(variable, "_sq$"))

# Calculate mean permutation importance for variables ending in _sq
mean_sq <- perm_imp_sq  %>%
  #group_by(radiuskm)
  summarise(mean_perm_imp_sq = mean(permutation.importance))

# Subset data for variables not ending in _sq
perm_imp_no_sq <- spec_imp %>%
  filter(!str_detect(variable, "_sq$"))

# Calculate mean permutation importance for variables not ending in _sq
mean_no_sq <- perm_imp_no_sq %>%
  #group_by(radiuskm)
  summarise(mean_perm_imp_no_sq = mean(permutation.importance))

# Merge the two data frames
#var_imp_means <- full_join(mean_sq, mean_no_sq, by = "variable")

# Violin plot of permutation importance put on the log scale


# Loop over unique radii
# Group permutation importance by variable and radius and calculate mean importance
var_imp_means <- perm_imp %>% 
  mutate(variable = str_extract(variable, "^\\w+(?![\\d.])")) %>%
  mutate(variable = case_when(
    str_detect(variable, "_sq_\\d+") ~ str_replace(variable, "_sq_\\d+", "_sq"),
    TRUE ~ variable)) %>%
  group_by(radiuskm, variable) %>%
# mutate(permutation.importance = log10(permutation.importance)) %>%
  mutate(variable = reorder(variable, -str_detect(variable, "_sq")))


var_imp_means_geo <-var_imp_means[!var_imp_means$radiuskm==1,]

# Create a list of plots for each unique radius
plot_list <- lapply(unique(var_imp_means_geo$radiuskm), function(r) {
  cat("\nCreating plot for radius:", r)
  
  # Subset the data for this radius
  radius_data <- var_imp_means %>% filter(radiuskm == r)
  
  radius_data <- radius_data %>%
    mutate(color = ifelse(str_detect(variable, "_sq$"), "1", "2"))
  
  
  # Create the plot
  p <- ggplot(radius_data, aes(x = variable, y = permutation.importance, fill = color)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.1, fill = "white") +
    labs(x = "Variable", y = "Permutation importance", title = paste( r, "km", "grain")) +
    theme_bw() +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = .9), axis.title.x = element_text(margin = margin(t = 10))) +  coord_cartesian(ylim = c(0, 80))
  
  return(p)
})


# Combine the plots into a multiplot with 6 panels
library (cowplot)
multiplot <- plot_grid(plotlist = plot_list, ncol = 3 )

# Save the plot to a file
ggsave("avg_perm_imp_multi.png", plot = multiplot, width = 12, height = 8, dpi = 300)
  

#PERFORMANCE METRICS

# Create a data frame for the AUC,CBI and AICc data
radius_data <- data.frame(radiuskm = unique(perf_stats$radiuskm))

performance_stats_auc <-perf_stats %>%
  #group_by(radiuskm) %>%
  filter(radiuskm != 1) %>%
  summarise(mean_value = mean(auc.train),
            median_value = median(auc.train),
            range_value = max(auc.train) - min(auc.train),
            min_value = min(auc.train),
            max_value = max(auc.train))

performance_stats_cbi <-perf_stats %>%
  filter(radiuskm != 1) %>%
  #group_by(radiuskm) %>%
  summarise(mean_value = mean(cbi.train),
            median_value = median(cbi.train),
            range_value = max(auc.train) - min(cbi.train),
            min_value = min(cbi.train),
            max_value = max(cbi.train))


sd = sqrt((sum((perf_stats$auc.train - mean(perf_stats$auc.train))^2))/(231-1))

# Add AUC, CBI, and AIC means to the data frame for each radius
radius_data$auc_mean <- tapply(perf_stats$auc.train, perf_stats$radiuskm, mean)
radius_data$cbi_mean <- tapply(perf_stats$cbi.train, perf_stats$radiuskm, mean)
radius_data$aicc_mean <- tapply(perf_stats$AICc, perf_stats$radiuskm, mean)


# Create the plot
radius_data$radiuskm <- as.numeric(radius_data$radiuskm)
AUC_CBI_avg <-ggplot(radius_data, aes(x = radiuskm)) +
  geom_line(aes(y = auc_mean, color = "AUC"), size=1) +
  geom_line(aes(y = cbi_mean, color = "CBI"),size=1, na.rm = TRUE) +
  geom_point(aes(y = auc_mean, color = "AUC"), size = 1) +
  geom_point(aes(y = cbi_mean, color = "CBI"), na.rm = TRUE, size = 1) +
  labs(x = "Radius (km)", y = "Performance", color="Performance Metric") +
  scale_color_manual(values = c("purple", "#009E73")) +
  scale_x_continuous(breaks = c(1,3,9,15,21,27,33)) +
  theme_classic()

# Create the plot
AIC_avg <-ggplot(radius_data, aes(x = radiuskm)) +
  geom_line(aes(y = aicc_mean, color = "AICc"), size=1) +
  geom_point(aes(y = aicc_mean, color = "AICc"), size = 1) +
  labs(x = "Radius (km)", y = "Performance", color="Performance Metric") +
  scale_color_manual(values = c("blue")) +
  scale_x_continuous(breaks = c(1,3,9,15,21,27,33)) +
  theme_classic()

AIC_avg <- AIC_avg + scale_y_reverse()

#Create multiplot of the average performance metrics
# Create the multiplot
avg_perf_multiplot <- plot_grid(AUC_CBI_avg, AIC_avg, ncol=2)

ggdraw() +
  draw_label("Average Model Performance", fontface = "bold", size = 14, x = 0.5, y = 0.97) +
  avg_perf_multiplot

# Display the multiplot
multiplot

#count number of times per radii permutation importance is above zero (how many variables included in model on average)

# Group by species and radii and count number of rows with non-zero permutation importance
counts <- perm_imp %>%
  group_by(species, radiuskm) %>%
  summarise(count = sum(permutation.importance > 2)) #2.5

# Calculate the average count over all species
mean_count <- mean(counts$count)

#sum the top 7 variables for each species and average them to see how much variation the top 7 soak up generally

#species tests
#olinguito
# filter data for species of interest
neblina_data <- filter(perf_stats, species == "Tremarctos_ornatus")

# calculate average performance by radius for AUC and CBI
neblina_radius_data <- neblina_data %>%
  group_by(radiuskm) %>%
  summarize(auc_mean = mean(auc.train, na.rm = TRUE),
            cbi_mean = mean(cbi.train, na.rm = TRUE))

# create the plot
ggplot(neblina_radius_data, aes(x = radiuskm)) +
  geom_line(aes(y = auc_mean, color = "AUC"), size = 1) +
  geom_line(aes(y = cbi_mean, color = "CBI"), size = 1) +
  geom_point(aes(y = auc_mean, color = "AUC"), size = 3) +
  geom_point(aes(y = cbi_mean, color = "CBI"), size = 3) +
  labs(x = "Radius (km)", y = "Performance") +
  scale_color_manual(name = "Metric", values = c("purple", "#009E73")) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.8))

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots")
# Get unique species names
species_names <- unique(perf_stats$species)

# Loop through  all species and create performance graphs
for (s in species_names) {
  
  # Filter data for current species
  species_data <- perf_stats[perf_stats$species == s,]
  
  # Calculate means for AUC and CBI
  auc_mean <- mean(species_data$auc.train, na.rm = TRUE)
  cbi_mean <- mean(species_data$cbi.train, na.rm = TRUE)
  
  # Create plot
  p <- ggplot(species_data, aes(x = radiuskm)) +
    geom_line(aes(y = auc.train, color = "AUC")) +
    geom_line(aes(y = cbi.train, color = "CBI"), na.rm = TRUE) +
    labs(title = paste("Performance for", s),
         x = "Radius (km)",
         y = "Performance") +
    scale_color_manual(values = c("purple", "#009E73"),
                       name = "Metric") +
    theme_classic() +
    theme(legend.position = "right") +
    geom_point(aes(y = auc.train), color = "purple", size = 2) +
    geom_point(aes(y = cbi.train), color = "#009E73", size = 2)
  
  # Save plot as pperf_stats
  ggsave(paste0(s, "_performance_stats.png"), p, width = 8, height = 6, device="png")
}

#combined plots
# Get unique species names
species_names <- unique(perf_stats$species)

# Set up plotting parameters
plot_width <- 10
plot_height <- 8
plot_dpi <- 400

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/performance_plots")
# Loop through species
for (s in species_names) {
  
  # Filter data for current species
  species_data <- perf_stats[perf_stats$species == s,]
  
  # Calculate means for AUC, CBI, and AIC
  auc_mean <- mean(species_data$auc.train, na.rm = TRUE)
  cbi_mean <- mean(species_data$cbi.train, na.rm = TRUE)
  #aic_mean <- mean(species_data$AICc, na.rm = TRUE)
  
  # Create AUC/CBI plot
  p1 <- ggplot(species_data, aes(x = radiuskm)) +
    geom_line(aes(y = auc.train, color = "AUC")) +
    geom_line(aes(y = cbi.train, color = "CBI"), na.rm = TRUE) +
    labs(title = gsub("_", " ", paste("CBI/AUC performance for", s)),
         x = "Radius (km)",
         y = "Performance") +
    scale_color_manual(values = c("purple", "#009E73"),
                       name = "Metric") +
    theme_classic() +
    theme(legend.position = "right") +
    geom_point(aes(y = auc.train), color = "purple", size = 2) +
    geom_point(aes(y = cbi.train), color = "#009E73", size = 2) +
    scale_x_continuous(breaks = unique(species_data$radiuskm)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  # Create AIC plot
  #p2 <- ggplot(species_data, aes(x = radiuskm)) +
  #  geom_line(aes(y = AICc), color = "skyblue") +
  #  geom_point(aes(y = AICc), color = "skyblue", size = 2) +
    #labs(title = gsub("_", " ", paste("AIC Performance for", s)),
       #  x = "Radius (km)",
       #  y = "AIC") +
  #  theme_classic() +
  #  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  #  scale_x_continuous(breaks = unique(species_data$radiuskm))
  
  # Combine plots
  p_combined <- cowplot::plot_grid(p1, ncol = 1, align = "h", axis = "tb", rel_widths= c(1,1))
  
  # Save plot as png file
  ggsave(paste0(s, "_performance_stats.png"), p_combined, width = plot_width, height = plot_height, dpi = plot_dpi, device = "png")
}

# Create Boxplot of model performance
# Read in your data as a data frame, where each row represents one observation
# and contains columns for species, radii, and boyce index

# Convert radii to a factor variable for better plotting
perf_stats$radiuskm<- as.factor(perf_stats$radiuskm)

# Create the box plot using ggplot2
CBI_plot <-ggplot(perf_stats, aes(x = radiuskm, y = cbi.train, fill = radiuskm)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(x = "Spatial Grain", y = "Continuous Boyce Index", title="Average Performance: CBI") +
  ylim(.4,1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

AUC_plot <-ggplot(perf_stats, aes(x = radiuskm, y = auc.train, fill = radiuskm)) +
 geom_boxplot() +
 stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
 labs(x = "Spatial Grain", y = "AUC", title="Average Performance: AUC") +
 ylim(.4,1) +
 theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#AIC_plot <- ggplot(perf_stats, aes(x = radiuskm, y = AICc, fill = radiuskm)) +
  #geom_boxplot() +
  #stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black") +
  #labs(x = "Radii", y = "AICc", title="Average Performance: AICc") +
  #ylim(0,4000) +
 # theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Combine the two plots into a two-panel plot
gridExtra::grid.arrange(AUC_plot, CBI_plot, ncol = 2)

ggsave("CBI_plot.png", CBI_plot, width = 8, height = 6, dpi = 300)

## Species groupings
spec_subset <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/Chapter_2_species_traits.csv")


# Pre-process the data
species_traits <- spec_subset %>% select(IUCN_species_name, body_mass_e, diet_cat_specific) %>% 
  mutate(body_mass_e = as.numeric(body_mass_e))

# Create categories based on quartiles
species_groups <- species_traits %>% 
  mutate(body_mass_category = cut(body_mass_e, quantile(body_mass_e, probs = seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4")))

# Filter the performance metrics for each species group
mass1 <- species_groups %>% filter(body_mass_category == "Q1")
mass2 <- species_groups %>% filter(body_mass_category == "Q2")
mass3 <- species_groups %>% filter(body_mass_category == "Q3")
mass4 <- species_groups %>% filter(body_mass_category == "Q4")

perf_stats$species <- gsub("_", " ", perf_stats$species) # remove underscore from species names
mass3_perf <- perf_stats %>% 
  filter(species %in% mass3$IUCN_species_name)

mass3_perf$radiuskm <- as.factor(mass3_perf$radiuskm)
AUC_plot_massQ1 <- ggplot(mass3_perf, aes(x = radiuskm, y = cbi.train, group = species, color = species)) +
  geom_line() +
  geom_point(aes(y = cbi.train, color = species), size = 1)+
  labs(x = "Radii", y = "AUC", title = "Performance: Mass Q1") +
  ylim(.4,1) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


AUC_CBI_avg <-ggplot(radius_data, aes(x = radiuskm)) +
  geom_line(aes(y = auc_mean, color = "AUC"), size=1) +
  geom_line(aes(y = cbi_mean, color = "CBI"),size=1, na.rm = TRUE) +
  geom_point(aes(y = auc_mean, color = "AUC"), size = 1) +
  geom_point(aes(y = cbi_mean, color = "CBI"), na.rm = TRUE, size = 1) +
  labs(x = "Radius (km)", y = "Performance", color="Performance Metric") +
  scale_color_manual(values = c("purple", "#009E73")) +
  scale_x_continuous(breaks = c(1,3,9,15,21,27,33)) +
  theme_classic()
           