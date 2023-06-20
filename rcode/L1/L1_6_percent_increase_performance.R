#Title: Percent increase in CBI and significance tests

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code evaluates the percent increase in performance between non-geodiversity models and geodiversity models. It also performs Mann-Witney tests on averaged model performance across each spatial grain.

#Data input: Model performance values csv file (Supplemental S4; stats_mean.csv) and 

#Data output: Table representing the percent increase between non-geodiversity and geodiversity models and significance values of Mann-Witney tests.

#Author: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills


#load model performance stats across all species (S4)
perf_stats <- read.csv("INSERT FILE PATH")


## Percent Increase in model performance
#highest performing models that are not 1km
highest_cbi_per_species_geo <- perf_stats %>%
  filter(radiuskm != 1) %>%
  group_by(species) %>%
  slice_max(order_by = cbi.train, n = 1)

highest_cbi_per_species_geo$auc.train <- NULL

#change column name
highest_cbi_per_species_geo <- highest_cbi_per_species_geo %>%
  rename(cbi_train_geodiv = cbi.train)
highest_cbi_per_species_geo <- highest_cbi_per_species_geo %>%
  rename(optimal_geodiv_grain = radiuskm)

#value of no geodiversity 
highest_cbi_per_species_no_geo <- perf_stats %>%
  filter(radiuskm == 1) %>%
  group_by(species) %>%
  slice_max(order_by = cbi.train, n = 1)

#remove columns you don't want
highest_cbi_per_species_no_geo$auc.train <- NULL
highest_cbi_per_species_no_geo$radiuskm <- NULL

#change column name
highest_cbi_per_species_no_geo <- highest_cbi_per_species_no_geo %>%
  rename(cbi_train_no_geodiv = cbi.train)

#merge two dataframes
cbi_dataframe <- merge (highest_cbi_per_species_no_geo, highest_cbi_per_species_geo, by="species")


#calculate percent increase
perc_increase_cbi <- perf_stats %>%
  filter(radiuskm != 1) %>%
  group_by(species) %>%
  slice_max(order_by = cbi.train, n = 1) %>%
  left_join(perf_stats %>%
              filter(radiuskm == 1) %>%
              select(species, cbi.train),
            by = "species") %>%
  mutate(cbi_perc_increase = mean((cbi.train.x - cbi.train.y) / cbi.train.y) * 100) %>%
  select(species, cbi_perc_increase)

#Merge CBI dataframe and percent increase dataframe by species name
perc_increase_cbi_full <- merge(cbi_dataframe, perc_increase_cbi, by="species")

#remove underscore in species name
perc_increase_cbi_full$species <- gsub("_", " ", perc_increase_cbi_full$species) # remove underscore from species names

#set working directory to where the data should be saved
setwd("INSERT FILE PATH")
write.csv(perc_increase_cbi_full, "percent_increase_cbi.csv")

# check average percent increase across all optimal runs 
mean_perc_increase <- mean(perc_increase_cbi_full$cbi_perc_increase) # 16% increase in CBI

## Significance test 
library(dplyr)

#Set reference spatial grain (1km)
reference_radius <- 1

#Set comparison spatial grain
comparison_radii <- c(3, 9, 15, 21, 27, 33)


#Geodiversity Mann-Witney tests

#set p value
p_values <- matrix(ncol = length(comparison_radii), nrow = length(comparison_radii))

#parameterize resulting matrix 
is_significant <- matrix(ncol = length(comparison_radii), nrow = length(comparison_radii))
row_names <- col_names <- comparison_radii

#Run significance test loop
for (i in 1:length(comparison_radii)) {
  for (j in 1:length(comparison_radii)) {
    if (i != j) {
      reference_mean_cbi <- perf_stats$cbi.train[perf_stats$radiuskm == comparison_radii[i]]
      comparison_mean_cbi <- perf_stats$cbi.train[perf_stats$radiuskm == comparison_radii[j]]
      test_result <- wilcox.test(perf_stats$cbi.train[perf_stats$radiuskm == comparison_radii[i]],
     perf_stats$cbi.train[perf_stats$radiuskm == comparison_radii[j]])
      
      p_values[i, j] <- test_result$p.value
      is_significant[i, j] <- test_result$p.value < 0.05
    }
  }
}

# Assign row and column names to the matrix
rownames(p_values) <- row_names
colnames(p_values) <- col_names
rownames(is_significant) <- row_names
colnames(is_significant) <- col_names

p_values
is_significant


