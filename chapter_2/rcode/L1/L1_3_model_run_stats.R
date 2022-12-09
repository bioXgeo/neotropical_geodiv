
#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code calculates statistics for 10 species model sets testing geodiversity predictors with variability calculated over 3, 9, 15, 21, 27 and 33 km. Outputs are: 
#1) table of permutation importance for each species for each radii-- top 6 predictors
#2) Average permutation importance for each variable category
#3) AUC & BIC tables and figures for each species
#4) In the end, a summary of average permutation importance for each radii over data poor and data rich species (which are the top performing categories)

#Authors: Beth E. Gerstner

#Date: 1/21/22

#Load library
library(dplyr)
library(stringr)
library(reshape2)

#Species 1
# Aotus brumbacki

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
a_brumbacki.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^a_brumbacki_permutation")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_brumbacki.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
a_brumbacki_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
a_brumbacki_pi_df <-as.data.frame(bind_rows(a_brumbacki_perm_importance, .id = "column_label"))

# remove filename column
a_brumbacki_pi_df$filename <-NULL

# add species name to datafrrame
a_brumbacki_pi_df$column_label <- "Aotus brumbacki"
colnames(a_brumbacki_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
a_brumbacki_top_6 <- a_brumbacki_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#a_brumbacki_var_count <-a_brumbacki_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_ab <-a_brumbacki_top_6 %>% mutate(group = 
                                                  case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                                      variable == "sd_3km" ~ "sd",
                                                                      variable == "bio5_sd_3km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_3km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_3km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_3km" ~ "bio14_sd",
                                                                      variable == "cloud_3km" ~ "cloud_sd",
                                                                      variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                                      variable == "sd_9km" ~ "sd",
                                                                      variable == "bio5_sd_9km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_9km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_9km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_9km" ~ "bio14_sd",
                                                                      variable == "cloud_9km" ~ "cloud_sd",
                                                                      variable == "sd_15km" ~ "sd",
                                                                      variable == "bio5_sd_15km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_15km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_15km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_15km" ~ "bio14_sd",
                                                                      variable == "cloud_15km" ~ "cloud_sd",
                                                                      variable == "sd_21km" ~ "sd",
                                                                      variable == "bio5_sd_21km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_21km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_21km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_21km" ~ "bio14_sd",
                                                                      variable == "cloud_21km" ~ "cloud_sd",
                                                                      variable == "sd_27km" ~ "sd",
                                                                      variable == "bio5_sd_27km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_27km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_27km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_27km" ~ "bio14_sd",
                                                                      variable == "cloud_27km" ~ "cloud_sd",
                                                                      variable == "sd_33km" ~ "sd",
                                                                      variable == "bio5_sd_33km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_33km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_33km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_33km" ~ "bio14_sd",
                                                                      variable == "cloud_33km" ~ "cloud_sd",
                                                                      variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                                      variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                                      variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                                      variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                                      variable == "srtm_crop" ~ "srtm_crop",
                                                                      variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_ab$variable_used <- new_var_names_ab$group
new_var_names_ab$group <- NULL
new_var_names_ab <- as.data.frame(new_var_names_ab)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
ab_count <-new_var_names_ab %>% count(variable_used, sort=T)
write.csv(ab_count,"a_brumbacki_variable_count.csv")

#change it to numeric so we can sort
new_var_names_ab$radii <- as.numeric(new_var_names_ab$radii)
new_var_names_ab<- arrange(new_var_names_ab, radii)

#turn the radii column back to factor so we can plot
new_var_names_ab$radii <- as.factor(new_var_names_ab$radii)

write.csv(new_var_names_ab,"a_brumbacki_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Ab_perm_imp <-new_var_names_ab %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. brumbacki")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_ab <-new_var_names_ab %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_ab, "avg_perm_per_var_imp_ab.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_ab$variable, "km")
new_var_names_ab$agg_column <- bing
perm_radii.ab <- dcast(new_var_names_ab, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.ab, "perm_importance_cleaned_ab.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
a_brumbacki.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^a_brumbacki")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_brumbacki.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
a_brumbacki_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
a_brumbacki_perf_df <-as.data.frame(bind_rows(a_brumbacki_performance, .id = "column_label"))

#remove filename column
a_brumbacki_perf_df$filename <-NULL

#add species name
a_brumbacki_perf_df$column_label <- "Aotus brumbacki"
colnames(a_brumbacki_perf_df)[1] <- c("scientific_name")

#change order of radii
a_brumbacki_perf_df$auc.train<- as.numeric(a_brumbacki_perf_df$auc.train)
a_brumbacki_perf_df$radii <- as.numeric(a_brumbacki_perf_df$radii)
a_brumbacki_perf_df <- arrange(a_brumbacki_perf_df, radii)
a_brumbacki_perf_df$radii<- as.factor(a_brumbacki_perf_df$radii)

Ab_perf_stat_auc <-
  ggplot(data=a_brumbacki_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("Model Performance: AUC for A. brumbacki")+
  ylab("AUC") + xlab("radii (km)")

Ab_perf_stat_cbi <-
  ggplot(data=a_brumbacki_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("Model Performance: CBI for A. brumbacki")+
  ylab("CBI") + xlab("radii (km)")

#_______
#Species 2
#Bassaricyon neblina

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
b_neblina.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^b_neblina")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(b_neblina.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
b_neblina_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
b_neblina_pi_df <-as.data.frame(bind_rows(b_neblina_perm_importance, .id = "column_label"))

# remove filename column
b_neblina_pi_df$filename <-NULL

# add species name to datafrrame
b_neblina_pi_df$column_label <- "Bassaricyon neblina"
colnames(b_neblina_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
b_neblina_top_6 <- b_neblina_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#b_neblina_var_count <-b_neblina_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Bn <-b_neblina_top_6 %>% mutate(group = 
                                                case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                          variable == "sd_3km" ~ "sd",
                                                          variable == "bio5_sd_3km" ~ "bio5_sd",
                                                          variable == "bio6_sd_3km" ~ "bio6_sd",
                                                          variable == "bio13_sd_3km" ~ "bio13_sd",
                                                          variable == "bio14_sd_3km" ~ "bio14_sd",
                                                          variable == "cloud_3km" ~ "cloud_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                          variable == "sd_9km" ~ "sd",
                                                          variable == "bio5_sd_9km" ~ "bio5_sd",
                                                          variable == "bio6_sd_9km" ~ "bio6_sd",
                                                          variable == "bio13_sd_9km" ~ "bio13_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd",
                                                          variable == "cloud_9km" ~ "cloud_sd",
                                                          variable == "sd_15km" ~ "sd",
                                                          variable == "bio5_sd_15km" ~ "bio5_sd",
                                                          variable == "bio6_sd_15km" ~ "bio6_sd",
                                                          variable == "bio13_sd_15km" ~ "bio13_sd",
                                                          variable == "bio14_sd_15km" ~ "bio14_sd",
                                                          variable == "cloud_15km" ~ "cloud_sd",
                                                          variable == "sd_21km" ~ "sd",
                                                          variable == "bio5_sd_21km" ~ "bio5_sd",
                                                          variable == "bio6_sd_21km" ~ "bio6_sd",
                                                          variable == "bio13_sd_21km" ~ "bio13_sd",
                                                          variable == "bio14_sd_21km" ~ "bio14_sd",
                                                          variable == "cloud_21km" ~ "cloud_sd",
                                                          variable == "sd_27km" ~ "sd",
                                                          variable == "bio5_sd_27km" ~ "bio5_sd",
                                                          variable == "bio6_sd_27km" ~ "bio6_sd",
                                                          variable == "bio13_sd_27km" ~ "bio13_sd",
                                                          variable == "bio14_sd_27km" ~ "bio14_sd",
                                                          variable == "cloud_27km" ~ "cloud_sd",
                                                          variable == "sd_33km" ~ "sd",
                                                          variable == "bio5_sd_33km" ~ "bio5_sd",
                                                          variable == "bio6_sd_33km" ~ "bio6_sd",
                                                          variable == "bio13_sd_33km" ~ "bio13_sd",
                                                          variable == "bio14_sd_33km" ~ "bio14_sd",
                                                          variable == "cloud_33km" ~ "cloud_sd",
                                                          variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                          variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                          variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                          variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                          variable == "srtm_crop" ~ "srtm_crop",
                                                          variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Bn$variable_used <- new_var_names_Bn$group
new_var_names_Bn$group <- NULL
new_var_names_Bn <- as.data.frame(new_var_names_Bn)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Bn_count <-new_var_names_Bn %>% count(variable_used, sort=T)
write.csv(Bn_count,"b_neblina_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Bn$radii <- as.numeric(new_var_names_Bn$radii)
new_var_names_Bn<- arrange(new_var_names_Bn, radii)

#turn the radii column back to factor so we can plot
new_var_names_Bn$radii <- as.factor(new_var_names_Bn$radii)

write.csv(new_var_names_Bn,"b_neblina_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Bn_perm_imp <-new_var_names_Bn %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance B. neblina")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Bn <-new_var_names_Bn %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Bn, "avg_perm_per_var_imp_Bn.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Bn$variable, "km")
new_var_names_Bn$agg_column <- bing
perm_radii.Bn <- dcast(new_var_names_Bn, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Bn, "perm_importance_cleaned_Bn.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
b_neblina.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^b_neblina")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(b_neblina.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
b_neblina_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
b_neblina_perf_df <-as.data.frame(bind_rows(b_neblina_performance, .id = "column_label"))

#remove filename column
b_neblina_perf_df$filename <-NULL

#add species name
b_neblina_perf_df$column_label <- "Bassaricyon neblina"
colnames(b_neblina_perf_df)[1] <- c("scientific_name")

#change order of radii
b_neblina_perf_df$auc.train<- as.numeric(b_neblina_perf_df$auc.train)
b_neblina_perf_df$radii <- as.numeric(b_neblina_perf_df$radii)
b_neblina_perf_df <- arrange(b_neblina_perf_df, radii)
b_neblina_perf_df$radii<- as.factor(b_neblina_perf_df$radii)


#Plot AUC figs
Bn_perf_stat_auc <-
  ggplot(data=b_neblina_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("AUC: B. neblina")+
  ylab("AUC") + xlab("radii (km)")

Bn_perf_stat_cbi <-
  ggplot(data=b_neblina_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("CBI: B. neblina")+
  ylab("CBI") + xlab("radii (km)")

#_____ 
#species 3
#Tremarctos ornatus

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
t_ornatus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^t_ornatus")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(t_ornatus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
t_ornatus_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
t_ornatus_pi_df <-as.data.frame(bind_rows(t_ornatus_perm_importance, .id = "column_label"))

# remove filename column
t_ornatus_pi_df$filename <-NULL

# add species name to datafrrame
t_ornatus_pi_df$column_label <- "Tremarctos ornatus"
colnames(t_ornatus_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
t_ornatus_top_6 <- t_ornatus_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#t_ornatus_var_count <-t_ornatus_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_To <-t_ornatus_top_6 %>% mutate(group = 
                                                case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                          variable == "sd_3km" ~ "sd",
                                                          variable == "bio5_sd_3km" ~ "bio5_sd",
                                                          variable == "bio6_sd_3km" ~ "bio6_sd",
                                                          variable == "bio13_sd_3km" ~ "bio13_sd",
                                                          variable == "bio14_sd_3km" ~ "bio14_sd",
                                                          variable == "cloud_3km" ~ "cloud_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                          variable == "sd_9km" ~ "sd",
                                                          variable == "bio5_sd_9km" ~ "bio5_sd",
                                                          variable == "bio6_sd_9km" ~ "bio6_sd",
                                                          variable == "bio13_sd_9km" ~ "bio13_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd",
                                                          variable == "cloud_9km" ~ "cloud_sd",
                                                          variable == "sd_15km" ~ "sd",
                                                          variable == "bio5_sd_15km" ~ "bio5_sd",
                                                          variable == "bio6_sd_15km" ~ "bio6_sd",
                                                          variable == "bio13_sd_15km" ~ "bio13_sd",
                                                          variable == "bio14_sd_15km" ~ "bio14_sd",
                                                          variable == "cloud_15km" ~ "cloud_sd",
                                                          variable == "sd_21km" ~ "sd",
                                                          variable == "bio5_sd_21km" ~ "bio5_sd",
                                                          variable == "bio6_sd_21km" ~ "bio6_sd",
                                                          variable == "bio13_sd_21km" ~ "bio13_sd",
                                                          variable == "bio14_sd_21km" ~ "bio14_sd",
                                                          variable == "cloud_21km" ~ "cloud_sd",
                                                          variable == "sd_27km" ~ "sd",
                                                          variable == "bio5_sd_27km" ~ "bio5_sd",
                                                          variable == "bio6_sd_27km" ~ "bio6_sd",
                                                          variable == "bio13_sd_27km" ~ "bio13_sd",
                                                          variable == "bio14_sd_27km" ~ "bio14_sd",
                                                          variable == "cloud_27km" ~ "cloud_sd",
                                                          variable == "sd_33km" ~ "sd",
                                                          variable == "bio5_sd_33km" ~ "bio5_sd",
                                                          variable == "bio6_sd_33km" ~ "bio6_sd",
                                                          variable == "bio13_sd_33km" ~ "bio13_sd",
                                                          variable == "bio14_sd_33km" ~ "bio14_sd",
                                                          variable == "cloud_33km" ~ "cloud_sd",
                                                          variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                          variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                          variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                          variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                          variable == "srtm_crop" ~ "srtm_crop",
                                                          variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_To$variable_used <- new_var_names_To$group
new_var_names_To$group <- NULL
new_var_names_To <- as.data.frame(new_var_names_To)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
To_count <-new_var_names_To %>% count(variable_used, sort=T)
write.csv(To_count,"t_ornatus_variable_count.csv")

#change it to numeric so we can sort
new_var_names_To$radii <- as.numeric(new_var_names_To$radii)
new_var_names_To<- arrange(new_var_names_To, radii)

#turn the radii column back to factor so we can plot
new_var_names_To$radii <- as.factor(new_var_names_To$radii)

write.csv(new_var_names_To,"t_ornatus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

To_perm_imp <-new_var_names_To %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance T. ornatus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_To <-new_var_names_To %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_To, "avg_perm_per_var_imp_To.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_To$variable, "km")
new_var_names_To$agg_column <- bing
perm_radii.To <- dcast(new_var_names_To, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.To, "perm_importance_cleaned_To.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
t_ornatus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^t_ornatus")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(t_ornatus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
t_ornatus_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
t_ornatus_perf_df <-as.data.frame(bind_rows(t_ornatus_performance, .id = "column_label"))

#remove filename column
t_ornatus_perf_df$filename <-NULL

#add species name
t_ornatus_perf_df$column_label <- "Tremarctos ornatus"
colnames(t_ornatus_perf_df)[1] <- c("scientific_name")

#change order of radii
t_ornatus_perf_df$auc.train<- as.numeric(t_ornatus_perf_df$auc.train)
t_ornatus_perf_df$radii <- as.numeric(t_ornatus_perf_df$radii)
t_ornatus_perf_df <- arrange(t_ornatus_perf_df, radii)
t_ornatus_perf_df$radii<- as.factor(t_ornatus_perf_df$radii)


#Plot AUC figs
To_perf_stat_auc <-
  ggplot(data=t_ornatus_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: T. ornatus")+
  ylab("AUC") + xlab("radii (km)")

To_perf_stat_cbi <-
  ggplot(data=t_ornatus_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: T. ornatus")+
  ylab("CBI") + xlab("radii (km)")

library(ggpubr)
ggarrange(To_perf_stat_auc, To_perf_stat_cbi, Bn_perf_stat_auc, Bn_perf_stat_cbi + rremove("x.text"), 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

#__________
#species 4
#Plecturocebus ornatus

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
P_ornatus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^p_ornatus")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(P_ornatus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
P_ornatus_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
P_ornatus_pi_df <-as.data.frame(bind_rows(P_ornatus_perm_importance, .id = "column_label"))

# remove filename column
P_ornatus_pi_df$filename <-NULL

# add species name to datafrrame
P_ornatus_pi_df$column_label <- "Plecturocebus ornatus"
colnames(P_ornatus_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
P_ornatus_top_6 <- P_ornatus_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#P_ornatus_var_count <-P_ornatus_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Po <-P_ornatus_top_6 %>% mutate(group = 
                                                case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                          variable == "sd_3km" ~ "sd",
                                                          variable == "bio5_sd_3km" ~ "bio5_sd",
                                                          variable == "bio6_sd_3km" ~ "bio6_sd",
                                                          variable == "bio13_sd_3km" ~ "bio13_sd",
                                                          variable == "bio14_sd_3km" ~ "bio14_sd",
                                                          variable == "cloud_3km" ~ "cloud_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                          variable == "sd_9km" ~ "sd",
                                                          variable == "bio5_sd_9km" ~ "bio5_sd",
                                                          variable == "bio6_sd_9km" ~ "bio6_sd",
                                                          variable == "bio13_sd_9km" ~ "bio13_sd",
                                                          variable == "bio14_sd_9km" ~ "bio14_sd",
                                                          variable == "cloud_9km" ~ "cloud_sd",
                                                          variable == "sd_15km" ~ "sd",
                                                          variable == "bio5_sd_15km" ~ "bio5_sd",
                                                          variable == "bio6_sd_15km" ~ "bio6_sd",
                                                          variable == "bio13_sd_15km" ~ "bio13_sd",
                                                          variable == "bio14_sd_15km" ~ "bio14_sd",
                                                          variable == "cloud_15km" ~ "cloud_sd",
                                                          variable == "sd_21km" ~ "sd",
                                                          variable == "bio5_sd_21km" ~ "bio5_sd",
                                                          variable == "bio6_sd_21km" ~ "bio6_sd",
                                                          variable == "bio13_sd_21km" ~ "bio13_sd",
                                                          variable == "bio14_sd_21km" ~ "bio14_sd",
                                                          variable == "cloud_21km" ~ "cloud_sd",
                                                          variable == "sd_27km" ~ "sd",
                                                          variable == "bio5_sd_27km" ~ "bio5_sd",
                                                          variable == "bio6_sd_27km" ~ "bio6_sd",
                                                          variable == "bio13_sd_27km" ~ "bio13_sd",
                                                          variable == "bio14_sd_27km" ~ "bio14_sd",
                                                          variable == "cloud_27km" ~ "cloud_sd",
                                                          variable == "sd_33km" ~ "sd",
                                                          variable == "bio5_sd_33km" ~ "bio5_sd",
                                                          variable == "bio6_sd_33km" ~ "bio6_sd",
                                                          variable == "bio13_sd_33km" ~ "bio13_sd",
                                                          variable == "bio14_sd_33km" ~ "bio14_sd",
                                                          variable == "cloud_33km" ~ "cloud_sd",
                                                          variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                          variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                          variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                          variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                          variable == "srtm_crop" ~ "srtm_crop",
                                                          variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Po$variable_used <- new_var_names_Po$group
new_var_names_Po$group <- NULL
new_var_names_Po <- as.data.frame(new_var_names_Po)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Po_count <-new_var_names_Po %>% count(variable_used, sort=T)
write.csv(Po_count,"P_ornatus_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Po$radii <- as.numeric(new_var_names_Po$radii)
new_var_names_Po<- arrange(new_var_names_Po, radii)

#turn the radii column back to factor so we can plot
new_var_names_Po$radii <- as.factor(new_var_names_Po$radii)

write.csv(new_var_names_Po,"P_ornatus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Po_perm_imp <-new_var_names_Po %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance P. ornatus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Po <-new_var_names_Po %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Po, "avg_perm_per_var_imp_Po.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Po$variable, "km")
new_var_names_Po$agg_column <- bing
perm_radii.Po <- dcast(new_var_names_Po, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Po, "perm_importance_cleaned_Po.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
P_ornatus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^p_ornatus")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(P_ornatus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
P_ornatus_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
P_ornatus_perf_df <-as.data.frame(bind_rows(P_ornatus_performance, .id = "column_label"))

#remove filename column
P_ornatus_perf_df$filename <-NULL

#add species name
P_ornatus_perf_df$column_label <- "Plecturocebus ornatus"
colnames(P_ornatus_perf_df)[1] <- c("scientific_name")

#change order of radii
P_ornatus_perf_df$auc.train<- as.numeric(P_ornatus_perf_df$auc.train)
P_ornatus_perf_df$radii <- as.numeric(P_ornatus_perf_df$radii)
P_ornatus_perf_df <- arrange(P_ornatus_perf_df, radii)
P_ornatus_perf_df$radii<- as.factor(P_ornatus_perf_df$radii)


#Plot AUC figs
Po_perf_stat_auc <-
  ggplot(data=P_ornatus_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: P. ornatus")+
  ylab("AUC") + xlab("radii (km)")

Po_perf_stat_cbi <-
  ggplot(data=P_ornatus_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: P. ornatus")+
  ylab("CBI") + xlab("radii (km)")

#______
#Species 5
#Ateles hybridus

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
a_hybridus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^a_hybridus")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_hybridus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
a_hybridus_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
a_hybridus_pi_df <-as.data.frame(bind_rows(a_hybridus_perm_importance, .id = "column_label"))

# remove filename column
a_hybridus_pi_df$filename <-NULL

# add species name to datafrrame
a_hybridus_pi_df$column_label <- "Ateles hybridus"
colnames(a_hybridus_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
a_hybridus_top_6 <- a_hybridus_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#a_hybridus_var_count <-a_hybridus_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Ah <-a_hybridus_top_6 %>% mutate(group = 
                                                 case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                           variable == "sd_3km" ~ "sd",
                                                           variable == "bio5_sd_3km" ~ "bio5_sd",
                                                           variable == "bio6_sd_3km" ~ "bio6_sd",
                                                           variable == "bio13_sd_3km" ~ "bio13_sd",
                                                           variable == "bio14_sd_3km" ~ "bio14_sd",
                                                           variable == "cloud_3km" ~ "cloud_sd",
                                                           variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                           variable == "sd_9km" ~ "sd",
                                                           variable == "bio5_sd_9km" ~ "bio5_sd",
                                                           variable == "bio6_sd_9km" ~ "bio6_sd",
                                                           variable == "bio13_sd_9km" ~ "bio13_sd",
                                                           variable == "bio14_sd_9km" ~ "bio14_sd",
                                                           variable == "cloud_9km" ~ "cloud_sd",
                                                           variable == "sd_15km" ~ "sd",
                                                           variable == "bio5_sd_15km" ~ "bio5_sd",
                                                           variable == "bio6_sd_15km" ~ "bio6_sd",
                                                           variable == "bio13_sd_15km" ~ "bio13_sd",
                                                           variable == "bio14_sd_15km" ~ "bio14_sd",
                                                           variable == "cloud_15km" ~ "cloud_sd",
                                                           variable == "sd_21km" ~ "sd",
                                                           variable == "bio5_sd_21km" ~ "bio5_sd",
                                                           variable == "bio6_sd_21km" ~ "bio6_sd",
                                                           variable == "bio13_sd_21km" ~ "bio13_sd",
                                                           variable == "bio14_sd_21km" ~ "bio14_sd",
                                                           variable == "cloud_21km" ~ "cloud_sd",
                                                           variable == "sd_27km" ~ "sd",
                                                           variable == "bio5_sd_27km" ~ "bio5_sd",
                                                           variable == "bio6_sd_27km" ~ "bio6_sd",
                                                           variable == "bio13_sd_27km" ~ "bio13_sd",
                                                           variable == "bio14_sd_27km" ~ "bio14_sd",
                                                           variable == "cloud_27km" ~ "cloud_sd",
                                                           variable == "sd_33km" ~ "sd",
                                                           variable == "bio5_sd_33km" ~ "bio5_sd",
                                                           variable == "bio6_sd_33km" ~ "bio6_sd",
                                                           variable == "bio13_sd_33km" ~ "bio13_sd",
                                                           variable == "bio14_sd_33km" ~ "bio14_sd",
                                                           variable == "cloud_33km" ~ "cloud_sd",
                                                           variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                           variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                           variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                           variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                           variable == "srtm_crop" ~ "srtm_crop",
                                                           variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Ah$variable_used <- new_var_names_Ah$group
new_var_names_Ah$group <- NULL
new_var_names_Ah <- as.data.frame(new_var_names_Ah)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Ah_count <-new_var_names_Ah %>% count(variable_used, sort=T)
write.csv(Ah_count,"a_hybridus_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Ah$radii <- as.numeric(new_var_names_Ah$radii)
new_var_names_Ah<- arrange(new_var_names_Ah, radii)

#turn the radii column back to factor so we can plot
new_var_names_Ah$radii <- as.factor(new_var_names_Ah$radii)

write.csv(new_var_names_Ah,"a_hybridus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Ah_perm_imp <-new_var_names_Ah %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. hybridus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Ah <-new_var_names_Ah %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Ah, "avg_perm_per_var_imp_Ah.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Ah$variable, "km")
new_var_names_Ah$agg_column <- bing
perm_radii.Ah <- dcast(new_var_names_Ah, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Ah, "perm_importance_cleaned_Ah.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
a_hybridus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^a_hybridus")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_hybridus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
a_hybridus_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
a_hybridus_perf_df <-as.data.frame(bind_rows(a_hybridus_performance, .id = "column_label"))

#remove filename column
a_hybridus_perf_df$filename <-NULL

#add species name
a_hybridus_perf_df$column_label <- "Ateles hybridus"
colnames(a_hybridus_perf_df)[1] <- c("scientific_name")

#change order of radii
a_hybridus_perf_df$auc.train<- as.numeric(a_hybridus_perf_df$auc.train)
a_hybridus_perf_df$radii <- as.numeric(a_hybridus_perf_df$radii)
a_hybridus_perf_df <- arrange(a_hybridus_perf_df, radii)
a_hybridus_perf_df$radii<- as.factor(a_hybridus_perf_df$radii)


#Plot AUC figs
Ah_perf_stat_auc <-
  ggplot(data=a_hybridus_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: A. hybridus")+
  ylab("AUC") + xlab("radii (km)")

Ah_perf_stat_cbi <-
  ggplot(data=a_hybridus_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: A. hybridus")+
  ylab("CBI") + xlab("radii (km)")

#_____
#species 6
#Aotus vociferans
# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
a_vociferans.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^a_vociferans")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_vociferans.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
a_vociferans_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
a_vociferans_pi_df <-as.data.frame(bind_rows(a_vociferans_perm_importance, .id = "column_label"))

# remove filename column
a_vociferans_pi_df$filename <-NULL

# add species name to datafrrame
a_vociferans_pi_df$column_label <- "Aotus vociferans"
colnames(a_vociferans_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
a_vociferans_top_6 <- a_vociferans_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#a_vociferans_var_count <-a_vociferans_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Av <-a_vociferans_top_6 %>% mutate(group = 
                                                   case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                             variable == "sd_3km" ~ "sd",
                                                             variable == "bio5_sd_3km" ~ "bio5_sd",
                                                             variable == "bio6_sd_3km" ~ "bio6_sd",
                                                             variable == "bio13_sd_3km" ~ "bio13_sd",
                                                             variable == "bio14_sd_3km" ~ "bio14_sd",
                                                             variable == "cloud_3km" ~ "cloud_sd",
                                                             variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                             variable == "sd_9km" ~ "sd",
                                                             variable == "bio5_sd_9km" ~ "bio5_sd",
                                                             variable == "bio6_sd_9km" ~ "bio6_sd",
                                                             variable == "bio13_sd_9km" ~ "bio13_sd",
                                                             variable == "bio14_sd_9km" ~ "bio14_sd",
                                                             variable == "cloud_9km" ~ "cloud_sd",
                                                             variable == "sd_15km" ~ "sd",
                                                             variable == "bio5_sd_15km" ~ "bio5_sd",
                                                             variable == "bio6_sd_15km" ~ "bio6_sd",
                                                             variable == "bio13_sd_15km" ~ "bio13_sd",
                                                             variable == "bio14_sd_15km" ~ "bio14_sd",
                                                             variable == "cloud_15km" ~ "cloud_sd",
                                                             variable == "sd_21km" ~ "sd",
                                                             variable == "bio5_sd_21km" ~ "bio5_sd",
                                                             variable == "bio6_sd_21km" ~ "bio6_sd",
                                                             variable == "bio13_sd_21km" ~ "bio13_sd",
                                                             variable == "bio14_sd_21km" ~ "bio14_sd",
                                                             variable == "cloud_21km" ~ "cloud_sd",
                                                             variable == "sd_27km" ~ "sd",
                                                             variable == "bio5_sd_27km" ~ "bio5_sd",
                                                             variable == "bio6_sd_27km" ~ "bio6_sd",
                                                             variable == "bio13_sd_27km" ~ "bio13_sd",
                                                             variable == "bio14_sd_27km" ~ "bio14_sd",
                                                             variable == "cloud_27km" ~ "cloud_sd",
                                                             variable == "sd_33km" ~ "sd",
                                                             variable == "bio5_sd_33km" ~ "bio5_sd",
                                                             variable == "bio6_sd_33km" ~ "bio6_sd",
                                                             variable == "bio13_sd_33km" ~ "bio13_sd",
                                                             variable == "bio14_sd_33km" ~ "bio14_sd",
                                                             variable == "cloud_33km" ~ "cloud_sd",
                                                             variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                             variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                             variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                             variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                             variable == "srtm_crop" ~ "srtm_crop",
                                                             variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Av$variable_used <- new_var_names_Av$group
new_var_names_Av$group <- NULL
new_var_names_Av <- as.data.frame(new_var_names_Av)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Av_count <-new_var_names_Av %>% count(variable_used, sort=T)
write.csv(Av_count,"a_vociferans_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Av$radii <- as.numeric(new_var_names_Av$radii)
new_var_names_Av<- arrange(new_var_names_Av, radii)

#turn the radii column back to factor so we can plot
new_var_names_Av$radii <- as.factor(new_var_names_Av$radii)

write.csv(new_var_names_Av,"a_vociferans_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Av_perm_imp <-new_var_names_Av %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. vociferans")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Av <-new_var_names_Av %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Av, "avg_perm_per_var_imp_Av.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Av$variable, "km")
new_var_names_Av$agg_column <- bing
perm_radii.Av <- dcast(new_var_names_Av, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Av, "perm_importance_cleaned_Av.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
a_vociferans.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^a_vociferans")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_vociferans.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
a_vociferans_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
a_vociferans_perf_df <-as.data.frame(bind_rows(a_vociferans_performance, .id = "column_label"))

#remove filename column
a_vociferans_perf_df$filename <-NULL

#add species name
a_vociferans_perf_df$column_label <- "Aotus vociferans"
colnames(a_vociferans_perf_df)[1] <- c("scientific_name")

#change order of radii
a_vociferans_perf_df$auc.train<- as.numeric(a_vociferans_perf_df$auc.train)
a_vociferans_perf_df$radii <- as.numeric(a_vociferans_perf_df$radii)
a_vociferans_perf_df <- arrange(a_vociferans_perf_df, radii)
a_vociferans_perf_df$radii<- as.factor(a_vociferans_perf_df$radii)


#Plot AUC figs
Av_perf_stat_auc <-
  ggplot(data=a_vociferans_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: A. vociferans")+
  ylab("AUC") + xlab("radii (km)")

Av_perf_stat_cbi <-
  ggplot(data=a_vociferans_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: A. vociferans")+
  ylab("CBI") + xlab("radii (km)")

#_____
#species 7
#Cebus capucinus

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
c_capucinus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^c_capucinus")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(c_capucinus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
c_capucinus_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
c_capucinus_pi_df <-as.data.frame(bind_rows(c_capucinus_perm_importance, .id = "column_label"))

# remove filename column
c_capucinus_pi_df$filename <-NULL

# add species name to datafrrame
c_capucinus_pi_df$column_label <- "Cebus capucinus"
colnames(c_capucinus_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
c_capucinus_top_6 <- c_capucinus_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#c_capucinus_var_count <-c_capucinus_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Cc <-c_capucinus_top_6 %>% mutate(group = 
                                                  case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                            variable == "sd_3km" ~ "sd",
                                                            variable == "bio5_sd_3km" ~ "bio5_sd",
                                                            variable == "bio6_sd_3km" ~ "bio6_sd",
                                                            variable == "bio13_sd_3km" ~ "bio13_sd",
                                                            variable == "bio14_sd_3km" ~ "bio14_sd",
                                                            variable == "cloud_3km" ~ "cloud_sd",
                                                            variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                            variable == "sd_9km" ~ "sd",
                                                            variable == "bio5_sd_9km" ~ "bio5_sd",
                                                            variable == "bio6_sd_9km" ~ "bio6_sd",
                                                            variable == "bio13_sd_9km" ~ "bio13_sd",
                                                            variable == "bio14_sd_9km" ~ "bio14_sd",
                                                            variable == "cloud_9km" ~ "cloud_sd",
                                                            variable == "sd_15km" ~ "sd",
                                                            variable == "bio5_sd_15km" ~ "bio5_sd",
                                                            variable == "bio6_sd_15km" ~ "bio6_sd",
                                                            variable == "bio13_sd_15km" ~ "bio13_sd",
                                                            variable == "bio14_sd_15km" ~ "bio14_sd",
                                                            variable == "cloud_15km" ~ "cloud_sd",
                                                            variable == "sd_21km" ~ "sd",
                                                            variable == "bio5_sd_21km" ~ "bio5_sd",
                                                            variable == "bio6_sd_21km" ~ "bio6_sd",
                                                            variable == "bio13_sd_21km" ~ "bio13_sd",
                                                            variable == "bio14_sd_21km" ~ "bio14_sd",
                                                            variable == "cloud_21km" ~ "cloud_sd",
                                                            variable == "sd_27km" ~ "sd",
                                                            variable == "bio5_sd_27km" ~ "bio5_sd",
                                                            variable == "bio6_sd_27km" ~ "bio6_sd",
                                                            variable == "bio13_sd_27km" ~ "bio13_sd",
                                                            variable == "bio14_sd_27km" ~ "bio14_sd",
                                                            variable == "cloud_27km" ~ "cloud_sd",
                                                            variable == "sd_33km" ~ "sd",
                                                            variable == "bio5_sd_33km" ~ "bio5_sd",
                                                            variable == "bio6_sd_33km" ~ "bio6_sd",
                                                            variable == "bio13_sd_33km" ~ "bio13_sd",
                                                            variable == "bio14_sd_33km" ~ "bio14_sd",
                                                            variable == "cloud_33km" ~ "cloud_sd",
                                                            variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                            variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                            variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                            variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                            variable == "srtm_crop" ~ "srtm_crop",
                                                            variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Cc$variable_used <- new_var_names_Cc$group
new_var_names_Cc$group <- NULL
new_var_names_Cc <- as.data.frame(new_var_names_Cc)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Cc_count <-new_var_names_Cc %>% count(variable_used, sort=T)
write.csv(Cc_count,"c_capucinus_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Cc$radii <- as.numeric(new_var_names_Cc$radii)
new_var_names_Cc<- arrange(new_var_names_Cc, radii)

#turn the radii column back to factor so we can plot
new_var_names_Cc$radii <- as.factor(new_var_names_Cc$radii)

write.csv(new_var_names_Cc,"c_capucinus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Cc_perm_imp <-new_var_names_Cc %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance C. capucinus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Cc <-new_var_names_Cc %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Cc, "avg_perm_per_var_imp_Cc.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Cc$variable, "km")
new_var_names_Cc$agg_column <- bing
perm_radii.Cc <- dcast(new_var_names_Cc, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")


write.csv(perm_radii.Cc, "perm_importance_cleaned_Cc.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
c_capucinus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^c_capucinus")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(c_capucinus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
c_capucinus_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
c_capucinus_perf_df <-as.data.frame(bind_rows(c_capucinus_performance, .id = "column_label"))

#remove filename column
c_capucinus_perf_df$filename <-NULL

#add species name
c_capucinus_perf_df$column_label <- "Cebus capucinus"
colnames(c_capucinus_perf_df)[1] <- c("scientific_name")

#change order of radii
c_capucinus_perf_df$auc.train<- as.numeric(c_capucinus_perf_df$auc.train)
c_capucinus_perf_df$radii <- as.numeric(c_capucinus_perf_df$radii)
c_capucinus_perf_df <- arrange(c_capucinus_perf_df, radii)
c_capucinus_perf_df$radii<- as.factor(c_capucinus_perf_df$radii)


#Plot AUC figs
Cc_perf_stat_auc <-
  ggplot(data=c_capucinus_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: C. capucinus")+
  ylab("AUC") + xlab("radii (km)")

Cc_perf_stat_cbi <-
  ggplot(data=c_capucinus_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: C. capucinus")+
  ylab("CBI") + xlab("radii (km)")

#______
#species 8
#Lagothrix lagthricha

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
l_lagothricha.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^l_lagothricha")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(l_lagothricha.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
l_lagothricha_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
l_lagothricha_pi_df <-as.data.frame(bind_rows(l_lagothricha_perm_importance, .id = "column_label"))

# remove filename column
l_lagothricha_pi_df$filename <-NULL

# add species name to datafrrame
l_lagothricha_pi_df$column_label <- "Lagothrix lagothricha"
colnames(l_lagothricha_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
l_lagothricha_top_6 <- l_lagothricha_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#l_lagothricha_var_count <-l_lagothricha_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Ll <-l_lagothricha_top_6 %>% mutate(group = 
                                                    case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                              variable == "sd_3km" ~ "sd",
                                                              variable == "bio5_sd_3km" ~ "bio5_sd",
                                                              variable == "bio6_sd_3km" ~ "bio6_sd",
                                                              variable == "bio13_sd_3km" ~ "bio13_sd",
                                                              variable == "bio14_sd_3km" ~ "bio14_sd",
                                                              variable == "cloud_3km" ~ "cloud_sd",
                                                              variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                              variable == "sd_9km" ~ "sd",
                                                              variable == "bio5_sd_9km" ~ "bio5_sd",
                                                              variable == "bio6_sd_9km" ~ "bio6_sd",
                                                              variable == "bio13_sd_9km" ~ "bio13_sd",
                                                              variable == "bio14_sd_9km" ~ "bio14_sd",
                                                              variable == "cloud_9km" ~ "cloud_sd",
                                                              variable == "sd_15km" ~ "sd",
                                                              variable == "bio5_sd_15km" ~ "bio5_sd",
                                                              variable == "bio6_sd_15km" ~ "bio6_sd",
                                                              variable == "bio13_sd_15km" ~ "bio13_sd",
                                                              variable == "bio14_sd_15km" ~ "bio14_sd",
                                                              variable == "cloud_15km" ~ "cloud_sd",
                                                              variable == "sd_21km" ~ "sd",
                                                              variable == "bio5_sd_21km" ~ "bio5_sd",
                                                              variable == "bio6_sd_21km" ~ "bio6_sd",
                                                              variable == "bio13_sd_21km" ~ "bio13_sd",
                                                              variable == "bio14_sd_21km" ~ "bio14_sd",
                                                              variable == "cloud_21km" ~ "cloud_sd",
                                                              variable == "sd_27km" ~ "sd",
                                                              variable == "bio5_sd_27km" ~ "bio5_sd",
                                                              variable == "bio6_sd_27km" ~ "bio6_sd",
                                                              variable == "bio13_sd_27km" ~ "bio13_sd",
                                                              variable == "bio14_sd_27km" ~ "bio14_sd",
                                                              variable == "cloud_27km" ~ "cloud_sd",
                                                              variable == "sd_33km" ~ "sd",
                                                              variable == "bio5_sd_33km" ~ "bio5_sd",
                                                              variable == "bio6_sd_33km" ~ "bio6_sd",
                                                              variable == "bio13_sd_33km" ~ "bio13_sd",
                                                              variable == "bio14_sd_33km" ~ "bio14_sd",
                                                              variable == "cloud_33km" ~ "cloud_sd",
                                                              variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                              variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                              variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                              variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                              variable == "srtm_crop" ~ "srtm_crop",
                                                              variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Ll$variable_used <- new_var_names_Ll$group
new_var_names_Ll$group <- NULL
new_var_names_Ll <- as.data.frame(new_var_names_Ll)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Ll_count <-new_var_names_Ll %>% count(variable_used, sort=T)
write.csv(Ll_count,"l_lagothricha_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Ll$radii <- as.numeric(new_var_names_Ll$radii)
new_var_names_Ll<- arrange(new_var_names_Ll, radii)

#turn the radii column back to factor so we can plot
new_var_names_Ll$radii <- as.factor(new_var_names_Ll$radii)

write.csv(new_var_names_Ll,"l_lagothricha_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Ll_perm_imp <-new_var_names_Ll %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance L. lagothricha")+
  ylab("permutation importance") + xlab("radii (km)")


#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Ll <-new_var_names_Ll %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Ll, "avg_perm_per_var_imp_Ll.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Ll$variable, "km")
new_var_names_Ll$agg_column <- bing
perm_radii.Ll <- dcast(new_var_names_Ll, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Ll, "perm_importance_cleaned_Ll.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
l_lagothricha.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^l_lagothricha")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(l_lagothricha.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
l_lagothricha_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
l_lagothricha_perf_df <-as.data.frame(bind_rows(l_lagothricha_performance, .id = "column_label"))

#remove filename column
l_lagothricha_perf_df$filename <-NULL

#add species name
l_lagothricha_perf_df$column_label <- "Lagothrix lagothricha"
colnames(l_lagothricha_perf_df)[1] <- c("scientific_name")

#change order of radii
l_lagothricha_perf_df$auc.train<- as.numeric(l_lagothricha_perf_df$auc.train)
l_lagothricha_perf_df$radii <- as.numeric(l_lagothricha_perf_df$radii)
l_lagothricha_perf_df <- arrange(l_lagothricha_perf_df, radii)
l_lagothricha_perf_df$radii<- as.factor(l_lagothricha_perf_df$radii)


#Plot AUC figs
Ll_perf_stat_auc <-
  ggplot(data=l_lagothricha_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: L. lagothricha")+
  ylab("AUC") + xlab("radii (km)")

Ll_perf_stat_cbi <-
  ggplot(data=l_lagothricha_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: L. lagothricha")+
  ylab("CBI") + xlab("radii (km)")

#_______
#species 9
#Alouatta palliata

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
a_palliata.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^a_palliata")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_palliata.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
a_palliata_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
a_palliata_pi_df <-as.data.frame(bind_rows(a_palliata_perm_importance, .id = "column_label"))

# remove filename column
a_palliata_pi_df$filename <-NULL

# add species name to datafrrame
a_palliata_pi_df$column_label <- "Alouatta palliata"
colnames(a_palliata_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
a_palliata_top_6 <- a_palliata_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#a_palliata_var_count <-a_palliata_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_Ap <-a_palliata_top_6 %>% mutate(group = 
                                                 case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                           variable == "sd_3km" ~ "sd",
                                                           variable == "bio5_sd_3km" ~ "bio5_sd",
                                                           variable == "bio6_sd_3km" ~ "bio6_sd",
                                                           variable == "bio13_sd_3km" ~ "bio13_sd",
                                                           variable == "bio14_sd_3km" ~ "bio14_sd",
                                                           variable == "cloud_3km" ~ "cloud_sd",
                                                           variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                           variable == "sd_9km" ~ "sd",
                                                           variable == "bio5_sd_9km" ~ "bio5_sd",
                                                           variable == "bio6_sd_9km" ~ "bio6_sd",
                                                           variable == "bio13_sd_9km" ~ "bio13_sd",
                                                           variable == "bio14_sd_9km" ~ "bio14_sd",
                                                           variable == "cloud_9km" ~ "cloud_sd",
                                                           variable == "sd_15km" ~ "sd",
                                                           variable == "bio5_sd_15km" ~ "bio5_sd",
                                                           variable == "bio6_sd_15km" ~ "bio6_sd",
                                                           variable == "bio13_sd_15km" ~ "bio13_sd",
                                                           variable == "bio14_sd_15km" ~ "bio14_sd",
                                                           variable == "cloud_15km" ~ "cloud_sd",
                                                           variable == "sd_21km" ~ "sd",
                                                           variable == "bio5_sd_21km" ~ "bio5_sd",
                                                           variable == "bio6_sd_21km" ~ "bio6_sd",
                                                           variable == "bio13_sd_21km" ~ "bio13_sd",
                                                           variable == "bio14_sd_21km" ~ "bio14_sd",
                                                           variable == "cloud_21km" ~ "cloud_sd",
                                                           variable == "sd_27km" ~ "sd",
                                                           variable == "bio5_sd_27km" ~ "bio5_sd",
                                                           variable == "bio6_sd_27km" ~ "bio6_sd",
                                                           variable == "bio13_sd_27km" ~ "bio13_sd",
                                                           variable == "bio14_sd_27km" ~ "bio14_sd",
                                                           variable == "cloud_27km" ~ "cloud_sd",
                                                           variable == "sd_33km" ~ "sd",
                                                           variable == "bio5_sd_33km" ~ "bio5_sd",
                                                           variable == "bio6_sd_33km" ~ "bio6_sd",
                                                           variable == "bio13_sd_33km" ~ "bio13_sd",
                                                           variable == "bio14_sd_33km" ~ "bio14_sd",
                                                           variable == "cloud_33km" ~ "cloud_sd",
                                                           variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                           variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                           variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                           variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                           variable == "srtm_crop" ~ "srtm_crop",
                                                           variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_Ap$variable_used <- new_var_names_Ap$group
new_var_names_Ap$group <- NULL
new_var_names_Ap <- as.data.frame(new_var_names_Ap)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
Ap_count <-new_var_names_Ap %>% count(variable_used, sort=T)
write.csv(Ap_count,"a_palliata_variable_count.csv")

#change it to numeric so we can sort
new_var_names_Ap$radii <- as.numeric(new_var_names_Ap$radii)
new_var_names_Ap<- arrange(new_var_names_Ap, radii)

#turn the radii column back to factor so we can plot
new_var_names_Ap$radii <- as.factor(new_var_names_Ap$radii)

write.csv(new_var_names_Ap,"a_palliata_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Ap_perm_imp <-new_var_names_Ap %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. palliata")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_Ap <-new_var_names_Ap %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_Ap, "avg_perm_per_var_imp_Ap.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_Ap$variable, "km")
new_var_names_Ap$agg_column <- bing
perm_radii.Ap <- dcast(new_var_names_Ap, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.Ap, "perm_importance_cleaned_Ap.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
a_palliata.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^a_palliata")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_palliata.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
a_palliata_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
a_palliata_perf_df <-as.data.frame(bind_rows(a_palliata_performance, .id = "column_label"))

#remove filename column
a_palliata_perf_df$filename <-NULL

#add species name
a_palliata_perf_df$column_label <- "Alouatta palliata"
colnames(a_palliata_perf_df)[1] <- c("scientific_name")

#change order of radii
a_palliata_perf_df$auc.train<- as.numeric(a_palliata_perf_df$auc.train)
a_palliata_perf_df$radii <- as.numeric(a_palliata_perf_df$radii)
a_palliata_perf_df <- arrange(a_palliata_perf_df, radii)
a_palliata_perf_df$radii<- as.factor(a_palliata_perf_df$radii)


#Plot AUC figs
Ap_perf_stat_auc <-
  ggplot(data=a_palliata_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: A. palliata")+
  ylab("AUC") + xlab("radii (km)")

Ap_perf_stat_cbi <-
  ggplot(data=a_palliata_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: A. palliata")+
  ylab("CBI") + xlab("radii (km)")

#_____
#species 10
#Alouatta seniculus

# Set working directory to folder where permutation importance is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")

# Pull in files matching species name
a_seniculus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance", pattern="^a_seniculus")

# Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_seniculus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

# add column of radii to each table in list
a_seniculus_perm_importance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


# add id column to the dataframe
a_seniculus_pi_df <-as.data.frame(bind_rows(a_seniculus_perm_importance, .id = "column_label"))

# remove filename column
a_seniculus_pi_df$filename <-NULL

# add species name to datafrrame
a_seniculus_pi_df$column_label <- "Alouatta seniculus"
colnames(a_seniculus_pi_df)[1] <- c("scientific_name")

#------ 
#Find top 6 variables for each model/radii combinationn
a_seniculus_top_6 <- a_seniculus_pi_df %>%
  group_by(radii) %>%
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

#use this to count over ALL species, will add up the number of times different variables are used
#a_seniculus_var_count <-a_seniculus_top_6 %>% count(variable, sort = TRUE)

#change variable names in new column so we can summarize in a figure
new_var_names_As <-a_seniculus_top_6 %>% mutate(group = 
                                                  case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                            variable == "sd_3km" ~ "sd",
                                                            variable == "bio5_sd_3km" ~ "bio5_sd",
                                                            variable == "bio6_sd_3km" ~ "bio6_sd",
                                                            variable == "bio13_sd_3km" ~ "bio13_sd",
                                                            variable == "bio14_sd_3km" ~ "bio14_sd",
                                                            variable == "cloud_3km" ~ "cloud_sd",
                                                            variable == "bio14_sd_9km" ~ "bio14_sd", 
                                                            variable == "sd_9km" ~ "sd",
                                                            variable == "bio5_sd_9km" ~ "bio5_sd",
                                                            variable == "bio6_sd_9km" ~ "bio6_sd",
                                                            variable == "bio13_sd_9km" ~ "bio13_sd",
                                                            variable == "bio14_sd_9km" ~ "bio14_sd",
                                                            variable == "cloud_9km" ~ "cloud_sd",
                                                            variable == "sd_15km" ~ "sd",
                                                            variable == "bio5_sd_15km" ~ "bio5_sd",
                                                            variable == "bio6_sd_15km" ~ "bio6_sd",
                                                            variable == "bio13_sd_15km" ~ "bio13_sd",
                                                            variable == "bio14_sd_15km" ~ "bio14_sd",
                                                            variable == "cloud_15km" ~ "cloud_sd",
                                                            variable == "sd_21km" ~ "sd",
                                                            variable == "bio5_sd_21km" ~ "bio5_sd",
                                                            variable == "bio6_sd_21km" ~ "bio6_sd",
                                                            variable == "bio13_sd_21km" ~ "bio13_sd",
                                                            variable == "bio14_sd_21km" ~ "bio14_sd",
                                                            variable == "cloud_21km" ~ "cloud_sd",
                                                            variable == "sd_27km" ~ "sd",
                                                            variable == "bio5_sd_27km" ~ "bio5_sd",
                                                            variable == "bio6_sd_27km" ~ "bio6_sd",
                                                            variable == "bio13_sd_27km" ~ "bio13_sd",
                                                            variable == "bio14_sd_27km" ~ "bio14_sd",
                                                            variable == "cloud_27km" ~ "cloud_sd",
                                                            variable == "sd_33km" ~ "sd",
                                                            variable == "bio5_sd_33km" ~ "bio5_sd",
                                                            variable == "bio6_sd_33km" ~ "bio6_sd",
                                                            variable == "bio13_sd_33km" ~ "bio13_sd",
                                                            variable == "bio14_sd_33km" ~ "bio14_sd",
                                                            variable == "cloud_33km" ~ "cloud_sd",
                                                            variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                            variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                            variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                            variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                            variable == "srtm_crop" ~ "srtm_crop",
                                                            variable == "cloud_crop" ~ "cloud_crop"))

# Add column for the variable categories used (e.g., sd of cloud cover)
new_var_names_As$variable_used <- new_var_names_As$group
new_var_names_As$group <- NULL
new_var_names_As <- as.data.frame(new_var_names_As)

#set working directory to top performers folder
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers")

#How many times particular  variables contributed to the model (top 6) for a species
As_count <-new_var_names_As %>% count(variable_used, sort=T)
write.csv(As_count,"a_seniculus_variable_count.csv")

#change it to numeric so we can sort
new_var_names_As$radii <- as.numeric(new_var_names_As$radii)
new_var_names_As<- arrange(new_var_names_As, radii)

#turn the radii column back to factor so we can plot
new_var_names_As$radii <- as.factor(new_var_names_As$radii)

write.csv(new_var_names_As,"a_seniculus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

As_perm_imp <-new_var_names_As %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. seniculus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_As <-new_var_names_As %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
  arrange(desc(permutation.importance))

#write average permutation importance to file
write.csv(avg_perm_imp_As, "avg_perm_per_var_imp_As.csv")

#Get a table that has the variable and the permutation importances at each radii (table version of figure)
bing <-str_detect(new_var_names_As$variable, "km")
new_var_names_As$agg_column <- bing
perm_radii.As <- dcast(new_var_names_As, variable_used + agg_column ~ radii, value.var="permutation.importance")

#write table to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/permutation_importance")
write.csv(perm_radii.As, "perm_importance_cleaned_As.csv")


#_________
#Model performance stats


# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats")

#Pull in files matching species name
a_seniculus.files <- list.files("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/perf_stats", pattern="^a_seniculus")

#Combine files to be one long list 
library(data.table)
result <- rbindlist(sapply(a_seniculus.files, fread,simplify = FALSE), idcol = 'filename')

data_list <- split(result, f = result$filename)                     # Split data
data_list                               

#add column of radii to each table in list
a_seniculus_performance <- Map(cbind, data_list, radii = c("1","15","21","27","3","33","9"))


#add id column to the dataframe
a_seniculus_perf_df <-as.data.frame(bind_rows(a_seniculus_performance, .id = "column_label"))

#remove filename column
a_seniculus_perf_df$filename <-NULL

#add species name
a_seniculus_perf_df$column_label <- "Alouatta seniculus"
colnames(a_seniculus_perf_df)[1] <- c("scientific_name")

#change order of radii
a_seniculus_perf_df$auc.train<- as.numeric(a_seniculus_perf_df$auc.train)
a_seniculus_perf_df$radii <- as.numeric(a_seniculus_perf_df$radii)
a_seniculus_perf_df <- arrange(a_seniculus_perf_df, radii)
a_seniculus_perf_df$radii<- as.factor(a_seniculus_perf_df$radii)


#Plot AUC figs
As_perf_stat_auc <-
  ggplot(data=a_seniculus_perf_df, aes(x=radii, y=auc.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("AUC: A. seniculus")+
  ylab("AUC") + xlab("radii (km)")

As_perf_stat_cbi <-
  ggplot(data=a_seniculus_perf_df, aes(x=radii, y=cbi.train, group=scientific_name)) +
  geom_point() +
  geom_line(color="purple") +
  ggtitle("CBI: A. seniculus")+
  ylab("CBI") + xlab("radii (km)")


#_________________________
#Summary stats over all species
#multispecies plot (AUC)
all_species <-rbind(b_neblina_perf_df, t_ornatus_perf_df, a_vociferans_perf_df, c_capucinus_perf_df, l_lagothricha_perf_df, P_ornatus_perf_df, a_hybridus_perf_df, a_palliata_perf_df, a_seniculus_perf_df, a_brumbacki_perf_df)
multi_spec_perf_stat_auc <-
ggplot(data=all_species, aes(x=radii, y=auc.train, group=scientific_name, col=scientific_name)) +
geom_point() +
geom_line() +
ggtitle("Model Performance: AUC for all")+
ylab("AUC") + xlab("radii (km)") + guides(col=guide_legend("species"))

multi_spec_perf_stat_CBI <-
  ggplot(data=test, aes(x=radii, y=cbi.train, group=scientific_name, col=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("Model Performance: CBI for all")+
  ylab("CBI") + xlab("radii (km)") + guides(col=guide_legend("species"))

#montane species
mont.spec <-rbind(b_neblina_perf_df, t_ornatus_perf_df)

mont_spec_perf_stat_auc <-
  ggplot(data=mont.spec, aes(x=radii, y=auc.train, group=scientific_name, col=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("Model Performance: AUC for montane species")+
  ylab("AUC") + xlab("radii (km)") + guides(col=guide_legend("species"))

#lowland species
low.spec <-rbind(a_vociferans_perf_df, c_capucinus_perf_df, l_lagothricha_perf_df, P_ornatus_perf_df, a_seniculus_perf_df, a_brumbacki_perf_df)

low_spec_perf_stat_auc <-
  ggplot(data=low.spec, aes(x=radii, y=auc.train, group=scientific_name, col=scientific_name)) +
  geom_point() +
  geom_line() +
  ggtitle("Model Performance: AUC for lowland species")+
  ylab("AUC") + xlab("radii (km)") + guides(col=guide_legend("species"))

#Average permutation importance across species per radii (can get at the question, on average how well are these variables performing at each radii)
b_neblina_pi_df <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers/b_neblina_perm_imp_all_runs.csv")
t_ornatus_pi_df <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/run1_results/top_performers/t_ornatus_perm_imp_all_runs.csv")
b_neblina_pi_df$X <- NULL
t_ornatus_pi_df$X <- NULL
mont.spec.perm.imp <-rbind(b_neblina_pi_df, t_ornatus_pi_df)

#plot average permutation importance at each radii for montane species
 mont_avg_radii_mult_spec <- mont.spec.perm.imp %>%group_by(variable_used) %>% 
   summarise_at(vars("permutation.importance"), mean) %>%
   arrange(desc(permutation.importance))
mont_avg_radii_mult_spec$radii <- as.factor(mont_avg_radii_mult_spec$radii)


montane_spec_perm_imp <-
  ggplot(data= mont_avg_radii_mult_spec , aes(x=radii, y=permutation.importance, group=variable_used, col=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation Importance: montane species")+
  ylab("PI") + xlab("radii (km)") + guides(col=guide_legend("variable category"))




 #lowland average perm importance
low.spec.perm.imp <- rbind(new_var_names_As, new_var_names_Cc, new_var_names_Ll, new_var_names_Po, new_var_names_Ap, new_var_names_ab)


lowland_avg_radii_mult_spec <- low.spec.perm.imp %>%group_by(variable_used) %>% 
  summarise_at(vars("permutation.importance"), mean) %>%
  arrange(desc(permutation.importance))
lowland_avg_radii_mult_spec <- as.data.frame(lowland_avg_radii_mult_spec)
lowland_avg_radii_mult_spec$radii <- as.factor(lowland_avg_radii_mult_spec$radii)


lowland_spec_perm_imp <-
  ggplot(data= lowland_avg_radii_mult_spec , aes(x=radii, y=permutation.importance, group=variable_used, col=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation Importance: lowland species")+
  ylab("PI") + xlab("radii (km)") + guides(col=guide_legend("variable category"))
#-----
#data rich vs data poor
rich.spec.perm.imp <- rbind(new_var_names_As, new_var_names_Cc, new_var_names_Ll, new_var_names_Ap, new_var_names_To)


rich_avg_radii_mult_spec <- rich.spec.perm.imp %>%group_by(variable) %>% 
  summarise_at(vars("permutation.importance"), mean) %>%
  arrange(desc(permutation.importance))



poor.spec.perm.imp <- rbind(new_var_names_Bn, new_var_names_ab, new_var_names_Ah, new_var_names_Av, new_var_names_Po)

poor_avg_radii_mult_spec <- poor.spec.perm.imp %>%group_by(variable) %>% 
  summarise_at(vars("permutation.importance"), mean) %>%
  arrange(desc(permutation.importance))




#Model for Olinguito using top performing variables

#test for olinguito
#plot average permutation importance at each radii for montane species
bn_avg_radii_olinguito <- b_neblina_pi_df %>%group_by(variable_used) %>% 
  summarise_at(vars("permutation.importance"), mean) %>%
  arrange(desc(permutation.importance))
bn_avg_radii_olinguito$radii <- as.factor(bn_avg_radii_olinguito$radii)

olin_spec_perm_imp <-
  ggplot(data= bn_avg_radii_olinguito, aes(x=radii, y=permutation.importance, group=variable_used, col=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation Importance: montane species")+
  ylab("PI") + xlab("radii (km)") + guides(col=guide_legend("variable category"))

#Count number of times variable at each radii was used

#How many times particular  variables contributed to the model (top 6) for a species

all_perm <- rbind(new_var_names_As, new_var_names_Bn, new_var_names_Cc, new_var_names_To, new_var_names_Ap, new_var_names_Av, new_var_names_Ll, new_var_names_Po, new_var_names_Ah)

Full_count <-all_perm %>% count(variable, sort=T)
Full_count_geodiv <- Full_count[7:42,]
Full_count_geodiv$percent <-(Full_count_geodiv$n/10)*100
Full_count_nongeo <- Full_count[1:6,]
Full_count_nongeo$percent <- (Full_count_nongeo$n/78)*100

full_count_percent <- rbind(Full_count_nongeo,Full_count_geodiv)



Full_count_category <-all_perm %>% count(variable_used, sort=T)
write.csv(Full_count_category,"full_count_categories.c")


Full_count_category$percent <- (Full_count_category$n/78)*100
t <- full_count_percent[order(percent),]




#change it to numeric so we can sort
new_var_names_As$radii <- as.numeric(new_var_names_As$radii)
new_var_names_As<- arrange(new_var_names_As, radii)

#turn the radii column back to factor so we can plot
new_var_names_As$radii <- as.factor(new_var_names_As$radii)

write.csv(new_var_names_As,"a_seniculus_perm_imp_all_runs.csv")

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

As_perm_imp <-new_var_names_As %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. seniculus")+
  ylab("permutation importance") + xlab("radii (km)")

#average permutation importance for each variable across all radii
#Will look at average permutation importance across all radii for a given variable set and then see which specific radii within the variable set led to highest gains in performance. Need a table of performance at each radii. Cross reference radii/top 6 variable sets/AUC/BOYCE
avg_perm_imp_As <-new_var_names_As %>% group_by(variable_used) %>% summarise(permutation.importance = mean(permutation.importance)) %>%
