#Analyzing top predictors

#A. palliata permutation importance

ap_1x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_palliata/evaluation/a_palliata_permutation_imp_e.mx.csv")
ap_3x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_palliata/evaluation/a_palliata_permutation_imp_e.mx.3.csv")
ap_15x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_palliata/evaluation/a_palliata_permutation_imp_e.mx.15.csv")
ap_31x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/31x/Alouatta_palliata/evaluation/a_palliata_permutation_imp_e.mx.31.csv")

ap_1x_perm <- ap_1x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

ap_1x_perm$radii <- c("0")

ap_3x_perm <- ap_3x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

ap_3x_perm$radii <- c("3")


ap_15x_perm <- ap_15x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

ap_15x_perm$radii <- c("15")

ap_31x_perm <- ap_31x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

ap_31x_perm$radii <- c("31")

ap_percent_contribution <- rbind(ap_1x_perm,ap_3x_perm,ap_15x_perm,ap_31x_perm)

ap_percent_contribution %>% count(variable, sort = TRUE)


#all variables
ap_1x_perm_all <- ap_1x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

ap_1x_0rm <- ap_1x_perm_all[ap_1x_perm_all$permutation.importance > 0,]

ap_1x_0rm$radii<- c("0")

ap_3x_perm_all <- ap_3x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

ap_3x_0rm <- ap_3x_perm_all[ap_3x_perm_all$permutation.importance > 0,]

ap_3x_0rm$radii <- c("3")


ap_15x_perm_all <- ap_15x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

ap_15x_0rm <-ap_15x_perm_all[ap_15x_perm_all$permutation.importance > 0,]

ap_15x_0rm$radii <- c("15")

ap_31x_perm_all <- ap_31x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

ap_31x_0rm <-ap_31x_perm_all[ap_31x_perm_all$permutation.importance > 0,]

ap_31x_0rm$radii <- c("31")



ap_percent_contribution_all <- rbind(ap_1x_0rm,ap_3x_0rm,ap_15x_0rm,ap_31x_0rm)

#change variable names in new column
new_var_names_ap <-ap_percent_contribution_all %>% mutate(group =
                     case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                               variable == "sd_3km" ~ "sd",
                               variable == "bio5_sd_3km" ~ "bio5_sd",
                               variable == "bio6_sd_3km" ~ "bio6_sd",
                               variable == "bio13_sd_3km" ~ "bio13_sd",
                               variable == "bio14_sd_3km" ~ "bio14_sd",
                               variable == "sd_15km" ~ "sd",
                               variable == "bio5_sd_15km" ~ "bio5_sd",
                               variable == "bio6_sd_15km" ~ "bio6_sd",
                               variable == "bio13_sd_15km" ~ "bio13_sd",
                               variable == "bio14_sd_15km" ~ "bio14_sd",
                               variable == "sd_31km" ~ "sd",
                               variable == "bio5_sd_31km" ~ "bio5_sd",
                               variable == "bio6_sd_31km" ~ "bio6_sd",
                               variable == "bio13_sd_31km" ~ "bio13_sd",
                               variable == "bio14_sd_31km" ~ "bio14_sd",
                               variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                               variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                               variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                               variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                               variable == "srtm_crop" ~ "srtm_crop"))
                               
new_var_names_ap$variable_used <- new_var_names_ap$group
new_var_names_ap$group <- NULL
  
#How many times particular variables contributed to the model
new_var_names_ap %>% count(variable_used, sort = TRUE)
new_var_names_ap$radii <- as.factor(new_var_names_ap$radii)

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

Ap_perm_imp <-new_var_names_ap %>%
ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. palliata")+
  ylab("permutation importance") + xlab("radii (km)")

#________________________________________________________________________________________________________________
#A. seniculus permutation importance

as_1x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_seniculus/evaluation/a_seniculus_permutation_imp_e.mx.as.csv")
as_3x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_seniculus/evaluation/a_seniculus_permutation_imp_e.mx.as.3km.csv")
as_15x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_seniculus/evaluation/a_seniculus_permutation_imp_e.mx.as.15km.csv")
as_31x <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/31x/Alouatta_seniculus/evaluation/a_seniculus_permutation_imp_e.mx.as.31km.csv")

as_1x_perm <- as_1x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

as_1x_perm$radii <- c("0")

as_3x_perm <- as_3x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

as_3x_perm$radii <- c("3")


as_15x_perm <- as_15x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

as_15x_perm$radii <- c("15")

as_31x_perm <- as_31x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:6)

as_31x_perm$radii <- c("31")

as_percent_contribution <- rbind(as_1x_perm,as_3x_perm,as_15x_perm,as_31x_perm)

as_percent_contribution %>% count(variable, sort = TRUE)


#all variables
as_1x_perm_all <- ap_1x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

as_1x_0rm <- as_1x_perm_all[as_1x_perm_all$permutation.importance > 0,]

as_1x_0rm$radii<- c("0")

as_3x_perm_all <- as_3x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

as_3x_0rm <- as_3x_perm_all[as_3x_perm_all$permutation.importance > 0,]

as_3x_0rm$radii <- c("3")


as_15x_perm_all <- as_15x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

as_15x_0rm <-as_15x_perm_all[as_15x_perm_all$permutation.importance > 0,]

as_15x_0rm$radii <- c("15")

as_31x_perm_all <- as_31x %>%                                      # Top N highest values by group
  arrange(desc(permutation.importance)) %>% 
  slice(1:10)

as_31x_0rm <-as_31x_perm_all[as_31x_perm_all$permutation.importance > 0,]

as_31x_0rm$radii <- c("31")



as_percent_contribution_all <- rbind(as_1x_0rm,as_3x_0rm,as_15x_0rm,as_31x_0rm)

#change variable names in new column
new_var_names_as <-as_percent_contribution_all %>% mutate(group =
                                                            case_when(variable == "bio14_sd_3km" ~ "bio14_sd", 
                                                                      variable == "sd_3km" ~ "sd",
                                                                      variable == "bio5_sd_3km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_3km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_3km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_3km" ~ "bio14_sd",
                                                                      variable == "sd_15km" ~ "sd",
                                                                      variable == "bio5_sd_15km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_15km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_15km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_15km" ~ "bio14_sd",
                                                                      variable == "sd_31km" ~ "sd",
                                                                      variable == "bio5_sd_31km" ~ "bio5_sd",
                                                                      variable == "bio6_sd_31km" ~ "bio6_sd",
                                                                      variable == "bio13_sd_31km" ~ "bio13_sd",
                                                                      variable == "bio14_sd_31km" ~ "bio14_sd",
                                                                      variable == "bio13_1981.2010_V.2.1" ~ "bio13",
                                                                      variable == "bio14_1981.2010_V.2.1" ~ "bio14",
                                                                      variable == "bio5_1981.2010_V.2.1" ~ "bio5",
                                                                      variable == "bio6_1981.2010_V.2.1" ~ "bio6",
                                                                      variable == "srtm_crop" ~ "srtm_crop"))

new_var_names_as$variable_used <- new_var_names_as$group
new_var_names_as$group <- NULL

#How many times particular variables contributed to the model
new_var_names_as %>% count(variable_used, sort = TRUE)

new_var_names_as$radii <- as.numeric(new_var_names_as$radii)
new_var_names_as$radii <- as.factor(new_var_names_as$radii)

#Plot showing percent contribution of variable grouped by radii
library(ggplot2)

As_perm_imp <-new_var_names_as %>%
  ggplot( aes(x=radii, y=permutation.importance, group=variable_used, color=variable_used)) +
  geom_point() +
  geom_line() +
  ggtitle("Permutation importance A. seniculus")+
  ylab("permutation importance") + xlab("radii (km)")

library(tidyr)
spread(new_var_names_, key = radii, value = permutation.importance)

#____________________________________________________________________________________________________
#Boyce index
#A. palliata permutation importance

ap_1x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_palliata/evaluation/a_palliata_min_AIC_e.mx.csv")
ap_3x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_palliata/evaluation/a_palliata_min_AIC_e.mx.3.csv")
ap_15x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_palliata/evaluation/a_palliata_min_AIC_e.mx.15.csv")
ap_31x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/31x/Alouatta_palliata/evaluation/a_palliata_min_AIC_e.mx.31.csv")


#Boyce index point level model
ap_1x_boyce <- as.data.frame(ap_1x_b$cbi.train)

ap_3x_b_boyce <- as.data.frame(ap_3x_b$cbi.train)

ap_15x_b_boyce <- as.data.frame(ap_15x_b$cbi.train)

ap_31x_b_boyce <- as.data.frame(ap_31x_b$cbi.train)

boyce_ap <-cbind(ap_1x_boyce,ap_3x_b_boyce,ap_15x_b_boyce,ap_31x_b_boyce )
boyce_ap$species <- c("Alouatta palliata")

colnames(boyce_ap) <- c("CBI.point","CBI.3x","CBI.15x","CBI.31x")

#Boyce index
#A. seniculus permutation importance

as_1x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.csv")
as_3x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.3km.csv")
as_15x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.15km.csv")
as_31x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/31x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.31km.csv")


#Boyce index point level model
as_1x_boyce <- as.data.frame(as_1x_b$cbi.train)

as_3x_b_boyce <- as.data.frame(as_3x_b$cbi.train)

as_15x_b_boyce <- as.data.frame(as_15x_b$cbi.train)

as_31x_b_boyce <- as.data.frame(as_31x_b$cbi.train)

boyce_as <-cbind(as_1x_boyce,as_3x_b_boyce,as_15x_b_boyce,as_31x_b_boyce )
boyce_as$species <- c("Alouatta seniculus")

colnames(boyce_as) <- c("CBI.point","CBI.3x","CBI.15x","CBI.31x")

boyce_all <- rbind(boyce_ap, boyce_as)


#AUC


#AUC index point level model
ap_1x_auc <- as.data.frame(ap_1x_b$auc.train)

ap_3x_b_auc <- as.data.frame(ap_3x_b$auc.train)

ap_15x_b_auc <- as.data.frame(ap_15x_b$auc.train)

ap_31x_b_auc <- as.data.frame(ap_31x_b$auc.train)
AUC_ap <-cbind(ap_1x_auc,ap_3x_b_auc,ap_15x_b_auc,ap_31x_b_auc )
AUC_ap$species <- c("Alouatta palliata")

colnames(AUC_ap) <- c("AUC.point","AUC.3x","AUC.15x","AUC.31x")

#Boyce index
#A. seniculus permutation importance

as_1x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.csv")
as_3x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.3km.csv")
as_15x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.15km.csv")
as_31x_b <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/31x/Alouatta_seniculus/evaluation/a_seniculus_min_AIC_e.mx.as.31km.csv")


#Boyce index point level model
as_1x_auc <- as.data.frame(as_1x_b$auc.train)

as_3x_b_auc <- as.data.frame(as_3x_b$auc.train)

as_15x_b_auc <- as.data.frame(as_15x_b$auc.train)

as_31x_b_auc <- as.data.frame(as_31x_b$auc.train)
AUC_as <-cbind(as_1x_auc,as_3x_b_auc,as_15x_b_auc,as_31x_b_auc )
AUC_as$species <- c("Alouatta seniculus")

colnames(AUC_as) <- c("AUC.point","AUC.3x","AUC.15x","AUC.31x")

AUC_all <- rbind(AUC_ap, AUC_as)


