#Title: Spatially thinning occurrence records

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code runs r package 'spThin' over an entire suite of species occurrence records, saving species datasets as seperate files (full species occurrence datasets and spatially thinned datasets).

#Data output: CSV files of full occurrence dataset for each species and thinned occurrence records for each species.

#Author: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

#Date: 3/17/22

# load libraries
library(spThin)
library(stringr)

# read full occurrence dataset for all species
occ_full <- read.csv("INSERT FILE PATH")

# set working directory
setwd("INSERT FILE PATH")

# Takes the master species list for species, splits them into separate species files and thins each species dataset. Saves as it's own file.
for(i in unique(occ_full$species)){
  species1=occ_full[occ_full$species==i,]
  f_name <-  str_replace(i, " ", "_")
  write.csv(species1, paste(f_name,".csv",sep=""))
thinned_dataset_full <-thin( loc.data = species1, 
        lat.col = "decimalLatitude", long.col = "decimalLongitude", 
        spec.col = "species", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = paste(f_name,"_thinned_full/", sep=""), out.base = paste(f_name,"_thinned", sep=""), 
        write.log.file = TRUE,
        log.file = paste(f_name,"_thinned_full_log_file.txt", sep="")) 
}


