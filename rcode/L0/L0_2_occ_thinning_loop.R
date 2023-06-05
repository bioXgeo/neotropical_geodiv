#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code searches for candidate species to use by running spThin over an entire suite of species from BioModelos, saving species datasets as seperate files (full species occurrence datasets and spatially thinned datasets). Looking for both data rich and data poor primate species. There has to be enough occurrence records to be considered rich after using spThin to remove sampling bias from the datasets.

#Authors: Beth E. Gerstner

#Date: 3/17/22

#spThin test
library(spThin)
library(stringr)
#RUN AGAIN
# read full occurrence dataset for all species
occ_full <- read.csv("C:/Users/bgers/Desktop/new_montane_species.csv")

# set working directory
setwd("C:/Users/bgers/Desktop/new_thin")

# Takes the master species list for endemic species, splits them into seperate species files and thins each species dataset. Saves as it's own file.
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
        log.file = paste(f_name,"_thinned_full_log_file.txt", sep="")) #64 records of 115
}

