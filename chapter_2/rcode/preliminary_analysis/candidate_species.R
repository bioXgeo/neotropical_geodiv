#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code searches for candidate species to use. Looking for both data rich and data poor primate species. There has to be enough occurrence records to be considered rich after using spThin to remove sampling bias from the datasets.

#Authors: Beth E. Gerstner

#Date: 1/18/22

#spThin test
library(spThin)

#set directory
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset")

#Load in GBIF data
ateles <- read.csv("/Users/bethgerstner/Desktop/GBIF_primate/spThin_test_2.csv")
plecturocebus <- read.csv("/Users/bethgerstner/Desktop/GBIF_primate/spThin_test_3.csv")
sanguinus <- read.csv("/Users/bethgerstner/Desktop/GBIF_primate/spThin_test_4.csv")
alouatta <- read.csv("/Users/bethgerstner/Desktop/GBIF_primate/spThin_test_5.csv")

#Thin each dataset and see how many localities remain
thinned_dataset_full <-
  thin( loc.data = ateles, 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = "species", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "ateles_thinned_full/", out.base = "ateles_thinned", 
        write.log.file = TRUE,
        log.file = "ateles_thinned_full_log_file.txt") #64 records of 115

thinned_dataset_full_plec <-
  thin( loc.data = plecturocebus, 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = "species", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "plecturocebus_thinned_full/", out.base = "plecturocebus_thinned", 
        write.log.file = TRUE,
        log.file = "plecturocebus_thinned_full_log_file.txt" ) #34 of 201

thinned_dataset_full_plec_small <-
  thin( loc.data = plecturocebus, 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = "species", 
        thin.par = 5, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "plecturocebus_thinned_full_small/", out.base = "plecturocebus_thinned_small", 
        write.log.file = TRUE,
        log.file = "plecturocebus_thinned_full_log_file.txt" ) #53 of 201

thinned_dataset_full_sang <-
  thin( loc.data = sanguinus, 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = "species", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "sanguinus_thinned_full/", out.base = "sanguinus_thinned", 
        write.log.file = TRUE, 
        log.file = "sanguinus_thinned_full_log_file.txt" ) #88 of 801


thinned_dataset_full_alou <-
  thin( loc.data = alouatta, 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = "species", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "alouatta_thinned_full/", out.base = "alouatta_thinned", 
        write.log.file = TRUE,
        log.file = "alouatta_thinned_full_log_file.txt") #582 of 2330
