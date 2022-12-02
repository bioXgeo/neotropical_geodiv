#Biomodelos records
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(wallace)

#read in species list
scientific_names_mammals <- read.csv("C:/Users/bgers/Desktop/final_species_list.csv")
bioKey <- "5NbMdjylEPEN1cBCIsX9dl:3vbVvDAXQdjf40NeYHfN1g"

all.mammals.biomod <- data.frame()
for(i in 1:nrow(scientific_names_mammals)){
  species.i <- scientific_names_mammals[i,"species"]
  bioModelos <- occs_biomodelos(
    spN = species.i,
    bioKey = bioKey)
  occs_i <- bioModelos$cleaned
  occs_i <- occs_i[,c("scientific_name","latitude","longitude","year","state_province","record_type", "catalog_number", "institution_code")]
  all.mammals.biomod= rbind(all.mammals.biomod, occs_i)
}
#cheracebus lucifer requires special access?
all.mammals.1 <- data.frame()
for(i in 27:32){
  species.i <- scientfic_names_mammals[i,"species"]
  bioModelos <- occs_biomodelos(
    spN = species.i,
    bioKey = bioKey)
  occs_i <- bioModelos$cleaned
  occs_i <- occs_i[,c("scientific_name","latitude","longitude","year","state_province","record_type", "catalog_number", "institution_code")]
  all.mammals.1= rbind(all.mammals, occs_i)
}

all.mammals.full <- rbind(all.mammals, all.mammals.1)
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset")
library(readr)
#add the missing species somehow
write.csv(all.mammals.full,path="all_mammals_biomodelos.csv")

library(dplyr)
#count number of records per species
species_num <- all.mammals %>%
  count(scientific_name, sort = TRUE) 

bioModelos <- occs_biomodelos(
  spN = "Cheracebus lucifer",
  bioKey = bioKey)
occs_i <- bioModelos$cleaned
occs_i <- occs_i[,c("scientific_name","latitude","longitude","year","state_province","record_type", "catalog_number", "institution_code")]

library("dplyr")                                                
library("plyr")                                                 
library("readr")  

#downloaded certain species individually because of errors and pulled them in all together
single_data <- list.files(path = "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/biomodelos_singles",    
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                           
  bind_rows                                                      

single_data <- as.data.frame(single_data)

single_data_clean <- single_data[,c("species","latitude","longitude","year", "stateProvince","basisOfRecord","catalogNumber",
                                    "institutionCode")]

names(single_data_clean)


all.mammals <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/all_mammals_biomodelos_2022.csv")


library(data.table)
# Rename multiple columns for old to new
setnames(single_data_clean, old = c('species','stateProvince','basisOfRecord','catalogNumber','institutionCode'), 
         new = c('scientific_name','state_province','record_type','catalog_number','institution_code'))
names(single_data_clean)

#bind all biomodelos datasets together
full_biomodelos_data <- rbind(all.mammals,single_data_clean)
                              
#count records for each species
library(dplyr)
species_num_final <- full_biomodelos_data %>%
  count(scientific_name) 
library(janitor)
tabyl(full_biomodelos_data, scientific_name)

#read in biomodelos data
biomod_data <- read.csv("C:/Users/bgers/Desktop/final_species_list.csv")

prim_data <-read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/biomodelos_gbif_primate/GBIF_species_list.csv")
prim_locs <- prim_data[, c("species","numberOfOccurrences")]

record_num <- merge(prim_data, biomod_data, by="species", all.y=T)

write.csv(record_num, "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/biomodelos_gbif_primate/occ_comparison.csv")

