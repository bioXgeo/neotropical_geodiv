#GBIF Occurrence Data Retrieval 

#Download data by genus 
library(dismo)

#read in mammal database. This is the species subset from Biomodelos that qualify as validated/with model
data1_mammals<- read.csv("/Users/bethgerstner/Desktop/test_species_full.csv")
head(data1_mammals)
scientific_names_full <- data1_mammals$scientific_names
data_name_df_full <- as.data.frame(scientific_names_full)

#split scientific names into genus and species
library(tidyr)
library(dplyr)
library(dismo)
scientific_names_mammals_full <-separate(data_name_df_full, scientific_names_full, into = c("genus", "species"), sep= " (?=[^ ]+$)")
scientific_names_mammals_full=na.omit(scientific_names_mammals_full)
# Downloading GBIF data
# Only grabs 300 at a time. Need to automate this process for all mammal and bird species in the final database. 
# Ideally this code will put all of the records for each species in a single dataframe, but because this will be so big, maybe it is 
# better to break the original species list up into batches.

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/GBIF")
#ran alternative GBIF code that downloads from website
#all.mammals <- data.frame()

# #for(i in 1:nrow(scientific_names_mammals_full)){
#   genus.i <- scientific_names_mammals_full[i,"genus"]
#   species.i <- scientific_names_mammals_full[i,"species"]
#   gbif_mammals_full <- gbif(genus.i, species=species.i, geo=TRUE, sp=FALSE, 
#                removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
#   gbif_mammals_full <- gbif_mammals_full[,c("species","lat","lon","year")]
#   t <-gbif_mammals_full[!is.na(gbif_mammals_full$lon),]
#   all.mammals= rbind(all.mammals, t)
# }

#write.csv(all.mammals, "all.mammals.csv")

#subset all data to those post 2000
# all.mammals.2000 <- all.mammals[all.mammals$year >= 2000,]
all.mammals <- read.csv("/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/full_gbif_candidate_species_2000.csv")
occ_data_full <- all.mammals[,c("species","decimalLatitude","decimalLongitude", "year")]
write.csv(occ_data_full, "occ_data_full.csv")

all.mammals.count <-as.data.frame(all.mammals %>% count(species))
write.csv(all.mammals.count, "all_mammals_count.csv")



### TOKEN FOR ACCESSING IUCN API ###########################################
### This is only authorized to be used by people from MSU ##################
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
library(rredlist)

all_mammals_hab <- list()
for(i in scientific_names_full){
  habitat_mammals <- rl_habitats(i, key=token)
  all_mammals_hab[[i]] <-habitat_mammals
}

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/species_subset")
sink("all_mammals_hab.txt")
print(all_mammals_hab)
sink()

library(foreach)
rbindlist.v2 <- function(l)
{
  l <- l[lapply(l, class) == "list"]
  df <- foreach(element = l, .combine = bind_rows, .errorhandling = 'remove') %dopar%
    {df = unlist(element); df = as.data.frame(t(df)); rm(element); return(df)}
  rm(l)
  return(df)
}
all_mammals_hab_df <-rbindlist.v2(all_mammals_hab)

write.csv(all_mammals_hab_df, "all.mammals_hab_df.csv")


#_____________

#RUN THIS AGAIN
#read in mammal database. This is the species subset from Biomodelos that qualify as endemic (don't necessarily have models and are not validated)
data1_mammals_end<- read.csv("/Users/bethgerstner/Desktop/test_species_endemic.csv")
head(data1_mammals_end)
scientific_names_e <- data1_mammals_end$scientific_names
data_name_df_e <- as.data.frame(scientific_names_e)

#split scientific names into genus and species
library(tidyr)
library(dplyr)
scientific_names_mammals_e <-separate(data_name_df_e, scientific_names_e, into = c("genus", "species"), sep= " (?=[^ ]+$)")
scientific_names_mammals_e=na.omit(scientific_names_mammals_e)
# Downloading GBIF data
# Only grabs 300 at a time. Need to automate this process for all mammal and bird species in the final database. 
# Ideally this code will put all of the records for each species in a single dataframe, but because this will be so big, maybe it is 
# better to break the original species list up into batches.
all.mammals.e <- data.frame()

for(i in 1:nrow(scientific_names_mammals_e)){
  genus.i <- scientific_names_mammals_e[i,"genus"]
  species.i <- scientific_names_mammals_e[i,"species"]
  gbif_mammals_e <- gbif(genus.i, species=species.i, geo=TRUE, sp=FALSE, 
                       removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
  gbif_mammals_e <- gbif_mammals_e[,c("species","lat","lon","year")]
  t.e <-gbif_mammals_e[!is.na(gbif_mammals_e$lon),]
  all.mammals.e= rbind(all.mammals.e, t.e)
}

all.mammals.e.count <-as.data.frame(all.mammals.e %>% count(species))

occ_data_end <- all.mammals.e[,c("species","lat","lon")]
write.csv(occ_data_end, "occ_data_end.csv")

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/biomod_species_subset/occ/occ_records_endemic_species")
write.csv(all.mammals.e.count, "all.mammals.e.count.csv")


setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/candidate_species_dataset/biomod_species_subset/occ")
occ_data_endemic <- all.mammals.e[,c("species","lat","lon")]
write.csv(occ_data_endemic, "occ_data_endemic.csv")
write.csv(all.mammals.e, "all.mammals.e.csv")




?taxon

### TOKEN FOR ACCESSING IUCN API ###########################################
### This is only authorized to be used by people from MSU ##################
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
library(rredlist)

all_mammals_hab_e <- list()
for(i in scientific_names_e){
  habitat_mammals_e <- rl_habitats(i, key=token)
  all_mammals_hab_e[[i]] <-habitat_mammals_e
}

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/species_subset")
sink("all_mammals_hab_e.txt")
print(all_mammals_hab_e)
sink()

library(foreach)
rbindlist.v2 <- function(l)
{
  l <- l[lapply(l, class) == "list"]
  df <- foreach(element = l, .combine = bind_rows, .errorhandling = 'remove') %dopar%
    {df = unlist(element); df = as.data.frame(t(df)); rm(element); return(df)}
  rm(l)
  return(df)
}
all_mammals_hab_e_df <-rbindlist.v2(all_mammals_hab_e)
class()
write.csv(all_mammals_hab_e_df, "all.mammals_hab_e_df.csv")
