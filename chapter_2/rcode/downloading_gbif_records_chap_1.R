# ADD SUMMARY AND MORE INFORMATION
# fill in your gbif.org credentials 
user <- "bgerstner90" # your gbif.org username 
pwd <- "hermione1" # your gbif.org password
email <- "bgerstner90@gmail.com" # your email 

library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_
# Mammals with validated models on BioModelos
mammal_list <- "/Users/bethgerstner/Desktop/test_species_full.csv"
# match the names 
gbif_taxon_keys_mammals <- 
  readr::read_csv(mammal_list) %>% 
  pull("scientific_names") %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(class == "Mammalia") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys
# !!very important here to use "type=in"!!
# make the download request to GBIF 

occ_download(
  pred_in("taxonKey", gbif_taxon_keys_mammals),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2000),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)





