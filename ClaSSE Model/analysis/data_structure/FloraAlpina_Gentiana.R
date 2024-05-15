setwd("~/ETH MASTER/Master Thesis/Gentiana/")

## Wenna Gentiana dataset
gentiana <- read.csv("Gentiana_biorange_biorange_check_rev7.csv", header = TRUE)
head(gentiana)


## Flora Alpina column selection
#install.packages("tidyverse")
library(tidyverse)
### install.packages("devtools")
# devtools::install_github("r-lib/conflicted")
library(conflicted) # filter() and lag() is used in base-R stats and in dplyr, loading this library will always make sure 
                    #to give me an error if functions are ambiguous and will ask me to specify with plyr::filter() 
conflicts_prefer(dplyr::filter)

###df_gentiana
## load flora alpina
load("../DataCollection/FloraAlpina.RData")
names(dat)

df_gentiana <- dat %>%
  filter(naming_genus == "Gentiana")
df_gentiana <- df_gentiana %>%
  select(contains("naming") | contains("habitat") | contains("alt_")) ##species names, habitat, altitude... for distribution add... | contains("dist_") ; if i want to use the commented out codes use as name df_gentiana_temp for this row!!!
#df_geogr_temp <- df_gentiana[,185:194] ## alpine systems
#df_gentiana <- cbind(df_gentiana_temp, df_geogr_temp)



## filter out 
#species_code_temp <- df_gentiana$naming_species_code # preserve
df_gentiana <- df_gentiana %>% select(-ends_with("code")) #filter out
#df_gentiana <- cbind(species_code_temp, df_gentiana)

df_gentiana <- df_gentiana %>% select(!(contains(c("habitat_epiphyt","habitat_parasite"))))


df_gentiana <- df_gentiana %>%
  mutate(taxa = paste(naming_genus, naming_species, sep = "_")) %>%
  select(-naming_genus, -naming_species, -naming_family, -naming_hybrid, -naming_subspecies_co)

df_gentiana <- df_gentiana %>%
  select(order(names(.))) ## reorder in alphabetical order

## transform all characters to binary 1 & 0 (& 2)
df_gentiana_num <- df_gentiana %>%
  select(-c(tail(names(.), 2))) %>% names(.)
df_gentiana <- df_gentiana %>%
    mutate(across(all_of(df_gentiana_num), ~case_when(
      . == "common" ~ "1",
      . == "absent" ~ "0",
      . == "sometimes" ~ "0", ## remove the "sometimes" category
      TRUE ~ .  # If none of the above conditions are met, keep the original value
    )))
 

df_gentiana[, df_gentiana_num] <- df_gentiana[, df_gentiana_num] %>%
  mutate_all(as.integer)


## create new categories
# merge nival & alpine to arctic, subalpine to boreal, montain & colline to temperate
df_gentiana <- df_gentiana %>%
  mutate(tundra_alpine = ifelse(rowSums(select(., alt_nival, alt_alpine)  == 1) > 0, 1, 0))

df_gentiana$boreal_subalpine <- df_gentiana$alt_subalpine

df_gentiana <- df_gentiana %>%
  mutate(temperate = ifelse(rowSums(select(., alt_montane, alt_colline)  == 1) > 0, 1, 0))



#make here habitat_one :eight open and convert name of nine to closed 

habit_open <- df_gentiana %>%
  select(contains("habitat_")) %>%
  select(-"habitat_nine")

df_gentiana$closed <- df_gentiana$habitat_nine

df_gentiana <- df_gentiana %>%
  mutate(open = ifelse(rowSums(habit_open == 1) > 0, 1, 0))


##Final table
df_gentiana <- df_gentiana[, 16:ncol(df_gentiana)]

gentiana_FA <- df_gentiana[which(df_gentiana$taxa %in% gentiana$taxa), ]
gentiana_minusFA <- gentiana %>% filter(!(taxa %in% df_gentiana$taxa)) %>% select(taxa, A, a)

#####
### Compare what Wenna send me with the Flora Alpina dataset
head(gentiana$taxa)
tail(gentiana$taxa) 

head(df_gentiana$naming_species)
tail(df_gentiana$naming_species) ## the dataset gentiana and df_gentiana are arranged by alphabetical order

gentiana <- merge(gentiana, df_gentiana_temp, by = "taxa", all.x = TRUE)
sum(gentiana$taxa %in% df_gentiana$taxa) # 23 matched taxa, 27 if I include the subspecies

## species that are not from Flora Alpina
gentiana_taxa_minusFA <- gentiana[is.na(gentiana$arctic), "taxa"]
(gentiana_taxa_minusFA <- gsub("_", " ", gentiana_taxa_minusFA))


#####
##remove all temporary variables from the global environment
# List all objects in the global environment
all_objects <- ls()

# Filter objects with names ending in "_temp"
objects_to_remove <- all_objects[grep("_temp$", all_objects)]
objects_to_remove <- c("df_gentiana", "spp.list2", "spp.list3", "spp.list4", "habit_open", "id_names", "selC", "last.warning", "spp.split", "ttt", "spplist")
## remove all values that are not a dataframe
objects_to_remove <- all_objects[c("df_gentiana", "dat", "gentiana_FA", "gentiana_minusFA")]
objects_to_remove <- all_objects[!all_objects %in% c("df_gentiana", "dat", "gentiana_FA", "gentiana_minusFA")]
# Remove the selected objects
rm(list = objects_to_remove)

