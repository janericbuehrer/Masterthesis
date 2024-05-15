setwd("C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana")
library(tidyverse)
library(ape)
tree <- read.nexus("data/Gentiana_cor.nex")
plot(tree)
print(tree) # gives information on the tree file

## Get the number of tips (leaves) in the tree
#n_tips <- length(tree$tip.label)
#print(paste("Number of tips:", n_tips))


## correct spelling mistakes of tip labels in the nexus file
library(rWCVP)


tip_names <- tree$tip.label # Extract tip names

## make structure similar to the wcvp databank
tip_names <- gsub("_", " ", tip_names) 
df_names <- data.frame(scientific_name = tip_names)

## check matches
matches <- wcvp_match_names(df_names,
                 name_col = "scientific_name")

fuzzy_matches <- matches %>%
  filter(str_detect(match_type, "Fuzzy")) %>%
  mutate(
    keep = case_when( #set up a keep column
      match_similarity < 0.9 ~ NA_real_, # fill with blank for dissimilar names
     # wcvp_author_edit_distance == 0 ~ 1, # fill with 1 if authors identical
      match_edit_distance == 1 ~ 1, # fill with 1 if only one letter different
    )
  )
##### 
#unnecessary
fuzzy_matches %>%
  filter(match_edit_distance == 2,
         !multiple_matches,
         match_similarity > 0.9
  )
#how many did this resolve?
table(fuzzy_matches$keep, useNA = "always")

tibble(fuzzy_matches$scientific_name, fuzzy_matches$wcvp_name, fuzzy_matches$keep, fuzzy_matches$match_edit_distance)
######
## correct the fuzzy matches and put them back into the phylogeny
tip_names_corrected <- matches$wcvp_name
tip_names_corrected <- tip_names_corrected[!duplicated(tip_names_corrected)] # Exclude redundant entries while preserving order
tip_names_corrected <- gsub(" ", "_", tip_names_corrected)
tree[["tip.label"]] <- tip_names_corrected


write.nexus(tree, file = "Gentiana_cor.nex") # Write the tree to a Nexus file