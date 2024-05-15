## phylogenetic signaling
library(phytools)
library(ape)
#load data
tree_file <- paste0(path, "data/Gentiana_cor.nex")
dist_file <- paste0(path, "data/Gent_completeModel_nat.csv")
tree <- read.nexus(tree_file)
if (!inherits(tree, "phylo")) {
  tree <- as.phylo(tree)
}
dist <- read.csv2(dist_file, header = TRUE)

# make a named vector out of dist$biome_0
dist_biome <- dist$state %>% as.numeric() %>% as.vector()
names(dist_biome) <- dist$taxa
if (length(dist_biome) != length(tree$tip.label)) {
  stop("Mismatch between the number of tips and the number of data points.")
}

# reorder the dist_biome vector to match the tree$tip.label order
dist_biome <- dist_biome[tree$tip.label]
str(dist_biome)
lambda <- phylosig(x = dist_biome, tree = tree, method = "lambda", test = TRUE) 
K <- phylosig(x = dist_biome, tree = tree, method = "K", test = TRUE, nsim = 1000)
