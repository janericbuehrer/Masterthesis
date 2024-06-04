path <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/"
setwd(path)

library(ggtree)
library(ggplot2)
library(RevGadgets)
library(gridExtra)

### read in and process the ancestral states
result_file_climbio <- "output/Vol2/m8/states_results_8.tree"
result_file_vegf <- "output/VegF/m6/states_results.tree"

#p_anc <- processAncStates(result_file, labels_as_numbers = TRUE)           # two ways of labelling the states: labels_as_numbers creates factors and in the right order. 


### !plot models one by one!
# Habitat (vegf)                                                                            # I am able to label the states here, but i don't know how to extract them out of this datatype.
p_anc <- processAncStates(result_file_vegf, state_labels = c("0" = "closed", 
                                                        "1" = "open", 
                                                        "2" = "both"))
# climate biome                                                                            # However, I don't know how to label them on a later stage as i am not able to extract them out of this datatype.
p_anc <- processAncStates(result_file_climbio, state_labels = c("0" = "temperate (TM)", 
                                                        "1" = "boreal/subalpine (BS)", 
                                                        "2" = "BS & TM", 
                                                        "3"= "tundra/alpine (TA)", 
                                                        "4" = "TA & TM", 
                                                        "5" = "TA & BS", 
                                                        "6" = "all biomes"))
### specify colours of states

# habitat model
library("ggsci")
library("scales")
show_col(pal_jco("default")(10))
state_color <- c(pal_jco("default")(2),"#A73030FF") # for the three states
state_color <- state_color[c(1,3,2)]
names(state_color) <- p_anc@state_labels
#state_color <- c("#EC7041", "#993404", "#FEC44F") # for the three states

# biome model
state_color <- c("#33CCFF", "#003300", "#006600", "#999933", "violet", "#66FF00", "#FFFF99")
names(state_color) <- p_anc@state_labels
state_color <- state_color[c(4,6,2,7,5,3,1)]


### plot the ancestral states

#new_legend_labels <- c("no biome", "temperate (T)", "boreal/subalpine (BS)", "tundra/alpine (TA)", "T & BS", "BS & TA", "all biomes")

pie_states <- plotAncStatesPie(p_anc,
                               # Include cladogenetic events
                               cladogenetic = TRUE, 
                               # Add text labels to the tip pie symbols
                               tip_labels_states = FALSE,
                               # Offset those text labels slightly
                               tip_labels_states_offset = .05,
                               # Pass in your named and ordered color vector
                               pie_colors = state_color, 
                               # Offset the tip labels to make room for tip pies
                               tip_labels_offset = .3, 
                               # Move tip pies right slightly 
                               tip_pie_nudge_x = .07,
                               # Change the size of node and tip pies  
                               tip_pie_size = 0.25,
                               node_pie_size = 0.6) +
  # Move the legend 
  theme(legend.position = c(0.1, 0.75))

map_states <- plotAncStatesMAP(p_anc,
                         cladogenetic = TRUE,
                         timeline = FALSE, # prints a timeline in Ma beneath the tree
                         tree_layout = "rectangular",
                         node_color = state_color,
                         node_labels_as = "state_posterior",
                         state_transparency = 0.75,
                         tip_labels_offset = 0.2,
                         tip_labels_size = 2.5, 
                         # increase tip states symbol size
                         tip_states_size = 3) +
  # adjust legend position and remove color guide
  theme(legend.position = c(0.2, 0.87)) #+ 
  #guides(color = "none")
                                      
  
pdf(paste0(path, "plots/anc_reconstruction/pie_anc_states_vegf.pdf"), width = 8.3, height = 11.7)

grid.arrange(pie_states,map_states, ncol = 2)

dev.off()                        
                         

fig_name <- "anc_reconstruction_biome_map" 
ggsave(paste0(fig_name, ".pdf"), map_states, width= 16.6, height = 23.4, units = "in") # width = 16.6, height = 23.4
fig_name <- "anc_reconstruction_biome_pie" 
ggsave(paste0(fig_name, ".pdf"), pie_states, width= 16.6, height = 23.4, units = "in") # width = 16.6, height = 23.4

