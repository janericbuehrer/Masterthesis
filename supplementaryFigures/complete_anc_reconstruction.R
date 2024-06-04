library(ggtree)
library(ggplot2)
library(RevGadgets)
library(gridExtra)
library(viridis)

### read in and process the ancestral states
path <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/"
result_file <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/output/completeModel_test/m3/comp_results.tree"
p_anc <- processAncStates(result_file, labels_as_numbers = TRUE)

### specify colours of states
ncolor <- length(p_anc@state_labels) # useful for color palettes
state_color <- setNames( viridis(n = ncolor, option = "D"), p_anc@state_labels)



### plot the ancestral states

#pie_states <- plotAncStatesPie(p_anc,
#                               # Include cladogenetic events
#                               cladogenetic = TRUE, 
#                               # Add text labels to the tip pie symbols
#                               tip_labels_states = TRUE,
#                               # Offset those text labels slightly
#                               tip_labels_states_offset = .08,
#                               # Pass in your named and ordered color vector
#                               pie_colors = state_color, 
#                               # Offset the tip labels to make room for tip pies
#                               tip_labels_offset = .2, 
#                               # Move tip pies right slightly 
#                               tip_pie_nudge_x = .07,
#                               # Change the size of node and tip pies  
#                               tip_pie_size = 0.6,
#                               node_pie_size = 1.0) +
#  # Move the legend 
#  theme(legend.position = c(0.1, 0.75))
#
map_states <- plotAncStatesMAP(p_anc,
                               cladogenetic = TRUE,
                               timeline = TRUE, # prints a timeline in Ma beneath the tree
                               tip_labels_states = TRUE,
                               tree_layout = "rectangular",
                               node_color = state_color,
                               node_labels_as = "state",
                               state_transparency = 0.75,
                               tip_labels_offset = 0.8,
                               tip_labels_size = 2.5, 
                               # increase tip states symbol size
                               tip_states_size = 3) +
  # adjust legend position and remove color guide
  theme(legend.position = c(0.2, 0.87)) 

# save plots together
#pdf(paste0(path, "complete_anc_reconstruction.pdf"), width = 20, height = 20)
#
#grid.arrange(pie_states,map_states, ncol = 2)
#
#dev.off()                        

# save plots separately
fig_name <- "MAP_anc_reconstruction_complete" 
ggsave(paste0(path, fig_name, ".pdf"), map_states, width= 16.6, height = 23.4, units = "in")
map_states

fig_name <- "pie_anc_reconstruction_complete"
ggsave(paste0(path, fig_name, ".pdf"), pie_states, width=20, height=30)
