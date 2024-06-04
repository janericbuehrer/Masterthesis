library(RevGadgets)
library(tidyverse)
library(ggplot2)
library(ggtree)
library(grid)
library(gridExtra)
library(ape)
library(gt)
library(ggfun)

path <- "~/ETH Master/Master Thesis/Gentiana"

# specify the input file
mod_climbio_file <- paste0(path, "/output/Vol2/m8/biome_model_8.log")
mod_vegf_file <- paste0(path, "/output/VegF/m6/model.log")

# read the input file
trace_climbio <- readTrace(paths = mod_climbio_file)
trace_climbio <- removeBurnin(trace_climbio, 0.25)

trace_vegf <- readTrace(paths = mod_vegf_file)
trace_vegf <- removeBurnin(trace_vegf, 0.25)


## climate biome model ##
name_anagen_climbio <- trace_climbio[[1]] %>% select(contains("biome_shift")) %>% colnames
#name <- name_anagen_climbio[-(grep("4", name_anagen_climbio))] # remove outlier
#name <- name[-(grep("total_biome_shifts", name))] # remove total_biome_shifts
#name <- name[c(1,2,3,5,4,6)] # rearrange the order
name_anagen_climbio_plot <- c("TM to BS", "TM to TA", "BS to TM", "BS to TA", "BS & TM to TA", "TA to TM", "TA to BS", "TA & TM to BS", "TA & BS to TM")
colnames(trace_climbio[[1]])[5:13] <- name_anagen_climbio_plot
## plot
# color palette used here: paletteer_d("ggthemes::Tableau_20")
p_conservatism_tm <- plotTrace(trace = trace_climbio, vars = c("speciation_conserved_tm", "TM to BS", "TM to TA"), color = c("speciation_conserved_tm" = "#4E79A7FF", "TM to BS" = "#59A14F" , "TM to TA" = "#8CD17D"))
p_conservatism_bs <- plotTrace(trace = trace_climbio, vars = c("speciation_conserved_bs", "BS to TM", "BS to TA"), color = c("speciation_conserved_bs" = "#4E79A7FF", "BS to TM" = "#F28E2BFF" , "BS to TA" = "#FFBE7DFF"))
p_conservatism_ta <- plotTrace(trace = trace_climbio, vars = c("speciation_conserved_ta", "TA to BS", "TA to TM"), color = c("speciation_conserved_ta" = "#4E79A7FF", "TA to BS" = "#F1CE63FF" , "TA to TM" = "#B6992DFF"))

## Habitat (vegf) model ##
name_anagen_vegf_plot <- c("closed to open", "open to closed") # change names
colnames(trace_vegf[[1]])[c(5, 6)] <- name_anagen_vegf_plot
## plot
# color palette used here: paletteer_d("ggthemes::Miller_Stone")
p_conservatism_open <- plotTrace(trace = trace_vegf, vars = c("speciation_conserved_open", "open to closed"), color = c("speciation_conserved_open" =  "#4E79A7FF", "open to closed" = "#FF9D9AFF")) 
p_conservatism_closed <- plotTrace(trace = trace_vegf, vars = c("speciation_conserved_closed", "closed to open"), color = c("speciation_conserved_closed" =  "#4E79A7FF", "closed to open" = "#E15759FF"))

# Access the ggplot object and customize it
# Customize x and y limits
p_tm_gg <- p_conservatism_tm[[1]] + 
  xlim(0, 2.5) +  # Set your desired x-axis limits
  ylim(0, 15) +  # Set your desired y-axis limits
  labs(title = NULL, color = "TM Biome")+
  scale_color_manual(
    values = c("speciation_conserved_tm" = "#4E79A7FF", 
               "TM to BS" = "#59A14F", 
               "TM to TA" = "#8CD17D"),
    labels = c("speciation_conserved_tm" = "Conserving TM", 
               "TM to BS" = "TM to BS", 
               "TM to TA" = "TM to TA")
  )

p_bs_gg <- p_conservatism_bs[[1]] + 
  xlim(0, 2.5) +  # Set your desired x-axis limits
  ylim(0, 15) +  # Set your desired y-axis limits
  labs(title = NULL, color = "BS Biome")+
  scale_color_manual(
    values = c("speciation_conserved_bs" = "#4E79A7FF", 
               "BS to TM" = "#F28E2BFF", 
               "BS to TA" = "#FFBE7DFF"),
    labels = c("speciation_conserved_bs" = "Conserving BS", 
               "BS to TM" = "BS to TM", 
               "BS to TA" = "BS to TA")
  )
p_ta_gg <- p_conservatism_ta[[1]] + 
  xlim(0, 2.5) +  # Set your desired x-axis limits
  ylim(0, 15) +  # Set your desired y-axis limits
  labs(title = NULL, color = "TA Biome")+
  scale_color_manual(
    values = c("speciation_conserved_ta" = "#4E79A7FF", 
               "TA to BS" = "#F1CE63FF", 
               "TA to TM" = "#B6992DFF"),
    labels = c("speciation_conserved_ta" = "Conserving TA", 
               "TA to BS" = "TA to BS", 
               "TA to TM" = "TA to TM")
  )
p_open_gg <- p_conservatism_open[[1]] + 
  xlim(0, 3.5) +  # Set your desired x-axis limits
  ylim(0, 16) +  # Set your desired y-axis limits
  labs(title = NULL, color = "Open Habitats") + 
  scale_color_manual(values = c("speciation_conserved_open" = "#4E79A7FF",  
                                "open to closed" = "#FF9D9AFF"), 
                     labels = c("speciation_conserved_open" = "Conserving Open",  
                                "open to closed" = "Open to Closed")
  )

p_closed_gg <- p_conservatism_closed[[1]] +
  xlim(0, 3.5) +  # Set your desired x-axis limits
  ylim(0, 16) +  # Set your desired y-axis limits
  labs(title = NULL, color = "Closed Habitats")+
  scale_color_manual(
    values = c("speciation_conserved_closed" = "#4E79A7FF", 
               "closed to open" = "#E15759FF"),
    labels = c("speciation_conserved_closed" = "Conserving Closed", 
               "closed to open" = "Closed to Open")
  )

### customize the plots
# Function to customize plots
customize_plot <- function(plot, xlim, ylim = c(0, 16), title = NULL) {
  plot + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(linewidth = 1, colour = "black"),
      axis.line.y = element_line(linewidth = 1, colour = "black"),
      text = element_text(size = 16, colour = "black"),
      axis.ticks = element_line(linewidth = 1, colour = "black"),
      legend.position = "none",
      legend.key = element_blank()
    ) +
    labs(title = title)+
    xlim(xlim) +
    ylim(ylim)
}


# Customize your existing plots
p_tm_gg <- customize_plot(p_conservatism_tm[[1]], xlim = c(0, 0.6), title = "TM Biome")
p_bs_gg <- customize_plot(p_conservatism_bs[[1]], xlim = c(0, 3), title = "BS Biome")
p_ta_gg <- customize_plot(p_conservatism_ta[[1]], ylim = c(0, 17), xlim = c(0, 0.6), title = "TA Biome")
p_open_gg <- customize_plot(p_conservatism_open[[1]], xlim = c(0, 0.6), title = "Open Habitats")
p_closed_gg <- customize_plot(p_conservatism_closed[[1]], xlim = c(0, 3), title = "Closed Habitats")


# Dummy plot for creating a unified legend with geom_tile and custom order
legend_plot <- ggplot() + 
  geom_tile(aes(x = 1, y = 1, fill = factor("Biome/Habitat stasis", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("TM to BS", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("TM to TA", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("BS to TM", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("BS to TA", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("TA to BS", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("TA to TM", 
                                            levels = c("Biome/Habitat stasiss", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("open to closed", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  geom_tile(aes(x = 1, y = 1, fill = factor("closed to open", 
                                            levels = c("Biome/Habitat stasis", "TM to BS", "TM to TA", "BS to TM", "BS to TA", "TA to BS", "TA to TM", "open to closed", "closed to open"))), width = 0.1, height = 0.1) +
  scale_fill_manual(values = c("Biome/Habitat stasis" = "#4E79A7FF",
                               "TM to BS" = "#59A14F",
                               "TM to TA" = "#8CD17D",
                               "BS to TM" = "#F28E2BFF",
                               "BS to TA" = "#FFBE7DFF",
                               "TA to BS" = "#F1CE63FF",
                               "TA to TM" = "#B6992DFF",
                               "open to closed" = "#FF9D9AFF",
                               "closed to open" = "#E15759FF")) +
  labs(fill = "Biome Conservatism vs. Shifts") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 6 * 2), # Increase legend text size by 50%
        legend.title = element_text(size = 8 * 1.75), # Increase legend title size by 50%
        legend.key.size = unit(1.2 * 2, "lines")) + # Increase legend key size by 50%
  guides(fill = guide_legend(ncol = 2)) # Set the number of columns in the legend to 2


# Extract the unified legend
legend <- get_legend(legend_plot)


# Arrange the plots into a single layout
combined_plot <- grid.arrange(
  arrangeGrob(p_tm_gg, p_bs_gg, p_ta_gg, ncol = 3, nrow = 1),
  arrangeGrob( p_open_gg, p_closed_gg, legend, ncol = 3, nrow = 1),
  nrow = 2 )# Arrange plots in one column 

# Save the arranged plots to a PDF file
ggsave(paste0(path, "/plots/conservatism_hypo/traces_columnwise_normalized_w_title.pdf"), combined_plot, width = 15, height = 10)


