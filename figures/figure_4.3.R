library(RevGadgets)
library(tidyverse)
library(ggplot2)
library(ggtree)
library(grid)
library(gridExtra)
library(ape)
library(gt)

path <- "C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/"

# specify the input file
mod_climbio_file <- paste0(path, "output/Vol2/m8/biome_model_8.log")
mod_vegf_file <- paste0(path,"output/VegF/m6/model.log")
complex_file <- paste0(path,"output/completeModel_test/m3/comp_model.log")

# read in the input file
trace_climbio <- readTrace(paths = mod_climbio_file)
trace_climbio <- removeBurnin(trace_climbio, 0.25)

trace_vegf <- readTrace(paths = mod_vegf_file)
trace_vegf <- removeBurnin(trace_vegf, 0.25)

trace_complex <- readTrace(paths = complex_file)
trace_complex <- removeBurnin(trace_complex, 0.25)

############### assess convergence with coda #############
# library(coda)
# trace_climbio_MCMC <- as.mcmc(trace_climbio[[1]]) # ESS values
# effectiveSize(trace_climbio_MCMC)
# traceplot(trace_climbio_MCMC)
# 
# trace_vegf_MCMC <- as.mcmc(trace_vegf[[1]]) # ESS values
# effectiveSize(trace_vegf_MCMC)
# traceplot(trace_vegf_MCMC)
# 
# trace_complex_MCMC <- as.mcmc(trace_complex[[1]]) # ESS values
# effectiveSize(trace_complex_MCMC)
# traceplot(trace_complex_MCMC)
###############################################################

##############################
# anagenetic_dispersal rates #
##############################

## climate biome model ##
name_anagen_climbio <- trace_climbio[[1]] %>% select(contains("biome_shift")) %>% colnames

name_anagen_climbio_plot <- c("TM to BS", "BS to TM", "BS to TA", "TA to BS", "BS & TM to TA", "TA & BS to TM")
colnames(trace_climbio[[1]])[c(5,7,8,11,9,13)] <- name_anagen_climbio_plot # reorder and label by the biome shift names

## plot
# color palette used here: paletteer_d("ggthemes::Tableau_20")
p_anagen_climbio <- plotTrace(trace = trace_climbio, vars = name_anagen_climbio_plot, color = c("TM to BS" = "#8CD17D" , "BS to TM" = "#59A14F", "BS to TA" = "#F1CE63", 
                                                                                                "TA to BS" = "#B6992D", "BS & TM to TA" = "#86BCB6" , "TA & BS to TM" = "#499894"))

## Habitat (vegf) model ##
name_anagen_vegf_plot <- c("closed to open", "open to closed") # change names
colnames(trace_vegf[[1]])[c(5, 6)] <- name_anagen_vegf_plot
## plot
# color palette used here: paletteer_d("ggthemes::Miller_Stone")
p_anagen_vegf <- plotTrace(trace = trace_vegf, vars = name_anagen_vegf_plot, color = c("closed to open" = "#FBB04E" , "open to closed" = "#B66353"))



## complex model ##

name_anagen_complex <-  c("between_biome_gain_in_open", "between_biome_gain_in_closed", "within_open_gain_in_biome", "within_closed_gain_in_biome", "within_biome_gain_in_open", "within_biome_gain_in_closed")
colnames(trace_complex[[1]])[c(5, 6, 7, 8, 40, 41)] <- name_anagen_complex
## plot
p_anagen_complex <- plotTrace(trace = trace_complex, vars = name_anagen_complex, color = c("between_biome_gain_in_open" =  "#86BCB6", "between_biome_gain_in_closed" = "#499894", "within_open_gain_in_biome" = "#8CD17D", 
                                                                                           "within_closed_gain_in_biome" = "#59A14F", "within_biome_gain_in_open" = "#FBB04E" , "within_biome_gain_in_closed" = "#B66353"))
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
      legend.position = "right",
      legend.key = element_blank()
    ) +
    labs(title = NULL, color = title)+
    xlim(xlim) +
    ylim(ylim) +
    guides(color = guide_legend(override.aes = list(linetype = "solid", size = 5))) # Customize the linetype of legend
}

# Access the ggplot object and customize it
# Customize x and y limits and more
p_anagen_climbio_gg <- customize_plot(p_anagen_climbio[[1]], xlim = c(0, 3.5), ylim = c(0, 15), title = "Biome Shifts") # Set your desired x-axis and y-axis limits
p_anagen_vegf_gg <- customize_plot(p_anagen_vegf[[1]], xlim = c(0, 3.5), ylim = c(0, 16), title = "Habitat Shifts")
p_anagen_complex_gg <- customize_plot(p_anagen_complex[[1]], xlim = c(0, 3), ylim = c(0, 18), title = "Habitat and Biome Shifts")

# Arrange the plots into a single layout
combined_plot <- grid.arrange(
  p_anagen_climbio_gg,
  p_anagen_vegf_gg,
  p_anagen_complex_gg,
  ncol = 1  # Arrange plots in one column
)

# Save the arranged plots to a PDF file
ggsave(paste0(path, "plots/transition/trace_biome_shift_costumized.pdf"), combined_plot, width = 10, height = 15)


###### create summary table ####
#### summerize the trace of all anagenetic_dispersal rates ####
## bind the summary statistics of all anagenetic_dispersal rates into a single dataframe
#biome model
summary_trace <- summarizeTrace(trace = trace_climbio, vars =  name_anagen_climbio_plot) # summary of the trace 1, meaning the first chain. If I would have multiple runs 
# then it would provide me with the summary statistics of trace1, 2, 3,... and all traces combined
sum_trace_climbio_df  <- do.call(rbind, lapply(summary_trace, function(x) x$trace_1))
sum_trace_climbio_df <- sum_trace_climbio_df  %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter") 
sum_trace_climbio_df$model <- "Climatic biome model" # for grouping purposes
sum_trace_climbio_df <- sum_trace_climbio_df %>% arrange(desc(mean))

#Habitat model (vegf)
summary_trace <- summarizeTrace(trace = trace_vegf, vars = name_anagen_vegf_plot) 
sum_trace_vegf_df <- do.call(rbind, lapply(summary_trace, function(x) x$trace_1))
sum_trace_vegf_df <- sum_trace_vegf_df %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter")
sum_trace_vegf_df$model <- "Vegetation formation model" # for grouping purposes
sum_trace_vegf_df <- sum_trace_vegf_df %>% arrange(desc(mean))

## complex model
summary_trace_complex <- summarizeTrace(trace = trace_complex, vars = name_anagen_complex)
sum_trace_complex_df <- do.call(rbind, lapply(summary_trace_complex, function(x) x$trace_1))
sum_trace_complex_df <- sum_trace_complex_df %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter")
sum_trace_complex_df$model <- "Complex model" # for grouping purposes
sum_trace_complex_df <- sum_trace_complex_df %>% arrange(desc(mean))

## combine the summary statistics of both models
sum_trace_df <- rbind(sum_trace_climbio_df, sum_trace_vegf_df, sum_trace_complex_df) %>% gt(groupname_col = "model", row_group_as_column = TRUE) %>% gt::tab_header(title = "Summary of biome shift rates")




# Save the table
gtsave(sum_trace_df, file = file_path)


