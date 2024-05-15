library(RevGadgets)
library(tidyverse)
library(ggplot2)
library(ggtree)
library(grid)
library(gridExtra)
library(ape)
library(gt)

setwd("~/ETH Master/Master Thesis/Gentiana")

# specify the input file
mod_climbio_file <- "output/Vol2/m8/biome_model_8.log"
mod_vegf_file <- "output/VegF/m6/model.log"
complex_file <- "output/completeModel_test/m3/comp_model.log"

# read the input file
trace_climbio <- readTrace(paths = mod_climbio_file)
trace_climbio <- removeBurnin(trace_climbio, 0.25)

trace_vegf <- readTrace(paths = mod_vegf_file)
trace_vegf <- removeBurnin(trace_vegf, 0.25)

trace_complex <- readTrace(paths = complex_file)
trace_complex <- removeBurnin(trace_complex, 0.25)

############### assess convergence with coda #############
library(coda)
trace_climbio_MCMC <- as.mcmc(trace_climbio[[1]]) # ESS values
effectiveSize(trace_climbio_MCMC)
traceplot(trace_climbio_MCMC)

trace_vegf_MCMC <- as.mcmc(trace_vegf[[1]]) # ESS values
effectiveSize(trace_vegf_MCMC)
traceplot(trace_vegf_MCMC)

trace_complex_MCMC <- as.mcmc(trace_complex[[1]]) # ESS values
effectiveSize(trace_complex_MCMC)
traceplot(trace_complex_MCMC)
###############################################################

##############################
# anagenetic_dispersal rates #
##############################

## clim bio model ##
name_anagen_climbio <- trace_climbio[[1]] %>% select(contains("biome_shift")) %>% colnames
#name <- name_anagen_climbio[-(grep("4", name_anagen_climbio))] # remove outlier
#name <- name[-(grep("total_biome_shifts", name))] # remove total_biome_shifts
#name <- name[c(1,2,3,5,4,6)] # rearrange the order
name_anagen_climbio_plot <- c("TM to BS", "BS to TM", "BS to TA", "TA to BS", "BS & TM to TA", "TA & BS to TM")
colnames(trace_climbio[[1]])[c(5,7,8,11,9,13)] <- name_anagen_climbio_plot
## plot
# color palette used here: paletteer_d("ggthemes::Tableau_20")
p_anagen_climbio <- plotTrace(trace = trace_climbio, vars = name_anagen_climbio_plot, color = c("TM to BS" = "#8CD17D" , "BS to TM" = "#59A14F", "BS to TA" = "#F1CE63", 
                                                                                                "TA to BS" = "#B6992D", "BS & TM to TA" = "#86BCB6" , "TA & BS to TM" = "#499894"))


## vegf model ##
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

# Access the ggplot object and customize it
# Customize x and y limits
p_anagen_climbio_gg <- p_anagen_climbio[[1]] + 
  xlim(0, 3.5) +  # Set your desired x-axis limits
  ylim(0, 15) +  # Set your desired y-axis limits
  labs(title = NULL, color = "Biome Shifts")
p_anagen_vegf_gg <- p_anagen_vegf[[1]] + 
  xlim(0, 3.5) +  # Set your desired x-axis limits
  ylim(0, 15) +  # Set your desired y-axis limits
  labs(title = NULL, color = "Funtion Shifts")
p_anagen_complex_gg <- p_anagen_complex[[1]] + 
  xlim(0, 3) +  # Set your desired x-axis limits
  ylim(0, 18) +  # Set your desired y-axis limits
  labs(title = NULL, color = "Function and Biome Shifts")

# Arrange the plots into a single layout
combined_plot <- grid.arrange(
  p_anagen_climbio_gg,
  p_anagen_vegf_gg,
  p_anagen_complex_gg,
  ncol = 1  # Arrange plots in one column
)

# Save the arranged plots to a PDF file
ggsave("trace_biome_shift_combined_rowwise_3.pdf", combined_plot, width = 10, height = 15)



#### summerize the trace of all anagenetic_dispersal rates ####
## bind the summary statistics of all anagenetic_dispersal rates into a single dataframe
#climbio
summary_trace <- summarizeTrace(trace = trace_climbio, vars =  name_anagen_climbio) # summary of the trace 1, meaning the first chain. If I would have multiple runs 
                                                                            # then it would provide me with the summary statistics of trace1, 2, 3,... and all traces combined
sum_trace_climbio_df  <- do.call(rbind, lapply(summary_trace, function(x) x$trace_1))
sum_trace_climbio_df <- sum_trace_climbio_df  %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter") 
sum_trace_climbio_df$model <- "Climatic biome model" # for grouping purposes
sum_trace_climbio_df <- sum_trace_climbio_df %>% arrange(desc(mean))

#vegf
summary_trace <- summarizeTrace(trace = trace_vegf, vars = name_anagen_vegf) 
sum_trace_vegf_df <- do.call(rbind, lapply(summary_trace, function(x) x$trace_1))
sum_trace_vegf_df <- sum_trace_vegf_df %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter")
sum_trace_vegf_df$model <- "Vegetation formation model" # for grouping purposes
sum_trace_vegf_df <- sum_trace_vegf_df %>% arrange(desc(mean))

## combine the summary statistics of both models
sum_trace_df <- rbind(sum_trace_climbio_df, sum_trace_vegf_df) %>% gt(groupname_col = "model", row_group_as_column = TRUE) %>% gt::tab_header(title = "Summary of biome shift rates")

## save table
gtsave(sum_trace_df, file = "plots/summary_biome_shift_rates_desc.tex")

####################
# speciation rates #
####################
## climbio model
summary_trace_climbio <- summarizeTrace(trace = trace_climbio, vars =  c("speciation_conserved_tm", "speciation_conserved_bs", "speciation_conserved_ta", 
                                                                         "total_sp_strict_conservatism", 
                                                                         "speciation_within_biome", "speciation_between_biome", 
                                                                         "total_sp_relaxed_conservatism", 
                                                                         "total_speciation"))
sum_trace_climbio_df  <- do.call(rbind, lapply(summary_trace_climbio, function(x) x$trace_1))
sum_trace_climbio_df <- sum_trace_climbio_df  %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter")
sum_trace_climbio_df$model <- "Climatic biome model" # for grouping purposes

## vegf model
summary_trace_vegf <- summarizeTrace(trace = trace_vegf, vars =  c("speciation_conserved_closed", "speciation_conserved_open", 
                                                                   "total_sp_strict_conservatism",
                                                                   "speciation_within_biome_closed", "speciation_within_biome_open", "speciation_between_biome",
                                                                   "total_sp_relaxed_conservatism", 
                                                                   "total_speciation"))
sum_trace_vegf_df  <- do.call(rbind, lapply(summary_trace_vegf, function(x) x$trace_1))
sum_trace_vegf_df <- sum_trace_vegf_df  %>% round(., digits = 3) %>% as.data.frame %>% rownames_to_column(var = "parameter")
sum_trace_vegf_df$model <- "Vegetation formation model" # for grouping purposes

## combine the summary statistics of both models
sum_trace_df <- rbind(sum_trace_climbio_df, sum_trace_vegf_df) %>% gt(groupname_col = "model", row_group_as_column = TRUE) %>% gt::tab_header(title = "Summary of speciation rates")
sum_trace_df

## save table
gtsave(sum_trace_df, file = "plots/summary_speciation_rates.tex")

