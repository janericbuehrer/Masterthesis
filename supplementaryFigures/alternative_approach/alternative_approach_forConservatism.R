### analyzing events.tsv output
#load libraries
library(tidyverse)
library(readr)
library(RevGadgets)
library(purrr)
library(ggpubr)

path <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/"
setwd(path)

# First convert output to Json with python script and randomly sample iterations with the R-file sample_from_JSON
# Then read in the JSON file
library(jsonlite)
library(rjson)

json_file <- paste0(path, "output/Vol2/m8/biome_stoch_8_corrected_sampled_3.json") ## "output/VegF/m6/stoch_corrected_sampled_500.json"
json_data <- read_json(json_file)


# Now you can access the elements of the 'stoch_map' column as lists
# For example, to access the 'startState' of the first element:

json_data$stoch_map[[1]]$'1'$events[[1]]$newState

# INFO: structure of json data
#stoch_map[[1]] --> iteration 1
#stoch_map[[1]]$'1' --> list of node 1 elements: startState, endState, etc.
#stoch_map[[1]]$'1'$startState --> start state of node 1 in iteration 1
# events represents another layer of list, therefore repeat the same process to access the elements of events
#stoch_map[[1]]$'1'$events[[1]]$newState --> new state of after event 1 at node 1 in iteration 1

  
# Source functions used for the analysis of the stochastic character MAP output as JSON-file
source("C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/scripts/R/get_events_fromJSON.R")

############################
##### get events counts ####
############################

# Initialize results lists
results_state_conserved <- list()
results_state_changes <- list()
results_state_relaxed_conserved <- list()
results_counter <- list()

# Loop over iterations
for (iteration in 1:length(json_data$stoch_map)) {
  # Process the current iteration data
  iteration_data <- json_data$stoch_map[[iteration]]
  
  iteration_results <- get_events_fromJSON(iteration_data)
  
  # Store results for the current iteration
  results_state_conserved[[iteration]] <- list(conserved_states = iteration_results$conserved_states, 
                                               conserved_times = iteration_results$conserved_times)
  
  results_state_changes[[iteration]] <- list(
    transition_startStates = iteration_results$transition_startStates,
    transition_endStates = iteration_results$transition_endStates, 
    transition_times = iteration_results$transition_times
  )
  results_state_relaxed_conserved[[iteration]] <- list(relaxed_states = iteration_results$relaxed_conserved_states, 
                                                       relaxed_times = iteration_results$relaxed_conserved_times)
  
  results_counter[[iteration]] <- list(
    conservatism_counter = iteration_results$conservatism_counter,
    relaxed_conservatism_counter = iteration_results$relaxed_conservatism_counter,
    transition_counter = iteration_results$transition_counter,
    conserved_times = iteration_results$conserved_times,
    undefined_event = iteration_results$undefined_event
  )
}

# Remove last iteration variables
rm("iteration_data", "iteration")

#####################################
### summarise results of counter ####
#####################################

## quantifying the number of conserved, relaxed conserved and transition events

# Combine results and convert to dataframe, then unnest the columns to be able to calculate the mean and standard deviation of each counter
results_counter_df <- 
  do.call(rbind, results_counter) %>% 
  as.data.frame() %>% 
  unnest(cols = c(conservatism_counter, relaxed_conservatism_counter, transition_counter, conserved_times, undefined_event))

# remove undefined events
results_counter_df <- results_counter_df %>% select(-undefined_event)

# Calculate mean and standard deviation of each counter
summary_counters <- 
  results_counter_df %>% 
  summarise(mean_conservatism = mean(conservatism_counter),
            mean_relaxed_conservatism = mean(relaxed_conservatism_counter),
            mean_transition = mean(transition_counter),
            sd_conservatism = sd(conservatism_counter),
            sd_relaxed_conservatism = sd(relaxed_conservatism_counter),
            sd_transition = sd(transition_counter)
            )

summary_counters_medianIQR <- 
  results_counter_df %>% 
  summarise(n = n(),
            median_conservatism = median(conservatism_counter),
            median_relaxed_conservatism = median(relaxed_conservatism_counter),
            median_transition = median(transition_counter),
            iqr_conservatism = IQR(conservatism_counter),
            iqr_relaxed_conservatism = IQR(relaxed_conservatism_counter),
            iqr_transition = IQR(transition_counter)
            )


# transform the summary table to have 3 rows with the 3 categories of events and the n, median and iqr of each category
summary_counters_medianIQR <- summary_counters_medianIQR %>% gather(key = "event_type", value = "value", -n) %>% 
  separate(event_type, into = c("event_type", "measure"), sep = "_") %>% 
  spread(key = measure, value = value) %>% 
  select(-n) %>%
  t() %>% 
  as.data.frame()
# use the first row as column names
summary_counters_medianIQR <- summary_counters_medianIQR %>% rownames_to_column(var = "variable")
colnames(summary_counters_medianIQR) <- summary_counters_medianIQR[1,]
summary_counters_medianIQR <- summary_counters_medianIQR[-1,]
summary_counters_medianIQR$n <- length(results_counter_df)
summary_counters_medianIQR$event_type <- as.factor(summary_counters_medianIQR$event_type)

# reshape counter data for wilcoxon test and boxplot
library(reshape2)
melt_results_boxplot <- melt(results_counter_df, value.name = "counts", variable.name = "event_type")
melt_results_boxplot$event_type <- as.factor(melt_results_boxplot$event_type)

# statisitical test for the differences between the three types of events
library(rstatix)
#library(coin)
stat.test <- melt_results_boxplot %>%
  rstatix::wilcox_test(counts ~ event_type, paired = FALSE) %>%
  add_significance()
stat.test
eff <- melt_results_boxplot  %>% wilcox_effsize(counts ~ event_type, paired = FALSE)
stat_c <- cbind(stat.test[, 2:8], eff$effsize, eff$magnitude) %>% as.data.frame()
stat_c$model <- "Climatic biome model"
stat_v$model <- "Vegetation formation model"
stat.test <- stat.test %>% 
  rstatix::add_xy_position(x = "group")

# combine the summary statistics of both models
library(gt)
stats <- rbind(stat_c, stat_v)  %>%
   mutate(group1 = gsub(pattern = "_counter", replacement = "", x = group1))  %>% 
    mutate(group2 =gsub(pattern = "_counter", replacement = "", x = group2))%>% 
  gt(groupname_col = "model", row_group_as_column = TRUE)
## save table
gtsave(stats, file = "plots/stats_conservarism.tex")

####################
### plot results ###
####################

### differences between the three types of events ###
library(ggplot2)
library(ggpubr)

#### boxplot of event counts ###
boxplot <- ggplot(melt_results_boxplot, aes(x = event_type, y = counts)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.25) +
  labs(y = "Counts",  x = "")+
  stat_pvalue_manual(stat.test, tip.length = 0.03, y.position = "y.position", xmin = "group1",
                     xmax = "group2", step.increase = 0.05) +
  labs(subtitle = get_test_label(stat.test, detailed = F))+
  scale_x_discrete(labels = c("strict conservatism", "relaxed conservatism", "biome shifts"))+
  theme_classic() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15))


#### Rate densities of events ####

p <- plot_rate_densities(tree_file_path = "~/ETH Master/Master Thesis/Gentiana/data/Gentiana_cor.nex", results_counter_df = results_counter_df, xlim = c(0,0.5), ylim = c(0, NA))
# save the plot
ggsave("Conserved_vs_shift_density_climbio.pdf", plot = p, path = paste0(path, "plots/conservatism_hypo/"), width = 12, height = 10, units = "in") ## dimensions are wrong...
ggsave("bxp_event_counts_vegf.pdf", plot = boxplot, path = paste0(path, "plots/conservatism_hypo/"), width = 12, height = 10, units = "in")


#############################################################
### summarise the results of the state changes and stasis ###
#############################################################

# transition events per biome state
# structure of the list
results_state_changes[[1]]$transition_startStates[1] # example of the first transition event, output startState
results_state_changes[[1]]$transition_endStates[1] # example of the first transition event, output endState

# summarise the number of transitions across iterations
summary_state_change <- mySummary_state_change(results_state_changes)


# recombine the list so that the rows are the sublist descriptions and store all table[1,i] into the first list element and so on
state_change_counts_df <- 
  do.call(rbind, summary_state_change$transition_counts) %>% 
  t() %>%
  as.data.frame()
# from summary_state_change$transition_counts combine all counts into class ta and name it into_ta and from_ta
colnames(state_change_counts_df) <- c("t_bs", "t_ta", "bs_t", "bs_ta", "bs.t_ta", "ta_t", "ta_bs",  "ta.t_bs", "ta.bs_t")
state_change_counts_into_ta <- state_change_counts_df %>%
  select(contains("_ta")) %>%
  summarise("into_ta" = sum(select(.)))

# plot biome shift rates
p_shifts <- plot_change_rate_densities(tree_file_path = "~/ETH Master/Master Thesis/Gentiana/data/Gentiana_cor.nex", state_change_counts_df = state_change_counts_df, vegf = TRUE, no_generalists = FALSE, xlim = c(0,0.17), ylim = c(0, NA))
p_shifts