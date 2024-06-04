#########################
### Sample iterations ###
#########################

## make a function out of the sampling process
sample_iterations <- function(dat, n_samples, burnin = FALSE) {
  # Set seed for reproducibility
  set.seed(8123)
  # If burnin is TRUE, remove the first 25% of iterations
  if (burnin) {
    dat <- dat[-(1:round(length(dat$iteration) * 0.25))]
  }
  
  # Get the total number of iterations
  total_iterations <- length(dat$iteration)
  
  # Sample n_samples iterations
  sampled_indices <- sample(total_iterations, n_samples)
  
  # Extract sampled iterations from results
  sampled_json <- dat$stoch_map[sampled_indices]
  
  return(sampled_json)
}



############################
### Get events from JSON ###
############################

### create function to retrieve conservatism along the phylogeny####

## loop over all iterations (not part of function)
## loop over every branch in the iteration:
## iterate over the events list of each branch
## if "events" array is empty. conservatism_counter +1 and store endState in vector conserved_states
## if cladoEvent == TRUE and isChange == TRUE, relaxed_conservatism_counter +1 and store previousState and newState in a list relaxed_conserved_states
## if cladoEvent == TRUE and isChange == FALSE, conservatism_counter +1 and store newState in vector conserved_states

## function
get_events_fromJSON <- function(iteration_data) {
  transition_startStates <- c()
  transition_endStates <- c()
  conserved_states <- c()
  relaxed_conserved_states <- list()
  conserved_times <- c()
  relaxed_conserved_times <- c()
  transition_times <- c()
  conservatism_counter <- 0
  relaxed_conservatism_counter <- 0
  transition_counter <- 0
  undefined_event <- 0
  
  # Loop over branches
  for (branch in 1:length(iteration_data)) {
    # Check if events list is empty
    if (length(iteration_data[[branch]]$events) == 0) {
      conservatism_counter <- conservatism_counter + 1
      conserved_states <- c(conserved_states, iteration_data[[branch]]$endState[[1]])
      conserved_times <- c(conserved_times, iteration_data[[branch]]$startTime[[1]], iteration_data[[branch]]$duration[[1]])
    } else {
      # Loop over events in the branch
      for (i in 1:length(iteration_data[[branch]]$events)) {
        event <- iteration_data[[branch]]$events[[i]]
        
        if ("cladoEvent" %in% names(event)) {
          if (event$isChange == FALSE) {
            if (event$newState[[1]] %in% c(0, 1, 3)) { ## exclude multiple biome occupancy states(!= 0, 1, 3) from conservatism counter
              conservatism_counter <- conservatism_counter + 1
              conserved_states <- c(conserved_states, event$newState[[1]])
              conserved_times <- c(conserved_times, event$time[[1]]) ## "time" in sublist "event" represents the start time of the event. If it is a nodeEvent then the first entry of time is equal to startTime of the branch.
            } else {
                undefined_event <- undefined_event + 1
            }
          } else {
            relaxed_conservatism_counter <- relaxed_conservatism_counter + 1
            relaxed_conserved_states[[length(relaxed_conserved_states) + 1]] <- c(event$previousState[[1]], event$newState[[1]])
            relaxed_conserved_times <- c(relaxed_conserved_times, event$time[[1]])
          }
        } else {
          ## make sure that the event is a range expansion and not a range contraction (extinction in one biome)
          if (event$previousState[[1]] < event$newState[[1]]) {
            transition_counter <- transition_counter + 1
            transition_startStates <- c(transition_startStates, event$previousState[[1]])
            transition_endStates <- c(transition_endStates, event$newState[[1]])
            transition_times <- c(transition_times, event$time[[1]])
          }
        }
      }
    }
  }
  
  # Return the results for the current iteration
  return(list(
    conserved_states = conserved_states,
    relaxed_conserved_states = relaxed_conserved_states,
    transition_startStates = transition_startStates,
    transition_endStates = transition_endStates,
    conserved_times = conserved_times,
    relaxed_conserved_times = relaxed_conserved_times,
    transition_times = transition_times,
    conservatism_counter = conservatism_counter,
    relaxed_conservatism_counter = relaxed_conservatism_counter,
    transition_counter = transition_counter,
    undefined_event = undefined_event
  ))
}


##################################
### summarise conserved states ###
##################################

# Function to count conserved states per iteration

mySummary_state_conserved <- function(df, vegf = TRUE) {
  
  # stasis events per biome
  conservatism_df <- sapply(df, '[', seq(max(sapply(df, length))))
  conservatism_df <- as.data.frame(conservatism_df)
  colnames(conservatism_df) <- paste0("it_", 1:length(conservatism_df))
  
  state_counts <- lapply(conservatism_df,table) # Count occurrences per column using table
  state_counts_df <- do.call(rbind, state_counts) # Convert list to data frame; iteration as rows, state as columns
  
  if(vegf){
    
    colnames(state_counts_df) <- c("closed", "open", "both")
    summary_stats <- state_counts_df %>% as.data.frame() %>%
      summarise(mean_closed = mean(closed),
                mean_open = mean(open),
                mean_both = mean(both),
                sd_closed = sd(closed),
                sd_open = sd(open),
                sd_both = sd(both))
  } else {
    
    colnames(state_counts_df) <- c("tm", "bs", "tm_bs", "ta", "tm_ta", "bs_ta", "all")
    summary_stats <- state_counts_df %>% as.data.frame() %>%
      summarise(mean_t = mean(tm),
                mean_bs = mean(bs),
                mean_t_bs = mean(tm_bs),
                mean_ta = mean(ta),
                mean_t_ta = mean(tm_ta),
                mean_bs_ta = mean(bs_ta),
                mean_all = mean(all),
                sd_t = sd(tm),
                sd_bs = sd(bs),
                sd_t_bs = sd(tm_bs),
                sd_ta = sd(ta),
                sd_t_ta = sd(tm_ta),
                sd_bs_ta = sd(bs_ta),
                sd_all = sd(all))
  }
  
  return(summary_stats)
}



################################
### summarise states changes ###
################################
mySummary_state_change <- function(df) {
  # Get events counts per iteration
  transition_table <- lapply(df, function(x){table(x$transition_startStates, x$transition_endStates)})
  # initialise lists to store mean and sd of counts per cell of transition table
  cell_counts <- list()
  cell_mean <- list()
  cell_sd <- list()
  # Get row and column names from the first iteration (all iterations should have the same names)
  colnames_endState <- colnames(transition_table[[1]])
  rownames_startState <- rownames(transition_table[[1]])
  
  for (row in 1:nrow(transition_table[[1]])) { # each iteration should show the same number of rows and columns, use first iteration.
    for (col in 1:ncol(transition_table[[1]])) {
      # get counts per cell for all iterations
      cell_counts_per_iteration <- sapply(transition_table, function(x) x[row, col]) 
      # skip col that resulted in zeros
      if (all(cell_counts_per_iteration == 0)) {next
      } else {
        # store mean and sd of counts per cell (transition from row to column biome)
        cell_counts[[paste(rownames_startState[row], colnames_endState[col], sep = "_")]] <- cell_counts_per_iteration
        cell_mean[[paste(rownames_startState[row], colnames_endState[col], sep = "_")]] <- mean(cell_counts_per_iteration, na.rm = TRUE)
        cell_sd[[paste(rownames_startState[row], colnames_endState[col], sep = "_")]] <- sd(cell_counts_per_iteration, na.rm = TRUE)
        }
    }
  }
  return(list(transition_counts = cell_counts, transition_mean = cell_mean, transition_sd = cell_sd))
}


################################
### plot densities of events ###
################################
plot_rate_densities <- function(tree_file_path, results_counter_df, xlim = c(0, 0.5), ylim = c(0, NA)) {
  
  # Load required libraries
  library(ape)
  library(ggplot2)
  library(viridis)
  # Read the tree and calculate total branch lengths
  index_tree <- read.nexus(tree_file_path)
  lengths_all <- sum(index_tree$edge.length)
  
  # Calculate rates of event counter per total branch length
  rate_counter <- results_counter_df / lengths_all
  
  # Create the density plot
  ggplot(rate_counter) +
    geom_density(aes(x = conservatism_counter, fill = "Conservatism"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
    geom_density(aes(x = relaxed_conservatism_counter, fill = "Relaxed Conservatism"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
    geom_density(aes(x = transition_counter, fill = "Shifts"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(linewidth = 1, colour = "black"),
      text = element_text(size = 16, colour = "black"),
      axis.ticks = element_line(linewidth = 1, colour = "black"),
      legend.key = element_blank()
    ) +
    xlim(xlim) +  # Adjust xlim based on data range
    ylim(ylim) +  # Automatically adjust ylim
    labs(x = "", y = "density", fill = "", title = "Events along the phylogeny") +  # Label for legend
    #scale_fill_continuous(viridis)
    scale_fill_manual(values = c("Conservatism" = "red", "Relaxed Conservatism" = "grey", "Shifts" = "blue"))
}


#################################
### plot densities of changes ###
#################################
plot_change_rate_densities <- function(tree_file_path, state_change_counts_df, vegf = TRUE, no_generalists = FALSE, xlim = c(0, 0.5), ylim = c(0, NA)) {
  
  # Load required libraries
  library(ape)
  library(ggplot2)
  library(viridisLite)
  
  # Read the tree and calculate total branch lengths
  index_tree <- read.nexus(tree_file_path)
  lengths_all <- sum(index_tree$edge.length)
  
  # Calculate rates of event counter per total branch length
  rate_state_change <- state_change_counts_df / lengths_all
  
  if (vegf){
    colnames(rate_state_change) <- c("into_open", "into_closed")
    
    ggplot(data = rate_state_change)+
      # Use Reduce to sequentially add layers
      geom_density(aes(x = into_open, fill = "0_2"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
      geom_density(aes(x = into_closed, fill = "1_2"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(linewidth = 1, colour = "black"),
        text = element_text(size = 16, colour = "black"),
        axis.ticks = element_line(linewidth = 1, colour = "black"),
        legend.key = element_blank()
      ) +
      xlim(c(0, 0.2)) +  # Adjust xlim based on data range
      ylim(c(0, NA)) +  # Automatically adjust ylim
      labs(x = "", y = "density", fill = "") +  # Label for legend
      scale_fill_manual(values = c("0_2" = "grey", "1_2" = "green"), labels = c("0_2" = "closed to open", "1_2" = "open to closed"))
  } else {
    
    colnames(rate_state_change) <- c("tm_bs", "tm_ta", "bs_tm", "bs_ta", "bs.tm_ta", "ta_tm", "ta_bs",  "ta.tm_bs", "ta.bs_tm")
    
    if (no_generalists) {
      rate_state_change <- rate_state_change %>% select(!contains("."))
      ggplot(data = rate_state_change)+
       
        geom_density(aes(x = t_bs, fill = "tm -> bs"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = t_ta, fill = "tm -> ta"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = bs_t, fill = "bs -> tm"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = bs_ta, fill = "bs -> ta"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta_t, fill = "ta -> tm"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta_bs, fill = "ta -> bs"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(linewidth = 1, colour = "black"),
          text = element_text(size = 16, colour = "black"),
          axis.ticks = element_line(linewidth = 1, colour = "black"),
          legend.key = element_blank()
        ) +
        xlim(xlim) +  # Adjust xlim based on data range
        ylim(ylim) +  # Automatically adjust ylim
        labs(x = "", y = "density", fill = "") +  # Label for legend
        scale_fill_manual(values = c("tm -> bs" = "grey", "tm -> ta" = "blue", "bs -> tm" = "red", "bs -> ta" = "green", "ta -> tm" = "purple", "ta -> bs" = "orange"))
    } else {
      
    
      ggplot(data = rate_state_change)+
    
        geom_density(aes(x = t_bs, fill = "tm -> bs"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = t_ta, fill = "tm -> ta"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = bs_t, fill = "bs -> tm"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = bs_ta, fill = "bs -> ta"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = bs.t_ta, fill = "bs & tm -> ta"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta_t, fill = "ta -> tm"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta_bs, fill = "ta -> bs"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta.t_bs, fill = "ta & tm -> bs"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        geom_density(aes(x = ta.bs_t, fill = "ta & bs -> tm"), alpha = 0.5, weight = 0, adjust = 2, linetype = 0) +
        
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(linewidth = 1, colour = "black"),
          text = element_text(size = 16, colour = "black"),
          axis.ticks = element_line(linewidth = 1, colour = "black"),
          legend.key = element_blank()
        ) +
        xlim(xlim) +  # Adjust xlim based on data range
        ylim(ylim) +  # Automatically adjust ylim
        labs(x = "", y = "density", fill = "") +  # Label for legend
        scale_fill_manual(values = c("tm -> bs" = "grey", "tm -> ta" = "blue", "bs -> tm" = "red", "bs -> ta" = "green", 
                                     "bs & tm -> ta" = "yellow", "ta -> tm" = "purple", "ta -> bs" = "orange", "ta & tm -> bs"= "pink", "ta & bs -> tm" = "brown"))
    }
  }
  
}