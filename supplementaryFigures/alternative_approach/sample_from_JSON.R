# Load necessary libraries
library(jsonlite)
# Load data
path <- "/home/buehrerj/gentiana/output/vegf/m6/"
input_path <- paste0(path, "stoch_corrected.json")
output_path <- paste0(path, "stoch_corrected_sampled_")
json_data <- read_json(input_path)

#########################
### Sample iterations ###
#########################

## make a function out of the sampling process
sample_iterations <- function(dat, n_samples, burnin = FALSE) {
  # Set seed for reproducibility
  set.seed(8123)
  
  # Get the total number of iterations
  total_iterations <- length(dat$stoch_map)
  
  # If burnin is TRUE, remove the first 25% of iterations
  if (burnin) {
    dat$stoch_map <- dat$stoch_map[-(1:round(total_iterations * 0.25))]
    # Update total number of iterations
    total_iterations <- length(dat$stoch_map)
  } 
  
  
  
  # Sample n_samples iterations
  sampled_indices <- sample(total_iterations, n_samples)
  
  # Extract sampled iterations from results
  sampled_json <- list()
  sampled_json$stoch_map <- dat$stoch_map[sampled_indices]
  
  return(sampled_json)
}

sampled_json_3 <- sample_iterations(dat = json_data, n_samples = 3, burnin = TRUE)
sampled_json_500 <- sample_iterations(dat = json_data, n_samples = 500, burnin = TRUE)

# write the sampled json to a file
write_json(sampled_json_3, path = paste0(output_path, "3.json"))
write_json(sampled_json_500, path = paste0(output_path, "500.json"))
