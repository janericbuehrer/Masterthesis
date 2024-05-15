###Functions for printing and copying

# anagenetic rate matrix print
myprint_anagen <- function(x){
  for (i in 1:length(x)) {
    phrase <- as.character(x[[i]])
    cat(phrase, "\n")
  }
}

# clado events print
myprint_cladogen <- function(x, start){
  for (i in 1:length(x)) {
    phrase <- paste0("clado_events[", i + start, "] = [", x[[i]][[1]],"," , x[[i]][[2]],"," , x[[i]][[3]], "]")
    cat(phrase, "\n")
  }
}



#######################################
#clado events for anagenetic dispersal#
#######################################

int_states <- c(00001, 00010, 00011, 00100, 00101, 00110, 00111, 01000, 
                01001, 01010, 01011, 01100, 01101, 01110, 01111, 10000, 10001, 
                10010, 10011, 10100, 10101, 10110, 10111, 11000, 11001, 11010, 
                11011, 11100, 11101, 11110, 11111)
#int_states <- int_states[1:7]

# Initialize empty list to store anagenetic events

event_names <- list()
event_matrix <- list()

# initialize the subtraction of the states
operator <- c(1, 10, 100, 1000, 10000)
valid_i <- which(int_states[length(int_states)] >= operator)

for (j in 1: length(int_states)) {
  ancestor_state <- int_states[j]
  ancestor_state_index <- j-1
  #cat(j, "\n")

  for (i in operator[valid_i]){
    if (ancestor_state == i){
      cat(i, "skip cause i is the same as anc\n")
      
      next
    } else{
      d <- ancestor_state + i
      if (grepl("^[01]+$", as.character(d)) && d > 0) {
        d_index <- which(int_states == d)
        cat(d, "\n")
        anagen <- paste0("biome_shift_", ancestor_state_index, "_", d_index-1)
        event_names[[length(event_names) + 1]] <- list(anagen)
        event_matrix[[length(event_matrix) + 1]] <- list(paste0("r", "[", ancestor_state_index +1, "][", as.integer(d_index), "] := ", anagen))
      } else {
        next
      }
      
    }
  }
}
do.call(rbind, event_names)
do.call(rbind, event_matrix)


## print to copy to RevBayes
myprint_anagen(event_names)
myprint_anagen(event_matrix)

#### anagenetic_dispersal matrix EXTINCTION RATES #####

states <- c(
  '00001', '00010', '00011', '00100', '00101', '00110', '00111',
  '01000', '01001', '01010', '01011', '01100', '01101', '01110', '01111',
  '10000', '10001', '10010', '10011', '10100', '10101', '10110', '10111',
  '11000', '11001', '11010', '11011', '11100', '11101', '11110', '11111'
)
#states <- states[1:7]
# Filter states with more than one region occupation
valid_states <- which(sapply(strsplit(states, ""), function(x) sum(as.integer(x)) > 1))

# Initialize an empty list to store extinction events
extinction_events <- list()
event_ex_matrix <- list()
#r[5][2] := extinction_rates[3]
#r[5][3] := extinction_rates[2]

for (j in valid_states) {
  ancestor_state <- int_states[j]
  ancestor_state_index <- j-1
  cat("--",ancestor_state_index," | " , ancestor_state, "--", "\n")
  for (i in operator[valid_i]){
    if (i > ancestor_state) {
      next
    } else{
      d <- ancestor_state - i
      
      if (grepl("^[01]+$", as.character(d)) && d > 0) {
        ext_rate <- which(int_states == i) -1
        d_index <- which(int_states == d) -1
        cat(d, "\n")
        extinct <- paste0("extinction_rates[", ext_rate + 1, "]")
        extinction_events[[length(extinction_events) + 1]] <- list(paste0("r", "[", ancestor_state_index +1, "][", as.integer(d_index +1), "] := ", extinct))
      } else {
        next
      }
    }
  }
}

myprint_anagen(extinction_events)


##############################
####### Clado events #########
##############################

# Define the states (without Null state)
states <- c(
  '00001', '00010', '00011', '00100', '00101', '00110', '00111',
  '01000', '01001', '01010', '01011', '01100', '01101', '01110', '01111',
  '10000', '10001', '10010', '10011', '10100', '10101', '10110', '10111',
  '11000', '11001', '11010', '11011', '11100', '11101', '11110', '11111'
)
int_states <- c(00001, 00010, 00011, 00100, 00101, 00110, 00111, 01000, 
                01001, 01010, 01011, 01100, 01101, 01110, 01111, 10000, 10001, 
                10010, 10011, 10100, 10101, 10110, 10111, 11000, 11001, 11010, 
                11011, 11100, 11101, 11110, 11111)
#states <- states[1:7]
#int_states <- int_states[1:7]


##### clado events for wide sympatry #######

# Initialize an empty list to store cladogenetic events
clado_events_wide <- list()

# Generate cladogenetic events with state indices
for (i in 1:(length(states) - 1)) {
  if (sum(as.integer(strsplit(states[i], "")[[1]])) == 1) {
    ancestor_state_index <- i -1
    daughter1_state_index <- i -1
    daughter2_state_index <- i -1
    index <- 
      clado_events_wide[[length(clado_events_wide) + 1]] <- list(ancestor_state_index, daughter1_state_index, daughter2_state_index)
  } else {
    next
  }
}

## print to copy to RevBayes
myprint_cladogen(clado_events_wide, 0)


# Filter states with more than one region occupation
valid_states <- valid_states <- which(sapply(strsplit(states, ""), function(x) sum(as.integer(x)) > 1))

##### subset sympatry #######

# This algorithm creates all possible combinations of subset sympatry events. How it is defined here: 
# d represents a daughter inheriting a subset of the ancestor range, d is limited to a single range!!!!

clado_events_subset <- list()

# Generate all possible cladogenetic events for allopatric speciation
for (j in valid_states) {
  ancestor_state <- int_states[j]
  ancestor_state_index <- j-1
  for (i in int_states) {  # Loop only for states before ancestor
    d <- ancestor_state - i
    # Check if difference (d) is binary and positive (excluding 0)
    if (grepl("^[01]+$", as.character(d)) && d > 0) {
      # Check if only one bit in d is 1 (isolated region)
      if (sum(as.integer(strsplit(as.character(d), "")[[1]])) == 1) {
        #i_index <- which(int_states == i) -1
        d_index <- which(int_states == d) -1
        clado_events_subset[[length(clado_events_subset) + 1]] <- list(ancestor_state_index, ancestor_state_index, d_index)
        clado_events_subset[[length(clado_events_subset) + 1]] <- list(ancestor_state_index, d_index, ancestor_state_index)
      }
    }
  }
}

clado_events_subset <- unique(clado_events_subset)

## print to copy to RevBayes
myprint_cladogen(clado_events_subset, 5)

###### allopatry ########

# Initialize an empty list to store cladogenetic events for allopatric speciation
clado_events_allopatric <- list()

# Generate all possible cladogenetic events for allopatric speciation
for (j in valid_states) {
  ancestor_state <- int_states[j]
  ancestor_state_index <- j-1
  for (i in int_states) {  # Loop only for states before ancestor
    d <- ancestor_state - i
    # Check if difference (d) is binary and positive (excluding 0)
    if (grepl("^[01]+$", as.character(d)) && d > 0) {
      # Check if only one bit in d is 1 (isolated region)
      if (sum(as.integer(strsplit(as.character(d), "")[[1]])) == 1) {
        i_index <- which(int_states == i) -1
        d_index <- which(int_states == d) -1
        clado_events_allopatric[[length(clado_events_allopatric) + 1]] <- list(ancestor_state_index, i_index, d_index)
        clado_events_allopatric[[length(clado_events_allopatric) + 1]] <- list(ancestor_state_index, d_index, i_index)
      }
    }
  }
}

clado_events_allopatric <- unique(clado_events_allopatric)

## print to copy to RevBayes
myprint_cladogen(clado_events_allopatric, start = 155)



#Convert indices in clado_events_allopatric to bit format
clado_events_allopatric_bit <- lapply(clado_events_subset, function(event) {
  ancestor_state_bit <- states[event[[1]]+1]
  daughter1_state_bit <- states[event[[2]]+1]
  daughter2_state_bit <- states[event[[3]]+1]
  
  list(ancestor_state_bit, daughter1_state_bit, daughter2_state_bit)
})

# Print or use clado_events_allopatric_bit as needed
do.call(rbind, clado_events_allopatric_bit)

