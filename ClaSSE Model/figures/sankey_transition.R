setwd("~/ETH Master/Master Thesis/Gentiana")
# specify the input file
mod_climbio_file <- "output/Vol2/m8/biome_model_8.log"


# read the input file
trace_climbio <- readTrace(paths = mod_climbio_file)
trace_climbio <- removeBurnin(trace_climbio, 0.25)

## clim bio model
name_anagen_climbio <- trace_climbio[[1]] %>% select(contains("biome_shift")) %>% colnames

# plot the trace of all anagenetic_dispersal rates distribution
#name <- name_anagen_climbio[-(grep("biome_shift_4_6", name_anagen_climbio))] # remove outlier

#### summerize the trace of all anagenetic_dispersal rates ####
## bind the summary statistics of all anagenetic_dispersal rates into a single dataframe
summary_trace <- summarizeTrace(trace = trace_climbio, vars =  name_anagen_climbio) # summary of the trace 1, meaning the first chain. If I would have multiple runs 
# then it would provide me with the summary statistics of trace1, 2, 3,... and all traces combined
sum_trace_climbio_df  <- do.call(rbind, lapply(summary_trace, function(x) x$trace_1))

str(sum_trace_climbio_df)
sum_trace_climbio_df <- as.data.frame(sum_trace_climbio_df)
sum_trace_climbio_df$parameter <- rownames(sum_trace_climbio_df)

# Extract the start and end states from the parameter names
sum_trace_climbio_df  <- 
  sum_trace_climbio_df %>%
  mutate(
    startState = str_extract(parameter, pattern = "_\\d+"),
    endState = str_extract(parameter, pattern = "_\\d+$")
  )

sum_trace_climbio_df$startState <- c("temperate/montane (TM)",
                                     "temperate/montane (TM)", 
                                     "boreal/subalpine (BS)", 
                                     "boreal/subalpine (BS)",
                                     "TM & BS", 
                                     "tundra/alpine (TA)", 
                                     "tundra/alpine (TA)",
                                     "TM & TA",
                                     "BS & TA"
)
sum_trace_climbio_df$endState <- c("TM & BS", 
                                   "TM & TA", 
                                   "TM & BS", 
                                   "BS & TA", 
                                   "all biomes",
                                   "TM & TA",
                                   "BS & TA",
                                   "all biomes",
                                   "all biomes"
)

sum_trace_climbio_df <- sum_trace_climbio_df %>%
  filter(rownames(sum_trace_climbio_df) != "total_biome_shifts")

str(sum_trace_climbio_df)

# sankey chart 
library("networkD3")

# define source target and value
links <- sum_trace_climbio_df[, c("startState", "endState", "median")]
colnames(links) <- c("source", "target", "value")
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# change colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=TRUE,  colourScale=ColourScal, nodeWidth=40, fontSize=20, nodePadding=20)
p
