## `Model Building`

This folder contains all codes to build a ClaSSE model and to run an MCMC simulation on RevBayes. Within this directory, you will find Rev-files that contain scripts to run the phylogenetic models. Three different model were implemented for this study - Climate Biome Model (Gent_climbio.Rev), Function Biome Model (Gent_VegF.Rev) and the combined model that contains several files in the folder "complex_model". The latter is composed of a file capturing all anagenetic events and the other all cladogenetic events, which are then sourced in the main script named Gent_completeClaSSE_2.Rev.
Furthermore, the folder event_builder_functions holds useful functions to generate all possible combination of events for anagenesis and cladogenesis, written in R. 
