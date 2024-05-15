## install needed packages for bonsai
#install.packages(packages=c("coda","tools","RColorBrewer","entropy","xtable"), dependencies=TRUE)

## load the devtools package
#library(devtools)

## install bonsai
#install_github("mikeryanmay/bonsai/package")

library(bonsai)

## specify the input folder
input_dir  <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/bonsai/m7"

## specify the output folder (here it is the same as the input)
output_dir <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/bonsai/m7"

## name the bonsai project
name <- "Gentiana Biome-only ClaSSE m7"

## build the bonsai project
project <- bonsai(name, output_dir, input_dir)
## compile the markdown
project$makeRMarkdown()
