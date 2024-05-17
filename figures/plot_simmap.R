library(plotrix)
library(phytools)
library(viridis)

# set file path
setwd("~/ETH Master/Master Thesis/Gentiana")
climbio_character_file = "output/Vol2/m8/biome_character_8.tree" # "output/Vol2/m8/biome_shift_prob_8.tree"
vegf_character_file = "output/VegF/m6/character.tree"

files <- c(vegf_character_file, climbio_character_file)

# set pdf names
pdf_name <- c("output/vegf_simmap_6_woLEGEND.pdf", "output/climbio_simmap_8_woLEGEND.pdf")
title <- c("Vegetation formation evolution in Gentiana", "Climatic biome evolution in Gentiana")
write_pdf = FALSE
p <- 2

if (write_pdf) {
  pdf(pdf_name[p], width = 6, height = 10)
}

sim2 = read.simmap(file=files[p], format="phylip")

colors = vector()
for (i in 1:length( sim2$maps ) ) { 
  colors = c(colors, names(sim2$maps[[i]]) )
}
colors = sort(as.numeric(unique(colors)))

#cols = setNames( viridis(n = length(colors), option = "D"), colors)
if(p ==1){
  cols <- setNames(c("#993404", "#FEC44F", "#EC7041"), colors)
} else{
  cols <- setNames(c("#66FF00",  "#003300", "#006600", "#FFFF99", "#999933", "#33CCFF"), colors)
}

cols <- viridis(n=30)
#names(cols) <- colors

plotSimmap(sim2, cols, fsize=0.3, lwd=1, split.vertical=TRUE, ftype="i")
add.simmap.legend(colors=cols,prompt=FALSE,x=0,y=-0.5,
               vertical=FALSE, fsize = 1, cex = 0.8)
title(main= title[p],font.main=2,
    line=-1)

if (write_pdf) {
  dev.off()
}


### Biome conservatism #####
# read the input file
mod_biome_file <- "output/Vol2/m6/biome_stoch_6.log"
  "C:/Users/janer/Downloads/CLASSE_output(1)/CLASSE_output/habitat_smap_clean.log"
  
mod_vegf_file <- "output/VegF/m2/stoch.log"
vegf_simmap = read.simmap(file=vegf_character_file, format="phylip")
climbio_simmap = read.simmap(file=climbio_character_file, format="phylip")
info <- read.table(mod_biome_file)
info <- info[,-1]

summary(vegf_simmap, plot=FALSE)
summary(climbio_simmap, plot=FALSE)




## extras ####
# add legend
x = 0
y = 15

leg = names(cols)
colors = cols
y = y - 0:(length(leg) - 1)
x = rep(x, length(y))
text(x + 0.0005, y, leg, pos=4, cex=0.75)

mapply(draw.circle, x=x, y=y, col=colors, MoreArgs = list(nv=200, radius=0.001, border="white"))
