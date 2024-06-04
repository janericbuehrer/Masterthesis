####### Biome occupancy in numbers ####
## script for RESULTS introduction
# This script is used to calculate the proportion of species that are generalists and specialists, and to determine the most occupied biome.
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lessR)

#### what is the proportion of biome specialist (species occupying only one biome) 
### to biome generalist (species with at least 2 biome occupation)?

## climBio
climBio <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/data/Gent_biome_0.csv", header = T)
climBio <- climBio %>% mutate(sp_bio_rel = ifelse(biome_0 == 0 | biome_0 == 1 | biome_0 == 3, "specialist", "generalist"))
climBio$sp_bio_rel <- as.factor(climBio$sp_bio_rel)
climBio <- climBio %>% 
              mutate(labels = ifelse(biome_0 == "0", "temperate/montane (TM)", 
                                  ifelse(biome_0 == "1", "boreal/subalpine (BS)", 
                                         ifelse(biome_0 == "3", "tundra/alpine (TA)", 
                                                ifelse(biome_0 == "4", "TA & TM", 
                                                       ifelse(biome_0 == "5", "TA & BS", 
                                                              ifelse(biome_0 == "6", "all biomes", "BS & TM")))))))
climBio$labels <- factor(climBio$labels, levels = c("tundra/alpine (TA)", "boreal/subalpine (BS)","temperate/montane (TM)", "TA & BS", "BS & TM", "all biomes"))  ## labels[c(6,2,5,4,3,1)]

## bar chart

#color palette
state_color <- c("#33CCFF", "#003300", "#006600", "#999933", "#66FF00", "#FFFF99") ## TA & TM was excluded as it is not present in the data
state_color <- state_color[c(6,2,5,4,3,1)]
names(state_color) <- levels(climBio$labels)

# percentage to label the stacked bars
summary_climBio <- round(prop.table(table(climBio$labels)) * 100, 0)

climbio_bar <- 
  ggplot(climBio, aes(x = sp_bio_rel, fill = labels)) +
  geom_bar(stat = "count") +
  #geom_text(aes(label = labels, y= stat("count")), position = position_fill(vjust = 0.5)) + 
  labs(x = "", y = "Count", fill = "Biome States") +
  scale_fill_manual(values = state_color) +
  theme_minimal()

## pie chart
mydata <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/data/Gent_biome_bit.csv", header = T)
mybiome <- tibble(TA = sum(mydata$tundra_alpine), BS= sum(mydata$boreal_subalpine), TM= sum(mydata$temperate))
mybiome
#'The most occupied biome is the tundra/alpine, followed by the boreal/subalpine and the temperate biome.
#'Nevertheless, all three biomes are occupied almost evenly.

# transpose mybiome
mybiome <- as.data.frame(t(mybiome))
# plot mybiome as barplot, with rownames(mybiome) as x-axis and mybiome$V1 as y-axis
#barplot(mybiome$V1, names.arg = rownames(mybiome), col = "lightblue", main = "Biome occupation", ylab = "Number of species", xlab = "Biome")

state_color_pie <- state_color[1:3]
mybiome <- 
  mybiome %>% 
  mutate(perc = V1 / sum(V1)) %>%
  mutate(group = c("Tundra/Alpine", "Boreal/Subalpine", "Temperate/Montane")) %>%
  arrange(desc(perc)) %>%
  mutate(labels = scales::percent(perc))

state_color_pie <- state_color[1:3]
names(state_color_pie) <- mybiome$group
climbio_pie <- 
  ggplot(mybiome, aes(x = "", y = perc, fill = group)) +
    geom_col(color = "black") +
    geom_label(aes(label = labels), color = c("black", "white", "black"),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    guides(fill = guide_legend(title = "Biomes")) +
    scale_fill_manual(values = state_color_pie) +
    coord_polar(theta = "y") + 
    theme_void()


# Combine the plots using gridExtra
grid <- arrangeGrob(climbio_pie, climbio_bar, nrow = 2)

# Display the plot
grid.newpage()
plot(grid)

#'The proportion of species that are generalists is approx. 0.5, which indicates that there are as much generalist as specialist living today.
#'Moreover this suggests that the species are not only adapted to one specific biome, but can also live in other biomes.
#'Therefore my data indicates that these biome boarders are frequently crossed by those species, which is in line with the 
#'literature on mountain dwelling lineages (ref??).

## vegF
vegF <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/data/Gent_VegF_noNA_data_012.csv", header = T)
vegF <- vegF %>% mutate(sp_vegf_rel = ifelse(char_w.o_NA == 2, "generalist", "specialist"))
vegF$sp_vegf_rel <- as.factor(vegF$sp_vegf_rel)

vegF <- vegF %>% 
  mutate(labels = ifelse(char_w.o_NA == "0", "closed", 
                         ifelse(char_w.o_NA == "1", "open", "both"))) 

vegF$labels <- factor(vegF$labels, levels = c("closed", "open", "both"))
# color palette
library(viridis)
#colors <- c("#704040", "#ffa07a", "#c02942")
 
colors <- c("#993404", "#FEC44F", "#EC7041")


#colors <- viridis(n = 3, option = "D")
names(colors) <- levels(vegF$labels)
# percentage to label the stacked bars
summary_vegF <- round(prop.table(table(vegF$labels)) * 100, 0)

vegf_bar <- 
  ggplot(vegF, aes(x = sp_vegf_rel, fill = labels)) +
  geom_bar(stat = "count") +
  #geom_text(aes(label = labels, y= stat("count")), position = position_fill(vjust = 0.5)) + 
  labs(x = "", y = "Count", fill = "Biome States") +
  scale_fill_manual(values = colors) +
  theme_minimal()

#'My data on the vegetation formation of extant Gentiana species show that specialist are predominant. 79% of the species are specialist, preferring open habitats to closed ones.

## pie chart
## vegF
table(vegF$char_w.o_NA)
vegF_sum <- tibble(open = sum(vegF$char_w.o_NA == 1) +42, closed = sum(vegF$char_w.o_NA == 0)+42)
vegF_sum <- as.data.frame(t(vegF_sum))
vegF_sum
#'The most occupied vegetation formation is by far the open one, followed by the closed one. 
#'In contrast to the biome occupation, the open vegetation formation is occupied by 80% of the species, making it unevenly distributed.

vegF_sum <- 
  vegF_sum %>% 
  mutate(perc = V1 / sum(V1)) %>%
  mutate(group = rownames(.)) %>%
  arrange(desc(perc)) %>%
  mutate(labels = scales::percent(perc))


vegf_pie <-
  ggplot(vegF_sum, aes(x = "", y = perc, fill = group)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Biomes")) +
  scale_fill_manual(values = colors[1:2]) +
  coord_polar(theta = "y") + 
  theme_void()

# Combine the plots using gridExtra
# arrange with relative size = 1
plot_width <- 5  # Adjust this value to your desired size (in inches)
plot_height <- 4
rel_size <- 1
grid <- arrangeGrob(
  climbio_pie, vegf_pie, climbio_bar, vegf_bar,
  nrow = 2, ncol = 2
)
plot(grid)
ggsave("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/plots/summaryfigure_Data.pdf", grid, width = 10, height = 10, dpi = 300)
