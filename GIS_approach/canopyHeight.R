library(ggplot2)
library(gridExtra)
library(tidyverse)
library(sf)

#########################
## structure and log ####
#########################

# NA's in the canopy height extraction data - removed
# $vegf: non forest/open <5 m, forest/closed >5 m
# $outlier: IQR method to remove outliers per species' group (Problem: Gives huge range, very rare that an outlier is found)
#           lower bound = Q1 - 1.5*IQR
#           upper bound = Q3 + 1.5*IQR
# $sub_h: including std estimates for each occurrence into classification of vegf. 
#         uncertain estimates: exclude all estimates h that have a higher std than actual height
#         "both" means h >5 m & h - std < 5 m. vegf would classify this as closed but high std assumes that it may also be "open"
#         "both" is a conservative approach to classify the vegf as "open"
# I changed my approach and got rid of sub_h. I removed outliers by discarding the the upper 20% of std values.
# Following this I did the classification as described by $vegf and filtered out outliers as described by $outlier.
# I used two methods to classify whether a species is open or closed or both:
# The vegf categories where converted into numeric values and (i) the mean and sd (ii) the median and upper/lower bound with the 
# corresponding IQR were calculated per species.
#  
# (i) mean of distribution +- sd >= 1.5 --> open, otherwise closed, and for the case that it lower and upper bound lies in both spectrum --> both
# (ii) median of distribution and then classify if the upper and lower bound of the IQR lies in the open or closed or both spectrum
# I decided on using the first approach as the mean accounts better for two peaks in the distribution than the median. http://127.0.0.1:44651/graphics/plot_zoom_png?width=1920&height=1040
# also the distribution this time is limited to [1,2] as they actually represent categories. 

## read in data extracted from the global canopy height map
cph <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/DataCollection/Output_cph.csv", header = T)

## there are some NA's in the data
table(is.na(cph["h"]))
table(is.na(cph["std"]))
#' Na's are at the same coordinates for both data sets.
na_values <- which(is.na(cph$h))
na_values.df <- as.data.frame(cbind(na = na_values, lat=cph[na_values, "lat"], long=cph[na_values, "long"], species = cph[na_values, "species"]))
stat <- na_values.df %>% group_by(species) %>% summarise(n = n()) %>% arrange(desc(n)) %>% print(n = 20)
stats2 <- cph %>% group_by(species) %>% summarise(nr_occ = n()) %>% arrange(desc(nr_occ)) %>% print(n = 20)
stats <- merge(stat, stats2, by = "species")
plot(log(stats$nr_occ), log(stats$n))
#' general trend is that the species with the most occurrences also have the most NA's.

## visualize the na values on a map
cph$no_value <- ifelse(is.na(cph$h), "NA", "valid")
cph$no_value <- as.factor(cph$no_value)
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = cph,
             aes(x = long, y = lat, colour = no_value),
             size = 0.3, alpha = 0.8) +
  scale_colour_manual(values = c("NA" = "red", "valid" = "black")) +
  labs(colour = "Occurrences", title = "Distribution of NA values after extracting from Global Canopy Height Map") +
  theme_bw()

ggsave("C:/Users/janer/Documents/ETH Master/Master Thesis/DataCollection/NA_distribution_map.pdf", width = 22, height = 10, units = "cm")

### classification of vegf ####
## make a new column with 0 to 5 m being open grassland and >5 m being forest
## use this column to colour the points...
cph <- cph[!is.na(cph$h),]
cph$vegf <- ifelse(cph$h >= 10, "closed", "open")
cph$vegf <- as.factor(cph$vegf)
plot(cph$vegf)

## ploting tree height as a distribution on xlab and ylab shows me the frequency of the tree height
ggplot(cph, aes(x = h, fill = vegf)) +
  geom_histogram(binwidth = 0.5, colour = "black") +
  theme_bw() +
  labs(x = "Tree height (m)", y = "Frequency")

## ploting filtered by one species
cph %>% group_by(species) %>%
  drop_na(h) %>% # remove NA's
  filter("Gentiana_acaulis" %in% species) %>%
  ggplot(aes(x = h, fill = vegf)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  theme_bw() +
  labs(x = "Tree height (m)", y = "Frequency")



#### use std to exclude high uncertainty values ####
## remove the highest 20% of std values
quantile(cph$std, 0.8)
cph_filtered <- cph %>% filter(std < quantile(std, 0.8))
cph_filtered %>% arrange(desc(std)) %>% head(20)
cph_filtered %>% group_by(species) %>% summarise(n = n()) %>% arrange(desc(n)) %>% tail(10)

## plot the filtered data next to the raw data
grid.arrange(
  
  cph %>% ggplot(aes(x = h, fill = vegf)) +
    geom_histogram(binwidth = 0.5, colour = "black") +
    theme_bw() +
    labs(x = "Tree height (m)", y = "Frequency", title = "Raw data"),
  cph_filtered %>% ggplot(aes(x = h, fill = vegf)) +
    geom_histogram(binwidth = 0.5, colour = "black") +
    theme_bw() +
    labs(x = "Tree height (m)", y = "Frequency", title = "Filtered data"),
  
  ncol = 2
)


## IQR ######

# Calculate quartiles and IQR
df_Q1 <- cph_filtered %>% 
  group_by(species) %>% 
  summarise(Q1 = quantile(h, 0.25, na.rm = TRUE))

df_Q3 <- cph_filtered %>% 
  group_by(species) %>% 
  summarise(Q3 = quantile(h, 0.75, na.rm = TRUE))

df_IQR <- data.frame(IQR=df_Q3$Q3 - df_Q1$Q1, species=df_Q1$species)

# Define acceptable range
lower_bound <- data.frame(lw =df_Q1$Q1 - 1.5 * df_IQR$IQR, species=df_IQR$species)
upper_bound <- data.frame(up=df_Q3$Q3 + 1.5 * df_IQR$IQR, species=df_IQR$species)

# Add a column to indicate if a value is an outlier
cph_filtered <- cph_filtered %>%
  left_join(lower_bound, by = "species") %>%
  left_join(upper_bound, by = "species") %>%
  mutate(outlier = ifelse(h < lw | h > up, "out", "in")) 
# remove lw and up columns
cph_filtered <- cph_filtered %>% select(-lw, -up)

cph_filtered$outlier <- as.factor(cph_filtered$outlier)
summary(cph_filtered$outlier)
plot(cph_filtered$outlier)

## plot the filtered data and remove outlier, color with vegf
cph_filtered %>% 
  filter(species == "Gentiana_tianschanica") %>%
  ggplot(aes(x = h, fill = vegf)) +
  geom_histogram(binwidth = 0.5, colour = "black") +
  theme_bw() +
  labs(x = "Tree height (m)", y = "Frequency") +
  facet_wrap(~outlier)
#' outliers are uniformly distributed across tree heights


## summarize data ####
# from cph_filtered for each species and exclude outliers
cph_sum <- cph_filtered %>% group_by(species) %>% filter(outlier == "in") %>%
  summarise(species = unique(species), median_h = median(h, na.rm = T), 
            median_vegf = median(as.numeric(vegf)), 
            mean_vegf = mean(as.numeric(vegf), na.rm = T),
            sd_vegf = sd(as.numeric(vegf), na.rm = T), 
            IQR_vegf = IQR(as.numeric(vegf), na.rm =T), 
            Q1 = quantile(as.numeric(vegf), 0.25, na.rm = T), 
            Q3 = quantile(as.numeric(vegf), 0.75, na.rm = T), 
            n = n())
## To decide on the category open or closed or both, I will use two approch:
# (i) the mean_vegf +- sd_vegf
cph_sum <- cph_sum %>% 
  mutate(vegf_cat_sd = ifelse(1.5 >= (mean_vegf - sd_vegf) & 1.5 <= (mean_vegf + sd_vegf), "both", 
                           ifelse(1.5 <= (mean_vegf + sd_vegf), "open", "closed")))
# NA values adopt the category of the mean_vegf: 1=closed, 2=open
cph_sum$vegf_cat_sd[is.na(cph_sum$vegf_cat_sd)] <- 
  ifelse(cph_sum$mean_vegf[is.na(cph_sum$vegf_cat_sd)] > 1.5, "open", "closed")

cph_sum$vegf_cat_sd <-
  as.factor(cph_sum$vegf_cat_sd)

# (ii)the median_vegf and IQR_h
cph_sum <- cph_sum %>% 
  mutate(vegf_cat_iqr = ifelse(1.5 >= (Q1 - 1.5*IQR_vegf) & 1.5 <= (Q3 + 1.5*IQR_vegf), "both", 
                           ifelse(1.5 <= (Q1 - 1.5*IQR_vegf), "open", "closed")))
cph_sum$vegf_cat_iqr <-
  as.factor(cph_sum$vegf_cat_iqr)

### compare cph to my dataset ####
## read in the mydata
mydata <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/data/Gent_VegF_data.csv", header = T)
## transform values to same units
mydata <- mydata %>% 
          mutate(vegf = ifelse(character == 1, "open", ifelse(character == 2, "closed", ifelse(character == 0, NA, "both"))))
mydata$vegf <- as.factor(mydata$vegf)

plot(mydata$vegf)
#'My data is dominated by "open" species, which is also inline with common knowledge of the Gentiana species.

## match rows of mydata$taxa with cph_sum$species and compare values between mydata$vegf and cph_sum$vegf
# (i)
mydata_cph_sd <- mydata %>% left_join(cph_sum, by = c("taxa" = "species")) %>% 
  mutate(vegf_cat_sd_match = ifelse(vegf == vegf_cat_sd, "match", "no_match"))
table(mydata_cph_sd$vegf_cat_sd_match)
summary(mydata_cph_sd) # 38 NA's in mydata, 4 in mydata and 34 species that are not represented in the 
                      # extracted remote sensing data due to bad quality or not available coordinates.
# show the species that have a match
species_match_estimates <- mydata_cph_sd %>% filter(vegf_cat_sd_match == "match") %>% select(taxa, vegf) %>% print()

# show me the the counts between the categories of vegf and vegf_cat_sd
table(mydata=mydata_cph_sd$vegf, rs=mydata_cph_sd$vegf_cat_sd)
# plot the distribution of matches and no matches

wm <- borders("world", colour = "gray50", fill = "gray50")


cph_filtered %>% left_join(mydata_cph_sd, by = c(species = "taxa")) %>% select(species, long, lat, vegf_cat_sd_match, vegf_cat_sd) %>%
ggplot() +
  coord_fixed() +
  wm +
  geom_point(aes(x = long, y = lat, shape = vegf_cat_sd, color = vegf_cat_sd_match),
             size = 0.3, alpha = 0.8) +
  scale_colour_manual(values = c("match" = "red", "no_match" = "black")) +
  scale_shape_manual(values = c("open" = 1, "closed" = 2, "both" = 3)) +
  labs(colour = "", shape = "vegf", title = "Distribution of matching estimates between remote sensing data and literature research") +
  theme_bw()


# (ii)
mydata_cph_iqr <- mydata %>% left_join(cph_sum, by = c("taxa" = "species")) %>% 
  mutate(vegf_cat_iqr_match = ifelse(vegf == vegf_cat_iqr, "match", "no_match"))
table(mydata_cph_iqr$vegf_cat_iqr_match) # For gentiana producta & cruttwelli no data available in both datasets


###### old approach -> h - std ######
# paper states that std increases with height and is different between biomes... 
# therefore I exclude all estimates that have a higher std than actual height 
# --> wrong thinking, because high std will always be uncertain no matter how high the tree is!
cph <- cph %>%
  mutate(sub_h = ifelse(h - std < 0, "uncertain", ifelse(h - std > 5, "closed", ifelse(h - std < 5 & h < 5, "open", "both"))))
cph$sub_h <- factor(cph$sub_h, levels = c("closed", "open", "both", "uncertain"))
plot(cph$sub_h)
table(cph$sub_h == "uncertain" & cph$vegf == "open")
## exclude all uncertain values and outliers
cph <- cph %>% filter(sub_h != "uncertain") %>% filter(outlier != "out") %>% select(-outlier)
## how many values of vegf and sub_h are matching?
cph %>% group_by(vegf, sub_h) %>% summarise(n = n()) ## sub_h probably represents my data better than vegf...

# Plot the histogram and color sub_h differently
### filtered by species
cph %>% group_by(species) %>%
  filter("Gentiana_olivieri" %in% species) %>% # filter for a specific species
  ggplot(aes(x = h, fill = sub_h)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.6) +
  theme_bw() +
  labs(x = "Tree height (m)", y = "Frequency")
### across all species
cph %>% 
  ggplot(aes(x = h, fill = sub_h)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.6) +
  theme_bw() +
  labs(x = "Tree height (m)", y = "Frequency")

####### read data and clean old data#####
#cph <- read.csv("C:/Users/janer/Documents/ETH Master/Master Thesis/DataCollection/cph2.csv", header = F)
cph <- cph[-1,]
# Reset row indexing to start again at 1
rownames(cph) <- NULL
head(cph)
names(cph) <- c("index", "h", "std", "long", "lat", "species")
cph$long <- gsub("c\\(", "", cph$long)
cph$lat <- gsub(")", "", cph$lat)
cph$lat <- gsub(" ", "", cph$lat)
cph$index <- as.integer(cph$index)