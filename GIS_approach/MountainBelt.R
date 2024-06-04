setwd("C:/Users/janer/Documents/ETH MASTER/Master Thesis/DataCollection")

library(tidyverse)
library(ggplot2)

dat <- read.csv("chelsa_gentiana_raw.csv", header =T)

## select columns containing information on the habitat of the species
d <- dat %>% select(X, species, fieldNotes, habitat, ex_tlh, ex_mcb, basisOfRecord)
d <- d %>% filter(!is.na(habitat) | !is.na(fieldNotes))
d %>% group_by(species) %>% summarise(n = n()) %>% tail(10)

# species that needs to be checked
sp <- c("Gentiana_cinereifolia", "Gentiana_cruttwellii", "Gentiana_lineolata", "Gentiana_lycopodioides", "Gentiana_piasezkii", 
        "Gentiana_praeclara", "Gentiana_producta", "Gentiana_tianschanica", "Gentiana_ornata", "Gentiana_officinalis")

check_spieces <- d %>% filter(species %in% sp)

########################################
# Discrepancy between the two datasets #
########################################
# testing whether the data from ex_tlh and ex_mcb are showing similar results

alpine <- which(dat$ex_tlh < 0)
alpine_mcb <- which(dat$ex_mcb <= 3)
na_values <- dat %>% filter(is.na(ex_tlh) | is.na(ex_mcb))
matching <- intersect(alpine, alpine_mcb)
non_matching <- setdiff(alpine, alpine_mcb)

# checking the data for the wrong classification
table(dat[non_matching, "ex_mcb"])
wrong_class_5 <- dat[non_matching,] %>% filter(ex_mcb == 5)
wrong_class_4 <- dat[non_matching,] %>% filter(ex_mcb == 4)

wrong_occ_data <- wrong_class_4[wrong_class_4$basisOfRecord=="OCCURRENCE", ]

wrong_occ_data %>%
  group_by(species) %>%
  mutate(n = n())%>%
  select(species, basisOfRecord, n) %>%
  ggplot(aes(x = reorder(species, n), y = log10(n), fill = basisOfRecord)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(
    title = "Number of occurrences per species",
    x = "Species",
    y = "Number of occurrences"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_x_discrete(labels = function(x) gsub("Gentiana_", "", unique(x)))

###########################################
# Distribution of data points per species #
###########################################
# ex_tlh negative values are converted to A and positive values to a
dat <- dat %>% mutate(treeline = ifelse(ex_tlh < 0, "above", "below"))
dat$treeline <- as.factor(dat$treeline)

#mcb stats of dat per species
mcb_per_species <- dat %>% 
  drop_na(ex_mcb) %>% 
  group_by(species) %>% 
  summarise(n = n(), median = median(ex_mcb), q1 = quantile(ex_mcb, 0.25), q3= quantile(ex_mcb, 0.75), iqr= IQR(ex_mcb), 
            mean = mean(ex_mcb), sd = sd(ex_mcb), min = min(ex_mcb), max = max(ex_mcb))
#tlh stats of dat per species
tlh_per_species <- dat %>% 
  drop_na(ex_tlh) %>% 
  group_by(species) %>% 
  summarise(n = n(), median = median(ex_tlh), q1 = quantile(ex_tlh, 0.25), q3= quantile(ex_tlh, 0.75), iqr= IQR(ex_tlh),
            mean = mean(ex_tlh), sd = sd(ex_tlh), min = min(ex_tlh), max = max(ex_tlh))

#remove outliers / rare occurrences

### mcb
# Define acceptable range
lower_bound_mcb <- data.frame(lw =mcb_per_species$q1 - 1.5 * mcb_per_species$iqr, species=mcb_per_species$species)
upper_bound_mcb <- data.frame(up=mcb_per_species$q1 + 1.5 * mcb_per_species$iqr, species=mcb_per_species$species)
# Add a column to indicate if a value is an outlier
dat <- dat %>%
  left_join(lower_bound_mcb, by = "species") %>%
  left_join(upper_bound_mcb, by = "species") %>%
  mutate(outlier_mcb = ifelse(ex_mcb < lw | ex_mcb > up, "out", "in")) %>% select(-lw, -up)

dat$outlier_mcb <- as.factor(dat$outlier_mcb)
summary(dat$outlier_mcb)
plot(dat$outlier_mcb)

### tlh
# Define acceptable range
lower_bound_tlh <- data.frame(lw =tlh_per_species$q1 - 1.5 * tlh_per_species$iqr, species=tlh_per_species$species)
upper_bound_tlh <- data.frame(up=tlh_per_species$q1 + 1.5 * tlh_per_species$iqr, species=tlh_per_species$species)
# Add a column to indicate if a value is an outlier
dat <- dat %>%
  left_join(lower_bound_tlh, by = "species") %>%
  left_join(upper_bound_tlh, by = "species") %>%
  mutate(outlier_tlh = ifelse(ex_tlh < lw | ex_tlh > up, "out", "in")) %>% select(-lw, -up)

dat$outlier_tlh <- as.factor(dat$outlier_tlh)
summary(dat$outlier_tlh)
plot(dat$outlier_tlh)

table(dat$outlier_tlh == "out" & dat$outlier_mcb == "out") # less than 50% of the data points reveal to be outliers are shared between both datasets, 
                                                          # meaning that the outliers are not the same in both datasets!

# plot data and remove outlier, colour above or below treeline
dat %>% group_by(species) %>%
  drop_na(ex_mcb) %>%
  #filter(outlier_mcb == "in") %>%
  filter("Gentiana_cuneibarba" %in% species) %>%
  ggplot(aes(x = ex_mcb)) +
  geom_histogram(binwidth = 0.5, colour = "black") +
  theme_minimal() +
  labs(x = "biome", title = "Distribution of Biome Estimates by Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~outlier_mcb)


# use the q1 and q3 as range for biome occupation and transform the data from value 1 to 7 to bitformat
#multiple biome occupation
mcb <- mcb_per_species %>% 
  drop_na(median) %>%
  mutate(A = ifelse(q1 < 3.5, 1, 0),
         subA = ifelse(q1 >= 3.5 & q1 < 4.5, 1, 0),
         Temp = ifelse(q1 >= 4.5, 1, 0)) %>%
  mutate(A = ifelse(q3 < 3.5, 1, A),
         subA = ifelse(q3 >= 3.5 & q3 < 4.5, 1, subA),
         Temp = ifelse(q3 >= 4.5, 1, Temp))

tlh <- tlh_per_species %>% 
  drop_na(median) %>%
  mutate(Alp = ifelse(q1 < 0, 1, 0),
         alp = ifelse(q1 >= 0 & q1 < 4.5, 1, 0)) %>%
  mutate(Alp = ifelse(q3 < 0, 1, Alp),
         alp = ifelse(q3 >= 0, 1, alp))

# match rows of mcb and mydata
## read in the mydata
mydata <- read.csv2("C:/Users/janer/Documents/ETH Master/Master Thesis/Gentiana/data/Gent_biome_bit.csv", header = T)

# match rows of mcb and mydata
mcb <- mcb %>%
  left_join(mydata, by = c("species" = "taxa")) %>%
  mutate(match = ifelse(A == tundra_alpine & subA == boreal_subalpine & Temp == temperate, 1, 0)) #%>%
  #select(-c("n", "median", "q1", "q3", "iqr", "mean", "sd", "min", "max"))

table(mcb$match) # 29% matching species estimates; when removing alpine comparison then it goes up to 42% of matches
no_match.tbl <- mcb %>% filter(match ==0) %>% select(species, A, tundra_alpine, subA, boreal_subalpine, Temp , temperate)
summary(no_match.tbl)

# create a new column to indicate whether the species is found in the alpine region or not
mydata <- mydata %>%
  mutate(A = ifelse(tundra_alpine == 1, 1, 0),
         a = ifelse(boreal_subalpine == 1 | temperate == 1, 1, 0)
         )
# match rows of tlh and gentiana
name <- names(tlh)
tlh <- tlh %>%
  left_join(mydata, by = c("species" = "taxa")) %>%
  mutate(match = ifelse(Alp == A & alp == a, 1, 0)) %>%
  select(all_of(name), A, a, match)
  
table(tlh$match) # 53% matching species estimates
tbl_match <- mcb %>% filter(match ==1 & tlh$match==1) %>% select(species) # matches are in both datasets almost the same, 4 out of 55 matches of mcb are no matches in tlh
no_match.tbl <- tlh %>% filter(match ==0) %>% select(species, Alp, A, alp, a)
summary(no_match.tbl)
#plot distribution for each species

p <- dat %>% group_by(species) %>%
  filter("Gentiana_acaulis" %in% species) %>%
      ggplot(aes(x = ex_mcb, y = NULL)) +
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Species", y = "Biome", title = "Distribution of Biome Estimates by Species")

p <- dat %>% group_by(species) %>%
  filter("Gentiana_acaulis" %in% species) %>%
  ggplot(aes(x = X, y = ex_tlh)) +
  geom_jitter(na.rm = T) +
  stat_function(fun = dnorm, args = list(mean = mean(dat$ex_tlh), sd = sd(dat$ex_tlh)), color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

