library(sf)
library(tidyverse)
library(dplyr)
library(rgee)


#ee_install(py_env = "rgee") # It is just necessary once!

# Intialise ee
ee_Initialize()

setwd("C:/Users/janer/Documents/ETH Master/Master Thesis/DataCollection/")
# Read the input CSV file
all_plots = read.csv(file.path(getwd(),"gbif/Gentiana_occ.csv"))

## basis of Record
barplot(table(all_plots$basisOfRecord))
dist <- all_plots[all_plots$basisOfRecord == "OCCURRENCE", "species"]
dist_perspecies <- unique(dist)

## distribution of data points per species
test <- all_plots %>%
  group_by(species) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

all_plots %>%
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


# Select only unique points (lon/lat)
taxa_recall <- all_plots %>% distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)
unique_points = unique(all_plots[,c("decimalLatitude", "decimalLongitude" #, "species"
                                    )])

unique_points = na.omit(unique_points) # filter out any NA in coordinates
sum(is.na(unique_points)) # check if there are any NA left

# Create point object
# make sure you have right CRS
my_points_all = st_as_sf(unique_points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) # coordinate reference system (CRS)
rounds <- abs(nrow(my_points_all)/3)
loop_start <- c(1, rounds+1, rounds*2+1)
loop_end <- c(rounds, rounds*2, rounds*3)
my_cpH_data <- list()
my_cpH_std_data <- list()

for (i in 1:length(loop_start)) {
  my_points = my_points_all[loop_start[i]:loop_end[i],] 
  # Export to ee
  my_points_ee = sf_as_ee(my_points)
  # Load image
  gee_canopyH <- ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')
  gee_cpH_filtered <- gee_canopyH$clip(my_points_ee)
  
  gee_canopyH_std <- ee$Image('users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1')
  gee_cpH_std_filtered <- gee_canopyH_std$clip(my_points_ee)
  
  # Extract data
  my_cpH_data[[i]] <- ee_extract(gee_cpH_filtered,
                                 my_points_ee,
                                 fun = ee$Reducer$first(),
                                 scale = 10,
                                 sf = T)
  my_cpH_std_data[[i]] <- ee_extract(gee_cpH_std_filtered,
                                     my_points_ee,
                                     fun = ee$Reducer$first(),
                                     scale = 10,
                                     sf = T)
  
  
}

## convert my list to data frame
cph_data <- do.call(rbind, my_cpH_data)
cph_std_data <- do.call(rbind, my_cpH_std_data)


## add std as column to dat_cpH
dat_cph <- data.frame(h = cph_data$b1, std = cph_std_data$b1, ee = cph_data$geometry)
dat_cph <- cbind(dat_cph, long=unique_points$decimalLongitude, lat=unique_points$decimalLatitude)
dat_cph$species <- taxa_recall$species

dat_cph <- dat_cph[,c("h", "std", "long", "lat", "species", "geometry")]

## checking for Na's
table(is.na(cph_data))
table(is.na(cph_std_data))


# Save CSV
#write.csv(dat_modis, "./data/Output_LC_Type1.csv")
dat_cph <- as.matrix(dat_cph)
fp <- file.path(getwd(), "Output_cph.csv")
write.csv2(dat_cph, fp, row.names = FALSE)



#### MODIS ####
# Load image collection MOD12Q1
# Make sure you select correct GEE path and band names
gee_collection_modis <- ee$ImageCollection("MODIS/061/MCD12Q1")$
  filterBounds(my_points_ee)$ # filter only points of interest
  select("LC_Type1", "QC") # select bands for of interest (NDVI and quality)

# Extract data
# Make sure you indicate correct "scale" in meters
my_modis_data <- ee_extract(gee_collection_modis,
                            my_points_ee,
                            fun = ee$Reducer$first(),
                            scale = 500,
                            sf = T)


# Reformat if needed
# Note, this may be specific to MODIS, but with height product maybe there is no date
dat_modis <- my_modis_data %>%
  pivot_longer(-geometry, names_to = "date_variable", values_to = "value") %>%
  mutate(date = as.Date(gsub("X([0-9]{4})_([0-9]{2})_([0-9]{2})_.*", "\\1-\\2-\\3", date_variable)),
         variable = gsub(".*_(.*)", "\\1", date_variable)) %>%
  select(-date_variable) %>%
  pivot_wider(names_from = "variable" , values_from = "value")

## Process resulting dataset
dat$date = as.Date(dat$date) # format dates

# Look at data
head(dat)
