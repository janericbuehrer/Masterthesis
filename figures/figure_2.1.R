# Biome map
# Date: 03.06.2024


# libraries ---------------------------------------------------------------

library(sf)
library(mapview)
library(tidyverse)
library(viridis)
library(ggplot2)

# load data ---------------------------------------------------------------

wwf_terr_biom_sf_raw <- st_read("C:/Users/janer/Documents/ETH MASTER/Master Thesis/Biome_map/wwf_terr_ecos.shp") %>%
  janitor::clean_names()

wwf_terr_biom_sf_north_hemi <- wwf_terr_biom_sf_raw %>% 
  filter(realm %in% c("NA", "PA"))

NH_biomes <- wwf_terr_biom_sf_north_hemi %>% 
  group_by(biome) %>% 
  reframe(st_union(geometry)) %>% 
  st_as_sf()

biome_key <- tibble(
  biome_nr = 1:13,
  biome_name = c("Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Dry Broadleaf Forests",
                 "Tropical and Subtropical Coniferous Forests", 
                 "Temperate Broadleaf and Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga",
                 "Tropical and Subtropical Grasslands, Savannas and Shrublands", "Temperate Grasslands, Savannas and Shrublands", 
                 "Flooded Grasslands and Savannas", "High-Mountain Grasslands and Shrublands", "Tundra",
                 "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")
)

NH_biomes_names <- NH_biomes %>% left_join(biome_key, by = join_by("biome" == "biome_nr")) %>% 
  filter(biome %in% c(4,5,6,8, 10,11))

# mapview(NH_biomes_names, zcol = "biome_name", legend = F)


# prepare plot ------------------------------------------------------------

NH_biomes_simplified <- NH_biomes_names %>% 
  st_simplify(dTolerance = 0.8) %>%
  st_as_sf() %>% 
  arrange(biome)

mapview(NH_biomes_simplified, zcol = "biome_name", legend = F)


wm <- borders("world", colour = "gray80", fill = "gray100") # first layer: world map

plot <- ggplot() +
  wm +
  geom_sf(data = NH_biomes_names, aes(fill = biome_name)) + # biome layers
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  labs(title = NULL,
       fill = "Biome")

ggsave(plot = plot,
       filename = "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Biomes_NH.png",
       width = 15,
       height = 10,
       dpi = 300, 
       bg = "white")

