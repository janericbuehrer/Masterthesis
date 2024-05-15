library("GIFT")
library("dplyr")
library("knitr")
library("kableExtra")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("patchwork")

#all_sp <- GIFT_species()
#gift_shapes <- GIFT_shapes() # retrieves all shapefiles by default
#gentiana_sp <- all_sp[all_sp$work_genus == "Gentiana",]

###### map gentiana distribution #####
# get distribution of entire Genus Gentiana
species_list <- gsub("Gentiana ", "", gentiana_sp$work_species)
gentiana_distr <- list()
for (sp_name in species_list) {
  print(sp_name)
  gentiana_distr[[sp_name]] <- GIFT_species_distribution(genus = "Gentiana", epithet = sp_name, aggregation = TRUE)
}
distr_df <- do.call(rbind, gentiana_distr)


# create a column showing total species richness per entity
gentiana_richness <- distr_df %>%
  group_by(entity_ID) %>%
  summarise(total_sp = n())

# Merge with the shapes
rich_map <- dplyr::left_join(gift_shapes, gentiana_richness, by = "entity_ID") %>%
  dplyr::filter(stats::complete.cases(total_sp))

# richness map
ggplot(world) +
  geom_sf(color = "gray50") +
  geom_sf(data = rich_map, aes(fill = total_sp + 1)) +
  scale_fill_viridis_c("Species number\n(log-transformed)", trans = "log10",
                       labels = scales::number_format(accuracy = 1)) +
  labs(title = "Gentiana", subtitle = "Projection EckertIV") +
  coord_sf(crs = eckertIV) +
  theme_void()

# save the map
ggsave(filename = "C:/Users/janer/Documents/ETH MASTER/Master Thesis/DataCollection/gentiana_richness_map.pdf", plot = last_plot(), width = 20, height = 15)