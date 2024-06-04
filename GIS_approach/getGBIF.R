packages <- c("raster", "rgbif", "CoordinateCleaner", "tidyverse", "ape")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
library(tidyverse)
lapply(packages, library, character.only = TRUE) %>%
  invisible()

setwd("C:/Users/janer/Documents/ETH Master/Master Thesis/DataCollection/") ##local
#setwd("/home/buehrerj/gbif/") ##server
source(file.path(getwd(), "wslGbif.R"))

# Get taxa names
tree <- read.nexus("C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/data/Gentiana_cor.nex") ##local
#tree <- read.nexus("/home/buehrerj/gentiana/data/Gentiana_cor.nex") ##server
splist <- tree$tip.label

mat = matrix(ncol = 0, nrow = 0)
df=data.frame(mat)
for (i in splist){
  df1= wsl.gbif(i)%>%mutate(species = i)
  df=rbind(df,df1)
}
flags <- clean_coordinates(x = df, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("capitals", "centroids", 
                                     "equal","gbif", "institutions",
                                     "zeros", "duplicates", "urban"),
                           seas_ref = buffland) # most test are on by default
df <- df[flags$.summary, ]
#library(maptools)
#data(wrld_simpl) #加载世界地图。
#plot(wrld_simpl, col="tan2",bg="lightblue", border= NA) #图示地图。
#points(flags$decimalLongitude, flags$decimalLatitude, bg="red", col='white', pch=21, cex=1.2) #将从GBIF下载的数据映射到地图上 (图 9)。
#points(df$decimalLongitude, df$decimalLatitude, bg="blue", col='blue', pch=15, cex=0.5) #将从GBIF下载的数据映射到地图上 (图 9)。

#plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = df,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()



## save filtered occurrence data
write.csv(df,file.path(getwd(),"gbif/Gentiana_occ.csv"))

# read.csv(file.path(getwd(),"gbif/Gentiana_occ.csv")) -> df
## load chelsa maps
tlh <- raster(file.path(getwd(),"chelsa/CHELSA_tlh_1981-2010_V.2.1.tif"))
mcb <- raster(file.path(getwd(),"chelsa/CHELSA_mcb_1981-2010_V.2.1.tif"))

coordinates(df) = c("decimalLongitude","decimalLatitude") # specify column names
ex_tlh <- raster::extract(x = tlh, y = df)
ex_mcb <- raster::extract(x = mcb, y = df)
res <- data.frame(df, ex_tlh,ex_mcb)
summary(res)

## save raw output
write.csv(res, file.path(getwd(), "chelsa_gentiana_raw.csv"))

