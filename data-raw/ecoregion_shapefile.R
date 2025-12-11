## code to prepare `ecoregion_shapefile` dataset goes here
library(sf)

eco_shape <- st_read(dsn = "data-raw/shape_eco_simplified/shape_eco_simplified.shp")
eco_shape <- dplyr::filter(eco_shape, !Ecoregion %in% c("Arctic Ocean", "Azores", "Black Sea"))
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)


nea_features <- eco_shape$OBJECTID %in% c(1, 2, 9, 11, 13, 15, 17)
joined_regions <- st_union(eco_shape[nea_features, ])
joined_sfc <- st_sf(OBJECTID = 17, Ecoregion = "NE Atlantic", geometry = st_sfc(joined_regions))
eco_shape <- rbind(joined_sfc, eco_shape[!eco_shape$Ecoregion == "Oceanic Northeast Atlantic",c("OBJECTID", "Ecoregion")])
eco_shape <- eco_shape[match(sort(eco_shape$Ecoregion), eco_shape$Ecoregion),]
eco_shape$OBJECTID <- paste0(1:nrow(eco_shape))

usethis::use_data(eco_shape, overwrite = TRUE)

