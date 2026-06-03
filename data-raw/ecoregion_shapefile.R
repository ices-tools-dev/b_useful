## code to prepare `ecoregion_shapefile` dataset goes here
library(dplyr)
library(sf)

eco_shape <- st_read(dsn = "data-raw/shape_eco_simplified/shape_eco_simplified.shp")
required_regions <- eco_shape$Ecoregion[c(1,2,9,10,11,13,14,16,17)]
eco_shape <- dplyr::filter(eco_shape, Ecoregion %in% required_regions) #!Ecoregion %in% c("Arctic Ocean", "Azores", "Black Sea"))
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)

nea_features <- eco_shape$OBJECTID %in% c(1, 2, 9, 11, 13)
joined_regions <- st_union(eco_shape[nea_features, ])
joined_sfc <- st_sf(OBJECTID = 17, Ecoregion = "NE Atlantic", geometry = st_sfc(joined_regions))

eco_shape <- rbind(joined_sfc, eco_shape[!eco_shape$Ecoregion == "Oceanic Northeast Atlantic",c("OBJECTID", "Ecoregion")])
eco_shape <- eco_shape[match(sort(eco_shape$Ecoregion), eco_shape$Ecoregion),]
eco_shape <- filter(eco_shape, !Ecoregion %in% c("Bay of Biscay and the Iberian Coast", "Celtic Seas", "Greenland Sea"))



wmed <- gsa_areas %>%
  filter(F_SUBAREA == 37.1,
         !F_GSA_LIB %in% c("GSA 3", "GSA 4")) %>%
  st_transform(3035) %>%        # projected CRS, metres
  st_make_valid() %>%
  st_buffer(700) %>%            # close small gaps / overlaps
  st_union() %>%
  st_buffer(-700) %>%           # restore approximate boundary
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(Ecoregion = "Western Mediterranean Sea") %>%
  rename(geometry = "x") %>%
  st_transform(st_crs(gsa_areas))

cemed <- filter(gsa_areas, F_GSA_LIB %in% paste("GSA", c(15:20, 22,23,25), sep = " ")) %>% 
  st_transform(3035) %>%        # projected CRS, metres
  st_make_valid() %>%
  st_buffer(700) %>%            # close small gaps / overlaps
  st_union() %>%
  st_buffer(-700) %>%           # restore approximate boundary
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(Ecoregion = "Central and Eastern Mediterranean Sea") %>% 
  rename(geometry = "x") %>%
  st_transform(st_crs(gsa_areas))
  

eco_shape <- select(eco_shape, -OBJECTID) %>% 
  rbind(wmed,cemed)

eco_shape <- eco_shape %>% 
  st_make_valid %>% 
  st_union %>% 
  st_make_valid %>% 
  st_as_sf %>%
  mutate(Ecoregion = "B-USEFUL Study Area") %>% 
  rename(geometry = "x")
usethis::use_data(eco_shape, overwrite = TRUE)

