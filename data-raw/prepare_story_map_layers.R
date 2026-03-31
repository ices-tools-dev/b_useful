library(sf)
library(dplyr)
country_borders <- st_read("data-raw/CNTR_RG_60M_2024_4326.gpkg")

eu_countries <- country_borders[country_borders$"EU_STAT" == "T",]

usethis::use_data(eu_countries, country_borders, overwrite = TRUE, internal = FALSE)

trawls <- data.frame()
survey <- "NS-IBTS"
trawl <- icesDatras::getHHdata(survey = "NS-IBTS", year = 2020, quarter = 3) %>% 
  select(lon = HaulLong, lat = HaulLat) %>% 
  as.matrix %>% 
  st_multipoint %>% 
  st_sfc(crs = 4945) %>% 
    st_sf(data.frame(survey = survey))
          
trawls <- rbind(trawls, trawl)

usethis::use_data(trawls, overwrite = TRUE, internal = FALSE)
