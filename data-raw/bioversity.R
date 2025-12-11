## code to prepare `bioversity` dataset goes here
require(tidyverse)
require(vegan)
require(mFD)
require(RColorBrewer)
require(knitr)
#theme_set(theme_bw())

p.occurrence <- read_rds("./data-raw/biodiversity/Fish_p_occurrence.rds")
biomass <- read_rds("./data-raw/biodiversity/Fish_biomass.rds")

table(names(p.occurrence) == names(biomass))

species <- names(p.occurrence)[!names(p.occurrence) %in%
                                 c("Year", "Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c")]

threshold <- 0.5
realized_occurrence <- p.occurrence %>%
  mutate_at(.vars = species, # Apply transformation only to species
            .funs = function(x) ifelse(x >= threshold, 1, 0))

richness <- cbind(realized_occurrence %>% select(-all_of(species)), # Keep time and location variables
                  Richness = rowSums(realized_occurrence %>% select(all_of(species)))) # Sum of species occurrences


r.slopes <- richness %>%
  # define a unique identifier per location
  mutate(id = paste(longitude, latitude, sep = "_"),
         Year = as.numeric(as.character(Year))) %>%
  # compute lm per location
  split(.$id) %>%
  purrr::map(~lm(Richness ~ Year, data = .x)) %>% 
  purrr::map_df(broom::tidy, .id = 'id') %>%
  # Extract slope over time
  filter(term == 'Year') %>%
  # Recover longitude/latitude (now as unique identifier, i.e, id)
  tidyr::separate(id, c("longitude", "latitude"), sep = "_") %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         # Estimate confidence intervals
         lowerCI = estimate - 1.96 * std.error,
         upperCI = estimate + 1.96 * std.error)

slopeLims <- round(max(abs(range(r.slopes$estimate))) * c(-1, 1), 3)

# plotFun(r.slopes, variable = "estimate", size = 1) +
#   scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
#                         limits = slopeLims)

#Shannon diversity
shannon <- diversity(exp(biomass[, species]) - 1, index = "shannon")
nSp <- rowSums(ifelse(biomass[, species] > 0, 1, 0))
evenness <- shannon/log(nSp)

fish.diversity <- cbind(richness,
                        potentialRichness = nSp,
                        shannon = shannon,
                        evenness = evenness) %>%
  mutate(Year = as.numeric(as.character(Year)))

# plotFun(fish.diversity, variable = "shannon", years = c(2010, 2015, 2020))
# plotFun(fish.diversity, variable = "evenness", years = c(2010, 2015, 2020))



alpha_fd_indices <- readRDS("data-raw/biodiversity/alpha_fd_indices.rds")
functional_richness <- readRDS("data-raw/biodiversity/f_ric.rds")

alpha_fd_indices$functional_diversity_indices$sp_richn
functional_richness$functional_diversity_indices

fish.diversity <- cbind(fish.diversity,
                        fric_pa = functional_richness$functional_diversity_indices$fric,
                        alpha_fd_indices$functional_diversity_indices
                        )

ices_ecoregions <- st_read("data-raw/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% st_make_valid()
fish_div_spatial <- fish.diversity %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ices_ecoregions))
fish_div_spatial <-  fish_div_spatial %>%  st_join(ices_ecoregions)

# biodiversity <- list(richness=richness,
#                      richness_trends = list(richness_slopes = r.slopes,
#                                             limits = slopeLims))
usethis::use_data(fish.diversity, fish_div_spatial, overwrite = TRUE)


# dat <- readRDS("data-raw/BUSEFUL Decision-support tool data/NM.01.1980.rds")
# load("data-raw/BUSEFUL Decision-support tool data/annual_means_environment_2020-2050.RData")