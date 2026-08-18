# Top level script to prepare project data. Run this script to generate the data structures necessary for the app to run.
# Sources scripts specific to individual work packages as well as scripts preparing data for the app generally.

# Fetch output of data prep TAF repo
#TAF::cp("../b_useful_dst/output/wp3/*", to = "inst/app/www/wp3")
TAF::cp("../b_useful_dst/output/*", to = "inst/app/www")

TAF::cp("../b_useful_dst/data/*", to = "data")


source("data-raw/prep_colours.R")
cat("Project colours prepared\n")

source("data-raw/prep_project_texts.R")
cat("Project texts prepared\n")

# source("data-raw/wp2/prep_data_wp3.R")
# cat("WP3 data prepared\n")

region_codes <- c("greater_north_sea" = "NrS", 
                  "western_mediterranean_sea" = "w_med",
                  "central-eastern_mediterranean_sea" = "e_med",
                  "north_east_atlantic" = "nea")

usethis::use_data(region_codes, overwrite = TRUE)
