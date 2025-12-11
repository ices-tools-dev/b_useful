# Top level script to prepare project data. Run this script to generate the data structures necessary for the app to run.
# Sources scripts specific to individual work packages as well as scripts preparing data for the app generally.


# Prep WP2
source("data-raw/prep_colours.R")
cat("Project colours prepared\n")

source("data-raw/prep_project_texts.R")
cat("Project texts prepared\n")

# source("data-raw/wp2/prep_data_wp3.R")
# cat("WP3 data prepared\n")
