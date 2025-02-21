###########################################################################
###########################################################################
###                                                                     ###
###                       stack indicators                              ###
###                                                                     ###
###########################################################################
###########################################################################



# Libraries ---------------------------------------------------------------



libraries <- c(
  "bannerCommenter", "readxl", "readr", "openxlsx", "tidyverse", 
  "data.table", "dplyr", "stringr", "conflicted", "quarto", "knitr", "janitor"
)
for (package in libraries) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}
conflicted::conflicts_prefer(dplyr::filter)

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")


#### Run all the indicators ####
# script_files <- list.files(pattern = "^01.*\\.R$", full.names = TRUE)
# for (script in script_files) {
#   source(script)
# }

# get all the RDS with indicators

# Define the folder containing the .rds files
folder_path <- "C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes"

# List all .rds files starting with "indicateur"
rds_files <- list.files(folder_path, pattern = "^indicateur.*\\.rds$", full.names = TRUE)

# Read and stack all the files into a single data frame
combined_data <- purrr::map_dfr(rds_files, readRDS)

# Check the result
print(combined_data)

table(combined_data$province)
table(combined_data$territoire)

combined_data<-combined_data |> mutate(territoire=str_to_lower(territoire))

summary(combined_data)
# test
test<-combined_data |>filter(is.na(annee))

# Optionally save the combined data
saveRDS(combined_data, file = file.path(folder_path, "stacked_indicators.rds"))


# test --------------------------------------------------------------------
stacked_indicators<-read_rds("stacked_indicators.rds")
test_irumu<-stacked_indicators |> filter(territoire=="irumu" & quarter=="T2_2024")
test_irumu
