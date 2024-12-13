###########################################################################
###########################################################################
###                                                                     ###
###         CREATE FRAME                                                ###
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

Total_BD_conflict<-readRDS("Total_BD_conflict.rds")
data_raw<-Total_BD_conflict |> mutate(province=str_to_lower(province),
                                      territoire=str_to_lower(territoire))

#  create frame -----------------------------------------------------------


province_territoire_mapping <- data_raw %>%
  select(province, territoire) %>%
  distinct()

# Create a complete grid of all combinations
complete_frame <- province_territoire_mapping %>%
  expand_grid(
    quarter = unique(data_raw$quarter)
  )

complete_frame_ind<-complete_frame |> unique() |> mutate(annee=as.character(sub(".*_", "", quarter)))

saveRDS(complete_frame_ind, "complete_frame.rds")
