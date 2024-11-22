###########################################################################
###########################################################################
###                                                                     ###
###                            Création BD DEPLACEMENTS                 ###
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


# déplacement  ------------------------------------------------------------


WORKING_DisplacementDB_3_0 <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/WORKING_DisplacementDB_3.0.xlsx", 
                                         sheet = "EHTools", skip = 2)

WORKING_DisplacementDB_3_0 <-WORKING_DisplacementDB_3_0 |>  
  mutate(quarter = paste0("T", lubridate::quarter(startDate), "_", lubridate::year(startDate)),
         year=lubridate::year(startDate)) |> 
  rename(province=admin1Label, territoire=admin2Label)


#we want the population of each territoire
ATLAS_MT_2024_full <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/FINAL_UPDATE_20240826_ATLAS_MT_2024.xlsx", 
                                 sheet = "APERCU") 


ATLAS_MT_2024 <- ATLAS_MT_2024_full |> 
  select(Territoire, `Population (DPS 2023)`) |> rename(pop=2) |> mutate(Territoire=str_to_title(Territoire)) |> 
  # filter(!str_detect(Territoire, "(Ville)")) |> 
  # mutate(Territoire=str_trim((gsub("\\(Ville\\)", "", Territoire))),
    mutate(Territoire=ifelse(Territoire=="Beni (Territoire / Oicha)","Beni", Territoire)) |> 
  group_by(Territoire) |> summarise(pop_totale=sum(pop)) |> rename(TERRITOIRE=1)

saveRDS(ATLAS_MT_2024, "ATLAS_MT_2024.rds")
saveRDS(WORKING_DisplacementDB_3_0, "BD_deplacement.rds")
