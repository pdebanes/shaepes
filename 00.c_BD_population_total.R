
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

#we want the population of each territoire
ATLAS_MT_2024_full <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/FINAL_UPDATE_20240826_ATLAS_MT_2024.xlsx", 
                                 sheet = "APERCU") 



#petite hésitation sur Beni (ville) et Butembo (ville) => au final, je pense que ces deux sont comptés aussi dans leurs territoires respectifs
# pour Beni => pas de ville dans BD conflict, comptée comme partie de Beni territoire
# => on peut garder Beni ville? et faire un extract BD conflict pour Beni ville  
ATLAS_MT_2024 <- ATLAS_MT_2024_full |> 
  select(Province, Territoire, `Population (DPS 2023)`) |> rename(pop=3) |> mutate(territoire=str_to_lower(Territoire),
                                                                                   province=str_to_lower(Province)) 

table(ATLAS_MT_2024$territoire)

ATLAS_MT_2024 <-ATLAS_MT_2024 |> 
  # filter(!str_detect(Territoire, "Ville")) |> 
  mutate(territoire=ifelse(territoire=="beni (territoire / oicha)", "beni", territoire),
         territoire=ifelse(territoire=="beni (ville)", "beni-ville", territoire),
         territoire=ifelse(territoire=="butembo (ville)", "butembo", territoire)) |> 
  group_by(province, territoire) |> summarise(pop_totale=sum(pop)) |> rename(territoire=2)

saveRDS(ATLAS_MT_2024, "ATLAS_MT_2024.rds")

# table(ATLAS_MT_2024_full$Territoire)
# 
# 
# 
# list_lubero<-ATLAS_MT_2024_full |> filter(Territoire=="LUBERO") |> rename(zs=`Zone de santé`) |> mutate(zs=str_to_lower(zs)) |> select(zs) |>  unique() |> pull()
# list_lubero
# 
# list_beni<-ATLAS_MT_2024_full |> filter(Territoire=="BENI (Territoire / Oicha)") |> rename(zs=`Zone de santé`) |> mutate(zs=str_to_lower(zs)) |> select(zs) |>  unique() |> pull()
# list_beni
# 
# 
# ATLAS_MT_2024_full |> filter(Territoire=="BENI (Ville)") |> select(`Zone de santé`) |> unique()
# ATLAS_MT_2024_full |> filter(Territoire=="BUTEMBO (Ville)") |> select(`Zone de santé`) |> unique()
