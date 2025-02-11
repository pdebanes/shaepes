###########################################################################
###########################################################################
###                                                                     ###
###                            Création BD                              ###
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


# import BD conflict ------------------------------------------------------



sudkivu <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/BD_Conflict_V6.1.xlsx", 
                                     sheet = "Sud-Kivu")|> as.data.frame() |> 
  mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |> 
  select(-Trimestre) 

nordkivu <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/BD_Conflict_V6.1.xlsx", 
                      sheet = "Nord-Kivu") |> 
  as.data.frame() |> 
  mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |> 
  select(-Trimestre)|> rename(Chefferie=Chefferies)

ituri <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/BD_Conflict_V6.1.xlsx", 
                       sheet = "Ituri") |>  mutate(Trimestre = str_replace(Trimestre, "Trimestre_", "")) %>% # Remove "Trimestre"
  mutate(Quarter = paste0("T",Trimestre,"_",  ANNEE)) |> 
  select(-Trimestre)

# head(ituri$Quarter)
# maniema <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/BD_Conflict_V6.1.xlsx",
#                                sheet = "Maniema")|> as.data.frame() |>
#   mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |>
#   select(-Trimestre)

tanganyika <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/BD_Conflict_V6.1.xlsx", 
                               sheet = "Tanganyika")|> as.data.frame() |> 
  mutate(Trimestre = str_replace(Trimestre, "Trimestre", ""))%>%  
  mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |> 
  select(-Trimestre) 
                         
 #we want the population of each territoire
ATLAS_MT_2024_full <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/FINAL_UPDATE_20240826_ATLAS_MT_2024.xlsx", 
                          sheet = "APERCU") 

#petite hésitation sur Beni (ville) et Butembo (ville) => au final, je pense que ces deux sont comptés aussi dans leurs territoires respectifs
# pour Beni => pas de ville dans BD conflict, comptée comme partie de Beni territoire
# => on peut garder Beni ville? et faire un extract BD conflict pour Beni ville  
ATLAS_MT_2024 <- ATLAS_MT_2024_full |> 
select(Territoire, `Population (DPS 2023)`) |> rename(pop=2) |> mutate(Territoire=str_to_title(Territoire)) |> 
filter(!str_detect(Territoire, "Ville")) |> 
mutate(Territoire=ifelse(Territoire=="Beni (Territoire / Oicha)", "Beni", Territoire)) |> 
group_by(Territoire) |> summarise(pop_totale=sum(pop)) |> rename(TERRITOIRE=1)

saveRDS(ATLAS_MT_2024, "ATLAS_MT_2024.rds")

#par province
# ATLAS_MT_2024_prov <- ATLAS_MT_2024_full |> 
#   select(Province, `Population (DPS 2023)`) |> rename(pop=2) |> mutate(Province=str_to_lower(Province)) |> 
#   group_by(Province) |> summarise(pop_totale=sum(pop)) |> rename(province=1)
# 
# saveRDS(ATLAS_MT_2024_prov, "ATLAS_MT_2024_prov.rds")

#projection 2024
population_2024_ZS <- read_excel("C:/Users/MERCYCORPS/Downloads/drc-hpc-projection-population-2024.xlsx") |> select(c(1:7))

test<-population_2024_ZS |> 
  select(Province, `Population 2024`) |> rename(pop=2) |> mutate(Province=str_to_lower(Province)) |> 
  group_by(Province) |> summarise(pop_totale=sum(pop)) |> rename(province=1)
test

saveRDS(test, "2024_prov.rds")


# #2020
# cod_admpop_adm1_2020 <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/Population/cod_admpop_adm1_2020.csv")
#                          
#                          
# create total DB ---------------------------------------------------------
# Ensure all columns have the same type for each dataset
nord_kivu <- nordkivu |> mutate(across(everything(), as.character))  # Convert all columns to character
sud_kivu_total <- sudkivu |> mutate(across(everything(), as.character))  # Convert all columns to character
ituri_total <- ituri |> mutate(across(everything(), as.character))  # Convert all columns to character
tanganyika_total <- tanganyika |> mutate(across(everything(), as.character))  # Convert all columns to character


combined_data <- bind_rows(nord_kivu, sud_kivu_total, ituri_total, tanganyika_total)

combined_data<-combined_data%>% dplyr::select(where(not_all_na))
colnames(combined_data)

combined_data<-merge(combined_data, ATLAS_MT_2024, by="TERRITOIRE", all.x=T)

combined_data <- combined_data %>%
  mutate(across(contains("Score"), ~ as.numeric(as.character(.))))

combined_data <-combined_data |> clean_names()

combined_data <-combined_data |> filter(!is.na(territoire))

table(combined_data$quarter)

write_rds(combined_data, "Total_BD_conflict.rds")


# test power BI -----------------------------------------------------------

# 
# test<-Total_BD_conflict |> filter(territoire=="Lubero" & annee=="2024" & mois=="12") 
# 
# table(Total_BD_conflict$territoire)
# 
# bd_conflict<-readRDS("Total_BD_conflict.rds")
# 
# test_powerBI_conflict<-bd_conflict |> filter(province %in% c("Tanganyika") & score_dacces_humanitaire>3 & 
#   annee=="2024" & mois=="12" & as.numeric(jour)<=15)
# 
# 
# table(test_powerBI_conflict$cible_categorie)
# 
# # maniema<-maniema |> clean_names()
# test_powerBI_conflict<-maniema |> filter(score_dacces_humanitaire>3 & 
#                                                annee==2024 & mois==12 & jour<=15)
# 
# table(test_powerBI_conflict$cible_categorie)
# # write.xlsx(test_powerBI_conflict, "test_powerBI_conflict.xlsx")
