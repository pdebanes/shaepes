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
                         

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds") 

#par province
# ATLAS_MT_2024_prov <- ATLAS_MT_2024_full |> 
#   select(Province, `Population (DPS 2023)`) |> rename(pop=2) |> mutate(Province=str_to_lower(Province)) |> 
#   group_by(Province) |> summarise(pop_totale=sum(pop)) |> rename(province=1)
# 
# saveRDS(ATLAS_MT_2024_prov, "ATLAS_MT_2024_prov.rds")

#projection 2024
population_2024_ZS <- read_excel("C:/Users/MERCYCORPS/Downloads/drc-hpc-projection-population-2024.xlsx") |> select(c(1:7))

test<-population_2024_ZS |> 
  select(Territoire, `Population 2024`) |> rename(pop=2) |> mutate(Territoire=str_to_lower(Territoire)) |> 
  group_by(Territoire) |> summarise(pop_totale=sum(pop)) |> rename(territoire=1)
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

combined_data<-combined_data |> clean_names() |> 
  mutate(zone_de_sante=str_to_lower(zone_de_sante),
        territoire=str_to_lower(territoire)
        )


# clean noms de territoire ------------------------------------------------
list_lubero<-c("musienene" ,   "alimbongo" ,   "kayna"  , "lubero" ,  "masereka" , "biena" ,  "manguredjipa")
list_beni<- c("mabalako" , "oicha" ,    "mutwanga",  "kalunguta", "kamango"  , "vuhovi",    "kyondo" ) 
test<-combined_data |> filter(territoire=="lubero" & !zone_de_sante %in% c(list_lubero, "butembo"))
test<-combined_data |> filter(territoire=="beni" & !zone_de_sante %in% c(list_beni, "beni"))

combined_data<-combined_data |> 
  mutate(territoire=ifelse(zone_de_sante %in% list_lubero, "lubero", territoire),
         territoire=ifelse(zone_de_sante %in% list_beni, "beni", territoire))

# combined_data |> filter(territoire=="nyiragongo") |> select(territoire, zone_de_sante) |> unique()
# combined_data |> filter(territoire=="beni") |> select(territoire, zone_de_sante) |> unique()
# combined_data |> filter(territoire=="lubero") |> select(territoire, zone_de_sante) |> unique()

#I want to create the territory beni-ville
combined_data<-combined_data |>
  mutate(territoire=ifelse(territoire=="beni" & zone_de_sante=="beni", "beni-ville", territoire),
         territoire=ifelse(zone_de_sante %in% c("butembo", "katwa"), "butembo", territoire),
         territoire=ifelse(zone_de_sante %in% c("goma", "karisimbi"), "goma", territoire))




combined_data<-merge(combined_data, ATLAS_MT_2024, by="territoire" , all.x=T)

combined_data <- combined_data %>%
  mutate(across(contains("Score"), ~ as.numeric(as.character(.))))

combined_data <-combined_data 

combined_data <-combined_data |> filter(!is.na(territoire))

table(combined_data$quarter)

table(combined_data$territoire)



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
