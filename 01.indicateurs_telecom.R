###########################################################################
###########################################################################
###                                                                     ###
###                     indicateurs TELECOM                             ###
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
conflicted::conflicts_prefer(lubridate::year)

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")

telecom_raw <- read.xlsx("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/Telecom data T2 23.xlsx")
complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 

telecom_raw <-telecom_raw |> clean_names()|>mutate(province=gsub(" ", "", str_to_lower(provinces)))

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds") 
complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )


table(telecom_raw$province)
table(ATLAS_MT_2024$province)

# Define province name corrections
province_corrections <- c(
  "basuele" = "bas-uele",
  "equateur" = "equateur",
  "hautkatanga" = "haut-katanga",
  "hautlomami" = "haut-lomami",
  "hautuele" = "haut-uele",
  "ituri" = "ituri",
  "kasai" = "kasaï",
  "kasaicentral" = "kasaï-central",
  "kasaioriental" = "kasaï-oriental",
  "kinshasa" = "kinshasa",
  "kongocentral" = "kongo-central",
  "kwango" = "kwango",
  "kwilu" = "kwilu",
  "lomami" = "lomami",
  "lualaba" = "lualaba",
  "mai-ndombe" = "maï-ndombe",
  "maniema" = "maniema",
  "mongala" = "mongala",
  "nord-kivu" = "nord-kivu",
  "nord-ubangi" = "nord-ubangi",
  "sankuru" = "sankuru",
  "sud-kivu" = "sud-kivu",
  "sud-ubangi" = "sud-ubangi",
  "tanganyika" = "tanganyika",
  "tshopo" = "tshopo",
  "tshuapa" = "tshuapa"
)

# Clean and standardize province names in telecom_raw
telecom_raw <- telecom_raw %>%
  clean_names() %>%
  mutate(
    province = gsub(" ", "", str_to_lower(province)), # Remove spaces and lowercase
    province = recode(province, !!!province_corrections) # Apply corrections
  )

ATLAS_MT_2024<-ATLAS_MT_2024 |> group_by(province) |> reframe(pop_totale=sum(pop_totale))

telecom_raw <-telecom_raw  |>  left_join(ATLAS_MT_2024, by="province") |> select(-provinces)


# pop_test <- read.xlsx("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/Population/drc-hpc-projection-population-2024.xlsx")

complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 


# indicateurs -------------------------------------------------------------

# E007	Pourcentage moyen de couverture des services de télécommunications (ex. : réseau mobile) dans la province durant la période de référence.
# E008	Pourcentage moyen de la population ayant accès aux services de mobile money dans la province durant la période de référence.


# revenu global / total revenu --------------------------------------------

temp<-telecom_raw |> group_by(quarter, annee) |>  mutate(E007=nombre_dabonnes_mobile_abonnements_globaux/pop_totale,
                                                  E008=nb_abonnes_mobile_money_abonnements_monnaie_mobile/pop_totale) |>
  ungroup()|> select(quarter,annee, province,E007 , E008) 





complete<-complete_frame_ind |> left_join(temp, by=c("annee", "quarter", "province"))

complete<-complete |> arrange(annee, quarter, territoire, province) |> group_by(province, territoire) |> 
  fill(E007, .direction = "downup") %>% 
  fill(E008, .direction = "downup") %>% # Fill missing rolling_sd_4 values with the latest value
  ungroup()


complete<-complete |> 
  pivot_longer(cols = -c(quarter,annee, province, territoire), names_to = "indicator", values_to = "count") 

table(complete_frame_ind$province)
table(temp$province)

# 
saveRDS(complete, "indicateurs_BD_telecom.rds")
