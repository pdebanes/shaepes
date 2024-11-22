###########################################################################
###########################################################################
###                                                                     ###
###                           BD DTM                                    ###
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
DTM_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD Congo HAT - SHAEPES ALL/DTM.xlsx")
DTM_Overview_Sep24_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD Congo HAT - SHAEPES ALL/IOM DTM/DTM_DRC_NationalDisplacementOverview_Sep24.xlsx", 
                                 sheet = "Summary - Health Zone")


#### possibilité d'avoir des données plus récentes avec le national displacement overview mais ça paraît mieux de faire l'importation direct dans R


complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds") |> clean_names() |> mutate(territoire=str_to_lower(territoire))

# load DB -----------------------------------------------------------------

DTM <- DTM_raw |> 
  filter(!str_detect(admin_2_label, "(Ville)")) |>
  mutate(
           quarter = paste0("T", lubridate::quarter(Date), "_", lubridate::year(Date)),
         annee=lubridate::year(Date))|> 
  rename(province=admin_1_label, territoire=admin_2_label)|> 
  mutate(province=str_to_lower(province),
         territoire=str_to_lower(territoire),
         territoire=ifelse(grepl("beni",territoire), "beni", territoire)) |>
  filter(!is.na(province)) |> 
  clean_names() 

DTM <-DTM |> 
  mutate(H004t=pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus/population_dps_2023,
         H004bt=total_pdi_ind_fa36_sites_cccm_sites_spont/population_dps_2023) |> 
  select(province, territoire, annee, quarter, H004t, H004bt)
  
  

DTM_Overview_Sep24<-DTM_Overview_Sep24_raw |> 
  mutate(Date = lubridate::mdy("9-19-2024"),
         quarter = paste0("T", lubridate::quarter(Date), "_", lubridate::year(Date)),
         annee=lubridate::year(Date))|> 
  rename(province=admin_1_label, territoire=admin_2_label)|> 
  mutate(province=str_to_lower(province),
         territoire=str_to_lower(territoire)) |>
  filter(!is.na(province)) |> 
  clean_names() |> 
  filter(!str_detect(territoire, "(ville)")) |>
  select(province, territoire, annee, quarter, total_pdi_fa36_sites_cccm_sites_spont, pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus) |> 
  left_join(ATLAS_MT_2024, by="territoire") |> 
  mutate(H004t=pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus/pop_totale,
         H004bt=total_pdi_fa36_sites_cccm_sites_spont/pop_totale) |> 
  select(province, territoire, annee, quarter, H004t, H004bt)

DTM_total<-rbind(DTM, DTM_Overview_Sep24)

DTM_total<-complete_frame_ind |> 
  left_join(DTM_total, by=c("province", "territoire", "quarter", "annee"))

DTM_total<-DTM_total |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(H004=mean(H004t, na.rm=T),
          H004b=mean(H004bt, na.rm=T)) |> 
  pivot_longer(
    cols=c(H004, H004b),
    names_to = "indicator",
    values_to = "count"
  )

saveRDS(DTM_total, "indicateurs_DTM.rds")

# summary(DTM$as.numeric(`PRESSION DEMOGRAPHIQUE TOTALE`))

#more recent data
  

# API IOM -----------------------------------------------------------------

#on ne peut voir que les personnes déplacées
# install.packages("dtmapi")
# library(dtmapi)
# 
# countries_df <- get_all_countries()
# head(countries_df)
# operations_df <- get_all_operations()
# head(operations_df)
# 
# # Get IDP Admin 2 Data for Lebanon
# idp_admin2_df <- get_idp_admin2_data( CountryName='Democratic Republic of the Congo')
# head(idp_admin2_df)
# 
# 
# idp_admin2_df <- get_idp_admin2_data(CountryName='Democratic Republic of the Congo', FromRoundNumber=1, ToRoundNumber=10)
# head(idp_admin2_df)