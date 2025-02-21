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
conflicted::conflicts_prefer(lubridate::year)

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")
DTM_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IOM DTM/DTM.xlsx")
DTM_Overview_Sep24_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IOM DTM/DTM_DRC_NationalDisplacementOverview_Sep24.xlsx", 
                                 sheet = "Summary - Health Zone") |> 
  filter(admin_1_label %in% c("Nord-Kivu", "Ituri", "Sud-Kivu", "Tanganyika"))


DTM_Overview_t3_2023 <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IOM DTM/DTM_DRC_DISP_ATLAS_R2_2023_DB_PUBLIC_20231106.xlsx", 
                                     sheet = "AGG_BY_ZS") |> 
  filter(admin_1_label %in% c("Nord-Kivu", "Ituri", "Sud-Kivu", "Tanganyika"))




table(DTM_Overview_Sep24_raw$admin_1_label)
table(DTM_Overview_Sep24_raw$admin_2_label)
table(DTM_Overview_t3_2023$admin_2_label)

#### possibilité d'avoir des données plus récentes avec le national displacement overview mais ça paraît mieux de faire l'importation direct dans R


complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds") |> clean_names() |> mutate(territoire=str_to_lower(territoire))

DTM_raw<-DTM_raw |> mutate(admin_2_label=ifelse(admin_2_label%in% c("BENI (Territoire / Oicha)", "Beni Territoire"), "Beni", admin_2_label),
                           admin_2_label=ifelse(admin_2_label %in% c("BENI (Ville)", "Beni (Ville)"), "Beni-ville", admin_2_label),
                           admin_2_label=ifelse(admin_2_label%in% c("BUTEMBO (Ville)", "Butembo (Ville)"), "Butembo", admin_2_label))

DTM_Overview_Sep24_raw<-DTM_Overview_Sep24_raw |> mutate(admin_2_label=ifelse(admin_2_label=="BENI (Territoire / Oicha)", "Beni", admin_2_label),
                           admin_2_label=ifelse(admin_2_label=="BENI (Ville)", "Beni-ville", admin_2_label),
                           admin_2_label=ifelse(admin_2_label=="BUTEMBO (Ville)", "Butembo", admin_2_label))

DTM_Overview_t3_2023<-DTM_Overview_t3_2023 |> mutate(admin_2_label=ifelse(admin_2_label%in% c("BENI (Territoire / Oicha)", "Beni Territoire"), "Beni", admin_2_label),
                                                     admin_2_label=ifelse(admin_2_label %in% c("BENI (Ville)", "Beni (Ville)"), "Beni-ville", admin_2_label),
                                                     admin_2_label=ifelse(admin_2_label%in% c("BUTEMBO (Ville)", "Butembo (Ville)"), "Butembo", admin_2_label))

table(DTM_Overview_Sep24_raw$admin_2_label)
table(DTM_Overview_t3_2023$admin_2_label)
# load DB -----------------------------------------------------------------

DTM <- DTM_raw |> 
  # filter(!str_detect(admin_2_label, "(Ville)")) |>
  mutate(
           quarter = paste0("T", lubridate::quarter(Date), "_", lubridate::year(Date)),
         annee=lubridate::year(Date))|> 
  rename(province=admin_1_label, territoire=admin_2_label)|> 
  mutate(province=str_to_lower(province),
         territoire=str_to_lower(territoire)
         # territoire=ifelse(grepl("beni",territoire), "beni", territoire)
         ) |>
  filter(!is.na(province)) |> 
  clean_names() 

DTM <- DTM |> 
  mutate(H004t=pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus/population_dps_2023,
         H004bt=total_pdi_ind_fa36_sites_cccm_sites_spont/population_dps_2023) |> 
  select(province, territoire, annee, quarter, H004t, H004bt)

## DTM T3 2023

DTMt3_2023  <- DTM_Overview_t3_2023  |> 
  # filter(!str_detect(admin_2_label, "(Ville)")) |>
  mutate(
    quarter ="T3_2023",
    annee=2023)|> 
  rename(province=admin_1_label, territoire=admin_2_label)|> 
  mutate(province=str_to_lower(province),
         territoire=str_to_lower(territoire)
         # territoire=ifelse(grepl("beni",territoire), "beni", territoire)
  ) |>
  filter(!is.na(province)) |> 
  clean_names() 

DTMt3_2023 <- DTMt3_2023 |> 
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
  # filter(!str_detect(territoire, "(ville)")) |>
  # mutate(territoire=ifelse(territoire=="beni (territoire / oicha)","beni", territoire)) |> 
  select(province, territoire, annee, quarter, total_pdi_fa36_sites_cccm_sites_spont, pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus) |> 
  left_join(ATLAS_MT_2024, by="territoire") |> 
  mutate(H004t=pd_is_en_famille_daccueil_arrivees_derniers_36_mois_individus/pop_totale,
         H004bt=total_pdi_fa36_sites_cccm_sites_spont/pop_totale) |> 
  select(province, territoire, annee, quarter, H004t, H004bt)

summary(DTM_Overview_Sep24)

# test<-DTM_Overview_Sep24 |> filter(is.na(H004t)) |> unique()

DTM_total<-rbind(DTM, DTM_Overview_Sep24, DTMt3_2023)

DTM_total_frame<-complete_frame_ind |> 
  left_join(DTM_total, by=c("province", "territoire", "quarter", "annee"))

DTM_total_frame<-DTM_total_frame |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(H004=mean(H004t, na.rm=T),
          H004b=mean(H004bt, na.rm=T)) |> 
  pivot_longer(
    cols=c(H004, H004b),
    names_to = "indicator",
    values_to = "count"
  )

summary(DTM_total_frame)


# missing values vs pas de déplacement ------------------------------------
#dans l'idée, il faut des 0 si le territoire n'a jamais été dans la DTM sinon, on est sur du NA 



# indicateurs_DTM<-readRDS("indicateurs_DTM.rds")

list_dtm<-DTM_total |> select(territoire) |> arrange(territoire) |> unique() |> pull()

DTM_total_frame<-DTM_total_frame |> mutate(
  count=ifelse(!territoire %in% list_dtm & is.na(count), 0, count)
)

table(DTM_total_frame$territoire)

saveRDS(DTM_total_frame, "indicateurs_DTM.rds")

#pour T1 2024 on pourrait faire un model avec le nombre total de personnes déplacées de l'API

# summary(DTM$as.numeric(`PRESSION DEMOGRAPHIQUE TOTALE`))

# test threshold

dtm<-readRDS("indicateurs_DTM.rds")
  

# API IOM -----------------------------------------------------------------

#on ne peut voir que les personnes déplacées
# install.packages("dtmapi")
library(dtmapi)

# countries_df <- get_all_countries()
# head(countries_df)
# operations_df <- get_all_operations()
# head(operations_df)
# 
# # Get IDP Admin 2 Data for Lebanon
# idp_admin2_df <- get_idp_admin2_data( CountryName='Democratic Republic of the Congo')
# head(idp_admin2_df)

# 
# idp_admin2_df <- get_idp_admin2_data(CountryName='Democratic Republic of the Congo', FromRoundNumber=1, ToRoundNumber=10)
# 
# idp_admin2_df<-idp_admin2_df |> rename(province=admin1Name, territoire=admin2Name, annee=yearReportingDate) |> 
#   mutate(quarter=paste0("T", lubridate::quarter(reportingDate), "_", lubridate::year(reportingDate))) |> 
#   select(province, territoire, numPresentIdpInd, annee, quarter) |> 
#   filter(annee>2022)
# 
# idp_admin2_df<-idp_admin2_df |> 
#   mutate(province=str_to_lower(province), territoire=str_to_lower(territoire)) |> 
#   filter(province %in% c("sud-kivu", "ituri", "nord-kivu"))
# 
# table(idp_admin2_df$province)
# # head(idp_admin2_df)
# 
# # on fait un model H004/H004b en fonction du nombre de réfugiés
# 
# test_reg<-merge(DTM_total_frame, idp_admin2_df, by.x=