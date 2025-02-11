###########################################################################
###########################################################################
###                                                                     ###
###                     indicateurs GTACQ                               ###
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
conflicted::conflicts_prefer(lubridate::quarter)

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")


Monitoring_protection_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/Monitoring protection.xlsx", 
                                    sheet = "Monitoring incidents")

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds") |> mutate(TERRITOIRE=str_to_lower(TERRITOIRE)) |> clean_names()

complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )
# clean and prepare -------------------------------------------------------
ATLAS_MT_2024 <- ATLAS_MT_2024 |> distinct(territoire, .keep_all = TRUE)

  Monitoring_protection<-Monitoring_protection_raw |> clean_names() |> 
    mutate(date = as.Date(date), 
           province=str_to_lower(province),
           territoire=str_to_lower(territoires),
           quarter = paste0("T", quarter(date), "_", year(date)),
           annee=year(date)) |> 
           select(province, territoire, annee, quarter,contains( "total")) |> 
    pivot_longer(cols = -c(province, territoire, annee, quarter), names_to = "indicator") |> 
    mutate(indicator = recode(indicator,  "total" = "H006", 
                                          "total_vsbg" = "H006a", 
                                          "total_16_12" = "H006b"),
           province=recode(province, "nord kivu"="nord-kivu",     
                                     "sud kivu"="sud-kivu"))|> 
    left_join(ATLAS_MT_2024, by="territoire")|> 
    group_by(province, territoire, annee, quarter,indicator)|> 
    reframe(count=sum(value, na.rm=T)*100000/pop_totale) |> 
    ungroup()|> 
    unique() |> 
  filter(province %in% c("ituri",   "nord-kivu",   "sud-kivu", "tanganyika")) 

table(Monitoring_protection$province)

list_ind<-Monitoring_protection |> select(indicator) |> unique() |> pull()
list_ind

head(complete_frame_ind)
# créer un frame pour tous les trimestres  --------------------------------

complete_frame_ind <- complete_frame_ind %>%
  expand_grid(indicator = list_ind)


Monitoring_protection_tot<-complete_frame_ind |> 
  left_join(Monitoring_protection, by=c("province", "territoire", "quarter", "annee", "indicator"))

saveRDS(Monitoring_protection_tot, "indicateurs_BD_protection.rds")
  