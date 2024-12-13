############################################################################
############################################################################
###                                                                      ###
###                           INDICATEURS IPIS                           ###
###                                                                      ###
############################################################################
############################################################################

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

# data_raw_IPIS_incidents_old <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IPIS/2023_FilteredData_Kufatilia_IPIS.csv")

data_raw_IPIS_incidents <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IPIS/2024-12-12_FilteredData_Kufatilia_IPIS.csv")

# pas possible de merger parce que dans l'ancienne version, il n'y a pas les territoires

# data_raw_IPIS_incidents<-rbind(data_raw_IPIS_incidents_old, data_raw_IPIS_incidents_new)

data_raw_IPIS_visit <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/IPIS/cod_mines_curated_all_opendata_p_ipis.csv")

complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 


table(data_raw_IPIS_visit$province)


# more detailed database --------------------------------------------------



data_IPIS<-data_raw_IPIS_visit |> 
  mutate(
    quarter = paste0("T", lubridate::quarter(visit_date), "_", lubridate::year(visit_date)),
    annee=lubridate::year(visit_date),
    province = str_to_lower(province),
    territoire = str_to_lower(territoire)
  ) |> 
  select(annee, quarter, province, territoire,interference, armed_group1, frequency_armed_group1, armed_group2, frequency_armed_group2) |> 
  filter(annee>=2021 & interference==1 & province %in% c("nord-kivu", "sud-kivu", "tanganyika", "maniema", "ituri"))
  
# indicators visit--------------------------------------------------

table(data_raw_IPIS_visit$frequency_armed_group1)

#need to recode frequency
# Define a mapping for the 5-point Likert scale
likert_mapping <- c(
  "Permanent" = 5, "Permanente" = 5, 
  "Quotidien" = 5, "Plus d'une fois par semaine" = 5,
  "Une fois par semaine" = 4, "Hebdomadaire" = 4, "Chaque semaine" = 4,
  "Une fois tous les 15 jours" = 3, "Une fois par mois" = 3,
  "Trimestrielle" = 2, "Semestrielle" = 2,
  "Rarement" = 1, "Non permanente" = 1, "Occasionnelle" = 1, "Temporaire" = 1,
  "A l'improviste" = 1, "Aléatoire" = 1, "Brusquement" = 1,
  "Ils n'arrivent pas au site mais ils sécurisent l'aérodrome de Kikungwa" = 1,
  "Pas" = 1, "Moins d'une fois par mois" = 1, "Irrégulière" = 1, "Irregulière" = 1
)

# Recode the variable
data_IPIS <- data_IPIS %>%
  mutate(
    likert_scale1 = recode(frequency_armed_group1, !!!likert_mapping, .default = NA_real_),
    likert_scale2 = recode(frequency_armed_group2, !!!likert_mapping, .default = 0),
    final_scale = (coalesce(likert_scale1, 0) + likert_scale2) / 2  # Replace NA in likert_scale1 with 0
  )

# Check the result
table(data_IPIS$likert_scale1, useNA = "ifany")
table(data_IPIS$likert_scale2, useNA = "ifany")
table(data_IPIS$final_scale, useNA = "ifany")

#need to add a frame
data_IPIS<-data_IPIS |> 
  mutate(twogroups=ifelse(!is.na(armed_group1) & !is.na(armed_group2),1,0)) |> 
  group_by(annee, quarter, province, territoire) |> 
  reframe(
    E005A=n(), 
    E005C=sum(twogroups),
    E005B=mean(final_scale)
    
  ) |> 
  fill(E005B, .direction = "up") %>%  
  fill(E005B, .direction = "down") 
  
data_IPIS_frame<-complete_frame_ind |> 
  left_join(data_IPIS, by=c("province", "territoire", "quarter", "annee")) 

data_IPIS_frame <- data_IPIS_frame %>%
  group_by(province, territoire) %>%
  fill(starts_with("E005"), .direction = "updown") %>%  # Directly pass column selectors
  ungroup() |> 
  pivot_longer(cols = starts_with("E005"), names_to = "indicator", values_to = "count")


# test_frame<- data_IPIS %>%
#   select(annee, quarter, province, territoire) |> 
#   unique() |> 
#   group_by(province, territoire) %>%  # Group by province and territoire
#   slice_tail( n=1) %>%  # Keep the last value based on the year
#   ungroup() |> 
#   left_join(data_IPIS, by=c("annee", "quarter", "province", "territoire")) |> 
#   select(-quarter, -annee)

# indicators incidents--------------------------------------------------



tempE005D<-data_raw_IPIS_incidents |> 
  mutate(
    quarter = paste0("T", lubridate::quarter(date), "_", lubridate::year(date)),
    annee=lubridate::year(date),
    province = str_to_lower(province),
    territoire = str_to_lower(territoire)
  ) |> 
 group_by(annee, quarter, province, territoire) |> 
  reframe(
    count=n()
  )

tempE005D<-complete_frame_ind |> 
  left_join(tempE005D, by=c("province", "territoire", "quarter", "annee")) |> 
  mutate( indicator="E005D") |> 
  filter(annee>=2024) |> 
  group_by(province, territoire) %>%
  fill(count, .direction = "updown") 



data_IPIS_frame_tot<-rbind(data_IPIS_frame, tempE005D)
# total BD ----------------------------------------------------------------



saveRDS(data_IPIS_frame, "indicateurs_IPIS_fixed.rds")

