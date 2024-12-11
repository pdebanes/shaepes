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

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")


GTACQ_monitoring<-readRDS("BD_GTACQ_monitoring.rds")


# indicateurs -------------------------------------------------------------

temp<-GTACQ_monitoring |> 
  select(province, territoire, annee, quarter, contains("score")) |> 
  pivot_longer(cols = -c(province, territoire, annee, quarter), names_to = "indicator") |> 
  filter(indicator %in% c("score_final_besoins", "score_nut", 	
                    "score_sante")) |> 
  mutate(indicator = recode(indicator, 
                            "score_final_besoins" = "H001", 
                            "score_nut" = "H002", 
                            "score_sante" = "H009")) |> 
  group_by(province, territoire, annee, quarter,indicator) |> 
  reframe(count=mean(value, na.rm=T)) 

saveRDS(temp, "indicateurs_BD_GTACQ.rds")
