
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

bd_conflict<-readRDS("Total_BD_conflict.rds")


table(bd_conflict$cible_categorie)
# incidents graves NK vs autres  ----------------------------------------------
data_raw<-bd_conflict |> filter(score_dacces_humanitaire>3 & cible_categorie=="Population civile" & annee %in% c("2023", "2024"))
temp<-data_raw |> group_by(province, annee, quarter) |> reframe(count=n()) |> pivot_wider(names_from = province, values_from = count)


# incidents graves rutshuru  ----------------------------------------------
library(openxlsx)


data_raw<-bd_conflict |> filter(province=="Nord-Kivu" & score_dacces_humanitaire>3 & cible_categorie=="Population civile" & annee %in% c("2023", "2024"))

temp<-data_raw |> group_by(territoire, annee, quarter) |> reframe(count=n()) |> pivot_wider(names_from = territoire, values_from = count)
write.xlsx(temp, file="RGA_incidents.xlsx")

# types d'incident rutshuru  ----------------------------------------------
table(data_raw$territoire)
table(data_raw$categorie_dincident)
table(data_raw$type_dincident)

temp<-data_raw |> filter(territoire %in% c("Rutshuru" , "Lubero") & !quarter %in% c("T4_2023", "T4_2024")) |> group_by(territoire, annee, type_dincident) |> reframe(count=n()) |> pivot_wider(names_from = annee, values_from = count)
 write.xlsx(temp, file="RGA_type_incidents.xlsx")

