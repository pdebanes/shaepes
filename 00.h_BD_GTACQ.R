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


GTACQ_monitoring_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/GTACQ/GTACQ monitoring.xlsx")

GTACQ_Aout_2024_TOSHARE<- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/GTACQ/GTACQ_Proposition cadre analytique_Aout_2024_TOSHARE .xlsx",
                                     sheet = "BESOIN") |> mutate(Année=2024, Mois="Aout")

GTACQ_Septembre_2024_TOSHARE <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/GTACQ/GTACQ_Proposition cadre analytique_Septembre_2024_TOSHARE.xlsx",
                                           sheet = "BESOIN")|> mutate(Année=2024, Mois="Septembre")

GTACQ_octobre_2024_TOSHARE <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/GTACQ/GTACQ_Proposition cadre analytique_Octobre_2024_TOSHARE.xlsx",
                                           sheet = "BESOIN")|> mutate(Année=2024, Mois="Octobre")


GTACQ_monitoring <-rbind(GTACQ_monitoring_raw, GTACQ_Aout_2024_TOSHARE, GTACQ_Septembre_2024_TOSHARE,GTACQ_octobre_2024_TOSHARE)

month_map <- c(
  "Janvier" = 1, "Février" = 2, "Mars" = 3, "Avril" = 4, "Mai" = 5, "Juin" = 6,
  "Juillet" = 7, "Août" = 8, "Septembre" = 9, "Octobre" = 10, "Novembre" = 11, "Décembre" = 12, "Aout"=8
)

GTACQ_monitoring <-GTACQ_monitoring |> clean_names() |>   mutate(
  province = str_to_lower(province),
  territoire = str_to_lower(territoire),
  mois = recode(mois, !!!month_map),  # Map the French month names to numeric values
  mois = as.integer(mois),  # Ensure `mois` is numeric
  date = make_date(year = annee, month = mois, day = 1),  # Construct a date with the first day of the month
  quarter = paste0("T", quarter(date), "_", year(date)), 
  territoire=ifelse(territoire=="beni", "beni-ville", territoire),
    territoire=ifelse(territoire %in% c("oïcha", "oicha"), "beni", territoire)) |> 
  # select(province, territoire, annee, quarter, ) |> 
  mutate(
    across(
      matches("score"),  # Dynamically select columns containing "score"
      ~ as.numeric(gsub(",", ".", .))  # Replace commas with dots and convert to numeric
    )
  )

table(GTACQ_monitoring$territoire)

saveRDS(GTACQ_monitoring, "BD_GTACQ_monitoring.rds")
