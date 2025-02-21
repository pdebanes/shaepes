###########################################################################
###########################################################################
###                                                                     ###
###                     indicateurs GTS                                 ###
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

GTS_raw <- read.xlsx("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/GTS 2023.xlsx")
complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 



# indicateurs -------------------------------------------------------------

temp<-GTS_raw |> 
  select(loc_1, safe_daily, cover_needs, resilience_impression, trust) |> 
  pivot_longer(cols = -c(loc_1), names_to = "indicator") |> 
  group_by(loc_1,indicator) |> 
  reframe(count=mean(value, na.rm=T)) |> 
  mutate(indicator = recode(indicator, 
                            "safe_daily" = "S007", 
                            "cover_needs" = "H011", 
                            "resilience_impression" = "H012",
                            "trust" = "H013")) |> 
  rename(province=loc_1) |> 
  mutate(province = recode(province, 
                            "nordkivu" = "nord-kivu", 
                            "sudkivu" = "sud-kivu")) 

# Define the additional rows for Tanganyika
tanganyika_rows <- tibble(
  province = "tanganyika",
  indicator = c("H011", "H012", "S007", "H013"),
  count = NA_real_  # Set the mean_score as empty (NA)
)

# Add the new rows to the existing dataset
temp <- bind_rows(temp, tanganyika_rows)


complete<-complete_frame_ind |> left_join(temp, by="province")

table(complete$province)


saveRDS(complete, "indicateurs_BD_GTS.rds")
