############################################################################
############################################################################
###                                                                      ###
###                               BD REACH                               ###
###                                                                      ###
############################################################################
############################################################################


# Libraries ---------------------------------------------------------------



libraries <- c(
  "bannerCommenter", "readxl", "readr", "openxlsx", "tidyverse", 
  "data.table", "dplyr", "stringr", "conflicted", "quarto", "knitr", "janitor", "lubridate"
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



# load bd -----------------------------------------------------------------

MEB <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD Congo HAT - SHAEPES ALL/Monitoring REACH.xlsx", 
                               sheet = "MEB Dataset") |> 
  select(c(1:4)) |>   
  mutate(quarter = paste0("T", lubridate::quarter(Date), "_", lubridate::year(Date)),
          annee=lubridate::year(Date)) |> 
  clean_names() |> 
  rename(meb=cout_median_du_meb) |> 
  mutate(         province=str_to_lower(province)) |> 
  filter(province %in% c("ituri",   "nord-kivu",   "sud-kivu", "tanganyika")) |> 
  mutate(meb=as.numeric(meb))

PAM <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD Congo HAT - SHAEPES ALL/Monitoring REACH.xlsx", 
                               sheet = "PAM data") |> 
  select(c(1:5)) |>   
  mutate(quarter = paste0("T", lubridate::quarter(Date), "_", lubridate::year(Date)),
         annee=lubridate::year(Date)) |> 
  clean_names() |> select(-region) |> 
  rename(pam=cout_median_du_pma_total) |> 
         mutate(         province=str_to_lower(province)) |> 
  filter(province %in% c("ituri",   "nord-kivu",   "sud-kivu", "tanganyika")) |> 
  mutate(pam=as.numeric(pam))



saveRDS(MEB, "REACH_MEB.rds")
saveRDS(PAM, "REACH_PAM.rds")
