###########################################################################
###########################################################################
###                                                                     ###
###         INDICATEURS FIXES                                           ###
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
complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 


# pour les indicateurs POL  -----------------------------------------------

table1 <- data.frame(
  province="sud-kivu",
  territoire = c("uvira", "fizi"),
  annee = c(2023, 2023),
  count = c(1.097, 1.200),
  # indicator = c("P007", "P007"),
  stringsAsFactors = FALSE
)

temp1<-complete_frame_ind |> left_join(table1, by=c("annee",  "province", "territoire")) |> mutate(indicator="P007")

temp1<-temp1 |> arrange(annee, quarter, territoire, province) |> group_by(province, territoire) |> 
  fill(count, .direction = "downup") %>% 
  ungroup()

  table2 <- data.frame(
    province="sud-kivu",
    territoire = c("uvira", "fizi"),
    annee = c(2023, 2023),
    count = c(2.815, 2.329),
    indicator = "P008",
    stringsAsFactors = FALSE
  )

  temp2<-complete_frame_ind |> left_join(table2, by=c("annee",  "province", "territoire")) |> mutate(indicator="P008")
  
  temp2<-temp2 |> arrange(annee, quarter, territoire, province) |> group_by(province, territoire) |> 
    fill(count, .direction = "downup") %>% 
    ungroup()

  table3 <- data.frame(
    province= c("nord-kivu", "sud-kivu", "ituri"), 
    count = c(69, 46, 37),
    annee = 2023,
    stringsAsFactors = FALSE
  )
  
  temp3<-complete_frame_ind |> left_join(table3, by=c("annee",  "province")) |> mutate(indicator="P005")
  
  temp3<-temp3 |> arrange(annee, quarter, province, indicator) |> group_by(province,  indicator) |> 
    fill(count, .direction = "downup") %>% 
    ungroup()
  
  
  temp4<-rbind(temp1, temp2, temp3)
  
  
  saveRDS(temp4, "indicateurs_fixes.rds")

  

  

  
  
  


