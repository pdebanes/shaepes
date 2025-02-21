###########################################################################
###########################################################################
###                                                                     ###
###                          INDICATEURS HNO                            ###
###                                                                     ###
###########################################################################
###########################################################################



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


# load dataset ---------------------------------------------------------


jiaf_drc_2025 <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/HNO/jiaf_drc_2025.xlsx",
                               skip = 1,  sheet = "WS - 3.2 Intersectoral Severity") |>
  clean_names() |>
  select(starts_with("admin"), severite_definitive) |> 
  mutate(annee=2025)




drc_hno_2024 <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/HNO/drc_hno_2024.csv",
                              skip = 1, locale = locale(encoding = "latin1")) |> clean_names() |>
  select(c(1:7)) |> rename(severite_definitive=sgs_ej) |> filter(!is.na(province)) |> 
  mutate(annee=2024)

# we want to stack the datasets
colnames(jiaf_drc_2025)<-colnames(drc_hno_2024)


temp<-rbind(drc_hno_2024, jiaf_drc_2025)

temp<-temp |> group_by(province, territoire, annee) |> reframe(count=mean(severite_definitive, na.rm=T)) |> mutate(province=str_to_lower(province),
                                                                                                                   territoire=str_to_lower(territoire))

#load combined frame
complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) 



complete<-complete_frame_ind |> left_join(temp, by=c("province", "territoire", "annee"))

complete<-complete |> arrange(annee, quarter, territoire, province) |> group_by(province, territoire) |> 
  fill(count, .direction = "downup") |> 
  mutate(indicator="H008") 

table(complete$territoire)



saveRDS(complete, "indicateurs_HNO.rds")
