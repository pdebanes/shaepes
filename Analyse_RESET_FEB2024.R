###########################################################################
###########################################################################
###                                                                     ###
###                       ANALYSE RESET                                 ###
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



total_score_dim<-read_rds("shaepes_weighted_db.rds") |> 
  filter(territoire %in% c( "beni-ville", "irumu", "uvira") & annee=="2024") |> 
  select(territoire, quarter, group, score_dim) |> 
  pivot_wider(names_from = territoire, values_from = score_dim) |> 
  relocate(quarter, group, irumu)

total_shaepes<-read_rds("shaepes_final_score.rds") |> filter(territoire %in% c( "beni-ville", "irumu", "uvira")& annee=="2024")|> 
  select(-province) |> 
  pivot_wider(names_from = territoire, values_from = score_tot)


write.xlsx(total_score_dim, "total_score_dim_reset.xlsx")
write.xlsx(total_shaepes, "total_shaepes_reset.xlsx")



# security ----------------------------------------------------------------
stacked_indicators<-read_rds("stacked_indicators.rds")

irumu<-stacked_indicators |> filter(territoire=="irumu")

BD_conflict<-read_rds("Total_BD_conflict.rds") 

irumu_conflict<-BD_conflict|> filter(territoire=="irumu", annee==2024 & score_dacces_humanitaire>3 & quarter %in% c("T3_2024", "T4_2024"))
table(irumu_conflict$type_dincident)

beni_conflict<-BD_conflict|> filter(territoire=="beni-ville", annee==2024  & quarter %in% c("T3_2024", "T4_2024"))
table(beni_conflict$categorie_dincident)

uvira_conflict<-BD_conflict|> filter(territoire=="uvira", annee==2024  & quarter %in% c("T3_2024", "T4_2024"))
table(uvira_conflict$categorie_dincident)


camembert<-BD_conflict |> 
  filter(territoire %in% c( "beni-ville", "irumu", "uvira")& quarter %in% c("T3_2024", "T4_2024")) |> 
  group_by(territoire, auteur_sous_categorie_1) |>
  reframe(sum_aut=sum(n())) |> 
  pivot_wider(names_from = territoire, 
              values_from = sum_aut)

write.xlsx(camembert, "camembert_reset.xlsx")

camembert2<-BD_conflict |> 
  filter(territoire %in% c( "beni-ville", "irumu", "uvira")& quarter %in% c("T3_2024", "T4_2024")  & score_dacces_humanitaire>3) |> 
  group_by(territoire, auteur_sous_categorie_1) |>
  reframe(sum_aut=sum(n())) |> 
  pivot_wider(names_from = territoire, 
              values_from = sum_aut)

write.xlsx(camembert2, "camembert_reset2.xlsx")



