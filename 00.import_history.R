
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



# SHAEPES_history <- read_excel("~/DRC-CAT/SHAEPES/data Q2/Test V4 SHAEPES.xlsx")
Uvira <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING - tentative Uvira.xlsx", 
                                                             sheet = "Analysis_terr. 1", skip = 9) |> mutate(territoire="Uvira", trimestre="Q2_2024")

fizi <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING FIZI.xlsx", 
                                                  sheet = "Analysis_terr. 1", skip = 9)|> mutate(territoire="Fizi", trimestre="Q2_2024")

irumu <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING IRUMU.xlsx", 
                                                   sheet = "Analysis_terr. 1", skip = 9)|> mutate(territoire="Irumu", trimestre="Q2_2024")

total_reset_q2<-rbind(Uvira, fizi, irumu) |> clean_names()|>  dplyr::rename(w_indic=weighted_indicator_impact_score) |> relocate(territoire)




# compute score by dimension ----------------------------------------------
table(total_reset_q2$w_indic)
total_score_dim<-total_reset_q2 |> mutate(w_indic=as.numeric(w_indic))|> group_by(territoire, trimestre, dimension) |> reframe(score_dim=sum(w_indic))


# weighting of program RESET ----------------------------------------------
table(total_score_dim$dimension)
dim_weight_reset <- data.frame(
  dim_name = c("Security", 
               "Humanitarian", 
               "Access", 
               "Political and social inclusion", 
               "Environmental Hazards", 
               "Economic"),
  dim_weight = c(0.25, 0.25, 0.15, 0.1, 0.1, 0.15)
)



	
