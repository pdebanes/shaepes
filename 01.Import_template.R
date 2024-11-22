############################################################################
############################################################################
###                                                                      ###
###                      IMPORT TEMPLATE MONITORING                      ###
###                                                                      ###
############################################################################
############################################################################


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


TEMPLATE_V5_SHAEPES_MONITORING <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/data Q2/TEMPLATE V5 SHAEPES MONITORING.xlsx", 
                                                  sheet = "Dataset_terr. 1", skip = 3)

TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> select(-`Indicator Weight`, - `Indicator Impact Score`, -weighted_indicator_impact_score) |> clean_names()

TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> 
  mutate(ind_impact_score=ifelse(result_value/denominator<=threshold_level_1, 0, 
                                 ifelse(result_value/denominator>threshold_level_1 & result_value/denominator<=threshold_level_2, 2, 
                                        ifelse(result_value/denominator>threshold_level_2 & result_value/denominator<=threshold_level_3, 3,
                                               ifelse(result_value/denominator>threshold_level_3 & result_value/denominator<=threshold_level_4, 4,
                                                      ifelse(result_value/denominator>threshold_level_4 & result_value/denominator<=threshold_level_5, 5, "NA"))))))
# TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> select(-indicator_status)


#pour l'instant je laisse comme ça mais idée qu'il va en fait falloir que cette feuille soit un output avec la variable weighted_indicator_impact_score
