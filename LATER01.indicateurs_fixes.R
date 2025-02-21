###########################################################################
###########################################################################
###                                                                     ###
###                          INDICATEURS FIXES                          ###
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


# indicateurs HHI 2023---------------------------------------------------------

table <- data.frame(
  territoire = c("uvira", "fizi"),
  annee = c(2023, 2023),
  count = c(1.097, 1.200),
  indicator = c("P007", "P007"),
  stringsAsFactors = FALSE
)

table2 <- data.frame(
  territoire = c("uvira", "fizi"),
  annee = c(2023, 2023),
  count = c(2.815, 2.329	),
  indicator = c("P008", "P008"),
  stringsAsFactors = FALSE
)

table3<-rbind(table, table2)

# indicateurs HHI 2024---------------------------------------------------------

HHI_Dec2024 <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/CPTA/Fw_ HHI collecte de données/TM-VF-Poll_DBCheck.Dec2024.xlsx") |> clean_names()

colnames(HHI_Dec2024) <- sub("^\\d+_\\d+_", "", colnames(HHI_Dec2024))
