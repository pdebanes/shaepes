conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)


MEAL_URGENCES_For_CAT <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/CPTA/MEAL_URGENCES_For CAT.xlsx")
MEAL_URGENCES_For_CAT <- MEAL_URGENCES_For_CAT |> rename(date=1, 
                                                         q1_sensib_distrib=2,
                                                         q49_satisfait_distrib=3,
                                                         q52_mecanismes=4, 
                                                         q57_reponse_plainte=5, 
                                                         q61_bonne_info=6) |> clean_names() |> filter(!is.na(date))

# table(MEAL_URGENCES_For_CAT$Territoire)

MEAL_URGENCES_For_CAT <- MEAL_URGENCES_For_CAT |>  mutate(quarter = paste0("T", lubridate::quarter(date), "_", lubridate::year(date)),
                                                          year=lubridate::year(date))


questions <- c("q1_sensib_distrib", "q49_satisfait_distrib", "q52_mecanismes", "q57_reponse_plainte", "q61_bonne_info")

question_labels <- c(
  "q1_sensib_distrib" = "% des participants qui déclarent avoir eu suffisamment d'information avant le lancement de la distribution",
  "q49_satisfait_distrib" = "% des répondants qui sont satisfaits de la distribution",
  "q52_mecanismes" = "% des participants au programme qui étaient au courant de l’existence des mécanismes de gestion de feedbacks",
  "q57_reponse_plainte" = "% des répondants qui ont confirmé qu’ils ont reçu les réponses à leurs feedbacks",
  "q61_bonne_info" = "% des participants qui déclarent que le processus d’organisation de la distribution leur a été suffisamment bien expliqué"
)


# Création d'une liste pour stocker les résultats
results <- list()

for (question in questions) {
  # Agrégation et calcul des pourcentages pour chaque question
  temp <- MEAL_URGENCES_For_CAT |> 
    group_by(province, territoire, year, quarter, .data[[question]]) |> 
    rename(choix=.data[[question]]) |> 
    filter(!choix=="---") |> 
    reframe(count = n()) 
  
  temp <- temp |> 
    group_by(province, territoire, quarter) |> 
    mutate(total = sum(count)) |> 
    ungroup() |> 
    mutate(perc = count / total,
    indicator=question,
    label = question_labels[question]) |> 
    select(indicator, label, province, territoire, quarter, year, choix, perc )
  
  # Stockage des résultats pour chaque question
  results[[question]] <- temp
}

###############
# Flatten the results list
flat_results <- bind_rows(results, .id = "question")



write.xlsx(flat_results, "total_cpta_urgences.xlsx")
###############



# Création d'un fichier Excel avec plusieurs feuilles
output_file <- "results_CPTA_MEAL.xlsx"
wb <- createWorkbook()

for (question in names(results)) {
  addWorksheet(wb, sheetName = question)
  writeData(wb, sheet = question, x = results[[question]])
}

# Sauvegarder le fichier Excel
saveWorkbook(wb, file = output_file, overwrite = TRUE)


# analyse -----------------------------------------------------------------

temp<- flat_results |> select(-year) |>  pivot_wider(names_from = quarter, values_from = perc) 


write.xlsx(temp, "total_cpta_urgences.xlsx")
