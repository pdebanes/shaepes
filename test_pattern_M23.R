
libraries <- c(
  "bannerCommenter", "readxl", "readr", "openxlsx", "tidyverse", 
  "data.table", "dplyr", "stringr", "conflicted", "quarto", "knitr", "janitor", "ISOweek", "forecast", "tseries"
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
conflicted::conflicts_prefer(lubridate::yday)
conflicted::conflicts_prefer(lubridate::week)
conflicted::conflicts_prefer(lubridate::isoweek)
conflicted::conflicts_prefer(lubridate::year)


bd_conflict_raw<-readRDS("Total_BD_conflict_4Ooct24.rds")

# incidents totaux du M23 
table(bd_conflict_raw$`Auteur sous catégorie 1`)
table(bd_conflict_raw$`Score d'accès humanitaire`)

temp<-bd_conflict_raw |> filter(`Auteur sous catégorie 1`=="M23") |> group_by(ANNEE, MOIS,`Score d'accès humanitaire`) |> reframe(count=n())

temp <- temp %>%
  mutate(ANNEE=as.numeric(ANNEE),
         MOIS=as.numeric(MOIS),
         ANNEE_MOIS = paste(ANNEE, MOIS, sep = "-"),
         score=as.character(`Score d'accès humanitaire`)) %>%
  arrange(ANNEE, MOIS) |> 
  filter(ANNEE>2022)




# Créer le graphique à barres empilées
ggplot(temp, aes(x = ANNEE_MOIS, y = count, fill = `Score d'accès humanitaire`)) +
  geom_bar(stat = "identity") +
  # scale_fill_viridis_d() +  # Utiliser l'échelle de couleurs viridis
  labs(
    title = "Nombre d'incidents par mois (M23) - Score d'accès humanitaire",
    x = "Année-Mois",
    y = "Nombre d'incidents",
    fill = "Score humanitaire"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des labels de l'axe x
  )


#regarder le total par jour pour voir un cycle

table(bd_conflict_raw$`Cible (categorie)`)

temp<-bd_conflict_raw |> filter(`Auteur sous catégorie 1`=="M23" & `Score d'accès humanitaire`>3 & !`Cible (categorie)`=="Population civile") |>
  group_by(ANNEE, MOIS, JOUR) |> reframe(count=n())

temp<-temp |>  mutate(date = make_date(year = ANNEE, month = MOIS, day = JOUR)) |> arrange(date)

temp<-temp |> filter(ANNEE=="2024") |> arrange(date)
# Créer le graphique à barres empilées

# Créer le graphique à barres avec une courbe de lissage (cycle)
ggplot(temp, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Bar chart to show incidents per day
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 1) +  # Loess smoothing to identify cycles
  labs(
    title = "Nombre d'incidents par jour (M23) - Score d'accès humanitaire",
    x = "Date",
    y = "Nombre d'incidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des labels de l'axe x pour meilleure lisibilité
  )


# par semaine -------------------------------------------------------------
table(bd_conflict_raw$PROVINCE)

temp <- bd_conflict_raw %>%
  filter(`Auteur sous catégorie 1` == "M23" & `Score d'accès humanitaire` > 3 & PROVINCE=="Nord-Kivu") %>%
  mutate(date = make_date(year = ANNEE, month = MOIS, day = JOUR)) %>%
  mutate(week = isoweek(date), year = year(date))  # Extraire la semaine ISO et l'année |> 
filter(!ANNEE=="2022")

# Créer une variable date en utilisant ANNEE, MOIS, JOUR
temp <- temp %>%
  group_by(year, week) %>%
  summarise(count = n(), .groups = 'drop') |> 
  filter(!(year==2022 & week==4)) |> 
  mutate(iso_week = paste(year, sprintf("%02d", week), sep = "-W"),  # Combiner année et semaine
         date = ISOweek2date(paste0(iso_week, "-1")))  # Convertir en date (lundi de la semaine)

temp<-temp |> filter(year==2024)
# Créer le graphique à barres avec la prévision ARIMA

# Créer le graphique à barres empilées avec une courbe de lissage
plot <- ggplot(temp, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "#0072B2", alpha = 0.6) +  # Utiliser une couleur similaire à The Economist
  geom_smooth(method = "loess", color = "#D55E00", se = FALSE, size = 1) +  # Couleur d'accent pour la ligne
  labs(
    title = "Evolution du nombre d'incidents à fort impact du M23\nHors population civile",
    x = "",
    y = "Nombre d'incidents"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +  # Supprimer l'espace supplémentaire au-dessus des barres
  theme_economist() +  # Appliquer le thème de The Economist
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 0.5, size = 8),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"  # Supprimer la légende si elle n'est pas nécessaire
  )

# Afficher le graphique
print(plot)

