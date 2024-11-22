table(bd_conflict_raw$`Cible (categorie)`)


temp <- bd_conflict_raw %>%
  filter(`Auteur sous catégorie 1` == "M23" & `Score d'accès humanitaire` > 3 & PROVINCE=="Nord-Kivu" & 
           `Cible (categorie)` %in% c("Acteur armé non-étatique", "Autorités politico-administratives",
                                       "Forces armées nationales", "Forces armées internationales", "Personne armée non identifiée")) %>%
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
# Sauvegarder le graphique avec un fond blanc et haute résolution
ggsave(
  filename = "economist_style_incidents_M23_2024_NK.png",
  plot = plot,
  width = 12,
  height = 6,
  bg = "white",
  dpi = 300
)
