#avec FARDC

table(bd_conflict_raw$`Auteur (categorie)`)
table(bd_conflict_raw$`Auteur sous catégorie 1`[bd_conflict_raw$PROVINCE=="Nord-Kivu"  ])
table(bd_conflict_raw$`Auteur sous catégorie 2`[bd_conflict_raw$PROVINCE=="Nord-Kivu"  ])

wazalendo_groups <- c(
  "Nyatura ANCDH", "Nyatura CMC", "NDC-R/Guidon",
  "FDLR (FOCA et RUD)", "APCLS", "Nyatura UFDPC",
  "Mac Mbura", "MPA", "UPCRN", "FPC de Domi et Thadee",
  "Mai-mai PARECO", "Mai-mai PARECO/FF", "FDDH",
  "Mai-Mai UPLC","Mai-mai UPLC", "FPP/AP",
  "Mai-mai Mai-Mazembe de Gilbert", "Mai-mai Mazembe Yira", 
)


temp <- bd_conflict_raw %>%
  mutate(Acteurs=ifelse(`Auteur (categorie)` %in% c("Forces armées nationales", "Forces armées internationales"), "Forces armées étatiques", 
                      ifelse(`Auteur sous catégorie 1` %in% c("M23", "m23"), "M23", 
                             ifelse( `Auteur sous catégorie 2` %in% wazalendo_groups, "Wazalendo", "Autres GANE")))) |> 
  filter(  `Score d'accès humanitaire` > 3 & PROVINCE=="Nord-Kivu" &
           `Cible (categorie)` %in% c("Acteur armé non-étatique", "Autorités politico-administratives",
                                      "Forces armées nationales", "Forces armées internationales", "Personne armée non identifiée") &
            ! `Auteur (categorie)` %in% c("Population civile", "Societé civile / Groupe de Pression", "Acteur HDP", "Personne armée non identifiée")) %>%
  mutate(date = make_date(year = ANNEE, month = MOIS, day = JOUR))%>%
  mutate(week = isoweek(date), year = year(date))|>   # Extraire la semaine ISO et l'année 
filter(!ANNEE=="2022")

table(temp$Acteurs, exclude=F)
table(temp$`Auteur (categorie)`, exclude=F)

# !is.na(Acteurs) &
# Créer une variable date en utilisant ANNEE, MOIS, JOUR
temp <- temp %>%
  group_by(Acteurs, year, week) %>%
  summarise(count = n(), .groups = 'drop') |> 
  filter(!(year==2022 & week==4)) |> 
  mutate(iso_week = paste(year, sprintf("%02d", week), sep = "-W"),  # Combiner année et semaine
         date = ISOweek2date(paste0(iso_week, "-1")))  # Convertir en date (lundi de la semaine)

temp<-temp |> filter(year==2024)
# Créer le graphique à barres avec la prévision ARIMA

# Définir une palette de couleurs appropriée pour les acteurs
actor_colors <- c(
  "Forces armées étatiques" = "#1f78b4",  # Bleu
  "M23" = "#e31a1c" ,
  "Wazalendo"="gold",# Rouge
  "Autres GANE"="lightyellow"
)

# Créer le graphique à lignes lissées avec les acteurs colorés
plot <- ggplot(temp, aes(x = date, y = count, color = Acteurs)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Courbes lissées par acteur
  labs(
    title = "Évolution du nombre d'incidents à fort impact \n Nord-Kivu (hors population civile)",
    x = " ",
    y = "Nombre d'incidents par semaine",
    color = "Acteurs"
  ) +
  scale_color_manual(values = actor_colors) +  # Appliquer les couleurs définies
  scale_x_date(
    date_labels = "%b\n%Y",  # Afficher le mois et l'année sur deux lignes
    date_breaks = "1 month",
    expand = c(0, 0)
  ) +
  # scale_y_continuous(expand = c(0, 0)) +  # Supprimer l'espace supplémentaire au-dessus des courbes
  theme_economist() +  # Appliquer le thème de The Economist
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 0.1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Afficher le graphique
print(plot)

# Sauvegarder le graphique avec un fond blanc et haute résolution
ggsave(
  filename = "economist_style_incidents_agg.png",
  plot = plot,
  width = 12,
  height = 6,
  bg = "white",
  dpi = 300
)