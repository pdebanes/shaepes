
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

library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(ggrepel)


conflicted::conflicts_prefer(dplyr::filter)

source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")



# conflict ----------------------------------------------------------------


Total_BD_conflict <- readRDS("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/Total_BD_conflict.rds")

goma<-Total_BD_conflict |> filter(village_ville=="Goma" & as.numeric(annee)>=2023 & cible_categorie=="Population civile") |> 
  group_by(annee, quarter) |> 
  reframe(count=n())

write.xlsx(goma, "goma_quarter_pop_civile.xlsx")


# déplacements ------------------------------------------------------------


BD_deplacement <- readRDS("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/BD_deplacement.rds")

dep_dec_jan<-BD_deplacement |> filter(startDate>="2024-01-01" & province=="Nord-kivu")

|> 
  reframe(count=sum(`total ménages (depl + retour)`))

table(dep_dec_jan$territoire)


# DÉPLACEMENTS ORIGINES ---------------------------------------------------


displacement_or <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/WORKING_DisplacementDB_3.0.xlsx", 
                                         sheet = "Displacement", skip = 2)

displacement_or <-displacement_or |> filter(Date_publication_alerte>= "2024-01-01" & Nature_Mouvement=="Déplacement" &
                                              Province_destination %in% c("Nord-Kivu") )

displacement_or<-displacement_or |> 
  mutate(Territoire_origine=ifelse(Territoire_origine %in% c("Rutchuru", "Nkivu _ Rutshuru"), "Rutshuru", 
                                                                     ifelse(Territoire_origine=="Nkivu _ Masisi", "Masisi",
                                                                            Territoire_origine)
                                                                     ))
displacement_or$Territoire_origine<-gsub("Skivu _ ", "", displacement_or$Territoire_origine)
displacement_or$Territoire_origine<-gsub("Nkivu _ ", "", displacement_or$Territoire_origine)

displacement_or$Province_origine<-str_to_lower(displacement_or$Province_origine)
displacement_or$ZS_origine<-str_to_lower(displacement_or$ZS_origine)
         
table(displacement_or$Territoire_origine)
table(displacement_or$Province_origine)



displacement_or <-displacement_or |> mutate(
  Territoire_origine=ifelse(ZS_origine=="kayna", "Rutshuru", Territoire_origine),
  Province_origine=ifelse(ZS_origine=="kirotshe", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Rutshuru", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Kalehe", "sud-kivu", Province_origine),
  ZS_origine=ifelse(ZS_origine=="haurs-plateaux", "hauts plateaux", ZS_origine),
  ZS_origine=ifelse(ZS_origine=="manguredjipa", "mangurejipa", ZS_origine),
  Province_origine=ifelse(Territoire_origine=="Fizi", "sud-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="mangurejipa", "Lubero", Territoire_origine),
  Territoire_origine=ifelse(ZS_origine=="mweso", "Masisi", Territoire_origine),
  Province_origine=ifelse(Territoire_origine=="Beni", "nord-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="musienene", "Lubero", Territoire_origine),
  Province_origine=ifelse(Territoire_origine=="Masisi", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Lubero", "nord-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="pinga", "Walikale", Territoire_origine),
)




displacement_or <-displacement_or[!duplicated(displacement_or[,c("ID","ZS_origine")]),]
table(displacement_or$Province_origine)


displacement_or <-displacement_or |> group_by(ID) |> mutate(re_count=Menages_deplaces/n()) |> ungroup()


# temp<-displacement_or |> group_by(Province_origine,Territoire_origine, ZS_origine) |> reframe(count=sum(re_count, na.rm=T))
temp<-displacement_or |> group_by(Province_origine,Territoire_origine) |> reframe(count=sum(re_count, na.rm=T))
temp<-temp |> filter(!is.na(Province_origine))


write.xlsx(temp, "displacements_ZS_origine.xlsx")

list_terr<-temp |> select(Territoire_origine) |> unique() |> pull()

# Lire le shapefile ADM2 (districts)

adm_1<-st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm1_rgc_itos_20190911.shp")

adm_2<-st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm2_rgc_20190911.shp") |> filter(str_to_lower(ADM1_FR) %in% province_names)


  
  
# ETH_shapefile <- st_read("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 2. Fonds cartes (SHP)/rdc_zone_de_sante_09092019 (1)/RDC_Zone_de_sante_09092019.shp")
ETH_shapefile <- st_read("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 2. SHP/territoire/Territoire.shp")


ETH_shapefile_osm <- st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm2_rgc_20190911.shp")



# Vérifier les noms des provinces (ADM1_FR)
# province_names <- c("ituri" , "nord-kivu"  )
province_names <- c("nord-kivu"  )
# table(ETH_shapefile$PROVINCE)

ETH_shapefile<-ETH_shapefile_osm |> filter(str_to_lower(ADM1_FR) %in% province_names)
adm_1<-adm_1 |> filter(str_to_lower(ADM1_FR) %in% province_names)
table(adm_1$ADM1_FR)


# ETH_shapefile$health_zone <- tolower(trimws(ETH_shapefile$Nom))
# ETH_shapefile_osm$health_zone <- tolower(trimws(ETH_shapefile_osm$name))

# temp<-temp |> filter(count>1000)

# ETH_shapefile_osm<-ETH_shapefile_osm |> filter(PROVINCE %in% province_names)
# temp$health_zone <- tolower(trimws(temp$ZS_origine))
# Join the data table with the shapefile
map_data <- ETH_shapefile %>% 
  left_join(temp, by = c("ADM2_FR" = "Territoire_origine"))

# # Plot the map
# ggplot(data = map_data) +
#   geom_sf(aes(fill = count), color = "black", size = 0.2) +  # Fill based on 'households'
#   scale_fill_viridis_c(option = "plasma", name = "Nombre de ménages déplacés") +  # Color scale
#   coord_sf(expand = FALSE) +  # Ensure no clipping
#   theme_minimal() +                                               # Minimal theme
#   labs(title = "Zone de santé d'origines des déplacés du Nord-Kivu et Sud-Kivu depuis 2024",
#        caption = "Data source: EH tool") +
#   theme(legend.position = "right")


map_data <- map_data %>%
  mutate(
    count_bins = cut(
      count,
      breaks = c(0, 10000, 50000, 100000, Inf),  # Define breakpoints
      labels = c("<10000", "10001-50000", "50001-100000", ">100000"),  # Labels
      include.lowest = TRUE
    )
  )

city_coords <- data.frame(
  city = c("Goma", "Lubero", "Masisi"),
  lon = c(29.225, 29.254, 28.698),  # Longitudes
  lat = c(-1.682, -0.135, -1.389)   # Latitudes
)

# Extraire les coordonnées des centroïdes pour ADM1
test_liste<-map_data |>filter(!is.na(count))

# Extraire les coordonnées des centroïdes pour ADM1
adm1_centroids <- st_centroid(test_liste)
adm1_centroids_df <- adm1_centroids %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(ADM1_FR = test_liste$ADM2_FR)
library(ggrepel)

plot <- ggplot(data = map_data %>% filter(!is.na(count_bins))) +
  geom_sf(aes(fill = count_bins), color = "grey", size = 0.2) +  # Fill based on bins
  geom_sf(data = adm_2, fill = NA, color = "darkgrey", size = 2) +  # ADM2 boundaries
  geom_sf(data = adm_1, fill = NA, color = "black", size = 1.5) +  # ADM1 boundaries
  scale_fill_manual(
    values = c("darkgreen", "orange", "red", "darkred"),  
    name = "Nb de ménages déplacés",
    na.value = "white",  # Define colors for bins
    drop = TRUE # Legend title
  ) +
  coord_sf(expand = FALSE) +      # Ensure no clipping
  theme_minimal() +               # Minimal theme
  labs(title = "Territoire d'origine des ménages déplacés dans le Nord-Kivu \n 01-2024 à 02-2025 - Nord-Kivu",
       caption = "Data source: EH Tool") +
  theme(
    panel.grid = element_blank(), # Supprimer les lignes de la grille
    axis.text = element_blank(),  # Supprimer les textes des axes
    axis.ticks = element_blank(), # Supprimer les ticks des axes
    axis.title = element_blank(), # Supprimer les titres des axes
    legend.position = "right"     # Position de la légende
  ) +
  # Labels for ADM2 with a box
  geom_label_repel(data = adm1_centroids_df, aes(x = X, y = Y, label = ADM1_FR),
                   size = 3, color = "black", fontface = "bold", fill = "white", box.padding = 0.4)

plot

# Sauvegarder le graphique avec un fond blanc et haute résolution
ggsave(
  filename = "C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/carte_ZS_origine_déplacésNK.png",
  plot = plot,
  width = 12,
  height = 8,
  bg = "white",
  dpi = 300
)

# Save the plot as a PDF
# ggsave(
#   filename = "map_north_kivu.pdf",  # File name
#   plot = plot,                     # Plot object
#   device = "pdf",                  # File format
#   width = 10,                      # Width in inches
#   height = 8,                      # Height in inches
#   bg = "white"                     # Ensure a white background
# )


# même chose mais retournés récents ---------------------------------------


displacement_or <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/WORKING_DisplacementDB_3.0.xlsx", 
                              sheet = "Displacement", skip = 2)

table(displacement_or$Nature_Mouvement)

displacement_or <-displacement_or |> filter(Date_publication_alerte>= "2024-12-01" & Nature_Mouvement=="Retour" &
                                              Province_destination %in% c("Nord-Kivu") )

displacement_or<-displacement_or |> 
  mutate(Territoire_origine=ifelse(Territoire_origine %in% c("Rutchuru", "Nkivu _ Rutshuru"), "Rutshuru", 
                                   ifelse(Territoire_origine=="Nkivu _ Masisi", "Masisi",
                                          Territoire_origine)
  ))
displacement_or$Territoire_origine<-gsub("Skivu _ ", "", displacement_or$Territoire_origine)
displacement_or$Territoire_origine<-gsub("Nkivu _ ", "", displacement_or$Territoire_origine)

displacement_or$Province_origine<-str_to_lower(displacement_or$Province_origine)
displacement_or$ZS_origine<-str_to_lower(displacement_or$ZS_origine)

table(displacement_or$Territoire_origine)
table(displacement_or$Province_origine)



displacement_or <-displacement_or |> mutate(
  Territoire_origine=ifelse(ZS_origine=="kayna", "Rutshuru", Territoire_origine),
  Province_origine=ifelse(ZS_origine=="kirotshe", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Rutshuru", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Kalehe", "sud-kivu", Province_origine),
  ZS_origine=ifelse(ZS_origine=="haurs-plateaux", "hauts plateaux", ZS_origine),
  ZS_origine=ifelse(ZS_origine=="manguredjipa", "mangurejipa", ZS_origine),
  Province_origine=ifelse(Territoire_origine=="Fizi", "sud-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="mangurejipa", "Lubero", Territoire_origine),
  Territoire_origine=ifelse(ZS_origine=="mweso", "Masisi", Territoire_origine),
  Province_origine=ifelse(Territoire_origine=="Beni", "nord-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="musienene", "Lubero", Territoire_origine),
  Province_origine=ifelse(Territoire_origine=="Masisi", "nord-kivu", Province_origine),
  Province_origine=ifelse(Territoire_origine=="Lubero", "nord-kivu", Province_origine),
  Territoire_origine=ifelse(ZS_origine=="pinga", "Walikale", Territoire_origine),
)




displacement_or <-displacement_or[!duplicated(displacement_or[,c("ID","ZS_origine")]),]
table(displacement_or$Province_origine)


displacement_or <-displacement_or |> group_by(ID) |> mutate(re_count=Menages_deplaces/n()) |> ungroup()


# temp<-displacement_or |> group_by(Province_origine,Territoire_origine, ZS_origine) |> reframe(count=sum(re_count, na.rm=T))
temp<-displacement_or |> group_by(Province_origine,Territoire_origine) |> reframe(count=sum(re_count, na.rm=T))
temp<-temp |> filter(!is.na(Province_origine))


write.xlsx(temp, "displacements_retour.xlsx")

list_terr<-temp |> select(Territoire_origine) |> unique() |> pull()
province_names <- c("nord-kivu"  )
# Lire le shapefile ADM2 (districts)

adm_1<-st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm1_rgc_itos_20190911.shp")

adm_2<-st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm2_rgc_20190911.shp") |> filter(str_to_lower(ADM1_FR) %in% province_names)


ETH_shapefile_osm <- st_read("C:/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm2_rgc_20190911.shp")


ETH_shapefile<-ETH_shapefile_osm |> filter(str_to_lower(ADM1_FR) %in% province_names)

adm_1<-adm_1 |> filter(str_to_lower(ADM1_FR) %in% province_names)
table(adm_1$ADM1_FR)

map_data <- ETH_shapefile %>% 
  left_join(temp, by = c("ADM2_FR" = "Territoire_origine"))



map_data <- map_data %>%
  mutate(
    count_bins = cut(
      count,
      breaks = c(0, 5000, 10000, 25000, Inf),  # Define breakpoints
      labels = c("<5000", "5001-10 000", "10 001-25 000", ">25 000"),  # Labels
      include.lowest = TRUE
    )
  )

city_coords <- data.frame(
  city = c("Goma", "Lubero", "Masisi"),
  lon = c(29.225, 29.254, 28.698),  # Longitudes
  lat = c(-1.682, -0.135, -1.389)   # Latitudes
)

test_liste<-map_data |>filter(!is.na(count))

# Extraire les coordonnées des centroïdes pour ADM1
adm1_centroids <- st_centroid(test_liste)
adm1_centroids_df <- adm1_centroids %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(ADM1_FR = test_liste$ADM2_FR)

library(ggrepel)

plot <- ggplot(data = map_data %>% filter(!is.na(count_bins))) +
  geom_sf(aes(fill = count_bins), color = "grey", size = 0.2) +  # Fill based on bins
  geom_sf(data = adm_2, fill = NA, color = "darkgrey", size = 2) +  # ADM2 boundaries
  geom_sf(data = adm_1, fill = NA, color = "black", size = 1.5) +  # ADM1 boundaries
  scale_fill_manual(
    values = c("darkgreen", "orange", "red", "darkred"),  
    name = "Nb de ménages retournés",
    na.value = "white",  # Define colors for bins
    drop = TRUE # Legend title
  ) +
  coord_sf(expand = FALSE) +      # Ensure no clipping
  theme_minimal() +               # Minimal theme
  labs(title = "Territoire de destination des retournés dans le Nord-Kivu \n 12-2024 à 02-2025 - Nord-Kivu",
       caption = "Data source: EH Tool") +
  theme(
    panel.grid = element_blank(), # Supprimer les lignes de la grille
    axis.text = element_blank(),  # Supprimer les textes des axes
    axis.ticks = element_blank(), # Supprimer les ticks des axes
    axis.title = element_blank(), # Supprimer les titres des axes
    legend.position = "right"     # Position de la légende
  ) +
  # Labels for ADM2 with a box
  geom_label_repel(data = adm1_centroids_df, aes(x = X, y = Y, label = ADM1_FR),
                   size = 3, color = "black", fontface = "bold", fill = "white", box.padding = 0.4)

plot

# Sauvegarder le graphique avec un fond blanc et haute résolution
ggsave(
  filename = "C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/carte_retours_NK.png",
  plot = plot,
  width = 12,
  height = 8,
  bg = "white",
  dpi = 300
)
