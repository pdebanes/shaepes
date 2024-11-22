###########################################################################
###########################################################################
###                                                                     ###
###                          THRESHOLD SHAEPES                          ###
###                                                                     ###
###########################################################################
###########################################################################


libraries <- c(
  "bannerCommenter", "readxl", "readr", "openxlsx", "tidyverse", 
  "data.table", "dplyr", "stringr", "conflicted", "quarto", "knitr", "viridis"
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
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::month)
# conflicted::conflicts_prefer(dplyr::filter)

source("C:/Users/pauli/Documents/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")
# import excel files ------------------------------------------------------


nord_kivu<- read_excel("~/DRC-CAT/BD_Conflict_V6.1.xlsx", sheet = "Nord-Kivu") |> as.data.frame() |> 
  mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |> 
  select(-Trimestre)|> rename(Chefferie=Chefferies)

sud_kivu_total <- read_excel("~/DRC-CAT/BD_Conflict_V6.1.xlsx", 
                             sheet = "Sud-Kivu") |> as.data.frame() |> 
  mutate(Quarter = paste0("T", Trimestre, "_", ANNEE)) |> 
  select(-Trimestre) 
# sud_kivu <-sud_kivu_total|> filter(TERRITOIRE %in% c("Fizi", "Uvira"))
# table(sud_kivu$TERRITOIRE)

colnames(sud_kivu_total)
colnames(nord_kivu)

ituri_total<- read_excel("~/DRC-CAT/BD_Conflict_V6.1.xlsx", 
                         sheet = "Ituri") |> 
  mutate(Trimestre = str_replace(Trimestre, "Trimestre", "")) %>% # Remove "Trimestre"
  mutate(Quarter = paste0("T",Trimestre, "_", ANNEE)) |> 
  select(-Trimestre)
# ituri <- read_excel("~/DRC-CAT/BD_Conflict_V6.1.xlsx", 
#                     sheet = "Ituri")|> filter(TERRITOIRE %in% "Irumu")

#we want the population of each territoire
ATLAS_MT_2024_full <- read_excel("~/DRC-CAT/FINAL_UPDATE_20240826_ATLAS_MT_2024.xlsx", 
                                 sheet = "APERCU") 

ATLAS_MT_2024 <- ATLAS_MT_2024_full |> 
  select(Territoire, `Population (DPS 2023)`) |> rename(pop=2) |> mutate(Territoire=str_to_title(Territoire)) |> 
  filter(!str_detect(Territoire, "Ville")) |> 
  mutate(Territoire=ifelse(Territoire=="Beni (Territoire / Oicha)", "Beni", Territoire)) |> 
  group_by(Territoire) |> summarise(pop_totale=sum(pop)) |> rename(TERRITOIRE=1)


# create total DB ---------------------------------------------------------
# Ensure all columns have the same type for each dataset
nord_kivu <- nord_kivu |> mutate(across(everything(), as.character))  # Convert all columns to character
sud_kivu_total <- sud_kivu_total |> mutate(across(everything(), as.character))  # Convert all columns to character
ituri_total <- ituri_total |> mutate(across(everything(), as.character))  # Convert all columns to character

# colnames(sud_kivu_total)
# colnames(ituri_total)
# colnames(nord_kivu)
# Combine the datasets using rbind
combined_data <- bind_rows(nord_kivu, sud_kivu_total, ituri_total)

combined_data<-combined_data%>% dplyr::select(where(not_all_na))
colnames(combined_data)

combined_data<-merge(combined_data, ATLAS_MT_2024, by="TERRITOIRE", all.x=T)

combined_data <- combined_data %>%
  mutate(across(contains("Score"), ~ as.numeric(as.character(.))))

write_rds(combined_data, "Total_BD_conflict_4Ooct24.rds")

# A001 -------------------------------------------------------

total <- combined_data|>filter(`Score d'accès humanitaire`>3)
# table(ituri$TERRITOIRE)

temp<-total |> group_by(TERRITOIRE, ANNEE, Quarter)|> reframe(count=n()*100000/pop_totale, 
                                                              count2=n(), .groups = 'drop') |> unique()

temp_average<-temp |> group_by(TERRITOIRE, ANNEE) |> mutate(average=mean(count)) |> select(-Quarter, -count) |> unique()


# Create the histogram
p_histogram <- ggplot(temp, aes(x = count)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +  # Adjust binwidth as needed
  labs(title = "Histogram of incidents per 100,000 inhabitants",
       x = "Count",
       y = "Frequency") +
  scale_fill_viridis_d() +  # Use viridis color scale for fill
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black")  # Add axis lines for clarity
  )

# Print the histogram
print(p_histogram)

# ggsave("~/DRC-CAT/SHAEPES/histo.png", plot = p_histogram, width = 10, height = 6, bg="white")
# 

# table(nord_kivu$Trimestre)
# table(total$Quarter)

# Calculate the average for each TERRITOIRE
average_2024 <- temp_average %>%
  group_by(TERRITOIRE) %>%
  summarise(mean_year = mean(average, na.rm = TRUE)) %>%
  ungroup() %>%  # Ungroup to avoid issues with further processing
  arrange(mean_year) |>  # Reorder by mean_year 
  mutate(TERRITOIRE = reorder(TERRITOIRE, mean_year))  # Reorder by mean_year

# Create a color palette using viridis based on the ordered TERRITOIRE
colors <- viridis::viridis(length(unique(average_2024$TERRITOIRE)))

# Create a named vector for color mapping
color_mapping <- setNames(colors, unique(average_2024$TERRITOIRE))

# Create the line plot
p_line <- ggplot(temp_average, aes(x = as.factor(ANNEE), y = average, group = TERRITOIRE, color = reorder(TERRITOIRE, average))) +
  geom_line() +
  geom_point() +  # Add points for clarity
  labs(title = "Nombre moyen d'incidents par trimestre (rapporté à 100,000 habitants)",
       x = "Année",
       y = "Nombre d'incidents moyens") +
  scale_color_manual(values = color_mapping) +  # Use the color mapping
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black")  # Add axis lines for clarity
  )

# Print the line plot
print(p_line)

# Optionally save the line plot
# ggsave("~/DRC-CAT/SHAEPES/mean_incidents_per_year_pop.png", plot = p_line, width = 10, height = 6, bg = "white")

# Create the ordered bar plot
p_bar <- ggplot(average_2024, aes(x = TERRITOIRE, y = mean_year, fill = TERRITOIRE)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre moyen d'incidents par Territoire (2022-2024, rapporté à 100,000 habitants)",
       x = "Territoire",
       y = "Nombre d'incidents moyens") +
  scale_fill_manual(values = color_mapping) +  # Use the same colors for TERRITOIRE
  scale_y_continuous(breaks = seq(0, max(average_2024$mean_year, na.rm = TRUE), by = 2)) +  # Set y-axis breaks
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)

# Optionally save the bar plot
ggsave("~/DRC-CAT/SHAEPES/bar_plot_incidents_per_territoire.png", plot = p_bar, width = 10, height = 6)


summary(temp_average)


# P006 --------------------------------------------------------------------
# Nombre d'incidents de justice populaire rapporté à 100 000 habitants durant la période de référence.

#on veut regarder les incidents de justice populaire sur le trimestre
# table(nord_kivu$`Type d'incident`)


# Check combien dans les territoires d'intérêt
# total <- combined_data |> filter(`Type d'incident`=="Justice populaire" & TERRITOIRE %in% c("Uvira", "Fizi", "Irumu") & Quarter=="T2_2024")

#regards bdd totale avec tous les territoires
total <- combined_data |> filter(`Type d'incident`=="Justice populaire" )

temp<-total |> group_by(TERRITOIRE, ANNEE, Quarter)|> reframe(count=n()*100000/pop_totale, 
                                                              count2=n(), .groups = 'drop') |> unique()

temp_average<-temp |> group_by(TERRITOIRE, ANNEE) |> mutate(average=mean(count)) |> select(-Quarter, -count) |> unique()


# Create the histogram
p_histogram <- ggplot(temp, aes(x = count)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +  # Adjust binwidth as needed
  labs(title = "P006",
       x = "Count",
       y = "Frequency") +
  scale_fill_viridis_d() +  # Use viridis color scale for fill
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black")  # Add axis lines for clarity
  )
p_histogram


# E003 E004 ---------------------------------------------------------------

## MEB ----
#on utilise la BD REACH 
MEB <- read_excel("~/DRC-CAT/SHAEPES/REACH ICSM/Monitoring REACH.xlsx", 
                                  sheet = "Dataset")

MEB <-MEB |> 
  mutate(annee=year(Date), mois=month(Date)) |> 
  select(Date, annee, mois, Province, Territoire, `Coût médian du MEB` ) |> 
  rename(MEB=`Coût médian du MEB`) |> 
  mutate(quarter = case_when(
    mois %in% c(1, 2, 3) ~ "T1",   # Q1
    mois %in% c(4, 5, 6) ~ "T2",   # Q2
    mois %in% c(7, 8, 9) ~ "T3",   # Q3
    mois %in% c(10, 11, 12) ~ "T4"  # Q4
  )) |> 
  mutate(MEB=as.numeric(str_replace(MEB, " ", "")))

summary(MEB)

temp<-MEB |> group_by(annee, Province, Territoire) |> reframe(sd=sd(MEB, na.rm=T))

temp_pro<-temp |> group_by(Province) |> reframe(sd_mean=mean(sd, na.rm=T))

temp2<-temp |> group_by(Territoire) |> reframe(sd_mean=mean(sd, na.rm=T))

p_bar <- ggplot(temp2, aes(x = Territoire, y = sd_mean, fill = Territoire)) +
  geom_bar(stat = "identity") +
  labs(title = "Ecart-type MEB ",
       x = "Territoire",
       y = "Nombre d'incidents moyens") +
  # scale_fill_manual(values = color_mapping) +  # Use the same colors for TERRITOIRE
  # scale_y_continuous(breaks = seq(0, max(average_2024$mean_year, na.rm = TRUE), by = 2)) +  # Set y-axis breaks
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)

temp_average_year<-temp |> group_by(annee, Province) |> reframe(sd_mean=mean(sd, na.rm=T))
# temp_average_year<-temp_average_year |> group_by(annee) |> reframe(sd_mean_year=mean(sd_mean, na.rm=T))
temp_average_year<-temp_average_year |> pivot_wider(names_from = annee, values_from = sd_mean)

write.xlsx(temp_average_year, "SD_MEB_Province_Annee.xlsx", overwrite = T)


summary(temp_pro)

## PAM ----
PAM <- read_excel("~/DRC-CAT/SHAEPES/REACH ICSM/Monitoring REACH.xlsx", 
                  sheet = "PAM")

colnames(PAM)
PAM <-PAM |> 
  mutate(annee=year(Date), mois=month(Date)) |> 
  select(Date, annee, mois, Province, Territoire, "Coût médian du PMA\r\n(Total)"  ) |> 
  rename(PAM="Coût médian du PMA\r\n(Total)") |> 
  mutate(quarter = case_when(
    mois %in% c(1, 2, 3) ~ "T1",   # Q1
    mois %in% c(4, 5, 6) ~ "T2",   # Q2
    mois %in% c(7, 8, 9) ~ "T3",   # Q3
    mois %in% c(10, 11, 12) ~ "T4"  # Q4
  )) |> 
  mutate(PAM=as.numeric(str_replace(PAM, " ", "")))


temp<-PAM |> group_by(annee, Province, Territoire) |> reframe(sd=sd(PAM, na.rm=T))

temp_average_year<-temp |> group_by( Province) |> reframe(sd_mean=mean(sd, na.rm=T))
# temp_average_year<-temp_average_year |> group_by(annee) |> reframe(sd_mean_year=mean(sd_mean, na.rm=T))
# temp_average_year<-temp_average_year |> pivot_wider(names_from = annee, values_from = sd_mean)
temp_average_year
write.xlsx(temp_average_year, "SD_PAM_Province.xlsx", overwrite = T)


#  S001 S002 S003 ---------------------------------------------------------

## S001 ----
colnames(combined_data)

table(combined_data$`Categorie d'incident`)
table(combined_data$`Type d'incident`)
table(combined_data$`Cible (categorie)`)



temp<-combined_data |> filter(`Cible (categorie)`=="Acteur HDP") |> group_by(Quarter, PROVINCE, TERRITOIRE) |> reframe(count=n())

# try<-combined_data |> filter(TERRITOIRE=="Nyiragongo" & Quarter=="T1_2022" )

# Pour le threshold: en fonction du nombre d'incident total ? 
total_incidents_territoire<-combined_data |>
  filter(`Score d'accès humanitaire`>3)|> 
  group_by(Quarter, PROVINCE, TERRITOIRE) |> 
  reframe(total_incidents=n())

temp2<-merge(temp,total_incidents_territoire, by=c("Quarter", "PROVINCE", "TERRITOIRE"), all.x=T )

summary(temp2)

# Convert count and total_incidents_territoire to numeric if needed
temp2 <- temp2 %>%
  mutate(
    count = as.numeric(as.character(count)),  # Ensure count is numeric
    total_incidents = as.numeric(as.character(total_incidents))  # Ensure total_incidents_territoire is numeric
  )

# Now calculate the ratio
temp_total <- temp2 %>%
  mutate(ratio = count / total_incidents)

reset_s001<-temp_total |> filter(TERRITOIRE %in% c("Fizi", "Uvira", "Irumu") & Quarter=="T2_2024")

tryuvira<-combined_data |> filter(TERRITOIRE=="Uvira" & ANNEE=="2024") |>  select(c(1:5), `Type d'incident`, Quarter)

temp_total_province<-temp_total|> 
  group_by(Quarter, PROVINCE) |> 
  reframe(mean=mean(ratio, na.rm=T))
temp_total_province

table(temp_total$Quarter)

temp_total_province <- temp_total_province %>%
  mutate(Quarter = factor(Quarter, levels = c("T1_2022", "T2_2022","T3_2022", "T4_2022", "T1_2023", "T2_2023","T3_2023", "T4_2023",
                                              "T1_2024", "T2_2024", "T3_2024")))


temp_total_province_t<-temp_total_province |> filter(!Quarter %in% c("T1_2022", "T2_2022"))
# Create the ordered bar plot
p_bar <- ggplot(temp_total_province_t, aes(x = Quarter, y = mean, fill = PROVINCE)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Use position_dodge for side-by-side bars
  labs(title =  paste("# incidents sur les HDP / # incidents fort impact", "\n Moyenne par Province et Trimestre"),
       x = "Trimestre",
       y = "Moyenne") +
  scale_fill_viridis_d() +  # Use viridis color scale
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)

ggsave("S001_barplot_mean_by_province_and_quarter.png", plot = p_bar, width = 10, height = 6, bg="white")

summary(temp_total_province)

## S002 ----

table(combined_data$`Type d'incident`)

temp<-combined_data |> 
  filter(`Type d'incident`=="Taxe illégale et Extorsion" & `Auteur (categorie)` 
         %in% c("Forces armées nationales", "Acteur armé non-étatique",
                "Forces armées internationales"))|>
  group_by(Quarter, PROVINCE, TERRITOIRE) |> 
  reframe(count=n())


# Pour le threshold: en fonction du nombre d'incident total ? 
total_incidents_territoire<-combined_data |>
  # filter(`Score d'accès humanitaire`>=3)|>
  group_by(Quarter, PROVINCE, TERRITOIRE) |> 
  reframe(total_incidents=n())

temp2<-merge(temp,total_incidents_territoire, by=c("Quarter", "PROVINCE", "TERRITOIRE"), all.x=T )

summary(temp2)

# Convert count and total_incidents_territoire to numeric if needed
temp2 <- temp2 %>%
  mutate(
    count = as.numeric(as.character(count)),  # Ensure count is numeric
    total_incidents = as.numeric(as.character(total_incidents))  # Ensure total_incidents_territoire is numeric
  )

# Now calculate the ratio
temp_total <- temp2 %>%
  mutate(ratio = count / total_incidents)

reset_s002<-temp_total |> filter(TERRITOIRE %in% c("Fizi", "Uvira", "Irumu") & Quarter=="T2_2024")


temp_total_province<-temp_total|> 
  group_by(Quarter, PROVINCE) |> 
  reframe(mean=mean(ratio, na.rm=T))
temp_total_province

table(temp_total$Quarter)

temp_total_province <- temp_total_province %>%
  mutate(Quarter = factor(Quarter, levels = c("T1_2022", "T2_2022","T3_2022", "T4_2022", "T1_2023", "T2_2023","T3_2023", "T4_2023",
                                              "T1_2024", "T2_2024", "T3_2024")))


temp_total_province_t<-temp_total_province |> filter(!Quarter %in% c("T1_2022", "T2_2022"))
# Create the ordered bar plot
p_bar <- ggplot(temp_total_province_t, aes(x = Quarter, y = mean, fill = PROVINCE)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Use position_dodge for side-by-side bars
  labs(title =  paste("S002" ,  "\n Moyenne par Province et Trimestre"),
       x = "Trimestre",
       y = "Moyenne") +
  scale_fill_viridis_d() +  # Use viridis color scale
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)

ggsave("S002_barplot_mean_by_province_and_quarter.png", plot = p_bar, width = 10, height = 6, bg="white")

summary(temp_total)
## S003 ----


temp<-combined_data |> filter(`Score de priorité C-HAT`>=12) |> group_by(Quarter, PROVINCE, TERRITOIRE) |> reframe(count=n())

# try<-combined_data |> filter(`Score de priorité C-HAT`>=12 &  `Score d'accès humanitaire`<3)

# try<-combined_data |> filter(TERRITOIRE=="Nyiragongo" & Quarter=="T1_2022" )

# Pour le threshold: en fonction du nombre d'incident total ? 
total_incidents_territoire<-combined_data |>
  filter(`Score d'accès humanitaire`>3)|>
  group_by(Quarter, PROVINCE, TERRITOIRE) |> 
  reframe(total_incidents=n())

temp2<-merge(temp,total_incidents_territoire, by=c("Quarter", "PROVINCE", "TERRITOIRE"), all.x=T )

summary(temp2)

# Convert count and total_incidents_territoire to numeric if needed
temp2 <- temp2 %>%
  mutate(
    count = as.numeric(as.character(count)),  # Ensure count is numeric
    total_incidents = as.numeric(as.character(total_incidents))  # Ensure total_incidents_territoire is numeric
  )

# Now calculate the ratio
temp_total <- temp2 %>%
  mutate(ratio = count / total_incidents)

reset_s003<-temp_total |> filter(TERRITOIRE %in% c("Fizi", "Uvira", "Irumu") & Quarter=="T2_2024")



temp_total_province<-temp_total|> 
  group_by(Quarter, PROVINCE) |> 
  reframe(mean=mean(ratio, na.rm=T))
temp_total_province

table(temp_total$Quarter)

temp_total_province <- temp_total_province %>%
  mutate(Quarter = factor(Quarter, levels = c("T1_2022", "T2_2022","T3_2022", "T4_2022", "T1_2023", "T2_2023","T3_2023", "T4_2023",
                                              "T1_2024", "T2_2024", "T3_2024")))


temp_total_province_t<-temp_total_province |> filter(!Quarter %in% c("T1_2022", "T2_2022"))
# Create the ordered bar plot
p_bar <- ggplot(temp_total_province_t, aes(x = Quarter, y = mean, fill = PROVINCE)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Use position_dodge for side-by-side bars
  labs(title =  paste("S003 # incidents CAT>12 / # incidents fort impact", "\n Moyenne par Province et Trimestre"),
       x = "Trimestre",
       y = "Moyenne") +
  scale_fill_viridis_d() +  # Use viridis color scale
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)

ggsave("S003_barplot_mean_by_province_and_quarter.png", plot = p_bar, width = 10, height = 6, bg="white")

summary(temp_total)

table(combined_data$`Type d'incident`)


## valeurs RESET  ----------------------------------------------------------

reset_s001<-reset_s001 |> mutate(indicateur="S001") |> select(Quarter, TERRITOIRE, indicateur, ratio)
reset_s002<-reset_s002 |> mutate(indicateur="S002")|> select(Quarter, TERRITOIRE, indicateur, ratio)
reset_s003<-reset_s003 |> mutate(indicateur="S003")|> select(Quarter, TERRITOIRE, indicateur, ratio)

reset_secu<-rbind(reset_s001, reset_s002, reset_s003)

write.xlsx(reset_secu, "reset_secu.xlsx")
