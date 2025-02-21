###########################################################################
###########################################################################
###                                                                     ###
###         INDICATEURS BD INTERNES (CONFLICT + DEPLACEMENT)            ###
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

Total_BD_conflict<-readRDS("Total_BD_conflict.rds")
data_raw<-Total_BD_conflict |> mutate(province=str_to_lower(province),
                                      territoire=str_to_lower(territoire))

ATLAS_MT_2024<-readRDS("ATLAS_MT_2024.rds")
BD_deplacement<-readRDS("BD_deplacement.rds")

table(BD_deplacement$province)
BD_deplacement<-BD_deplacement |> mutate(province=str_to_lower(province),
                                         territoire=str_to_lower(territoire)
                                         # territoire=ifelse(territoire=="beni-ville", "beni", territoire)
                                         ) |> 
                                  filter(province %in% c("ituri", "sud-kivu", "nord-kivu", "tanganyika"))

table(BD_deplacement$territoire)
table(data_raw$territoire)
table(ATLAS_MT_2024$territoire)


complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )

data_raw<-data_raw |> mutate(annee=as.numeric(annee),
                             province=str_to_lower(province),
                             territoire=str_to_lower(territoire))

# A001 --------------------------------------------------------------------

total <- data_raw|>filter(score_dacces_humanitaire>3)
# table(ituri$TERRITOIRE)

temp1<-total |> group_by(province, territoire, annee, quarter)|> reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() 

# Ensure all combinations exist in the data
temp1 <- complete_frame_ind  %>%
  left_join(temp1, by = c("province", "territoire", "annee", "quarter")) |> 
  mutate(indicator="A001",
         count = ifelse(is.na(count), 0, as.numeric(count)))
  



# A002 --------------------------------------------------------------------

total <- data_raw|>filter(score_dacces_humanitaire>3)
# table(ituri$TERRITOIRE)

temp2<-total |> group_by(province, territoire, annee, quarter)|> reframe(count=mean(score_dacces_humanitaire, na.rm=T)) |> ungroup() |>  unique() 

temp2 <- complete_frame_ind  %>%
  left_join(temp2, by = c("province", "territoire", "annee", "quarter")) |> 
  mutate(indicator="A002",
         count = ifelse(is.na(count), 0, as.numeric(count)))

summary(temp2$count)

# A004 --------------------------------------------------------------------
# table(data_raw$categorie_dincident)
# table(data_raw$cible_categorie)

# temp2<-data_raw |> filter(grepl("manifestation", str_to_lower(type_dincident)))

temp3<-data_raw |> filter(categorie_dincident=="Manifestations_populaires" & cible_categorie=="Acteur HDP") |> 
                          group_by(province, territoire, annee, quarter)|> 
                          reframe(count=mean(score_dacces_humanitaire, na.rm=T)) |> 
                          ungroup() |>  unique() |> 
  select(province, territoire, annee, quarter, count)

temp3 <- complete_frame_ind  %>%
  left_join(temp3, by = c("province", "territoire", "annee", "quarter")) |> 
  mutate(indicator="A004",
         count = ifelse(is.na(count), 0, as.numeric(count)))


# A005 --------------------------------------------------------------------

temp4 <- data_raw %>%
  filter(categorie_dincident == "Manifestations_populaires") %>%
  mutate(flag = ifelse(type_dincident == "Manifestation violente", 1, 0)) %>%
  group_by(province, territoire, annee, quarter) %>%
  summarise(
    total_manifestations = n(),  # Total number of manifestations
    violent_manifestations = sum(flag, na.rm = TRUE),  # Total violent manifestations
    count = (violent_manifestations / total_manifestations) * 100  # Percentage calculation
  ) %>%
  ungroup() %>%
  # mutate(indicator = "A005") |> 
  select(province, territoire, annee, quarter, count)

summary(temp4)

#on remplace par 0 => en gros pas de manif tout court mais donc threshold pas élevé
temp4 <- complete_frame_ind  %>%
  left_join(temp4, by = c("province", "territoire", "annee", "quarter")) |> 
  mutate(indicator="A005",
         count = ifelse(is.na(count), 0, as.numeric(count)))


# A006 --------------------------------------------------------------------

temp5 <- data_raw %>%
  filter(cible_categorie == "Infrastructure de base")%>%
  group_by(province, territoire, annee, quarter) %>%
  reframe(count=n()) |> ungroup() |>  unique() |>
  # mutate(indicator="A006") |> 
  select(province, territoire, annee, quarter, count)

summary(temp5)

temp5 <- complete_frame_ind  %>%
  left_join(temp5, by = c("province", "territoire", "annee", "quarter")) |> 
  mutate(indicator="A006",
         count = ifelse(is.na(count), 0, as.numeric(count)))


# A007 --------------------------------------------------------------------

# table(ituri$TERRITOIRE)

temp6<-data_raw|>filter(score_dacces_humanitaire>3) |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=sd(score_dacces_humanitaire, na.rm=T)) |> 
  ungroup() |>  unique() |> mutate(indicator="A007")



# table(ituri$TERRITOIRE)

temp6<-data_raw|>filter(score_dacces_humanitaire>3) |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=sd(score_dacces_humanitaire, na.rm=T)) |> 
  ungroup() |>  unique() |> mutate(indicator="A007")


# A008 ----------------------
temp08<-data_raw|>filter(categorie_dincident=="Obstacles_Acces_humanitaire") |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()) |> ungroup() |>  unique() |> 
  mutate(indicator="A008")



#pas fan du threshold mais bon...

# A009 ----------------------
temp09<-data_raw|>filter(score_dacces_humanitaire>3 & categorie_dincident %in% c("Operations_militaires", "Opérations de Contre-insurrection")) |> 
                           group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()) |> ungroup() |>  unique() |> 
  mutate(indicator="A009")

#pb avec les threshold TBD après avoir vérifié...
summary(temp09$count)

# H016 --------------------------------------------------------------------
table(data_raw$categorie_dincident)


temp16<-data_raw|>filter(categorie_dincident=="Violations_droit_international_DIH_DIDH") |> group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
 mutate(indicator="H016")


# P001 --------------------------------------------------------------------
table(data_raw$auteur_categorie)

tempP001<-data_raw|>filter(score_dacces_humanitaire>3 & auteur_categorie=="Societé civile / Groupe de Pression")|> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="P001")

summary(tempP001$count)
# P003 --------------------------------------------------------------------
table(data_raw$cible_categorie)

tempP003<-data_raw|>filter(score_dacces_humanitaire>3 & cible_categorie=="Autorités politico-administratives") |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="P003")

summary(tempP003)

# P006 --------------------------------------------------------------------
table(data_raw$type_dincident)


tempP006<-data_raw|>filter(type_dincident=="Justice populaire")|> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="P006")

summary(tempP006$count)


# S001 --------------------------------------------------------------------
table(data_raw$cible_categorie)

# Pour le threshold: en fonction du nombre d'incident total ? 
tempS001 <- data_raw %>%
  mutate(
    target_hdp = cible_categorie == "Acteur HDP",
    high_score = score_dacces_humanitaire > 3
  ) %>%
  group_by(province, territoire, annee, quarter) %>%
  summarise(
    total_incidents = sum(high_score, na.rm = TRUE),
    count1 = sum(target_hdp, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(
    count = ifelse(total_incidents>0, count1 / total_incidents, 0),
    indicator = "S001"
  ) %>%
  select(province, territoire, annee, quarter, count, indicator)

summary(tempS001)

# S002 --------------------------------------------------------------------

tempS002<-data_raw |> 
  filter(type_dincident=="Taxe illégale et Extorsion" & auteur_categorie 
         %in% c("Forces armées nationales", "Acteur armé non-étatique",
                "Forces armées internationales"))|>
  group_by(province, territoire, annee, quarter) |> 
  reframe(count1=n())


total_incidents_territoire<-data_raw |>
  # filter(`Score d'accès humanitaire`>=3)|>
  group_by(province, territoire, annee, quarter) |> 
  reframe(total_incidents=n())

tempS002<-merge(tempS002,total_incidents_territoire, by=c("province", "territoire", "annee", "quarter"), all.x=T )
tempS002<-tempS002 |> mutate(count=count1/total_incidents) |> select(province, territoire, annee, quarter,count ) |> 
  mutate(indicator="S002")
summary(tempS002)

# S003 --------------------------------------------------------------------


tempS003<-data_raw |> filter(score_de_priorite_c_hat>=12) |> group_by(province, territoire, annee, quarter) |> reframe(count1=n())

# Pour le threshold: en fonction du nombre d'incident total ? 
total_incidents_territoire<-data_raw |>
  filter(score_dacces_humanitaire>3)|>
  group_by(province, territoire, annee, quarter) |> 
  reframe(total_incidents=n())

tempS003<-merge(tempS003,total_incidents_territoire, by=c("province", "territoire", "annee", "quarter"), all.x=T )
tempS003<-tempS003 |> mutate(count=count1/total_incidents) |> select(province, territoire, annee, quarter,count ) |> 
  mutate(indicator="S003",
         count=ifelse(is.na(count), 0, count))
summary(tempS003)

# S004 --------------------------------------------------------------------
table(data_raw$type_dincident)


tempS004<-data_raw|>filter(type_dincident %in% c("Braquage et Embuscade", "Enlèvement", "Violation de propriété privée" )) |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="S004",
         count=ifelse(is.na(count), 0, count))

summary(tempS004)

# S005 --------------------------------------------------------------------
table(data_raw$cible_categorie)
data_raw<-data_raw |> mutate(cible_categorie=ifelse(cible_categorie=="Population Civile", "Population civile", cible_categorie))

tempS005<-data_raw|>filter(cible_categorie %in% c("Population civile")) |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="S005")


summary(tempS005)

# test<-data_raw |> filter(categorie_dincident=="Operations_militaires") |> group_by(categorie_dincident, type_dincident, auteur_categorie) |> reframe(count=n()) |> 
#   pivot_wider(names_from = auteur_categorie, values_from = count)
# 
# test<-data_raw |> filter(cible_categorie=="Population Civile")

# S006 --------------------------------------------------------------------

tempS006<-data_raw|>filter(score_dacces_humanitaire<=3 & !categorie_dincident=="Catastrophes_naturelles") |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="S006") |> 
  select(province, territoire, annee, quarter,count, indicator )


# S008 --------------------------------------------------------------------
table(data_raw$categorie_dincident)

tempS008<-data_raw|>filter( categorie_dincident=="Violations_code_penal_civil_militaire") |> 
  group_by(province, territoire, annee, quarter)|> 
  reframe(count=n()*100000/pop_totale) |> ungroup() |>  unique() |> 
  mutate(indicator="S008",
         count=ifelse(is.na(count), 0, count)) |> 
  select(province, territoire, annee, quarter,count, indicator )

summary(tempS008)

# EH001 -------------------------------------------------------------------

table(BD_deplacement$eventReanson)


tempEH001<-BD_deplacement |> filter(eventReanson=="Catastrophe naturelle") |> select(province, territoire, annee, quarter, Deplacées) |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(count1=sum(Deplacées, na.rm=T))

tempEH001<-merge(tempEH001, ATLAS_MT_2024, by.x="territoire", by.y="territoire", all.x=T) |> 
  mutate(count=count1*100000/pop_totale,
                               indicator="EH001") |> 
  select(province, territoire, annee, quarter,count, indicator ) |> 
  mutate(annee=as.character(annee))



# H003 --------------------------------------------------------------------

tempH003<-BD_deplacement |> select(province, territoire, annee, quarter, Deplacées) |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(count1=sum(Deplacées, na.rm=T))

tempH003<-merge(tempH003, ATLAS_MT_2024, by.x="territoire", by.y="territoire", all.x=T) |> 
  mutate(count=count1*100000/pop_totale) |> 
  mutate(indicator="H003", 
         annee=as.character(annee)) |> 
  select(province, territoire, annee, quarter,count, indicator )


# H005 --------------------------------------------------------------------

tempH005<-BD_deplacement |> select(province, territoire, annee, quarter, Retournées) |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(count1=sum(Retournées, na.rm=T))

tempH005<-merge(tempH005, ATLAS_MT_2024, by.x="territoire", by.y="territoire", all.x=T) |> 
  mutate(count=count1*100000/pop_totale) |> 
  mutate(indicator="H005") |> 
  select(province, territoire, annee, quarter,count, indicator )


# H007 --------------------------------------------------------------------

table(BD_deplacement$description)
table(BD_deplacement$`total ménages (depl + retour)`)
tempH007<-BD_deplacement |> filter(description %in% c("En attente d'évaluation", "Diagnostic préliminaire")) |> 
select(province, territoire, annee, quarter, `total ménages (depl + retour)`) |> 
  group_by(province, territoire, annee, quarter) |> 
  reframe(count1=sum(`total ménages (depl + retour)`, na.rm=T))

tempH007<-merge(tempH007, ATLAS_MT_2024, by.x="territoire", by.y="territoire", all.x=T) |> 
  mutate(count=count1*100000/pop_totale) |> 
  mutate(indicator="H007") |> 
  select(province, territoire, annee, quarter,count, indicator )


# E005E --------------------------------------------------------------------
table(data_raw$type_dincident)
tempE005E <- data_raw|>filter( !type_dincident=="Sécurisation des dispositifs explosifs" & grepl("mines|minier", description))|>
  group_by(province, territoire, annee, quarter)|>
  reframe(count1=n()) |> ungroup() |>  unique()

tempE005E<-merge(tempE005E, ATLAS_MT_2024, by.x="territoire", by.y="territoire", all.x=T) |> 
  mutate(count=count1*100000/pop_totale) |> 
  mutate(indicator="E005E") |> 
  select(province, territoire, annee, quarter,count, indicator )
summary(tempE005E)



# BD totale  --------------------------------------------------------------



# temp_tables <- mget(ls(pattern = "^temp"))
# 
# # Bind all the tables together
# combined_temp <- bind_rows(temp_tables, .id = "source")
# 

# Get all objects in the environment starting with "temp"
list <- ls(pattern = "^temp")

# Initialize an empty list to store processed tables
processed_tables <- list()

# Iterate over each table, ensure consistency, and add to the list
for (table_name in list) {
  # Retrieve the table
  table <- get(table_name)
  
  # Standardize column names and convert to character type
  table <- table %>%
    mutate(across(everything(), as.character))  # Ensure consistent data types
  
  # Add the table to the list
  processed_tables[[table_name]] <- table
}

# Bind all the processed tables together
combined_temp <- do.call(rbind, processed_tables)

# Convert back to a tibble if needed
combined_temp <- as_tibble(combined_temp)


combined_temp <-combined_temp |> mutate(annee=as.numeric(annee))


list_ind<-combined_temp |> select(indicator) |> unique() |> pull()
list_ind

head(complete_frame_ind)
# créer un frame pour tous les trimestres  --------------------------------

complete_frame_ind <- complete_frame_ind %>%
  expand_grid(indicator = list_ind)

combined_total <- complete_frame_ind  %>%
  left_join(combined_temp, by = c("annee", "province", "territoire",  "quarter", "indicator")) |> 
  mutate( count = ifelse(is.na(count), 0, as.numeric(count)), 
          annee=ifelse(is.na(annee), sub(".*_", "", quarter),annee)) |> 
  filter(!quarter %in% c("T1_2022", "T2_2022"))

combined_total<-combined_total |> unique()

saveRDS(combined_total, "indicateurs_BD_internes.rds")

# table(indicateurs_BD_internes$indicator)

test<-combined_total |> filter(indicator=="E005E") |> group_by(province, annee, quarter) |> reframe(sum=mean(count))
summary(test)
