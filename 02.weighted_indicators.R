###########################################################################
###########################################################################
###                                                                     ###
###           IMPORT THRESHOLD ET PONDÉRATION DES INDICATEURS           ###
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

indicators_raw<-read_rds("stacked_indicators.rds") |> filter(!is.na(indicator))



# import threshold v5 -----------------------------------------------------

TEMPLATE_V5_SHAEPES_MONITORING <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/data Q2/TEMPLATE V5 SHAEPES MONITORING.xlsx", 
                                             sheet = "Dataset_terr. 1", skip = 3)

TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> 
  select(-`Indicator Weight`, - `Indicator Impact Score`, -`Weighted Indicator Impact Score`) |> 
  clean_names() |> 
  select(-result_value, -indicator_value_with_denominator)


TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> 
  mutate(group=str_extract(code, "^[A-Z]+"))

#pb avec le dénominateur parce que quand rapporté à la population => déjà fait dans le code BD conflict, 
#il faut juste reprendre le signe 

TEMPLATE_V5_SHAEPES_MONITORING$denominator <- ifelse(TEMPLATE_V5_SHAEPES_MONITORING$denominator >= 0, 1, -1)

head(TEMPLATE_V5_SHAEPES_MONITORING$denominator)

# TEMPLATE_V5_SHAEPES_MONITORING <-TEMPLATE_V5_SHAEPES_MONITORING |> select(-indicator_status)


# merge with indicators db  -----------------------------------------------


temp<-merge(indicators_raw, TEMPLATE_V5_SHAEPES_MONITORING, by.x= "indicator", by.y="code" )|> 
  mutate(across(starts_with("threshold_level_"), ~ as.numeric(.)))

temp <- temp |> 
  mutate(count_denum = round(count * denominator, 3),
         ind_impact_score = case_when(
           is.na(count_denum) ~ NA_real_,
           # count_denum <= threshold_level_0 ~ 0
           count_denum <= threshold_level_1 ~ 0,
           count_denum > threshold_level_1 & count_denum <= threshold_level_2 ~ 1,
           count_denum > threshold_level_2 & count_denum <= threshold_level_3 ~ 2,
           count_denum > threshold_level_3 & count_denum <= threshold_level_4 ~ 3,
           count_denum > threshold_level_4 & count_denum <= threshold_level_5 ~ 4,
           TRUE ~ 5
         ))

# pondération = 1 / le nombre d'indicateurs
temp <-temp |> 
  group_by(province, territoire, quarter, group) |> 
  mutate(indicator_weight = 1 / sum(!is.na(ind_impact_score)))|> ungroup()

# indicateurs pondérés

temp <-temp |> 
  mutate(weighted_ind=ind_impact_score*indicator_weight) 


# score card --------------------------------------------------------------


# compute score by dimension ----------------------------------------------

total_score_dim<-temp |>  group_by(province,territoire, annee, quarter, group) |> reframe(score_dim=sum(weighted_ind, na.rm=T)) |> 
mutate(
  dim_name = case_when(
    group == "S" ~ "Security",
    group == "H" ~ "Humanitarian",
    group == "A" ~ "Access",
    group == "P" ~ "Political and social inclusion",
    group == "EH" ~ "Environmental Hazards",
    group == "E" ~ "Economic",  # Assuming you meant "E" for Economic
    TRUE ~ NA_character_  # Handle unexpected values
  ))

# weighting of program RESET ----------------------------------------------


dim_weight_reset <- data.frame(
  dim_name = c("Security", 
               "Humanitarian", 
               "Access", 
               "Political and social inclusion", 
               "Environmental Hazards", 
               "Economic"),
  dim_weight = c(0.25, 0.25, 0.15, 0.1, 0.1, 0.15)
)

total_score_dim<-merge(total_score_dim, dim_weight_reset, by="dim_name", all.x=T)





# score total avec pondération des dimensions -----------------------------
total_score_dim<-total_score_dim |> mutate(tot=score_dim*dim_weight,
                                           annee=ifelse(is.na(annee),as.numeric(sub(".*_(\\d{4})$", "\\1", quarter)), annee)) 

total_shaepes<-total_score_dim |> 
  group_by(province, territoire, annee, quarter) |> reframe(score_tot=round(sum(tot, na.rm=T), 2))


# final datasets ----------------------------------------------------------



saveRDS(total_score_dim, "shaepes_weighted_db.rds")
saveRDS(total_shaepes, "shaepes_final_score.rds")


# tests -------------------------------------------------------------------

total_score_dim<-read_rds("shaepes_weighted_db.rds")
total_shaepes<-read_rds("shaepes_final_score.rds")


write.xlsx(total_score_dim, "total_score_dim_feb2025.xlsx")
write.xlsx(total_shaepes, "total_shaepes_feb2025.xlsx")



try<-total_shaepes |> filter(annee==2024)


test_irumu<-total_score_dim |> filter(territoire=="irumu" & quarter=="T2_2024")
test_irumu

test_fizi_dim<-total_score_dim |> filter(territoire=="fizi" & quarter=="T2_2024")
test_fizi_dim



test_fizi_weight<-temp |> filter(territoire=="fizi" & quarter=="T2_2024")
test_fizi_weight




test_irumu_det<-indicators_raw |> filter(territoire=="irumu" & quarter=="T2_2024")
test_irumu_det

test_fizi_det<-indicators_raw |> filter(territoire=="fizi" & quarter=="T2_2024")
test_fizi_det