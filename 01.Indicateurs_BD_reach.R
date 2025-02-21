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
  "data.table", "dplyr", "stringr", "conflicted", "quarto", "knitr", "janitor", "zoo"
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
conflicted::conflicts_prefer(lubridate::quarter)
conflicts_prefer(lubridate::year)
source("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/DRC-CAT/SHAEPES/shaepes/SHAEPES_functions.R")



# bases REACH -------------------------------------------------------


REACH_MEB<-readRDS("REACH_MEB.rds")
REACH_PAM<-readRDS("REACH_PAM.rds")

first_date<-REACH_MEB |> select(date) |> unique() |> slice(1) |> pull()
last_date<-REACH_MEB |> select(date) |> unique() |> slice(n())|> pull()

first_date_PAM<-REACH_PAM |> select(date) |> unique() |> slice(1) |> pull()
last_date_PAM<-REACH_PAM |> select(date) |> unique() |> slice(n())|> pull()


# create a frame  MEB---------------------------------------------------------

# Create a sequence of months for the date range
all_months <- seq.Date(from=as.Date(first_date),
  # from = as.Date("2022-09-01"), 
                       # to = today(), 
                       to=as.Date(last_date),
                       by = "month")

# Create a complete frame with all combinations of months, province, and territoire
complete_frame <- REACH_MEB %>%
  distinct(province, territoire) %>%
  expand_grid(date = all_months)  |> 
  mutate(
    quarter = paste0("T", quarter(date), "_", year(date)),  # Add quarter if missing
    annee = year(date)  # Add year if missing
  )

REACH_MEB <- complete_frame  %>%
  left_join(REACH_MEB, by = c("date", "province", "territoire", "annee", "quarter")) 


## calcul par province quarter-------------------------------------------------------
REACH_MEB_prov_quart <- REACH_MEB %>%
  group_by(province, quarter, annee) %>%
  summarise(
    median = median(as.numeric(meb), na.rm = TRUE),  # Calculate the median of `meb`
    .groups = "drop"  # Ensure the output is ungrouped after summarise
  ) %>%
  arrange(province, annee, quarter) %>%  # Ensure data is ordered
  group_by(province) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_4_prov = rollapply(median, width = 4, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup() |> 
  group_by(province) |> 
  fill(rolling_sd_4_prov, .direction = "downup") %>%  # Fill missing rolling_sd_4 values with the latest value
  ungroup() |> 
  select(-median)

head(REACH_MEB_prov_quart)

## calcul par territoire quarter -------------------------------------------------------
REACH_MEB_terr_quart <- REACH_MEB %>%
  group_by(province, territoire, quarter, annee) %>%
  summarise(
    median = median(as.numeric(meb), na.rm = TRUE),  # Calculate the median of `meb`
    .groups = "drop"  # Ensure the output is ungrouped after summarise
  ) %>%
  arrange(province, territoire, annee, quarter) %>%  # Ensure data is ordered
  group_by(province, territoire) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_4 = rollapply(median, width = 4, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup() |> 
  group_by(province, territoire) %>%
  fill(rolling_sd_4, .direction = "downup") %>%  # Fill missing rolling_sd_4 values with the latest value
  ungroup()


## calcul par territoire month -------------------------------------------------------
REACH_MEB_terr_month <- REACH_MEB %>%
  arrange(province, territoire, annee, quarter, date) %>%  # Ensure data is ordered
  group_by(province, territoire) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_12 = rollapply(meb, width = 12, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup()


## calcul par territoire -------------------------------------------------------
temp1 <- merge(REACH_MEB_terr_month, REACH_MEB_terr_quart, 
               by = c("province", "territoire", "annee", "quarter"), all.x = TRUE)
temp1 <- merge(temp1, REACH_MEB_prov_quart, 
               by = c("province", "annee", "quarter"), all.x = TRUE)

temp1 <- temp1 |> 
  arrange(province, territoire, annee, quarter, date) |> 
  mutate(new_roll12 = rolling_sd_12) |> 
  group_by(province, territoire) %>%
  fill(new_roll12, .direction = "downup") %>%
  ungroup() |> 
  rowwise() |> 
  mutate(moy_roll = ifelse(is.na(new_roll12), 
                           rolling_sd_4, 
                           mean(c(new_roll12, rolling_sd_4), na.rm = TRUE))) |> 
  ungroup()

temp1 <- temp1 |> mutate(
  final=ifelse(is.na(rolling_sd_12), moy_roll, rolling_sd_12),
  final=ifelse(is.na(final), rolling_sd_4_prov, final)
)

summary(temp1)


## on slice sur la dernière valeur du trimestre ----------------------------

indicateurs_meb <- temp1 %>%
  group_by(province, territoire, annee, quarter) %>%
  slice_max(order_by = date, n = 1) %>%
  ungroup() |> 
  rename(count=final) |> 
  mutate(indicator="E003") |> 
  select(province, territoire, annee, quarter,count, indicator )




# create a frame  PAM---------------------------------------------------------

# Create a sequence of months for the date range
all_months <- seq.Date(from=as.Date(first_date_PAM),
                       # from = as.Date("2022-09-01"), 
                       # to = today(), 
                       to=as.Date(last_date_PAM),
                       by = "month")

# Create a complete frame with all combinations of months, province, and territoire
complete_frame <- REACH_PAM %>%
  distinct(province, territoire) %>%
  expand_grid(date = all_months)  |> 
  mutate(
    quarter = paste0("T", quarter(date), "_", year(date)),  # Add quarter if missing
    annee = year(date)  # Add year if missing
  )

REACH_PAM <- complete_frame  %>%
  left_join(REACH_PAM, by = c("date", "province", "territoire", "annee", "quarter")) 

## calcul par province quarter -------------------------------------------------------
# REACH_PAM_prov_quart <- REACH_PAM %>%
#   group_by(province, quarter, annee) %>%
#   summarise(
#     median_prov = median(as.numeric(pam), na.rm = TRUE),  # Calculate the median of `meb`,
#     .groups = "drop"  # Ensure the output is ungrouped after summarise
#   ) |> 
#   fill(median_prov, .direction = "up") 

## calcul par province quarter-------------------------------------------------------

#il y a tellement de valeurs manquantes qu'on doit d'abord roll the median 
REACH_PAM_prov_quart <- REACH_PAM %>%
  group_by(province, quarter, annee) %>%
  summarise(
    median = median(as.numeric(pam), na.rm = TRUE),  # Calculate the median of `meb`
    .groups = "drop"  # Ensure the output is ungrouped after summarise
  )%>%
  group_by(province) |> 
  fill(median, .direction = "downup") %>%  # Fill missing rolling_sd_4 values with the latest value 
  arrange(province, annee, quarter) %>%  # Ensure data is ordered
  group_by(province) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_4_prov = rollapply(median, width = 4, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup() |> 
  mutate(
  rolling_sd_4_prov=ifelse(rolling_sd_4_prov==0, NA_integer_, rolling_sd_4_prov)
)



head(REACH_PAM_prov_quart)


## calcul par territoire quarter -------------------------------------------------------
REACH_PAM_terr_quart <- REACH_PAM %>%
  group_by(province, territoire, quarter, annee) %>%
  summarise(
    median = median(as.numeric(pam), na.rm = TRUE),  # Calculate the median of `meb`
    .groups = "drop"  # Ensure the output is ungrouped after summarise
  ) %>%
  arrange(province, territoire, annee, quarter) %>%  # Ensure data is ordered
  group_by(province, territoire) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_4 = rollapply(median, width = 3, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup() |> 
  group_by(province, territoire) %>%
  fill(rolling_sd_4, .direction = "up") %>%  # Fill missing rolling_sd_4 values with the latest value
  ungroup()


## calcul par territoire month -------------------------------------------------------
REACH_PAM_terr_month <- REACH_PAM %>%
  arrange(province, territoire, annee, quarter, date) %>%  # Ensure data is ordered
  group_by(province, territoire) %>%  # Group by province to calculate rolling standard deviation
  mutate(
    rolling_sd_12 = rollapply(pam, width = 2, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
  ) %>%
  ungroup()


## final table PAM -------------------------------------------------------
# temp1<-merge(REACH_PAM_terr_month, REACH_PAM_terr_quart, by=c("province", "territoire", "annee", "quarter"), all.x=T)
# temp1<-merge(temp1, REACH_PAM_prov_quart, by=c("province", "annee", "quarter"), all.x=T)
# 
# temp1<-temp1 |> mutate(pam=ifelse(is.na(pam), median, pam)) |> 
#   mutate(pam=ifelse(is.na(pam), median_prov, pam)) |> 
#   arrange(province, territoire, annee, quarter, date) %>%  # Ensure data is ordered
#   group_by(province, territoire) %>%  # Group by province to calculate rolling standard deviation
#   mutate(
#     rolling_sd_12 = rollapply(pam, width = 10, FUN = sd, align = "right", fill = NA)  # Rolling SD over 4 quarters
#   ) %>%
#   ungroup() |> 
#   mutate(rolling_sd_12=ifelse(is.na(rolling_sd_12), rolling_sd_4, rolling_sd_12))


temp1 <- merge(REACH_PAM_terr_month, REACH_PAM_terr_quart, 
               by = c("province", "territoire", "annee", "quarter"), all.x = TRUE)
temp1 <- merge(temp1, REACH_PAM_prov_quart, 
               by = c("province", "annee", "quarter"), all.x = TRUE)

temp1 <- temp1 |> 
  arrange(province, territoire, annee, quarter, date) |> 
  mutate(new_roll12 = rolling_sd_12) |> 
  group_by(province, territoire) %>%
  fill(new_roll12, .direction = "downup") %>%
  ungroup() |> 
  rowwise() |> 
  mutate(moy_roll = ifelse(is.na(new_roll12), 
                           rolling_sd_4, 
                           mean(c(new_roll12, rolling_sd_4), na.rm = TRUE))) |> 
  ungroup()

temp1 <- temp1 |> mutate(
  final=ifelse(is.na(rolling_sd_12), moy_roll, rolling_sd_12),
  final=ifelse(is.na(final), rolling_sd_4_prov, final)
) |> 
  arrange(province, territoire, annee, quarter) |> 
  group_by(province, territoire) %>%
  fill(final, .direction = "downup") %>%  # Fill missing rolling_sd_4 values with the latest value
  ungroup() |> 
  group_by(province) %>%
  fill(final, .direction = "downup") %>%  # Fill missing rolling_sd_4 values with the latest value
  ungroup()

summary(temp1)



## on slice sur la dernière valeur du trimestre ----------------------------

indicateurs_pam <- temp1 %>%
  group_by(province, territoire, annee, quarter) %>%
  slice_max(order_by = date, n = 1) %>%
  ungroup() |> 
  rename(count=final) |> 
  mutate(indicator="E004") |> 
  select(province, territoire, annee, quarter,count, indicator )


# BD totale REACH ---------------------------------------------------------
combined_total<-rbind(indicateurs_meb, indicateurs_pam)
saveRDS(combined_total, "indicateurs_BD_reach.rds")


summary(combined_total)


