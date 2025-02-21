###########################################################################
###########################################################################
###                                                                     ###
###                          INDICATEURS ENV                            ###
###                                                                     ###
###########################################################################
###########################################################################


# Included indicators are (for each dekad):
#   
#   10 day rainfall [mm] (rfh)
# rainfall 1-month rolling aggregation [mm] (r1h)
# rainfall 3-month rolling aggregation [mm] (r3h)
# rainfall long term average [mm] (rfh_avg)
# rainfall 1-month rolling aggregation long term average [mm] (r1h_avg)
# rainfall 3-month rolling aggregation long term average [mm] (r3h_avg)
# rainfall anomaly [%] (rfq)
# rainfall 1-month anomaly [%] (r1q)
# rainfall 3-month anomaly [%] (r3q)
# The administrative units used for aggregation are based on WFP data and contain 
# a Pcode reference attributed to each unit. The number of input pixels used to create the aggregates,
# is provided in the n_pixelscolumn.


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

data_env_raw <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/Indicateurs environnement V1.xlsx")

cod_rainfall_adm2_5ytd <- read_csv("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/cod-rainfall-adm2-5ytd.csv")
cod_rainfall_adm2_5ytd <- cod_rainfall_adm2_5ytd[-1, ]
#il faut récupérer le code des admin2/1
admincode <- read_excel("C:/Users/MERCYCORPS/mercycorps.org/CD - Crisis Analysis Team (CAT) - 01_Base de données/SHAEPES ALL/cod_adminboundaries_tabulardata.xlsx",
                        sheet=3)|> clean_names() |> 
  select(starts_with("adm"), -contains("0")) |> 
  mutate(
    adm2_fr=str_to_lower(adm2_fr),
    adm1_fr=str_to_lower(adm1_fr)
    ) |>
    unique() |> 
  mutate(adm2_fr=ifelse(adm2_fr=="beni", "beni-ville", adm2_fr),
         adm2_fr=ifelse(adm2_fr=="oïcha", "beni", adm2_fr))

table(admincode$adm2_fr)

cod_rainfall_adm2_5ytd<-merge(cod_rainfall_adm2_5ytd, admincode, by.x="ADM2_PCODE", by.y="adm2_pcode", all.x=T)

cod_rainfall_adm2_5ytd<-cod_rainfall_adm2_5ytd |> 
  mutate(
    quarter = paste0("T", lubridate::quarter(date), "_", lubridate::year(date)),
    annee=lubridate::year(date)
  ) 

complete_frame_ind<-readRDS("complete_frame.rds")  |> mutate(annee=as.numeric(annee)) |> filter(!annee==2022  )



# EH003 -------------------------------------------------------------
  
  tempEH003<-cod_rainfall_adm2_5ytd |> 
    rename(province=adm1_fr, 
           territoire=adm2_fr) |> 
    mutate(rfh=as.numeric(rfh)) |> 
    group_by(annee, quarter, province, territoire) |> 
    mutate(precipitations_cum=sum(rfh, na.rm=T)) |> 
    ungroup() |> 
    group_by(annee, quarter, province) |> 
    mutate(precipitations_cum_mean=abs(mean(precipitations_cum, na.rm=T) ))|> 
    ungroup()  |> 
    mutate(
        EH003=precipitations_cum_mean-precipitations_cum
      ) |> 
    select(annee, quarter, province, territoire, EH003, precipitations_cum) |> 
    unique() |> 
    filter(!is.na(annee))
  
  table(tempEH003$territoire)

  # tempEH003<-  tempEH003 |> 
  # mutate(territoire=ifelse(territoire=="beni",  "beni-ville", territoire),
  #                       territoire=ifelse(territoire=="oïcha",  "oicha", territoire)) 

# autres indicateurs  -------------------------------------------------------------
data_env <-data_env_raw |> rename(quarter=Trimestre) |> clean_names() |> 
    mutate(territoire=str_to_lower(territoire), 
           territoire=ifelse(territoire=="beni ville", "beni-ville", territoire)) |> 
  select(quarter, province, territoire,  starts_with("eh"), ndvi, pentes) |> 
  mutate(
    province=str_to_lower(province),
    territoire=str_to_lower(territoire),
    annee=2024,
  ) |> 
  mutate(province=ifelse(province=="sud kivu", "sud-kivu",
                         ifelse(province=="nord kivu", "nord-kivu",province))) |> 
  select(-eh_008_proxy_exposition_to_land_slopes) 

table(data_env$territoire)


data_env_comb_fr<-complete_frame_ind |> left_join(data_env, by=c("province", "territoire", "annee", "quarter"))

# merge -------------------------------------------------------------------
data_env_comb<-merge(tempEH003, data_env_comb_fr, by=c("annee", "quarter", "province", "territoire"), all.y=T) 


data_env_comb<-data_env_comb |> 
  mutate(
    EH008=precipitations_cum*ndvi*pentes,
    eh005_qualite_des_sources_deau_naturelles=as.numeric(eh005_qualite_des_sources_deau_naturelles)
  ) |> 
  rename(EH004=eh004_stabilite_de_la_vegetation_et_sensibilite_aux_changements,
         EH005=eh005_qualite_des_sources_deau_naturelles,
         EH007=eh007_quality_of_air
  ) |> 
  select(annee, quarter, province, territoire,  starts_with("eh")) 





# total frame -------------------------------------------------------------

data_env_comb<-data_env_comb |> mutate(annee = as.numeric(sub(".*_(\\d{4})$", "\\1", quarter)),
                                       territoire=ifelse(territoire=="beni ville", "beni-ville", territoire))




complete <- data_env_comb |>
  arrange(annee, quarter, territoire, province) |>
  group_by(province, territoire) |>
  fill(EH003, EH004, EH005, EH007, EH008, .direction = "downup") |>
  ungroup()



complete<-complete |>
  pivot_longer(cols=!c(annee, quarter, province, territoire), names_to = "indicator", values_to = "count")

colnames(data_env_comb)



# table(data_env_comb$territoire)

saveRDS(complete, "indicateurs_env.rds")












# source rainfall ---------------------------------------------------------




# https://data.humdata.org/dataset/cod-rainfall-subnational
# 
# 
# 10 day rainfall [mm] (rfh)
# rainfall 1-month rolling aggregation [mm] (r1h)
# rainfall 3-month rolling aggregation [mm] (r3h)
# rainfall long term average [mm] (rfh_avg)
# rainfall 1-month rolling aggregation long term average [mm] (r1h_avg)
# rainfall 3-month rolling aggregation long term average [mm] (r3h_avg)
# rainfall anomaly [%] (rfq)
# rainfall 1-month anomaly [%] (r1q)
# rainfall 3-month anomaly [%] (r3q)
