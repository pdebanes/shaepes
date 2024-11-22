
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



# SHAEPES_history <- read_excel("~/DRC-CAT/SHAEPES/data Q2/Test V4 SHAEPES.xlsx")
Uvira <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING - tentative Uvira.xlsx", 
                                                             sheet = "Analysis_terr. 1", skip = 9) |> mutate(territoire="Uvira", trimestre="Q2_2024")

fizi <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING FIZI.xlsx", 
                                                  sheet = "Analysis_terr. 1", skip = 9)|> mutate(territoire="Fizi", trimestre="Q2_2024")

irumu <- read_excel("~/DRC-CAT/SHAEPES/data Q2/TEMPLATE V4 SHAEPES MONITORING IRUMU.xlsx", 
                                                   sheet = "Analysis_terr. 1", skip = 9)|> mutate(territoire="Irumu", trimestre="Q2_2024")

total_reset_q2<-rbind(Uvira, fizi, irumu) |> clean_names()|>  dplyr::rename(w_indic=weighted_indicator_impact_score) |> relocate(territoire)




# compute score by dimension ----------------------------------------------
table(total_reset_q2$w_indic)
total_score_dim<-total_reset_q2 |> mutate(w_indic=as.numeric(w_indic))|> group_by(territoire, trimestre, dimension) |> reframe(score_dim=sum(w_indic))


# weighting of program RESET ----------------------------------------------
table(total_score_dim$dimension)
dim_weight_reset <- data.frame(
  dim_name = c("Security", 
               "Humanitarian", 
               "Access", 
               "Political and social inclusion", 
               "Environmental Hazards", 
               "Economic"),
  dim_weight = c(0.25, 0.25, 0.15, 0.1, 0.1, 0.15)
)

total_score_dim<-merge(total_score_dim, dim_weight_reset, by.x="dimension", by.y="dim_name", all.x=T)


total_score_dim<-total_score_dim |> mutate(tot=score_dim*dim_weight)

total_shaepes<-total_score_dim |> 
  group_by(territoire, trimestre) |> reframe(score_tot=round(sum(tot), 2))

write.xlsx(total_shaepes, "total_shaepes.xlsx")
	
write.xlsx(total_score_dim, "shaepes_score_dim.xlsx")


# détail by dimensions ----------------------------------------------------

# total_reset_q2_clean<-total_reset_q2 |> filter(!code=="S001bis") |> select(territoire, dimension, code, indicator, indicator_impact_score)
total_reset_q2_clean<-total_reset_q2 |> filter(!grepl("bis", code)) |> select(territoire, dimension, code, indicator, indicator_impact_score)
table(total_reset_q2_clean$dimension)

total_reset_q2_clean <- total_reset_q2_clean %>%
  mutate(dimension = recode(dimension,
                            "Access" = "Accès",
                            "Economic" = "Économique",
                            "Environmental Hazards" = "Risques environnementaux",
                            "Humanitarian" = "Humanitaire",
                            "Political and social inclusion" = "Inclusion politique et sociale",
                            "Security" = "Sécurité"))

head(total_reset_q2_clean)

p_bar <- ggplot(total_reset_q2_clean, aes(x = code, y = w_indic, fill = dimension)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(title = "Indicators by Territoire and Dimension",
       x = "Code",
       y = "Weighted Indicator (w_indic)") +
  facet_wrap(~ territoire) +  # Create a separate plot for each territoire
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add axis lines for clarity
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for better readability
  )

# Print the bar plot
print(p_bar)



# graph by dimension by territory -----------------------------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define fill and border color mappings for each territoire
color_mapping <- c(
  "Fizi" = "#FF7F00",          # Orange
  "Irumu" = "gold",            # Yellow
  "Uvira" = "lightsalmon"      # Orange Pastel
)

border_mapping <- c(
  "Fizi" = "#CC6A00",          # Darker Orange
  "Irumu" = "#DAA520",          # Darker Yellow (Goldenrod)
  "Uvira" = "#FFA07A"           # Salmon
)

# Split the dataset by territoire and dimension
split_data <- split(total_reset_q2_clean, list(total_reset_q2_clean$territoire, total_reset_q2_clean$dimension))

# Function to create and save plots
create_plot <- function(data) {
  # Extract current territoire and dimension
  current_territoire <- unique(data$territoire)
  current_dimension <- unique(data$dimension)
  
  # Get fill and border colors based on territoire
  fill_color <- color_mapping[current_territoire]
  border_color <- border_mapping[current_territoire]
  
  # Assign default colors if territoire is not in mapping
  if (is.na(fill_color)) {
    fill_color <- "#CCCCCC"
    border_color <- "#999999"
  }
  
  # Create the bar plot
  p_bar <- ggplot(data, aes(x = code, y = indicator_impact_score)) +
    geom_bar(stat = "identity", fill = fill_color, color = border_color, size = 0.5) +
    labs(
      title = paste("Indicators for", current_territoire, "-", current_dimension),
      x = "Code",
      y = NULL
    ) +
    scale_y_continuous(limits = c(0, 5)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  # Print the plot (optional)
  print(p_bar)
  
  # Create a safe filename
  safe_territoire <- gsub("[^A-Za-z0-9]", "_", current_territoire)
  safe_dimension <- gsub("[^A-Za-z0-9]", "_", current_dimension)
  
  # Define the filename
  filename <- paste0("barplot_", safe_territoire, "_", safe_dimension, ".png")
  
  # Save the plot with white background and high resolution
  ggsave(
    filename = filename,
    plot = p_bar,
    width = 10,
    height = 6,
    bg = "white",
    dpi = 300
  )
}

# Apply the function to each subset of the data
lapply(split_data, create_plot)

