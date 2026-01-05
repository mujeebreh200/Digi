# 1. Loading Packages
# Core libraries for data science and web application
library(shiny)        # Application framework
library(tidyverse)    # Data manipulation (dplyr, ggplot2, tidyr, pipes)
library(plotly)       # Interactive visualizations
library(bslib)        # Modern UI & Floating elements
library(DT)           # Interactive Tables
library(janitor)      # Cleaning messy header names
# 2. Loading and Privacy Cleaning
rta_raw <- read_csv("Questionaire.csv")

rta_clean <- rta_raw %>%
  clean_names() %>% 
  # --- PRIVACY STEP: REMOVE CONTACT NUMBER ---
  select(-contact_number) %>% 
  # --------------------------------------------
rename(
  helmet = use_of_helmet,
  area = area_of_accident,
  gcs = gcs_score,
  speed = speed_at_a_the_time_of_accident_in_km_hr,
  injury_type = type_of_tissue_injury,
  face_area = area_of_face_involved
) %>%
  # Fix Excel-to-R Date errors in GCS scores
  mutate(gcs = case_when(
    gcs == "15-Oct" ~ "10-15",
    gcs == "5-Oct"  ~ "5-10",
    TRUE ~ gcs
  ))
# 3. Filtering and Factors
df_final <- rta_clean %>%
  # Filter for records with known Speed and Area
  filter(speed != "Do not Know", area %in% c("Urban", "Rural")) %>%
  mutate(
    area = factor(area, levels = c("Urban", "Rural")),
    helmet = factor(helmet, levels = c("Yes", "No")),
    # Define Speed Parameters as an ordered Factor
    speed = factor(speed, levels = c(">20", ">40", ">60", ">80")),
    # Define GCS Severity
    gcs = factor(gcs, levels = c("1-5", "5-10", "10-15"))
  ) %>%
  drop_na(speed, helmet, area, gcs)
# 4. Pivoting (Wide to Long)
# Splitting multiple facial injury sites into separate observations
face_pivoted <- df_final %>%
  select(name, speed, area, face_area) %>%
  separate_rows(face_area, sep = ";") %>%
  mutate(face_area = trimws(face_area)) %>%
  filter(face_area != "")
# 5. Visualization & Text Details
speed_plot <- ggplot(df_final, aes(x = speed, fill = area)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Collision Speed Distribution by Region",
    subtitle = "Analysis of RTA cases at LUH Hyderabad",
    x = "Speed Parameter (km/hr)",
    y = "Frequency of Accidents",
    caption = "Confidential Clinical Data - Redacted for Research"
  ) +
  # Text Detail: Annotating the High-Risk group
  annotate("text", x = 4, y = 5, label = "High-Speed Critical Zone", 
           color = "red", fontface = "bold", angle = 90) +
  theme_minimal()
