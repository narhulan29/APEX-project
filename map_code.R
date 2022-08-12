
# Load packages -----------------------------------------------------------
library(readr)              # for loading csv files
library(ggplot2)            # for plotting
library(rnaturalearth)      # for mapping (ne_countries)
library(rnaturalearthdata)  # for mapping
library(rgeos)              # to get centroids
library(rworldmap)          # for mapping (getMap)
library(tidyverse)          # for tidying data
library(janitor)            # for clean_names function

# Load data -----------------------------------------------------------
setwd("~//Documents/Research/Sex ratio/APEX project/Data + code/data_190722")

divorcerate_1549 <- read.csv("DR(15-49)_full.csv", header = T, sep = ",", dec = ".", na.string = "NA")
famstructure <- read.csv("family_structure_full.csv", header = T, sep = ",", dec = ".", na.string = "NA")
childbirth <- read.csv("Mean age of women at childbirth_full.csv", header = T, sep = ",", dec = ".", na.string = "NA")
marriagerate_1549 <- read.csv("MR(15-49)_full.csv", header = T, sep = ",", dec = ".", na.string = "NA")
divorcerate <- read.csv("overall_divorce_rate.csv", header = T, sep = ",", dec = ".", na.string = "NA")
marriagerate <- read.csv("overall_marriage_rate.csv", header = T, sep = ",", dec = ".", na.string = "NA")

# Clean data -----------------------------------------------------------

divorcerate_1549 <- divorcerate_1549 %>%
  clean_names() %>%
  select(country = country) %>%
  mutate(divorcerate_1549 = 1) %>%
  mutate(country = recode(country, 
                "Bahamas" = "The Bahamas", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei", 
                "CÃTte d'Ivoire" = "Ivory Coast", "China, Hong Kong SAR" = "China", "Czechia" = "Czech Republic", 
                "Eswatini" = "Swaziland", "Lao PeopleÃ­s Democratic Republic" = "Laos", "Republic of Korea" = "South Korea", 
                "Republic of Moldova" = "Moldova", "Russian Federation" = "Russia", "Serbia" = "Republic of Serbia", 
                "The former Yugoslav Republic of Macedonia" = "Macedonia", "Timor-Leste" = "East Timor", 
                "Venezuela (Bolivarian Republic of)" = "Venezuela")) %>%
  na.omit()

famstructure <- famstructure %>%
  clean_names() %>%
  select(country = country) %>%
  mutate(famstructure = 1) %>%
  mutate(country = recode(country, 
                          "Bahamas" = "The Bahamas", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei",
                          "CÃ´te d'Ivoire" = "Ivory Coast", "China, Hong Kong SAR" = "China", "Congo" = "Republic of Congo", 
                          "Czechia" = "Czech Republic","Dem. Republic of the Congo" = "Democratic Republic of the Congo",
                          "Iran (Islamic Republic of)" = "Iran", "Lao People's Dem. Republic" = "Laos", "Republic of Korea" = "South Korea", 
                          "Republic of Moldova" = "Moldova","Russian Federation" = "Russia", "Serbia" = "Republic of Serbia", 
                          "Timor-Leste" = "East Timor", "Venezuela (Bolivarian Republic of)" = "Venezuela", "Viet Nam" = "Vietnam")) %>%
  na.omit()

childbirth <- childbirth %>%
  clean_names() %>%
  select(country = country) %>%
  mutate(childbirth = 1) %>%
  mutate(country = recode(country, 
                          "Korea" = "South Korea", "Slovak Republic" = "Slovakia", "United States" = "United States of America")) %>%
  na.omit()

marriagerate_1549 <- marriagerate_1549 %>%
  clean_names() %>%
  select(country = country) %>%
  mutate(marriagerate_1549 = 1) %>%
  mutate(country = recode(country, 
                          "Bahamas" = "The Bahamas", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei", 
                          "CÃTte d'Ivoire" = "Ivory Coast", "China, Hong Kong SAR" = "China", "Czechia" = "Czech Republic", 
                          "Eswatini" = "Swaziland", "Lao PeopleÃ­s Democratic Republic" = "Laos", "Republic of Korea" = "South Korea", 
                          "Republic of Moldova" = "Moldova", "Russian Federation" = "Russia", "Serbia" = "Republic of Serbia", 
                          "The former Yugoslav Republic of Macedonia" = "Macedonia", "Timor-Leste" = "East Timor", 
                          "Venezuela (Bolivarian Republic of)" = "Venezuela", "Guinea-Bissau" = "Guinea Bissau")) %>%
  na.omit()

divorcerate <- divorcerate %>%
  clean_names() %>%
  select(country = country_or_area) %>%
  mutate(divorcerate = 1) %>%
  mutate(country = recode(country, 
                          "Bahamas" = "The Bahamas", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei",
                          "CÃTte d'Ivoire" = "Ivory Coast", "China, Hong Kong SAR" = "China", "Czechia" = "Czech Republic", "Eswatini" = "Swaziland", 
                          "Iran (Islamic Republic of)" = "Iran", "Lao PeopleÃ­s Democratic Republic" = "Laos", "Republic of Korea" = "South Korea", 
                          "Republic of Moldova" = "Moldova", "Russian Federation" = "Russia", "Serbia" = "Republic of Serbia", 
                          "Timor-Leste" = "East Timor", "Venezuela (Bolivarian Republic of)" = "Venezuela", 
                          "The former Yugoslav Republic of Macedonia" = "Macedonia")) %>%
  na.omit()

marriagerate <- marriagerate %>%
  clean_names() %>%
  select(country = country_or_area_x) %>%
  mutate(marriagerate = 1) %>%
  mutate(country = recode(country, 
                          "Bahamas" = "The Bahamas", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei",
                          "CÃTte d'Ivoire" = "Ivory Coast", "China, Hong Kong SAR" = "China", "Czechia" = "Czech Republic", "Eswatini" = "Swaziland", 
                          "Iran (Islamic Republic of)" = "Iran", "Lao PeopleÃ­s Democratic Republic" = "Laos", "Republic of Korea" = "South Korea", 
                          "Republic of Moldova" = "Moldova", "Russian Federation" = "Russia", "Serbia" = "Republic of Serbia", 
                          "Timor-Leste" = "East Timor", "Venezuela (Bolivarian Republic of)" = "Venezuela", 
                          "The former Yugoslav Republic of Macedonia" = "Macedonia", "Guinea-Bissau" = "Guinea Bissau")) %>%
  na.omit()

# Merge data
country_data <- as.data.frame(Reduce(function(...) merge(..., all = T), 
                                     list(divorcerate_1549,
                                          famstructure,
                                          childbirth,
                                          marriagerate_1549,
                                          divorcerate,
                                          marriagerate)))

country_data <- country_data %>%
  mutate(country = tolower(factor(country))) %>%
  gather(., variable, dataset, 
        divorcerate_1549:marriagerate, 
        factor_key = T) %>%
  distinct() %>% # remove duplicated rows
  mutate(variable = recode(variable, 
                          "divorcerate_1549" = "divorce rate (15-49)",
                          "famstructure" = "family structure",
                          "childbirth" = "age at first birth",
                          "marriagerate_1549" = "marriage rate (15-49)",
                          "divorcerate" = "divorce rate",
                          "marriagerate" = "marriage rate"), 
         variable = factor(variable, levels = c("family structure",
                                                "age at first birth",
                                                "divorce rate (15-49)",
                                                "marriage rate (15-49)",
                                                "divorce rate",
                                                "marriage rate")))

# Remove redundant data frames
rm(divorcerate_1549, famstructure, childbirth, marriagerate_1549, divorcerate, marriagerate)

# Make map -----------------------------------------------------------

# Get world map
worldmap <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(!(continent %in% "Antarctica")) %>%
  select(country = sovereignt, geometry, continent) %>%
  mutate(country = tolower(country))
  

# Get central coordinates for countries
coords <- as.data.frame(
  gCentroid(
    getMap(resolution = "low"), 
    byid = T)) %>%
  mutate(country = factor(rownames(.)), 
         country = tolower(country))

rownames(coords) <- NULL

# Merge coordinates and data indicators together
coords <- merge(coords, country_data, all.y = T, by = "country")
coords <- coords %>%
  mutate(var_x = ifelse(dataset == 1, x, NA), 
         var_y = ifelse(dataset == 1, y, NA))

# Plot world map
ggplot(worldmap) +
  geom_sf(colour = "black", fill = "white") +
  geom_jitter(data = coords, 
              aes(x = var_x, y = var_y, fill = variable), 
              width = 1.5, height = 1.5, shape = 21, 
              alpha = 0.8, size = 1.5, colour = "black") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "pink")) +
  coord_sf(expand = F) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"))

# Plot map of Europe
ggplot(worldmap) + 
  geom_sf(colour = "black", fill = "white") +
  geom_jitter(data = coords, 
              aes(x = var_x, y = var_y, fill = variable), 
              width = 0.5, height = 1, shape = 21, 
              alpha = 0.8, size = 2.5, colour = "black") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "pink")) +
  coord_sf(xlim = c(-25,70), ylim = c(35,70), expand = FALSE) + # Europe's coordinates
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"))

# Plot map of Africa
ggplot(worldmap) + 
  geom_sf(colour = "black", fill = "white") +
  geom_jitter(data = coords, 
              aes(x = var_x, y = var_y, fill = variable), 
              width = 1, height = 1, shape = 21, 
              alpha = 0.8, size = 2, colour = "black") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "pink")) +
  coord_sf(xlim = c(-25,70), ylim = c(40, -40), expand = FALSE) + # Africas's coordinates
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"))

# Plot map of S. America
ggplot(worldmap) + 
  geom_sf(colour = "black", fill = "white") +
  geom_jitter(data = coords, 
              aes(x = var_x, y = var_y, fill = variable), 
              width = 1, height = 1, shape = 21, 
              alpha = 0.8, size = 2, colour = "black") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "pink")) +
  coord_sf(xlim = c(-95, -25), ylim = c(40, -60), expand = FALSE) + # S. America's coordinates
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"))




  