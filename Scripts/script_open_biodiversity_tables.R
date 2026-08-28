rm(list = ls())

library(readr)
library(sf)
library(terra)

folder <- "C:/Users/Proprietário/Documents/Data_paper_PELD_LAND/Github_2026_08_21d/Biodiversity_tables"

files <- list.files(
  path = folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Removing files that are metadata
files <- files[
  !grepl("metadata", basename(files), ignore.case = TRUE)
]

# Checking imported files
basename(files)

data_list <- lapply(
  files,
  read_delim,
  delim = ",",
  locale = locale(decimal_mark = "."),
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
)

names(data_list) <- tools::file_path_sans_ext(basename(files))
list2env(data_list, envir = .GlobalEnv)


#Anura
points <- st_as_sf(
  Anura_survey,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_anura <- vect(points)

plot(
  points_anura,
  pch = 19,
  main = "Anura phase1"
)


#Bird (phase 1)

# Subset Phase 1
Bird_phase1 <- subset(
  Bird_plot_information,
  project_phase == "1"
)

points <- st_as_sf(
  Bird_phase1,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_bird_phase1 <- vect(points)

plot(
  points_bird_phase1,
  pch = 19,
  main = "Bird phase 1"
)


#Bird (phase 2)

# Subset Phase 2
Bird_phase2 <- subset(
  Bird_plot_information,
  project_phase == "2"
)

points <- st_as_sf(
  Bird_phase2,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_bird_phase2 <- vect(points)

plot(
  points_bird_phase2,
  pch = 19,
  main = "Bird phase 2"
)


### Euglossini (phase 1)

# Subset Phase 1
Euglossini_phase1 <- subset(
  Euglossini_survey_phase1,
  project_phase == "1"
)

points <- st_as_sf(
  Euglossini_phase1,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_euglossini_phase1 <- vect(points)

plot(
  points_euglossini_phase1,
  pch = 19,
  main = "Euglossini phase 1"
)


### Euglossini (phase 2)

# Subset Phase 2
Euglossini_phase2 <- subset(
  Euglossini_survey_phase2,
  project_phase == "2"
)

points <- st_as_sf(
  Euglossini_phase2,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_euglossini_phase2 <- vect(points)

plot(
  points_euglossini_phase2,
  pch = 19,
  main = "Euglossini phase 2"
)


### Freshwater 

points <- st_as_sf(
  Freshwater_survey,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_freshwater <- vect(points)

plot(
  points_freshwater,
  pch = 19,
  main = "Freshwater phase 1"
)


#### Helminth

points <- st_as_sf(
  Helminth_survey,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_helminth <- vect(points)

plot(
  points_helminth,
  pch = 19,
  main = "Helminth phase 2"
)



### Herpetofauna (phase 1)

# Subset Phase 1
Herpeto_phase1 <- subset(
  Herpeto_plot_information,
  project_phase == "1"
)

points <- st_as_sf(
  Herpeto_phase1,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_Herpeto_phase1 <- vect(points)

plot(
  points_Herpeto_phase1,
  pch = 19,
  main = "Herpetofauna phase 1"
)



### Herpetofauna (phase 2)

# Subset Phase 2
Herpeto_phase2 <- subset(
  Herpeto_plot_information,
  project_phase == "2"
)

points <- st_as_sf(
  Herpeto_phase2,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_Herpeto_phase2 <- vect(points)

plot(
  points_Herpeto_phase2,
  pch = 19,
  main = "Herpetofauna phase 2"
)


#### Large mammal

Large_mammals_plot_information <- Large_mammals_plot_information[
  !is.na(Large_mammals_plot_information$Long) &
    !is.na(Large_mammals_plot_information$Lat),
]

points <- st_as_sf(
  Large_mammals_plot_information,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_large_mammal <- vect(points)

plot(
  points_large_mammal,
  pch = 19,
  main = "Large mammal phase 2"
)


#### Small mammal

Small_mammals_survey <- Small_mammals_survey[
  !is.na(Small_mammals_survey$Long) &
    !is.na(Small_mammals_survey$Lat),
]

points <- st_as_sf(
  Small_mammals_survey,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

points_small_mammal <- vect(points)

plot(
  points_small_mammal,
  pch = 19,
  main = "Small mammal phase 2"
)



### Forest wood plant (phase 1)

# Subset Phase 1
Forest_wood_phase1 <- subset(
  Woody_plant_plot_information,
  project_phase == "1" &
    vegetation_type %in% c("riparian_forest", "sdtf")
)

points <- st_as_sf(
  Forest_wood_phase1,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

Forest_wood_phase1 <- vect(points)

plot(
  Forest_wood_phase1,
  pch = 19,
  main = "Forest woody plant phase 1"
)


### Forest wood plant (phase 2)

# Subset Phase 2
Forest_wood_phase2 <- subset(
  Woody_plant_plot_information,
  project_phase == "2" &
    vegetation_type %in% c("riparian_forest", "sdtf")
)

points <- st_as_sf(
  Forest_wood_phase2,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

Forest_wood_phase2 <- vect(points)

plot(
  Forest_wood_phase2,
  pch = 19,
  main = "Forest woody plant phase 2"
)



### Savanna wood plant (phase 1)

# Subset Phase 1
Savanna_wood_phase1 <- subset(
  Woody_plant_plot_information,
  project_phase == "1" &
    vegetation_type %in% c("savanna", "forested_savanna")
)

points <- st_as_sf(
  Savanna_wood_phase1,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

Savanna_wood_phase1 <- vect(points)

plot(
  Savanna_wood_phase1,
  pch = 19,
  main = "Savanna woody plant phase 1"
)


### Savanna wood plant (phase 2)

# Subset Phase 2
Savanna_wood_phase2 <- subset(
  Woody_plant_plot_information,
  project_phase == "2" &
    vegetation_type %in% c("savanna", "forested_savanna")
)

points <- st_as_sf(
  Savanna_wood_phase2,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
)

Savanna_wood_phase2 <- vect(points)

plot(
  Savanna_wood_phase2,
  pch = 19,
  main = "Savanna woody plant phase 2"
)
