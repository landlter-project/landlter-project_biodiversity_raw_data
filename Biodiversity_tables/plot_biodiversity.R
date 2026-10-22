
# Install dependencies once, if needed:
# install.packages(c("here", "readr", "sf", "terra"))

library(readr)
library(sf)
library(terra)

# Explicitly identify the project root, independently of the computer's path.
here::i_am("Biodiversity_tables/plot_biodiversity.R")
folder <- here::here("Biodiversity_tables")
dir.create(folder, recursive = TRUE, showWarnings = FALSE)

# biodiversity tables
# https://github.com/landlter-project/landlter-project_biodiversity_raw_data/tree/main/Biodiversity_tables
base_url <- paste0(
  "https://raw.githubusercontent.com/landlter-project/",
  "landlter-project_biodiversity_raw_data/main/Biodiversity_tables/"
)

# Tables used for the analyses and plots below.
required_tables <- c(
  "Anura_survey.csv",
  "Bird_plot_information.csv",
  "Euglossini_survey_phase1.csv",
  "Euglossini_survey_phase2.csv",
  "Freshwater_survey.csv",
  "Helminth_survey.csv",
  "Herpeto_plot_information.csv",
  "Large_mammals_plot_information.csv",
  "Small_mammals_survey.csv",
  "Woody_plant_plot_information.csv"
)

download_table <- function(filename) {
  destination <- here::here("Biodiversity_tables", filename)
  if (file.exists(destination)) return(invisible(destination))

  url <- paste0(base_url, filename)
  message("Downloading ", filename, " from ", url)
  # Use a temporary file so an interrupted download is not reused as a CSV.
  temporary <- tempfile(tmpdir = folder, fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  tryCatch({
    status <- utils::download.file(url, temporary, mode = "wb", quiet = FALSE)
    if (status != 0L || file.info(temporary)$size == 0) {
      stop("Download failed or returned an empty file.")
    }
    if (!file.rename(temporary, destination)) {
      stop("Could not move the downloaded table to ", destination)
    }
  }, error = function(e) {
    stop(
      "Could not download ", filename, " from ", url,
      ". Check your internet connection and whether the CSV has been published. ",
      conditionMessage(e), call. = FALSE
    )
  })
  invisible(destination)
}

# Download all tables, including metadata, only when missing locally.
all_tables <- c(
  "Anura_metadata.csv",
  "Anura_survey.csv",
  "Bird_metadata.csv",
  "Bird_mist_net_recapture.csv",
  "Bird_mist_net_survey.csv",
  "Bird_plot_information.csv",
  "Bird_transect_survey.csv",
  "Euglossini_metadata.csv",
  "Euglossini_survey_phase1.csv",
  "Euglossini_survey_phase2.csv",
  "Freshwater_metadata.csv",
  "Freshwater_survey.csv",
  "Helminth_metadata.csv",
  "Helminth_survey.csv",
  "Herpeto_metadata.csv",
  "Herpeto_plot_information.csv",
  "Herpeto_survey.csv",
  "Large_mammals_metadata.csv",
  "Large_mammals_plot_information.csv",
  "Large_mammals_survey.csv",
  "Small_mammals_metadata.csv",
  "Small_mammals_survey.csv",
  "Summary_LAND_LTER.csv",
  "Woody_plant_metadata.csv",
  "Woody_plant_plot_information.csv",
  "Woody_plant_survey.csv"
)

invisible(lapply(all_tables, download_table))

# Import only the ten tables used in the original analyses and plots.
files <- here::here("Biodiversity_tables", required_tables)

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



