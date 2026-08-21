rm(list = ls())

library(readxl)
library(sf)
library(terra)
library(dplyr)

############# Land LTER Phase 1
#------------------------------------------------
# Table with data 
#------------------------------------------------

setwd("C:\\Users\\Proprietário\\Documents\\Data_paper_PELD_LAND\\Spatial_data_2026_08_20d")
getwd() 
data <- read_excel("Summary_data_LANDLTER.xlsx")

#### Selecting phase 1

# Subset Phase 1
data_phase1 <- subset(
  data,
  Project_phase == "1"
)

#------------------------------------------------
# Spatializing points of biological data
#------------------------------------------------

points <- st_as_sf(
  data_phase1,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

#------------------------------------------------
# Opening Mapbiomas map year 2019 - collection 11 
#https://brasil.mapbiomas.org/downloads/mapas-para-download-geotiff/
#------------------------------------------------

raster <- rast(
  "brazil_coverage-col11_2019.tif"
)

#------------------------------------------------
# Checking projection
#------------------------------------------------

points_raster <- st_transform(
  points,
  crs(raster)
)

# transformar sf em SpatVector
points_vect <- vect(points_raster)

#------------------------------------------------
# Crop raster to the area of the points
#------------------------------------------------

# extensão dos pontos
ext_points <- ext(points_vect)

# adicionar uma margem
ext_points_buffer <- extend(ext_points, 0.5)

# recortar o raster
raster_crop <- crop(raster, ext_points_buffer)

#------------------------------------------------
# Visualizing map and points
#------------------------------------------------

plot(raster_crop)

plot(
  points_vect,
  add = TRUE,
  pch = 19
)

#------------------------------------------------
# Extracting land cover values
#------------------------------------------------

values <- extract(
  raster,
  points_vect
)

#------------------------------------------------
# Adding values to the table
#------------------------------------------------

final_data_phase1 <- cbind(
  data_phase1,
  Pixel_value = values[, 2]
)
head(final_data_phase1)

## verifyng values
unique(final_data_phase1$Pixel_value)

## Adding the pixel value to land cover
legend <- read.csv(
  "legend_code_mapbiomas_brazil_collection_11.csv"
)

head(legend)

final_data_phase1 <- final_data_phase1 %>%
  left_join(
    legend %>% select(class_id, class_name_en),
    by = c("Pixel_value" = "class_id")
  ) %>%
  rename(
    Land_cover = class_name_en
  )

head(final_data_phase1)



############# Land LTER Phase 2

# Subset Phase 2
data_phase2 <- subset(
  data,
  Project_phase == "2"
)

#------------------------------------------------
# Spatializing points of biological data
#------------------------------------------------

points <- st_as_sf(
  data_phase2,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

#------------------------------------------------
# Opening Mapbiomas map year 2023 - collection 11 
#https://brasil.mapbiomas.org/downloads/mapas-para-download-geotiff/
#------------------------------------------------

raster <- rast(
  "brazil_coverage-col11_2023.tif"
)

#------------------------------------------------
# Checking projection
#------------------------------------------------

points_raster <- st_transform(
  points,
  crs(raster)
)

# transformar sf em SpatVector
points_vect <- vect(points_raster)

#------------------------------------------------
# Crop raster to the area of the points
#------------------------------------------------

# extensão dos pontos
ext_points <- ext(points_vect)

# adicionar uma margem
ext_points_buffer <- extend(ext_points, 0.5)

# recortar o raster
raster_crop <- crop(raster, ext_points_buffer)

#------------------------------------------------
# Visualizing map and points
#------------------------------------------------

plot(raster_crop)

plot(
  points_vect,
  add = TRUE,
  pch = 19
)

#------------------------------------------------
# Extracting land cover values
#------------------------------------------------

values <- extract(
  raster,
  points_vect
)

#------------------------------------------------
# Adding values to the table
#------------------------------------------------

final_data_phase2 <- cbind(
  data_phase2,
  Pixel_value = values[, 2]
)
head(final_data_phase2)

## verifyng values
unique(final_data_phase2$Pixel_value)

## Adding the pixel value to land cover
legend <- read.csv(
  "legend_code_mapbiomas_brazil_collection_11.csv"
)

head(legend)

final_data_phase2 <- final_data_phase2 %>%
  left_join(
    legend %>% select(class_id, class_name_en),
    by = c("Pixel_value" = "class_id")
  ) %>%
  rename(
    Land_cover = class_name_en
  )

head(final_data_phase2)

##### Final table

final_data <- bind_rows(
  final_data_phase1,
  final_data_phase2
)

write.csv(
  final_data,
  "Total_Points_LAND_LTER_LandCover.csv",
  row.names = FALSE
)
