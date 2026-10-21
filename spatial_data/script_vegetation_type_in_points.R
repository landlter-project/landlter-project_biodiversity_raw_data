
# Install dependencies once if needed:
# install.packages(c("here", "readxl", "sf", "terra", "dplyr"))

library(readxl)
library(sf)
library(terra)
library(dplyr)

here::i_am("spatial_data/script_vegetation_type_in_points.R")
spatial_folder <- here::here("spatial_data")
dir.create(spatial_folder, recursive = TRUE, showWarnings = FALSE)

# Source of the original MapBiomas Collection 11 maps:
# https://brasil.mapbiomas.org/downloads/mapas-para-download-geotiff/
# The LAND LTER raster subsets used here are distributed with this project:
# https://github.com/landlter-project/landlter-project_biodiversity_raw_data/tree/main/spatial_data
base_url <- paste0(
  "https://raw.githubusercontent.com/landlter-project/",
  "landlter-project_biodiversity_raw_data/main/spatial_data/"
)

download_if_missing <- function(filename) {
  destination <- here::here("spatial_data", filename)
  src_url <- paste0(base_url, filename)
  if (!file.exists(destination)) {
    message("Downloading ", filename, " from ", src_url)
    temporary <- tempfile(tmpdir = spatial_folder)
    on.exit(unlink(temporary), add = TRUE)
    tryCatch({
      status <- utils::download.file(src_url, temporary, mode = "wb")
      if (status != 0L || file.info(temporary)$size == 0) {
        stop("Download failed or returned an empty file.")
      }
      if (!file.rename(temporary, destination)) {
        stop("Could not save the downloaded file.")
      }
    }, error = function(e) {
      stop("Could not download ", src_url, ": ", conditionMessage(e),
           call. = FALSE)
    })
  }
  destination
}

phase1_file <- download_if_missing("MapBiomas_col11_2019_LANDLTER_Phase1.tif")
phase2_file <- download_if_missing("MapBiomas_col11_2023_LANDLTER_Phase2.tif")
data_file <- download_if_missing("summary_LAND_LTER.xlsx")
legend_file <- download_if_missing("legend_code_mapbiomas_brazil_collection_11.csv")

data <- readxl::read_excel(data_file)
legend <- read.csv(legend_file, stringsAsFactors = FALSE,
                   encoding = "UTF-8")
stopifnot(all(c("Project_phase", "Longitude", "Latitude") %in% names(data)))
stopifnot(all(c("class_id", "class_name_en") %in% names(legend)))
if (anyDuplicated(legend$class_id)) {
  stop("The MapBiomas legend must have only one row per class_id.")
}

extract_phase <- function(phase, raster_file, year) {
  phase_data <- data[!is.na(data$Project_phase) &
                       data$Project_phase == phase, , drop = FALSE]
  if (nrow(phase_data) == 0L) stop("No records for phase ", phase)
  coordinates <- lapply(phase_data[c("Longitude", "Latitude")],
                        function(x) suppressWarnings(as.numeric(x)))
  if (any(!is.finite(coordinates$Longitude)) ||
      any(!is.finite(coordinates$Latitude))) {
    stop("Missing or invalid coordinates in phase ", phase)
  }
  phase_data$Longitude <- coordinates$Longitude
  phase_data$Latitude <- coordinates$Latitude
  points <- sf::st_as_sf(phase_data, coords = c("Longitude", "Latitude"),
                         crs = 4326, remove = FALSE)
  raster <- terra::rast(raster_file)
  if (terra::nlyr(raster) != 1L) stop("Expected a single land-cover layer.")
  points_raster <- sf::st_transform(points, terra::crs(raster))
  points_vect <- terra::vect(points_raster)

  plot(raster, main = paste("Land LTER phase", phase, "- MapBiomas", year))
  plot(points_vect, add = TRUE, pch = 19)

  values <- terra::extract(raster, points_vect)
  phase_data$Pixel_value <- values[[2]]
  result <- dplyr::left_join(
    phase_data,
    dplyr::select(legend, class_id, class_name_en),
    by = c("Pixel_value" = "class_id")
  )
  result <- dplyr::rename(result, Land_cover = class_name_en)
  message("Phase ", phase, ": ", nrow(result), " records; ",
          sum(is.na(result$Pixel_value)), " without a raster value; ",
          sum(!is.na(result$Pixel_value) & is.na(result$Land_cover)),
          " values absent from the legend.")
  result
}

final_data_phase1 <- extract_phase("1", phase1_file, 2019)
final_data_phase2 <- extract_phase("2", phase2_file, 2023)
final_data <- dplyr::bind_rows(final_data_phase1, final_data_phase2)

output_file <- here::here("spatial_data", "Total_Points_LAND_LTER_LandCover.csv")
write.csv(final_data, output_file, row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: ", output_file)

