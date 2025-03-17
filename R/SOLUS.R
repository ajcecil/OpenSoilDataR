#' Fetch SOLUS100 Soil Data
#'
#' Downloads and processes soil property data from SOLUS100.
#'
#' @param aoi SpatRaster or SpatVector. Area of interest.
#' @param properties Character vector. List of soil properties to download.
#' @param depths Character vector. Soil depth intervals to download.
#' @param measures Character vector. Measures to download ("l", "h", "p", "rpi").
#' @param output_dir Character. Directory where files will be saved.
#' @param suffix Character. Optional suffix for filenames.
#' @return Saves downloaded and clipped rasters.
#' @import terra httr utils
#' @export
#' @examples
#' \dontrun{
#' # Define AOI and download SOLUS100 data
#' aoi <- rast("path_to_aoi.tif")
#' output_directory <- "path/to/your/directory"
#' fetch_SOL(
#'   aoi = aoi,
#'   properties = c("claytotal", "soc"),
#'   depths = c("0_cm", "30_cm"),
#'   measures = c("p", "l", "h"), #rpi is data heavy
#'   output_dir = "output_directory",
#'   suffix = "test"
#' )
#' }
#'
# Function to download and process SOLUS100 data
fetch_SOL <- function(aoi, properties, depths, measures, output_dir, suffix = "") {
  base_url <- "https://storage.googleapis.com/solus100pub/"

  valid_properties <- c(
    "anylithicdpt", "caco3", "cec7", "claytotal", "dbovendry", "ec", "ecec", "fragvol",
    "gypsum", "ph1to1h2o", "resdept", "sandco", "sandfine", "sandmed", "sandtotal",
    "sandvc", "sandvf", "sar", "silttotal", "soc"
  )

  valid_depths <- c("0_cm", "5_cm", "15_cm", "30_cm", "60_cm", "100_cm", "150_cm")
  valid_measures <- c("l", "h", "p", "rpi")

  if (!all(properties %in% valid_properties)) stop("Invalid property selected.")
  if (!all(depths %in% valid_depths)) stop("Invalid depth selected.")
  if (!all(measures %in% valid_measures)) stop("Invalid measure selected.")

  if (is.character(aoi) && grepl("\\.shp$", aoi)) {
    aoi <- vect(aoi)
  }

  if (!inherits(aoi, "SpatRaster") && !inherits(aoi, "SpatVector")) {
    stop("AOI must be a SpatRaster, SpatVector, or a shapefile path.")
  }

  for (prop in properties) {
    for (depth in depths) {
      for (measure in measures) {
        filename <- paste0(prop, "_", depth, "_", measure, ".tif")
        file_url <- paste0(base_url, filename)
        suffix_str <- ifelse(nchar(suffix) > 0, paste0("_", suffix), "")
        output_file <- file.path(output_dir, paste0(prop, "_", depth, "_", measure, suffix_str, ".tif"))

        # **Check if the file exists on the server before downloading**
        if (!file_exists_on_server(file_url)) {
          message("File does not exist on server: ", file_url, " Skipping...")
          next
        }

        # **Try downloading with timeout & fallback**
        download_with_fallback(file_url, output_file, timeout = 55)

        raster <- rast(output_file)

        # **Reproject AOI to Match Raster CRS**
        raster_crs <- crs(raster)
        if (crs(aoi) != raster_crs) {
          message("Reprojecting AOI to match raster CRS: ", raster_crs)
          aoi <- project(aoi, raster_crs)
        }

        # Crop the raster to AOI
        cropped_raster <- crop(raster, aoi)
        writeRaster(cropped_raster, output_file, overwrite = TRUE)
        message("Processed and saved: ", output_file)
      }
    }
  }
}


