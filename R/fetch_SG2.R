#' Fetch SoilGrids v2 Data
#'
#' Downloads and processes soil property data from SoilGrids v2, loading rasters into memory
#' while optionally saving them to disk.
#'
#' @param aoi SpatRaster, SpatVector, or file path to a shapefile. The area of interest.
#' @param properties Character vector. List of soil properties to download (e.g., `"clay"`, `"soc"`, `"phh2o"`).
#' @param depths Character vector. Soil depth intervals to download (e.g., `"0-5cm"`, `"5-15cm"`).
#' @param output_dir Character. Directory where files will be saved. Set to NULL to keep only in memory.
#' @param suffix Character. Optional suffix for file names.
#' @param crs Character. Coordinate reference system for the output. Default is `"EPSG:4326"`.
#' @param scale Numeric. Resolution in meters. Default is `250` (meters).
#' @param export Logical. If `TRUE`, saves downloaded rasters to `output_dir`. Default is `TRUE`.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{stack}}{A `SpatRaster` containing all downloaded layers.}
#'   \item{\code{file_paths}}{A character vector of file paths (if `export = TRUE`).}
#'   \item{\code{product}}{A character string `"SG2"` identifying the dataset.}
#' }
#'
#' @details
#' This function interfaces with the SoilGrids v2 dataset hosted on Google Earth Engine (GEE)
#' and allows users to extract soil property maps for a given area of interest (AOI).
#' Users can specify depth intervals and properties of interest, and optionally save the data
#' to a specified directory.
#'
#' @import terra rgeedim
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate and initialize Google Earth Engine
#' gd_authenticate(auth_mode = "notebook")
#' gd_initialize()
#'
#' # Define AOI and fetch SG2 data
#' output_directory <- "path/to/your/directory"
#' aoi_raster <- rast("path_to_aoi.tif")
#'
#' # Fetch SoilGrids data (without SOC conversion)
#' sg2_data <- fetch_SG2(
#'   aoi = aoi_raster,
#'   properties = c("phh2o", "soc"),
#'   depths = c("0-5cm", "5-15cm"),
#'   output_dir = output_directory,
#'   suffix = "example",
#'   crs = "EPSG:4326",
#'   scale = 250,
#'   export = TRUE,
#'   soctoperc = FALSE  # Keep SOC as g/kg
#' )
#'
#' # Fetch SoilGrids data with SOC converted to percentage
#' sg2_data_perc <- fetch_SG2(
#'   aoi = aoi_raster,
#'   properties = c("soc"),
#'   depths = c("0-5cm", "30-60cm"),
#'   output_dir = output_directory,
#'   suffix = "perc",
#'   crs = "EPSG:4326",
#'   scale = 250,
#'   export = TRUE,
#'   soctoperc = TRUE  # Convert SOC from g/kg to %
#' )
#'
#' # Access the loaded SpatRaster
#' plot(sg2_data$stack)
#' }


fetch_SG2 <- function(aoi, properties, depths,
                      output_dir = NULL, suffix = "",
                      crs = "EPSG:4326", scale = 250, export = TRUE) {

  require(terra)
  require(rgeedim)

  # Define the base path for SoilGrids data on Google Earth Engine
  soilgrids_base <- "projects/soilgrids-isric"

  # List of available depth intervals in SoilGrids
  depth_intervals <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")

  # List of available soil properties in SoilGrids
  soilgrids_properties <- c(
    "bdod", "cec", "cfvo", "clay", "nitrogen",
    "ocd", "ocs", "phh2o", "sand", "silt", "soc"
  )

  # Validate input properties
  invalid_properties <- setdiff(properties, soilgrids_properties)
  if (length(invalid_properties) > 0) {
    stop(paste("Invalid property selected:", paste(invalid_properties, collapse = ", ")))
  }

  # Validate input depths
  selected_depths <- intersect(depths, depth_intervals)
  if (length(selected_depths) == 0) stop("Invalid depth(s) selected.")

  # Load AOI if it is a shapefile
  if (is.character(aoi) && grepl("\\.shp$", aoi)) {
    aoi <- vect(aoi)  # Load as SpatVector
  }

  # Ensure AOI is either a SpatRaster or SpatVector and reproject to WGS84 if necessary
  if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    if (crs(aoi) != "EPSG:4326") {
      aoi <- project(aoi, "EPSG:4326")
    }
    bbox <- ext(aoi)  # Extract bounding box
  } else {
    stop("AOI must be a SpatRaster, SpatVector, or a shapefile path.")
  }

  # Convert bounding box to Google Earth Engine format
  bbox_gee <- gd_bbox(
    xmin = bbox$xmin,
    xmax = bbox$xmax,
    ymin = bbox$ymin,
    ymax = bbox$ymax
  )

  raster_list <- list()
  file_paths <- list()

  # Define conversion factors for each soil property
  conversion_factors <- list(
    "bdod" = 100,    # Bulk density: cg/cm³ → kg/dm³
    "cec" = 10,      # CEC: mmol(c)/kg → cmol(c)/kg
    "cfvo" = 10,     # Coarse fragments: cm³/dm³ → cm³/100cm³
    "clay" = 10,     # Clay: g/kg → g/100g (%)
    "nitrogen" = 100,# Total nitrogen: cg/kg → g/kg
    "phh2o" = 10,    # pH: pHx10 → pH
    "sand" = 10,     # Sand: g/kg → g/100g (%)
    "silt" = 10,     # Silt: g/kg → g/100g (%)
    "soc" = 100,     # SOC: dg/kg → g/100g (%)
    "ocd" = 10,      # Organic carbon density: hg/dm³ → kg/dm³
    "ocs" = 10       # Organic carbon stocks: t/ha → kg/m²
  )

  for (prop in properties) {
    # Construct the correct data path for the property
    soilgrids_path <- paste0(soilgrids_base, "/", prop, "_mean")

    for (depth in selected_depths) {
      band_name <- paste0(prop, "_", depth, "_mean")

      # Define output file path
      output_file <- if (!is.null(output_dir) && export) {
        file.path(output_dir, paste0("SG2_", band_name, suffix, ".tif"))
      } else {
        tempfile(fileext = ".tif")
      }

      # Download the raster band
      full_raster <- gd_image_from_id(soilgrids_path) |>
        gd_download(
          filename = output_file,
          region = bbox_gee,
          scale = scale,  # SoilGrids resolution is 250m
          crs = crs,
          resampling = "near",
          overwrite = TRUE,
          silent = FALSE
        )

      # Load raster into R
      raster <- rast(output_file)

      # Check if the expected band exists in the raster
      if (!(band_name %in% names(raster))) {
        message("Band ", band_name, " not found in ", output_file, ". Skipping...")
        next
      }

      depth_raster <- raster[[band_name]]

      # Apply conversion factor if applicable
      if (prop %in% names(conversion_factors)) {
        depth_raster <- depth_raster / conversion_factors[[prop]]
      }

      # Issue a warning if SOC is being converted
      if (prop == "soc") {
        message("⚠️  Soil Organic Carbon (SOC) has been converted from g/kg to % (g/100g).")
      }

      # Store file path if exporting
      if (export) file_paths <- append(file_paths, output_file)

      # Save raster to a temporary file before renaming
      temp_file <- paste0(output_file, "_tmp.tif")
      writeRaster(depth_raster, temp_file, overwrite = TRUE)
      file.rename(temp_file, output_file)

      # Store raster in the list
      raster_list[[paste0(prop, "_", depth)]] <- rast(output_file)
    }
  }

  # Ensure at least one raster was downloaded
  if (length(raster_list) == 0) {
    stop("No rasters were successfully downloaded.")
  }

  # Combine downloaded rasters into a single SpatRaster stack
  raster_stack <- rast()
  for (r in raster_list) {
    raster_stack <- c(raster_stack, r)
  }

  # Ensure the final raster stack is valid
  if (nlyr(raster_stack) == 0) {
    stop("Failed to create a valid SpatRaster stack.")
  }

  # Assign "SG2" as an attribute to the SpatRaster
  attr(raster_stack, "product") <- "SG2"

  # Return a structured output
  return(list(stack = raster_stack, file_paths = file_paths, product = "SG2"))
}
