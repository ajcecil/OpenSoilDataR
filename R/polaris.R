#' Download and Process POLARIS Soil Data
#'
#' This function downloads specified POLARIS soil property data, clips the raster,
#' and adjusts projections if necessary.
#'
#' @param aoi SpatRaster or SpatVector AOI.
#' @param properties Character vector of soil properties to download.
#' @param depths Character vector of depth intervals to download.
#' @param output_dir Directory to save the downloaded files.
#' @param suffix Optional suffix for file names.
#' @return Saves downloaded and clipped rasters.
#' @import terra rgeedim
#' @export
#' @note This function requires the `rgeedim` package. If you haven't installed it, run:
#' \code{install.packages("rgeedim")}
#' @examples
#' \dontrun{
#' # Install rgeedim first if not installed
#' install.packages("rgeedim")# Authenticate and initialize Google Earth Engine
#' gd_authenticate(auth_mode = "notebook") # Enter your credentials (not with ISU email)
#' gd_initialize()
#'
#' # Define AOI and download PSP data
#' output_directory <- "path/to/your/directory"
#' aoi_raster <- rast("path_to_aoi.tif")
#'
#' fetch_PSP(
#' aoi = aoi_raster,
#' properties = c("bd_mean", "clay_mean"),
#' depths = c("0_5", "30_60"),
#' output_dir = output_directory,
#' suffix = "kit",
#' crs = "EPSG:4326",
#' scale = 30)
#' }


# Function to extract POLARIS data
fetch_PSP <- function(aoi, properties,
                      depths, output_dir,
                      suffix = "", crs = "EPSG:4326",
                      scale = 30) {
  # Load AOI if it's a shapefile
  if (is.character(aoi) && grepl("\\.shp$", aoi)) {
    aoi <- vect(aoi)  # Load shapefile as SpatVector
  }

  # Ensure AOI is in SpatVector or SpatRaster format
  if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    # Convert AOI to WGS84 if needed
    if (crs(aoi) != "EPSG:4326") {
      aoi <- project(aoi, "EPSG:4326")
    }
    bbox <- ext(aoi)  # Extract bounding box
  } else {
    stop("AOI must be a SpatRaster, SpatVector, or a shapefile path.")
  }

  # Convert bounding box to GEE format
  bbox_gee <- gd_bbox(
    xmin = bbox$xmin,
    xmax = bbox$xmax,
    ymin = bbox$ymin,
    ymax = bbox$ymax
  )

  # POLARIS base path
  polaris_base <- "projects/sat-io/open-datasets/polaris"

  # Available depth intervals
  depth_intervals <- c("0_5", "5_15", "15_30", "30_60", "60_100", "100_200")

  # Available POLARIS soil properties
  polaris_properties <- c(
    "bd_mean", "clay_mean", "ksat_mean", "n_mean", "om_mean", "ph_mean",
    "sand_mean", "silt_mean", "theta_r_mean", "theta_s_mean", "lambda_mean",
    "hb_mean", "alpha_mean"
  )

  # Validate input properties
  invalid_properties <- setdiff(properties, polaris_properties)
  if (length(invalid_properties) > 0) {
    stop(paste("Invalid property selected:", paste(invalid_properties, collapse = ", ")))
  }

  # Validate input depths
  selected_depths <- intersect(depths, depth_intervals)
  if (length(selected_depths) == 0) stop("Invalid depth(s) selected.")

  # Loop through properties and depths
  for (prop in properties) {
    # Set the **correct** POLARIS path per property
    polaris_path <- paste0(polaris_base, "/", prop)

    for (depth in selected_depths) {
      # Use ONLY the depth ID (e.g., "bd_0_5", "clay_5_15")
      depth_id <- paste0(prop, "_", depth)
      depth_id <- gsub("_mean", "", depth_id)  # Remove `_mean` from ID

      # Append suffix if provided
      suffix_str <- ifelse(nchar(suffix) > 0, paste0("_", suffix), "")

      # Define output file path dynamically
      output_file <- file.path(output_dir, paste0(depth_id, suffix_str, ".tif"))

      # Extract the correct Image ID for the requested depth
      y <- paste0(polaris_path, "/", depth_id) |>  # Get the first matching image ID
        gd_image_from_id() |>
        gd_download(
          filename = output_file,
          region = bbox_gee,
          scale = scale,  # Match POLARIS 100m resolution
          crs = crs,  # Ensure output is in WGS84
          resampling = "near",
          overwrite = TRUE,
          silent = FALSE
        )

      message("Downloaded: ", output_file)
    }
  }
}



