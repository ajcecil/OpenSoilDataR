#' Fetch SoilGrids v2 Data
#'
#' Downloads and processes soil property data from SoilGrids v2.
#'
#' @param aoi SpatRaster or SpatVector. Area of interest.
#' @param properties Character vector. List of soil properties to download.
#' @param depths Character vector. Soil depth intervals to download.
#' @param output_dir Character. Directory where files will be saved.
#' @param suffix Character. Optional suffix for filenames.
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

#' # Define AOI and download SG2 data
#' output_directory <- "path/to/your/directory"
#' aoi_shapefile <- rast("path_to_aoi.shp")
#'
#' fetch_SG2(
#'   aoi = aoi_shapefile,
#'   properties = c("phh2o", "bdod")
#'   depths = c("0-5cm", "5-15cm",),
#'   output_dir = "output_directory",
#'   suffix = "ex",
#'   crs = "EPSG:4326",
#'   scale = 250)
#' }


fetch_SG2<- function(aoi, properties,
                     depths, output_dir,
                     suffix = "", crs = "EPSG:4326",
                     scale = 250) {

  # SoilGrids base path
  soilgrids_base <- "projects/soilgrids-isric"

  # Available depth intervals
  depth_intervals <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")

  # Available SoilGrids soil properties
  soilgrids_properties <- c(
    "bdod", "cec", "cfvo", "clay", "nitrogen",
    "ocd", "ocs", "phh2o", "sand", "silt", "soc"
  )

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

  # Validate input properties
  invalid_properties <- setdiff(properties, soilgrids_properties)
  if (length(invalid_properties) > 0) {
    stop(paste("Invalid property selected:", paste(invalid_properties, collapse = ", ")))
  }

  # Validate input depths
  selected_depths <- intersect(depths, depth_intervals)
  if (length(selected_depths) == 0) stop("Invalid depth(s) selected.")

  downloaded_files <- c()  # Store file paths for band extraction

  # Loop through properties
  for (prop in properties) {
    # Construct the correct SoilGrids path
    soilgrids_path <- paste0(soilgrids_base, "/", prop, "_mean")

    # Append suffix if provided
    suffix_str <- ifelse(nchar(suffix) > 0, paste0("_", suffix), "")

    # Define the output file for the full raster download
    full_raster_file <- file.path(output_dir, paste0(prop, "_full", suffix_str, ".tif"))

    # Download the full SoilGrids image (all bands)
    y <- gd_image_from_id(soilgrids_path) |>
      gd_download(
        filename = full_raster_file,
        region = bbox_gee,
        scale = scale,  # SoilGrids resolution is 250m
        crs = crs,
        resampling = "near",
        overwrite = TRUE,
        silent = FALSE
      )

    message("Downloaded full raster: ", full_raster_file)

    downloaded_files <- c(downloaded_files, full_raster_file)  # Store for band extraction
  }

  # Extract the correct depth bands after download
  for (file in downloaded_files) {
    prop <- basename(file)
    prop <- gsub("_full.*\\.tif$", "", prop)  # Extract property name from filename

    for (depth in selected_depths) {
      # Construct the correct band name (e.g., "sand_0-5cm_mean")
      band_name <- paste0(prop, "_", depth, "_mean")

      # Define the output file for the extracted depth
      output_file <- file.path(output_dir, paste0(prop, "_", depth, suffix_str, ".tif"))

      # Load the full raster and extract the correct band
      full_raster <- rast(file)
      if (!(band_name %in% names(full_raster))) {
        message("Band ", band_name, " not found in ", file, ". Skipping...")
        next
      }

      # Subset the raster to the correct band and save it
      depth_raster <- full_raster[[band_name]]
      writeRaster(depth_raster, output_file, overwrite = TRUE)

      message("Extracted and saved: ", output_file)
    }
  }
}


