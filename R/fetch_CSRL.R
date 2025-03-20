#' Download and Process CSRL Soil Property Data
#'
#' Downloads specified soil property data from the **CSRL Soil Properties Dataset** on Google Earth Engine (GEE),
#' loads the rasters into memory, and optionally saves them to disk.
#'
#' **⚠️ Note:** This dataset is intended for **statewide and regional soil analysis**.  
#' For **local-scale, fine-resolution soil data**, refer to **SSURGO (Soil Survey Geographic Database)**.
#'
#' @param aoi SpatRaster, SpatVector, or file path to a shapefile. The area of interest.
#' @param properties Character vector. List of soil properties to download (see **Lookup Table** below).
#' @param depths Character vector. Depth intervals to download (e.g., `"0-5"`, `"0-25"`, `"30-60"`).  
#'   Required only for `_profile` properties.
#' @param output_dir Character. Directory where files will be saved. Set to `NULL` to keep only in memory.
#' @param suffix Character. Optional suffix for file names.
#' @param crs Character. Coordinate reference system for the output. Default is `"EPSG:4326"`.
#' @param scale Numeric. Resolution in meters. Default is `800` (coarse for regional analysis).
#' @param tosoc Logical. If `TRUE`, converts Organic Matter (`som` or `som_max`) 
#' to Soil Organic Carbon (SOC) using the Van Bemmelen factor (1.724).
#' @param export Logical. If `TRUE`, saves downloaded rasters to `output_dir`. Default is `TRUE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{stack}}{A `SpatRaster` containing all downloaded layers.}
#'   \item{\code{file_paths}}{A character vector of file paths (if `export = TRUE`).}
#'   \item{\code{product}}{A character string `"CSRL"` identifying the dataset.}
#' }
#'
#' @details
#' This function accesses the **CSRL Soil Properties Dataset** in **Google Earth Engine (GEE)**,  
#' providing **bulk density, cation exchange capacity (CEC), sand/silt/clay fractions, soil organic matter (SOM),  
#' saturated hydraulic conductivity (Ksat), pH, and other soil properties**.  
#'
#' **Important:** `_profile` datasets contain multi-depth image collections.  
#' You must specify a depth for these properties (e.g., `"0-25"`, `"30-60"`).
#'
#' ## CSRL Soil Property Lookup Table
#'
#' | Category   | Property Name         | Uses Depths?       |
#' |------------|----------------------|--------------------|
#' | **Chemical** | `"caco3"`             | No                |
#' |            | `"cec"`                | No                |
#' |            | `"cec_profile"`        | Yes (`0-5`, `0-25`, `0-50`) |
#' |            | `"ec"`                 | No                |
#' |            | `"ph_profile"`         | Yes (`0-5`, `0-25`, `25-50`, `30-60`) |
#' |            | `"sar"`                | No                |
#' |            | `"som"`                | No                |
#' | **Physical** | `"bulk_density"`      | No                |
#' |            | `"clay_profile"`       | Yes (`0-5`, `0-25`, `25-50`, `30-60`) |
#' |            | `"ksat_max"`           | No                |
#' |            | `"sand_profile"`       | Yes (`0-5`, `0-25`, `25-50`, `30-60`) |
#' |            | `"silt_profile"`       | Yes (`0-5`, `0-25`, `25-50`, `30-60`) |
#' | **Land Use** | `"soil_order"`        | No                |
#' |            | `"soil_temp_regime"`   | No                |
#' |            | `"wind_erodibility_group"` | No           |
#'
#' @import terra sf dplyr tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(terra)
#' library(sf)
#' # Authenticate and initialize Google Earth Engine
#' gd_authenticate(auth_mode = "notebook")
#' gd_initialize()
#'
#' # Define AOI and output directory
#' output_directory <- "path/to/output"
#' aoi_raster <- rast("path_to_aoi.tif")
#'
#' # Fetch CSRL soil data (Profile & Non-Profile Properties)
#' csrl_data <- fetch_CSRL(
#'   aoi = aoi_raster,
#'   properties = c("sand_profile", "caco3", "ksat_max"),
#'   depths = c("0-25", "30-60"),  # Required only for profile datasets
#'   output_dir = output_directory,
#'   scale = 800,  # Default - for regional analysis
#'   suffix = "",
#'   export = TRUE
#' )
#'
#' # View downloaded raster stack
#' plot(csrl_data$stack)
#' }

fetch_CSRL <- function(aoi, properties, depths = NULL,
                       output_dir = NULL, suffix = "",
                       crs = "EPSG:4326", scale = 800, tosoc = FALSE, export = TRUE) {
  
  # Define base paths for CSRL datasets
  csrl_base <- "projects/earthengine-legacy/assets/projects/sat-io/open-datasets/CSRL_soil_properties"
  
  csrl_paths <- list(
    "chemical" = paste0(csrl_base, "/chemical"),
    "physical" = paste0(csrl_base, "/physical"),
    "land_use" = paste0(csrl_base, "/land_use")
  )
  
  # CSRL property lookup table (ALL PROPERTIES)
  csrl_properties <- list(
    # CHEMICAL
    "caco3" = list("path" = paste0(csrl_paths$chemical, "/caco3"), "is_profile" = FALSE),
    "cec" = list("path" = paste0(csrl_paths$chemical, "/cec"), "is_profile" = FALSE),
    "cec_profile" = list("path" = paste0(csrl_paths$chemical, "/cec_profile"), "is_profile" = TRUE,
                         "depths" = c("0-5", "0-25", "0-50"), "prefix" = "cec_"),
    "ec" = list("path" = paste0(csrl_paths$chemical, "/ec"), "is_profile" = FALSE),
    "ec_profile" = list("path" = paste0(csrl_paths$chemical, "/ec_profile"), "is_profile" = TRUE,
                        "depths" = c("0-5", "0-25"), "prefix" = "ec_"),
    "ph" = list("path" = paste0(csrl_paths$chemical, "/ph"), "is_profile" = FALSE),
    "ph_profile" = list("path" = paste0(csrl_paths$chemical, "/ph_profile"), "is_profile" = TRUE,
                        "depths" = c("0-5", "0-25", "25-50", "30-60"), "prefix" = "ph_"),
    "sar" = list("path" = paste0(csrl_paths$chemical, "/sar"), "is_profile" = FALSE),
    "som" = list("path" = paste0(csrl_paths$chemical, "/som"), "is_profile" = FALSE),
    "som_max" = list("path" = paste0(csrl_paths$chemical, "/som_max"), "is_profile" = FALSE),
    
    # PHYSICAL
    "bulk_density" = list("path" = paste0(csrl_paths$physical, "/bulk_density"), "is_profile" = FALSE),
    "clay" = list("path" = paste0(csrl_paths$physical, "/clay"), "is_profile" = FALSE),
    "clay_profile" = list("path" = paste0(csrl_paths$physical, "/clay_profile"), "is_profile" = TRUE,
                          "depths" = c("0-5", "0-25", "25-50", "30-60"), "prefix" = "clay_"),
    "drainage_class" = list("path" = paste0(csrl_paths$physical, "/drainage_class"), "is_profile" = FALSE),
    "ksat_mean" = list("path" = paste0(csrl_paths$physical, "/ksat_mean"), "is_profile" = FALSE),
    "ksat_min" = list("path" = paste0(csrl_paths$physical, "/ksat_min"), "is_profile" = FALSE),
    "ksat_max" = list("path" = paste0(csrl_paths$physical, "/ksat_max"), "is_profile" = FALSE),
    "rf_025" = list("path" = paste0(csrl_paths$physical, "/rf_025"), "is_profile" = FALSE),
    "sand" = list("path" = paste0(csrl_paths$physical, "/sand"), "is_profile" = FALSE),
    "sand_profile" = list("path" = paste0(csrl_paths$physical, "/sand_profile"), "is_profile" = TRUE,
                          "depths" = c("0-5", "0-25", "25-50", "30-60"), "prefix" = "sand_"),
    "silt" = list("path" = paste0(csrl_paths$physical, "/silt"), "is_profile" = FALSE),
    "silt_profile" = list("path" = paste0(csrl_paths$physical, "/silt_profile"), "is_profile" = TRUE,
                          "depths" = c("0-5", "0-25", "25-50", "30-60"), "prefix" = "silt_"),
    "soil_texture_profile" = list("path" = paste0(csrl_paths$physical, "/soil_texture_profile"), "is_profile" = TRUE,
                                  "depths" = c("0-5", "0-25", "25-50"), "prefix" = "soil_texture_"),
    "water_storage" = list("path" = paste0(csrl_paths$physical, "/water_storage"), "is_profile" = FALSE),
    "water_storage_profile" = list("path" = paste0(csrl_paths$physical, "/water_storage_profile"), "is_profile" = TRUE,
                                   "depths" = c("0-25", "0-50"), "prefix" = "water_storage_"),
    
    #LAND USE
    "hydrologic_group" = list("path" = paste0(csrl_paths$land_use, "/hydrologic_group"), "is_profile" = FALSE),
    "kw_025" = list("path" = paste0(csrl_paths$land_use, "/kw_025"), "is_profile" = FALSE),
    "lcc_i" = list("path" = paste0(csrl_paths$land_use, "/lcc_i"), "is_profile" = FALSE),
    "lcc_ni" = list("path" = paste0(csrl_paths$land_use, "/lcc_ni"), "is_profile" = FALSE),
    "resdept" = list("path" = paste0(csrl_paths$land_use, "/resdept"), "is_profile" = FALSE),
    "soil_depth" = list("path" = paste0(csrl_paths$land_use, "/soil_depth"), "is_profile" = FALSE),
    "soil_order" = list("path" = paste0(csrl_paths$land_use, "/soil_order"), "is_profile" = FALSE),
    "soil_temp_regime" = list("path" = paste0(csrl_paths$land_use, "/soil_temp_regime"), "is_profile" = FALSE),
    "survey_type" = list("path" = paste0(csrl_paths$land_use, "/survey_type"), "is_profile" = FALSE),
    "wind_erodibility_group" = list("path" = paste0(csrl_paths$land_use, "/wind_erodibility_group"), "is_profile" = FALSE),
    "wind_erodibility_index" = list("path" = paste0(csrl_paths$land_use, "/wind_erodibility_index"), "is_profile" = FALSE)
  )
  
  # Verify Valid Properties*
  invalid_properties <- setdiff(properties, names(csrl_properties))
  if (length(invalid_properties) > 0) {
    stop("Invalid properties: ", paste(invalid_properties, collapse = ", "))
  }
  
  # Ensure 'som' and 'som_max' are valid inputs for SOC conversion
  if (tosoc && !any(properties %in% c("som", "som_max"))) {
    stop("`tosoc = TRUE` requires either 'som' or 'som_max' in `properties`.")
  }
  
  # Load AOI if it's a shapefile
  if (is.character(aoi) && grepl("\\.shp$", aoi)) {
    aoi <- vect(aoi)
  }
  
  # Ensure AOI is either SpatRaster or SpatVector and reproject to WGS84
  if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
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
  
  raster_list <- list()
  file_paths <- list()
  
  for (prop in properties) {
    prop_info <- csrl_properties[[prop]]
    
    # Define selected depths correctly
    if (prop %in% c("som", "som_max")) {
      selected_depths <- c("0-25")
    } else if (prop_info$is_profile) {
      if (is.null(depths)) stop("Depth must be specified for profile property: ", prop)
      selected_depths <- intersect(depths, prop_info$depths)
      if (length(selected_depths) == 0) {
        stop("Invalid depth(s) for property: ", prop, ". Available: ", paste(prop_info$depths, collapse = ", "))
      }
    } else {
      selected_depths <- list(NULL)
    }
    
    for (depth in selected_depths) {
      dataset_path <- if (prop_info$is_profile) {
        paste0(prop_info$path, "/", prop_info$prefix, gsub("-", "", depth))
      } else {
        prop_info$path
      }
      
      depth_suffix <- if (!is.null(depth)) paste0("_", depth) else ""
      
      output_file <- file.path(output_dir, paste0("CSRL_", prop, depth_suffix, suffix, ".tif"))
      final_band_name <- paste0(prop, depth_suffix)
      
      if (tosoc && prop == "som_max") {
        output_file <- file.path(output_dir, paste0("CSRL_soc_max", depth_suffix, suffix, ".tif"))
        final_band_name <- paste0("soc_max", depth_suffix)
      } else if (tosoc && prop == "som") {
        output_file <- file.path(output_dir, paste0("CSRL_soc_stock", depth_suffix, suffix, ".tif"))
        final_band_name <- paste0("soc_stock", depth_suffix)
      } else if (prop == "som") {
        output_file <- file.path(output_dir, paste0("CSRL_som_stock", depth_suffix, suffix, ".tif"))  # Rename SOM to SOM_STOCK
        final_band_name <- paste0("som_stock", depth_suffix)
      } else if (prop == "som_max") {
        output_file <- file.path(output_dir, paste0("CSRL_som_max", depth_suffix, suffix, ".tif"))  
      }
      
      # Download the raster
      img <- gd_image_from_id(dataset_path)
      gd_download(
        img, filename = output_file, region = bbox_gee,
        scale = scale, crs = crs, resampling = "near",
        overwrite = TRUE, silent = FALSE
      )
      
      # Load raster and remove extra bands
      full_raster <- rast(output_file)
      
      # Select the correct band (avoid "b1" or "FILL_MASK")
      clean_raster <- full_raster[[1]]  # Take only the first band (assuming it's correct)
      
      
      if (prop == "som_max") {
        clean_raster <- clean_raster * 100  # Always convert to percentage
        message("Converted SOM_MAX to (%) by weight.")
        
        if (tosoc) {
          clean_raster <- clean_raster * 0.5807 * 100  # Apply Van Bemmelen conversion only if tosoc = TRUE
          message("Converted SOM_MAX to SOC_MAX (%) using Van Bemmelen factor (1.724).")
        }
        
        message("Depth interval 0-25 is made up.")
        
      } else if (prop == "som") {
        if (tosoc) {
          clean_raster <- clean_raster * 0.5807  # Apply Van Bemmelen conversion only when tosoc = TRUE
          message("Converted SOM to SOC_STOCK (kg/m²) using Van Bemmelen factor (1.724).")
        } else {
          message("SOM is in stock form (kg/m²) and remains unchanged.")
        }
        
        message("Depth interval 0-25 is made up.")
      }
      
      
      
      names(clean_raster) <- final_band_name
      
      temp_file <- paste0(output_file, "_tmp.tif")
      writeRaster(clean_raster, temp_file, overwrite = TRUE)
      file.rename(temp_file, output_file)
      
      raster_list[[final_band_name]] <- rast(output_file)
      file_paths <- append(file_paths, output_file)
      
      message("Downloaded and cleaned: ", final_band_name)
    }
  }
  
  # Ensure at least one raster was downloaded
  if (length(raster_list) > 0) {
    raster_stack <- do.call(c, raster_list)
  } else {
    stop("No valid rasters were found to stack.")
  }
  
  
  # Stack the rasters
  raster_stack <- rast()
  for (r in raster_list) {
    raster_stack <- c(raster_stack, r)
  }
  
  # Assign correct band names
  names(raster_stack) <- names(raster_list)
  
  if (nlyr(raster_stack) == 0) stop("Failed to create a valid SpatRaster stack.")
  
  # Assign "CSRL" as an attribute
  attr(raster_stack, "product") <- "CSRL"
  
  return(list(stack = raster_stack, file_paths = file_paths, product = "CSRL"))
}

