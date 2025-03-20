#' Compute Zonal Statistics for Multiple Soil Products
#'
#' This function calculates zonal statistics for soil property rasters from POLARIS (PSP),
#' SoilGrids v2 (SG2), SOLUS100 (SOL), and CSRL Soil props 800. It ensures that depth intervals from different
#' datasets are consistently translated, and supports depth-weighted averaging.
#'
#' @param soil_data A `SpatRaster` or list of `SpatRaster` objects from `fetch_PSP`, `fetch_SG2`, or `fetch_SOL`.
#' @param tdepth Numeric. Top depth of interest (cm).
#' @param bdepth Numeric. Bottom depth of interest (cm).
#' @param props Character vector. Standardized soil properties (e.g., `"clay"`, `"sand"`, `"bd"`).
#' @param shapes `sf` or `SpatVector`. Polygon boundaries for statistics. **The "Name" field must match plot names.**
#' @param plots Character vector. Names of plots to process (must match `shapes$Name`).
#' @param stats Character vector. Statistics to calculate (`"mean"`, `"min"`, `"max"`, `"sd"`).
#' @param wtd.mean Logical. If `TRUE`, computes depth-weighted mean across specified depth range.
#' @param MakePlot Logical. If `TRUE`, generates raster plots for each statistic.
#' @param output_dir Character. Directory to save output files. Set to `NULL` to disable file saving.
#' 
#' @return A list containing:
#'   - `$Unweighted`: Data frame of zonal statistics per depth interval.
#'   - `$Weighted`: Data frame of depth-weighted means (if `wtd.mean = TRUE`).
#'
#' @section Property Lookup:
#' **Standardized soil properties across datasets.**
#'
#' \preformatted{
#' property_lookup <- list(
#' # Bulk Density
#' "bd" = c(PSP = "bd_mean", SG2 = "bdod", SOL = "dbovendry", CSRL = "bulk_density"),
#' # Soil Texture (Sand, Silt, Clay)
#' "clay" = c(PSP = "clay_mean", SG2 = "clay", SOL = "claytotal", CSRL = "clay_profile"),
#' "sand" = c(PSP = "sand_mean", SG2 = "sand", SOL = "sandtotal", CSRL = "sand_profile"),
#' "silt" = c(PSP = "silt_mean", SG2 = "silt", SOL = "silttotal", CSRL = "silt_profile"),
#' # Soil Organic Matter / Carbon
#' "som" = c(PSP = "om_mean", SG2 = NA, SOL = NA, CSRL = "som_max"),
#' "soc" = c(PSP = "soc", SG2 = "soc", SOL = "soc_max", CSRL = "soc_max"),
#' "soc_stock" = c(PSP = NA, SG2 = "ocs", SOL = NA, CSRL = "soc_stock"),
#' "som_stock" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "som_stock"),
#pH & Chemistry
#' "ph" = c(PSP = "ph_mean", SG2 = "phh2o", SOL = "ph1to1h2o", CSRL = "ph_profile"),
#' "cec" = c(PSP = NA, SG2 = "cec", SOL = "cec7", CSRL = "cec_profile"),
#' "ec" = c(PSP = NA, SG2 = NA, SOL = "ec", CSRL = "ec_profile"),
#' "sar" = c(PSP = NA, SG2 = NA, SOL = "sar", CSRL = "sar"),
#' "caco3" = c(PSP = NA, SG2 = NA, SOL = "caco3", CSRL = "caco3"),
#' "gypsum" = c(PSP = NA, SG2 = NA, SOL = "gypsum", CSRL = NA),
#' # Hydrology & Conductivity
#' "ksat" = c(PSP = "ksat_mean", SG2 = NA, SOL = NA, CSRL = "ksat_max"),
#' "theta_s" = c(PSP = "theta_s_mean", SG2 = NA, SOL = NA, CSRL = NA),
#' "theta_r" = c(PSP = "theta_r_mean", SG2 = NA, SOL = NA, CSRL = NA),
#' # Coarse Fragments & Rock Volume
#' "coarse_fragments" = c(PSP = NA, SG2 = "cfvo", SOL = "fragvol", CSRL = "rf_025"),
# Land Use & Erosion
#' "wind_erodibility_group" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "wind_erodibility_group"),
#' "wind_erodibility_index" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "wind_erodibility_index"),
#' "soil_order" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "soil_order"),
#' "soil_temp_regime" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "soil_temp_regime")
#' }
#'
#' @import terra sf dplyr tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(terra)
#' library(sf)
#'
#' # Load polygon shapefile
#' plot_shapes <- st_read("path/to/plots.shp")
#'
#' # Fetch PSP soil data
#' psp_data <- fetch_PSP(aoi = plot_shapes, properties = c("clay_mean", "sand_mean"), depths = c("0_5", "5_15", "15_30"))
#'
#' # Compute zonal statistics
#' results <- s.zonalstats(
#'   soil_data = psp_data,
#'   tdepth = 0, bdepth = 20,
#'   props = c("clay", "sand"),
#'   shapes = plot_shapes,
#'   plots = c("NW_plot", "SW_plot"),
#'   stats = c("mean", "min", "max", "sd"),
#'   wtd.mean = TRUE,
#'   MakePlot = TRUE,
#'   output_dir = "path/to/output"
#' )
#'
#' # View results
#' head(results$Unweighted)
#' head(results$Weighted)
#' }



s.zonalstats <- function(soil_data, tdepth = 0, bdepth = 20,
                         props = c("sand", "clay"), shapes = plots,
                         plots = c("NW_plot", "SW_plot"), stats = c("mean", "min", "max", "sd"),
                         wtd.mean = TRUE, MakePlot = FALSE, output_dir = NULL) {


  if (MakePlot) {
    cat("You've selected the MakePlot = TRUE, this will slow processing a tad bit. Set to MakePlot = FALSE if you do not need plots. \n")
  }


  # **Property Lookup Table** - Standardizing soil properties across datasets
  property_lookup <- list(
    # Bulk Density
    "bd" = c(PSP = "bd_mean", SG2 = "bdod", SOL = "dbovendry", CSRL = "bulk_density"),
    
    # Soil Texture (Sand, Silt, Clay)
    "clay" = c(PSP = "clay_mean", SG2 = "clay", SOL = "claytotal", CSRL = "clay_profile"),
    "sand" = c(PSP = "sand_mean", SG2 = "sand", SOL = "sandtotal", CSRL = "sand_profile"),
    "silt" = c(PSP = "silt_mean", SG2 = "silt", SOL = "silttotal", CSRL = "silt_profile"),
    
    # Soil Organic Matter / Carbon
    "som" = c(PSP = "om_mean", SG2 = NA, SOL = NA, CSRL = "som_max"),
    "soc" = c(PSP = "soc", SG2 = "soc", SOL = "soc", CSRL = "soc_max"),
    "soc_stock" = c(PSP = NA, SG2 = "ocs", SOL = NA, CSRL = "soc_stock"),
    "som_stock" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "som_stock"),
    
    #pH & Chemistry
    "ph" = c(PSP = "ph_mean", SG2 = "phh2o", SOL = "ph1to1h2o", CSRL = "ph_profile"),
    "cec" = c(PSP = NA, SG2 = "cec", SOL = "cec7", CSRL = "cec_profile"),
    "ec" = c(PSP = NA, SG2 = NA, SOL = "ec", CSRL = "ec_profile"),
    "sar" = c(PSP = NA, SG2 = NA, SOL = "sar", CSRL = "sar"),
    "caco3" = c(PSP = NA, SG2 = NA, SOL = "caco3", CSRL = "caco3"),
    "gypsum" = c(PSP = NA, SG2 = NA, SOL = "gypsum", CSRL = NA),
    
    # Hydrology & Conductivity
    "ksat" = c(PSP = "ksat_mean", SG2 = NA, SOL = NA, CSRL = "ksat_max"),
    "theta_s" = c(PSP = "theta_s_mean", SG2 = NA, SOL = NA, CSRL = NA),
    "theta_r" = c(PSP = "theta_r_mean", SG2 = NA, SOL = NA, CSRL = NA),
    
    # Coarse Fragments & Rock Volume
    "coarse_fragments" = c(PSP = NA, SG2 = "cfvo", SOL = "fragvol", CSRL = "rf_025"),
    
    # Land Use & Erosion
    "wind_erodibility_group" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "wind_erodibility_group"),
    "wind_erodibility_index" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "wind_erodibility_index"),
    "soil_order" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "soil_order"),
    "soil_temp_regime" = c(PSP = NA, SG2 = NA, SOL = NA, CSRL = "soil_temp_regime")
  )
  

  # **Depth Interval Lookup Table** - Standardizing depth labels across datasets
  depth_interval_lookup <- list(
    "0_5" = c("0_5", "0-5cm", "0_cm", "0-5"),
    "5_15" = c("5_15", "5-15cm", "5_cm", "0-25"),
    "15_30" = c("15_30", "15-30cm", "15_cm", "0-25", "25-50"),
    "30_60" = c("30_60", "30-60cm", "30_cm", "30-60", "25-50"),
    "60_100" = c("60_100", "60-100cm", "60_cm", "30-60"),
    "100_200" = c("100_200", "100-200cm", "100_cm", "150_cm")
  )

  # **Depth Range Lookup Table** - Assigning numeric depths
  depth_range_lookup <- list(
    "0_5" = c(0, 5),
    "5_15" = c(5, 15),
    "15_30" = c(15, 30),
    "30_60" = c(30, 60),
    "60_100" = c(60, 100),
    "100_200" = c(100, 200)
  )



  # **Detect Dataset Types (PSP, SG2, SOL)**
  datasets <- list()
  if (inherits(soil_data, "SpatRaster")) {
    dataset_name <- attr(soil_data, "product")
    if (is.null(dataset_name)) stop("Dataset type unknown. Ensure data comes from fetch_PSP, fetch_SG2, or fetch_SOL.")
    datasets[[dataset_name]] <- soil_data
  } else if (is.list(soil_data)) {
    for (raster_stack in soil_data) {
      dataset_name <- attr(raster_stack, "product")
      if (is.null(dataset_name)) stop("Dataset type unknown. Ensure data comes from fetch_PSP, fetch_SG2, or fetch_SOL.")
      datasets[[dataset_name]] <- raster_stack
    }
  } else {
    stop("Invalid input: Provide a SpatRaster object or a list of SpatRasters from fetch_PSP, fetch_SG2, or fetch_SOL.")
  }

  # **Reproject rasters to match zones CRS**
  zones_crs <- crs(shapes)
  for (dataset_name in names(datasets)) {
    if (crs(datasets[[dataset_name]]) != zones_crs) {
      datasets[[dataset_name]] <- project(datasets[[dataset_name]], zones_crs)
    }
  }

  # **Initialize Results DataFrame**
  results_df <- data.frame()
  weighted_mean_df <- data.frame()

  for (dataset_name in names(datasets)) {
    cat("\nProcessing dataset:", dataset_name, "\n")

    soil_rasters <- datasets[[dataset_name]]

    for (plot in plots) {
      poly <- shapes[shapes$Name == plot, ]
      poly <- vect(st_geometry(poly))

      for (prop in props) {
        dataset_prop <- property_lookup[[prop]][[dataset_name]]
        if (is.na(dataset_prop)) next

        # **Match raster layers based on property & depth**
        matched_layers <- names(soil_rasters)[grepl(dataset_prop, names(soil_rasters))]

        if (length(matched_layers) == 0) {
          warning(paste("No raster files found for", dataset_prop, "in", dataset_name, ". Skipping..."))
          next
        }

        selected_layers <- list()
        depth_weights <- list()

        for (raster_name in matched_layers) {
          matched_depth <- NA

          # **Extract depth from filename using interval lookup**
          for (depth_name in names(depth_interval_lookup)) {
            if (any(grepl(paste(depth_interval_lookup[[depth_name]], collapse = "|"), raster_name))) {
              matched_depth <- depth_name
              break
            }
          }
          if (is.na(matched_depth)) next

          layer_depths <- depth_range_lookup[[matched_depth]]
          overlap <- pmin(bdepth, layer_depths[2]) - pmax(tdepth, layer_depths[1])

          if (overlap > 0) {
            selected_layers[[raster_name]] <- soil_rasters[[raster_name]]
            depth_weights[[raster_name]] <- overlap / (layer_depths[2] - layer_depths[1])
          }

          raster_layer <- soil_rasters[[raster_name]]

          for (stat in stats) {
            zonal_stats <- zonal(raster_layer, poly, stat)

            # Store per-depth statistics
            results_df <- rbind(results_df, data.frame(
              Product = dataset_name,
              SoilProperty = prop,
              Depth = matched_depth,
              Statistic = stat,
              Plot = plot,
              ZonalStats = round(zonal_stats[[1]], 2)
            ))
          }
        }

        # **Compute Weighted Mean Across Depths**
        if (wtd.mean && length(selected_layers) > 0) {
          selected_rasters <- rast(selected_layers)
          weights <- unlist(depth_weights)
          weighted_raster <- sum(selected_rasters * weights) / sum(weights)

          for (stat in stats) {
            zonal_stats <- zonal(weighted_raster, poly, stat)

            # Store weighted mean statistics separately
            weighted_mean_df <- rbind(weighted_mean_df, data.frame(
              Product = dataset_name,
              SoilProperty = prop,
              Depth = paste0(tdepth, "_", bdepth),  # Weighted depth interval
              Statistic = stat,
              Plot = plot,
              ZonalStats = round(zonal_stats[[1]], 2)
            ))

            # ✅ Plot raster if enabled
            if (MakePlot) {
              options(terra.pal = NULL)
              buffer_size = 50
              bbox <- st_bbox(shapes) + c(-buffer_size, -buffer_size, buffer_size, buffer_size)

              plot(raster_layer, col = terrain.colors(10), main = paste("Map:", dataset_name, "| Plot:", plot, "| Property:", prop, "|", stat, ":", round(zonal_stats[[1]], 2)), ext = bbox)
              plot(st_geometry(shapes), add = TRUE, border = "black", lwd = 2)
              plot(poly, add = TRUE, border = "red", lwd = 2.5)
            }
          }
        }
      }
    }
  }

  # **Save Results**
  if (!is.null(output_dir)) {
    write.csv(results_df, file = file.path(output_dir, "DSM_zonal_stats_merged.csv"), row.names = FALSE)
    if (wtd.mean) {
      write.csv(weighted_mean_df, file = file.path(output_dir, "DSM_zonal_stats_weighted.csv"), row.names = FALSE)
    }
  }

  # **Return both datasets**
  return(list(
    Unweighted = results_df,
    Weighted = if (wtd.mean) weighted_mean_df else NULL
  ))
}
