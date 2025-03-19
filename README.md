# OpenSoilDataR Tutorial 🚜🌱

#### by Meyer Bohn, Geospatial Laboratory for Soil Informatics --- Department of Agronomy, Iowa State University

This repository provides a tutorial on using **OpenSoilDataR** to fetch soil property rasters from:
- **POLARIS (PSP)** ```Chaney, N.W., Minasny, B., Herman, J.D., Nauman, T.W., et al. 2019. POLARIS Soil Properties: 30-m Probabilistic Maps of Soil Properties Over the Contiguous United States. Water Resour Res 55, 2916–2938. https://doi.org/10.1029/2018WR022797```
- **SoilGrids v2 (SG2)** ```Hengl, T., De Jesus, J.M., Heuvelink, G.B.M., Gonzalez, et al. 2017. SoilGrids250m: Global gridded soil information based on machine learning. PLoS One 12, e0169748. https://doi.org/10.1371/JOURNAL.PONE.0169748```
- **SOLUS100 (SOL)**  ```Nauman, T.W., Kienast-Brown, S., Roecker, S.M., Brungard, et al. 2024. Soil landscapes of the United States (SOLUS): Developing predictive soil property maps of the conterminous United States using hybrid training sets. Soil Science Society of America Journal 88, 2046–2065. https://doi.org/10.1002/SAJ2.20769```

- And compute **zonal statistics** for soil properties.

---

## 📥 Installation

Ensure you have the necessary **R packages** installed:

```r
install.packages(c("terra", "sf", "dplyr", "tidyr", "rgeedim"))
devtools::install_github("MollicMeyer/OpenSoilDataR")  # Replace with actual GitHub repo

````
##### Load the libraries:
```r
library(rgeedim)
library(OpenSoilDataR)
library(terra)

````

🔑 Authenticate & Initialize Google Earth Engine
Before fetching data, authenticate Google Earth Engine:

```r
gd_authenticate(auth_mode = "notebook")  # Authenticate
gd_initialize()  # Initialize

````

📍 Define Area of Interest (AOI)
Set your area of interest (AOI) using a raster file:

````
output_directory <- "path/to/output/directory"
aoi_raster <- rast("path/to/example_raster.tif")
````
🌍 Fetch Soil Property Data  
1️⃣ Fetch PSP Data (POLARIS)

````
psp_data <- fetch_PSP(
  aoi = aoi_raster,
  properties = c("om_mean", "clay_mean"),
  depths = c("0_5", "5_15", "15_30"),
  output_dir = output_directory,
  suffix = "example",
  crs = "EPSG:4326",
  scale = 30,
  export = TRUE,
  tosoc = TRUE, # converts to soc by van bemmelen 1.724
  convertOM = TRUE # comes in log10, converts to %
)

# Plot the loaded SpatRaster
plot(psp_data$stack)

````
2️⃣ Fetch SG2 Data (SoilGrids v2)

````
sg2_data <- fetch_SG2(
  aoi = aoi_raster,
  properties = c("soc", "clay"),
  depths = c("0-5cm", "5-15cm", "15-30cm"),
  output_dir = output_directory,
  suffix = "example",
  crs = "EPSG:4326",
  scale = 250,
  export = TRUE
)

# Plot the loaded SpatRaster
plot(sg2_data$stack)

````

3️⃣ Fetch SOL Data (SOLUS100) - This is slower for some non-integer files (soc) - has to download entire raster currently as hosted on google apis
````
sol_data <- fetch_SOL(
  aoi = aoi_raster,
  properties = c("claytotal"),
  depths = c("0_cm", "5_cm", "15_cm"),
  measures = c("p"),  #mean Prediction values "l" - 5th quantile - "h" 95th quantile - "rpi" - relative prediction interval, data heavy!!
  output_dir = output_directory,
  suffix = "example",
  export = TRUE
)

# Plot the loaded SpatRaster
plot(sol_data$stack)

````
📊 Compute Zonal Statistics  
1️⃣ Load the Plot Shapefile

````
plotpath <- "path/to/example_shapefile.shp"
zones <- st_read(plotpath)  # Read the shapefile
#Note: The shapefile must contain a "Name" field for identifying individual plots.

````
2️⃣ Run Zonal Statistics  
Gets the depth weighted value if you take a 20cm slice
````
zonal_results <- s.zonalstats(
  soil_data = list(sg2_data$stack, psp_data$stack, sol_data$stack),  # Multiple datasets
  tdepth = 0,
  bdepth = 20,
  props = c("sand", "clay", "soc"),
  shapes = zones,
  plots = c("NW_plot", "SW_plot"),
  stats = c("mean", "min", "max", "sd"),
  wtd.mean = TRUE,
  output_dir = "path/to/output/directory",
  MakePlot = TRUE
)

# **Check output**
print(zonal_results)

````
