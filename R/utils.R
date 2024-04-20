# Load Libraries
libraries <- c("janitor", "tidyverse", "sf", "here", "shiny", 'DT','shinythemes')
lapply(libraries, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})



# Function to Read Spatial Data (Shapefiles or GeoPackage) and Check/Reproject CRS
read_spatial_data_and_check_crs <- function(file_path, layer = NULL, target_crs = 2229) { # EPSG code for NAD83 / California zone 5 (ftUS)
  if (!is.null(layer)) {
    # Read GeoPackage layer
    sf_object <- st_read(here(file_path), layer = layer)
  } else {
    # Read shapefile or GeoPackage file
    sf_object <- st_read(here(file_path))
  }
  
  # Set the target CRS
  target_crs <- st_crs(paste0("EPSG:", target_crs))
  
  # Check if the CRS of the sf_object is not the target CRS
  if (!identical(st_crs(sf_object), target_crs)) {
    message("Reprojecting ", file_path, ifelse(!is.null(layer), paste0(" (Layer: ", layer, ")"), ""), " to ", target_crs$proj4string)
    sf_object <- st_transform(sf_object, crs = target_crs)
  } else {
    message(file_path, ifelse(!is.null(layer), paste0(" (Layer: ", layer, ")"), ""), " is already in the target CRS (", target_crs$proj4string, ").")
  }
  
  return(sf_object)
}


# Initialize a vector to store variable names
created_variables <- c()

# Function to create a variable name from a filename
create_var_name <- function(filename) {
  gsub("\\.(?:shp|gpkg)$", "", basename(filename))
}

# Iterate through each subfolder in the 'data' directory
subfolders <- list.dirs(here("data"), full.names = TRUE, recursive = FALSE)

for (folder in subfolders) {
  # List all shapefiles and GeoPackage files in the subfolder
  files <- list.files(folder, pattern = "\\.(?:shp|gpkg)$", full.names = TRUE)
  
  # Read each file and assign to a variable
  for (file in files) {
    var_name <- create_var_name(file)
    
    if (grepl("\\.shp$", file)) {
      # Read shapefile and check CRS
      shapefile_data <- read_spatial_data_and_check_crs(file)
      assign(var_name, shapefile_data, envir = .GlobalEnv)
    } else if (grepl("\\.gpkg$", file)) {
      # Read GeoPackage file
      layers <- st_layers(file)
      layer_names <- layers$name
      
      for (layer_name in layer_names) {
        # Read GeoPackage layer and check CRS
        geopackage_data <- read_spatial_data_and_check_crs(file, layer = layer_name)
        
        geopackage_var_name <- paste(var_name, layer_name, sep = "_")
        assign(geopackage_var_name, geopackage_data, envir = .GlobalEnv)
        created_variables <- c(created_variables, geopackage_var_name)
      }
    }
    
    created_variables <- c(created_variables, var_name)
  }
}