# Function to Read Shapefiles and Check/Reproject CRS
read_shapefile_and_check_crs <-
  function(file_path, target_crs_epsg = 2229) {
    # EPSG code for NAD83
    sf_object <- st_read(here(file_path))
    
    # Check if CRS EPSG code is not the target CRS EPSG code
    current_crs_epsg <- st_crs(sf_object)$epsg
    if (!is.na(current_crs_epsg) &&
        current_crs_epsg != target_crs_epsg) {
      message(
        "Reprojecting ",
        file_path,
        " from EPSG:",
        current_crs_epsg,
        " to EPSG:",
        target_crs_epsg
      )
      sf_object <- st_transform(sf_object, crs = target_crs_epsg)
    } else {
      message(file_path,
              " is already in the target CRS (EPSG:",
              target_crs_epsg,
              ").")
    }
    
    return(sf_object)
  }