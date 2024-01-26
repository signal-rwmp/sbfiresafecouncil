# Load Libraries
libraries <- c("janitor", "tidyverse", "sf", "here", "shiny")
lapply(libraries, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})


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

# Initialize a vector to store variable names
created_variables <- c()
sf_objects_list <- list()


# Function to create a variable name from a filename
create_var_name <- function(filename) {
  gsub("\\.shp$", "", basename(filename))
}

# Iterate through each subfolder in the 'data' directory
subfolders <-
  list.dirs(here("data"), full.names = TRUE, recursive = FALSE)
for (folder in subfolders) {
  
  # List all shapefiles in the subfolder
  shapefiles <-
    list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  
  # Read each shapefile and assign to a variable
  for (shapefile in shapefiles) {
    var_name <- create_var_name(shapefile)
    assign(var_name, read_shapefile_and_check_crs(shapefile))
    created_variables <- c(created_variables, var_name)
    sf_objects_list[[var_name]] <- read_shapefile_and_check_crs(shapefile)
  
  }
}


# Define the standard columns
standard_columns <-
  c("Agency",
    "DtAcqrd",
    "Acreage",
    "TF(DYS)",
    "PrjctNm",
    "TrtmntT",
    "DtComplt",
    "Other")

# UI with Dynamic Text Inputs
ui <- fluidPage(titlePanel("Shapefile Data Processor"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("shapefile", "Select a Shapefile:", choices = created_variables),
                    uiOutput("column_mapping_ui"),
                    actionButton("process", "Process Shapefile"),
                    actionButton("create_shapefile", "Create New Shapefile")
                  ),
                  mainPanel(tableOutput("data_preview"),
                            tableOutput("processed_data"))
                ))

server <- function(input, output, session) {
  
  # Reactive value to store the cumulative dataset
  cumulative_data <- reactiveVal(data.frame())
  
  # Helper function to update a list with a new key-value pair
  update_list <- function(lst, key, value) {
    lst[[key]] <- value
    lst
  }
  
  # Create a reactive value to store the mapping of OrglSN to geometries
  geometries_mapping <- reactiveVal(list())
  
  # Preview the data of the selected shapefile, excluding the geometry column
  output$data_preview <- renderTable({
    req(input$shapefile)
    shapefile <- get(input$shapefile)
    if ("sf" %in% class(shapefile)) {
      shapefile <- as.data.frame(shapefile)
    }
    head(shapefile[setdiff(names(shapefile), "geometry")])
  })
  
  # Update column mapping UI based on selected shapefile
  output$column_mapping_ui <- renderUI({
    req(input$shapefile)
    shapefile <- get(input$shapefile)
    shapefile_cols <- setdiff(names(shapefile), "geometry")
    
    map_ui <- lapply(standard_columns, function(col) {
      select_id <- paste0("map_", col)
      text_id <- paste0("text_", col)
      fluidRow(
        column(6, selectInput(select_id, label = paste("Map", col, "to:"), 
                              choices = c("Null", shapefile_cols, "Enter value manually"))),
        column(6, uiOutput(text_id))  # Dynamic UI for text input
      )
    })
    do.call(tagList, map_ui)
  })
  
  # Dynamic text input based on dropdown selection
  lapply(standard_columns, function(col) {
    output[[paste0("text_", col)]] <- renderUI({
      select_id <- paste0("map_", col)
      if(!is.null(input[[select_id]]) && input[[select_id]] == "Enter value manually") {
        textInput(paste0("manual_", col), label = paste("Enter value for", col), value = "")
      }
    })
  })
  
  
  # Process the shapefile when the button is clicked
  observeEvent(input$process, {
    req(input$shapefile)
    shapefile <- sf_objects_list[[input$shapefile]]
    geometry <- st_geometry(shapefile)
    shapefile_df <- as.data.frame(shapefile) %>% select(-geometry)
    
    # Function to get value or mapped column
    get_value_or_column <- function(col_name, shapefile) {
      map_input <- input[[paste0("map_", col_name)]]
      manual_input <- input[[paste0("manual_", col_name)]]
      
      if(map_input == "Enter value manually") {
        # Use the manual input if "Enter value manually" is selected
        return(rep(manual_input, nrow(shapefile)))
      } else if(map_input != "Null") {
        # Use the selected column from the shapefile
        return(shapefile[[map_input]])
      } else {
        # Return NA if "Null" is selected
        return(rep(NA, nrow(shapefile)))
      }
    }
    
    # Create a new tibble with user inputs and selected columns
    new_data <- tibble(
      OrgnlSN = input$shapefile,
      Agency = get_value_or_column("Agency", shapefile),
      DtAcqrd = get_value_or_column("DtAcqrd", shapefile),
      Acreage = get_value_or_column("Acreage", shapefile),
      `TF(DYS)` = get_value_or_column("TF(DYS)", shapefile),
      PrjctNm = get_value_or_column("PrjctNm", shapefile),
      TrtmntT = get_value_or_column("TrtmntT", shapefile),
      DtComplt = get_value_or_column("DtComplt", shapefile),
      Other = get_value_or_column("Other", shapefile)
    )
 
    geometries_mapping(update_list(geometries_mapping(), input$shapefile, geometry))
    # Append to the cumulative dataset
    try({
      cumulative_data(bind_rows(cumulative_data(), new_data))
    }, silent = TRUE)
  }
  
  
  )
  
  # Render the processed data
  
  
  

  # Handle the creation of a new shapefile
  observeEvent(input$create_shapefile, {
    # Get the processed data
    processed_data <- cumulative_data()
    
    
    # Ensure there is data to process
    if (nrow(processed_data) == 0) {
      showModal(modalDialog(
        title = "No Data",
        "There is no processed data to create a shapefile."
      ))
      return()
    }
    
    
    # Retrieve geometries using OrgnlSN
    # Assuming geometries_mapping and processed_data are in sync
    geometries <- unlist(geometries_mapping(), recursive = FALSE)
    
    # Ensure that the number of geometries matches the number of rows in processed_data
    if (length(geometries) != nrow(processed_data)) {
      showModal(modalDialog(title = "Error", "Mismatch between data rows and geometries."))
      return()
    }

    # Combine the geometries into a single sfc object
    geometries_sf <- st_sfc(geometries)
    
    
    # Ensure that the number of geometries matches the number of rows in processed_data
    if (length(geometries_sf) != nrow(processed_data)) {
      showModal(modalDialog(title = "Error", "Mismatch between data rows and geometries."))
      return()
    }
    
    
    
    # Create a new shapefile with the combined geometries
    new_shapefile <- st_sf(processed_data, geometry = geometries_sf) |> 
      st_set_crs('EPSG:2229')
    
    # conversion of a boolean field to string
    new_shapefile$`TF(DYS)` <- as.character(new_shapefile$`TF(DYS)`)
    new_shapefile$DtAcqrd <- as.character(new_shapefile$DtAcqrd)
    new_shapefile$DtComplt <- as.character(new_shapefile$DtComplt)
    new_shapefile$TrtmntT <- as.character(new_shapefile$TrtmntT)
    new_shapefile$PrjctNm <- as.character(new_shapefile$PrjctNm)
    new_shapefile$Agency <- as.character(new_shapefile$Agency)
    new_shapefile$Other <- as.character(new_shapefile$Other)
    
    st_write(new_shapefile, "new_shapefile.gpkg", driver = "GPKG", append = FALSE)
    
    showModal(modalDialog(
      title = "Success",
      "New shapefile created successfully."
    ))
  })
  
  # Display the processed data
  output$processed_data <- renderTable({
    cumulative_data()
  })
}


# Run the app
shinyApp(ui, server)