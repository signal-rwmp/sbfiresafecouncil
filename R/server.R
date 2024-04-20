server <- function(input, output, session) {
  login_status <- reactiveVal(FALSE)
  selected_shapefile <- reactiveVal(NULL)
  selected_cell_value <- reactiveVal(NULL)
  panel_state <- reactiveVal("login") # Default panel state
  selected_shapefile_to_update <- reactiveVal(NULL)
  # Filter to get only 'Merged' files
  merged_files <- created_variables[grepl("^Merged", created_variables)]
  sf_files <- created_variables[!grepl("^Merged", created_variables)]
  
  # Initialize a reactive value to store changes
  change_log <- reactiveVal(data.frame(
    Type = character(),
    Column = character(),
    Row = integer(),
    OldValue = character(),
    NewValue = character(),
    stringsAsFactors = FALSE
  ))
  
  observe({
    if (!is.null(input$action_type) && input$action_type == "new") {
      login_disabled <- is.null(input$user_name) || nchar(input$user_name) == 0
    } else {
      login_disabled <- is.null(input$user_name) || nchar(input$user_name) == 0
    }
    updateActionButton(session, "login", disabled = login_disabled)
  })
  
  
  
  
  observe({
    if (panel_state() == "login") {
      
      
      updateSelectInput(session, "shapefile_to_update", choices = merged_files)
      updateSelectInput(session, "update_with_shapefile", choices = sf_files)
    }
  })
  
  
  observeEvent(input$login, {
    req(input$user_name)
    login_status(TRUE)
    panel_state("modifyValues") # Move to modify values panel after login
    
    if (input$action_type == "update") {
      selected_shapefile_to_update(input$update_with_shapefile)
    }
  })
  
  # Update visibility for current panel
  output$currentPanel <- reactive({ panel_state() })
  outputOptions(output, "currentPanel", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$shapefile, {
    selected_shapefile(input$shapefile)
  })
  
  
  # Handle transition to column mapping panel
  observeEvent(input$columnMapping, {
    panel_state("columnMapping") # Change to column mapping panel
  })
  
  
  
  
  
  ## Populate shapefile dropdown if needed
  observe({
    if (panel_state() == "modifyValues") {
      if (!is.null(selected_shapefile_to_update())) {
        updateSelectInput(session, "shapefile", choices = selected_shapefile_to_update())
      } else {
        updateSelectInput(session, "shapefile", choices = sf_files)
      }
    }
  })
  
  # Populate shapefile dropdown on column mapping page
  observe({
    if (panel_state() == "columnMapping") {
      if (!is.null(selected_shapefile_to_update())) {
        updateSelectInput(session, "shapefile_mapping", choices = selected_shapefile_to_update())
      } else {
        updateSelectInput(session, "shapefile_mapping", choices = sf_files)
      }
    }
  })
  
  # Update selected shapefile when dropdown selection changes on column mapping page
  observeEvent(input$shapefile_mapping, {
    selected_shapefile(input$shapefile_mapping)
  })
  
  
  
  
  # Preview shapefile data
  output$dataPreview <- renderDT(server = FALSE,
                                 {
                                   req(selected_shapefile())
                                   shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
                                   
                                   datatable(
                                     shapefile,
                                     extensions = c('Scroller'),
                                     options = list(scroller = TRUE,
                                                    scrollY = 800,
                                                    deferRender = TRUE,
                                                    info = TRUE
                                     ),
                                     selection = list(mode = 'single', target = 'cell'),
                                     editable =  'cell',
                                     
                                   )
                                 })
  
  
  # Preview shapefile data
  output$data_preview <- renderDT(
    {
      req(selected_shapefile())
      shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
      
      datatable(
        shapefile,
        extensions = c('Scroller'),
        options = list(scroller = TRUE,
                       scrollY = 400,
                       deferRender = TRUE,
                       info = TRUE
        ),
        selection = 'none'
        
      )
    })
  
  # Populate column dropdown
  observe({
    req(selected_shapefile())
    updateSelectInput(session, "columnToModify", choices = names(get(selected_shapefile(), envir = .GlobalEnv)))
  })
  
  # Check unique values and display dropdown
  output$uniqueValuesUI <- renderUI({
    req(input$columnToModify)
    shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
    unique_values <- unique(shapefile[[input$columnToModify]])
    if (length(unique_values) <= 10) {
      selectInput("valueToModify", "Select Value to Modify:", choices = unique_values)
    } else {
      p("The selected column has more than 10 unique values. Modification is not allowed.")
    }
  })
  
  # Display input box for new value
  output$newValueUI <- renderUI({
    req(input$valueToModify)
    textInput("newValue", "Enter New Value:")
  })
  
  # Update column value in the data
  observeEvent(input$updateColumnValue, {
    req(selected_shapefile(), input$columnToModify, input$valueToModify, input$newValue)
    shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
    indices <- which(shapefile[[input$columnToModify]] == input$valueToModify)
    old_values <- unique(shapefile[[input$columnToModify]][indices])  # Capture unique old values
    shapefile[[input$columnToModify]][shapefile[[input$columnToModify]] == input$valueToModify] <- input$newValue
    assign(selected_shapefile(), shapefile, envir = .GlobalEnv)
    selected_shapefile(selected_shapefile())  # Trigger reactivity
    
    
    ## Log the change
    if (length(old_values) <= 10) {  # Only include old values detail if they are few
      old_value_summary <- paste(old_values, collapse = ", ")
    } else {
      old_value_summary <- paste(length(old_values), "values changed")
    }
    
    new_entry <- data.frame(
      Type = "Column",
      Column = input$columnToModify,
      RowsAffected = length(indices),
      Row = NA,
      OldValue = old_value_summary,
      NewValue = input$newValue,
      stringsAsFactors = FALSE
    )
    change_log(rbind(change_log(), new_entry))  # Append new entry to the log
  })
  
  
  
  observeEvent(input$dataPreview_cell_clicked, {
    cell_info <- input$dataPreview_cell_clicked
    
    if (!is.null(cell_info$value)) {
      value <- as.character(cell_info$value)
      selected_cell_value(value)  # Update the reactive variable
    }
  })
  
  #  Display selected cell value or unique values from selected column
  
  output$info <- renderUI({
    req(input$dataPreview_cells_selected)
    p(paste("Selected Cell Value:", selected_cell_value()))
    
  })
  
  #
  # Update selected cell value with new value to shapefile
  observeEvent(input$dataPreview_cell_edit, {
    info <- input$dataPreview_cell_edit
    if (is.null(info) || is.null(info$value)) return()  # Exit if no edit info
    
    # Retrieve the current shapefile data
    shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
    if (!"sf" %in% class(shapefile)) return()  # Check if it's an sf object
    
    try({
      # Adjust for zero-based index from DT and apply the new value
      row_index <- info$row   
      column_index <- info$col 
      old_value <- shapefile[row_index, column_index, drop = TRUE]
      shapefile[row_index, column_index] <- as.character(info$value)  # Convert to character if needed
      
      # Update the global environment and reactive value
      assign(selected_shapefile(), shapefile, envir = .GlobalEnv)
      selected_shapefile(selected_shapefile())  # Refresh reactive variable to update UI
    }, silent = TRUE)
    
    
    
    # Log changes
    new_entry <- data.frame(
      Type = "Cell",
      Column = names(shapefile)[column_index],
      RowsAffected = NA,
      Row = row_index,
      OldValue = as.character(old_value),
      NewValue = as.character(info$value),
      stringsAsFactors = FALSE
    )
    change_log(rbind(change_log(), new_entry))  # Append new entry to the log
    
  })
  
  
  output$changeLogTable <- renderTable({
    change_log()  # Render the change log as a table
  })
  
  
  
  
  
  #Reactive value to store the cumulative dataset
  cumulative_data <- reactiveVal(data.frame())
  
  # Update column mapping UI based on selected shapefile
  output$column_mapping_ui <- renderUI({
    req(selected_shapefile())
    shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
    #shapefile_cols <- setdiff(names(shapefile), "geometry")
    get_non_geom_columns <- function(sf_object) {
      geom_col_name <- names(st_geometry(sf_object))
      non_geom_cols <- setdiff(names(sf_object), geom_col_name)
      return(non_geom_cols)
    }
    
    # Usage
    shapefile_cols <- get_non_geom_columns(shapefile)
    
    
    
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
      if (!is.null(input[[select_id]]) &&
          input[[select_id]] == "Enter value manually") {
        textInput(
          paste0("manual_", col),
          label = paste("Enter value for", col),
          value = ""
        )
      }
    })
  })
  
  # Process the shapefile when the button is clicked
  observeEvent(
    input$process,
    {
      req(selected_shapefile())
      shapefile <- get(selected_shapefile(), envir = .GlobalEnv)
      
      # Function to get value or mapped column
      get_value_or_column <- function(col_name, shapefile) {
        map_input <- input[[paste0("map_", col_name)]]
        manual_input <- input[[paste0("manual_", col_name)]]
        
        if (map_input == "Enter value manually") {
          # Use the manual input if "Enter value manually" is selected
          return(rep(manual_input, nrow(shapefile)))
        } else if (map_input != "Null") {
          # Use the selected column from the shapefile
          return(shapefile[[map_input]])
        } else {
          # Return NA if "Null" is selected
          return(rep(NA, nrow(shapefile)))
        }
      }
      
      # Get the entered username
      username <- input$user_name
      
      # Get the current timestamp
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      
      # Create a new tibble with user inputs and selected columns
      new_data <- tibble(
        OrgnlSN = selected_shapefile(),
        Agency = get_value_or_column("Agency", shapefile),
        DtAcqrd = get_value_or_column("DtAcqrd", shapefile),
        Acreage = get_value_or_column("Acreage", shapefile),
        TF_DYS = get_value_or_column("TF_DYS", shapefile),
        PrjctNm = get_value_or_column("PrjctNm", shapefile),
        TrtmntT = get_value_or_column("TrtmntT", shapefile),
        DtComplt = get_value_or_column("DtComplt", shapefile),
        Other = get_value_or_column("Other", shapefile),
        Username = username,
        Timestamp = timestamp
      )
      #print(paste("New data dimensions:", dim(new_data)))  # Debugging statement
      
      # Append to the cumulative dataset
      try({
        cumulative_data(bind_rows(cumulative_data(), new_data))
      }, silent = TRUE)
    })
  
  
  
  
  
  output$processed_data <- renderDT({
    datatable(
      cumulative_data(),
      extensions = c('Scroller'),
      options = list(scroller = TRUE, scrollY = 800, deferRender = TRUE, info = TRUE),
      selection = 'none'
    )
  })
  
  
  # Handle the creation of a new shapefile
  observeEvent(input$create_shapefile, {
    # Get the processed data
    processed_data <- cumulative_data()
    print(paste("Processed data dimensions:", dim(processed_data)))  # Debugging statement
    
    # Ensure there is data to process
    if (nrow(processed_data) == 0) {
      showModal(modalDialog(
        title = "No Data",
        "There is no processed data to create a shapefile."
      ))
      return()
    }
    # Access the current state of change_log
    log_data <- change_log()
    log_file_path <- here('www', "change_log.txt")
    
    # Check if the log_data is not empty
    if (nrow(log_data) > 0) {
      # Get the entered username
      username <- isolate(input$user_name)  # use isolate to avoid reactivity
      
      # Get the current timestamp
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Create a single line with username and timestamp
      user_time_stamp_line <- paste("User:", username, "Timestamp:", timestamp, sep = "\t")
      
      # Write the username and timestamp line to the file
      write_lines(user_time_stamp_line, log_file_path, append = TRUE)
      write_lines("--------------------------------------------------", log_file_path, append = TRUE)
      # Write the change log data below the username and timestamp
      write.table(
        log_data,
        log_file_path,
        sep = "\t",
        row.names = FALSE,
        col.names = !file.exists(log_file_path),  # write column names only if file doesn't exist
        quote = FALSE,
        append = TRUE
      )
      write_lines("--------------------------------------------------", log_file_path, append = TRUE)
      # Optionally, clear log_data after writing, if you don't need to accumulate the entries
      # change_log(data.frame(Type = character(), Column = character(), Row = integer(),
      #                      OldValue = character(), NewValue = character(),
      #                      stringsAsFactors = FALSE))
    }
    
    # Create a list to store sf objects
    sf_list <- list()
    
    # Loop through unique values in OrgnlSN and create sf objects
    for (sn in unique(processed_data$OrgnlSN)) {
      original_shapefile <- get(sn)
      #print(paste("Original shapefile dimensions for", sn, ":", dim(original_shapefile)))  # Debugging statement
      
      # Assuming the original shapefiles are 'sf' objects
      if ("sf" %in% class(original_shapefile)) {
        # Create a new sf object with processed data and original geometry
        sf_object <- st_sf(processed_data[processed_data$OrgnlSN == sn, ], geometry = original_shapefile$geometry)
        sf_list[[sn]] <- sf_object
      }
    }
    
    # Combine the sf objects
    new_shapefile <- do.call(rbind, sf_list)
    #print(paste("New shapefile dimensions:", dim(new_shapefile)))  # Debugging statement
    
    
    # Set the CRS of the new_shapefile to the target CRS
    new_shapefile <- st_set_crs(new_shapefile, st_crs(2229))
    # Get the data directory
    data_dir <- here("data","Merged")
    # Create a GeoPackage filename with the current date
    geopackage_filename <- paste0("Merged_", format(Sys.Date(), "%Y%m%d"), ".gpkg")
    # Create the full path to save the GeoPackage file
    geopackage_path <- file.path(data_dir, geopackage_filename)
    # Create a unique layer name with timestamp
    layer_name <- paste0("layer_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    
    
    
    
    if (input$action_type == "new") {
      # Write the new shapefile as a new layer in the GeoPackage
      st_write(new_shapefile, geopackage_path, layer = layer_name, append = TRUE)
      
      showModal(modalDialog(
        title = "Success",
        paste("New layer added to GeoPackage:", geopackage_path, "with layer name:", layer_name)
      ))
    }  else if (input$action_type == "update") {
      # Get the selected shapefiles for updating
      shapefile_to_update <- input$shapefile_to_update
      update_with_shapefile <- new_shapefile
      
      # Read the shapefiles from the environment variables
      existing_shapefile <- get(shapefile_to_update, envir = .GlobalEnv)
      #new_shapefile_data <- processed_data
      
      # Print column names for debugging
      #print(paste("Columns in existing_shapefile:", paste(names(existing_shapefile), collapse = ", ")))
      #print(paste("Columns in new_shapefile_data:", paste(names(update_with_shapefile), collapse = ", ")))
      
      if (input$update_action == "add") {
        # Add the geometry column from the existing shapefile to the new shapefile data
        #new_shapefile_data$geom <- existing_shapefile$geom
        
        
        #Rename the geometry column to 'geom'
        st_geometry(update_with_shapefile) <- "geom"
        
        # Add the rows of the new shapefile to the existing shapefile
        updated_shapefile <- rbind(existing_shapefile, update_with_shapefile)
        
        if (is.na(st_crs(updated_shapefile))) {
          # Set the CRS if it's NA (not available)
          updated_shapefile <- st_set_crs(updated_shapefile, 2229)
        } else {
          # Transform the CRS to EPSG:2229
          updated_shapefile <- st_transform(updated_shapefile, 2229)
        }
        
        # Write the updated shapefile as a new layer in the GeoPackage
        st_write(updated_shapefile, geopackage_path, layer = layer_name, append = TRUE)
        
        showModal(modalDialog(
          title = "Success",
          paste("Rows added to new shapefile:", geopackage_path, "with layer name:", layer_name)
        ))
      } else if (input$update_action == "replace") {
        
        # Replace specific rows in the existing shapefile with the new shapefile data
        #Rename the geometry column to 'geom'
        st_geometry(update_with_shapefile) <- "geom"
        
        existing_shapefile <- existing_shapefile %>%
          filter(!OrgnlSN %in% unique(update_with_shapefile$OrgnlSN)) |> # Remove rows with matching OrgnlSN
          bind_rows(update_with_shapefile)  # Add the new shapefile data
        
        
        
        
        if (is.na(st_crs(update_with_shapefile))) {
          # Set the CRS if it's NA (not available)
          existing_shapefile <- st_set_crs(existing_shapefile, 2229)
        } else {
          # Transform the CRS to EPSG:2229
          existing_shapefile <- st_transform(existing_shapefile, 2229)
        }
        
        # Write the updated shapefile as a new layer in the GeoPackage
        st_write(existing_shapefile, geopackage_path, layer = layer_name, append = TRUE)
        
        showModal(modalDialog(
          title = "Success",
          paste("Rows replaced in new shapefile:", geopackage_path, "with layer name:", layer_name)
        ))
      }
    }
  })
  
  
}