
# UI with Dynamic Text Inputs
ui <- fluidPage(
  #includeCSS(here("www","styles.css")),  # Link to your CSS file
  #shinythemes::themeSelector(),# theme selector is only meant to be used while developing an application
  theme = shinytheme("readable"),
  titlePanel("Shapefile Data Processor"),
  
  # Conditional Panel for Login
  conditionalPanel(
    condition = "output.currentPanel === 'login'",
    wellPanel(
      textInput("user_name", "Enter your name:", ""),
      radioButtons("action_type", "Choose action:",
                   choices = c("Start New Shapefile" = "new", "Update Existing Shapefile" = "update")),
      
      conditionalPanel(
        condition = "input.action_type == 'update'",
        selectInput("shapefile_to_update", "Select Shapefile to Update:", choices = NULL),
        selectInput("update_with_shapefile", "Update With Shapefile:", choices = NULL),
        radioButtons("update_action", "Update Action:",
                     choices = c("Add Data" = "add", "Replace Section" = "replace")),
        uiOutput("orgnlSN_selector")
      ),
      actionButton("login", "Login", disabled = TRUE)
    )
  ),
  
  # New Shapefile Modify Values Page
  conditionalPanel(
    condition = "output.currentPanel === 'modifyValues'",
    fluidRow(
      column(12,
             tags$h3("Modify individual cells or columns in the selected shapefile. Raw Data is NOT altered"),
             tags$p("Please select a file from the dropdown menu below to begin. You can modify individual cells by double clicking on the cell or entire columns in the selected shapefile. Columns with more 10 unique values are restricted from large chages as these are generally numerical columns. Column changes require pressing 'Update Column Value' buton while cell changes are instantenous. Attempting to modify the geometry will crash the application. No current UNDO button so if an error is made you'll have to start over. Click the 'Next Page' button to proceed to the column mapping page."),
      )
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("shapefile", "Select a Shapefile:", choices = NULL),
        selectInput("columnToModify", "Select Column to Modify:", choices = NULL),
        uiOutput("uniqueValuesUI"),
        uiOutput("newValueUI"),
        actionButton("updateColumnValue", "Update Column Value"),
        actionButton("columnMapping", "Next Page"),
        uiOutput("info"),
        tableOutput("changeLogTable")
      ),
      mainPanel(
        DTOutput("dataPreview")
      )
    )
  ),
  
  # Column Mapping Page
  conditionalPanel(
    condition = "output.currentPanel === 'columnMapping'",
    fluidRow(
      column(12,
             tags$h3("Map columns from the selected shapefile to the standard columns. You can also enter values manually for each column."),
             tags$p("Please select a file from the dropdown menu below to begin. You can map columns from the selected shapefile to the standard columns or enter values manually. When entering values manually the entire column will have the entered data value. Click the 'Process Data' button to stage your processed data. If creating a new shapefile then proceed to the your next choice in the dropdown. Click the 'Create New Shapefile' button to create a new shapefile with the processed data."),
      )
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("shapefile_mapping", "Select a Shapefile:", choices = NULL),
        uiOutput("column_mapping_ui"),
        actionButton("process", "Process Data"),
        actionButton("create_shapefile", "Create New Shapefile")
      ),
      mainPanel(
        DTOutput('data_preview'),
        DTOutput("processed_data")
      )
    )
  )
)