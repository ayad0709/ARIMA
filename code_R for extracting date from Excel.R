# Load required libraries
install.packages(c("shiny", "readxl"))
library(shiny)
library(readxl)

# Define the Shiny app
shinyApp(
  # Define the user interface
  ui = fluidPage(
    titlePanel("Excel Date Column Parser"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose an Excel File",
                  accept = c(".xlsx")
        ),
        uiOutput("date_col_ui")
      ),
      mainPanel(
        tableOutput("table")
      )
    )
  ),
  
  # Define the server logic
  server = function(input, output, session) {
    # Load the data and make it available in a reactive variable
    data <- reactive({
      file <- input$file
      if (is.null(file)) {
        return(NULL)
      }
      read_excel(file$datapath)
    })
    
    # Dynamically generate the UI for choosing the date column
    output$date_col_ui <- renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      selectInput("date_col", "Choose the Date Column:", names(data()))
    })
    
    # Process the data and render the table
    output$table <- renderTable({
      if (is.null(data()) || is.null(input$date_col)) {
        return(NULL)
      }
      
      df <- data()
      
      # Check if the specified column is correctly parsed as Date
      # If not, manually convert it to Date type
      if (!inherits(df[[input$date_col]], "Date")) {
        df[[input$date_col]] <- as.Date(df[[input$date_col]], origin = "1899-12-30")
      }
      
      # Format the date column to "Day/Month/Year"
      df[[input$date_col]] <- format(df[[input$date_col]], "%d/%m/%Y")
      
      return(df)
    })
  }
)
