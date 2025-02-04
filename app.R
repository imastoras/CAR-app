################################################################################
library(stringdist)
library(dplyr)
library(mice)
library(tidyr)
library(tidyverse)
library(shiny)
library(janitor)
library(DT)
library(ggplot2)
library(shinyjs)
library(colourpicker)
library(gginference)
library(vroom)
library(readxl)
library(bslib)
library(thematic)
library(shinydashboard)
library(plotly)
library(htmlwidgets)
library(shinythemes)
library(shinyWidgets)
library(cluster)
library(factoextra)
library(NbClust)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(gbm)
library(pROC)
library(nnet)
library(rminer)
library(shinyalert)
library(shinyjs)
library(viridis)
library(GGally)
library(igraph)
library(haven)
library(rlang)

################################################################################
ui <- fluidPage(
  
 
  theme = bs_theme(bootswatch = "bootstrap", secondary = "rgb(233, 236, 239)"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
    
    # Custom CSS for full-screen background on the home page and load data tab
    tags$style(HTML("
    /* Home page background full-screen */
    #home-page {
      background-image: url('image.png');  /* Path to home page background image */
      background-size: cover;
      background-position: center center;
      background-repeat: no-repeat;
      height: 100vh;  /* Full-screen height */
      width: 100vw;  /* Full-screen width */
      display: flex;
      justify-content: center;
      align-items: center;
      position: relative;
    }
    
    /* Load Data page background */
    #load-data-page {
      background-image: url('load_data.jpg');  /* Path to your background image */
      background-size: cover;  /* Ensure the image covers the container */
      background-position: center center;  /* Center the image */
      background-repeat: no-repeat;  /* Prevent tiling */
      height: 450px;  /* Adjust height to match the logo */
      position: relative;  /* Ensure the image stays behind the content */
      margin-bottom: 20px;  /* Add spacing between the logo and text */
    }
    
    /* Remove background for other tabs */
    .main-app-content {
      background-color: white;  /* Set a default background for other tabs */
    }
    
  "))
  ),
    
  # Home Page (initially visible)
  conditionalPanel(
    condition = "output.showHomePage",  # Conditional for showing homepage
    fluidRow(
      id = "home-page",  # Add id for the home page background control
      column(12, align = "center",
             div(
               class = "logo-container",  # This div contains the background and the logo
               img(src = "car_logo.png", height = "300px", class = "animate__animated animate__bounce")
             ),  
             br(),
             br(),
             br(),
             br(),
             actionButton("start_button", "Start CAR Application!", class = "btn btn-primary btn-lg"),  # Add Start button
             br(),
             br(),
             br(),
             br(),
             br(),
             p("Data protection & Disclaimer: As operators of this site, we take the protection of your personal information very seriously.", 
               style = "color:black;"),
             p("This open app is solely for academic/research purposes and does not save any of your data or your statistical tests/plots.", 
               style = "color:black;"),
             br()
      )
    )
  ),
  
  # Main App (hidden initially)
  conditionalPanel(
    condition = "output.showMainApp",  # Conditional for showing the main app
    navbarPage(
      windowTitle = "CAR App",

      # Add title correctly within the navbarPage
      title = div(
        class = "navbar-header", 
        div(
          class = "container-fluid", 
          div(class = "navbar-brand", 
              img(src = "logo_small1.png", height = "60px", style = "display: inline-block;")  # Add logo here
          )
        )
      ),
      
      ############################################################################
      tabPanel("Load Data",
               fluidRow(
                 id = "load-data-page",  # Add id for the load data background control
                 column(12, align = "center",
                        div(style = "margin-top: 150px;",  # Adjust to center vertically
                            # This empty div only contains the background image and is kept to preserve the background styling.
                            
                        )
                 )
               ),
               # This new row contains the content and appears below the background image.
               fluidRow(
                 # Left column with the first disclaimer text
                 column(4, align = "right", style = "padding-top: 20px;",
                        h6("1) Variable names must be on the 1st line of your Excel/SPSS file. 
                           If there are comments on your 1st line, please remove them manually.")
                 ),
                 
                 # Center column with fileInput and df_status
                 column(4, align = "center",
                        div(style = "margin-top: 20px;",
                            fileInput("file1", "Choose File", accept = c(".xls", ".xlsx", ".csv", ".sav")),
                            textOutput("df_status"),
                            br()
                        )
                 ),
                 
                 # Right column with the second disclaimer text
                 column(4, align = "left", style = "padding-top: 20px;",
                        h6("2) CAR application works ideally in data with at most 1000 observations. 
                           Data with more observations can be analysed as well but with more computational time.")
                 )
               ),
               
               # Bottom row for the return button centered below everything
               fluidRow(
                 column(12, align = "center",
                        div(style = "margin-top: 20px;",
                            actionButton("return_home", "Return to Home Page", class = "btn btn-secondary btn-lg")
                        )
                 )
               )
      ),
      ############################################################################         
      tabPanel("Cleaning",
               sidebarLayout(
                 sidebarPanel(
                   h3("Clean your Data with one click!"),
                   actionButton("automated_cleaning", "Automated Cleaning"),
                   uiOutput("manipulate_options"),
                   actionButton("apply_single", "Apply"),
                   uiOutput("further_options"),
                   actionButton("apply_additional", "Apply"),
                   hr(),
                   h3("Download your Clean Data!"),
                   downloadButton("download_final_data", "Download Finalized Data")  # Add this line for the download button
                 ),
                 mainPanel(
                   card(
                     card_header("Your Data"),
                     DT::dataTableOutput("data_table")
                   ),
                   actionButton("undo", "Undo Last Change"),  # Add Undo button
                   actionButton("reset", "Restore Initial Data"),
                   p(""),
                   uiOutput("explanation_card")
                 )
               )
      ),
      ############################################################################
      tabPanel("Descriptive Statistics and Statistical Tests",
               sidebarLayout(
                 sidebarPanel(
                   h4("Descriptive Statistics"),
                   checkboxInput("summary_choice", "Explore the characteristics of your Data", value = FALSE),
                   hr(),
                   h4("Statistical Tests"),
                   selectInput("test_mode", "Select Test Mode:", choices = c("Single Variable", "Between Two Variables", "ANOVA"), selected = "Single Variable"),
                   
                   # Conditional panel for Single Variable tests
                   conditionalPanel(
                     condition = "input.test_mode == 'Single Variable'",
                     selectInput("test_var", "Select Variable for Statistical Test:", choices = NULL),
                     
                     # Output conditional UI elements based on variable type
                     uiOutput("test_options")
                   ),
                   
                   # Conditional panel for Between Two Variables tests
                   conditionalPanel(
                     condition = "input.test_mode == 'Between Two Variables'",
                     selectInput("test_var1", "Select First Variable:", choices = NULL),
                     selectInput("test_var2", "Select Second Variable:", choices = NULL),
                     
                     # Conditional panel for test type based on variable selection
                     uiOutput("test_type_options"),
                     
                     # Display error message if mixed types are selected
                     textOutput("two_var_error_message")
                   ),
                   
                   # Conditional UI for when ANOVA is selected
                   conditionalPanel(
                     condition = "input.test_mode == 'ANOVA'",
                     
                     # Select dependent variables
                     selectInput("dependent_vars", "Select Dependent Variable(s):", 
                                 choices = NULL,  
                                 multiple = TRUE),
                     
                     # Select independent variables
                     selectInput("independent_vars", "Select Independent Variable(s):", 
                                 choices = NULL,  
                                 multiple = TRUE)
                   ),
                   
                   actionButton("test_apply", "Apply")
                 ),
                 
                 mainPanel(
                   card(card_header("Data Descriptive Statistics"),
                        verbatimTextOutput("data_str"),
                        verbatimTextOutput("data_summary")
                   ), 
                   card(card_header("Statistic Tests"),
                        uiOutput("test_explanation"),
                        textOutput("ANOVA_explanation"),
                        verbatimTextOutput("test_summary"),
                        plotOutput("test_plot"),
                        textOutput("error_message"),
                        downloadButton("download_plot", "Download Plot")
                   )
                 )
               )
      ),
      ############################################################################        
      tabPanel("Plotting",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("plot_type", "Select Plot Type:", choices = c("Scatter Plot", "Box Plot", "Bar Plot", "Violin Plot","Heatmap", "Bubble Chart","Pie","Line Graph with Error Bars",
                                                                             "Contour Plot","Network Graph","Candlestick Chart","Time Series", "Mapbox Plot")),
                   uiOutput("plot_options"),
                   textInput("plot_title1", "Plot Title:", value = "Interactive Plot"),
                   downloadButton("download_plot1", "Download Plot")
                 ),
                 mainPanel(
                   card(card_header("Data Plotting"),
                        plotlyOutput("plot1")),
                   card(card_header("Plot Explanation"),
                        uiOutput("plot_explanation"))
                 )
               )
      ),
      ############################################################################      
      tabPanel("Machine Learning",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("ml_type", "Select Machine Learning Model:", choices = c("-", "Clustering", "Linear Regression", "CART", "SVM", "Boost Gradient Model")),
                   uiOutput("ml_specific_ui"),
                   actionButton("ml_apply", "Apply")
                 ),
                 mainPanel(
                   uiOutput("ml_results_ui")
                 )
               )
      )
    )
  )
)

 ###############################################################################
server <- function(input, output, session) {
  
  # Reactive value to manage the current state (Home or Main App)
  rv <- reactiveValues(showHome = TRUE, showMain = FALSE)
  
  # Toggle homepage and main app visibility
  output$showHomePage <- reactive({
    rv$showHome
  })
  output$showMainApp <- reactive({
    rv$showMain
  })
  outputOptions(output, "showHomePage", suspendWhenHidden = FALSE)
  outputOptions(output, "showMainApp", suspendWhenHidden = FALSE)
  
  # Observe the Start button click
  observeEvent(input$start_button, {
    rv$showHome <- FALSE  # Hide homepage
    rv$showMain <- TRUE   # Show main app
  })
  
  # Observe the Return to Home button click to go from main app to home page
  observeEvent(input$return_home, {
    rv$showHome <- TRUE  # Show homepage
    rv$showMain <- FALSE  # Hide main app
  })
  
  ##############################################################################
  df_copy <- reactiveVal(NULL)
  df_initial <- reactiveVal(NULL)
  # ReactiveValues to store the history of the data frames
  df_history <- reactiveValues(history = list())
  
  observe({
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    
    # Check if the file extension is supported
    if (!(ext %in% c("csv", "xls", "xlsx", "sav"))) {
      df_copy(NULL)
      df_initial(NULL)
      output$df_status <- renderText({
        "Unsupported file type. Please upload a .csv, .xls, .xlsx, or .sav file."
      })
      return(NULL)  # Exit the observer if the file type is unsupported
    }
    
    # Load the data based on the file extension
    df <- switch(ext,
                 "csv" = read.csv(file),
                 "xls" = read_excel(file),
                 "xlsx" = read_excel(file),
                 "sav" = read_sav(file))
    
    # Update reactive values with the loaded data
    df_initial(df)  # Store the initial data
    df_copy(df)  # Set the current data as initial data
    df_history$history <- list(df)  # Initialize history with the loaded data
    
    # Display the initial data in the main panel
    output$data_table <- DT::renderDataTable({
      DT::datatable(df_initial())
    })
    
    # Set status to show successful load
    output$df_status <- renderText({
      "Dataset loaded successfully."
    })
  })
  
  # Show initial status if no file is loaded
  output$df_status <- renderText({
    if (is.null(df_copy())) {
      "Dataset is not loaded yet. (Accepted types xls/xlsx/csv/sav)"
    } else {
      "Dataset loaded successfully."
    }
  })
  
  ##############################################################################
  # Helper function to push to history
  push_to_history <- function(df) {
    if (length(df_history$history) == 0 || !identical(df, tail(df_history$history, n = 1)[[1]])) {
      df_history$history <- append(df_history$history, list(df))
    }
  }
  
  # Helper function to pop from history
  pop_from_history <- function() {
    if (length(df_history$history) > 1) {
      df_history$history <- df_history$history[-length(df_history$history)]
      return(tail(df_history$history, n = 1)[[1]])
    }
    NULL
  }                   
  
  
  # Data cleaning Phase
  automated_modifications_text <- reactiveVal(HTML(paste("Data aren't Auto-Cleaned yet. If you want so, please select Automated Cleaning button.")))
  
  # Automated Cleaning
  observeEvent(input$automated_cleaning, {
    
    # Show warning message for automated cleaning
    showModal(modalDialog(
      title = "Warning",
      "Automated Cleaning sometimes identifies incorrectly the class of a variable. Please after cleaning check Descriptive Characteristics of your data in order to confirm that your data are saved properly!",
      easyClose = TRUE,
      footer = NULL
    ))
    
    df_before <- df_copy()  # Store the original dataset
    df <- df_copy()
    
    # Track changes for each column
    column_changes <- list()  # List to track changes at the column level
    
    tryCatch({
      # Step 1: Clean names
      df <- clean_names(df)
      modified_cols <- colnames(df)[!colnames(df) %in% colnames(df_before)]
      if (length(modified_cols) > 0) {
        for (col in modified_cols) {
          column_changes[[col]] <- c(column_changes[[col]], "Column name was cleaned")
        }
      }
      
      # Step 2: Remove empty rows and columns
      df_before_empty <- df
      df <- df %>% remove_empty(c("rows", "cols"))
      modified_cols <- setdiff(colnames(df_before_empty), colnames(df))
      if (length(modified_cols) > 0) {
        for (col in modified_cols) {
          column_changes[[col]] <- c(column_changes[[col]], "Empty rows and/or columns were removed")
        }
      }
      
      # Step 3: Replace commas with periods
      df_before_commas <- df
      for (i in seq_along(df)) {
        if (is.character(df[[i]])) {
          original_col <- df[[i]]
          df[[i]] <- gsub(",", ".", df[[i]])
          if (!identical(original_col, df[[i]])) {
            col_name <- colnames(df)[i]
            column_changes[[col_name]] <- c(column_changes[[col_name]], "Commas replaced with periods")
          }
        }
      }
      
      # Step 4: Convert columns based on content
      df_before_convert <- df
      for (i in seq_along(df)) {
        original_col <- df[[i]]
        col_name <- colnames(df)[i]
        column <- as.character(df[[i]])
        num_count <- sum(grepl("^-?\\d+\\.?\\d*$", column))
        column <- gsub(",", ".", column)
        
        if (num_count / length(column) >= 0.5) {
          column <- gsub("[^0-9.-]+", "", column)
          column <- as.numeric(column)
        } else {
          column <- as.factor(column)
        }
        
        if (!identical(original_col, column)) {
          df[[i]] <- column
          column_changes[[col_name]] <- c(column_changes[[col_name]], "Column content was converted based on type")
        }
      }
      
      # Step 5: Combine similar levels in factor columns
      df_before_levels <- df
      for (i in seq_along(df)) {
        original_col <- df[[i]]
        col_name <- colnames(df)[i]
        if (!is.numeric(df[[i]])) {
          levels_char <- as.character(df[[i]])
          level_counts <- table(tolower(levels_char))
          similar_levels <- lapply(names(level_counts), function(level) {
            matched_levels <- levels_char[tolower(levels_char) == tolower(level)]
            return(unique(matched_levels))
          })
          for (sim_levels in similar_levels) {
            if (length(sim_levels) > 1) {
              most_frequent_level <- head(sim_levels[which.max(table(tolower(sim_levels)))], 1)
              levels_char[levels_char %in% sim_levels] <- most_frequent_level
            }
          }
          df[[i]] <- factor(levels_char, levels = unique(levels_char))
        }
        
        if (!identical(original_col, df[[i]])) {
          column_changes[[col_name]] <- c(column_changes[[col_name]], "Similar levels in factor columns were combined")
        }
      }
      
      # Step 6: Handle date columns
      df_before_dates <- df
      for (i in seq_along(df)) {
        original_col <- df[[i]]
        col_name <- colnames(df)[i]
        if (sum(grepl("\\d{2}/\\d{2}/\\d{4}|\\d{4}/\\d{2}/\\d{2}|\\d{4}-\\d{2}-\\d{2}|\\d{2}-\\d{2}-\\d{4}", df[[i]])) / length(df[[i]]) > 0.5) {
          column <- as.Date(df[[i]], format = "%Y-%m-%d")
          if (!identical(original_col, column)) {
            df[[i]] <- column
            column_changes[[col_name]] <- c(column_changes[[col_name]], "Date columns were detected and converted")
          }
        }
      }
      
      # Step 7: Remove currency symbols
      df_before_currency <- df
      for (i in seq_along(df)) {
        original_col <- df[[i]]
        col_name <- colnames(df)[i]
        if (any(grepl("[\\$€¥£]", df[[i]]))) {
          column <- as.numeric(gsub("[^0-9.-]", "", df[[i]]))
          if (!identical(original_col, column)) {
            df[[i]] <- column
            column_changes[[col_name]] <- c(column_changes[[col_name]], "Currency symbols were removed")
          }
        }
      }
      
      # Step 8: Remove constant columns
      df_before_constants <- df
      df <- df %>% remove_constant()
      removed_cols <- setdiff(colnames(df_before_constants), colnames(df))
      if (length(removed_cols) > 0) {
        for (col in removed_cols) {
          column_changes[[col]] <- c(column_changes[[col]], "Constant columns were removed")
        }
      }
      
      # Step 9: Convert remaining character variables to factors
      df_before_factors <- df
      for (i in seq_along(df)) {
        if (is.character(df[[i]])) {
          original_col <- df[[i]]
          df[[i]] <- as.factor(df[[i]])
          col_name <- colnames(df)[i]
          if (!identical(original_col, df[[i]])) {
            column_changes[[col_name]] <- c(column_changes[[col_name]], "Character columns were converted to factors")
          }
        }
      }
      
      # Step 10: Convert binary dummy variables to factors
      df_before_binaries <- df
      for (i in seq_along(df)) {
        if (is.numeric(df[[i]]) && all(df[[i]] %in% c(0, 1), na.rm = TRUE)) {
          original_col <- df[[i]]
          df[[i]] <- as.factor(df[[i]])
          col_name <- colnames(df)[i]
          if (!identical(original_col, df[[i]])) {
            column_changes[[col_name]] <- c(column_changes[[col_name]], "Binary numeric columns (0/1) were converted to factors")
          }
        }
      }
      
      # Save the cleaned data
      df_copy(df)
      push_to_history(df)  # Save the new version to history
      
      # Update data table
      output$data_table <- DT::renderDataTable(DT::datatable(df))
      
      # Create explanation text with HTML formatting
      explanation_text <- "<strong>Automated Cleaning has modified the following columns:</strong><br><br>"
      
      # Generate the explanation text
      for (col in names(column_changes)) {
        changes <- paste(column_changes[[col]], collapse = ", ")
        explanation_text <- paste0(explanation_text, "<strong>", col, ":</strong> ", changes, ".<br><br>")
      }
      
      explanation_text(HTML(explanation_text))
      text <- automated_modifications_text(HTML(explanation_text))
      
      
      
      output$image <- renderUI({
        NULL  # No image, just text
      })
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        HTML(paste(
          "Automated Cleaning Process stopped due to the following error. Please clean your data using the interactive choices in the sidepanel.<br>",
          "An error occurred: ", e$message
        )),        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # Helper function to return a description based on the change type
  change_type_description <- function(change_type) {
    switch(change_type,
           "clean_names" = "Column names were cleaned",
           "empty_removed" = "Empty rows and/or columns were removed",
           "converted" = "Column content was converted based on type",
           "combined_levels" = "Similar levels in factor columns were combined",
           "converted_date" = "Date columns were detected and converted",
           "removed_currency" = "Currency symbols were removed",
           "removed_constants" = "Constant columns were removed",
           "converted_to_factor" = "Character columns were converted to factors",
           "converted_binary_to_factor" = "Binary numeric columns (0/1) were converted to factors",
           "Unknown modification"
    )
  }
  
  # Reset to Initial Data
  observeEvent(input$reset, {
    df_copy(df_initial())  # Reset the current data to the initial data
    output$data_table <- DT::renderDataTable(DT::datatable(df_initial()))
  })
  
  observeEvent(input$undo, {
    df_previous <- pop_from_history()  # Get the previous state
    if (!is.null(df_previous)) {
      df_copy(df_previous)  # Revert to the previous state
      output$data_table <- DT::renderDataTable(DT::datatable(df_previous))
    } else {
      showModal(modalDialog(
        title = "Warning",
        "No more changes to undo.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  # Interactive Cleaning
  output$manipulate_options <- renderUI({
    if (!is.null(df_copy())) {
      tagList(
        hr(),
        h3("Interactive Cleaning"),
        h5("Cleaning options for single variable"),
        selectInput("change_var", "Enter the name of the variable you want to change:", choices = colnames(df_copy()), selected = colnames(df_copy())[1]),
        selectInput("manipulate_choice", "What would you like to do with this variable?",
                    choices = list("-","Separate it into 2 variables", "Change its type", "Remove it")),
        conditionalPanel(
          condition = "input.manipulate_choice == '-'"
        ),
        conditionalPanel(
          condition = "input.manipulate_choice == 'Separate it into 2 variables'",
          textInput("separator", "Please type your separator in this variable:", value = ""),
          textInput("col1", "Please type the name of the 1st new variable created:", value = ""),
          textInput("col2", "Please type the name of the 2nd new variable created:", value = "")
        ),
        conditionalPanel(
          condition = "input.manipulate_choice == 'Change its type'",
          selectInput("new_type", "What type would you like to make this variable?",
                      choices = list("As numeric", "As factor", "As Date"))
        )
      )
    }
  })
  
  output$further_options <- renderUI({
    if (!is.null(df_copy())) {
      tagList(
        p(""),
        h5("Cleaning options for multiple variables"),
        selectInput("further_choice", "Choose an action:",
                    choices = list("-", "Impute missing values", "Detect and remove duplicates", "Gather variables", "Spread variables", "Unite variables")),
        conditionalPanel(
          condition = "input.further_choice == '-'"
        ),
        conditionalPanel(
          condition = "input.further_choice == 'Gather variables' || input.further_choice == 'Spread variables' || input.further_choice == 'Unite variables'",
          selectInput("var1", "Enter the name of the 1st variable:", choices = colnames(df_copy())),
          selectInput("var2", "Enter the name of the 2nd variable:", choices = colnames(df_copy()))
        ),
        conditionalPanel(
          condition = "input.further_choice == 'Gather variables'",
          textInput("col", "Please type the name of the new variable created:", value = ""),
          textInput("value", "Please type what the numbers represent in these 2 variables:", value = "")
        ),
        conditionalPanel(
          condition = "input.further_choice == 'Spread variables'",
          textInput("key", "Please type the name of the variable which values will be used as the new variables names:", value = ""),
          textInput("value", "Please type the name of the variable which values will be used as the values of the new variables:", value = "")
        ),
        conditionalPanel(
          condition = "input.further_choice == 'Unite variables'",
          textInput("unite_sep", "Please type the unite string between these 2 variables:", value = ""),
          textInput("unite_name", "Please type the name of the new united variable:", value = ""),
          selectInput("unite_type", "Save the new variable as:",
                      choices = list("Numeric", "Factor"))
        )
      )
    }
  })
  
  
  observeEvent(input$apply_single, {
    df <- df_copy()
    
    # Save the state before changes
    push_to_history(df)
    
    tryCatch({
      if (input$manipulate_choice == "Separate it into 2 variables") {
        df <- separate(df, input$change_var, into = c(input$col1, input$col2), sep = input$separator)
      } else if (input$manipulate_choice == "Change its type") {
        if (input$new_type == "As numeric") {
          df[[input$change_var]] <- as.numeric(df[[input$change_var]])
        } else if (input$new_type == "As factor") {
          df[[input$change_var]] <- as.factor(df[[input$change_var]])
        } else if (input$new_type == "As Date") {
          df[[input$change_var]] <- convert_to_date(df[[input$change_var]])
        }
      } else if (input$manipulate_choice == "Remove it") {
        df <- df[, !(colnames(df) %in% input$change_var)]
      }
      
      df_copy(df)
      push_to_history(df)  # Save the new version to history
      
      output$data_table <- DT::renderDataTable(DT::datatable(df))
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  observeEvent(input$apply_additional, {
    df <- df_copy()
    
    # Save the state before changes
    push_to_history(df)
    
    tryCatch({
      
      if (input$further_choice == "Impute missing values") {
        
        showModal(modalDialog(
          title = "Warning",
          "This procedure might take a while, please wait.",
          easyClose = TRUE,
          footer = NULL
        ))
        
        
        
        imp.data <- mice(data = df, m = ceiling((sum(is.na(df)) / (nrow(df) * ncol(df)) * 100)),
                         maxit = 10, seed = 12345, method = "cart", print = FALSE)
        df <- complete(imp.data, 1)
      } else if (input$further_choice == "Detect and remove duplicates") {
        df <- df %>% distinct()
      } else if (input$further_choice == "Gather variables") {
        df <- df %>%
          gather(
            !!input$var1, 
            !!input$var2, 
            key = !!input$col, 
            value = !!input$value
          )      } else if (input$further_choice == "Spread variables") {
        df <- df %>% spread(key = input$key, value = input$value)
      } else if (input$further_choice == "Unite variables") {
        new_var <- paste(df[[input$var1]], df[[input$var2]], sep = input$unite_sep)
        if (input$unite_type == "Numeric") {
          if (any(grepl("[^0-9.]", new_var))) {
            showModal(modalDialog(
              title = "Warning",
              "The new variable contains letters, cannot convert to numeric. The new variable is saved as factor",
              easyClose = TRUE,
              footer = NULL
            ))
            new_var <- as.factor(new_var)
          } else {
            new_var <- as.numeric(new_var)
          }
        } else if (input$unite_type == "Factor") {
          new_var <- as.factor(new_var)
        }
        df[[input$unite_name]] <- new_var
      }
      
      
      
      df_copy(df)
      push_to_history(df)  # Save the new version to history
      
      output$data_table <- DT::renderDataTable(DT::datatable(df))
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  
  # Download handler for the finalized data
  output$download_final_data <- downloadHandler(
    filename = function() {
      paste("finalized_dataset", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- df_copy()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Define a reactive value to track the current explanation
  explanation_text <- reactiveVal(HTML(paste("
<strong>Automated Cleaning helps in automatically identifying and removing inconsistencies in the data, like correcting data types, removing empty rows/columns, and dealing with date formats.</strong><br><br>
If you click Automated Cleaning button, the algorithm will scan your data and proceed to some of the following modifications:
<br>
<ul>
  <li><strong>Column Names Were Cleaned:</strong> This step standardizes column names by removing special characters, converting to lowercase, and replacing spaces with underscores. It ensures that column names are consistent and easier to work with in analyses.</li>
  <li><strong>Empty Rows and/or Columns Were Removed:</strong> This operation removes any rows or columns that are entirely empty (i.e., contain no data). This helps in cleaning up the dataset by eliminating unnecessary or irrelevant data, which can simplify subsequent analysis.</li>
  <li><strong>Commas Replaced with Periods:</strong> This modification replaces commas with periods in character columns. This is often necessary for consistency in numeric data, especially if the dataset uses commas as decimal separators, which can cause issues in numeric operations.</li>
  <li><strong>Column Content Was Converted Based on Type:</strong> Columns are converted to appropriate data types based on their content. For example, columns with mostly numeric values (after replacing commas) are converted to numeric type, while those with non-numeric values are converted to factors (categorical data). This ensures that each column has the correct data type for analysis.</li>
  <li><strong>Similar Levels in Factor Columns Were Combined:</strong> This step merges similar levels in factor columns. For instance, if a factor variable has levels like 'yes', 'Yes', and 'YES', this operation consolidates them into a single level to reduce redundancy and make the data more consistent.</li>
  <li><strong>Date Columns Were Detected and Converted:</strong> Columns that contain date-like data are converted to Date type. This allows for proper handling of dates in subsequent analyses, enabling date-specific operations such as sorting and filtering.</li>
  <li><strong>Currency Symbols Were Removed:</strong> This operation removes currency symbols (such as $, €, ¥) from numeric columns and converts the remaining values to numeric type. This is crucial for accurate numerical computations, especially if currency symbols are mixed with numbers.</li>
  <li><strong>Constant Columns Were Removed:</strong> Columns with constant values (where all entries are the same) are removed from the dataset. These columns do not provide any variability or useful information for analysis and can be safely discarded.</li>
  <li><strong>Character Columns Were Converted to Factors:</strong> Character columns (text data) are converted to factors. Factors are categorical variables in R that are used for statistical modeling. This conversion is useful for handling categorical data efficiently in analyses.</li>
  <li><strong>Binary Numeric Columns (0/1) Were Converted to Factors:</strong> Numeric columns that contain only binary values (0 and 1) are converted to factors. This is often done to treat binary data as categorical variables, which can be more informative for certain types of analyses.</li>
</ul>
")))
  
  
  
  observeEvent(input$manipulate_choice, {
    if (input$manipulate_choice == "Separate it into 2 variables") {
      explanation_text(HTML(paste("The <strong>Separate it into 2 variables</strong> option allows you to split a single column into two separate columns based on a specified separator.")))
      output$image <- renderUI({
        img(src = "separate.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$manipulate_choice == "Change its type") {
      explanation_text(HTML(paste("The <strong>Change its type</strong> option lets you convert a variable to a different data type, such as numeric, factor, or date.")))
      output$image <- renderUI({
        img(src = "Change_type.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$manipulate_choice == "Remove it") {
      explanation_text(HTML(paste("The <strong>Remove it</strong> option allows you to delete a specific variable from your dataset.")))
      output$image <- renderUI({
        img(src = "Delete.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    }
  })
  
  observeEvent(input$further_choice, {
    if (input$further_choice == "Impute missing values") {
      explanation_text(HTML(paste("The <strong>Impute missing values</strong> option estimates and fills in missing data based on patterns found in the complete cases.")))
      output$image <- renderUI({
        img(src = "impute_missing_values.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$further_choice == "Detect and remove duplicates") {
      explanation_text(HTML(paste("The <strong>Detect and remove duplicates</strong> option identifies and removes any duplicated rows in your dataset.")))
      output$image <- renderUI({
        img(src = "duplicates.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$further_choice == "Gather variables") {
      explanation_text(HTML(paste("The <strong>Gather variables</strong> function reshapes data from wide to long format, combining multiple columns into a single column with key-value pairs.")))
      output$image <- renderUI({
        img(src = "gather.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$further_choice == "Spread variables") {
      explanation_text(HTML(paste("The <strong>Spread variables</strong> function reshapes data from long to wide format, spreading key-value pairs across multiple columns.")))
      output$image <- renderUI({
        img(src = "spread.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    } else if (input$further_choice == "Unite variables") {
      explanation_text(HTML(paste("The <strong>Unite variables</strong> function combines two or more columns into a single column, often using a specified separator.")))
      output$image <- renderUI({
        img(src = "unite.png", height = "300px", width = "600px",
            style = "display: block; margin-left: auto; margin-right: auto;")
      })
    }
  })
  
  # Render the explanation text in a card
  output$explanation_card <- renderUI({
    card( 
      card_header("Explanation"),
      card_body(
        p(explanation_text()),
        uiOutput("image"),
        # Flex container to hold the select input and apply button next to each other on the left
        div(
          style = "display: flex; align-items: center; gap: 10px;",  # Container for alignment and spacing
          selectInput(
            inputId = "explanation_choice",
            label = "See again:",
            choices = c(
              "Automated Cleaning Info" = "cleaning_info",
              "Automated Cleaning Modifications" = "cleaning_modifications",
              "Remove Function Description" = "remove_function",
              "Separate Function Description" = "separate_function",
              "Change Type Function Description" = "change_type_function",
              "Impute Missing Values Description" = "impute_missing",
              "Detect and Remove Duplicates Description" = "remove_duplicates",
              "Gather Variables Description" = "gather_variables",
              "Spread Variables Description" = "spread_variables",
              "Unite Variables Description" = "unite_variables"
            ),
            selected = "cleaning_info",
            width = "250px"  # Adjust width as needed
          ),
          actionButton(
            inputId = "apply_info", 
            label = "Apply",
            style = "margin-left: 0;"  # Remove any left margin that may push it away
          )
        )
      )
    )
  })
  
  # Observe event for apply button
  observeEvent(input$apply_info, {
    selected_info <- input$explanation_choice
    
    # Define the content and images for each information type
    info_content <- list(
      cleaning_info = list(
        text = HTML(paste("
<strong>Automated Cleaning helps in automatically identifying and removing inconsistencies in the data, like correcting data types, removing empty rows/columns, and dealing with date formats.</strong><br><br>
If you click Automated Cleaning button, the algorithm will scan your data and proceed to some of the following modifications:
<br>
<ul>
  <li><strong>Column Names Were Cleaned:</strong> This step standardizes column names by removing special characters, converting to lowercase, and replacing spaces with underscores. It ensures that column names are consistent and easier to work with in analyses.</li>
  <li><strong>Empty Rows and/or Columns Were Removed:</strong> This operation removes any rows or columns that are entirely empty (i.e., contain no data). This helps in cleaning up the dataset by eliminating unnecessary or irrelevant data, which can simplify subsequent analysis.</li>
  <li><strong>Commas Replaced with Periods:</strong> This modification replaces commas with periods in character columns. This is often necessary for consistency in numeric data, especially if the dataset uses commas as decimal separators, which can cause issues in numeric operations.</li>
  <li><strong>Column Content Was Converted Based on Type:</strong> Columns are converted to appropriate data types based on their content. For example, columns with mostly numeric values (after replacing commas) are converted to numeric type, while those with non-numeric values are converted to factors (categorical data). This ensures that each column has the correct data type for analysis.</li>
  <li><strong>Similar Levels in Factor Columns Were Combined:</strong> This step merges similar levels in factor columns. For instance, if a factor variable has levels like 'yes', 'Yes', and 'YES', this operation consolidates them into a single level to reduce redundancy and make the data more consistent.</li>
  <li><strong>Date Columns Were Detected and Converted:</strong> Columns that contain date-like data are converted to Date type. This allows for proper handling of dates in subsequent analyses, enabling date-specific operations such as sorting and filtering.</li>
  <li><strong>Currency Symbols Were Removed:</strong> This operation removes currency symbols (such as $, €, ¥) from numeric columns and converts the remaining values to numeric type. This is crucial for accurate numerical computations, especially if currency symbols are mixed with numbers.</li>
  <li><strong>Constant Columns Were Removed:</strong> Columns with constant values (where all entries are the same) are removed from the dataset. These columns do not provide any variability or useful information for analysis and can be safely discarded.</li>
  <li><strong>Character Columns Were Converted to Factors:</strong> Character columns (text data) are converted to factors. Factors are categorical variables in R that are used for statistical modeling. This conversion is useful for handling categorical data efficiently in analyses.</li>
  <li><strong>Binary Numeric Columns (0/1) Were Converted to Factors:</strong> Numeric columns that contain only binary values (0 and 1) are converted to factors. This is often done to treat binary data as categorical variables, which can be more informative for certain types of analyses.</li>
</ul>
")),
        image = NULL
      ),
      cleaning_modifications = list(
        text = automated_modifications_text(),  # Uses the existing explanation_text reactive value
        image = NULL
      ),
      remove_function = list(
        text = HTML(paste("The <strong>Remove it</strong> option allows you to delete a specific variable from your dataset.")),
        image = img(src = "Delete.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      separate_function = list(
        text = HTML(paste("The <strong>Separate it into 2 variables</strong> option allows you to split a single column into two separate columns based on a specified separator.")),
        image = img(src = "separate.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      change_type_function = list(
        text = HTML(paste("The <strong>Change its type</strong> option lets you convert a variable to a different data type, such as numeric, factor, or date.")),
        image = img(src = "Change_type.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      impute_missing = list(
        text = HTML(paste("The <strong>Impute missing values</strong> option estimates and fills in missing data based on patterns found in the complete cases.")),
        image = img(src = "impute_missing_values.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      remove_duplicates = list(
        text = HTML(paste("The <strong>Detect and remove duplicates</strong> option identifies and removes any duplicated rows in your dataset.")),
        image = img(src = "duplicates.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      gather_variables = list(
        text = HTML(paste("The <strong>Gather variables</strong> function reshapes data from wide to long format, combining multiple columns into a single column with key-value pairs.")),
        image = img(src = "gather.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      spread_variables = list(
        text = HTML(paste("The <strong>Spread variables</strong> function reshapes data from long to wide format, spreading key-value pairs across multiple columns.")),
        image = img(src = "spread.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      unite_variables = list(
        text = HTML(paste("The <strong>Unite variables</strong> function combines two or more columns into a single column, often using a specified separator.")),
        image = img(src = "unite.png", height = "300px", width = "600px",
                    style = "display: block; margin-left: auto; margin-right: auto;")
      )
    )
    
    # Get the selected content and image
    selected_content <- info_content[[selected_info]]
    
    # Update the Automated Cleaning Modifications explanation if the user selects it
    if (selected_info == "cleaning_modifications") {
      explanation_text(explanation_text())
    }
    
    # Display the selected information in a modal dialog
    showModal(modalDialog(
      title = "Information",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      tagList(
        selected_content$text,
        selected_content$image
      )
    ))
  })
  
  ##############################################################################
  
  # Data Descriptive Characteristics Phase
  observeEvent(input$summary_choice, {
    if (input$summary_choice == TRUE) {
      df <- df_copy()
      output$data_str <- renderPrint({ str(df) })
      output$data_summary <- renderPrint({ summary(df) })
    } else {
      output$data_str <- NULL
      output$data_summary <- NULL
    }
  })
  
  # Update the choices for the variable select inputs
  observe({
    df <- df_copy()  
    updateSelectInput(session, "test_var", choices = colnames(df_copy()))
    updateSelectInput(session, "test_var1", choices = colnames(df_copy()))
    updateSelectInput(session, "test_var2", choices = colnames(df_copy()))
    updateSelectInput(session, "dependent_vars", choices = colnames(df_copy()))
    updateSelectInput(session, "independent_vars", choices = colnames(df_copy()))
  })
  
  # Helper function to check if a column is numeric
  isNumeric <- function(column) {
    is.numeric(df_copy()[[column]])
  }
  
  # UI Output for  test options
  output$test_options <- renderUI({
    df <- df_copy()
    req(input$test_var)  # Require that input$test_var is not NULL
    
    # Extract the selected variable data
    var_data <- df[[input$test_var]]
    
    
    if (isNumeric(input$test_var)) {
      tagList(
        selectInput("numeric_test_type", "Select Numeric Test Type:", choices = c("Normality Test", "Test Against a Value")),
        conditionalPanel(
          condition = "input.numeric_test_type == 'Test Against a Value'",
          numericInput("test_value", "Enter Test Value:", value = 0),
          radioButtons("comparison_operator", "Select Comparison Operator:", choices = c("≠ Test Value" = "neq", "< Test Value" = "lt", "> Test Value" = "gt"))
        )
      )
    } else if (is.factor(df_copy()[[input$test_var]])) {
      tagList(
        selectInput("factor_test_type", "Select Factor Test Type:", choices = c("Chi-Squared Test", "One-Sample Proportion Test")),
        conditionalPanel(
          condition = "input.factor_test_type == 'One-Sample Proportion Test'",
          numericInput("test_value", "Enter Test Value:", value = 0.5),
          radioButtons("comparison_operator", "Select Comparison Operator:", choices = c("≠ Test Value" = "neq", "< Test Value" = "lt", "> Test Value" = "gt"))
        )
      )
    } 
  })
  
  # UI Output for  test type options between two variables
  output$test_type_options <- renderUI({
    df <- df_copy()
    req(input$test_var1, input$test_var2)
    
    var_data1 <- df[[input$test_var1]]
    var_data2 <- df[[input$test_var2]]
    
    if (is.numeric(var_data1) && is.numeric(var_data2)) {
      selectInput("two_var_test", "Select Test Type:", choices = c("Mean Comparison", "SD Comparison", "Pearson Correlation Coefficient", "Spearman's Rank Correlation Coefficient", "Kendall's Tau"))
    } else if (is.factor(var_data1) && is.factor(var_data2)) {
      selectInput("two_var_test", "Select Test Type:", choices = c("Independence Test", "McNemar's Test", "Cramer's V", "Fisher's Exact Test"))
    } else {
      # Hide the test type selection if variable types are mixed
      NULL
    }
  })
  
  # Error message if two variables are mixed types
  output$two_var_error_message <- renderText({
    req(input$test_var1, input$test_var2)
    if ((isNumeric(input$test_var1) && is.factor(df_copy()[[input$test_var2]])) ||
        (is.factor(df_copy()[[input$test_var1]]) && isNumeric(df_copy()[[input$test_var2]]))) {
      "Error: Please select either two numeric variables or two factor variables."
    } else {
      ""
    }
  })
  
  # Helper function to check if a column is a factor with exactly two levels
  isTwoLevelFactor <- function(column) {
    is.factor(column) && length(levels(column)) == 2
  }
  
  
  # Provide explanations for the chosen test
  output$test_explanation <- renderUI({
    if (input$test_mode == "Single Variable" && !is.null(input$test_var) && input$test_var != "") {
      var_data <- df_copy()[[input$test_var]]
      if (is.numeric(var_data)) {
        if (input$numeric_test_type == "Normality Test") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The Kolmogorov-Smirnov test is a non-parametric test that assesses whether a sample follows a specific distribution, in this case, a normal distribution. The test compares the empirical distribution of the sample data to the cumulative distribution function (CDF) of a normal distribution. If the <b>p-value</b> is greater than 0.05, we fail to reject the null hypothesis, meaning the data does not significantly deviate from normality. However, if the <b>p-value</b> is less than 0.05, it suggests that the data deviates significantly from a normal distribution, indicating non-normality.</li>",
                     "<li><b>Plot Interpretation:</b> The Q-Q plot visually compares the sample data to a normal distribution. If the data points fall along the red line, it suggests that the data follows a normal distribution. Deviations from this line indicate departures from normality, which should be considered when interpreting the Kolmogorov-Smirnov test results.</li>",
                     "</ul>"))
        } else if (input$numeric_test_type == "Test Against a Value") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The one-sample t-test compares the sample mean of a numerical variable to a specified value. This test evaluates whether the sample mean is significantly different from, less than, or greater than the specified value based on the chosen comparison operator. The <b>null hypothesis</b> states that there is no difference between the sample mean and the specified value. A <b>p-value</b> less than 0.05 suggests rejecting the null hypothesis, indicating that the sample mean significantly differs from the test value.</li>",
                     "<li><b>Plot Interpretation:</b> The histogram shows the distribution of the sample data, and the red dashed line represents the specified test value. This plot helps visualize the location of the sample mean relative to the test value.</li>",
                     "</ul>"))
        } else {
          HTML(paste("Please select a valid numeric test type."))
        }
      } else if (is.factor(var_data)) {
        if (input$factor_test_type == "Chi-Squared Test") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The Chi-Squared test assesses the independence or association between categorical variables. It tests whether the observed frequencies in each category differ significantly from the expected frequencies under the <b>null hypothesis</b> of independence. A <b>p-value</b> less than 0.05 indicates a significant association between the categorical variables.</li>",
                     "<li><b>Plot Interpretation:</b> The plot visualizes the distribution of the observed versus expected frequencies for each category. Larger deviations indicate a stronger association between categories, which should be considered in conjunction with the test result.</li>",
                     "</ul>"))
        } else if (input$factor_test_type == "One-Sample Proportion Test") {
          if (isTwoLevelFactor(var_data)) {
            HTML(paste("<ul>",
                       "<li><b>Explanation:</b> The one-sample proportion test evaluates whether the proportion of a specific category in a binary (two-level) factor variable significantly differs from a specified proportion. A <b>p-value</b> less than 0.05 indicates a significant difference between the observed proportion and the test value.</li>",
                       "<li><b>Plot Interpretation:</b> The bar plot shows the observed frequencies for each category, with a red dashed line representing the expected proportion under the null hypothesis.</li>",
                       "</ul>"))
          } else {
            HTML(paste("The One-Sample Proportion Test can only be conducted for a factor variable with exactly two levels."))
          }
        } else {
          HTML(paste("Please select a valid factor test type."))
        }
      } else {
        HTML(paste("This test is applicable only for numeric or categorical data."))
      }
    } else if (input$test_mode == "Between Two Variables" && !is.null(input$test_var1) && !is.null(input$test_var2)) {
      var_data1 <- df_copy()[[input$test_var1]]
      var_data2 <- df_copy()[[input$test_var2]]
      
      if (is.numeric(var_data1) && is.numeric(var_data2)) {
        if (input$two_var_test == "Mean Comparison") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The two-sample t-test compares the means of two numerical variables or groups. The <b>null hypothesis</b> states that the means of the two groups are equal. A <b>p-value</b> less than 0.05 suggests rejecting the null hypothesis, indicating a significant difference in means between the two groups.</li>",
                     "</ul>"))
        } else if (input$two_var_test == "SD Comparison") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The variance test compares the variability (standard deviations) of two numerical variables. A <b>p-value</b> less than 0.05 suggests that one group has significantly more variability than the other.</li>",
                     "</ul>"))
        } else if (input$two_var_test == "Pearson Correlation Coefficient") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The Pearson correlation coefficient measures the strength and direction of the linear relationship between two numerical variables. A <b>p-value</b> less than 0.05 indicates that the correlation is statistically significant.</li>",
                     "</ul>"))
        } else if (input$two_var_test == "Spearman's Rank Correlation Coefficient") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> Spearman's rank correlation coefficient measures the strength and direction of the monotonic relationship between two ranked variables. A <b>p-value</b> less than 0.05 indicates a significant monotonic relationship.</li>",                     "</ul>"))
        } else if (input$two_var_test == "Kendall's Tau") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> Kendall’s tau measures the strength of the association between two variables based on concordance and discordance. A <b>p-value</b> less than 0.05 indicates a significant association between the two variables.</li>",                     "</ul>"))
        } else {
          HTML(paste("Please select a valid test type for numerical variables."))
        }
      } else if (is.factor(var_data1) && is.factor(var_data2)) {
        if (input$two_var_test == "Independence Test") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> The Chi-square test of independence tests whether there is a significant association between two categorical variables. A <b>p-value</b> less than 0.05 suggests a significant association between the variables.</li>",                     "</ul>"))
        } else if (input$two_var_test == "McNemar's Test") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> McNemar's test is a non-parametric test used to analyze paired nominal data. It is primarily applied to 2x2 contingency tables to determine if there are significant differences in proportions between two related groups. This test is typically used when analyzing before-and-after situations or matched pairs. The <b>null hypothesis</b> posits that there is no difference in the proportions of the two related groups. A <b>p-value</b> less than 0.05 suggests a significant change in the proportions between the paired groups.</li>",                     "</ul>"))
        } else if (input$two_var_test == "Cramer's V") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> Cramer's V is a measure of the strength of association between two categorical variables. It is based on the Chi-square statistic and provides a value between 0 and 1, where 0 indicates no association and 1 indicates a perfect association. Cramer's V is particularly useful for larger contingency tables with more than two categories. The <b>null hypothesis</b> assumes no association between the variables. A larger Cramer's V value suggests a stronger relationship between the variables.</li>",                     "</ul>"))
        } else if (input$two_var_test == "Fisher's Exact Test") {
          HTML(paste("<ul>",
                     "<li><b>Explanation:</b> Fisher’s Exact Test evaluates the association between two categorical variables, typically in a 2x2 contingency table. A <b>p-value</b> less than 0.05 indicates a significant association between the variables.</li>",                     "</ul>"))
        } else {
          HTML(paste("Please select a valid test type for categorical variables."))
        }
      } else {
        HTML(paste("Please select either two numerical variables or two categorical variables for comparison."))
      }
    }  else {
      HTML(paste("Select variables and a test type to see the description."))
    }
  })
  

  
  
  # Perform the appropriate test based on the selected variable type
  observeEvent(input$test_apply, {
    df <- df_copy()
    
    
    if (input$test_mode == "Single Variable" && !is.null(input$test_var) && input$test_var != "") {
      var_data <- df[[input$test_var]]
      
      if (is.numeric(var_data)) {
        # Determine which test to run for numeric data
        if (input$numeric_test_type == "Normality Test") {
          # Kolmogorov-Smirnov test for normality
          ks_result <- ks.test(var_data, "pnorm")
          output$test_summary <- renderPrint({
            print(ks_result)
          })
          output$test_plot <- renderPlot({
            ggplot(data.frame(x = var_data), aes(sample = x)) +
              stat_qq(color = "blue") +
              stat_qq_line(color = "red") +
              ggtitle("Q-Q Plot for Kolmogorov-Smirnov Test")
          })
        } else if (input$numeric_test_type == "Test Against a Value") {
          test_value <- input$test_value
          comparison <- input$comparison_operator
          
          # Perform the appropriate test based on the selected comparison operator
          if (comparison == "neq") {
            t_result <- t.test(var_data, mu = test_value)
          } else if (comparison == "lt") {
            t_result <- t.test(var_data, mu = test_value, alternative = "less")
          } else if (comparison == "gt") {
            t_result <- t.test(var_data, mu = test_value, alternative = "greater")
          }
          
          output$test_summary <- renderPrint({
            print(t_result)
          })
          output$test_plot <- renderPlot({
            ggplot(data.frame(x = var_data), aes(x = x)) +
              geom_histogram(bins = 30, fill = "skyblue", color = "black") +
              geom_vline(xintercept = test_value, linetype = "dashed", color = "red") +
              ggtitle(paste("T-Test Against Value", test_value))
          })
        }
      } else if (is.factor(var_data)) {
        # Determine which test to run for numeric data
        if (input$factor_test_type == "Chi-Squared Test") {
          # Chi-square test for categorical variables
          chi_square_result <- chisq.test(table(var_data))
          output$test_summary <- renderPrint({
            print(chi_square_result)
          })
          output$test_plot <- renderPlot({
            ggchisqtest(chi_square_result, colaccept = "grey89" , colreject = "black")
          })
        } else if (input$factor_test_type == "One-Sample Proportion Test") {
          if (!isTwoLevelFactor(var_data)) {
            # Error message if factor has more than 2 levels
            output$test_summary <- renderPrint({
              "Error: One-Sample Proportion Test can only be conducted for a factor variable with exactly two levels."
            })
            output$test_plot <- NULL
          } else {
            # Perform the One-Sample Proportion Test for a factor with exactly two levels
            test_value <- input$test_value
            comparison <- input$comparison_operator
            
            # Create a table of the factor levels
            factor_table <- table(var_data)
            
            # Calculate the proportion of successes
            p_observed <- factor_table[2] / sum(factor_table)
            
            # Perform the proportion test
            if (comparison == "neq") {
              prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value)
            } else if (comparison == "lt") {
              prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value, alternative = "less")
            } else if (comparison == "gt") {
              prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value, alternative = "greater")
            }
            
            output$test_summary <- renderPrint({
              print(prop_result)
            })
            output$test_plot <- renderPlot({
              ggplot(data.frame(x = factor_table), aes(x = names(factor_table), y = factor_table)) +
                geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                geom_hline(yintercept = test_value * sum(factor_table), linetype = "dashed", color = "red") +
                ggtitle(paste("One-Sample Proportion Test Value", test_value))
            })
          }
        }
      } else {
        # Display message for other variable types
        output$test_summary <- renderPrint({
          print("Selected variable is neither numeric nor categorical.")
        })
        output$test_plot <- renderPlot({
          NULL
        })
      }
    } else if (input$test_mode == "Between Two Variables" && !is.null(input$test_var1) && !is.null(input$test_var2) && input$test_var1 != "" && input$test_var2 != "") {
      var_data1 <- df[[input$test_var1]]
      var_data2 <- df[[input$test_var2]]
      
      # Error message output initialization
      output$error_message <- renderText({ "" })
      
      if (is.numeric(var_data1) && is.numeric(var_data2)) {
        if (input$two_var_test == "Mean Comparison") {
          t_test_result <- t.test(var_data1, var_data2)
          output$test_summary <- renderPrint({ print(t_test_result) })
          output$test_plot <- renderPlot({
            NULL
          })
        } else if (input$two_var_test == "SD Comparison") {
          var_test_result <- var.test(var_data1, var_data2)
          output$test_summary <- renderPrint({ print(var_test_result) })
          output$test_plot <- renderPlot({
            NULL
          })
        } else if (input$two_var_test == "Pearson Correlation Coefficient") {
          pearson_result <- cor.test(var_data1, var_data2, method = "pearson")
          output$test_summary <- renderPrint({ print(pearson_result) })
          output$test_plot <- renderPlot({
            NULL
          })
        } else if (input$two_var_test == "Spearman's Rank Correlation Coefficient") {
          spearman_result <- cor.test(var_data1, var_data2, method = "spearman")
          output$test_summary <- renderPrint({ print(spearman_result) })
          output$test_plot <- renderPlot({
            NULL
          })
        } else if (input$two_var_test == "Kendall's Tau") {
          kendall_result <- cor.test(var_data1, var_data2, method = "kendall")
          output$test_summary <- renderPrint({ print(kendall_result) })
          output$test_plot <- renderPlot({
            NULL
          })
        } else {
          output$test_summary <- renderPrint({ "Please select a valid test type for numerical variables." })
          output$test_plot <- renderPlot({ NULL })
        }
      } else if (is.factor(var_data1) && is.factor(var_data2)) {
        if (input$two_var_test == "Independence Test") {
          if (length(unique(var_data1)) > 1 && length(unique(var_data2)) > 1) {
            chi_square_result <- chisq.test(table(var_data1, var_data2))
            output$test_summary <- renderPrint({ print(chi_square_result) })
            output$test_plot <- renderPlot({
              ggchisqtest(chi_square_result, colaccept = "grey89" , colreject = "black")
            })
          } else {
            output$error_message <- renderText({ "Independence Test requires more than one level for each factor variable." })
            output$test_summary <- renderPrint({ "Independence Test cannot be performed due to insufficient factor levels." })
            output$test_plot <- renderPlot({ NULL })
          }
        } else if (input$two_var_test == "McNemar's Test") {
          if (length(unique(var_data1)) == 2 && length(unique(var_data2)) == 2) {
            mcnemar_result <- mcnemar.test(table(var_data1, var_data2))
            output$test_summary <- renderPrint({ print(mcnemar_result) })
            output$test_plot <- renderPlot({
              NULL
            })
          } else {
            output$error_message <- renderText({ "McNemar's Test requires exactly two levels for each factor variable." })
            output$test_summary <- renderPrint({ "McNemar's Test cannot be performed due to factor level requirements." })
            output$test_plot <- renderPlot({ NULL })
          }
        } else if (input$two_var_test == "Cramer's V") {
          if (length(unique(var_data1)) > 1 && length(unique(var_data2)) > 1) {
            chi_square_result <- chisq.test(table(var_data1, var_data2))
            cramer_v <- sqrt(chi_square_result$statistic / (sum(table(var_data1, var_data2)) * (min(length(unique(var_data1)), length(unique(var_data2))) - 1)))
            output$test_summary <- renderPrint({ print(paste("Cramer's V:", cramer_v)) })
            output$test_plot <- renderPlot({
              NULL
            })
          } else {
            output$error_message <- renderText({ "Cramer's V requires more than one level for each factor variable." })
            output$test_summary <- renderPrint({ "Cramer's V cannot be performed due to insufficient factor levels." })
            output$test_plot <- renderPlot({ NULL })
          }
        } else if (input$two_var_test == "Fisher's Exact Test") {
          if (length(unique(var_data1)) <= 2 && length(unique(var_data2)) <= 2) {
            fisher_result <- fisher.test(table(var_data1, var_data2))
            output$test_summary <- renderPrint({ print(fisher_result) })
            output$test_plot <- renderPlot({
              NULL
            })
          } else {
            output$error_message <- renderText({ "Fisher's Exact Test requires exactly two levels for each factor variable." })
            output$test_summary <- renderPrint({ "Fisher's Exact Test cannot be performed due to factor level requirements." })
            output$test_plot <- renderPlot({ NULL })
          }
        } else {
          output$error_message <- renderText({ "Please select a valid test type for categorical variables." })
          output$test_summary <- renderPrint({ "Invalid test type for categorical variables." })
          output$test_plot <- renderPlot({ NULL })
        }
      } else {
        output$error_message <- renderText({ "Selected variables are of mixed types. Please ensure both variables are either numeric or categorical." })
        output$test_summary <- renderPrint({ "Selected variables are of mixed types." })
        output$test_plot <- renderPlot({ NULL })
      }
    } else if (input$test_mode == "ANOVA"){
      # Get the selected dependent and independent variables
      dep_vars <- input$dependent_vars
      ind_vars <- input$independent_vars
      
      # Provide explanations for the chosen test
      output$test_explanation <- renderText({
        if (input$test_mode == "ANOVA"){
          if(all(dep_type == "numeric") && length(dep_vars) == 1 && all(ind_type == "factor")) {
            HTML(paste("<ul>",
                       "<li><b>Explanation:</b> ANOVA compares the means of one or more groups. A significant <b>p-value</b> (less than 0.05) suggests that at least one group's mean is different from the others.</li>",
                       "<li><b>Plot Interpretation:</b> A boxplot shows the distribution of the dependent variable for each group. Significant differences between the positions of boxes suggest a significant difference in means between groups.</li>",
                       "</ul>"))
          } else if (all(dep_type == "numeric") && length(dep_vars) > 1 && all(ind_type == "factor")) {
            HTML(paste("<ul>",
                       "<li><b>Explanation:</b> MANOVA assesses the effect of one or more categorical independent variables on two or more dependent variables. A significant <b>p-value</b> (less than 0.05) suggests that there is a difference in the mean vectors across the groups.</li>",
                       "<li><b>Plot Interpretation:</b> A scatterplot matrix visualizes pairwise relationships between the dependent variables across groups, highlighting potential clusters or patterns.</li>",
                       "</ul>"))
          }
        } else {
          NULL
        }
        })
      
      # Check if the selection is valid
      if(length(dep_vars) == 0 || length(ind_vars) == 0) {
        output$test_summary <- renderText("Please select at least one dependent and one independent variable.")
        return()
      }
      
      # Check the type of variables (factor or numeric)
      dep_type <- sapply(df[, dep_vars], class)
      ind_type <- sapply(df[, ind_vars], class)
      
      # Determine the type of ANOVA to perform based on the selections
      if(all(dep_type == "numeric") && length(dep_vars) == 1 && all(ind_type == "factor")) {
        # Perform a one-way ANOVA
        formula <- as.formula(paste(dep_vars, "~", paste(ind_vars, collapse = "+")))
        anova_result <- aov(formula, data = df)
        output$test_summary <- renderPrint(summary(anova_result))
        
        # Plot the result
        output$test_plot <- renderPlot({
          ggplot(df, aes(x = df[[dep_vars]], y = df[[ind_vars]], fill = df[[ind_vars]])) +
            geom_boxplot() +
            scale_fill_viridis_d()+
            labs(title = "ANOVA Plot", x = dep_vars, y = ind_vars , fill = names(df[[ind_vars]]) )
        })
        
      } else if(all(dep_type == "numeric") && length(dep_vars) > 1 && all(ind_type == "factor")) {
        # Perform a MANOVA
        formula <- as.formula(paste("cbind(", paste(dep_vars, collapse = ","), ") ~", paste(ind_vars, collapse = "+")))
        manova_result <- manova(formula, data = df)
        output$test_summary <- renderPrint(summary(manova_result))
        
        # Show warning message for automated cleaning
        showModal(modalDialog(
          title = "Warning",
          "This procedure might take a while, please wait.",
          easyClose = TRUE,
          footer = NULL
        ))
        
        # Plot the result (e.g., using a scatterplot matrix)
        output$test_plot <- renderPlot({
          # ggpairs to plot relationships between dependent variables with respect to factor(s)
          ggpairs(
            df, 
            columns = which(names(df) %in% dep_vars),  # Columns with dependent variables
            aes(color = df[[ind_vars[1]]])  # Coloring based on the first independent factor
          ) +
            scale_color_viridis_d() +  # Apply Viridis color scale
            theme_minimal() +
            labs(title = "MANOVA Scatterplot Matrix by Factors")
        })
        
          
        
      } else {
        # If the selection cannot be used for ANOVA/MANOVA
        output$test_summary <- renderText("Invalid selection for ANOVA/MANOVA. Ensure that dependent variables are numeric and independent variables are factors.")
        output$test_plot <- renderPlot({
          NULL
        })
      }
    }
  })
  
  
  # Download handler for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("statistical_test_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Open the PNG device
      png(file, width = 800, height = 600)
      
      df <- df_copy()
      
      
      if (input$test_mode == "Single Variable" && !is.null(input$test_var) && input$test_var != "") {
        var_data <- df[[input$test_var]]
        
        if (is.numeric(var_data)) {
          # Determine which test to run for numeric data
          if (input$numeric_test_type == "Normality Test") {
            # Kolmogorov-Smirnov test for normality
            ks_result <- ks.test(var_data, "pnorm")
            plot <- ggplot(data.frame(x = var_data), aes(sample = x)) +
              stat_qq(color = "blue") +
              stat_qq_line(color = "red") +
              ggtitle("Q-Q Plot for Kolmogorov-Smirnov Test")
            print(plot)
          } else if (input$numeric_test_type == "Test Against a Value") {
            test_value <- input$test_value
            comparison <- input$comparison_operator
            
            # Perform the appropriate test based on the selected comparison operator
            if (comparison == "neq") {
              t_result <- t.test(var_data, mu = test_value)
            } else if (comparison == "lt") {
              t_result <- t.test(var_data, mu = test_value, alternative = "less")
            } else if (comparison == "gt") {
              t_result <- t.test(var_data, mu = test_value, alternative = "greater")
            }
            plot <- ggplot(data.frame(x = var_data), aes(x = x)) +
              geom_histogram(bins = 30, fill = "skyblue", color = "black") +
              geom_vline(xintercept = test_value, linetype = "dashed", color = "red") +
              ggtitle(paste("T-Test Against Value", test_value))
            print(plot)
          }
        } else if (is.factor(var_data)) {
          # Determine which test to run for numeric data
          if (input$factor_test_type == "Chi-Squared Test") {
            # Chi-square test for categorical variables
            chi_square_result <- chisq.test(table(var_data))
            plot <- ggchisqtest(chi_square_result, colaccept = "grey89" , colreject = "black")
            print(plot)
          } else if (input$factor_test_type == "One-Sample Proportion Test") {
            if (!isTwoLevelFactor(var_data)) {
              plot <- ggplot() + ggtitle("No plot available")
              print(plot)
            } else {
              # Perform the One-Sample Proportion Test for a factor with exactly two levels
              test_value <- input$test_value
              comparison <- input$comparison_operator
              
              # Create a table of the factor levels
              factor_table <- table(var_data)
              
              # Calculate the proportion of successes
              p_observed <- factor_table[2] / sum(factor_table)
              
              # Perform the proportion test
              if (comparison == "neq") {
                prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value)
              } else if (comparison == "lt") {
                prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value, alternative = "less")
              } else if (comparison == "gt") {
                prop_result <- prop.test(x = factor_table[2], n = sum(factor_table), p = test_value, alternative = "greater")
              }
              
              
              plot <- ggplot(data.frame(x = factor_table), aes(x = names(factor_table), y = factor_table)) +
                geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                geom_hline(yintercept = test_value * sum(factor_table), linetype = "dashed", color = "red") +
                ggtitle(paste("One-Sample Proportion Test Value", test_value))
              print(plot)
              
            }
          }
        } else { 
          plot <- ggplot() + 
            ggtitle("No plot available for the selected variable type.")
          print(plot)
        }
      } else if (input$test_mode == "Between Two Variables" && !is.null(input$test_var1) && !is.null(input$test_var2) && input$test_var1 != "" && input$test_var2 != "") {
        var_data1 <- df[[input$test_var1]]
        var_data2 <- df[[input$test_var2]]
        
        if (is.numeric(var_data1) && is.numeric(var_data2)) {
          plot <- ggplot() + ggtitle("No plot available")
          print(plot)
        } else if (is.factor(var_data1) && is.factor(var_data2)) {
          if (input$two_var_test == "Independence Test") {
            if (length(unique(var_data1)) > 1 && length(unique(var_data2)) > 1) {
              chi_square_result <- chisq.test(table(var_data1, var_data2))
              plot <- ggchisqtest(chi_square_result, colaccept = "grey89" , colreject = "black")
              print(plot)
            } else {
              plot <- ggplot() + ggtitle("No plot available")
              print(plot)
            }
          }  else {
            plot <- ggplot() + ggtitle("No plot available for the selected test type.")
            print(plot)
          }
        } else {
          plot <- ggplot() + ggtitle("No plot available for mixed types.")
          print(plot)
        }
      } else if (input$test_mode == "ANOVA"){
        set.seed(123)
        # Get the selected dependent and independent variables
        dep_vars <- input$dependent_vars
        ind_vars <- input$independent_vars
        
        # Check if the selection is valid
        if(length(dep_vars) == 0 || length(ind_vars) == 0) {
          plot <- ggplot() + ggtitle("No plot available for the selected test type.")
          print(plot)
        }
        
        # Check the type of variables (factor or numeric)
        dep_type <- sapply(df[, dep_vars], class)
        ind_type <- sapply(df[, ind_vars], class)
        
        # Determine the type of ANOVA to perform based on the selections
        if(all(dep_type == "numeric") && length(dep_vars) == 1 && all(ind_type == "factor")) {
          # Perform a one-way ANOVA
          formula <- as.formula(paste(dep_vars, "~", paste(ind_vars, collapse = "+")))
          anova_result <- aov(formula, data = df)

          plot <- ggplot(df, aes(x = df[[dep_vars]], y = df[[ind_vars]], fill = df[[ind_vars]])) +
              geom_boxplot() +
              scale_fill_viridis_d()+
              labs(title = "ANOVA Plot", x = dep_vars, y = ind_vars , fill = names(df[[ind_vars]]) )
          print(plot)
          
        } else if(all(dep_type == "numeric") && length(dep_vars) > 1 && all(ind_type == "factor")) {
          # Perform a MANOVA
          formula <- as.formula(paste("cbind(", paste(dep_vars, collapse = ","), ") ~", paste(ind_vars, collapse = "+")))
          manova_result <- manova(formula, data = df)
          # Use ggpairs for scatterplot matrix
          plot <-  ggpairs(
            df, 
            columns = which(names(df) %in% dep_vars),  # Columns with dependent variables
            aes(color = df[[ind_vars[1]]])  # Coloring based on the first independent factor
          ) +
            scale_color_viridis_d() +  # Apply Viridis color scale
            theme_minimal() +
            labs(title = "MANOVA Scatterplot Matrix by Factors")
          print(plot)
          
        } else {
          # If the selection cannot be used for ANOVA/MANOVA
          plot <- ggplot() + ggtitle("No plot available for the selected test type.")
          print(plot)
        }
      }
      
      # Close the PNG device
      dev.off()
    }
  )
 
  ##############################################################################
  # Data Plotting Phase
  
  # Define color scales mapping
  color_scales <- list(
    Viridis = "Viridis",
    Inferno = "Inferno",
    Cividis = "Cividis"
  )
  
  # Update the choices for the variable select inputs
  observe({
    df <- df_copy()
    updateSelectInput(session, "x_var1", choices = names(df))
    updateSelectInput(session, "y_var1", choices = names(df))
    updateSelectInput(session, "z_var1", choices = names(df))
    updateSelectInput(session, "size_var", choices = names(df))
    updateSelectInput(session, "pie_var", choices = names(df))
    updateSelectInput(session, "error_var", choices = names(df))
    updateSelectInput(session, "node_var", choices = names(df))
    updateSelectInput(session, "source_var", choices = names(df))
    updateSelectInput(session, "target_var", choices = names(df))
  })
  
  # Render UI elements for the plotting phase
  output$plot_options <- renderUI({
    req(df_copy())  # Ensure df_copy is not NULL
    if (input$plot_type == "Scatter Plot") {
      tagList(
        checkboxInput("add_regression", "Add Regression Line", value = FALSE),
        checkboxInput("add_line", "Add Geom Line", value = FALSE),
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        checkboxInput("add_grouping", "Add Grouping Variable", value = FALSE),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Box Plot") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        checkboxInput("add_grouping", "Add Grouping Variable", value = FALSE),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Bar Plot") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        checkboxInput("add_grouping", "Add Grouping Variable", value = FALSE),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Violin Plot") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        checkboxInput("add_grouping", "Add Grouping Variable", value = FALSE),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Heatmap") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        selectInput("z_var1", "Z-axis variable:", choices = names(df_copy())),
        selectInput("color_scale", "Color Scale:", choices = c("Viridis","Inferno", "Cividis"))
      )
    } else if (input$plot_type == "Bubble Chart") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        selectInput("size_var", "Bubble Size variable:", choices = names(df_copy())),
        checkboxInput("add_grouping", "Add Grouping Variable", value = FALSE),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Pie") {
      tagList(
        selectInput("pie_var", "Variable for Pie Chart:", choices = names(df_copy()))
      )
    } else if (input$plot_type == "Line Graph with Error Bars") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        selectInput("error_var", "Error variable:", choices = names(df_copy())),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Contour Plot") {
      tagList(
        selectInput("x_var1", "X-axis variable:", choices = names(df_copy())),
        selectInput("y_var1", "Y-axis variable:", choices = names(df_copy())),
        selectInput("z_var1", "Z-axis variable (for contour levels):", choices = names(df_copy())),
        selectInput("color_scale", "Color Scale:", choices = c("Viridis", "Inferno", "Cividis"))
      )
    } else if (input$plot_type == "Network Graph") {
      tagList(
        selectInput("node_var", "Node Variable:", choices = names(df_copy())),
        selectInput("source_var", "Source (Edges) Variable:", choices = names(df_copy())),
        selectInput("target_var", "Target (Edges) Variable:", choices = names(df_copy())),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Candlestick Chart") {
      tagList(
        selectInput("date_var", "Date variable:", choices = names(df_copy())),
        selectInput("open_var", "Open variable:", choices = names(df_copy())),
        selectInput("high_var", "High variable:", choices = names(df_copy())),
        selectInput("low_var", "Low variable:", choices = names(df_copy())),
        selectInput("close_var", "Close variable:", choices = names(df_copy()))
      )
    } else if (input$plot_type == "Time Series") {
      tagList(
        selectInput("date_var", "Date variable:", choices = names(df_copy())),
        selectInput("source_var", "Value variable:", choices = names(df_copy())),
        selectInput("color_scale", "Color Scale:", choices = c("Viridis", "Inferno", "Cividis")),
        uiOutput("grouping_options")
      )
    } else if (input$plot_type == "Mapbox Plot") {
      tagList(
        selectInput("lat_var", "Latitude Variable:", choices = names(df_copy())),
        selectInput("lon_var", "Longitude Variable:", choices = names(df_copy())),
        selectInput("color_var", "Explanation Variable:", choices = names(df_copy()), selected = NULL)
      )
    } 
  })
  
  output$grouping_options <- renderUI({
    req(df_copy())  # Ensure df_copy is not NULL
    if (input$add_grouping) {
      selectInput("z_var1", "Group variable:", choices = names(df_copy()))
    } else {
      colourInput("color_picker", "Select Color:", value = "blue")
    }
  })
  
  # Function to generate plot
  generate_plot <- function(df, plot_type, x_var, y_var, z_var, size_var, pie_var, add_line, add_regression, smooth_span, add_grouping, plot_title, color, color_scale, error_var, node_var, source_var, target_var,
                            date_var, open_var, high_var, low_var, close_var,lat_var, lon_var, color_var, mapbox_center_lat, mapbox_center_lon) {
    validate(
      need(is.data.frame(df), "Data must be a data frame"),
      need(x_var %in% names(df), "x_var must be a valid column name"),
      need(y_var %in% names(df), "y_var must be a valid column name")
    )
    
    # Determine plot type
    if (plot_type == "Scatter Plot") {
      if (add_grouping == FALSE && add_line == FALSE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "scatter", mode = "markers", marker = list(color = color))
      } else if (add_grouping == FALSE && add_line == TRUE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "scatter", mode = "lines+markers", marker = list(color = color))
      } else if (add_grouping == TRUE && add_line == FALSE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "scatter", mode = "markers", color = ~get(z_var))
      } else if (add_grouping == TRUE && add_line == TRUE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "scatter", mode = "lines+markers", color = ~get(z_var))
      }
    } else if (plot_type == "Box Plot") {
      if (add_grouping == FALSE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "box", boxpoints = "all", fillcolor = color, line = list(color = color))
      } else {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "box", boxpoints = "all", color = ~get(z_var))
      }
    } else if (plot_type == "Bar Plot") {
      if (add_grouping == FALSE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "bar", marker = list(color = color))
      } else {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "bar", color = ~get(z_var))
      }
    } else if (plot_type == "Violin Plot") {
      if (add_grouping == FALSE) {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "violin", fillcolor = color, line = list(color = color))
      } else {
        p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "violin", color = ~get(z_var))
      }
    } else if (plot_type == "Heatmap") {
      p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
                   type = "heatmap", colorscale = color_scales[[color_scale]])
    } else if (plot_type == "Bubble Chart") {
      p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var), size = ~get(size_var),
                   type = "scatter", mode = "markers", marker = list(color = color, opacity = 0.6, line = list(width = 2)),
                   sizes = c(10, 1000)) # Adjust the size range as needed
      if (add_grouping) {
        p <- p %>% add_trace(color = ~get(z_var))
      }
    } else if (plot_type == "Pie") {
      if (is.factor(df[[pie_var]]) || is.character(df[[pie_var]])) {
        # If pie_var is categorical
        freq_table <- as.data.frame(table(df[[pie_var]]))
        p <- plot_ly(data = freq_table, labels = ~Var1, values = ~Freq,
                     type = "pie", textinfo = "label+percent")
      } else {
        # If pie_var is numeric, show an empty plot with a warning
        p <- plot_ly() %>% layout(
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        )
      }
    } else if (plot_type == "Line Graph with Error Bars") {
      # Check if selected error variable is numeric
      validate(
        need(is.numeric(df[[error_var]]), "Error variable must be numeric")
      )
      p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var),
                     type = "scatter", mode = "lines+markers", error_y = ~list(array = df[[error_var]], color = color))
    }else if (plot_type == "Contour Plot") {
      # Generate Contour Plot
      p <- plot_ly(data = df, x = ~get(x_var), y = ~get(y_var), z = ~get(z_var), 
                   type = "contour", colorscale = color_scales[[color_scale]])
    } else if (plot_type == "Network Graph") {
      # Create network graph
      G <- graph_from_data_frame(df, directed = FALSE)
      L <- layout_with_fr(G)
      
      vs <- V(G)
      es <- as.data.frame(get.edgelist(G))
      
      Xn <- L[,1]
      Yn <- L[,2]
      
      network <- plot_ly(x = Xn, y = Yn, mode = "markers", text = vs$name, hoverinfo = "text")
      
      edge_shapes <- list()
      for(i in 1:nrow(es)) {
        v0 <- es[i, 1]
        v1 <- es[i, 2]
        edge_shapes[[i]] <- list(
          type = "line",
          line = list(color = color, width = 0.3),
          x0 = Xn[v0],
          y0 = Yn[v0],
          x1 = Xn[v1],
          y1 = Yn[v1]
        )
      }
      
      p <- network %>%
        layout(
          title = plot_title,
          shapes = edge_shapes,
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
    }else if (plot_type == "Candlestick Chart") {
      p <- plot_ly(data = df, x = ~get(date_var), type = "candlestick",
                   open = ~get(open_var), close = ~get(close_var),
                   high = ~get(high_var), low = ~get(low_var))
      p <- p %>% layout(title = plot_title)
    }  else if (plot_type == "Time Series") {
      p <- plot_ly(data = df, x = ~get(date_var), y = ~get(source_var), type = 'scatter', mode = 'lines', marker = list(color = color), line = list(color = color)) %>%
        layout(
          xaxis = list(zerolinecolor = 'ffff' , zerolinewidth = 2, gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = 'ffff' , zerolinewidth = 2, gridcolor = 'ffff'),
          plot_bgcolor = '#e5ecf6', width = 900
        )
    } else if (plot_type == "Mapbox Plot") {
      
      validate(
        need(lat_var %in% names(df) && is.numeric(df[[lat_var]]), "Latitude variable must be numeric"),
        need(lon_var %in% names(df) && is.numeric(df[[lon_var]]), "Longitude variable must be numeric")
      )
      
      # Calculate the center of the map by taking the mean of latitude and longitude
      center_lat <- mean(df[[lat_var]], na.rm = TRUE)
      center_lon <- mean(df[[lon_var]], na.rm = TRUE)
      
      # Generate Mapbox Plot
      p <- plot_ly(
        data = df,
        lat = ~get(lat_var),
        lon = ~get(lon_var),
        type = 'scattermapbox',
        marker = list(color = "fuchsia"),
        hovertext = ~paste(lat_var, ": ", get(lat_var), "<br>", lon_var, ": ", get(lon_var), "\n", get(color_var))
      ) %>%
        layout(
          mapbox = list(
            style = 'open-street-map',
            zoom = 2.5,
            center = list(lon = center_lon, lat = center_lat)
          ),
          title = plot_title
        )
    } else {
      p <- plot_ly()
    }
    
    # Add regression line to Scatter Plot
    if (add_regression && plot_type == "Scatter Plot") {
      lm_model <- lm(get(y_var) ~ get(x_var), data = df)
      p <- p %>% add_trace(y = predict(lm_model), name = "Regression Line", line = list(color = color))
    }
    
    if (plot_type != "Mapbox Plot") {
      # Customize plot title
      p <- p %>% layout(title = plot_title,
                        xaxis = list(title = x_var),
                        yaxis = list(title = y_var))
    }
    
    
    return(p)
  }
  
  # Render plot in UI
  output$plot1 <- renderPlotly({
    df <- df_copy()
    generate_plot(df, input$plot_type, input$x_var1, input$y_var1, input$z_var1, input$size_var, input$pie_var,input$add_line, input$add_regression, input$smooth_span, input$add_grouping, 
                  input$plot_title1, input$color_picker, input$color_scale, input$error_var, input$node_var, input$source_var, input$target_var, input$date_var, input$open_var, input$high_var, input$low_var, input$close_var,
                  input$lat_var, input$lon_var, input$color_var, input$mapbox_center_lat, input$mapbox_center_lon)
  })
  
  # Download plot
  output$download_plot1 <- downloadHandler(
    filename = function() {
      paste(input$plot_type, "plot.html", sep = "_")
    },
    content = function(file) {
      df <- df_copy()
      p <- generate_plot(df, input$plot_type, input$x_var1, input$y_var1, input$z_var1, input$size_var, input$pie_var,input$add_line, input$add_regression, input$smooth_span, input$add_grouping, 
                         input$plot_title1, input$color_picker, input$color_scale, input$error_var, input$node_var, input$source_var, input$target_var, input$date_var, input$open_var, input$high_var,
                         input$low_var, input$close_var,input$lat_var, input$lon_var, input$color_var, input$mapbox_center_lat, input$mapbox_center_lon)
      htmlwidgets::saveWidget(as_widget(p), file)
    }
  )
  
  # Define plot explanations
  plot_explanations <- reactive({
    plot_type <- input$plot_type
    switch(plot_type,
           "Scatter Plot" = "
      <strong>Scatter Plot:</strong>
      <p>A scatter plot displays points representing values for two continuous variables on a Cartesian plane. Each point represents an observation from the dataset, with the position on the X-axis showing the value of the first variable and the position on the Y-axis showing the value of the second variable.</p>
      <p><strong>Interpretation:</strong> Scatter plots are used to visualize the relationship between two variables. Patterns, trends, and correlations (positive or negative) between variables can be observed. For example, if points trend upwards from left to right, it indicates a positive correlation.</p>
      <p><strong>Suitable Variables:</strong> Both variables should be continuous. If you include a grouping variable, different groups can be color-coded or marked with different shapes to distinguish them.</p>",
           "Box Plot" = "
      <strong>Box Plot:</strong>
      <p>A box plot (or box-and-whisker plot) summarizes the distribution of a continuous variable by displaying its quartiles and any outliers. It shows the median, the first and third quartiles (the edges of the box), and the range of the data (whiskers).</p>
      <p><strong>Interpretation:</strong> The box plot helps identify the spread and skewness of the data. The length of the box represents the interquartile range (IQR), while the whiskers show the range of the data. Outliers are plotted as individual points outside the whiskers. This plot is useful for comparing distributions across different categories.</p>
      <p><strong>Suitable Variables:</strong> The Y-axis should be a continuous variable, while the X-axis can be categorical to compare distributions across different groups.</p>",
           "Bar Plot" = "
      <strong>Bar Plot:</strong>
      <p>A bar plot displays categorical data with rectangular bars. The length of each bar is proportional to the value it represents. It can be used to show the frequency, count, or other measures for each category.</p>
      <p><strong>Interpretation:</strong> Bar plots are useful for comparing quantities across different categories. The height or length of the bars represents the value associated with each category. Horizontal or vertical bars can be used depending on the orientation of the categories.</p>
      <p><strong>Suitable Variables:</strong> The X-axis should be a categorical variable, while the Y-axis should be a continuous variable representing the measure of interest.</p>",
           "Violin Plot" = "
      <strong>Violin Plot:</strong>
      <p>A violin plot combines aspects of a box plot and a density plot. It shows the distribution of a continuous variable across different categories. The shape of the plot resembles a violin, with the width representing the density of the data at different values.</p>
      <p><strong>Interpretation:</strong> Violin plots provide a deeper understanding of the data distribution and its density. They show the probability density of the data at different values and highlight the differences between categories. This plot is useful for comparing distributions across multiple groups.</p>
      <p><strong>Suitable Variables:</strong> The Y-axis should be a continuous variable, while the X-axis should be a categorical variable to show distribution across different groups.</p>",
           "Heatmap" = "
      <strong>Heatmap:</strong>
      <p>A heatmap visualizes data in a matrix format, with values represented by colors. It helps identify patterns, correlations, or anomalies in data across two dimensions.</p>
      <p><strong>Interpretation:</strong> Heatmaps are useful for visualizing complex datasets, showing how values change across two dimensions. Color gradients represent different values, making it easy to spot trends, clusters, or anomalies in the data.</p>
      <p><strong>Suitable Variables:</strong> The X and Y axes should represent categorical or continuous variables, and the color intensity represents the value in the matrix.</p>",
           "Bubble Chart" = "
      <strong>Bubble Chart:</strong>
      <p>A bubble chart is an extension of the scatter plot, where an additional variable is represented by the size of the bubbles. It shows the relationship between three continuous variables simultaneously.</p>
      <p><strong>Interpretation:</strong> Bubble charts help visualize the relationships and distributions of three variables. The position of each bubble represents the first two variables, while the size of the bubble represents the third variable. This plot is useful for detecting patterns and trends in multi-dimensional data.</p>
      <p><strong>Suitable Variables:</strong> Two continuous variables for the X and Y axes, and one continuous variable for the bubble size.</p>",
           "Pie" = "
      <strong>Pie Chart:</strong>
      <p>A pie chart displays data as slices of a pie, with each slice representing a category's proportion of the whole. The size of each slice is proportional to the category's value relative to the total.</p>
      <p><strong>Interpretation:</strong> Pie charts are useful for showing the composition of a whole and the proportion of each category. They are most effective when the number of categories is limited, allowing for easy comparison of proportions.</p>
      <p><strong>Suitable Variables:</strong> The variable should be categorical or discrete, with the values representing the size of each slice.</p>",
           "Line Graph with Error Bars" = "
      <strong>Line Graph with Error Bars:</strong>
      <p>A line graph with error bars shows data points connected by lines, with error bars representing variability or uncertainty around the data points. It is useful for visualizing trends and the reliability of measurements over time or another continuous variable.</p>
      <p><strong>Interpretation:</strong> This plot helps in understanding trends and the variability of data. The line shows the trend, while error bars provide insight into the uncertainty or variability around the data points. It is especially useful for displaying time series data with uncertainty measurements.</p>
      <p><strong>Suitable Variables:</strong> Both the X and Y axes should be continuous, and the error bars represent the variability around the Y values.</p>",
           "Contour Plot" = "
      <strong>Contour Plot:</strong>
      <p>A contour plot displays the levels of a variable in a two-dimensional space, using contour lines to represent different levels or values. It is useful for visualizing the topography or density of data points.</p>
      <p><strong>Interpretation:</strong> Contour plots help visualize how a variable changes across two dimensions. Contour lines represent different values, and the plot shows regions of equal value, making it easier to see patterns or gradients in the data.</p>
      <p><strong>Suitable Variables:</strong> The X and Y axes should be continuous, and the contour levels represent the value of the third continuous variable.</p>",
           "Network Graph" = "
      <strong>Network Graph:</strong>
      <p>A network graph represents relationships between entities as nodes and edges. Nodes represent entities, and edges represent connections or interactions between them. It is useful for visualizing complex networks or social interactions.</p>
      <p><strong>Interpretation:</strong> Network graphs show how entities are connected and the structure of relationships within the network. They can help identify key nodes, clusters, and overall network structure.</p>
      <p><strong>Suitable Variables:</strong> Nodes and edges should be represented by categorical or discrete variables, where edges define connections between nodes.</p>",
           "Candlestick Chart" = "
      <strong>Candlestick Chart:</strong>
      <p>A candlestick chart is used in financial analysis to show the open, high, low, and close prices of a financial instrument over time. Each candlestick represents a time period (e.g., day, week) and shows the price movement within that period.</p>
      <p><strong>Interpretation:</strong> Candlestick charts are used to analyze price trends and patterns. The body of the candlestick represents the open and close prices, while the wicks represent the high and low prices. Patterns in candlesticks can indicate potential price movements or trends.</p>
      <p><strong>Suitable Variables:</strong> The X-axis should represent time, and the Y-axis should represent price values with additional variables for open, high, low, and close prices.</p>",
           "Time Series" = "
      <strong>Time Series:</strong>
      <p>A time series plot displays data points in time order. It is used to analyze trends, patterns, and seasonal variations over time.</p>
      <p><strong>Interpretation:</strong> Time series plots help identify trends and patterns over time, such as seasonality, cycles, and overall trends. They are useful for forecasting future values based on historical data.</p>
      <p><strong>Suitable Variables:</strong> The X-axis should represent time, and the Y-axis should represent a continuous variable measured over time.</p>",
           "Mapbox Plot" = "
      <strong>Mapbox Plot:</strong>
      <p>A Mapbox plot visualizes geographical data on a map. It allows plotting locations and seeing spatial relationships between data points. It can include various types of visualizations like points, lines, and polygons on an interactive map.</p>
      <p><strong>Interpretation:</strong> Mapbox plots are useful for analyzing spatial data, identifying geographical patterns, and visualizing the distribution of data points across different locations.</p>
      <p><strong>Suitable Variables:</strong> Latitude and longitude for spatial coordinates, and any additional variables for different visual attributes like size, color, or shape of the plotted data points.</p>"
    )
  })
  
  # Render the explanation card
  output$plot_explanation <- renderUI({
    req(input$plot_type)  # Ensure plot_type is not NULL
    HTML(plot_explanations())
  })
  
  
  ##############################################################################
  # Clustering and Machine Learning Phase
  
  observe({
    df <- df_copy()
    updateSelectInput(session, "dependent_var", choices = names(df_copy()))
    updateSelectInput(session, "independent_vars", choices = names(df_copy()))
    updateSelectInput(session, "test_var", choices = names(df_copy()))
    updateSelectInput(session, "test_var1", choices = names(df_copy()))
    updateSelectInput(session, "test_var2", choices = names(df_copy()))
  })
  
  # Dynamically generate the UI elements for clustering and ML models
  output$ml_specific_ui <- renderUI({
    if (input$ml_type == "Clustering") {
      tagList(
        plotOutput("number_of_clusters"),
        downloadButton("download_optimal_clusters", "Download Optimal Number of Clusters"),
        numericInput("num_clusters", "Number of Clusters:", value = 2, min = 2, step = 1)
      )
    } else if (input$ml_type != "-") {
      tagList(
        selectInput("dependent_var", "Select Dependent Variable:", choices = names(df_copy())),
        selectInput("independent_vars", "Select Independent Variables:", choices = names(df_copy()), multiple = TRUE)
      )
    }
  })
  
  output$number_of_clusters <- renderPlot({
    set.seed(1234)
    df <- df_copy()
    df_dummie <- df_copy()
    
    # Show warning message for automated cleaning
    showModal(modalDialog(
      title = "Warning",
      "This procedure might take a while, please wait.",
      easyClose = TRUE,
      footer = NULL
    ))
    
    # Check for missing values
    if (any(is.na(df_dummie))) {
      # If there are missing values, omit them and show a warning notification
      df_dummie <- na.omit(df_dummie)
      }
    
    # Convert all categorical variables to numeric
    df_dummie[sapply(df_dummie, is.factor)] <- data.matrix(df_dummie[sapply(df, is.factor)])
    if (nrow(df_dummie) > 1) {
      gap_stat <- clusGap(df_dummie, FUN = kmeans, K.max = 15)
      plot <- fviz_gap_stat(gap_stat)
      plot
    } else {
      plot.new()
      text(0.5, 0.5, "Insufficient data after cleaning", cex = 1.5)
    }
  })
  
  # Download handler for optimal number of clusters
  output$download_optimal_clusters <- downloadHandler(
    filename = function() {
      paste("optimal_clusters_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      set.seed(1234)
      df <- df_copy()
      df_dummie <- df_copy()
      
      if (any(is.na(df_dummie))) {
        df_dummie <- na.omit(df_dummie)
      }
      
      df_dummie[sapply(df_dummie, is.factor)] <- data.matrix(df_dummie[sapply(df, is.factor)])
      
      if (nrow(df_dummie) > 1) {
        gap_stat <- clusGap(df_dummie, FUN = kmeans, K.max = 15)
        plot <- fviz_gap_stat(gap_stat)
        
        ggsave(file, plot = plot, device = "png", width = 8, height = 6)
      } else {
        png(file, width = 800, height = 600)
        plot.new()
        text(0.5, 0.5, "Insufficient data after cleaning", cex = 1.5)
        dev.off()
      }
    }
  )
  
  # Dynamically generate the results UI based on the selected ML type
  output$ml_results_ui <- renderUI({
    
    if (input$ml_type == "Clustering") {
      tagList(
        card(card_header("Clustering Results"),
             uiOutput("cluster_info"),
             plotOutput("cluster_plot"),
             downloadButton("download_cluster_plot", "Download Cluster Plot"),
             uiOutput("cluster_summary_info"),
             verbatimTextOutput("cluster_summary"))
      )
    } else if (input$ml_type == "Boost Gradient Model") {
      tagList(
        card(card_header("Boost Gradient Model Results"),
             uiOutput("boost_gradient_info"),
             verbatimTextOutput("ml_summary"),
             verbatimTextOutput("ml_accuracy"),
             plotOutput("boost_plot"),
             downloadButton("download_boost_plot", "Download Relative Influence Plot"))
      )
    } else if(input$ml_type == "Linear Regression"){
      tagList(
        card(card_header("ML Model Results"),
             uiOutput("linear_regression_info"),
             verbatimTextOutput("ml_summary"),
             verbatimTextOutput("ml_confusion_matrix"),
             verbatimTextOutput("ml_accuracy"),
             plotOutput("ml_plot"),
             downloadButton("download_ml_plot", "Download Main Plot"),
             plotOutput("ml_other_plot1"),
             downloadButton("download_ml_other_plot1", "Download Feature Importance Plot"))
      )
      } else if (input$ml_type == "CART"){
        tagList(
          card(card_header("ML Model Results"),
               uiOutput("CART_info"),
               verbatimTextOutput("ml_summary"),
               verbatimTextOutput("ml_confusion_matrix"),
               verbatimTextOutput("ml_accuracy"),
               plotOutput("ml_plot"),
               downloadButton("download_ml_plot", "Download Main Plot"),
               plotOutput("ml_other_plot1"),
               downloadButton("download_ml_other_plot1", "Download Feature Importance Plot"))
        )
      } else if (input$ml_type == "SVM") {
        tagList(
          card(card_header("ML Model Results"),
               uiOutput("SVM_info"),
               verbatimTextOutput("ml_summary"),
               verbatimTextOutput("ml_confusion_matrix"),
               verbatimTextOutput("ml_accuracy"),
               plotOutput("ml_plot"),
               downloadButton("download_ml_plot", "Download Main Plot"),
               plotOutput("ml_other_plot1"),
               downloadButton("download_ml_other_plot1", "Download Feature Importance Plot"))
        )
      } 
      
    
  })
  
  
  
  # Text for Clustering
  output$cluster_info <- renderUI({
    HTML(paste("<p><strong>Clustering</strong> is a machine learning technique used to group similar data points together based on certain features without pre-labeling. The primary goal is to discover natural groupings in the data.</p>
<ul>
  <li><strong>Optimal Number of Clusters</strong>: 
    <ul>
      <li>Determined using the <strong>gap statistic</strong>.</li>
      <li><strong>Gap Statistic</strong>: Compares the total within-cluster variation for different numbers of clusters against what would be expected under a random distribution of data. This helps to identify the most appropriate number of clusters.</li>
    </ul>
  </li>
  <li><strong>K-means Clustering</strong>:
    <ul>
      <li><strong>Algorithm</strong>: Assigns each data point to the nearest cluster centroid using the <strong>Euclidean distance</strong>.</li>
      <li><strong>Objective</strong>: Minimize the within-cluster variance.</li>
    </ul>
  </li>
</ul>"))
  })
  output$cluster_summary_info <- renderUI({
    HTML(paste("<p>The summary of a clustering model provides insights into the results and characteristics of the clustering process:</p>
<ul>
  <li><strong>Cluster Sizes</strong>: 
    <ul>
      <li>Shows the number of data points assigned to each cluster.</li>
      <li>Helps in understanding how the data is distributed across clusters.</li>
    </ul>
  </li>
  <li><strong>Cluster Centers (Centroids)</strong>: 
    <ul>
      <li>Represents the mean values of the features for the data points in each cluster.</li>
      <li>These coordinates define the central point of each cluster.</li>
    </ul>
  </li>
  <li><strong>Within-Cluster Sum of Squares (WSS)</strong>: 
    <ul>
      <li>The sum of squared distances between data points and their respective cluster centroids.</li>
      <li>A lower WSS indicates that clusters are more compact and well-defined.</li>
    </ul>
  </li>
</ul>"))
  })
  
  # Text for Gradient Boosting Model
  output$boost_gradient_info <- renderUI({
    HTML(paste("<p><strong>Gradient Boosting</strong> is an ensemble machine learning technique that builds a strong predictive model by combining multiple weak models (typically decision trees). It works iteratively to improve model accuracy.</p>
<ul>
  <li><strong>Process</strong>:
    <ul>
      <li>Each new model corrects the errors of the previous models.</li>
      <li>The iterative process continues until the overall error is minimized.</li>
    </ul>
  </li>
  <li><strong>Features</strong>:
    <ul>
      <li><strong>Feature Importance</strong>: Highlights which variables play the most significant roles in predictions.</li>
      <li><strong>Evaluation Metrics</strong>:
        <ul>
          <li><strong>RMSE (Root Mean Squared Error)</strong> for regression tasks.</li>
          <li><strong>AUC (Area Under the Curve)</strong> for classification tasks.</li>
        </ul>
      </li>
    </ul>
  </li>
  <li><strong>Data Splitting</strong>:
    <ul>
      <li>Data is split into <strong>70% training</strong> and <strong>30% testing</strong> sets.</li>
    </ul>
  </li>
</ul>
               <p><strong>Feature Importance</strong>: 
  <ul>
    <li>Shows which features are most influential in the model’s predictions.</li>
  </ul>
</p>
<p><strong>Performance Metrics</strong>: 
  <ul>
    <li>Provides metrics like RMSE or AUC to evaluate model accuracy and performance.</li>
  </ul>
</p>"))
  })
  
  # Text for Linear Regression Model
  output$linear_regression_info <- renderUI({
    HTML(paste("<p><strong>Linear Regression</strong> is a statistical method used to predict the relationship between a dependent variable and one or more independent variables by fitting a linear equation to the observed data.</p>
<ul>
  <li><strong>Objective</strong>:
    <ul>
      <li>Predict a continuous outcome by modeling the linear relationship between the dependent variable and the predictors.</li>
    </ul>
  </li>
  <li><strong>Adaptation for Classification</strong>:
    <ul>
      <li>While designed for regression, it can be adapted for binary classification using <strong>logistic regression</strong>.</li>
      <li><strong>Logistic Regression</strong>: Uses a <strong>sigmoid function</strong> to map predictions to probabilities between 0 and 1, which can be used for classification.</li>
    </ul>
  </li>
  <li><strong>Limitations</strong>:
    <ul>
      <li>Linear regression for classification can produce values outside the [0, 1] range, making <strong>logistic regression</strong> a more appropriate model for binary outcomes.</li>
    </ul>
  </li>
  <li><strong>Data Splitting</strong>:
    <ul>
      <li>Data is split into <strong>70% training</strong> and <strong>30% testing</strong> sets for classification purposes.</li>
    </ul>
  </li>
</ul>
         <p><strong>Predictive Relationship</strong>: 
  <ul>
    <li>Describes how well the model predicts the dependent variable based on independent variables.</li>
  </ul>
</p>
<p><strong>Performance Metrics</strong>: 
  <ul>
    <li>Evaluates model accuracy and effectiveness in prediction.</li>
  </ul>
</p>"))
  })
  
  # Text for CART Model
  output$CART_info <- renderUI({
    HTML(paste("<p><strong>CART (Classification and Regression Trees)</strong> is a machine learning algorithm used for making predictions through decision trees. It handles both classification and regression tasks.</p>
<ul>
  <li><strong>Decision Trees</strong>:
    <ul>
      <li>Start with the entire dataset at the root and recursively split the data based on features to create decision rules.</li>
      <li><strong>Classification Trees</strong>: Use measures like <strong>Gini impurity</strong> or <strong>entropy</strong> for splitting.</li>
      <li><strong>Regression Trees</strong>: Focus on minimizing <strong>variance</strong> or <strong>mean squared error</strong>.</li>
    </ul>
  </li>
  <li><strong>Leaf Nodes</strong>:
    <ul>
      <li><strong>Classification</strong>: Represent class labels based on majority class within the node.</li>
      <li><strong>Regression</strong>: Represent predicted values as the average target values in the node.</li>
    </ul>
  </li>
  <li><strong>Features</strong>:
    <ul>
      <li><strong>Interpretability</strong>: Trees are easy to visualize and understand, making predictions transparent.</li>
      <li><strong>Flexibility</strong>: Can handle various types of data and do not assume specific data distributions.</li>
    </ul>
  </li>
  <li><strong>Data Splitting</strong>:
    <ul>
      <li>Data is split into <strong>70% training</strong> and <strong>30% testing</strong> sets for classification purposes.</li>
    </ul>
  </li>
</ul>
           <p><strong>Decision Rules</strong>: 
  <ul>
    <li>Highlights the rules used to split the data and make predictions.</li>
  </ul>
</p>
<p><strong>Tree Structure</strong>: 
  <ul>
    <li>Visual representation helps in understanding how decisions are made.</li>
  </ul>
</p>    "))
  })
  
  # Text for SVM Model
  output$SVM_info <- renderUI({
    HTML(paste("<p><strong>Support Vector Machine (SVM)</strong> is a supervised learning algorithm used for classification and regression tasks. It finds the optimal boundary separating different classes in a feature space.</p>
<ul>
  <li><strong>Classification</strong>:
    <ul>
      <li>Maps data into a high-dimensional space and finds the <strong>hyperplane</strong> that maximally separates the classes.</li>
      <li><strong>Support Vectors</strong>: Nearest data points to the hyperplane that define its position.</li>
    </ul>
  </li>
  <li><strong>Kernel Trick</strong>:
    <ul>
      <li>Used to handle non-linearly separable data by transforming it into a higher-dimensional space.</li>
      <li>Common kernels include <strong>polynomial</strong>, <strong>radial basis function (RBF)</strong>, and <strong>sigmoid</strong>.</li>
    </ul>
  </li>
  <li><strong>Regression (SVR)</strong>:
    <ul>
      <li>Aims to find a function that deviates from target values by a minimal margin within a specified tolerance.</li>
    </ul>
  </li>
  <li><strong>Features</strong>:
    <ul>
      <li>Effective in high-dimensional spaces and for complex datasets.</li>
      <li>Requires careful tuning of parameters like kernel type and regularization.</li>
    </ul>
  </li>
</ul>
              <p><strong>Hyperplane and Margin</strong>: 
  <ul>
    <li>Describes the separation boundary and margin used to classify data.</li>
  </ul>
</p>
<p><strong>Kernel Functions</strong>: 
  <ul>
    <li>Details on the kernel functions used and their impact on model performance.</li>
  </ul>
</p> "))
  })
  
  # Handle the application of clustering or ML models
  observeEvent(input$ml_apply, {
    df <- df_copy()
    
    # Clear previous outputs before running a new model
    output$ml_plot <- renderPlot(NULL)
    output$ml_summary <- renderPrint(NULL)
    output$ml_accuracy <- renderPrint(NULL)
    output$cluster_plot <- renderPlot(NULL)
    output$boost_plot <- renderPlot(NULL)
    output$ml_other_plot1 <- renderPlot(NULL)
    output$ml_other_plot2 <- renderPlot(NULL)
    
    if (input$ml_type == "Clustering") {
      df <- df_copy()
      
      # Check for missing values
      if (any(is.na(df))) {
        # If there are missing values, omit them and show a warning notification
        df <- na.omit(df)
        showNotification("Your data contains missing values. We omitted these to cluster the Data. If you want all your observations please go to Data Cleaning and check the option Impute Missing Values", 
                         type = "warning",
                         duration = NULL)
      }
      
      # Check for factor variables and notify the user
      if (any(sapply(df, is.factor))) {
        showNotification("Factor variables have been transformed into dummy numerical variables for clustering. This transformation may introduce bias. To avoid this, consider removing factor variables or handling them appropriately in the Data Cleaning section.", 
                         type = "warning",
                         duration = NULL)
      }
      
      # Perform Clustering
      set.seed(123)
      k <- input$num_clusters
      df_dummie <- df_copy()
      # Check for missing values
      if (any(is.na(df_dummie))) {
        # If there are missing values, omit them and show a warning notification
        df_dummie <- na.omit(df_dummie)
      }
      df_dummie[sapply(df_dummie, is.factor)] <- data.matrix(df_dummie[sapply(df, is.factor)])
      kmeans_clusters <- kmeans(df_dummie, centers = k)
      
      output$cluster_plot <- renderPlot({
        set.seed(123)
        fviz_cluster(
          kmeans_clusters,
          data = df_dummie,
          geom = "point",
          stand = TRUE,
          ellipse.type = "convex",
          main = "K-means Clustering",
          ggtheme = theme_minimal()
        )
      })
      
      output$cluster_summary <- renderPrint({
        kmeans_clusters
      })
      
    }
    else if (input$ml_type == "Boost Gradient Model") {
      dep_var <- req(input$dependent_var)
      indep_vars <- req(input$independent_vars)
      if (dep_var == "-" || is.null(indep_vars) || any(indep_vars == "-")) {
        return(NULL)
      }
      
      # Split the data into training and testing sets
      set.seed(123)
      df <- df_copy()
      # Check for missing values
      if (any(is.na(df))) {
        # If there are missing values, omit them and show a warning notification
        df <- na.omit(df)
        showNotification("Your data contains missing values. We omitted these to create the model the Data. If you want all your observations please go to Data Cleaning and check the option Impute Missing Values", type = "warning")
      }
      trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
      trainData <- df[trainIndex,]
      testData <- df[-trainIndex,]
      
      formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
      model <- gbm(formula, data = trainData, distribution = "gaussian", n.trees = 100)
      
      predictions <- predict(model, testData, n.trees = 100)
      accuracy <- postResample(predictions, testData[[dep_var]])
      
      if (is.numeric(df[[dep_var]])) {
        output$ml_accuracy <- renderPrint({
          paste("RMSE:", accuracy[1], " R-squared:", accuracy[2])
        })
      } else {
        # Generate predictions as probabilities
        probabilities <- predict(model, testData, type = "response")
        predictions <- ifelse(probabilities > 0.5, 1, 0)
        predictions <- factor(predictions, levels = c(0, 1))
        
        # Check if levels in predictions overlap with actual levels
        if (all(levels(predictions) %in% levels(testData[[dep_var]]))) {
          # Confusion matrix to assess accuracy
          accuracy <- confusionMatrix(predictions, testData[[dep_var]])
          
          output$ml_accuracy <- renderPrint({
            accuracy
          })
        } else {
          output$ml_accuracy <- renderPrint({
            "The predictions did not contain all levels present in the actual data."
          })
        }
      }
      
      
      # Display relative influence plot
      output$boost_plot <- renderPlot({
        summary(model, main = "Relative Influence Plot")
      })
      
      output$ml_summary <- renderPrint({
        summary(model)
      })
      
    }
    
    else if (input$ml_type != "-") {
      dep_var <- req(input$dependent_var)
      indep_vars <- req(input$independent_vars)
      if (dep_var == "-" || is.null(indep_vars) || any(indep_vars == "-")) {
        return(NULL)
      }
      
      if (input$ml_type == "Linear Regression") {

        # Split the data into training and testing sets
        set.seed(123)
        df <- df_copy()
        # Check for missing values
        if (any(is.na(df))) {
          # If there are missing values, omit them and show a warning notification
          df <- na.omit(df)
          showNotification("Your data contains missing values. We omitted these to create the model the Data. If you want all your observations please go to Data Cleaning and check the option Impute Missing Values", type = "warning")
        }
        trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
        trainData <- df[trainIndex,]
        testData <- df[-trainIndex,]
        
        formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
        if (is.numeric(df[[dep_var]])) {
          # Linear Regression model for numeric dependent variable
          model <- lm(formula, data = trainData)
          
          predictions <- predict(model, testData)
          
          # Display summary of the model
          output$ml_summary <- renderPrint({
            summary(model)
          })
          
          # Calculate accuracy metrics
          accuracy <- postResample(predictions, testData[[dep_var]])
          
          output$ml_accuracy <- renderPrint({
            paste("RMSE:", accuracy[1], " R-squared:", accuracy[2])
          })
          
          # Plot actual vs predicted values
          output$ml_plot <- renderPlot({
            ggplot(testData, aes_string(x = dep_var, y = predictions)) +
              geom_point() +
              geom_abline(intercept = 0, slope = 1, color = "red") +
              labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")
          })
          
          # Feature importance plot based on absolute coefficient values
          output$ml_other_plot1 <- renderPlot({
            coef_df <- as.data.frame(summary(model)$coefficients)
            coef_df$Variable <- rownames(coef_df)
            coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
            
            ggplot(coef_df, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
              geom_bar(stat = "identity") +
              coord_flip() +
              scale_fill_viridis_d()+
              labs(title = "Feature Importance", x = "Variables", y = "Absolute Coefficient Value")
          })
          
        } else if (is.factor(df[[dep_var]])) {
          levels_count <- nlevels(df[[dep_var]])
          
          if (levels_count == 2) {
            # Logistic Regression model for binary factor dependent variable
            model <- tryCatch(
              {
                glm(formula, data = trainData, family = binomial)
              },
              warning = function(w) {
                message("Warning: ", w)
                NULL
              },
              error = function(e) {
                message("Error: ", e)
                NULL
              }
            )
            
            if (!is.null(model)) {
              # Generate predictions as probabilities
              probabilities <- predict(model, testData, type = "response")
              predictions <- ifelse(probabilities > 0.5, 1, 0)
              predictions <- factor(predictions, levels = c(0, 1))
              
              # Display summary of the model
              output$ml_summary <- renderPrint({
                summary(model)
              })
              
              # Check if levels in predictions overlap with actual levels
              if (all(levels(predictions) %in% levels(testData[[dep_var]]))) {
                # Confusion matrix to assess accuracy
                accuracy <- confusionMatrix(predictions, testData[[dep_var]])
                
                output$ml_accuracy <- renderPrint({
                  accuracy
                })
              } else {
                output$ml_accuracy <- renderPrint({
                  "The predictions did not contain all levels present in the actual data."
                })
              }
              # Convert factor variable to numeric for plotting
              df_long <- df %>%
                mutate(Predictions = predict(model, newdata = df))
              
              # Generate the plot with ggplot
              output$ml_plot <- renderPlot({
                ggplot(df_long, aes(x = rownames(df_long), y = Predictions, color = df[[dep_var]])) +
                  geom_point() +
                  geom_line(aes(group = df[[dep_var]]), alpha = 0.7) +
                  labs(title = "Predictions by Factor Level", x = "Index", y = "Predicted Values") +
                  theme_minimal()
              })
              # Feature importance plot for binary logistic regression
              output$ml_other_plot1 <- renderPlot({
                coef_df <- as.data.frame(summary(model)$coefficients)
                coef_df$Variable <- rownames(coef_df)
                coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
                
                ggplot(coef_df, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
                  geom_bar(stat = "identity") +
                  coord_flip() +
                  scale_fill_viridis_d()+
                  labs(title = "Feature Importance", x = "Variables", y = "Absolute Coefficient Value")
              })
              
            } else {
              output$ml_accuracy <- renderPrint({
                "Model did not converge. Consider regularizing or simplifying the model."
              })
            }
            
          } else if (levels_count > 2) {
            # Multinomial Logistic Regression for multi-class factor dependent variable
            model <- tryCatch(
              {
                multinom(formula, data = trainData)
              },
              warning = function(w) {
                message("Warning: ", w)
                NULL
              },
              error = function(e) {
                message("Error: ", e)
                NULL
              }
            )
            
            if (!is.null(model)) {
              # Generate predictions
              predictions <- predict(model, testData, type = "class")
              
              # Display summary of the model
              output$ml_summary <- renderPrint({
                summary(model)
              })
              
              # Confusion matrix to assess accuracy
              accuracy <- confusionMatrix(predictions, testData[[dep_var]])
              
              output$ml_accuracy <- renderPrint({
                accuracy
              })
              # Convert factor variable to numeric for plotting
              df_long <- df %>%
                mutate(Predictions = predict(model, newdata = df))
              
              # Generate the plot with ggplot
              output$ml_plot <- renderPlot({
                ggplot(df_long, aes(x = rownames(df_long), y = Predictions, color = df[[dep_var]])) +
                  geom_point() +
                  geom_line(aes(group = df[[dep_var]]), alpha = 0.7) +
                  labs(title = "Predictions by Factor Level", x = "Index", y = "Predicted Values") +
                  theme_minimal()
              })
              # Feature importance plot for multinomial logistic regression
              output$ml_other_plot1 <- renderPlot({
                coef_df <- as.data.frame(summary(model)$coefficients)
                coef_df$Variable <- rownames(coef_df)
                
                # Reshape data for plotting
                coef_df_long <- reshape2::melt(coef_df, id.vars = "Variable", variable.name = "Class", value.name = "Estimate")
                
                ggplot(coef_df_long, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
                  geom_bar(stat = "identity", position = "dodge") +
                  coord_flip() +
                  scale_fill_viridis_d()+
                  labs(title = "Feature Importance (by Class)", x = "Variables", y = "Absolute Coefficient Value")
              })
              
            } else {
              output$ml_accuracy <- renderPrint({
                "Model did not converge. Consider regularizing or simplifying the model."
              })
            }
          }
        }
      } else if (input$ml_type == "CART") {
        
        # Split the data into training and testing sets
        set.seed(123)
        df <- df_copy()
        
        # Check for missing values
        if (any(is.na(df))) {
          # If there are missing values, omit them and show a warning notification
          df <- na.omit(df)
          showNotification("Your data contains missing values. We omitted these to create the model the Data. If you want all your observations please go to Data Cleaning and check the option Impute Missing Values", type = "warning")
        }
        
        trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
        trainData <- df[trainIndex,]
        testData <- df[-trainIndex,]
        
        formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
        if (is.factor(df[[dep_var]])) {
          # Classification Tree
          model <- rpart(formula, data = trainData, method = "class")
          
          predictions <- predict(model, testData, type = "class")
          predictions <- factor(predictions, levels = levels(df[[dep_var]]))
          
          accuracy <- confusionMatrix(predictions, testData[[dep_var]])
          
          output$ml_accuracy <- renderPrint({
            accuracy
          })
          
        } else {
          # Regression Tree
          model <- rpart(formula, data = trainData, method = "anova")
          
          predictions <- predict(model, testData)
          accuracy <- postResample(predictions, testData[[dep_var]])
          
          output$ml_accuracy <- renderPrint({
            paste("RMSE:", accuracy[1], " R-squared:", accuracy[2])
          })
        }
        
        output$ml_plot <- renderPlot({
          rpart.plot(model)
        })
        
        # Feature importance plot
        output$ml_other_plot1 <- renderPlot({
          importance <- varImp(model)
          importance_df <- as.data.frame(importance)
          importance_df$Variable <- rownames(importance_df)
          
          ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall, fill = Variable)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_viridis_d()+
            labs(title = "Feature Importance", x = "Variables", y = "Importance")
        })
      } 
      else if (input$ml_type == "SVM") {
        
        # Split the data into training and testing sets
        set.seed(123)
        # Check for missing values
        if (any(is.na(df))) {
          # If there are missing values, omit them and show a warning notification
          df <- na.omit(df)
          showNotification("Your data contains missing values. We omitted these to create the model the Data. If you want all your observations please go to Data Cleaning and check the option Impute Missing Values", type = "warning")
        }
        trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
        trainData <- df[trainIndex,]
        testData <- df[-trainIndex,]
        
        # Generate the formula dynamically
        formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
        
        # Determine if the dependent variable is a factor (classification) or numeric (regression)
        if (is.factor(df[[dep_var]])) {
          # Classification case
          output$ml_plot <- renderPlot(NULL) # Reset plot to avoid any carryover issues
          
          # Convert independent factor variables into dummy variables
          trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
          testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
          
          # Fit the SVM model for classification
          model <- svm(formula, data = trainData, kernel = "radial", probability = TRUE)
          
          # Predictions
          predictions <- predict(model, testData, probability = TRUE)
          
          # Calculate accuracy using confusion matrix
          accuracy <- confusionMatrix(predictions, testData[[dep_var]])
          
          # Render outputs for SVM classification
          output$ml_accuracy <- renderPrint({ accuracy })
          output$ml_summary <- renderPrint({ summary(model) })
          
          output$ml_plot <- renderPlot({
            # PCA for plotting when there are multiple explanatory variables
            pca <- prcomp(testData_dummies, scale. = TRUE)
            pca_data <- data.frame(pca$x, Class = testData[[dep_var]])
            ggplot(pca_data, aes(x = PC1, y = PC2, color = Class)) +
              geom_point(size = 2) +
              labs(title = "SVM Classification with PCA", x = "Principal Component 1", y = "Principal Component 2") +
              theme_minimal()
          })
          
        } else {
          # Regression case
          output$ml_plot <- renderPlot(NULL) # Reset plot to avoid any carryover issues
          
          # Convert independent variables (if necessary) into dummy variables
          trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
          testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
          
          # Fit the SVM model for regression
          model <- svm(formula, data = trainData, kernel = "radial")
          
          # Predictions
          predictions <- predict(model, testData)
          
          # Calculate accuracy using RMSE and R-squared
          accuracy <- postResample(predictions, testData[[dep_var]])
          
          # Render outputs for SVM regression
          output$ml_accuracy <- renderPrint({ paste("RMSE:", accuracy[1], "R-squared:", accuracy[2]) })
          output$ml_summary <- renderPrint({ summary(model) })
          
          output$ml_plot <- renderPlot({
            # PCA for plotting when there are multiple explanatory variables
            pca <- prcomp(testData_dummies, scale. = TRUE)
            pca_data <- data.frame(pca$x, Predicted = predictions)
            ggplot(pca_data, aes(x = PC1, y = PC2, color = Predicted)) +
              geom_point(size = 2) +
              labs(title = "SVM Regression with PCA", x = "Principal Component 1", y = "Principal Component 2") +
              theme_minimal()
          })
        }
        
        # Permutation importance with absolute values
        feature_importance <- sapply(indep_vars, function(var) {
          # Permute the values of the variable
          permuted_test <- testData
          permuted_test[[var]] <- sample(permuted_test[[var]])
          
          # Make predictions with the permuted data
          if (is.factor(df[[dep_var]])) {
            permuted_predictions <- predict(model, permuted_test, probability = TRUE)
            permuted_accuracy <- confusionMatrix(permuted_predictions, testData[[dep_var]])
            importance_score <- abs(accuracy$overall['Accuracy'] - permuted_accuracy$overall['Accuracy'])
          } else {
            permuted_predictions <- predict(model, permuted_test)
            permuted_rmse <- postResample(permuted_predictions, testData[[dep_var]])[1]
            importance_score <- abs(accuracy[1] - permuted_rmse)
          }
          return(importance_score)
        })
        
        # Create a data frame for the importance plot
        importance_df <- data.frame(
          Variable = names(feature_importance),
          Importance = feature_importance
        )
        
        # Plot the feature importance with absolute values
        output$ml_other_plot1 <- renderPlot({
          ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance , fill = Variable)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_viridis_d()+
            labs(title = "Feature Importance (Permutation, Absolute)", x = "Variables", y = "Absolute Importance")
        })
      }
      output$ml_summary <- renderPrint({
        summary(model)
      })
    }
  })
  
  
  output$download_cluster_plot <- downloadHandler(
    filename = function() {
      paste("cluster_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- df_copy()
      # Perform Clustering
      set.seed(123)
      k <- input$num_clusters
      df_dummie <- df_copy()
      # Check for missing values
      if (any(is.na(df_dummie))) {
        # If there are missing values, omit them and show a warning notification
        df_dummie <- na.omit(df_dummie)
      }
      df_dummie[sapply(df_dummie, is.factor)] <- data.matrix(df_dummie[sapply(df, is.factor)])
      kmeans_clusters <- kmeans(df_dummie, centers = k)
      
      set.seed(123)
      plot <-  fviz_cluster(
        kmeans_clusters,
        data = df_dummie,
        geom = "point",
        stand = TRUE,
        ellipse.type = "convex",
        main = "K-means Clustering",
        ggtheme = theme_minimal()
      )
      print(plot)
      dev.off()
    }
  )
  
  output$download_ml_plot <- downloadHandler(
    filename = function() {
      paste("main_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      
      if (input$ml_type != "-") {
        dep_var <- req(input$dependent_var)
        indep_vars <- req(input$independent_vars)
        if (dep_var == "-" || is.null(indep_vars) || any(indep_vars == "-")) {
          plot <- ggplot() + ggtitle("No plot available. Please select a Machine Learning Model.")
          print(plot)
        }
        
        if (input$ml_type == "Linear Regression") {
          
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)
          }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          if (is.numeric(df[[dep_var]])) {
            # Linear Regression model for numeric dependent variable
            model <- lm(formula, data = trainData)
            predictions <- predict(model, testData)
            
            # Plot actual vs predicted values
            plot <- ggplot(testData, aes_string(x = dep_var, y = predictions)) +
              geom_point() +
              geom_abline(intercept = 0, slope = 1, color = "red") +
              labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")
            print(plot)
            
          } else if (is.factor(df[[dep_var]])) {
            levels_count <- nlevels(df[[dep_var]])
            
            if (levels_count == 2) {
              # Logistic Regression model for binary factor dependent variable
              model <- tryCatch(
                {
                  glm(formula, data = trainData, family = binomial)
                },
                warning = function(w) {
                  message("Warning: ", w)
                  NULL
                },
                error = function(e) {
                  message("Error: ", e)
                  NULL
                }
              )
              
              if (!is.null(model)) {
                # Generate predictions as probabilities
                probabilities <- predict(model, testData, type = "response")
                predictions <- ifelse(probabilities > 0.5, 1, 0)
                predictions <- factor(predictions, levels = c(0, 1))
                
                
                # Check if levels in predictions overlap with actual levels
                if (all(levels(predictions) %in% levels(testData[[dep_var]]))) {
                  # Confusion matrix to assess accuracy
                  accuracy <- confusionMatrix(predictions, testData[[dep_var]])
                } 
                
                # Convert factor variable to numeric for plotting
                df_long <- df %>%
                  mutate(Predictions = predict(model, newdata = df))
                
                # Generate the plot with ggplot
                plot <- ggplot(df_long, aes(x = rownames(df_long), y = Predictions, color = df[[dep_var]])) +
                  geom_point() +
                  geom_line(aes(group = df[[dep_var]]), alpha = 0.7) +
                  labs(title = "Predictions by Factor Level", x = "Index", y = "Predicted Values") +
                  theme_minimal()
                print(plot)
              }
            } else if (levels_count > 2) {
              # Multinomial Logistic Regression for multi-class factor dependent variable
              model <- tryCatch(
                {
                  multinom(formula, data = trainData)
                },
                warning = function(w) {
                  message("Warning: ", w)
                  NULL
                },
                error = function(e) {
                  message("Error: ", e)
                  NULL
                }
              )
              
              if (!is.null(model)) {
                # Generate predictions
                predictions <- predict(model, testData, type = "class")
                # Confusion matrix to assess accuracy
                accuracy <- confusionMatrix(predictions, testData[[dep_var]])
                
                # Convert factor variable to numeric for plotting
                df_long <- df %>%
                  mutate(Predictions = predict(model, newdata = df))
                
                # Generate the plot with ggplot
                plot <- ggplot(df_long, aes(x = rownames(df_long), y = Predictions, color = df[[dep_var]])) +
                  geom_point() +
                  geom_line(aes(group = df[[dep_var]]), alpha = 0.7) +
                  labs(title = "Predictions by Factor Level", x = "Index", y = "Predicted Values") +
                  theme_minimal()
                print(plot)
                
                
              } 
            }
          }
        } else if (input$ml_type == "CART") {
          
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)           }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          if (is.factor(df[[dep_var]])) {
            # Classification Tree
            model <- rpart(formula, data = trainData, method = "class")
            
            predictions <- predict(model, testData, type = "class")
            predictions <- factor(predictions, levels = levels(df[[dep_var]]))
            
            accuracy <- confusionMatrix(predictions, testData[[dep_var]])
            
          } else {
            # Regression Tree
            model <- rpart(formula, data = trainData, method = "anova")
            predictions <- predict(model, testData)
            accuracy <- postResample(predictions, testData[[dep_var]])
          }
          
          plot <- rpart.plot(model)
          print(plot)
          
        } 
        else if (input$ml_type == "SVM") {
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)
          }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          # Generate the formula dynamically
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          
          # Determine if the dependent variable is a factor (classification) or numeric (regression)
          if (is.factor(df[[dep_var]])) {
            
            # Convert independent factor variables into dummy variables
            trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
            testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
            
            # Fit the SVM model for classification
            model <- svm(formula, data = trainData, kernel = "radial", probability = TRUE)
            
            # Predictions
            predictions <- predict(model, testData, probability = TRUE)
            
            # Calculate accuracy using confusion matrix
            accuracy <- confusionMatrix(predictions, testData[[dep_var]])
            
            
            # PCA for plotting when there are multiple explanatory variables
            pca <- prcomp(testData_dummies, scale. = TRUE)
            pca_data <- data.frame(pca$x, Class = testData[[dep_var]])
            plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Class)) +
                geom_point(size = 2) +
                labs(title = "SVM Classification with PCA", x = "Principal Component 1", y = "Principal Component 2") +
                theme_minimal()
            print(plot)
            
          } else {
            
            # Convert independent variables (if necessary) into dummy variables
            trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
            testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
            
            # Fit the SVM model for regression
            model <- svm(formula, data = trainData, kernel = "radial")
            
            # Predictions
            predictions <- predict(model, testData)
            
            # Calculate accuracy using RMSE and R-squared
            accuracy <- postResample(predictions, testData[[dep_var]])
            
           
            # PCA for plotting when there are multiple explanatory variables
            pca <- prcomp(testData_dummies, scale. = TRUE)
            pca_data <- data.frame(pca$x, Predicted = predictions)
            plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Predicted)) +
                geom_point(size = 2) +
                labs(title = "SVM Regression with PCA", x = "Principal Component 1", y = "Principal Component 2") +
                theme_minimal()
            print(plot)
          }
      }
      }
      dev.off()
    }
  )
  
  output$download_ml_other_plot1 <- downloadHandler(
    filename = function() {
      paste("feature_importance_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      if (input$ml_type != "-") {
        dep_var <- req(input$dependent_var)
        indep_vars <- req(input$independent_vars)
        if (dep_var == "-" || is.null(indep_vars) || any(indep_vars == "-")) {
          plot <- ggplot() + ggtitle("No plot available. Please select a Machine Learning Model.")
          print(plot)
        }
        
        if (input$ml_type == "Linear Regression") {
          
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)
          }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          if (is.numeric(df[[dep_var]])) {
            # Linear Regression model for numeric dependent variable
            model <- lm(formula, data = trainData)
            predictions <- predict(model, testData)
            
            # Calculate accuracy metrics
            accuracy <- postResample(predictions, testData[[dep_var]])
            
            # Feature importance plot based on absolute coefficient values
            
            coef_df <- as.data.frame(summary(model)$coefficients)
            coef_df$Variable <- rownames(coef_df)
            coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
            
            plot <- ggplot(coef_df, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
              geom_bar(stat = "identity") +
              coord_flip() +
              scale_fill_viridis_d()+
              labs(title = "Feature Importance", x = "Variables", y = "Absolute Coefficient Value")
            print(plot)
            
          } else if (is.factor(df[[dep_var]])) {
            levels_count <- nlevels(df[[dep_var]])
            
            if (levels_count == 2) {
              # Logistic Regression model for binary factor dependent variable
              model <- tryCatch(
                {
                  glm(formula, data = trainData, family = binomial)
                },
                warning = function(w) {
                  message("Warning: ", w)
                  NULL
                },
                error = function(e) {
                  message("Error: ", e)
                  NULL
                }
              )
              
              if (!is.null(model)) {
                # Generate predictions as probabilities
                probabilities <- predict(model, testData, type = "response")
                predictions <- ifelse(probabilities > 0.5, 1, 0)
                predictions <- factor(predictions, levels = c(0, 1))
                
                # Check if levels in predictions overlap with actual levels
                if (all(levels(predictions) %in% levels(testData[[dep_var]]))) {
                  # Confusion matrix to assess accuracy
                  accuracy <- confusionMatrix(predictions, testData[[dep_var]])
                  
                } 
                # Convert factor variable to numeric for plotting
                df_long <- df %>%
                  mutate(Predictions = predict(model, newdata = df))
                
                # Feature importance plot for binary logistic regression
                coef_df <- as.data.frame(summary(model)$coefficients)
                coef_df$Variable <- rownames(coef_df)
                coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
                
                plot <- ggplot(coef_df, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
                  geom_bar(stat = "identity") +
                  coord_flip() +
                  scale_fill_viridis_d()+
                  labs(title = "Feature Importance", x = "Variables", y = "Absolute Coefficient Value")
                print(plot)
                
              } 
            } else if (levels_count > 2) {
              # Multinomial Logistic Regression for multi-class factor dependent variable
              model <- tryCatch(
                {
                  multinom(formula, data = trainData)
                },
                warning = function(w) {
                  message("Warning: ", w)
                  NULL
                },
                error = function(e) {
                  message("Error: ", e)
                  NULL
                }
              )
              
              if (!is.null(model)) {
                # Generate predictions
                predictions <- predict(model, testData, type = "class")
                
                # Confusion matrix to assess accuracy
                accuracy <- confusionMatrix(predictions, testData[[dep_var]])
                
                # Convert factor variable to numeric for plotting
                df_long <- df %>%
                  mutate(Predictions = predict(model, newdata = df))
                
                # Feature importance plot for multinomial logistic regression
                coef_df <- as.data.frame(summary(model)$coefficients)
                coef_df$Variable <- rownames(coef_df)
                
                # Reshape data for plotting
                coef_df_long <- reshape2::melt(coef_df, id.vars = "Variable", variable.name = "Class", value.name = "Estimate")
                
                plot <- ggplot(coef_df_long, aes(x = reorder(Variable, abs(Estimate)), y = abs(Estimate), fill = Variable)) +
                  geom_bar(stat = "identity", position = "dodge") +
                  coord_flip() +
                  scale_fill_viridis_d()+
                  labs(title = "Feature Importance (by Class)", x = "Variables", y = "Absolute Coefficient Value")
                print(plot)
                
              }
            }
          }
        } else if (input$ml_type == "CART") {
          
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)           }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          if (is.factor(df[[dep_var]])) {
            # Classification Tree
            model <- rpart(formula, data = trainData, method = "class")
            
            predictions <- predict(model, testData, type = "class")
            predictions <- factor(predictions, levels = levels(df[[dep_var]]))
            
            accuracy <- confusionMatrix(predictions, testData[[dep_var]])
            
          } else {
            # Regression Tree
            model <- rpart(formula, data = trainData, method = "anova")
            predictions <- predict(model, testData)
            accuracy <- postResample(predictions, testData[[dep_var]])
            
          }
          
          
          # Feature importance plot
          importance <- varImp(model)
          importance_df <- as.data.frame(importance)
          importance_df$Variable <- rownames(importance_df)
          
          plot <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall, fill = Variable)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_viridis_d()+
            labs(title = "Feature Importance", x = "Variables", y = "Importance")
          print(plot)
        } 
        else if (input$ml_type == "SVM") {
          
          # Split the data into training and testing sets
          set.seed(123)
          df <- df_copy()
          # Check for missing values
          if (any(is.na(df))) {
            # If there are missing values, omit them and show a warning notification
            df <- na.omit(df)
          }
          trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
          trainData <- df[trainIndex,]
          testData <- df[-trainIndex,]
          
          # Generate the formula dynamically
          formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
          
          # Determine if the dependent variable is a factor (classification) or numeric (regression)
          if (is.factor(df[[dep_var]])) {
            
            # Convert independent factor variables into dummy variables
            trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
            testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
            
            # Fit the SVM model for classification
            model <- svm(formula, data = trainData, kernel = "radial", probability = TRUE)
            
            # Predictions
            predictions <- predict(model, testData, probability = TRUE)
            
            # Calculate accuracy using confusion matrix
            accuracy <- confusionMatrix(predictions, testData[[dep_var]])
            
          } else {
            
            # Convert independent variables (if necessary) into dummy variables
            trainData_dummies <- model.matrix(~ . - 1, data = trainData[indep_vars])
            testData_dummies <- model.matrix(~ . - 1, data = testData[indep_vars])
            
            # Fit the SVM model for regression
            model <- svm(formula, data = trainData, kernel = "radial")
            
            # Predictions
            predictions <- predict(model, testData)
            
            # Calculate accuracy using RMSE and R-squared
            accuracy <- postResample(predictions, testData[[dep_var]])
          }
          
          # Permutation importance with absolute values
          feature_importance <- sapply(indep_vars, function(var) {
            # Permute the values of the variable
            permuted_test <- testData
            permuted_test[[var]] <- sample(permuted_test[[var]])
            
            # Make predictions with the permuted data
            if (is.factor(df[[dep_var]])) {
              permuted_predictions <- predict(model, permuted_test, probability = TRUE)
              permuted_accuracy <- confusionMatrix(permuted_predictions, testData[[dep_var]])
              importance_score <- abs(accuracy$overall['Accuracy'] - permuted_accuracy$overall['Accuracy'])
            } else {
              permuted_predictions <- predict(model, permuted_test)
              permuted_rmse <- postResample(permuted_predictions, testData[[dep_var]])[1]
              importance_score <- abs(accuracy[1] - permuted_rmse)
            }
            return(importance_score)
          })
          
          # Create a data frame for the importance plot
          importance_df <- data.frame(
            Variable = names(feature_importance),
            Importance = feature_importance
          )
          
          # Plot the feature importance with absolute values
          plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance , fill = Variable)) +
              geom_bar(stat = "identity") +
              coord_flip() +
              scale_fill_viridis_d()+
              labs(title = "Feature Importance (Permutation, Absolute)", x = "Variables", y = "Absolute Importance")
          print(plot)
        }
      }
      dev.off()
    }
  )
  
  output$download_boost_plot <- downloadHandler(
    filename = function() {
      paste("relative_importance_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      if (input$ml_type == "Boost Gradient Model") {
        dep_var <- req(input$dependent_var)
        indep_vars <- req(input$independent_vars)
        if (dep_var == "-" || is.null(indep_vars) || any(indep_vars == "-")) {
          plot <- ggplot() + ggtitle("No plot available. Please select a Machine Learning Model.")
          print(plot)
        }
        
        # Split the data into training and testing sets
        set.seed(123)
        df <- df_copy()
        # Check for missing values
        if (any(is.na(df))) {
          # If there are missing values, omit them and show a warning notification
          df <- na.omit(df)           }
        trainIndex <- createDataPartition(df[[dep_var]], p = .7, list = FALSE, times = 1)
        trainData <- df[trainIndex,]
        testData <- df[-trainIndex,]
        
        formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
        model <- gbm(formula, data = trainData, distribution = "gaussian", n.trees = 100)
        
        predictions <- predict(model, testData, n.trees = 100)
        accuracy <- postResample(predictions, testData[[dep_var]])
        
        if (is.numeric(df[[dep_var]])) {
          plot <- ggplot() + ggtitle("No plot available. Please select a Machine Learning Model.")
          print(plot)
        } else {
          # Generate predictions as probabilities
          probabilities <- predict(model, testData, type = "response")
          predictions <- ifelse(probabilities > 0.5, 1, 0)
          predictions <- factor(predictions, levels = c(0, 1))
          
          # Check if levels in predictions overlap with actual levels
          if (all(levels(predictions) %in% levels(testData[[dep_var]]))) {
            # Confusion matrix to assess accuracy
            accuracy <- confusionMatrix(predictions, testData[[dep_var]])
          } 
        }
        
        # Display relative influence plot
        plot <- summary(model, main = "Relative Influence Plot")
        print(plot)
        
      }
      dev.off()
    }
  )
  
  
}




shinyApp(ui, server)

