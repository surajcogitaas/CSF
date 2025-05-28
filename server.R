
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)

# options(shiny.error = browser)

#### Server ######
# Define the Server Logic
server <- function(input, output, session) {
  
  # Store for selected models by model type
  selected_models_store <- reactiveValues(data = list())
  
  ############ Project Directory selection ####################
  # Define accessible root directories 
  roots <- c(Home = normalizePath("~"), Projects = "C:/")
  
  # Enable directory selection with folder navigation
  shinyDirChoose(input, "select_dir", roots = roots, allowDirCreate = TRUE,session = session)

  # Store directory path (initially set to current working directory)
  dir_path <- reactiveVal(getwd())
  
  # # Update `dir_path` when user selects a directory
  observeEvent(input$select_dir, {
    req(input$select_dir)  # Ensure input is not NULL
    dir_path(parseDirPath(roots, input$select_dir))  # Update path
    # print(dir_path())
    if(is.null(input$select_dir) || length(input$select_dir) == 0 || length(dir_path()) == 0){
      dir_path(getwd())
    }else{
      # dir_path(parseDirPath(roots, input$select_dir))
    }
  })

  # Display selected directory
  output$selected_dir <- renderPrint({
    dir_path()
  })
  ###############################################################
  # Set up directory selection
  # selected_dir <- "C:/Users/Suraj/OneDrive - CogitaasAVA/Desktop/Post Modelling/CSF-Folder/"
  
  ########################################################### Move/Copy Fils Box ##################################################
  # volumes <-  dir_path()#roots # c(Home = normalizePath("~"), Projects = getwd())
  observe({
    req(dir_path())
    roots <- c(MyVolume = dir_path())
    shinyDirChoose(input, "srcFolder", roots = roots)
    shinyDirChoose(input, "destFolder", roots = roots)
  })
  
  
  fileList <- reactiveVal(NULL)
  
  observeEvent(input$srcFolder, {
    req(input$srcFolder)
    src <- parseDirPath(c(MyVolume = dir_path()), input$srcFolder)
    output$srcDir <- renderText({ paste("Source Directory:", src) })
    files <- list.files(src, pattern = "(?i)\\.(xlsx?|csv)$", full.names = TRUE, recursive = FALSE)
    files <- files[!file.info(files)$isdir]
    fileList(files)
    choices <- c("All" = "all", setNames(files, basename(files)))
    updateSelectInput(session, "dataFile", choices = choices)
  })
  
  observeEvent(input$destFolder, {
    req(input$destFolder)
    dest <- parseDirPath(c(MyVolume = dir_path()), input$destFolder)
    output$destDir <- renderText({ paste("Destination Directory:", dest) })
  })
  
  processFiles <- function(fun, successMsg) {
    req(input$dataFile, input$destFolder)
    dest <- parseDirPath(c(MyVolume = dir_path()), input$destFolder)
    selected <- if ("all" %in% input$dataFile) fileList() else input$dataFile
    results <- sapply(selected, function(f) {
      fun(f, file.path(dest, basename(f)))
    })
    output$message <- renderText(
      if (all(results)) paste(length(selected),successMsg," ") else "Operation failed!"
    )
  }
  
  observeEvent(input$move, processFiles(file.rename, "files moved successfully!"))
  observeEvent(input$copy, processFiles(function(from, to) file.copy(from, to, overwrite = TRUE), "files copied successfully!"))
  
  #Reset button clear files list, selections and messages.
  observeEvent(input$reset, {
    fileList(NULL)
    updateSelectInput(session, "dataFile", choices = character(0))
    output$srcDir <- renderText({ "" })
    output$destDir <- renderText({ "" })
    output$message <- renderText({ "" })
  })
  
  ################ file uploading #############
  # Define the directory that contains Do file
  D0_dir <- reactive({file.path(dir_path(),"Onboard/2. D0" )}) #"Onboard/4. Modelling/writedata/Selected models"
  
  # Observe the reactive directory and update the file choices accordingly
  observe({
    current_dir <- D0_dir()
    files <- list.files(
      path = current_dir,
      pattern = "\\.(csv)$",
      full.names = TRUE,recursive = FALSE
    )
    files <- files[!file.info(files)$isdir]
    fileList(files)
    
    choices <- c(setNames(files, basename(files)))
    updateSelectInput(session, "D0_file", choices = choices)
    
  })
  
  # When the upload button is clicked, read the selected files
  D0_df <- eventReactive(input$upload_btn, {
    # print(input$D0_file)
    req(input$D0_file)
    file_1 <- input$D0_file
    
    # Ensure at least one file is selected
    if (is.null(file_1))
      return(NULL)
    
    # Read each file based on its extension
    ext <- tolower(tools::file_ext(file_1))
    if (ext == "csv") {
      D0_df <- read.csv(file_1)
    } else {
      NULL
    }

    return(D0_df)
    
  })
  
  #Define a directory contain LO file
  L0_dir <- reactive({file.path(dir_path(),"Onboard/4. Modelling/writedata/Selected models" )}) #
  
  # Observe the reactive directory and update the file choices accordingly
  observe({
    current_dir <- L0_dir()
    files <- list.files(
      path = current_dir,
      pattern = "\\.(xls|xlsx)$",
      full.names = TRUE,recursive = FALSE
    )
    files <- files[!file.info(files)$isdir]
    fileList(files)
    
    choices <- c(setNames(files, basename(files)))
    updateSelectInput(session, "L0_file", choices = choices)
    
  })
  
  # observe({print(input$L0_file)})
  
  # When the upload button is clicked, read each selected file separately
  file_list <- eventReactive(input$upload_btn,{
    req(input$L0_file)
    files = input$L0_file
    # print(files)
    
    preprocess_fun <- function(file_1){
      # Read file based on its extension
      ext <- tolower(tools::file_ext(file_1))
      # Extract sheet name when excel file is uploaded
      if(ext %in% c("xls", "xlsx")){
        sheets <- excel_sheets(file_1)
        if (c("FinalM0") %in% sheets) {
          df <- read_excel(file_1,sheet = "FinalM0")
          # Round all numeric variable to 3 decimal places
          df <- df %>% mutate(across(where(is.numeric), ~round(.x, 3)))

          if (!"Price_pval" %in% colnames(df)) {
            df$Price_pval <- NA  # or provide a default value
          }

          if (!"method" %in% colnames(df)) {
            df$method <- "Unknown"  # Set a default method
          }

          #Remove rows with all NAs
          df <- df[rowSums(is.na(df)) != ncol(df), ]
          
        }else{
           NULL
        }
      }else{
         NULL
      }
      
      # print(df)
      return(df)
    }
    
    lst <- lapply(files, preprocess_fun)
    
    files_names <- gsub("Wtd_avg_MCV_", "", tools::file_path_sans_ext(basename(files))) #"Brand","Variant","PackType","PPG"
    
    # Use file names (without extension) as names for the list elements
    lst <- setNames(lst,files_names)    #tools::file_path_sans_ext(basename(files))
    return(lst)
  })
  # observe({print(names(file_list()))})
  
  # Render the combined data table
  output$combined_data <- DT::renderDataTable({
    req(D0_df())
    DT::datatable(D0_df())
  })
  
  
  ##################################################################### Dashboard Start ###########################################################################
  #Select model for...
  output$modelof_ui <- renderUI({
    req(file_list())
    modelfor <- gsub("Wtd_avg_MCV_", "", names(file_list())) #c("Brand","Variant","PackType","PPG")
    selectInput("modelof", "model selection for",choices = modelfor,selected = modelfor[1])
  })
  
  median_df <- reactive({
    
    req(file_list(), input$upload_btn, input$L0_file)  # Ensure inputs exist
    
    files <- input$L0_file
    if (length(files) == 0) return(NULL)  # Handle empty file list
    
    mid_value <- function(clm, na.rm = FALSE) {
      if (na.rm) clm <- na.omit(clm)
      sort_clm <- sort(clm, na.last = NA)
      n <- length(sort_clm)
      if (n == 0) return(NA)
      return(sort_clm[ceiling(n / 2)])
    }
    
    median_fun <- function(file_1) {
      ext <- tolower(tools::file_ext(file_1))
      
      if (ext %in% c("xls", "xlsx")) {
        
        sheets <- excel_sheets(file_1)
        df <- read_excel(file_1, sheet = "FinalM0")
        
        if ("median" %in% sheets) {
          m_df <- read_excel(file_1, sheet = "median")
          # return(list(full_df = df, filtered_df = df))  # Return the same df twice
        } else{
          
          # df <- read_excel(file_1, sheet = "FinalM0")
          
          m_df <- df %>% 
            filter(Price_pval=="Yes") %>%  
            mutate(selectedmodels = as.character(selectedmodels)) %>%
            group_by(Channel, Brand, Variant, PackType, PPG) %>%
            reframe(median_val = mid_value(CSF.CSF, na.rm = TRUE)) %>%
            left_join(df, by = c("Channel", "Brand", "Variant", "PackType", "PPG")) %>%
            ungroup() %>%
            mutate(selectedmodels = ifelse(selectedmodels == "1" & CSF.CSF == median_val, "Yes", selectedmodels)) %>%
            dplyr::select(-median_val) %>%
            dplyr::select(names(df)) %>% 
            filter(selectedmodels == "Yes")
          
          # df_yes <- df %>% filter(selectedmodels == "Yes")  # Create filtered dataframe
          
          wb <- loadWorkbook(file_1)
          addWorksheet(wb, "median")
          writeData(wb, "median", m_df)
          saveWorkbook(wb, file_1, overwrite = TRUE)
          
          print("✅ 'median' sheet added successfully.") 
        }
        
        # return(list(full_df = df, median_df = m_df))  # Return both versions
        # list(full_df = df, median_df = m_df)  # Return both versions
      }
      
      ## IF ALL COMBINATION CONTAIN 1'S THEN PUT "Yes" TO CORRESPONDING MEDIAN
      df_with_m <- df %>% 
        dplyr::select(everything()) %>% 
        mutate(across(c(selectedmodels),as.character)) %>%  # Convert necessary columns to character
        mutate(initial_val = case_when(
          Index %in% m_df$Index ~ 'median',
          TRUE ~ "1"
        )) %>% 
        group_by(across(all_of(c("Channel", "Brand", "Variant", "PackType", "PPG")))) %>% 
        mutate(
          flag_all_ones = all(selectedmodels == "1"),  # Check if all values in group are "1"
          selectedmodels = case_when(
            flag_all_ones & initial_val == "median" ~ "Yes",  # If all 1s, set median row to Yes
            TRUE ~ selectedmodels  # Otherwise, keep original values
          )
        ) %>% 
        dplyr::select(-flag_all_ones) %>% 
        ungroup()
      
      return(list(full_df = df_with_m, median_df = m_df))
    }
    
    lst <- lapply(files, median_fun)
    files_names <- gsub("Wtd_avg_MCV_", "", tools::file_path_sans_ext(basename(files)))
    lst <- setNames(lst, files_names)
    
    return(lst)  # List of lists: each element has full_df and filtered_df
  })
  
  
  L0_table <- reactive({
    req(!(is.na(input$L0_indicator)))
    df <- median_df()[[input$L0_indicator]]$full_df
  })
  
  L2_table <- reactive({
    req(!(is.na(input$L2_indicator)))
    df <- median_df()[[input$L2_indicator]]$full_df
  })

  L3_table <- reactive({
    req(!(is.na(input$L3_indicator)))
    df <- median_df()[[input$L3_indicator]]$full_df
  })
  
    
  #select targeting file
  target_df <- reactive({
    req(input$modelof, any(!is.null(L0_table()), !is.null(L2_table()), !is.null(L3_table())))
    
    #targetinf file
    # target_file <-  input$modelof
    
    # if(!is.na(current_selection)){
    #   return(current_selection())
    # }
    print(input$modelof)
    print(!is.null(selected_models_store$data[[input$modelof]]))
    # Check if store data for this mdel 
    if (!is.null(selected_models_store$data[[input$modelof]])) {
      
      return(selected_models_store$data[[input$modelof]])
      
    }else if (input$modelof == input$L0_indicator) {
      return(L0_table())
    }else if(input$modelof == input$L2_indicator){
      return(L2_table())
    }else if(input$modelof == input$L3_indicator){
      return(L3_table())
    }else {
      return(NULL)
    }
    
  })
  # target_df <- reactive({
  #   req(input$modelof, any(!is.null(L0_table()), !is.null(L2_table()), !is.null(L3_table())))
  #   
  #   #targetinf file
  #   target_file <-  input$modelof
  #   
  #   # if(!is.na(current_selection)){
  #   #   return(current_selection())
  #   # }
  #   
  #   # print(target_file)
  #   if (target_file == input$L0_indicator) {
  #     return(L0_table())
  #   }else if(target_file == input$L2_indicator){
  #     return(L2_table())
  #   }else if(target_file == input$L3_indicator){
  #     return(L3_table())
  #   }else {
  #     return(NULL)
  #   }
  #   
  # })
  
  # Initialize reactiveValues to store selections data files
  # models_selected_df <- reactiveValues(data = list())
  
  # Initialize models_selected_df with empty data frames for each modelof value
  # observeEvent(file_list(), {
  #   req(file_list())
  #   modelfor <- gsub("Wtd_avg_MCV_", "", names(file_list())) # e.g., "Brand", "PPG", etc.
  #   for (model in modelfor) {
  #     if (is.null(models_selected_df$data[[model]]) || nrow(models_selected_df$data[[model]]) > 0) {
  #       print(paste("Initializing models_selected_df for:", model))
  #       # models_selected_df$data[[model]] <- data.frame() # Initialize empty data frame
  #       # Initialize with target_df() when available
  #       current_model <- model
  #       temp_data <- reactive({
  #         req(input$modelof == current_model)
  #         target_df()
  #       })
  #       if (!is.null(temp_data)) {
  #         print("Run -----models_selected_df$data[[model]] <- temp_data")
  #         models_selected_df$data[[model]] <- temp_data()
  #         # print(paste("str(models_selected_df$data[[model]]) : ",str(models_selected_df$data[[model]])))
  #       } else {
  #         print("Run -----data.frame()")
  #         models_selected_df$data[[model]] <- data.frame() # Fallback to empty data frame
  #       }
  #     }
  #   }
  # })
  
  # # Initialize models_selected_df with original data
  # observeEvent(file_list(), {
  #   req(file_list())
  #   modelfor <- gsub("Wtd_avg_MCV_", "", names(file_list()))
  #   for (model in modelfor) {
  #     if (!is.null(models_selected_df$data[[model]]) && nrow(models_selected_df$data[[model]]) > 0) {
  #       print(paste("Model already initialized:", model))
  #     } else {
  #       print(paste("Initializing models_selected_df for:", model))
  #       current_model <- model
  #       temp_data <- reactive({
  #         req(input$modelof == current_model)
  #         target_df()
  #       })
  #       if (!is.null(temp_data)) {
  #         models_selected_df$data[[model]] <- temp_data()
  #       } else {
  #         models_selected_df$data[[model]] <- data.frame()
  #       }
  #     }
  #   }
  # })

  #Retrieve Selections when switching models
  L0_df <- reactive({
    req(input$modelof, file_list())
    #Get current data
    current_data <- target_df()
    return(current_data)
    })
  
  # L0_df <- reactive({
  #   req(input$modelof, file_list())
  #   if (!is.null(models_selected_df$data[[input$modelof]])) {
  #     return(models_selected_df$data[[input$modelof]])
  #   } else {
  #     return(target_df())
  #   }
  # })
  # Retrieve selections when switching models
  # L0_df <- reactive({
  #   req(input$modelof, file_list())
  #   current_data <- target_df()
  #   validate(need(!is.null(current_data), paste("No data available for", input$modelof)))
  #   
  #   print(paste("Checking saved data for:", input$modelof))
  #   print(paste("Has saved data:", !is.null(models_selected_df$data[[input$modelof]]) && nrow(models_selected_df$data[[input$modelof]]) > 0))
  #   
  #   if (!is.null(models_selected_df$data[[input$modelof]]) && nrow(models_selected_df$data[[input$modelof]]) > 0) {
  #     print(paste("Returning saved selections for:", input$modelof))
  #     return(models_selected_df$data[[input$modelof]])
  #   } else {
  #     print(paste("Using original data for:", input$modelof))
  #     return(current_data) # Return target_df without overwriting
  #   }
  # })
  
  # L0_df <- reactive({
  #   req(input$modelof)
  #   m <- input$modelof
  #   if (!is.null(models_selected_df$data[[m]])) {
  #     models_selected_df$data[[m]]
  #   } else {
  #     target_df_for(m)
  #   }
  # })
  
  # L0_df <- reactive({
  #   req(input$modelof, file_list())
  # 
  #   #Get current data
  #   current_data <- target_df()
  #   validate(need(!is.null(current_data), paste("No data available for", input$modelof)))
  #   
  #   print(paste("Checking saved data for:", input$modelof))
  #   print(paste("Has saved data:", !is.null(models_selected_df$data[[input$modelof]]) && nrow(models_selected_df$data[[input$modelof]]) > 0))
  #   
  #   #Check if there are saved selections for the current modelof
  #   if (!is.null(models_selected_df$data[[input$modelof]]) && nrow(models_selected_df$data[[input$modelof]]) > 0) {
  #     print(paste("Returning saved selections for:", input$modelof))
  #     return(models_selected_df$data[[input$modelof]])
  # 
  #   } else {
  #     # Initialize with current data if no selections exist
  #     print(paste("Initializing with original data for:", input$modelof))
  #     models_selected_df$data[[input$modelof]] <- current_data
  #     return(current_data)
  #   }
  # 
  # })
  
  # #Retrieve Selections when switching models
  # L0_df <- reactive({
  #   req(input$modelof, file_list())
  #   current_data <- target_df()
  #   validate(need(!is.null(current_data), paste("No data available for", input$modelof)))
  #   
  #   print(paste("Checking saved data for:", input$modelof))
  #   print(paste("Has saved data A:", !is.null(models_selected_df$data[[input$modelof]])))
  # 
  #   # view(models_selected_df$data[[input$modelof]])
  #   print(paste("Has saved data B:", str(models_selected_df$data[[input$modelof]])))
  # 
  #   if (!is.null(models_selected_df$data[[input$modelof]]) && nrow(models_selected_df$data[[input$modelof]]) > 0) {
  #     print(paste("Returning saved selections for:", input$modelof))
  #     return(models_selected_df$data[[input$modelof]])
  # 
  #   } else {
  #     # Initialize with a default or empty data frame
  #     print(paste("Using original data for:", input$modelof))
  #     return(current_data) # Return target_df() without overwriting models_selected_df
  #   }
  # 
  # })

  
  # # Select targeting file
  # L0_df <- reactive({
  #   req(input$modelof, any(!is.null(L0_table()), !is.null(L2_table()), !is.null(L3_table())))
  # 
  #   #targetinf file
  #   target_file = input$modelof
  #   
  #   # if(!is.na(current_selection)){
  #   #   return(current_selection())
  #   # }
  # 
  #   # print(target_file)
  #   if (target_file == input$L0_indicator) {
  #     return(L0_table())
  #   }else if(target_file == input$L2_indicator){
  #     return(L2_table())
  #   }else if(target_file == input$L3_indicator){
  #     return(L3_table())
  #   }else {
  #     return(NULL)
  #   }
  # 
  # })
  
  
  
  ############################################################### Filters start #############################################
  # Create a persistent version of L0_df() just for filtering.
  filter_data_store <- reactiveVal()
  
  # Initialize once (This runs only once, even if L0_df() updates later.)
  observeEvent(input$modelof, {
    print(paste("True", input$modelof))
    req(L0_df())
    isolate({
      filter_data_store(L0_df())
      
    })
    print(dim(filter_data_store()))
  })
  
  # observeEvent(input$modelof, {
  #   print(paste("True", input$modelof))
  #   req(L0_df())
  #   filter_data_store(L0_df())
  #   print(dim(filter_data_store()))
  # }, once = TRUE)
  # 
  
  
  
  # Controlled update only when user clicks refresh
  observeEvent(input$refresh_filters, {
    req(filter_data_store())
    filter_data_store(filter_data_store())
  })
  # actionButton("reset_filter_data", "Refresh Filter Data")
  
  
  
  
  
  # Check if necessary columns exist in the data

  # Dynamic Price_pval Filter
  output$price_pval_ui <- renderUI({
    req(filter_data_store())  # Ensure filtered dataset is available
    price_values <- unique(na.omit(filter_data_store()$Price_pval))  # Remove NULLs
    selectInput("price_pval", "Select Price P-Value:", choices = c("All", price_values), selected = "Yes")
  })
  
  # Dynamic Method Filter
  output$method_ui <- renderUI({
    req(filter_data_store())  # Ensure filtered dataset is available
    methods <- unique(na.omit(filter_data_store()$method))  # Remove NULLs
    selectInput("method", "Select Method:", choices = c("All",methods), selected = NULL, multiple = TRUE)
  })
  
  
  # Numeric Filter for Distribution_elas (>=0 or NaN)
  output$distribution_elas_ui <- renderUI({
    req(filter_data_store())  # Ensure dataset is available
    checkboxInput("distribution_elas", "Filter Distribution Elas (>=0 or NaN):", value = TRUE)
  })
  
  # Numeric Filter for Category_elas (>=0 or NaN)
  output$category_elas_ui <- renderUI({
    req(filter_data_store())  # Ensure dataset is available
    checkboxInput("category_elas", "Filter Category Elas (>=0 or NaN):", value = TRUE)
  })
  
  # Dynamic Channel Filter
  output$channel_ui <- renderUI({
    req(filter_data_store()) #Ensure Data is available
    channels <- unique(na.omit(filter_data_store()$Channel)) #Remove Nulls
    selectInput("channel", "Select Channel:", choices = c("All", channels), selected = "All")
  })
  
  # Dynamic Brand Filter (Based on Selected Channel)
  output$brand_ui <- renderUI({
    req(filter_data_store(), input$modelof) # Ensure filtered dataset is available
    selected_clm <- input$modelof
    choices <- unique(na.omit(filter_data_store()[[selected_clm]]))  # Remove NULLs
    selectInput("brand", paste("Select ",selected_clm,":",sep = ""), choices = c("All", choices), selected = "All")
  })
  
  # Dynamically update column selection inputs
  output$x_var_ui <- renderUI({
    req(filter_data_store())
    x_var_choise <- c("Index")
    selectInput("x_var", "X-axis variable:", choices = x_var_choise, selected = "Index")
  })
  
  output$y_var_ui <- renderUI({
    req(filter_data_store())
    y_var_choice <- c("CSF.CSF","MCV.MCV")
    selectInput("y_var", "Y-axis variable:", choices = y_var_choice, selected = y_var_choice[1])
  })
  
  ####### filtering based on New Inputs
  filter_df_1 <- reactive({
    req(filter_data_store())  # Ensure dataset is available
    df <- filter_data_store()
    
    if(nrow(df) == 0) return(NULL) # Ensure df is not empty
    
    # Trim whitespace in Price_pval
    # df$Price_pval <- trimws(df$Price_pval)
    
    # Apply Price_pval Filter
    if (!is.null(input$price_pval) && input$price_pval != "All") {
      df <- df[df$Price_pval %in% input$price_pval, ]
    }
    
    # Apply Methode Filter
    if (!is.null(input$method) && !("All" %in% input$method)) {
      df <- df[df[["method"]] %in% input$method, ]
    }
    
    # Apply Distribution_elas Filter
    if (!is.null(input$distribution_elas) && input$distribution_elas) {
      df <- df[((df$Distribution_elas >= 0)|is.na(df$Distribution_elas)), ]
    }
    
    # Apply Category_elas Filter
    if (!is.null(input$category_elas) && input$category_elas) {
      df <- df[((df$Category_elas >= 0)|is.na(df$Category_elas)), ]
    }
    
    # Apply Channnel Filter
    if (!is.null(input$channel) && input$channel != "All") {
      df <- df[df$Channel %in% input$channel, ]
    }
    
    # Apply Brand Filter
    selected_clm <- input$modelof
    if (!is.null(input$brand) && input$brand != "All") {
      df <- df[df[[selected_clm]] %in% input$brand, ]
    }
    
    # Removes rows where all values are NA after  filtering
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    return(df)
  })
  
  # print(filter_df_1)
  
  ########### Dyanamic RPIto Filter
  output$rpito_ui <- renderUI({
    req(filter_df_1())
    rpito_value <- unique(na.omit(filter_df_1()$RPIto))  # Get available values
    selectInput("rpito", "Select RPIto:", choices = rpito_value, multiple = TRUE)
  })
  
  filter_df_rpi <- reactive({
    req(filter_df_1())  # Ensure dataset is available
    df <- filter_df_1()
    # Apply RPIto Filter
    if ((!is.null(input$rpito)) && (nrow(df)>0)) {
      df <- df[df$RPIto %in% input$rpito, ]
    }
    
    return(df)
  })
  
  ############## Dynamic Actual distvar filter
  output$actualdist_ui <- renderUI({
    req(filter_df_rpi())
    values <- unique(na.omit(filter_df_rpi()$actualdistvar))  # Get available values
    
    selectInput("actualdist", "Select actualdistvar:", choices = values, multiple = TRUE)
  })
  filter_df_actdist <- reactive({
    req(filter_df_rpi())  # Ensure dataset is available
    df <- filter_df_rpi()
    # Apply RPIto Filter
    if (!is.null(input$actualdist) && (nrow(df)>0)) {
      df <- df[df$actualdistvar %in% input$actualdist, ]
    }
    
    return(df)
  })
  
  ## Dyanamic Adj Rsquare 
  output$adjrsq_ui <- renderUI({
    req(filter_df_actdist())
    
    values <- unique(na.omit(filter_df_actdist()$Adj.Rsq)) #get unique values
    min_val <- min(values, na.rm=TRUE)
    max_val <- max(values, na.rm=TRUE)
    if(length(values) ==1) {
      numericInput("adjrsq","Adj Rsquare:", value = values,min = values,max = values)
      # sliderInput("adjrsq","Adj Rsquare:",min = values,max = values, value = c(values))
    }else if(min_val==max_val){
      sliderInput("adjrsq","Adj Rsquare:",min = min(values),max = max(values), value = c(min(values),max(values)))
    }else{
      sliderInput("adjrsq","Adj Rsquare:",min = min_val,max = max_val, value = c(min_val,max_val))
    }
    
  })
  
  filter_df_adjrsq <- reactive({
    req(filter_df_actdist())
    df <- filter_df_actdist()
    # Apply adjrsq filter
    if(!is.null(input$adjrsq)){
      if((length(input$adjrsq)==1)){
        df <- df[df$Adj.Rsq >= input$adjrsq & df$Adj.Rsq <=input$adjrsq,]
      }else if(length(input$adjrsq)>1){
        df <- df[df$Adj.Rsq >= input$adjrsq[1] & df$Adj.Rsq <=input$adjrsq[2] ,]
      }
    }
    
    return(df)
  })
  ################### Dynamic CSF filter
  output$csf_ui <- renderUI({
    req(filter_df_adjrsq())
    values <- unique(na.omit(filter_df_adjrsq()$CSF.CSF))
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)
    if(length(values) ==1) {
      numericInput("csf","CSF:", value = values,min = values,max = values)
    }else if(min_val==max_val){
      sliderInput("csf","CSF:",min = min_val,max = max_val, value = c(min_val,max_val))
    }else{
      sliderInput("csf","CSF:",min = min_val,max = max_val, value = c(min_val,max_val))
    }
  })
  filter_df_csf <- reactive({
    req(filter_df_adjrsq())
    df <- filter_df_adjrsq()
    #Apply filter on csf
    if(!is.null(input$csf) && ("CSF.CSF" %in% colnames(df))){
      if((length(input$csf)==1)){
        df <- df[df$CSF.CSF >= input$csf & df$CSF.CSF <=input$csf,]
      }else if(length(input$csf)>1){
        df <- df[df$CSF.CSF >= input$csf[1] & df$CSF.CSF <=input$csf[2] ,]
      }else{
        df <- df[df$CSF.CSF >= input$csf[1] & df$CSF.CSF <=input$csf[2] ,]
      }
    }
    return(df)
  })
  
  #################### Sorting on Adj R sq OR CSF 
  output$sort_ui <- renderUI({
    req(filter_df_csf())
    radioButtons("sort_by", "Sort on:",choices = c("Adj Rsq"="Adj.Rsq", "CSF"="CSF.CSF"),selected = "Adj.Rsq")
  })
  
  
  sorted_df <- reactive({
    req(input$sort_by)
    df <- filter_df_csf()
    req(input$sort_by)
    if ((!is.null(input$sort_by)) && (length(input$sort_by)>0) && (input$sort_by %in% colnames(df))) {
      df <- df[order(df[[input$sort_by]], decreasing = TRUE), ]
    }
    
    return(df)
  })
  
  
  ################################################ Filters End  ################################################################

  ## after all filter, rename the filtred data 
  final_df <- reactive({
    df <- sorted_df()
    return(df)
  })
  
  ############### Model selection #########################################################################################################
  output$select_ui <- renderUI({
    req(final_df())
    df <- final_df()
    
    # numericInput("select_m","Select Model Index:",value = 0, min = min(df$Index),max = max(df$Index))
    # selectInput("select_m","Select Model Index:",choices =(df$Index), multiple = TRUE)
    selectizeInput("select_m","Select Model Index:", choices =NULL, multiple=FALSE)
  })
  observe({
    updateSelectizeInput(session, "select_m", choices = final_df()$Index, server = TRUE)
  })

  
  modelselection_df <- reactive({
    req(final_df())
    df <- final_df()
    
    df$selectedmodels <- as.character(df$selectedmodels)
    
    selected_id <- input$select_m
    # print(selected_id)
    
    if(all(!is.null(selected_id)) && all(!is.na(selected_id)) && all(selected_id %in% df$Index)){
      df[(df$Index %in% selected_id),"selectedmodels"] <- "Yes"
      
      # df$selectedmodels[df$Index %in% selected_id] <- "Yes"
    }
    
    return(df)
  })
  
  
  # Save and reset button
  output$savebttn_ui <- renderUI({
    tagList(
      actionButton("save_bttn","Save selected model"),
      actionButton("unsave_bttn","Reset selected model",icon = icon("undo"))
    )
  })
  
  #Reactive value to store selected models
  selected_models_df <- reactiveVal(data.frame())
  observe({
    req(L0_df())
    selected_models_df(L0_df()[0,])
  })
  
  # # Save selected model
  # observeEvent(input$save_bttn,{
  #   req(input$select_m, modelselection_df(), final_df(),input$modelof)
  # 
  #   # print(input$select_m)
  # 
  #   df <- modelselection_df()
  #   selected_idx <- input$select_m
  # 
  #   if(all(selected_idx>0)){
  #     selected_data <- df[(df$Index %in% selected_idx), ,drop=FALSE] #get selected row
  # 
  #     ## Avoide duplicate entries
  #     #Get current saved data
  #     # current_data <- models_selected_df$data[[input$modelof]]
  #     # if (nrow(current_data)==0) {current_data <- target_df()}
  #     current_data <- selected_models_df()
  # 
  #     # Remove existing entries with the same combination of Channel, Brand, Variant, PackType, and PPG
  #     updated_data <- current_data %>%
  #       anti_join(selected_data, by = c("Channel", "Brand", "Variant", "PackType", "PPG")) %>%
  #       bind_rows(selected_data) #%>% arrange(Index)
  #     selected_models_df(updated_data)
  # 
  #     # Immediately update models_selected_df$data to persist selections
  #     current_df <- L0_df()
  #     updated_current_df <- anti_join(current_df, selected_data, by = c("Channel", "Brand", "Variant", "PackType", "PPG")) %>%
  #       bind_rows(selected_data) %>% arrange(Index)
  # 
  #     deselected_df <- anti_join(current_df, updated_current_df, by = "Index") %>%
  #       mutate(selectedmodels = as.character(1))
  # 
  #     final_updated_df <- bind_rows(deselected_df, updated_current_df)
  # 
  #     models_selected_df$data[[input$modelof]] <- final_updated_df
  #     showNotification(paste("Selections for", input$modelof, "saved successfully!"), type = "message")
  # 
  #   }
  # 
  # })
  
  # Save selected model
  observeEvent(input$save_bttn,{
    req(input$select_m, modelselection_df(), final_df())

    # print(input$select_m)

    df <- modelselection_df()
    selected_idx <- input$select_m

    if(all(selected_idx>0)){
      selected_data <- df[(df$Index %in% selected_idx), ,drop=FALSE] #get selected row

      ## Avoide duplicate entries
      #Get current saved data
      # current_data <- models_selected_df$data[[input$modelof]]
      # if (nrow(current_data)==0) {current_data <- target_df()}
      current_data <- selected_models_df()

      # Remove existing entries with the same combination of Channel, Brand, Variant, PackType, and PPG
      updated_data <- current_data %>%
        anti_join(selected_data, by = c("Channel", "Brand", "Variant", "PackType", "PPG")) %>%
        bind_rows(selected_data) %>%
        arrange(Index)

      # # Append the new selection
      # updated_data <- bind_rows(updated_data, selected_data)

      # Update the reactive data frame
      selected_models_df(updated_data)
      # models_selected_df$data[[input$modelof]] <- updated_data
      showNotification(paste("Selections for", input$modelof, "saved successfully!"), type = "message")
    }

  })
  

  
  # # Reset selected models
  # observeEvent(input$unsave_bttn,{
  #   req(input$select_m)
  #   
  #   # Reset selected_model_df
  #   updated_data <- selected_models_df()[!(selected_models_df()$Index %in% input$select_m),]
  #   selected_models_df(updated_data)
  #   
  #   #Reset model_selected_df to origial data
  #   models_selected_df$data[[input$modelof]] <- target_df()
  #   showNotification(paste("Selections for", input$modelof, "reset successfully!"), type = "message")
  # })
  
  # Reset selected models
  observeEvent(input$unsave_bttn,{
    req(input$select_m)

    # Reset selected_model_df
    updated_data <- selected_models_df()[!(selected_models_df()$Index %in% input$select_m),]
    selected_models_df(updated_data)
    # models_selected_df$data[[input$modelof]] <- target_df()
    showNotification(paste("Selections for", input$modelof, "reset successfully!"), type = "message")
  })
  
  #model selected rows
  output$L0_file_contents <- renderRHandsontable({
    req(selected_models_df())
    df <- selected_models_df() 
    df <- df %>%
      dplyr::select(method,Channel,Brand,Variant,PackType,PPG,selectedmodels,RPIto,Adj.Rsq,AIC,MCV.MCV,CSF.CSF,actualdistvar,Index,initial_val)
    dataframe1 <- rhandsontable(df) %>% hot_cols(readOnly = TRUE)
    return(dataframe1)
  })
  
  # model updating process
  selected_models_temp <- reactive({
    req(selected_models_df(),L0_df())
    
    current_df <- L0_df()
    selected_df <- selected_models_df()
    
    updated_current_df <- anti_join(current_df,selected_df,by = c("Channel", "Brand", "Variant", "PackType", "PPG")) %>% 
      bind_rows(selected_df) %>% 
      arrange(Index)
    
    # updated_current_df <- bind_rows(updated_current_df,selected_df)
    # updated_current_df <- updated_current_df %>% arrange(Index) #arrange
    
    return(updated_current_df)
  })

  # final updated model selected file(Wtd_avg_MCV_File)
  final_selected_models <- reactive({
    req(selected_models_temp(),L0_df())
    
    current_df <- L0_df()
    updated_df <- selected_models_temp()

    updated_current_df <- anti_join(current_df,updated_df, by = "Index") %>% 
      mutate(selectedmodels =  as.character(1)) %>% # deselect that models
      bind_rows(updated_df)
    
    # updated_current_df <- updated_current_df %>%  mutate(selectedmodels =  as.character(1)) # deselect that models
    # updated_current_df <- bind_rows(updated_current_df,updated_df)

    return(updated_current_df)
    # current_df$selectedmodels <- as.character(current_df$selectedmodels)
    # return(current_df)
  })
  
  observeEvent(input$save_bttn, {
    req(input$modelof, final_selected_models())
    
    selected_models_store$data[[input$modelof]] <- final_selected_models()
    # print(dim(selected_models_store$data[[input$modelof]]))
  })
  
  
  # #Upadte selection upon saving
  # observeEvent(input$save_button, {
  #   req(input$modelof, final_selected_models())
  #   models_selected_df$data[[input$modelof]] <- final_selected_models()
  #   showNotification(paste("Selections for", input$modelof, "saved successfully!"), type = "message")
  # })
  
  # # Debugging: Print contents of models_selected_df to verify
  # observe({
  #   req(input$modelof)
  #   print(paste("Current modelof:", input$modelof))
  #   print(paste("Has saved data C:", !is.null(models_selected_df$data[[input$modelof]]))) #&& nrow(models_selected_df$data[[input$modelof]]) > 0))
  #   # print("Contents of models_selected_df$data[[input$modelof]]:")
  #   # print(str(models_selected_df$data[[input$modelof]]))
  #   # print("Contents of models_selected_df$data:")
  #   # print(str(models_selected_df$data))
  # })
  
  # #Retrieve Selections when switching models
  # current_selection <- reactive({
  #   req(input$modelof)
  #   if (!is.null(models_selected_df$data[[input$modelof]])) {
  #     selected_models$data[[input$modelof]]
  #   } else {
  #     # Initialize with a default or empty data frame
  #     L0_df()
  #   }
  # })
  
  # Final updated model selected file
  output$allmodels <- renderRHandsontable({
    req(final_selected_models())
    
    #selected_models_store$data[[input$modelof]]
    df <- final_selected_models() %>%
      filter(selectedmodels == "Yes") %>%
      dplyr::select(method,Channel,Brand,Variant,PackType,PPG,selectedmodels,RPIto,Adj.Rsq,AIC,MCV.MCV,CSF.CSF,actualdistvar,Index,initial_val) %>%
      arrange(Channel,Brand,Variant,PackType,PPG)

    dataframe1 <- rhandsontable(df) %>% hot_cols(readOnly = TRUE)
    return(dataframe1)
  })
  # output$allmodels <- renderRHandsontable({
  #   req(models_selected_df$data[[input$modelof]])
  #   df <- models_selected_df$data[[input$modelof]] %>%
  #     filter(selectedmodels == "Yes") %>%
  #     dplyr::select(method, Channel, Brand, Variant, PackType, PPG, selectedmodels, RPIto, Adj.Rsq, AIC, MCV.MCV, CSF.CSF, actualdistvar, Index, initial_val) %>%
  #     arrange(Channel, Brand, Variant, PackType, PPG)
  #   rhandsontable(df) %>% hot_cols(readOnly = TRUE)
  # })

  
  # Toggle visibility with a smooth slide animation
  observeEvent(input$toggle_table, {
    toggle("table_container", anim = TRUE, animType = "slide", time = 0.5)
  })
  
  output$L0_file_filtered <- DT::renderDataTable({
    req(final_selected_models())
    df <- final_selected_models()
    dataframe1 <- DT::datatable(df, options = list(scrollX = TRUE))
    return(dataframe1)
  })
  
  # Upadate original files
  observeEvent(input$update_file_bttn,{
    req(final_selected_models(), input$L0_file, input$modelof)
    
    #Select file filepath which is choosen in input$modelof
    # print(paste('modelof',input$modelof))
    file_path <- input$L0_file[which(grepl(input$modelof, tools::file_path_sans_ext(basename(input$L0_file)), ignore.case = TRUE))]
    # file_path <- normalizePath(file_path, mustWork = FALSE)
    # print(file_path)
    
    # # Ensure correct file path handling
    # file_path <- normalizePath(input$L0_file, mustWork = FALSE)
    # print(file_path)
    
    tryCatch({
      # Read the existing Excel file
      wb <- loadWorkbook(file_path)
      
      # Check if "FinalM0" sheet exists; if Yes, delete it
      if ('FinalM0' %in% names(wb)) {
        removeWorksheet(wb, "FinalM0")
      }
      
      #  Add updated data as "FinalM0:
      addWorksheet(wb, "FinalM0" )
      writeData(wb, "FinalM0", final_selected_models())
      
      # save workbook ar the same location
      saveWorkbook(wb, file_path, overwrite = TRUE)
      
      # Show success message
      showNotification(paste(input$modelof,"file updated successfully!",sep = " "), type = "message", duration = 5)
      
    }, error = function(e){
      # Show error message if something goes wrong
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
    
  })
  
  
  # Reactive expression for the plot
  plot_reactive <- reactive({
    req(final_df(), input$x_var, input$y_var)
    df <- final_df()
    # print(paste("Sorting by:", input$sort_by))
    #sort the data 
    df <- df[order(df[[input$sort_by]], decreasing = TRUE), ]
    df <- df %>% mutate(across(all_of(input$x_var),~factor(.,levels=.)))
    df <- df %>% mutate(hover_text = paste("Index:",Index,"<br>CSF:",CSF.CSF,"<br>MCV",MCV.MCV,"<br>Adj Rsq:",Adj.Rsq,"<br>AIC:",AIC))
    
    #Extract Yellow an green color from viridis color palette
    yellow_color <- "#08519C" #viridis(256, option = "D")[256] #"#FDE725FF" "#08519C"
    green_color <- "#006D2C" #viridis(256, option = "D")[128]  #"#21908DFF"
    
    #Assign fixed colors for OwnPrice and NetSegment
    color_list <- c("OwnPrice" = yellow_color, "NetSegment" = green_color)
    
    #Assign random colors for other categories(except OwnPrice and NetSegment)
    other_rpito <- setdiff(unique(df$RPIto), c("OwnPrice","NetSegment"))
    
    #Combine multiple palettes (e.g., Set3, Paired, Dark2) to generate 50 distinct colors
    palette_1 <- brewer.pal(12, "Set3")
    palette_2 <- brewer.pal(12, "Paired")
    palette_3 <- brewer.pal(8, "Set2")
    # Combine all palettes together to get 50 distinct colors
    combined_colors <- c(palette_1, palette_2, palette_3)
    
    
    # Remove the colors which are in color_list from the Set1 palette
    # available_colors <- setdiff(viridis(256), unname(color_list))
    available_colors <- setdiff(combined_colors, unname(color_list))
    
    # Sample random colors from the remaining palette (excluding Fixed color)
    random_colors <- sample(available_colors, length(other_rpito)) #
    
    # Add random colors for other categories
    color_list_final <- c(color_list,setNames(random_colors,other_rpito))
    
    if (input$plot_type == "Scatter Plot") {
      p <- plot_ly(df, x = ~get(input$x_var), y = ~get(input$y_var), type = "scatter", mode = "markers")
    } else if (input$plot_type == "Box Plot") {
      p <- plot_ly(df, x = ~get(input$x_var), y = ~get(input$y_var), type = "box")
    } else if (input$plot_type == "Bar Plot") {
      p <- plot_ly(df, x = ~get(input$x_var), y = ~get(input$y_var), type = "bar",color = ~RPIto,colors = color_list_final, hovertext=~hover_text, hoverinfo="text")
    }
    
    # Apply plot settings
    p <- p %>% layout(title = paste(input$channel,"-",input$brand),
                      xaxis = list(title = input$x_var),
                      yaxis = list(title = input$y_var),
                      hovermode = "closest",
                      barmode = "group")
    return(p)
  })
  
  # Render Plot
  output$plot <- renderPlotly({
    plot_reactive()
  })
  
  # # Download Plot
  # output$download_plot <- downloadHandler(
  #   filename = function() { paste(input$plot_type, "plot.html", sep = "_") },
  #   content = function(file_1) { 
  #     htmlwidgets::saveWidget(plot_reactive(), file_1) 
  #   }
  # )
  ###################################################################### Dashbroad End ############################################################################

  ########################################################### M1 to Result:-Sidebar start ###########################################################
  # output$toggle_ui <- renderUI({
  #   actionButton("toggle", "Toggle Sidebar")
  # })
  # observeEvent(input$toggle,{
  #   runjs("
  #         var sidebar = document.getElementById('sidebar_1');
  #         var main = document.getElementById('main_1');
  #         if (sidebar.style.display === 'none') {
  #           sidebar.style.display = 'block';
  #           main.classList.remove('expanded-main');
  #         } else {
  #           sidebar.style.display = 'none';
  #           main.classList.add('expanded-main');
  #         }
  #     ")
  # })
  
  # #Uplaod D0 file
  # D0_df <- reactive({
  #   inFile <- input$salesfile
  #   if (is.null(inFile))
  #     return(NULL)
  # 
  #   if (str_sub(inFile$datapath,str_length(inFile$datapath)-3,str_length(inFile$datapath)) == ".csv") {
  #     D0_df <- read.csv(inFile$datapath) #, header = input$header
  #   } else{
  #     D0_df <- read.xlsx(inFile$datapath)
  #   }
  # })
  
  # #All columns data
  # output$D0_file_contents <- DT::renderDataTable({
  #   DT::datatable(D0_df())
  # })

  # D0_file name 
  D0_file_name <- reactive({
    req(D0_df())
    if(is.null(input$D0_file)){
      return(NULL)
    }else{
      return(basename(input$D0_file))
    }
  })
  # observe({print(D0_file_name())})
  
  
  ## Trigger Script Execution 
  observeEvent(input$run_process, {
    output$process_status <- renderText("Processing...Please wait.")
    
    # Capture user selections from UI
    project_root <<- dir_path()
    Base_Path <<- project_root
    path <<-  project_root
    
    print(paste("Debug: project_root =", project_root))
    print(paste("Debug: Base_Path =", Base_Path))
    
    # # Check if Base_Path is NULL or empty
    # if (is.null(Base_Path) || Base_Path == "") {
    #   stop("Error: Base_Path is empty or not set correctly.")
    # }
    
    
    # D0_file_path <- input$salesfile$datapath  # File path of uploaded file
    # Extract file name with extensio
    
    D0_file <<-  D0_file_name() #input$salesfile$name #"KHC_Spoonables_USA_D0_modelling.csv" #input$salesfile #"KHC_Spoonables_USA_D0_modelling.csv" #input$salesfile
    print(paste("Debug: D0_file =", D0_file))
    
    worktype <<- input$worktype
    Integration_process_needed <<- input$integration_needed
    input_type <<-   input$input_type #"Brand_Variant"
    L0_indicator <<- input$L0_indicator
    L1_indicator <<- input$L1_indicator
    L2_indicator <<- input$L2_indicator
    L3_indicator <<- input$L3_indicator
    datafrequency <<- input$datafrequency
    print(paste("Debug: datafrequency =", datafrequency))
    
    csf_period <<- as.numeric(input$csf_period)
    CMA_input <<- 30  # Can be UI-controlled
    NSV_input <<- 80  # Can be UI-controlled
    restofcategory_included <<- "Yes"
    
    #Source required scripts
    # print(paste("Checking file path:", paste(project_root, worktype, "/8. Codes/functions", "R_Packages.R", sep = "/")))
    
    source(file.path(project_root, worktype, "8. Codes/functions", "R_Packages.R"))
    source(file.path(project_root, worktype, "8. Codes/functions", "Integrator.R"))
    
    # Run integration if needed
    if (Integration_process_needed == "Yes") {
      integrator1(project_root, Integration_process_needed, input_type, worktype)
    }
    
    # Run main output process
    source(file.path(project_root, worktype, "8. Codes/functions", "Output Main2.R"))
    result <- Output_Main2(project_root, Integration_process_needed, input = input_type, 
                           L0_indicator, L2_indicator, L3_indicator, L1_indicator, 
                           datafrequency, CMA_input, NSV_input, D0_file, 
                           restofcategory_included, worktype, csf_period)
    # print("Calling Output_Main2 function")
    # print(paste("Project Root:", project_root))
    # print(paste("D0 File Path:", D0_file_path))
    
    
    # Display status updates
    output$process_output <- renderText(result)
    output$process_status <- renderText("Process Completed Successfully!")

  })
  

  ########################################################### M1 to Result:-Sidebar end ################################################################
  
}

