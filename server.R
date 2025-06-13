
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
  
  observe({
    req(file_list())  
    print(names(file_list()))
    })
  
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
  
  #Store the data in selected_models_store
  observeEvent(input$upload_btn,{
    
    inputs_indicator <- list(
      L0_indicator = input$L0_indicator,
      L2_indicator = input$L2_indicator,
      L3_indicator = input$L3_indicator
    )
    # print(paste("!is.na(input$L0_indicator) :",!(input$L0_indicator=="NA")))
    for(indicator_name in names(inputs_indicator)){
      
      input_val <- inputs_indicator[[indicator_name]]
      
      # Skip if input_val is "NA" or NA
      if (!is.null(input_val) && input_val != "NA" && !is.na(input_val)) {
        #Assign reactive table based on input_na,e
        selected_models_store$data[[input_val]] <- switch(indicator_name,
                                                          L0_indicator = L0_table(),
                                                          L2_indicator = L2_table(),
                                                          L3_indicator = L3_table()
                                                        )
      }
    }
  })
  
  #select targeting file
  target_df <- reactive({
    req(input$modelof, any(!is.null(L0_table()), !is.null(L2_table()), !is.null(L3_table())))
  
    print(input$modelof)
    print(!is.null(selected_models_store$data[[input$modelof]]))
    # Check if store data for this mdel 
    if (!is.null(selected_models_store$data[[input$modelof]])) {
      
      return(selected_models_store$data[[input$modelof]])
      
    }else{NULL}
    # else if (input$modelof == input$L0_indicator) {
    #   return(L0_table())
    # }else if(input$modelof == input$L2_indicator){
    #   return(L2_table())
    # }else if(input$modelof == input$L3_indicator){
    #   return(L3_table())
    # }else {
    #   return(NULL)
    # }
    
  })
  

  #Retrieve Selections when switching models
  L0_df <- reactive({
    req(input$modelof, file_list())
    #Get current data
    current_data <- target_df()
    return(current_data)
    })

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
  
  ################ Double Click selection-Start ##############
  click_tracker <- reactiveValues(last_click = NULL, last_index = NULL)
  
  observeEvent(event_data("plotly_click", source = "modelplot"), {
    click_data <- event_data("plotly_click", source = "modelplot")
    req(click_data)
    
    current_time <- Sys.time()
    clicked_index <- click_data$x
    
    # If previous click was on same index and within 0.5 seconds => treat as double-click
    if (!is.null(click_tracker$last_click) &&
        difftime(current_time, click_tracker$last_click, units = "secs") < 0.5 &&
        click_tracker$last_index == clicked_index) {
      
      # Simulated double-click: update the input
      updateSelectizeInput(session, "select_m", selected = clicked_index)
      
      showNotification(paste("Model Index", clicked_index, "selected via double-click"), type = "message")
    }
    
    # Update click tracker
    click_tracker$last_click <- current_time
    click_tracker$last_index <- clicked_index
  })
  ################# Double Click selection-End ##############
  
  ### Data frame aftr model selection
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
  
  observe({
    req(input$modelof, final_selected_models())

    selected_models_store$data[[input$modelof]] <- final_selected_models()
    # print(dim(selected_models_store$data[[input$modelof]]))
  })
  
  observe({
    req(selected_models_store)
    print(paste("check list",names(selected_models_store$data)))
  })
  
  # Final updated model selected file for visualization
  allmodels_df <- reactive({
    req(final_selected_models())
    df <- final_selected_models() %>%
      filter(selectedmodels == "Yes") %>%
      dplyr::select(method,Channel,Brand,Variant,PackType,PPG,selectedmodels,RPIto,Adj.Rsq,AIC,MCV.MCV,CSF.CSF,lastyravgprice,actualdistvar,Index,initial_val) %>%
      arrange(Channel,Brand,Variant,PackType,PPG)
    return(df)
  })
  
  ##########
  # selected model Data frame View
  output$allmodels <- renderRHandsontable({
    req(final_selected_models())
    
    #selected_models_store$data[[input$modelof]]
    df <- allmodels_df()
    
    dataframe1 <- rhandsontable(df) %>% hot_cols(readOnly = TRUE)
    return(dataframe1)
  })
  
  # Toggle visibility with a smooth slide animation FOR dATA fRAME
  observeEvent(input$toggle_table, {
    toggle("table_container", anim = TRUE, animType = "slide", time = 0.5)
  })
  # Toggle visibility with a smooth slide animation FOR GRAPHS CSF,MCV, PRICE
  observeEvent(input$toggle_table_1, {
    toggle("table_container_1", anim = TRUE, animType = "slide", time = 0.5)
  })
  
  # output$L0_file_filtered <- DT::renderDataTable({
  #   req(final_selected_models())
  #   df <- final_selected_models()
  #   dataframe1 <- DT::datatable(df, options = list(scrollX = TRUE))
  #   return(dataframe1)
  # })
  
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
  
  ################### Reactive expression for the plot
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
      p <- plot_ly(df, x = ~get(input$x_var), y = ~get(input$y_var), type = "bar",color = ~RPIto,colors = color_list_final, hovertext=~hover_text, hoverinfo="text",source = "modelplot") %>% event_register("plotly_click")
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
  
  ################### Stacked Bar chart
  stacked_plot_reactive <- reactive({
    req(allmodels_df(), input$modelof, input$channel)
    
    # View(allmodels_df())
    df <- allmodels_df() %>% 
      dplyr::filter(Channel==input$channel) %>% 
      dplyr::select(Channel,input$modelof, MCV.MCV, CSF.CSF, lastyravgprice, initial_val) %>% 
      mutate(TopBar = MCV.MCV - lastyravgprice,
             Price_mid = lastyravgprice/2,
             CSF_pos = lastyravgprice + (TopBar*0.1),#0.5,#(TopBar/10),
             MCV_top = MCV.MCV + 0.05) %>%
      mutate(border_col = ifelse(initial_val == 'median', "red","#0E2841"))
    # View(df)
    
    #Create Stacked bar plot
    p <- plot_ly(df, x = ~get(input$modelof)) %>% 
      # Bottom: Price
      add_trace(y = ~lastyravgprice, 
                type = 'bar', 
                name = "Price",
                marker= list(color = "#156082", line = list(color = ~border_col, width = 2)),#list(color = '#08519C'),
                text = ~paste("Price:",round(lastyravgprice,2)),#~CSF_lable, textposition = "inside", insidetextanchor = "start",
                textposition = "inside",
                insidetextanchor = "middle",
                textfont = list(color = 'white', size = 12),
                hoverinfo = "text",
                hovertext = ~paste("For:", input$modelof,
                                   "<br>MCV:", round(MCV.MCV, 2),
                                   "<br>CSF:", round(CSF.CSF, 2),  
                                   "<br>Price:", round(lastyravgprice, 2))) %>%
      # "#F0F8FF""#B0E0E6""#BFEFFF""#D6F0FF""#87CEEB""#87CEFA""#000080""#00008B""#191970""#003153""#002366""#2A2F4A""#0A1E5E" '#08519C'  "#156082""#DCEAF7""#0E2841"
      #Top bar:MCV extra
      add_trace(y = ~TopBar, 
                type = 'bar', 
                name = 'MCV Extra',
                marker= list(color = "#DCEAF7", line = list(color = ~border_col, width = 2)), #list(color = 'lightblue'),
                # text = ~paste("MCV:",round(MCV.MCV,2)),
                # textposition = "inside",
                # insidetextanchor = "end",
                # textfont = list(color = "black", size=12),
                hoverinfo = "text",
                hovertext = ~paste("For:", input$modelof,
                                   "<br>MCV:", round(MCV.MCV, 2),
                                   "<br>CSF:", round(CSF.CSF, 2),  
                                   "<br>Price:", round(lastyravgprice, 2))) %>% 
      
      # CSF label
      add_text(x = ~get(input$modelof), y = ~CSF_pos,
               text = ~paste("CSF:", round(CSF.CSF,2)),
               textposition = "inside",
               insidetextanchor = "end",
               textfont = list(size = 12, color = "#0E2841",family = "Arial Black"), 
               showlegend = FALSE,
               hoverinfo = "skip"
               ) %>% 
      # MCV label
      add_text(x = ~get(input$modelof), y = ~MCV_top,
               text = ~paste("MCV:",round(MCV.MCV,2)),
               textposition = "top center",
               textfont = list(size = 12, color = 'black'),
               sgowlegend = FALSE,
               hoverinfo = "skip")
  
      # Layout
      p <- p %>% 
        layout(barmode = 'stack',
             title = paste(input$channel, "-", input$modelof),
             xaxis = list(title = input$modelof),
             yaxis = list(title = paste("Price"," ","CSF"," ","MCV"),sep = " "),
             hovermode = "closest",
             showlegend = FALSE)
    
    return(p)
  })
  output$stacked_plot_op <- renderPlotly({
    stacked_plot_reactive()
  })
  
  ###################
  ########## Graph for MSP, CSF, price
  
  #Grpah for MCV
  mcv_plot_reactive <- reactive({
    req(allmodels_df(), input$modelof, input$channel)
    
    df <- allmodels_df() %>% 
      dplyr::select(Channel,input$modelof, MCV.MCV) %>% 
      dplyr::filter(Channel==input$channel)
    p <- plot_ly(df, x = ~get(input$modelof), y = ~MCV.MCV, type = "bar",
            color = ~get(input$modelof),colors = "Set2")#hovertext=~hover_text, hoverinfo="text"
    
    # Apply plot settings
    p <- p %>% layout(title = paste(input$channel,"-",input$modelof),
                      xaxis = list(title = input$modelof),
                      yaxis = list(title = "MCV"),
                      hovermode = "closest",
                      barmode = "group")
    return(p)
  })
  
  # Render Plot for selected model
  output$mcv_plot <- renderPlotly({
    mcv_plot_reactive()
  })
  
  #Grpah for CSF
  csf_plot_reactive <- reactive({
    req(allmodels_df(), input$modelof, input$channel)
    
    df <- allmodels_df() %>% 
      dplyr::select(Channel,input$modelof, CSF.CSF) %>% 
      dplyr::filter(Channel==input$channel)
    p <- plot_ly(df, x = ~get(input$modelof), y = ~CSF.CSF, type = "bar",
                 color = ~get(input$modelof),colors = "Set2")#hovertext=~hover_text, hoverinfo="text"
    
    # Apply plot settings
    p <- p %>% layout(title = paste(input$channel,"-",input$modelof),
                      xaxis = list(title = input$modelof),
                      yaxis = list(title = "CSF"),
                      hovermode = "closest",
                      barmode = "group")
    return(p)
  })
  
  # Render Plot for selected model
  output$csf_plot <- renderPlotly({
    csf_plot_reactive()
  })
  
  #Grpah for Price
  price_plot_reactive <- reactive({
    req(allmodels_df(), input$modelof, input$channel)
    
    df <- allmodels_df() %>% 
      dplyr::select(Channel,input$modelof, lastyravgprice) %>% 
      dplyr::filter(Channel==input$channel)
    p <- plot_ly(df, x = ~get(input$modelof), y = ~lastyravgprice, type = "bar",
                 color = ~get(input$modelof),colors = "Set2")#hovertext=~hover_text, hoverinfo="text"
    
    # Apply plot settings
    p <- p %>% layout(title = paste(input$channel,"-",input$modelof),
                      xaxis = list(title = input$modelof),
                      yaxis = list(title = "Price"),
                      hovermode = "closest",
                      barmode = "group")
    return(p)
  })
  
  # Render Plot for selected model
  output$price_plot <- renderPlotly({
    price_plot_reactive()
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
  
  #msp file storage
  msp_storage <- reactiveValues(data =list())
  
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
                           restofcategory_included, worktype, csf_period, D0_Data = D0_df(), selected_models_store = selected_models_store)
    # print("Calling Output_Main2 function")
    # print(paste("Project Root:", project_root))
    # print(paste("D0 File Path:", D0_file_path))
    # print(paste("result :",names(result)))
    # print(paste("result 1 :",View(result[["L0L2.csv"]][["allMSP"]])))
    
    level_list <- names(result)
    for(lev_i in 1:length(level_list)){
      level_selected <- level_list[lev_i]
      
      if(!is.null(result[[level_selected]][["allMSP"]])){
        print(paste("✅ Stored allMSP for:", level_selected))
        msp_storage$data[[level_selected]] <- result[[level_selected]][["allMSP"]]
      }else{
        msp_storage$data[[level_selected]] <- NULL
        print(paste("⚠️ No allMSP found for:", level_selected))
      }
    }
    
    # print(paste("names(msp_storage$data) :",names(msp_storage$data)))
    # print(paste("names(msp_storage$data) data:",View(msp_storage$data[[level_list[length(level_list)]]])))
    
    
    # Display status updates
    # output$process_output <- renderText(result)
    output$process_status <- renderText("Process Completed Successfully!")
  })
  
  ########################################################### M1 to Result:-Sidebar end ################################################################
  
  ##################################################### START CREATING GRAPHS AFTER INTEGRATOR CODE ########################################################################################
  # Add msp files for further use
  # observe({print(paste("names(msp_storage$data) :",names(msp_storage$data)))})
  
  # # MSP file
  # output$available_levels <- renderUI({
  #   selectInput("selected_level", "Select Level", choices = names(msp_storage$data))
  # })
  # 
  # 
  # output$msp_file <- renderRHandsontable({
  #   req(input$selected_level)
  #   req(msp_storage$data[[input$selected_level]])
  #   
  #   df <- msp_storage$data[[input$selected_level]]
  #   
  #   # print(paste("class :",class(df)))
  #   # view(df)
  #   if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
  #     return(NULL)  # Don't render anything
  #   }
  # 
  #   dataframe1 <- rhandsontable(df) %>% hot_cols(readOnly = TRUE)
  #   return(dataframe1)
  # })
  
  ##################################### MSP Graphs
  ## Select level for graph
  # # MSP file
  # output$available_level_op <- renderUI({
  #   levels = names(msp_storage$data)
  #   selectInput("graph_level_ip", "Graph for", choices = levels)
  # })
  # 
  # # Reactieve to get Selected level data 
  # level_df <- reactive({
  #   req(input$graph_level_ip)
  #   req(msp_storage$data[[input$graph_level_ip]])
  # 
  #   df <- msp_storage$data[[input$graph_level_ip]]
  #   # print(paste("class :",class(df)))
  #   # view(df)
  #   if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
  #     return(NULL)  # Don't render anything
  #   }
  #   return(df)
  # })
  
  # #Reactieve to get Selected L0 level data
  # L0_level_df <- reactive({
  #   req(msp_storage$data)
  #   req(names(msp_storage$data)[1])
  #   level <- names(msp_storage$data)[1]
  # 
  #   df <- msp_storage$data[[level]]
  #   # print(paste("class :",class(df)))
  #   # view(df)
  #   if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
  #     return(NULL)  # Don't render anything
  #   }
  #   return(df)
  # })
  
  ############################ MSP L0 Level Graph
  ## L0_level Graphs title
  output$L0msp_title <- renderUI({
    HTML(paste("<div style='text-align:center; font-size:18px; font_weight:bold;'>MSP across ",input$L0_indicator,"</div>"))
  })
  
  # select channel, Dynamic channel dropdown based on selected lavel.
  output$L0channel_op <- renderUI({
    req(msp_storage$data)
    req(names(msp_storage$data)[1])
    # req(L0_level_df())
    # df <- L0_level_df()
    level <- names(msp_storage$data)[1]
    df <- msp_storage$data[[level]]
    channels <- unique(na.omit(df$Channel))
    selectInput("L0channel_ip", "Select Channel", choices = channels)
  })
  
  # Reactive for graphs data
  L0grapha_df <- reactive({
    # req(L0_level_df(), input$channel_ip)
    req(msp_storage$data)
    req(names(msp_storage$data)[1])
    level <- names(msp_storage$data)[1]
    df <- msp_storage$data[[level]]
    df <- df[df$Channel %in% input$L0channel_ip,]
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
      return(NULL)  # Don't render anything
    }
    return(df)
  })
  
  # observe({
  #   view(grapha_df())
  # })
  
  # Reactive expression for plot
  L0msp_plotly_reactive <- reactive({
    req(L0grapha_df())
    df <- L0grapha_df()
    # View(df)
    
    df <- df %>% 
      dplyr::select(Channel, input$L0_indicator, MSP, CSF, MCV, Price, MShare, NewMShare ) %>%
      mutate(hover_text =  paste("Channel:",Channel, 
                                 paste0("<br>",input$L0_indicator,":"),get(input$L0_indicator), 
                                 "<br>MCV:", paste(round(MCV,2)),
                                 "<br>CSF:", paste(round(CSF,2)),
                                 "<br>Price:", paste(round(Price,2)),
                                 "<br>MShare:", paste(round(MShare,4)*100,"%"),
                                 "<br>NewMShare:", paste(round(NewMShare,4)*100,"%")
                                 )
             )
    
    p <- plot_ly(data = df, x = ~get(input$L0_indicator), y = ~MSP, 
                 type = 'bar', 
                 color = ~get(input$L0_indicator), colors = "Set2", 
                 text = ~paste(round(MSP,4)*100,"%"),
                 textposition = "inside",
                 insidetextanchor = "end",
                 textfont = list(color = 'Black', size = 12),
                 hovertext= ~ hover_text,
                 hoverinfo = "text"
                 # marker = list(
                 #   line = list(color = 'black', width = 1.5)
                 # )
                 )
    
    #Apply plot settings
    p <- p %>% layout(title = NULL,#'MSP',
                      xaxis = list(title = input$L0_indicator),
                      yaxis = list(title = "MSP (%)"),
                      hovermode = "closest",
                      barmode = "group"
                      # margin = list(b=100) #Give extra space for long brnad names
                      )
    return(p)
  })
  
  # Render plot
  output$L0msp_plot <- renderPlotly({
    L0msp_plotly_reactive()
  })
  
  
  ############################ MSP L0L2 Level Graph
  ## L0L2_level Graphs title
  output$L0L2msp_title <- renderUI({
    HTML(paste("<div style='text-align:center; font-size:18px; font_weight:bold;'>MSP across ",input$L0_indicator,"-",input$L2_indicator,"</div>"))
  })
  
  # select channel, Dynamic channel dropdown based on selected lavel.
  output$L0L2channel_op <- renderUI({
    req(msp_storage$data)
    req(names(msp_storage$data)[2])
    # req(L0_level_df())
    # df <- L0_level_df()
    level <- names(msp_storage$data)[2]
    df <- msp_storage$data[[level]]
    channels <- unique(na.omit(df$Channel))
    selectInput("L0L2channel_ip", "Select Channel", choices = channels)
  })
  # select Level0, Dynamic Brand dropdown based on selected lavel.
  output$L0L2Brand_op <- renderUI({
    req(msp_storage$data)
    req(names(msp_storage$data)[2],input$L0L2channel_ip )
  
    level <- names(msp_storage$data)[2]
    df <- msp_storage$data[[level]] %>% dplyr::filter(Channel == input$L0L2channel_ip)
    Brands <- unique(na.omit(df$Brand))
    selectInput("L0L2brand_ip", "Select Brand", choices = c("All",Brands), selected = "All")
  })
  
  # Reactive for graphs data
  L0L2grapha_df <- reactive({
    req(msp_storage$data)
    req(names(msp_storage$data)[2], input$L0L2channel_ip,input$L0L2brand_ip)
    
    level <- names(msp_storage$data)[2]
    df <- msp_storage$data[[level]]
    if (!is.null(input$L0L2brand_ip) && input$L0L2brand_ip != "All") {
      df <- df %>% dplyr::filter(Channel == input$L0L2channel_ip) %>% dplyr::filter(Brand == input$L0L2brand_ip)
    }else{
      df <- df %>% dplyr::filter(Channel == input$L0L2channel_ip)
    }
    # df <- df[df$Channel %in% input$channel_ip,]
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
      return(NULL)  # Don't render anything
    }
    return(df)
  })
  
  # observe({
  #   view(grapha_df())
  # })
  
  # Reactive expression for plot
  L0L2msp_plotly_reactive <- reactive({
    req(L0L2grapha_df())
    df <- L0L2grapha_df()
    # View(df)
    level0 <- input$L0_indicator # Brand
    level2 <- input$L2_indicator # Variant
    
    df <- df %>% 
      dplyr::select(Channel, level0,level2, MSP, CSF, MCV, Price, MShare, NewMShare ) %>%
      mutate(hover_text =  paste("Channel:",Channel, 
                                 paste0("<br>",level0,":"),get(level0),
                                 paste0("<br>",level2,":"),get(level2),
                                 "<br>MCV:", paste(round(MCV,2)),
                                 "<br>CSF:", paste(round(CSF,2)),
                                 "<br>Price:", paste(round(Price,2)),
                                 "<br>MShare:", paste(round(MShare,4)*100,"%"),
                                 "<br>NewMShare:", paste(round(NewMShare,4)*100,"%"))) %>% 
      arrange(Channel, level0, level2)
    
    p <- plot_ly(data = df, x = ~get(level0), y = ~MSP, 
                 type = 'bar',
                 barmode = 'group',
                 color = ~get(level2), colors = "Set2", 
                 text = ~paste(round(MSP,4)*100,"%"),
                 textposition = "inside",
                 insidetextanchor = "end",
                 textfont = list(color = 'Black', size = 12),
                 hovertext= ~ hover_text,
                 hoverinfo = "text"
                 # marker = list(
                 #   line = list(color = 'black', width = 1.5)
                 # )
    )
    
    #Apply plot settings
    p <- p %>% layout(title = NULL,#'MSP',
                      xaxis = list(title = paste0(level0,"-",level2),type = 'category'),
                      yaxis = list(title = "MSP (%)"),
                      hovermode = "closest",
                      # barmode = "group"
                      bargap = 0.3, # space between category group
                      bargroupgap = 0.1 # space between bar in a group
                      # margin = list(b=100) #Give extra space for long brnad names
    )
    return(p)
  })
  
  # Render plot
  output$L0L2msp_plot <- renderPlotly({
    L0L2msp_plotly_reactive()
  })
  
  ######################## MSP L0L2L3 level graph
  
  ## L0L2_level Graphs title
  output$L0L2L3msp_title <- renderUI({
    HTML(paste("<div style='text-align:center; font-size:18px; font_weight:bold;'>MSP across ",input$L0_indicator,"-",input$L2_indicator,"-",input$L3_indicator,"</div>"))
  })
  # select channel, Dynamic channel dropdown based on selected lavel.
  output$L0L2L3channel_op <- renderUI({
    req(msp_storage$data, names(msp_storage$data)[3])
    
    level <- names(msp_storage$data)[3]
    df <- msp_storage$data[[level]]
    channels <- unique(na.omit(df$Channel))
    selectInput("L0L2L3channel_ip", "Select Channel", choices = channels)
  })
  # select Level0, Dynamic Brand dropdown based on selected lavel.
  output$L0L2L3Brand_op <- renderUI({
    req(msp_storage$data)
    req(names(msp_storage$data)[3], input$L0L2L3channel_ip)
    # req(L0_level_df())
    # df <- L0_level_df()
    level <- names(msp_storage$data)[3]
    df <- msp_storage$data[[level]] %>% dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip)
    
    # Brands <- unique(na.omit(df$Brand))
    Brands <- df %>% dplyr::select(input$L0_indicator) %>% drop_na() %>% dplyr::distinct() %>% pull()
    
    selectInput("L0L2L3brand_ip", paste("Select", input$L0_indicator,sep = " "), choices = c("All",Brands), selected = "All")
  })
  # select Level2, Dynamic PPG/Variant dropdown based on selected lavel.
  output$L0L2L3Variant_op <- renderUI({
    req(msp_storage$data)
    req(names(msp_storage$data)[3], input$L0L2L3channel_ip, input$L0L2L3brand_ip)
    # req(L0_level_df())
    # df <- L0_level_df()
    level <- names(msp_storage$data)[3]
    
    if (input$L0L2L3brand_ip != "All") {
      df <- msp_storage$data[[level]] %>% 
        dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip) %>% 
        dplyr::filter(get(input$L0_indicator) == input$L0L2L3brand_ip)
    }else{
      df <- msp_storage$data[[level]] %>% 
        dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip) 
    }
    variants <- df %>% dplyr::select(input$L2_indicator) %>% drop_na() %>% dplyr::distinct() %>% pull()
    selectInput("L0L2L3Variant_ip", paste("Select", input$L2_indicator,sep = " "), choices = c("All",variants), selected = "All")
  })
  # Reactive for graphs data
  L0L2L3grapha_df <- reactive({
    # req(L0_level_df(), input$channel_ip)
    req(msp_storage$data)
    req(names(msp_storage$data)[3], input$L0L2L3channel_ip, input$L0L2L3brand_ip, input$L0L2L3Variant_ip)
    level <- names(msp_storage$data)[3]
    df <- msp_storage$data[[level]]
    # View(df)
    if (input$L0L2L3brand_ip != "All" && input$L0L2L3Variant_ip != 'All' ) {
      
      df <- df %>% dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip) %>% 
        dplyr::filter(get(input$L0_indicator) == input$L0L2L3brand_ip) %>% 
        dplyr::filter(get(input$L2_indicator) == input$L0L2L3Variant_ip)
    }else if(input$L0L2L3brand_ip != "All"){
      
      df <- df %>% dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip)%>% 
        dplyr::filter(get(input$L0_indicator) == input$L0L2L3brand_ip)
    }else if(input$L0L2L3Variant_ip != "All"){
      
      df <- df %>% dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip) %>% 
        dplyr::filter(get(input$L2_indicator) == input$L0L2L3Variant_ip)
    }else{
      df <- df %>% dplyr::filter(get(input$L1_indicator) == input$L0L2L3channel_ip)
    }
    # view(df)
    # df <- df[df$Channel %in% input$channel_ip,]
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
      return(NULL)  # Don't render anything
    }
    return(df)
  })
  # observe({
  #   view(L0L2L3grapha_df())
  # })
  
  # Reactive expression for plot
  L0L2L3msp_plotly_reactive <- reactive({
    req(L0L2L3grapha_df())
    df <- L0L2L3grapha_df()
    # View(df)
    level0 <- input$L0_indicator # Brand
    level1 <- input$L1_indicator
    level2 <- input$L2_indicator # Variant
    level3 <- input$L3_indicator # PPG
    
    df <- df %>% 
      dplyr::select(!!!rlang::syms(c(level1, level0,level2, level3)), MSP, CSF, MCV, Price, MShare, NewMShare ) %>%
      arrange(!!!rlang::syms(c(level1, level0,level2, level3)))
    
    # complete to make all L0-L2 combos for selected channel
    df <- df %>% 
      group_by(!!rlang::sym(level1)) %>% 
      tidyr::complete(
        !!rlang::sym(level0), 
        !!rlang::sym(level2), 
        !!rlang::sym(level3), 
        fill = list(MSP = 0, CSF = 0, MCV = 0, Price = 0, MShare = 0, NewMShare = 0)) %>% 
      ungroup()
    
    
    df <- df %>% 
      mutate(hover_text =  paste("Channel:",get(level1), 
                                 paste0("<br>",level0,":"),get(level0),
                                 paste0("<br>",level2,":"),get(level2),
                                 paste0("<br>",level3,":"),get(level3),
                                 "<br>MCV:", paste(round(MCV,2)),
                                 "<br>CSF:", paste(round(CSF,2)),
                                 "<br>Price:", paste(round(Price,2)),
                                 "<br>MShare:", paste(round(MShare,4)*100,"%"),
                                 "<br>NewMShare:", paste(round(NewMShare,4)*100,"%")))
  
    # View(df)
    
    #Create plot: one trace per PPG group
    p <- plot_ly()
    
    print(paste("unique(df[[level3]])) : ",unique(df[[level3]])))
    for (L3val in unique(df[[level3]])) {
      df_sub <- df[df[[level3]] == L3val, ]
      
      # optional: skip if df_sub is empty
      if(nrow(df_sub) == 0) next
      
      p <- p %>% 
        add_bars(
          x = list(df_sub[[level0]], df_sub[[level2]]),
          y = df_sub$MSP,
          name = as.character(L3val),
          text = paste0(round(df_sub$MSP,4)*100,"%"),
          textposition = "inside",
          insidetextanchor = "end",
          textfont = list(color = 'black', size = 11),
          hovertext = df_sub$hover_text,
          hoverinfo = "text"
        )
    }
    
    # p <- p %>%
    #   layout(
    #     xaxis = list(title = paste(level0, "&", level2), tickangle = -30),
    #     barmode = 'group',
    #     title = paste("MSP by", level0, "and", level2),
    #     yaxis = list(title = "MSP (%)"),
    #     showlegend = TRUE
    #   )
    # 
    
    # p <- plot_ly(data = df, x = ~interaction(get(level0), get(level3), sep = " | "), y = ~MSP, 
    #              type = 'bar',
    #              barmode = 'group',
    #              color = ~get(level2), colors = "Set2", 
    #              text = ~paste(round(MSP,4)*100,"%"),
    #              textposition = "inside",
    #              insidetextanchor = "end",
    #              textfont = list(color = 'Black', size = 12),
    #              hovertext= ~ hover_text,
    #              hoverinfo = "text"
    #              # marker = list(
    #              #   line = list(color = 'black', width = 1.5)
    #              # )
    # )
    
    # # # Apply plot settings
    # p <- p %>% layout(title = NULL,#'MSP',
    #                   barmode = "group",
    #                   xaxis = list(title = paste0(level3),type = 'multicategory'),#list(title = paste0(level0, " | ", level2), type = 'category'),#list(title = paste0(level0),type = 'multicategory'),
    #                   yaxis = list(title = "MSP (%)"),
    #                   hovermode = "closest",
    #                   # barmode = "group"
    #                   bargap = 0.3, # space between category group
    #                   bargroupgap = 0.1 # space between bar in a group
    #                   # margin = list(b=100) #Give extra space for long brnad names
    # )
    return(p)
  })
  
  # Render plot
  output$L0L2L3msp_plot <- renderPlotly({
    L0L2L3msp_plotly_reactive()
  })
  
  
}

