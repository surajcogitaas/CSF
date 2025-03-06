
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)

# options(shiny.error = browser)

#### Server ######
# Define the Server Logic
server <- function(input, output, session) {
  
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
    print(input$D0_file)
    req(input$D0_file)
    file <- input$D0_file
    
    # Ensure at least one file is selected
    if (is.null(file))
      return(NULL)
    
    # Read each file based on its extension
    ext <- tolower(tools::file_ext(file))
    if (ext == "csv") {
      D0_df <- read.csv(file)
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
    
    preprocess_fun <- function(file){
      # Read file based on its extension
      ext <- tolower(tools::file_ext(file))
      # Extract sheet name when excel file is uploaded
      if(ext %in% c("xls", "xlsx")){
        sheets <- excel_sheets(file)
        if (c("FinalM0") %in% sheets) {
          df <- read_excel(file,sheet = "FinalM0")
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
  # observe({print(input$L0_file)})
  

  ## median work
  median_df <- reactive({
    
    req(file_list)
    
    files <-  input$L0_file
    # print(files)
    median_fun <- function(file){
      # Read file based on its extension
      ext <- tolower(tools::file_ext(file))
      # Extract sheet name when excel file is uploaded
      if(ext %in% c("xls", "xlsx")){
        sheets <- excel_sheets(file)
        if ("median" %in% sheets) {
          df <- read_excel(file,sheet = "median")
        }else{
          #If median sheet does not exist
          df <- read_excel(file,sheet = "FinalM0")
          df <- df %>%
            mutate(selectedmodels = as.character(selectedmodels)) %>%  #Convert to character
            group_by(Channel, !!sym(input$modelof)) %>%
            mutate(median_val = median(CSF.CSF, na.rm=TRUE)) %>%
            ungroup() %>%
            mutate(selectedmodels = ifelse((selectedmodels =="1" ) & (CSF.CSF == median_val), "Yes", selectedmodels))
            # select(-median_val) #remove temp column
          
          # Extract rows where selectedmodels == "Yes"
          df <- df %>% filter(selectedmodels == "Yes")
          
          # Load existing workbook or create a new one
          wb <- loadWorkbook(file)
          addWorksheet(wb, "median")
          writeData(wb, "median", df)
          
          # Save changes to the same file
          saveWorkbook(wb, file, overwrite = TRUE)
          print("✅ 'median' sheet added successfully.")
        }
        return(df)
        
      }else{
        return(NULL)
      }
    }
    
    lst <- lapply(files, median_fun)
    
    files_names <- gsub("Wtd_avg_MCV_", "", tools::file_path_sans_ext(basename(files))) #"Brand","Variant","PackType","PPG"
    
    # Use file names (without extension) as names for the list elements
    lst <- setNames(lst,files_names)    #tools::file_path_sans_ext(basename(files))
    
    return(lst)
    
  })
  
  
  
  
  L0_df <- reactive({
    
    req(file_list(),input$modelof)
    #Cleaned files names
    files_names <- names(file_list()) #"Brand","Variant","PackType","PPG"
    
    #targetinf file
    target_file <- input$modelof
    
    #index of that file
    idx <- match(target_file,files_names)
    if (!is.na(idx)) {
      df <- file_list()[[idx]]
    } else {
      print("Invalid index or empty file_list")
      df <- NULL  # or handle the error as needed
    }
    
    return(df)
    
  })
  
  # ###Create median work
  # reactive()
  
  
  
  ## Filters
  # Check if necessary columns exist in the data

  # Dynamic Price_pval Filte
  output$price_pval_ui <- renderUI({
    req(L0_df())  # Ensure filtered dataset is available
    price_values <- unique(na.omit(L0_df()$Price_pval))  # Remove NULLs
    selectInput("price_pval", "Select Price P-Value:", choices = c("All", price_values), selected = "Yes")
  })
  
  # Dynamic Method Filter
  output$method_ui <- renderUI({
    req(L0_df())  # Ensure filtered dataset is available
    methods <- unique(na.omit(L0_df()$method))  # Remove NULLs
    selectInput("method", "Select Method:", choices = c("All", methods), selected = "SOLS", multiple = TRUE)
  })
  
  # Numeric Filter for Distribution_elas (>=0 or NaN)
  output$distribution_elas_ui <- renderUI({
    req(L0_df())  # Ensure dataset is available
    checkboxInput("distribution_elas", "Filter Distribution Elas (>=0 or NaN):", value = TRUE)
  })
  
  # Numeric Filter for Category_elas (>=0 or NaN)
  output$category_elas_ui <- renderUI({
    req(L0_df())  # Ensure dataset is available
    checkboxInput("category_elas", "Filter Category Elas (>=0 or NaN):", value = TRUE)
  })
  
  # Dynamic Channel Filter
  output$channel_ui <- renderUI({
    req(L0_df()) #Ensure Data is available
    channels <- unique(na.omit(L0_df()$Channel)) #Remove Nulls
    selectInput("channel", "Select Channel:", choices = c("All", channels), selected = "All")
  })
  
  # Dynamic Brand Filter (Based on Selected Channel)
  output$brand_ui <- renderUI({
    req(L0_df(), input$modelof) # Ensure filtered dataset is available
    selected_clm <- input$modelof
    choices <- unique(na.omit(L0_df()[[selected_clm]]))  # Remove NULLs
    selectInput("brand", paste("Select ",selected_clm,":",sep = ""), choices = c("All", choices), selected = "All")
  })
  
  # Dynamically update column selection inputs
  output$x_var_ui <- renderUI({
    req(L0_df())
    x_var_choise <- c("Index")
    selectInput("x_var", "X-axis variable:", choices = x_var_choise, selected = "Index")
  })
  
  output$y_var_ui <- renderUI({
    req(L0_df())
    y_var_choice <- c("CSF.CSF","MCV.MCV")
    selectInput("y_var", "Y-axis variable:", choices = y_var_choice, selected = y_var_choice[1])
  })
  
  ####### filtering based on New Inputs
  filter_df_1 <- reactive({
    req(L0_df())  # Ensure dataset is available
    df <- L0_df()
    
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
  
  ## after all filter, rename the filtred data 
  final_df <- reactive({
    df <- sorted_df()
    return(df)
  })
  
  ############### Model selection 
  output$select_ui <- renderUI({
    req(final_df())
    df <- final_df()
    
    # numericInput("select_m","Select Model Index:",value = 0, min = min(df$Index),max = max(df$Index))
    # selectInput("select_m","Select Model Index:",choices =(df$Index), multiple = TRUE)
    selectizeInput("select_m","Select Model Index:", choices =NULL, multiple=TRUE)
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
  
  observeEvent(input$save_bttn,{
    req(input$select_m, modelselection_df())
    
    # print(input$select_m)
    
    df <- modelselection_df()
    selected_idx <- input$select_m
    
    if(all(selected_idx>0)){
      selected_data <- df[(df$Index %in% selected_idx), ,drop=FALSE] #get selected row
      
      #Avoide duplicate entries
      current_data <- selected_models_df()
      if(all(!(selected_data$Index %in% current_data$Index))){
        updated_data <- rbind(current_data, selected_data) # Append row
        selected_models_df(updated_data) #update reactive value
      }
    }
    
  })
  
  # Reset selected models
  observeEvent(input$unsave_bttn,{
    req(input$select_m)
    
    updated_data <- selected_models_df()[!(selected_models_df()$Index %in% input$select_m),]
    selected_models_df(updated_data)
    
    #update after removing selected model
    
  })
  
  
  output$L0_file_filtered <- DT::renderDataTable({
    req(modelselection_df())
    df <- modelselection_df()
    dataframe1 <- DT::datatable(df, options = list(scrollX = TRUE))
    return(dataframe1)
  })
  
  #model selected rows
  output$L0_file_contents <- renderRHandsontable({
    req(selected_models_df())
    df <- selected_models_df() 
    # df <- df %>% 
    #   select(method,Channel,Brand,Variant,PackType,PPG,selectedmodels,RPIto,Adj.Rsq,AIC,MCV.MCV,CSF.CSF,actualdistvar,Index)
    dataframe1 <- rhandsontable(df) %>% hot_cols(readOnly = TRUE)
    return(dataframe1)
  })
  
  # Download File
  # file_name1 <- reactiveVal(input$modelfile$name)
  # sheet_name1 <- reactiveVal(input$sheet)
  # print(input$modelfile$name)
  # print(input$sheet)
  
  output$download_file1 <- downloadHandler(
    
    filename = function() { 
      req(input$modelfile)
      paste(tools::file_path_sans_ext(input$modelfile$name),"_selected models",".xlsx",sep = "") 
    },
    
    content = function(file) {
      req(selected_models_df())
      write.xlsx(selected_models_df(), file, sheetName = input$sheet, overwrite = TRUE, rowNames=FALSE)
    }
  )
  
  
  
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
  #   content = function(file) { 
  #     htmlwidgets::saveWidget(plot_reactive(), file) 
  #   }
  # )
  ###################################################################### Dashbroad End ############################################################################

  ########################################################### M1 to Result:-Sidebar start ###########################################################
  output$toggle_ui <- renderUI({
    actionButton("toggle", "Toggle Sidebar")
  })
  observeEvent(input$toggle,{
    runjs("
          var sidebar = document.getElementById('sidebar_1');
          var main = document.getElementById('main_1');
          if (sidebar.style.display === 'none') {
            sidebar.style.display = 'block';
            main.classList.remove('expanded-main');
          } else {
            sidebar.style.display = 'none';
            main.classList.add('expanded-main');
          }
      ")
  })
  
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
  #All columns data
  output$D0_file_contents <- DT::renderDataTable({
    DT::datatable(D0_df())
  })

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

