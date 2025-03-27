rm(list = ls())
library(shiny)
library(dplyr)
library(shinydashboard)
library(bslib)
library(shinyjs)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(reshape)
library(zoo)
library(anytime)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)

library(plotly)
library(shinyFiles)
library(tools)
library(RColorBrewer)
library(rhandsontable)
library(data.table)
library(openxlsx)


ui <- fluidPage(
 
  # tags$h2('Cogitaas'), #heading
  # p('Post Modeling'),# placeholder text
  # p('Cogitaas'),
  # shinythemes::themeSelector(),
  # theme = bslib::bs_theme(bootswatch = "darkly"),
  # theme = bslib::bs_theme(bootswatch = "flatly"),
  theme = shinytheme("flatly"),
  # theme = bslib::bs_theme(bootswatch = "cosmo"),
  # theme = bslib::bs_theme(bootswatch = "simplex"),
  # theme = bslib::bs_theme(bootswatch = "yeti"),
  # theme = bslib::bs_theme(bootswatch = "slate"),
  # selected_dir <- "C:/Users/Suraj/OneDrive - CogitaasAVA/Desktop/Post Modelling/CSF-Folder",
  fluidRow(
    column(4,
           tags$h2('Cogitaas')
    ),
    column(8,
           div(
    # style =  "top:20px; right:20px; display:flex; align-items:center; gap:10px",
    style = "display: flex; justify-content: flex-end; align-items: center; gap: 10px; margin-top: 20px;",
    shinyDirButton("select_dir", "Select Project Directory", "Select a directory"),
    # br(),
    verbatimTextOutput("selected_dir"),
  ),)),
  
  
  # div(
  #   style = "position:absolute; top:20px; right:20px; display:flex; align-items:center; gap:10px",
  #   shinyDirButton("select_dir", "Select Project Directory", "Select a directory"),
  #   br(),
  #   verbatimTextOutput("selected_dir"),
  # ),

  #First Nav bar page
  navbarPage(
    position = c("static-top"), #"fixed-top", "fixed-bottom","static-top"),
    title="CSF",
    fluid = TRUE,
    collapsible = TRUE,
    
    navbarMenu(
      title = "Post Modeling",
      tabPanel("Model Selection",
               dashboardPage(
                 dashboardHeader(disable = TRUE),
                 dashboardSidebar(disable = TRUE),
                 dashboardBody(
                   tabsetPanel(
                     tabPanel("Model Selection Dashboard",
                              
                              fluidRow(
                                column(12,
                                  box(title = "Move/Copy Files", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                      fluidRow(
                                        column(2,
                                               shinyDirButton("srcFolder", "Source Folder", "Select source folder"),
                                        ),
                                        column(3, 
                                               verbatimTextOutput("srcDir"),
                                        ),
                                        column(3,
                                               selectInput("dataFile", "Files:", choices = NULL, multiple = TRUE),
                                        ),
                                        column(2, offset = 2,
                                               actionButton("reset", "Reset")
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(2,
                                               shinyDirButton("destFolder", "Destination Folder", "Select destination folder"),
                                        ),
                                        column(3,
                                               verbatimTextOutput("destDir"),
                                        ),
                                        column(2,
                                               actionButton("copy", "Copy Files"),
                                        ),
                                        column(2,
                                               actionButton("move", "Move Files"),
                                        ),
                                        column(3,
                                               verbatimTextOutput("message")
                                        )
                                        
                                      ),
                                      
                                  )
                                )
                              ),
                              
                              fluidRow(
                                column(12,
                                  box(title = "Configuration Details:", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                    fluidRow(
                                      column(2,
                                             # Row for selecting work type (Dropdown)
                                             selectInput("worktype", "Select Work Type", choices = c("Onboard", "Continuity","Refresh")),
                                      ),
                                      column(3,
                                             # Row for integration process selection (Yes/No)
                                             radioButtons("integration_needed", "Integration Process Needed?", 
                                                          choices = c("Yes", "No"), selected = "No"),
                                      ),
                                      column(2,
                                             
                                             conditionalPanel(
                                               condition = "input.integration_needed == 'Yes'",
                                               fluidRow(
                                                 column(12,
                                                        selectInput("input_type", "Select Input Type", 
                                                                    choices = c("Brand_Variant", "Brand_PPG", "Brand_PackType", "Brand_Variant_PPG"),selected = "Brand_Variant")
                                                 )
                                               )
                                             ),
                                             
                                      ),
                                      # column(1,),
                                      column(2,
                                             # Row for frequency selection
                                             selectInput("datafrequency", "Data Frequency", 
                                                         choices = c("Weekly","Monthly"))
                                      ),
                                      
                                      column(2,
                                             # Row for csf_period selection
                                             selectInput("csf_period", "csf Period", 
                                                         choices = c(52, 12))
                                      ),
                                      
                                      
                                      
                                    ),
                                    # Granularity/Scope Selection
                                    tags$h4("Granularity/Scope Selection:"),
                                    fluidRow(
                                      column(3, selectInput("L0_indicator", "L0 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')),
                                      column(3, conditionalPanel(
                                        condition = "input.L0_indicator != 'NA'",
                                        selectInput("L1_indicator", "L1 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
                                      )),
                                      column(3, conditionalPanel(
                                        condition = "input.L1_indicator != 'NA'",
                                        selectInput("L2_indicator", "L2 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
                                      )),
                                      column(3, conditionalPanel(
                                        condition = "input.L2_indicator != 'NA'",
                                        selectInput("L3_indicator", "L3 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
                                      ))
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                column(3,
                                       box(title = "Controls", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                    
                                           # Multi-select dropdown for files; choices will be updated from the reactive directory
                                           selectInput("D0_file", "Select D0 File", choices = NULL),
                                           
                                           selectInput("L0_file", "Select L0 File", choices = NULL, multiple = TRUE),
                                           
                                           
                                           # Button to trigger the file loading process
                                           actionButton("upload_btn", "Upload Files"),
                                           
                                           
                                           uiOutput("modelof_ui"),
                                           
                                           uiOutput("distribution_elas_ui"),  # Dynamic Channel Filter
                                           uiOutput("category_elas_ui"),  # Dynamic Channel Filter
                                           uiOutput("price_pval_ui"),  # Dynamic Channel Filter
                                           
                                           
                                           
                                           radioButtons("plot_type", "Select Plot Type:",
                                                        choices = c("Scatter Plot", "Box Plot", "Bar Plot"), selected = "Bar Plot"),
                                           
                                           
                                           downloadButton("download_file1", "Download File")
                                       ),),
                                column(9,
                                       box(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                           fluidRow(
                                             column(3,
                                                    uiOutput("channel_ui"),  # Dynamic Channel Filter
                                             ),
                                             column(3,
                                                    uiOutput("brand_ui"),    # Dynamic Brand Filter
                                             ),
                                             column(2,
                                                    uiOutput("x_var_ui"),  # Dynamically generated X-axis dropdown
                                             ),
                                             column(2,
                                                    uiOutput("y_var_ui"),  # Dynamically generated Y-axis dropdown
                                             ),
                                             column(2,
                                                    uiOutput("method_ui"),  # Dynamic Channel Filter
                                             )
                                           ),
                                           fluidRow(
                                             column(3,
                                                    uiOutput("rpito_ui")  # Dynamic RPIto Filter
                                             ),
                                             column(3,
                                                    uiOutput("actualdist_ui")  # Dynamic actualdistvar
                                             ),
                                             column(3,
                                                    uiOutput("adjrsq_ui") # Dynamic AdjRsq
                                             ),
                                             column(3,
                                                    uiOutput("csf_ui") # Dynamic AdjRsq
                                             )
                                           )
                                           
                                           
                                       ),
                                       
                                       box(title = "Plot", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                           fluidRow(
                                             column(width = 12,
                                                    plotlyOutput("plot")
                                             )
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3,
                                                    uiOutput("sort_ui") # Dynamic Sort option
                                             ),
                                             column(width = 3,
                                                    uiOutput("select_ui")
                                                    
                                             ),
                                             column(3,
                                                    uiOutput("savebttn_ui")
                                             ),
                                             column(3,
                                                    uiOutput("unsavebttn_ui")
                                                    
                                             )
                                           )
                                           
                                       )
                                ),),
                              
                              fluidRow(
                                box(title = "Uploaded Data", width = 12, status = "info", solidHeader = TRUE,
                                    
                                    fluidRow(
                                      column(12,
                                             # DTOutput("L0_file_contents"))
                                             # DT::dataTableOutput("L0_file_contents",width = 'auto')
                                             rHandsontableOutput("L0_file_contents")
                                      ),
                                      column(12,
                                             rHandsontableOutput("allmodels")
                                      ),
                                      column(12,
                                             DT::dataTableOutput("L0_file_filtered",width = 'auto')
                                      )
                                    )
                                )
                              )
                     )
                   )
                 )
               )
        
      ),
      #Tap1
      tabPanel(
        title="M1 to Results",
        # h2("Content of Tab 1"),
        useShinyjs(),
        tags$head(
          tags$style(HTML("
                          .hidden-sidebar {display: none !important;}
                          .expanded-main{width:100% !important; flex: 0 0 100% !impotant; max-width: 100% !important;}
                          "))
        ),
        uiOutput("toggle_ui"),
        sidebarLayout(
          sidebarPanel(
            id = "sidebar_1",
            tags$h2("Configuration Details"),
            tags$hr(),
            # p("place controls here"),
            # File uploader
            fileInput('salesfile', "Upload D0 File", 
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.xlsx'
                      )
            ),
            
            fluidRow(
              column(4,
                     # Row for selecting work type (Dropdown)
                     selectInput("worktype", "Select Work Type", choices = c("Onboard", "Continuity","Refresh")),
              ),
              column(3,
                     # Row for integration process selection (Yes/No)
                     radioButtons("integration_needed", "Integration Process Needed?", 
                                  choices = c("Yes", "No"), selected = "No"),
                     
              ),
              column(5,
                     # Conditionally show the input_type dropdown if Integration process is "Yes"
                     conditionalPanel(
                       condition = "input.integration_needed == 'Yes'",
                       fluidRow(
                         column(12,
                                selectInput("input_type", "Select Input Type", 
                                            choices = c("Brand_Variant", "Brand_PPG", "Brand_PackType", "Brand_Variant_PPG"),selected = "Brand_Variant")
                         )
                       )
                     ),
              )
            ),
          
            # Granularity/Scope Selection
            tags$h4("Granularity/Scope Selection:"),
            fluidRow(
              column(3, selectInput("L0_indicator", "L0 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')),
              column(3, conditionalPanel(
                condition = "input.L0_indicator != 'NA'",
                selectInput("L1_indicator", "L1 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
              )),
              column(3, conditionalPanel(
                condition = "input.L1_indicator != 'NA'",
                selectInput("L2_indicator", "L2 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
              )),
              column(3, conditionalPanel(
                condition = "input.L2_indicator != 'NA'",
                selectInput("L3_indicator", "L3 Indicator", choices = c("NA","Channel","Brand","Variant","PackType","PPG","PackSize"), selected = 'NA')
              ))
            ),
            
            fluidRow(
              column(6,
                     # Row for frequency selection
                     selectInput("datafrequency", "Data Frequency", 
                                 choices = c("Weekly","Monthly"))
              ),
              column(6,
                     # Row for csf_period selection
                     selectInput("csf_period", "csf Period", 
                                 choices = c(52, 12))
              )
            ),
            fluidRow(
              column(4,
                      # sidebarPanel(
                        actionButton("run_process", "Run Process", icon = icon("play")),
                      # )
                     
              ),
              column(8,
                     verbatimTextOutput("process_status")
              )
            )
            
          ),
          mainPanel(
            id = "main_1",
            h2('Main content of Tab 1'),
            p("Display main content"),
            DT::dataTableOutput("D0_file_contents")
          )
        )
      ),
    ),

    tabPanel(
      title="Tab 2",
      h2("Content of Tab 2"),
      
    ),
    tabPanel(
      title="Tab 3",
      h2("Content of Tab 3")
    ),
    
    navbarMenu(
      title = "temp"
      
    ),
    
  )
)

