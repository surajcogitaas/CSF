Output_Main2 <- function(project_root, Integration_process_needed, input, L0_indicator, L2_indicator, L3_indicator, L1_indicator, datafrequency, CMA_input, NSV_input, D0_file, restofcategory_included, worktype, csf_period){
  
  
  if(Integration_process_needed == "Yes"){
    
    integrator2(project_root, input,worktype)
    
    ##############################################
    
    'process all M0 files through column selector function'
    
    filelist <- list.files(file.path(project_root, worktype, "4. Modelling/writedata/selected models"))
    filelist <- filelist[grep("Wtd_avg_MCV", filelist)]
    
    if(worktype == "Refresh"){
      
      filelist <- filelist[grep("dilute", filelist)]
    }
    
    for(hh in filelist){
      FinalM0 <- hh
      column_selector(project_root, FinalM0, worktype)
    }
    
    ####### L2 MCV process If L2 is already present in Base M0#####################
    
    L0_file <- paste("integrate_", L0_indicator, ".csv", sep = "")
    
    
    if(L2_indicator != "NA"){
      
      L2_adjustment2(project_root, D0_file, worktype)
      
    }
    
    
    ########## L3 adjustment ###################################
    
    if(L3_indicator != "NA"){
      
      L3_adjustment2(project_root, D0_file, L3_indicator, worktype, csf_period)
      
    }
    ###### volume decomposition ##### based on L0 only #### 
    
    volume_decomposition(project_root, L0_file, Integration_process_needed,worktype)    
    
    #########  RPI curves #################################
    
    if(L2_indicator != "NA" & L3_indicator != "NA"){
      
      curves_level <- c("L0.csv", "L0L2.csv", "L0L2L3.csv")
      
      for(ss in curves_level){
        
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included,worktype)
        
      }
    }else if(L2_indicator != "NA" & L3_indicator == "NA"){
      
      curves_level <- c("L0.csv", "L0L2.csv")
      
      for(ss in curves_level){
        
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included,worktype)
        
      }
    }else if(L2_indicator == "NA" & L3_indicator == "NA"){
      curves_level <- c("L0.csv")
      
      for(ss in curves_level){
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included, worktype)
        
      }  
    }
  
  }else{
    
    ##############################################
    
    'process all M0 files through column selector function'
    
    filelist <- list.files(file.path(project_root, worktype, "4. Modelling/writedata/selected models"))
    filelist <- filelist[grep("Wtd_avg_MCV", filelist)]
    
    if(worktype == "Refresh"){
      
      filelist <- filelist[grep("dilute", filelist)]
    }
    
    #hh <- "Wtd_avg_MCV_Brand.xlsx"
    for(hh in filelist){
      FinalM0 <- hh
      column_selector(project_root, FinalM0, worktype)
    }
    
    
    L0 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", "column_selector_Brand.csv"),row.names = NULL)
    
    D0 <- read.csv(file.path(Base_Path, worktype, "2. D0", D0_file),row.names = NULL)
    D0$Market <- tolower(D0$Market)
    D0$Channel <- tolower(D0$Channel)
    D0$Region <- tolower(D0$Region)
    D0$Category <- tolower(D0$Category)
    D0$SubCategory <- tolower(D0$SubCategory)
    D0$Brand <- tolower(D0$Brand)
    D0$Variant <- tolower(D0$Variant)
    D0$PackType <- tolower(D0$PackType)
    D0$PPG <- tolower(D0$PPG)
    D0$PackSize <- tolower(D0$PackSize)
    
    
    lastyrdata <- D0 %>% 
      dplyr::select(everything()) %>% filter(Brand != "cat1") %>%
      group_by(Year, Week) %>% summarise(Volume = sum(Volume)) %>%
      arrange(Year, Week) %>% ungroup()
    
    if(datafrequency == "Weekly"){
      
      lastyrdata <- lastyrdata[c((nrow(lastyrdata)-(csf_period-1)):nrow(lastyrdata)),]
    }else{
      lastyrdata <- lastyrdata[c((nrow(lastyrdata)-(csf_period-1)):nrow(lastyrdata)),]
      
    }
    
    lastyrdata$yw <- paste(lastyrdata$Year, lastyrdata$Week, sep = "_")
    D0$yw <- paste(D0$Year, D0$Week, sep = "_")
    D0$ly <- lastyrdata$yw[match(D0$yw, lastyrdata$yw)]
    unique(D0$ly)
    
    D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
    colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
    d=2
    for(d in c(1:5)){
      if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
        D0_summarytotal[d] <- "all"
      }
    }
    
    
    D0 <- D0 %>% filter(!is.na(D0$ly)) 
    D0_summary <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
    colnames(D0_summary) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue", "Volume")    
    
    d=5
    for(d in c(1:5)){
      if(unique(D0_summary[,d])[1] == tolower(paste("all", colnames(D0_summary)[d], sep = ""))){
        D0_summary[d] <- "all"
      }
    }
    
    L0 <- left_join(L0, D0_summary, by = c(colnames(D0_summary)[1:6]))    
    
    L0$Revenue.x <- L0$Revenue.y
    L0$Volume.x <- L0$Volume.y
    
    L0 <- L0[,-c((ncol(L0)-1), ncol(L0))]
    colnames(L0)[c(12,13)] <- c("Revenue", "Volume")
    
    L0 <- left_join(L0, D0_summarytotal, by = colnames(D0_summarytotal)[1:6])
    L0$Price <- L0$Revenue / L0$Volume
    L0$CSF <- L0$MCV.MCV / L0$Price
    write.csv(L0, file.path(Base_Path, worktype, "9. Validator Output", "L0.csv"), row.names = FALSE)
    
    
    if(L2_indicator != "NA"){
      
      L2_adjustment1(project_root, D0_file, L0_indicator, L2_indicator, worktype, csf_period)
      
    }
    #
    ######### L3 adjustment ###################################
    
    if(L3_indicator != "NA"){
      
      L3_adjustment1(project_root, D0_file, L3_indicator, worktype, csf_period)
      
    }
    ###### volume decomposition ##### based on L0 only #### 
    
    L0_file <- paste("column_selector_", L0_indicator, ".csv", sep = "")
    volume_decomposition(project_root, L0_file, Integration_process_needed, worktype)    
    
    #########  RPI curves #################################
    
    if(L2_indicator != "NA" & L3_indicator != "NA"){
      
      curves_level <- c("L0.csv", "L0L2.csv", "L0L2L3.csv")
      
      for(ss in curves_level){
        
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included, worktype)
        
      }
    }else if(L2_indicator != "NA" & L3_indicator == "NA"){
      
      curves_level <- c("L0.csv", "L0L2.csv")
      
      for(ss in curves_level){
        
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included, worktype)
        
      }
    }else if(L2_indicator == "NA" & L3_indicator == "NA"){
      curves_level <- c("L0.csv")
      
      for(ss in curves_level){
        file_for_curves <- ss
        RPI_curves(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included, worktype)
        
      }  
    }
    
  }
  
}
