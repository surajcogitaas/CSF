
'Are you want to Run Integrator code 
 If Yes then
 take input file as Brand_Variant or Brand_PPG or Brand_PackType or Brand_Variant_PPG
'
integrator1 <- function(brand_model_df, Integration_process_needed, input, worktype){

  df_m0 <- brand_model_df #read.xlsx(file.path(Base_Path, worktype, "4. Modelling/writedata/selected models", "Wtd_avg_MCV_Brand.xlsx" ), sheet = 3)
  
if(Integration_process_needed == "Yes"){

input <- input   ### or input can be Brand_Variant or Brand_PPG or Brand_PackType or Brand_Variant_PPG
sp <- strsplit(input, "_")

if(length(sp[[1]])==2){
  
  integratormapping <- as.data.frame(unique(df_m0$Brand))
  integratormapping$V1 <- NA
  integratormapping$V2 <- NA
  colnames(integratormapping)[c(1:ncol(integratormapping))] <- c("ModelBrand", sp[[1]][1], sp[[1]][2])
  
  
}else{
  
  integratormapping <- as.data.frame(unique(df_m0$Brand))
  integratormapping$V1 <- NA
  integratormapping$V2 <- NA
  integratormapping$V3 <- NA
  colnames(integratormapping)[c(1:ncol(integratormapping))] <- c("ModelBrand", sp[[1]][1], sp[[1]][2], sp[[1]][3])
  
}

return(integratormapping) #write.csv(integratormapping, file.path(Base_Path, worktype, "2. D0", "integrator_mapping.csv"), row.names = F)

}}


#############################################################################################################
#############################################################################################################


integrator2 <- function(project_root, input, worktype){

'fill integrator mapping by user input '

integratormapping <- read.csv(file.path(Base_Path, worktype, "2. D0", "integrator_mapping.csv"),row.names = NULL)
integratormapping_original <- integratormapping
orig_names <- colnames(integratormapping)
colnames(integratormapping)[c(2:ncol(integratormapping))] <- paste("integrate", c(2:ncol(integratormapping)), sep = "")
integratormapping$ModelBrand2 <- gsub(" ", ".", integratormapping$ModelBrand)

input <- input   ### or input can be Brand_Variant or Brand_PPG or Brand_PackType or Brand_Variant_PPG
sp <- strsplit(input, "_")

#####  Reading file to be integrated ##################

df_m0 <- read.xlsx(file.path(Base_Path, worktype, "4. Modelling/writedata/selected models", "Wtd_avg_MCV_Brand.xlsx" ), sheet = 3)

#### cross elas calculated as avg of candidate models #####

df_m01 <- df_m0 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG)
df_m02 <- df_m0[,c((match("Category_elas", colnames(df_m0))+1):(match("beta0", colnames(df_m0))-1))]
df_m02[is.na(df_m02)] <- 0
df_m02[df_m02 < 0] <- 0
df_m02[df_m02 == 0] <- NA

df_m03 <- cbind(df_m01, df_m02)
avg_cross_elas <- df_m03 %>% 
  dplyr::select(everything()) %>% 
  group_by(Market, Channel, Region, Category, SubCategory, Brand, Variant, PackType, PPG) %>% summarise_all(mean, na.rm = T) %>%
  arrange(Market, Channel, Region, Category, SubCategory, Brand, Variant, PackType, PPG)  %>% ungroup()

colnames(df_m0)

df_m04 <- df_m0 %>% filter(selectedmodels == "Yes")
df_m05 <- df_m04 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG, MCV.MCV, CSF.CSF, Price_elas, Revenue, Volume)


df_m06 <- left_join(df_m05, avg_cross_elas, by=c("Market", "Channel", "Region", "Category", "SubCategory", "Brand", "Variant", "PackType", "PPG"))

########## In M1 models, cross elas replaced with avg cross elas #

df_m07 <- left_join(df_m06, integratormapping, by = c("Brand" = "ModelBrand"))

#### 
reqcols <- c("Market", "Channel", "Region", "Category", "SubCategory", "Brand", "Variant", "PackType", "PPG")


if(length(sp[[1]])==2){

integrate1 <- df_m07 %>% 
  dplyr::select(everything()) %>% 
  group_by(Market, Channel, Region, Category, SubCategory, integrate2) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume), Price_elas = weighted.mean(Price_elas, Volume),
                                     Revenue=sum(Revenue), Volume = sum(Volume)) %>% 
  arrange(Market, Channel, Region, Category, SubCategory, integrate2) %>% ungroup()

crosselas1 <- df_m07 %>% 
  group_by(Market, Channel, Region, Category, SubCategory, integrate2) %>% 
  summarise_at(vars(colnames(df_m07)[(match("Volume", colnames(df_m07))+1):(ncol(df_m07)-3)]), funs(weighted.mean(., Volume, na.rm = T)))


crosselas11 <- crosselas1 %>% pivot_longer(cols = !c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2"), names_to = "Competition", values_to = "Elas")
df_m055 <- df_m05[,c(1:5, 6, 14)]
df_m055[,ncol(df_m055)-1] <- gsub(" ", ".", df_m055[,ncol(df_m055)-1])

crosselas11 <- left_join(crosselas11, df_m055, by = c("Market", "Channel", "Region", "Category", "SubCategory", "Competition" =  "Brand"))
crosselas11 <- left_join(crosselas11, integratormapping, by = c("Competition" =  "ModelBrand2"))

cross_elas_integrate1 <- crosselas11 %>%
  dplyr::select(everything()) %>%
  group_by(Market, Channel, Region, Category, SubCategory, integrate2.x, integrate2.y) %>% summarise(cross_elas = weighted.mean(Elas, Volume, na.rm = T)) %>%
  arrange(Market, Channel, Region, Category, SubCategory, integrate2.x, integrate2.y) %>% ungroup() %>% spread(key = integrate2.y, value = cross_elas , fill = 0)

integrate1$Price <- integrate1$Revenue / integrate1$Volume
integrate1$CSF <- integrate1$MCV / integrate1$Price

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

D0 <- left_join(D0, integratormapping_original, by = c("Brand" = "ModelBrand"))
colnames(D0)[c((ncol(D0)-1):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-1):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-1):ncol(D0))])-2))    
colnames(D0)[6] <- "Brand.x"

D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    

for(d in c(1:5)){
  if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
    D0_summarytotal[d] <- "all"
  }
}

colnames(integrate1)
integrate1 <- left_join(integrate1, D0_summarytotal, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2" = colnames(D0_summarytotal)[6]))


integrate1 <- left_join(integrate1, cross_elas_integrate1, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2" = "integrate2.x"))
colnames(integrate1)[6] <- orig_names[2]

addcols <- data.frame(matrix(nrow=nrow(integrate1), ncol = 3))
colnames(addcols) <- reqcols[is.na(match(reqcols,colnames(integrate1)))]
addcols[is.na(addcols)] <- "all"
integrate1 <- cbind(integrate1, addcols)

#######

integrate2 <- df_m07 %>% 
  dplyr::select(everything()) %>% 
  group_by(Market, Channel, Region, Category, SubCategory, integrate3) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume), Price_elas = weighted.mean(Price_elas, Volume),
                                                                                     Revenue=sum(Revenue), Volume = sum(Volume)) %>% 
  arrange(Market, Channel, Region, Category, SubCategory, integrate3) %>% ungroup()


crosselas2 <- df_m07 %>% 
  group_by(Market, Channel, Region, Category, SubCategory, integrate3) %>% 
  summarise_at(vars(colnames(df_m07)[(match("Volume", colnames(df_m07))+1):(ncol(df_m07)-3)]), funs(weighted.mean(., Volume, na.rm = T)))


crosselas22 <- crosselas2 %>% pivot_longer(cols = !c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3"), names_to = "Competition", values_to = "Elas")
df_m055 <- df_m05[,c(1:5, 6, 14)]
df_m055[,ncol(df_m055)-1] <- gsub(" ", ".", df_m055[,ncol(df_m055)-1])

crosselas22 <- left_join(crosselas22, df_m055, by = c("Market", "Channel", "Region", "Category", "SubCategory", "Competition" =  "Brand"))
crosselas22 <- left_join(crosselas22, integratormapping, by = c("Competition" =  "ModelBrand2"))


cross_elas_integrate2 <- crosselas22 %>%
  dplyr::select(everything()) %>%
  group_by(Market, Channel, Region, Category, SubCategory, integrate3.x, integrate3.y) %>% summarise(cross_elas = weighted.mean(Elas, Volume, na.rm = T)) %>%
  arrange(Market, Channel, Region, Category, SubCategory, integrate3.x, integrate3.y) %>% ungroup() %>% spread(key = integrate3.y, value = cross_elas , fill = 0)

integrate2$Price <- integrate2$Revenue / integrate2$Volume
integrate2$CSF <- integrate2$MCV / integrate2$Price

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

D0 <- left_join(D0, integratormapping_original, by = c("Brand" = "ModelBrand"))
colnames(D0)[c((ncol(D0)-1):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-1):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-1):ncol(D0))])-2))    
colnames(D0)[6] <- "Brand.x"

D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L2_indicator, colnames(D0))]]), sum)  
colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L2_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    

for(d in c(1:5)){
  if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
    D0_summarytotal[d] <- "all"
  }
}

integrate2 <- left_join(integrate2, D0_summarytotal, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3" = colnames(D0_summarytotal)[6]))


integrate2 <- left_join(integrate2, cross_elas_integrate2, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3" = "integrate3.x"))
colnames(integrate2)[6] <- orig_names[3]

addcols <- data.frame(matrix(nrow=nrow(integrate2), ncol = 3))
colnames(addcols) <- reqcols[is.na(match(reqcols,colnames(integrate2)))]
addcols[is.na(addcols)] <- "all"
integrate2 <- cbind(integrate2, addcols)

write.csv(integrate1, file.path(Base_Path, worktype, "9. Validator Output", paste("integrate_",orig_names[2],".csv", sep = "")), row.names = F)
write.csv(integrate1, file.path(Base_Path, worktype, "9. Validator Output", "L0.csv"), row.names = F)

write.csv(integrate2, file.path(Base_Path, worktype, "9. Validator Output", paste("integrate_",orig_names[3],".csv", sep = "")), row.names = F)


}else{
  
  integratormapping <- read.csv(file.path(Base_Path, worktype, "2. D0", "integrator_mapping.csv"),row.names = NULL)
  integratormapping_original <- integratormapping
  orig_names <- colnames(integratormapping)
  colnames(integratormapping)[c(2:ncol(integratormapping))] <- paste("integrate", c(2:ncol(integratormapping)), sep = "")
  integratormapping$ModelBrand2 <- gsub(" ", ".", integratormapping$ModelBrand)
  
    integrate1 <- df_m07 %>% 
    dplyr::select(everything()) %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate2) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume), Price_elas = weighted.mean(Price_elas, Volume),
                                                                                       Revenue=sum(Revenue), Volume = sum(Volume)) %>% 
    arrange(Market, Channel, Region, Category, SubCategory, integrate2) %>% ungroup()
  
  crosselas1 <- df_m07 %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate2) %>% 
    summarise_at(vars(colnames(df_m07)[(match("Volume", colnames(df_m07))+1):(ncol(df_m07)-4)]), funs(weighted.mean(., Volume, na.rm = T)))
  
    
  crosselas11 <- crosselas1 %>% pivot_longer(cols = !c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2"), names_to = "Competition", values_to = "Elas")
  df_m055 <- df_m05[,c(1:5, 6, 14)]
  df_m055[,ncol(df_m055)-1] <- gsub(" ", ".", df_m055[,ncol(df_m055)-1])
  
  crosselas11 <- left_join(crosselas11, df_m055, by = c("Market", "Channel", "Region", "Category", "SubCategory", "Competition" =  "Brand"))
  crosselas11 <- left_join(crosselas11, integratormapping, by = c("Competition" =  "ModelBrand2"))
  
  cross_elas_integrate1 <- crosselas11 %>%
    dplyr::select(everything()) %>%
    group_by(Market, Channel, Region, Category, SubCategory, integrate2.x, integrate2.y) %>% summarise(cross_elas = weighted.mean(Elas, Volume, na.rm = T)) %>%
    arrange(Market, Channel, Region, Category, SubCategory, integrate2.x, integrate2.y) %>% ungroup() %>% spread(key = integrate2.y, value = cross_elas , fill = 0)
  
  integrate1$Price <- integrate1$Revenue / integrate1$Volume
  integrate1$CSF <- integrate1$MCV / integrate1$Price
  

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
  
  D0 <- left_join(D0, integratormapping_original, by = c("Brand" = "ModelBrand"))
  colnames(D0)[c((ncol(D0)-2):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-2):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-2):ncol(D0))])-2))    
  colnames(D0)[6] <- "Brand.x"
  
  D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
  colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
  
  for(d in c(1:5)){
    if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
      D0_summarytotal[d] <- "all"
    }
  }
  
  colnames(integrate1)
  integrate1 <- left_join(integrate1, D0_summarytotal, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2" = colnames(D0_summarytotal)[6]))
  
  
  integrate1 <- left_join(integrate1, cross_elas_integrate1, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate2" = "integrate2.x"))
  colnames(integrate1)[6] <- orig_names[2]
  
  
  integrate2 <- df_m07 %>% 
    dplyr::select(everything()) %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate3) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume), Price_elas = weighted.mean(Price_elas, Volume),
                                                                                       Revenue=sum(Revenue), Volume = sum(Volume)) %>% 
    arrange(Market, Channel, Region, Category, SubCategory, integrate3) %>% ungroup()
  
  
  crosselas2 <- df_m07 %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate3) %>% 
    summarise_at(vars(colnames(df_m07)[(match("Volume", colnames(df_m07))+1):(ncol(df_m07)-4)]), funs(weighted.mean(., Volume, na.rm = T)))
  
  
  crosselas22 <- crosselas2 %>% pivot_longer(cols = !c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3"), names_to = "Competition", values_to = "Elas")
  df_m055 <- df_m05[,c(1:5, 6, 14)]
  df_m055[,ncol(df_m055)-1] <- gsub(" ", ".", df_m055[,ncol(df_m055)-1])
  
  crosselas22 <- left_join(crosselas22, df_m055, by = c("Market", "Channel", "Region", "Category", "SubCategory", "Competition" =  "Brand"))
  crosselas22 <- left_join(crosselas22, integratormapping, by = c("Competition" =  "ModelBrand2"))
  
  cross_elas_integrate2 <- crosselas22 %>%
    dplyr::select(everything()) %>%
    group_by(Market, Channel, Region, Category, SubCategory, integrate3.x, integrate3.y) %>% summarise(cross_elas = weighted.mean(Elas, Volume, na.rm = T)) %>%
    arrange(Market, Channel, Region, Category, SubCategory, integrate3.x, integrate3.y) %>% ungroup() %>% spread(key = integrate3.y, value = cross_elas , fill = 0)
  
  integrate2$Price <- integrate2$Revenue / integrate2$Volume
  integrate2$CSF <- integrate2$MCV / integrate2$Price
  
  
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
  
  D0 <- left_join(D0, integratormapping_original, by = c("Brand" = "ModelBrand"))
  colnames(D0)[c((ncol(D0)-2):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-2):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-2):ncol(D0))])-2))    
  colnames(D0)[6] <- "Brand.x"
  
  D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
  colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
  
  for(d in c(1:5)){
    if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
      D0_summarytotal[d] <- "all"
    }
  }
  
  colnames(integrate1)
  integrate2 <- left_join(integrate2, D0_summarytotal, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3" = colnames(D0_summarytotal)[6]))
  
  
  integrate2 <- left_join(integrate2, cross_elas_integrate2, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate3" = "integrate3.x"))
  colnames(integrate2)[6] <- orig_names[3]
  
  integrate3 <- df_m07 %>% 
    dplyr::select(everything()) %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate4) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume), Price_elas = weighted.mean(Price_elas, Volume),
                                                                                       Revenue=sum(Revenue), Volume = sum(Volume)) %>% 
    arrange(Market, Channel, Region, Category, SubCategory, integrate4) %>% ungroup()
  
  crosselas3 <- df_m07 %>% 
    group_by(Market, Channel, Region, Category, SubCategory, integrate4) %>% 
    summarise_at(vars(colnames(df_m07)[(match("Volume", colnames(df_m07))+1):(ncol(df_m07)-4)]), funs(weighted.mean(., Volume, na.rm = T)))
  
  crosselas33 <- crosselas3 %>% pivot_longer(cols = !c("Market", "Channel", "Region", "Category", "SubCategory", "integrate4"), names_to = "Competition", values_to = "Elas")
  df_m055 <- df_m05[,c(1:5, 6, 14)]
  df_m055[,ncol(df_m055)-1] <- gsub(" ", ".", df_m055[,ncol(df_m055)-1])
  
  crosselas33 <- left_join(crosselas33, df_m055, by = c("Market", "Channel", "Region", "Category", "SubCategory", "Competition" =  "Brand"))
  crosselas33 <- left_join(crosselas33, integratormapping, by = c("Competition" =  "ModelBrand2"))
  
  cross_elas_integrate3 <- crosselas33 %>%
    dplyr::select(everything()) %>%
    group_by(Market, Channel, Region, Category, SubCategory, integrate4.x, integrate4.y) %>% summarise(cross_elas = weighted.mean(Elas, Volume, na.rm = T)) %>%
    arrange(Market, Channel, Region, Category, SubCategory, integrate4.x, integrate4.y) %>% ungroup() %>% spread(key = integrate4.y, value = cross_elas , fill = 0)
  
  integrate3$Price <- integrate3$Revenue / integrate3$Volume
  integrate3$CSF <- integrate3$MCV / integrate3$Price
  
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
  
  D0 <- left_join(D0, integratormapping_original, by = c("Brand" = "ModelBrand"))
  colnames(D0)[c((ncol(D0)-2):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-2):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-2):ncol(D0))])-2))    
  colnames(D0)[6] <- "Brand.x"
  
  D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]]), sum)  
  colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
  
  for(d in c(1:5)){
    if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
      D0_summarytotal[d] <- "all"
    }
  }
  
  colnames(integrate1)
  integrate3 <- left_join(integrate3, D0_summarytotal, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate4" = colnames(D0_summarytotal)[6]))
  
  
  integrate3 <- left_join(integrate3, cross_elas_integrate3, by = c("Market", "Channel", "Region", "Category", "SubCategory", "integrate4" = "integrate4.x"))
  colnames(integrate3)[6] <- orig_names[4]
  
  write.csv(integrate1, file.path(Base_Path, worktype, "9. Validator Output", paste("integrate_",orig_names[2],".csv", sep = "")), row.names = F)
  write.csv(integrate1, file.path(Base_Path, worktype, "9. Validator Output", paste("L0.csv", sep = "")), row.names = F)
  
  write.csv(integrate2, file.path(Base_Path, worktype, "9. Validator Output", paste("integrate_",orig_names[3],".csv", sep = "")), row.names = F)
  write.csv(integrate3, file.path(Base_Path, worktype, "9. Validator Output", paste("integrate_",orig_names[4],".csv", sep = "")), row.names = F)
  
  }
}

#################################################################################################
#################################################################################################


column_selector <- function(project_root, FinalM0, worktype){
sp <- strsplit(FinalM0, "_")
sp2 <- strsplit(sp[[1]][4], "\\.")

df_m0 <- read.xlsx(file.path(Base_Path, worktype, "4. Modelling/writedata/selected models", FinalM0), sheet = 3)

#### cross elas calculated as avg of candidate models #####

df_m01 <- df_m0 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG)
df_m02 <- df_m0[,c((match("Category_elas", colnames(df_m0))+1):(match("beta0", colnames(df_m0))-1))]
df_m02[is.na(df_m02)] <- 0
df_m02[df_m02 < 0] <- 0
df_m02[df_m02 == 0] <- NA

df_m03 <- cbind(df_m01, df_m02)
avg_cross_elas <- df_m03 %>% 
  dplyr::select(everything()) %>% 
  group_by(Market, Channel, Region, Category, SubCategory, Brand, Variant, PackType, PPG) %>% summarise_all(mean, na.rm = T) %>%
  arrange(Market, Channel, Region, Category, SubCategory, Brand, Variant, PackType, PPG)  %>% ungroup()

colnames(df_m0)

df_m04 <- df_m0 %>% filter(selectedmodels == "Yes")
df_m05 <- df_m04 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG, MCV.MCV, Price_elas, Revenue, Volume)

df_m05$Price <- df_m05$Revenue / df_m05$Volume
df_m05$CSF <- df_m05$MCV.MCV / df_m05$Price

df_m06 <- left_join(df_m05, avg_cross_elas, by=c("Market", "Channel", "Region", "Category", "SubCategory", "Brand", "Variant", "PackType", "PPG"))
colnames(df_m06)

if(length(which(is.na(df_m06$Revenue)))>0){
  df_m06 <- df_m06[-which(is.na(df_m06$Revenue)),]
}

write.csv(df_m06, file.path(Base_Path, worktype, "9. Validator Output", paste("column_selector_",sp2[[1]][1],".csv", sep = "")), row.names = F)

}

#############################################################################################
###############################################################################################
  
L2_adjustment2 <- function(project_root, D0_file, worktype){  
        
        ############## L2 MCV adjustment 2 ##################################
        
        integratormapping <- read.csv(file.path(Base_Path, worktype, "2. D0", "integrator_mapping.csv"),row.names = NULL)
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
        
        D0 <- left_join(D0, integratormapping, by = c("Brand" = "ModelBrand"))
        colnames(D0)[c((ncol(D0)-1):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-1):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-1):ncol(D0))])-2))    
        colnames(D0)[6] <- "Brand.x"
        
        D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]]), sum)  
        colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
        
        for(d in c(1:5)){
          if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
            D0_summarytotal[d] <- "all"
          }
        }
        
        
        L2 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", "column_selector_Brand.csv"),row.names = NULL)
        colnames(L2)
        L2 <- L2[,c(1:6,match("MCV.MCV", colnames(L2)), match("Revenue", colnames(L2)), match("Volume", colnames(L2)))]
        
        L2 <- left_join(L2, integratormapping, by = c("Brand" =  "ModelBrand"))
        L2 <- L2[,-match("Brand", colnames(L2))]
        colnames(L2)[c(6,9)] <- c("L2_MCV","Brand")
        L2 <- L2[,c(1:5,9,10,6,7,8)]
        
        L2$temp <- L2$Volume * L2$L2_MCV
        
        L2_temp <- aggregate(L2[c(9,10,11)], list(grp = L2[[1]], L2[[2]], L2[[3]], L2[[4]], L2[[5]], L2[[6]], L2[[7]]), sum)  
        colnames(L2_temp) <- c(colnames(L2)[c(1:7)], "Revenue", "Volume", "temp")    
        
        L2_temp$L2_MCV <- L2_temp$temp / L2_temp$Volume
        
        colnames(L2_temp)
        L2 <- L2_temp[,c(1:7,11,8,9)]
        
        L2$Price <- L2$Revenue / L2$Volume
        L2$CSF <- L2$L2_MCV / L2$Price
        L2$CSF[which(L2$CSF<1)] <- 1.05
        L2$L2_MCV <- L2$CSF * L2$Price
        
        L2 <- left_join(L2, D0_summarytotal, by = colnames(D0_summarytotal)[1:7])
        write.csv(L2, file.path(Base_Path, worktype, "9. Validator Output", "L0L2.csv"), row.names = F)
}
  


####################################################################################################
        ####################### L3 MCV adjustment 2 ##################################

L3_adjustment2 <- function(project_root, D0_file, L3_indicator, worktype, csf_period){  
  
        integratormapping <- read.csv(file.path(Base_Path, worktype, "2. D0", "integrator_mapping.csv"),row.names = NULL)
        L0L2 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", "L0L2.csv"),row.names = NULL)
        if(colnames(L0L2)[1]=="X"){L0L2 <- L0L2[,-1]}
        
        L3 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", "column_selector_", L3_indicator, ".csv" ),row.names = NULL)
        
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
        
        L3 <- L3[,c(1:5,match(L3_indicator,colnames(L3)),match("MCV.MCV",colnames(L3)))]
        
        L0L2L3 <- left_join(L0L2, L3, by = c("Market", "Channel", "Region", "Category", "SubCategory"))
        
        lastyrdata <- D0 %>% 
          dplyr::select(everything()) %>% filter(Brand != "cat1") %>%
          group_by(Year, Week) %>% summarise(Revenue = sum(SalesValue), Volume = sum(Volume)) %>%
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
        
        D0 <- left_join(D0, integratormapping, by = c("Brand" = "ModelBrand"))
        colnames(D0)[c((ncol(D0)-1):ncol(D0))] <- substr(colnames(D0)[c((ncol(D0)-1):ncol(D0))], 1, (nchar(colnames(D0)[c((ncol(D0)-1):ncol(D0))])-2))    
        colnames(D0)[6] <- "Brand.x"
        
        D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]], D0[[match(L3_indicator, colnames(D0))]]), sum)  
        colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)), match(L3_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
        
        for(d in c(1:5)){
          if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
            D0_summarytotal[d] <- "all"
          }
        }
        
        D0 <- D0 %>% filter(!is.na(D0$ly)) 
        D0_summary <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]], D0[[match(L3_indicator, colnames(D0))]]), sum)  
        colnames(D0_summary) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)), match(L3_indicator, colnames(D0)))], "Revenue", "Volume")    
        
        d=5
        for(d in c(1:5)){
          if(unique(D0_summary[,d])[1] == tolower(paste("all", colnames(D0_summary)[d], sep = ""))){
            D0_summary[d] <- "all"
          }
        }
        
        
        L0L2L3 <- left_join(L0L2L3, D0_summary, by = c(colnames(D0_summary)[1:8]))    
        L0L2L3$Volume.y[is.na(L0L2L3$Volume.y)] <- 0  
        
        L0L2L3 <- left_join(L0L2L3, D0_summarytotal, by = c(colnames(D0_summarytotal)[1:8]))    
        L0L2L3$Volume_Total.y[is.na(L0L2L3$Volume_Total.y)] <- 0  
        
        
        colnames(L0L2L3)
        wt_MCV <- L0L2L3 %>%
          dplyr::select(everything()) %>%
          group_by_at(1:7) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume.y)) %>%
          ungroup()
        
        L0L2L3 <- left_join(L0L2L3, wt_MCV, by = c(colnames(wt_MCV)[1:7]))
        L0L2L3$index <- L0L2L3$MCV / L0L2L3$L2_MCV
        L0L2L3$L3_MCV <- L0L2L3$MCV.MCV / L0L2L3$index     
        
        L0L2L3 <- L0L2L3 %>% filter(Volume.y > 0)
        L0L2L3 <- L0L2L3[,c(1:7,match(L3_indicator, colnames(L0L2L3)), ncol(L0L2L3), match("Revenue.y", colnames(L0L2L3)), match("Volume.y", colnames(L0L2L3)), match("Revenue_Total.y", colnames(L0L2L3)), match("Volume_Total.y", colnames(L0L2L3)))]
        
        colnames(L0L2L3)[c(10,11,12,13)] <- c("Revenue", "Volume", "Revenue_Total", "Volume_Total")
        
        L0L2L3$Price <- L0L2L3$Revenue / L0L2L3$Volume
        L0L2L3$CSF <- L0L2L3$L3_MCV / L0L2L3$Price
        L0L2L3$CSF[which(L0L2L3$CSF<1)] <- 1.05
        L0L2L3$L3_MCV <- L0L2L3$CSF * L0L2L3$Price
        
        L0L2L3 <- L0L2L3[,c(1:11,14,15,12,13)]
        
        write.csv(L0L2L3, file.path(Base_Path, worktype, "9. Validator Output", "L0L2L3.csv"), row.names = F)
        
}


####################################################################################################
############## Volume decomposition ##################################

volume_decomposition <- function(project_root, L0_file, Integration_process_needed, worktype){
  
  CategoryM0_file <- "FinalM0_Category.csv"
  L0 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", L0_file ),row.names = NULL)
  if(colnames(L0)[1]=="X"){L0 <- L0[,-1]}
  colnames(L0)[grep("MCV", colnames(L0))] <- "MCV"
  
  category_m0 <- read.csv(file.path(Base_Path, worktype, "4. Modelling/writedata/selected models", CategoryM0_file ),row.names = NULL)
  
  category_m0 <- category_m0 %>% filter(selectedmodels == "Yes")
  category_m01 <- category_m0 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG, MCV = MCV.MCV, Price_elas, Revenue, Volume)
  category_m02 <- category_m01 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG, cat_elas = Price_elas, CatVal = Revenue, CatVol = Volume)
  
  #L0 <- bind_rows(L0, category_m01)
  L01 <- subset(L0, select = -c(Price, CSF, MCV, Price_elas, Revenue))
  
  if(Integration_process_needed == "Yes"){
    L01 <- L01[,c(1:6,(ncol(L01)-2):ncol(L01), 7,10:(ncol(L01)-3))]
  }
  
  colnames(L01)[c((match("Volume", colnames(L01))+1):ncol(L01))] <- gsub("\\.", " ", colnames(L01)[c((match("Volume", colnames(L01))+1):ncol(L01))])
  
  L01 <- left_join(L01, category_m02[,c(1:5,10:12)], by = c("Market", "Channel", "Region", "Category", "SubCategory")) 
  L01$MS <- L01$Volume / L01$CatVol
  L01$cat_elas_new <- L01$cat_elas * L01$MS
  L01$cat_vol_chng <- L01$cat_elas_new * L01$CatVol
  
  k=11
  for(k in c(11:(match("cat_elas", colnames(L01))-1))){
    
    L01[,k][which(L01$Brand == colnames(L01)[k])] <- 0
    
  }
  
  vol_chng <- L01$Volume * L01[,c(11:(match("cat_elas", colnames(L01))-1))]
  vol_chng$category_loss <- -L01$cat_vol_chng
  
  vol_decom <- vol_chng/rowSums(vol_chng, na.rm = T)
  colnames(L01)
  vol_decom <- cbind(L01[,c(1:9)], vol_decom)
  
  write.csv(vol_decom, file.path(Base_Path, worktype, "9. Validator Output", "Volume_decomposition.csv"), row.names = F)
  write.csv(category_m0, file.path(Base_Path, worktype, "9. Validator Output", "FinalM0_Category.csv"), row.names = F)
  
}


#################################################################################################################
############# MSP RPI curves #########################################

RPI_curves <- function(project_root, file_for_curves, CategoryM0_file, CMA_input, NSV_input, restofcategory_included, worktype){

L0 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", file_for_curves),row.names = NULL)
colnames(L0)[grep("MCV", colnames(L0))] <- "MCV"

CategoryM0_file <- "FinalM0_Category.csv"

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

write.csv(D0, file.path(Base_Path, worktype, "9. Validator Output", "D0.csv"), row.names = F)

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

csf_period_styr <- min(lastyrdata$Year)
csf_period_stwk <- min(lastyrdata$Week[lastyrdata$Year==csf_period_styr])
csf_period_endyr <- max(lastyrdata$Year)
csf_period_enwk <- max(lastyrdata$Week[lastyrdata$Year==csf_period_endyr])

D0$yw <- paste(D0$Year, D0$Week, sep = "_")
D0$ly <- lastyrdata$yw[match(D0$yw, lastyrdata$yw)]
unique(D0$ly)

D0 <- D0 %>% filter(!is.na(D0$ly)) %>%  filter(BrCatId == "Category") 
D0_summary <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]]), sum)  
colnames(D0_summary) <- c(colnames(D0)[c(1:5)], "Revenue", "Volume")    

d=5
for(d in c(1:5)){
  if(unique(D0_summary[,d])[1] == tolower(paste("all", colnames(D0_summary)[d], sep = ""))){
    D0_summary[d] <- "all"
  }
}

category_m0 <- read.csv(file.path(Base_Path, worktype, "4. Modelling/writedata/selected models", CategoryM0_file ),row.names = NULL)

category_m0 <- category_m0 %>% filter(selectedmodels == "Yes")
category_m01 <- category_m0 %>% dplyr::select(Market, Channel, Region,Category,SubCategory, Brand, Variant, PackType, PPG, MCV = MCV.MCV, Price_elas, Revenue, Volume)

category_m01 <- left_join(category_m01, D0_summary, by = c(colnames(D0_summary)[1:5]))    
colnames(category_m01)

category_m01$Revenue.x <- category_m01$Revenue.y
category_m01$Volume.x <- category_m01$Volume.y

category_m01 <- category_m01[,-c((ncol(category_m01)-1), ncol(category_m01))]
colnames(category_m01)[c((ncol(category_m01)-1), ncol(category_m01))] <- c("Revenue", "Volume")

L0 <- bind_rows(L0, category_m01)

allcurvedata <- data.frame(matrix(nrow = 0, ncol = 4))
allMSP <- data.frame(matrix(nrow = 0, ncol = 4))

i=j=k=l=m=1
mktlist <- unique(L0$Market)
for(i in c(1:length(mktlist))){
  mkt=mktlist[i]
  L0_mkt <- L0 %>% filter(Market == mkt)
  
  chnllist <- unique(L0_mkt$Channel)
  for(j in c(1:length(chnllist))){
    chnl=chnllist[j]
    L0_chnl <- L0_mkt %>% filter(Channel == chnl)
    
    reglist <- unique(L0_chnl$Region)
    for(k in c(1:length(reglist))){
      reg=reglist[k]
      L0_reg <- L0_chnl %>% filter(Region == reg)
      
      catlist <- unique(L0_reg$Category)
      for(l in c(1:length(catlist))){
        cat=catlist[l]
        L0_cat <- L0_reg %>% filter(Category == cat)
        
        subcatlist <- unique(L0_cat$SubCategory)
        for(m in c(1:length(subcatlist))){
          subcat=subcatlist[l]
          L0_subcat <- L0_cat %>% filter(SubCategory == subcat)
        
          
          if(restofcategory_included == "No"){
          
            L0_subcat <- L0_subcat %>% filter(Brand != "restofcategory")
          
          }else{
            
            L0_subcat <- L0_subcat
            
          }
          
          if(length(which(is.na(L0_subcat$Revenue)))>0){
            L0_subcat <- L0_subcat[-which(is.na(L0_subcat$Revenue)),]
          }
          
          L0_subcat$Price <- L0_subcat$Revenue / L0_subcat$Volume
          L0_subcat$Price_ModelPeriod <- L0_subcat$Revenue_Total / L0_subcat$Volume_Total
          L0_subcat$Price_elas[c(1:(nrow(L0_subcat)-1))] <- (L0_subcat$Price[c(1:(nrow(L0_subcat)-1))] / (L0_subcat$Price[c(1:(nrow(L0_subcat)-1))] - L0_subcat$MCV[c(1:(nrow(L0_subcat)-1))]))*5
          
          L0_subcat$CS <- (0.5 * (L0_subcat$MCV-L0_subcat$Price) * L0_subcat$Volume) + L0_subcat$Revenue
          L0_subcat$MShare <- L0_subcat$Revenue / L0_subcat$Revenue[match("all", L0_subcat$Brand)]
          L0_subcat$NewMShare <- L0_subcat$CS/sum(L0_subcat$CS[1:(nrow(L0_subcat)-1)]) * sum(L0_subcat$MShare[1:(nrow(L0_subcat)-1)])
          L0_subcat$MSP <- L0_subcat$NewMShare - L0_subcat$MShare 
          
          colnames(L0_subcat)
          
          if(file_for_curves == "L0L2.csv"){
            
            L0_subcat$Compname <- paste(L0_subcat[,match(L0_indicator, colnames(L0_subcat))], L0_subcat[,match(L2_indicator, colnames(L0_subcat))], sep = "_")
            level <- "L0L2" 
            
          }else if(file_for_curves == "L0L2L3.csv"){
            
            L0_subcat$Compname <- paste(L0_subcat[,match(L0_indicator, colnames(L0_subcat))], L0_subcat[,match(L2_indicator, colnames(L0_subcat))], L0_subcat[,match(L3_indicator, colnames(L0_subcat))], sep = "_")
            level <- "L0L2L3"
            
          }else{
            
            L0_subcat$Compname <- L0_subcat[,match(L0_indicator, colnames(L0_subcat))]
            level <- "L0"
            
            }
          
          L0_subcat_wcategory <- L0_subcat
          L0_subcat <- L0_subcat[-nrow(L0_subcat),]
          if(nrow(L0_subcat)==0){next}
          
          allMSP <- bind_rows(allMSP, L0_subcat)
          
          pr_chng <- seq(-0.50,0.50,length=501)
          
          n=2
          for(n in c(1:nrow(L0_subcat))){
          
          otherbrands_CS <- sum(L0_subcat$CS[-n])
          otherbrands_avgvol <- sum(L0_subcat$Volume[-n])
          otherbrands_avgval <- sum(L0_subcat$Revenue[-n])
          scope_MS <- sum(L0_subcat$MShare)
          MShare <- L0_subcat$MShare[n]
          CategoryPrice <- L0_subcat_wcategory$Price[nrow(L0_subcat_wcategory)]
          CategoryElas <- L0_subcat_wcategory$Price_elas[nrow(L0_subcat_wcategory)]
          
          curve_for <- L0_subcat[n,]  
          
          curve_data <- as.data.frame(cbind(pr_chng, MCV = curve_for$MCV, Price = curve_for$Price, Volume = curve_for$Volume , Price_elas = curve_for$Price_elas))
          curve_data$new_price <- curve_data$Price + as.numeric(curve_data$pr_chng * curve_data$Price) 
          
          #curve_data$new_Vol2 <- ((curve_data$MCV-curve_data$new_price)/(curve_data$MCV-curve_data$Price))*curve_data$Volume
          curve_data$deltaV <- curve_data$Price_elas/5*(curve_data$new_price-curve_data$Price)*curve_data$Volume/curve_data$Price
          curve_data$new_Vol <- curve_data$Volume + curve_data$deltaV
          curve_data$new_Val <- curve_data$new_Vol * curve_data$new_price
          
          curve_data$newCS <-( 0.5 * ((curve_data$MCV - curve_data$new_price) * curve_data$new_Vol)) + curve_data$new_Val
          #curve_data$newCS2 <- 0.5 * (((curve_data$MCV - curve_data$new_price)^2) * curve_data$Volume) / (curve_data$MCV - curve_data$Price)
          
          curve_data$otherbrand_CS <- otherbrands_CS
          curve_data$otherbrand_avgval <- otherbrands_avgval
          curve_data$category_avgval <- L0_subcat_wcategory$Revenue[nrow(L0_subcat_wcategory)]
          curve_data$category_avgvol <- L0_subcat_wcategory$Volume[nrow(L0_subcat_wcategory)]
          
          curve_data$total_CS <- curve_data$newCS + curve_data$otherbrand_CS
          curve_data$new_MShare <- (curve_data$newCS/curve_data$total_CS) * scope_MS
          curve_data$MShare <- MShare
          curve_data$New_MSP <- curve_data$new_MShare - curve_data$MShare
          
          curve_data$CategoryPrice <- CategoryPrice
          curve_data$CategoryPrice_chng <- curve_data$pr_chng * curve_data$MShare
          curve_data$new_CategoryPrice <- curve_data$CategoryPrice + (curve_data$CategoryPrice * curve_data$CategoryPrice_chng)
          
          curve_data$Cat_deltaV <- CategoryElas/5*(curve_data$new_CategoryPrice-curve_data$CategoryPrice)* curve_data$category_avgvol/curve_data$CategoryPrice
          curve_data$new_CatVol <- curve_data$category_avgvol + curve_data$Cat_deltaV  
          curve_data$new_CatVal <- curve_data$new_CatVol * curve_data$new_CategoryPrice  
          
          curve_data$new_value_using_category <- curve_data$new_CatVal * curve_data$new_MShare
          curve_data$new_volume_using_category <- curve_data$new_value_using_category / curve_data$new_price
          
          #### CMA calculations 
          
          curve_data$current_CMA <- CMA_input/100 * curve_data$Price
          curve_data$current_Cost <- curve_data$Price - curve_data$current_CMA
          curve_data$New_CMA_perunit <- (curve_data$new_price - curve_data$current_Cost)
          curve_data$New_CMA <- (curve_data$New_CMA_perunit * curve_data$new_volume_using_category) * (NSV_input/100) ##New changes
          curve_data$Current_total_CMA <- (curve_data$current_CMA * curve_data$Volume)*(NSV_input/100) ##New changes
          curve_data$New_CMA_percent <- curve_data$New_CMA / curve_data$Current_total_CMA
          
          curve_data$NSV <- curve_data$new_value_using_category * (NSV_input/100)
          curve_data$Current_value <- (curve_data$Price * curve_data$Volume)* (NSV_input/100) ##New changes
          curve_data$NSV_percent <- curve_data$NSV / curve_data$Current_value
          #curve_data$NSV_percent <- curve_data$NSV / curve_data$NSV[which(curve_data$pr_chng == 0)]
          
          comp_price <- as.data.frame(t(as.data.frame(L0_subcat$Price)))
          colnames(comp_price) <- L0_subcat$Compname
          comp_price$category <- CategoryPrice ##New changes
          
          curve_data <- cbind(curve_data,comp_price)
          rpi_data <- curve_data$new_price / curve_data[,c((match("NSV_percent", colnames(curve_data))+1):ncol(curve_data))]
          colnames(rpi_data) <- paste(colnames(curve_data)[c((match("NSV_percent", colnames(curve_data))+1):ncol(curve_data))], "rpi", sep = "_")
          #colnames(rpi_data) <- paste(colnames(rpi_data), "rpi", sep = "_")
          
          curve_data <- cbind(curve_data,rpi_data)
          
          curve_data$Market <- mkt
          curve_data$Channel <- chnl
          curve_data$Region <- reg
          curve_data$Category <- cat
          curve_data$SubCategory <- subcat
          
          cbind_data <- as.data.frame(L0_subcat[n,c((match("SubCategory", colnames(L0_subcat))+1):(match("MCV", colnames(L0_subcat))-1))])
          colnames(cbind_data) <- colnames(L0_subcat)[c((match("SubCategory", colnames(L0_subcat))+1):(match("MCV", colnames(L0_subcat))-1))]
          
          curve_data <- cbind(curve_data, cbind_data)
          
          curve_data$csf_period_styr <- csf_period_styr
          curve_data$csf_period_stwk <- csf_period_stwk
          curve_data$csf_period_endyr <- csf_period_endyr
          curve_data$csf_period_enwk <- csf_period_enwk
          curve_data$csf_period <- csf_period
          
          allcurvedata <- bind_rows(allcurvedata, curve_data) 
          
          }
          
        }
      }
    }   
  } 
}

write.csv(allcurvedata, file.path(Base_Path, worktype, "9. Validator Output", paste("RPI Curves_", level, ".csv", sep = "")), row.names = F)
write.csv(allMSP, file.path(Base_Path, worktype, "9. Validator Output", paste("MSP_", level, ".csv", sep = "")), row.names = F)


}

#################################################################################################################
################################################################################################################
        
L2_adjustment1 <- function(project_root, D0_file, L0_indicator, L2_indicator, worktype, csf_period){  

        ############## L2 MCV adjustment 1 ##################################
        
        L0 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", paste("column_selector_", L0_indicator, ".csv", sep = "")),row.names = NULL)
        L2 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", paste("column_selector_", L2_indicator, ".csv", sep = "")),row.names = NULL)
        
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
        
        L0 <- L0[,c(1:5,match(L0_indicator, colnames(L2)), match("MCV.MCV", colnames(L2)))]
        L2 <- L2[,c(1:5,match(L2_indicator, colnames(L2)), match("MCV.MCV", colnames(L2)))]
        
        L0L2 <- left_join(L0, L2, by = c("Market", "Channel", "Region", "Category", "SubCategory"))
        
        colnames(D0)    
        
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
        
        D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]]), sum)  
        colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
        
        for(d in c(1:5)){
          if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
            D0_summarytotal[d] <- "all"
          }
        }
        
        D0 <- D0 %>% filter(!is.na(D0$ly)) 
        D0_summary <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]]), sum)  
        colnames(D0_summary) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)))], "Revenue", "Volume")    
        
        d=5
        for(d in c(1:5)){
          if(unique(D0_summary[,d])[1] == tolower(paste("all", colnames(D0_summary)[d], sep = ""))){
            D0_summary[d] <- "all"
          }
        }
        
        
        L0L2 <- left_join(L0L2, D0_summary, by = c(colnames(D0_summary)[1:7]))    
        L0L2$Volume[is.na(L0L2$Volume)] <- 0  
        
        L0L2$MCV.MCV.y[which(L0L2$Channel == "target" & L0L2$Brand == "restofcategory" & L0L2$Variant == "mayo")] <- 18
        wt_MCV <- L0L2 %>% 
          dplyr::select(everything()) %>% 
          group_by(Market, Channel, Region, Category, SubCategory, Brand) %>% summarise(MCV=weighted.mean(MCV.MCV.y, Volume)) %>% 
          arrange(Market, Channel, Region, Category, SubCategory, Brand) %>% ungroup()
        
        L0L2 <- left_join(L0L2, wt_MCV, by = c(colnames(wt_MCV)[1:6]))
        
        
        L0L2$index <- L0L2$MCV / L0L2$MCV.MCV.x
        L0L2$L2_MCV <- L0L2$MCV.MCV.y / L0L2$index     
        
        L0L2 <- L0L2 %>% filter(Volume > 0)
        L0L2 <- L0L2 %>% filter(Revenue > 0)
        L0L2 <- L0L2[,c(1:6, 8, ncol(L0L2), match("Revenue", colnames(L0L2)),match("Volume", colnames(L0L2)) )]
        
        
        L0L2$Price <- L0L2$Revenue / L0L2$Volume
        L0L2$CSF <- L0L2$L2_MCV / L0L2$Price
        L0L2$CSF[which(L0L2$CSF==Inf)] <- 1.05
        L0L2$CSF[which(L0L2$CSF<1)] <- 1.05
        L0L2$L2_MCV <- L0L2$CSF * L0L2$Price
        
        L0L2 <- left_join(L0L2, D0_summarytotal, by = colnames(D0_summarytotal)[1:7])
        write.csv(L0L2, file.path(Base_Path, worktype, "9. Validator Output", "L0L2.csv"), row.names = F)
        
}


################################################################################################################
############################################################################################################


L3_adjustment1 <- function(project_root, D0_file, L3_indicator, worktype, csf_period){ 

        ####################### L3 MCV adjustment 1 ##################################

        L0L2 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", "L0L2.csv"),row.names = NULL)
        if(colnames(L0L2)[1]=="X"){L0L2 <- L0L2[,-1]}

        L3 <- read.csv(file.path(Base_Path, worktype, "9. Validator Output", paste("column_selector_", L3_indicator, ".csv", sep = "")),row.names = NULL)

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

        colnames(L0L2)

        colnames(L3)
        L3 <- L3[,c(1:5,match(L3_indicator,colnames(L3)),match("MCV.MCV",colnames(L3)) )]

        L0L2L3 <- left_join(L0L2, L3, by = c("Market", "Channel", "Region", "Category", "SubCategory"))
        colnames(D0)

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

        D0_summarytotal <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]], D0[[match(L3_indicator, colnames(D0))]]), sum)  
        colnames(D0_summarytotal) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)), match(L3_indicator, colnames(D0)))], "Revenue_Total", "Volume_Total")    
        
        for(d in c(1:5)){
          if(unique(D0_summarytotal[,d])[1] == tolower(paste("all", colnames(D0_summarytotal)[d], sep = ""))){
            D0_summarytotal[d] <- "all"
          }
        }
        
        
        D0 <- D0 %>% filter(!is.na(D0$ly))
        D0_summary <- aggregate(D0[c(16,17)], list(grp = D0[[1]], D0[[2]], D0[[3]], D0[[4]], D0[[5]], D0[[match(L0_indicator, colnames(D0))]], D0[[match(L2_indicator, colnames(D0))]], D0[[match(L3_indicator, colnames(D0))]]), sum)
        colnames(D0_summary) <- c(colnames(D0)[c(1:5,match(L0_indicator, colnames(D0)), match(L2_indicator, colnames(D0)), match(L3_indicator, colnames(D0)))], "Revenue", "Volume")

        d=5
        for(d in c(1:5)){
          if(unique(D0_summary[,d])[1] == tolower(paste("all", colnames(D0_summary)[d], sep = ""))){
            D0_summary[d] <- "all"
          }
        }

        L0L2L3 <- left_join(L0L2L3, D0_summary, by = c(colnames(D0_summary)[1:8]))
        L0L2L3$Volume.y[is.na(L0L2L3$Volume.y)] <- 0

        L0L2L3 <- left_join(L0L2L3, D0_summarytotal, by = c(colnames(D0_summarytotal)[1:8]))    
        L0L2L3$Volume_Total.y[is.na(L0L2L3$Volume_Total.y)] <- 0  
        
        
        wt_MCV <- L0L2L3 %>%
          dplyr::select(everything()) %>%
          group_by_at(1:7) %>% summarise(MCV=weighted.mean(MCV.MCV, Volume.y)) %>%
          ungroup()
        
        L0L2L3 <- left_join(L0L2L3, wt_MCV, by = c(colnames(wt_MCV)[1:7]))
        L0L2L3$index <- L0L2L3$MCV / L0L2L3$L2_MCV
        L0L2L3$L3_MCV <- L0L2L3$MCV.MCV / L0L2L3$index

        L0L2L3 <- L0L2L3 %>% filter(Volume.y > 0)
        colnames(L0L2L3)
        
        L0L2L3 <- L0L2L3[,c(1:7,match(L3_indicator, colnames(L0L2L3)), ncol(L0L2L3), match("Revenue.y", colnames(L0L2L3)), match("Volume.y", colnames(L0L2L3)), match("Revenue_Total.y", colnames(L0L2L3)), match("Volume_Total.y", colnames(L0L2L3)))]
        colnames(L0L2L3)[c(10,11,12,13)] <- c("Revenue", "Volume", "Revenue_Total", "Volume_Total")
        
        L0L2L3$Price <- L0L2L3$Revenue / L0L2L3$Volume
        L0L2L3$CSF <- L0L2L3$L3_MCV / L0L2L3$Price
        L0L2L3$CSF[which(L0L2L3$CSF<1)] <- 1.05
        L0L2L3$L3_MCV <- L0L2L3$CSF * L0L2L3$Price
        
        L0L2L3 <- L0L2L3[,c(1:11,14,15,12,13)]
        write.csv(L0L2L3, file.path(Base_Path, worktype, "9. Validator Output", "L0L2L3.csv"), row.names = F)

        ###############################################################################################
}
        
        
##############################################################################################################################        
        
        