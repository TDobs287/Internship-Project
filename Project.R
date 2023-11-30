
# NOTICE -- Majority of project's code has been removed for protection and privacy. 
#           The following is data manipulation techniques and data visualization code only.




    #### File Reader ----
    file_data <- reactive({
      
      #Reads in the input file
      file_input <- input$file
      
      #If the file is empty, return
      if(is.null(file_input)){return()}
      
      #Reads in the data frame first to determine how to properly read in the file's table
      read_table <- read.table(file = file_input$datapath, header = FALSE, fill = TRUE)
      
      #If the data frame is a table from a txt file, read it in this way
      if("V5" %in% colnames(read_table))
      {
        #Reads in a text file
        OPTI_data <- read_delim(file = file_input$datapath, col_names = FALSE, skip = 1)
        
        #Reformat data
        rename_OPTI_data <- OPTI_data %>% 
          mutate(X6 = as.character(X6)) %>% 
          select(X5:X8) %>% 
          rename(Wafer_Num = X5, Site = X6, FirstThickness = X7, GOF = X8)
        
        OPTI_table <- rename_OPTI_data
        
        
        ## 1st TH Mean
        
        #Groups up the 1st TH of all sites
        OPTI_dt_WIDE_TH <- dcast(OPTI_table,
                                 Wafer_Num ~ Site,
                                 value.var = c("FirstThickness"))
        
        #Isolates the site columns
        OPTI_TH_cols <- OPTI_dt_WIDE_TH %>% 
          select(2:6)
        
        #Takes the mean of the 1st TH for each site
        OPTI_dt_TH_Mean <- rowMeans(OPTI_TH_cols, na.rm = TRUE)
        
        #Puts the mean data into a data frame
        OPTI_df_TH <- data.frame(t(OPTI_dt_TH_Mean))
        
        #Transforms the data frame matrix
        OPTI_df2_TH <- t(OPTI_df_TH)
        
        #Renames the data column
        OPTI_df2_TH_rename <- data.table(OPTI_df2_TH) %>% 
          rename(Mean = V1)
        
        
        #Appends the rv data combined with the TH mean and site data table
        OPTI_Append_RV <- qpcR:::cbind.na(DB_data, OPTI_TH_cols, OPTI_df2_TH_rename)
        
        
        #Select the relevant columns for the plot
        OPTI_dt_TH_Mean <- OPTI_Append_RV %>% 
          select(3,6,7,8,9,10,11)
        
        #Extend the data frame
        OPTI_dt_LONG_TH <- reshape2::melt(OPTI_dt_TH_Mean, id.vars = "Slot")
        
        #Reorder the data frame
        OPTI_dt_LONG_TH_reorder <- setorder(OPTI_dt_LONG_TH)
        
        #Rename the data frame
        OPTI_dt_LONG_TH_rename <- OPTI_dt_LONG_TH_reorder %>% 
          rename(Site = variable, FirstThickness = value)
        
        
        
        ## GOF Mean
        
        #Groups up the GOF of all sites
        OPTI_dt_WIDE_GOF <- dcast(OPTI_table,
                                  Wafer_Num ~ Site,
                                  value.var = c("GOF"))
        
        #Isolates the site columns
        OPTI_GOF_cols <- OPTI_dt_WIDE_GOF %>% 
          select(2:6)
        
        #Takes the mean of the 1st TH for each site
        OPTI_dt_GOF_Mean <- rowMeans(OPTI_GOF_cols, na.rm = TRUE)
        
        #Puts the mean data into a data frame
        OPTI_df_GOF <- data.frame(t(OPTI_dt_GOF_Mean))
        
        #Transforms the data frame matrix
        OPTI_df2_GOF <- t(OPTI_df_GOF)
        
        #Renames the data column
        OPTI_df2_GOF <- data.table(OPTI_df2_GOF) %>% 
          rename(Mean = V1)
        
        
        #Appends the rv data combined with the GOF mean and site data table
        OPTI_Append_RV2 <- qpcR:::cbind.na(DB_data, OPTI_GOF_cols, OPTI_df2_GOF)
        
        
        #Select the relevant columns for the plot
        OPTI_dt_GOF_Mean <- OPTI_Append_RV2 %>% 
          select(3,6,7,8,9,10,11)
        
        #Extend the data frame
        OPTI_dt_LONG_GOF <- reshape2::melt(OPTI_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorder the data frame
        OPTI_dt_LONG_GOF_reorder <- setorder(OPTI_dt_LONG_GOF)
        
        #Rename the data frame
        OPTI_dt_LONG_GOF_rename <- OPTI_dt_LONG_GOF_reorder %>% 
          rename(GOF = value) %>% 
          select(GOF)
        
        
        
        ## OPTI Position
        
        #Select the relevant columns for the OPTI Position
        OPTI_Position <- OPTI_Append_RV %>% 
          select(2,6,7,8,9,10,11)
        
        #Extend the data frame
        OPTI_Position <- reshape2::melt(OPTI_Position, id.vars = "Position")
        
        #Creates a slot table to align with the positions
        OPTI_Position_Order <- reshape2::melt(OPTI_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the positions to match the slot
        OPTI_Position <- qpcR:::cbind.na(OPTI_Position_Order, OPTI_Position)
        
        #Reorder the data frame
        OPTI_Position <- setorder(OPTI_Position)
        
        #Select only the Position column
        OPTI_Position <- OPTI_Position %>% 
          select(4)
        
        
        
        ## OPTI Facing
        
        #Select the relevant columns for the OPTI WaferDirection
        OPTI_WaferDirection <- OPTI_Append_RV %>% 
          select(4,6,7,8,9,10,11)
        
        #Extend the data frame
        OPTI_WaferDirection <- reshape2::melt(OPTI_WaferDirection, id.vars = "WaferDirection")
        
        #Creates a slot table to align with the WaferDirection
        OPTI_WaferDirection_Order <- reshape2::melt(OPTI_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the facings to match the slot
        OPTI_WaferDirection <- qpcR:::cbind.na(OPTI_WaferDirection_Order, OPTI_WaferDirection)
        
        #Reorder the data frame
        OPTI_WaferDirection <- setorder(OPTI_WaferDirection)
        
        #Select only the WaferDirection column
        OPTI_WaferDirection <- OPTI_WaferDirection %>% 
          select(4)
        
        
        ## OPTI Senior Loaded
        
        #Select the relevant columns for the OPTI WaferDirection
        OPTI_SeniorLoaded <- OPTI_Append_RV %>% 
          select(5,6,7,8,9,10,11)
        
        #Extend the data frame
        OPTI_SeniorLoaded <- reshape2::melt(OPTI_SeniorLoaded, id.vars = "SeniorLoaded")
        
        #Creates a slot table to align with the WaferDirection
        OPTI_SeniorLoaded_Order <- reshape2::melt(OPTI_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the facings to match the slot
        OPTI_SeniorLoaded <- qpcR:::cbind.na(OPTI_SeniorLoaded_Order, OPTI_SeniorLoaded)
        
        #Reorder the data frame
        OPTI_SeniorLoaded <- setorder(OPTI_SeniorLoaded)
        
        #Select only the WaferDirection column
        OPTI_SeniorLoaded <- OPTI_SeniorLoaded %>% 
          select(4)
        
        
        
        ## Final OPTI
        
        #Append all the isolated OPTI data into one data frame
        clean_OPTI_data <- qpcR:::cbind.na(OPTI_dt_LONG_TH_rename, OPTI_dt_LONG_GOF_rename, OPTI_Position, OPTI_WaferDirection, OPTI_SeniorLoaded) %>% 
          filter(Site %in% input$site_info) %>%    #Allows user input to control what sites are displayed in the plot
          
          return(clean_OPTI_data)
      }
      
      
      #If the data frame is a table from a csv file, read it in this way
      else
      {
        #Reads in a csv file
        KLA_data <- read_csv(file = file_input$datapath, col_names = FALSE, skip = 1)
        
        
        ##KLA TH
        
        #KLA Site 1 isolated
        KLA_Site1_TH <- KLA_data %>%
          filter(X1 == "1") %>%
          select(X2)
        colnames(KLA_Site1_TH) <- 1
        KLA_Site1_TH <- rowid_to_column(KLA_Site1_TH)
        KLA_Site1_TH <- rename(KLA_Site1_TH, Wafer_Num = rowid)
        
        
        #KLA Site 2 isolated
        KLA_Site2_TH <- KLA_data %>%
          filter(X1 == "2") %>%
          select(X2) %>%
          rename(Site_2 = X2)
        colnames(KLA_Site2_TH) <- 2
        
        #KLA Site 3 isolated
        KLA_Site3_TH <- KLA_data %>%
          filter(X1 == "3") %>%
          select(X2) %>%
          rename(Site_3 = X2)
        colnames(KLA_Site3_TH) <- 3
        
        #KLA Site 4 isolated
        KLA_Site4_TH <- KLA_data %>%
          filter(X1 == "4") %>%
          select(X2) %>%
          rename(Site_4 = X2)
        colnames(KLA_Site4_TH) <- 4
        
        #KLA Site 5 isolated
        KLA_Site5_TH <- KLA_data %>%
          filter(X1 == "5") %>%
          select(X2) %>%
          rename(Site_5 = X2)
        colnames(KLA_Site5_TH) <- 5
        
        #KLA Mean isolated
        KLA_Mean_TH <- KLA_data %>%
          filter(X1 == "MEAN") %>%
          select(X2) %>%
          rename(Mean = X2)
        
        
        #Appends the rv data combined with the TH mean and site data table
        KLA_Add_RV <- qpcR:::cbind.na(DB_data, KLA_Site1_TH, KLA_Site2_TH, KLA_Site3_TH, KLA_Site4_TH, KLA_Site5_TH, KLA_Mean_TH)
        
        #Select the relevant columns for the plot
        KLA_dt_TH_Mean <- KLA_Add_RV %>% 
          select(3,7,8,9,10,11,12)
        
        #Extend the data frame
        KLA_dt_LONG_TH <- reshape2::melt(KLA_dt_TH_Mean, id = c("Slot"))
        
        #Reorder the data frame
        KLA_dt_LONG_TH_reorder <- setorder(KLA_dt_LONG_TH)
        
        #Rename the dataframe
        KLA_dt_LONG_TH_rename <- KLA_dt_LONG_TH_reorder %>%
          rename(Site = variable, FirstThickness = value)
        
        
        
        ##KLA GOF
        
        #KLA Site 1 isolated
        KLA_Site1_GOF <- KLA_data %>%
          filter(X1 == "1") %>%
          select(X3) %>%
          rename(Site_1 = X3)
        colnames(KLA_Site1_GOF) <- 1
        KLA_Site1_GOF <- rowid_to_column(KLA_Site1_GOF)
        KLA_Site1_GOF <- rename(KLA_Site1_GOF, Wafer_Num = rowid)
        
        
        #KLA Site 2 isolated
        KLA_Site2_GOF <- KLA_data %>%
          filter(X1 == "2") %>%
          select(X3) %>%
          rename(Site_2 = X3)
        colnames(KLA_Site2_GOF) <- 2
        
        #KLA Site 3 isolated
        KLA_Site3_GOF <- KLA_data %>%
          filter(X1 == "3") %>%
          select(X3) %>%
          rename(Site_3 = X3)
        colnames(KLA_Site3_GOF) <- 3
        
        #KLA Site 4 isolated
        KLA_Site4_GOF <- KLA_data %>%
          filter(X1 == "4") %>%
          select(X3) %>%
          rename(Site_4 = X3)
        colnames(KLA_Site4_GOF) <- 4
        
        #KLA Site 5 isolated
        KLA_Site5_GOF <- KLA_data %>%
          filter(X1 == "5") %>%
          select(X3) %>%
          rename(Site_5 = X3)
        colnames(KLA_Site5_GOF) <- 5
        
        #KLA Mean isolated
        KLA_Mean_GOF <- KLA_data %>%
          filter(X1 == "MEAN") %>%
          select(X3) %>%
          rename(Mean = X3)
        
        #Appends the rv data combined with the GOF mean and site data table
        KLA_Add_RV2 <- qpcR:::cbind.na(DB_data, KLA_Site1_GOF, KLA_Site2_GOF, KLA_Site3_GOF, KLA_Site4_GOF, KLA_Site5_GOF, KLA_Mean_GOF)
        
        #Select the relevant columns for the plot
        KLA_dt_GOF_Mean <- KLA_Add_RV2 %>% 
          select(3,7,8,9,10,11,12)
        
        #Extend the data frame
        KLA_dt_LONG_GOF <- reshape2::melt(KLA_dt_GOF_Mean, id = c("Slot"))
        
        #Reorder the data frame
        KLA_dt_LONG_GOF_reorder <- setorder(KLA_dt_LONG_GOF)
        
        #Rename the data frame
        KLA_dt_LONG_GOF_rename <- KLA_dt_LONG_GOF_reorder %>%
          rename(Site = variable, GOF = value) %>%
          select(GOF)
        
        
        
        ##KLA Position
        
        #Select the relevant columns for the KLA Position
        KLA_Position <- KLA_Add_RV %>% 
          select(2,7,8,9,10,11,12)
        
        #Extend the data frame
        KLA_Position <- reshape2::melt(KLA_Position, id.vars = "Position")
        
        #Creates a slot table to align with the positions
        KLA_Position_Order <- reshape2::melt(KLA_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the positions to match the slot
        KLA_Position <- qpcR:::cbind.na(KLA_Position_Order, KLA_Position)
        
        #Reorder the data frame
        KLA_Position <- setorder(KLA_Position)
        
        #Select only the Position column
        KLA_Position <- KLA_Position %>% 
          select(4)
        
        
        
        ##KLA Facing
        
        #Select the relevant columns for the KLA Facing
        KLA_WaferDirection <- KLA_Add_RV %>% 
          select(4,7,8,9,10,11,12)
        
        #Extend the data frame
        KLA_WaferDirection <- reshape2::melt(KLA_WaferDirection, id.vars = "WaferDirection")
        
        #Creates a slot table to align with the facings
        KLA_WaferDirection_Order <- reshape2::melt(KLA_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the facings to match the slot
        KLA_WaferDirection <- qpcR:::cbind.na(KLA_WaferDirection_Order, KLA_WaferDirection)
        
        #Reorder the data frame
        KLA_WaferDirection <- setorder(KLA_WaferDirection)
        
        #Select only the Facing column
        KLA_WaferDirection <- KLA_WaferDirection %>% 
          select(4)
        
        
        
        ##KLA Senior Loaded
        
        #Select the relevant columns for the OPTI WaferDirection
        KLA_SeniorLoaded <- KLA_Add_RV %>% 
          select(5,6,7,8,9,10,11)
        
        #Extend the data frame
        KLA_SeniorLoaded <- reshape2::melt(KLA_SeniorLoaded, id.vars = "SeniorLoaded")
        
        #Creates a slot table to align with the WaferDirection
        KLA_SeniorLoaded_Order <- reshape2::melt(KLA_dt_GOF_Mean, id.vars = "Slot")
        
        #Reorders the facings to match the slot
        KLA_SeniorLoaded <- qpcR:::cbind.na(KLA_SeniorLoaded_Order, KLA_SeniorLoaded)
        
        #Reorder the data frame
        KLA_SeniorLoaded <- setorder(KLA_SeniorLoaded)
        
        #Select only the WaferDirection column
        KLA_SeniorLoaded <- KLA_SeniorLoaded %>% 
          select(4)
        
        
        
        ##Final KLA
        
        #Append all the isolated KLA data into one data frame
        clean_KLA_data <- qpcR:::cbind.na(KLA_dt_LONG_TH_rename, KLA_dt_LONG_GOF_rename, KLA_Position, KLA_WaferDirection, KLA_SeniorLoaded)
        
        #Reclassify the type of data in the table
        clean_KLA_data <- clean_KLA_data %>%
          filter(Site %in% input$site_info) %>%    #Allows user input to control what sites are displayed in the plot
          mutate(First_Thickness = as.numeric(First_Thickness),
                 GOF = as.numeric(GOF))
        
        return(clean_KLA_data)
      }
      
    })




  
  #### PLOT RESULTS ----  
    
    #Plot function for 1st Thickness data
    plotFunc1 <- function(){
      
      #Colors representing the sites and mean, respectively
      pal <- c("red", "blue", "green", "orange", "magenta", "cyan")
      
      #Plot the file data and include textual information for the graphed points
      fig <- plot_ly(data = filteredData()) %>%
        add_trace(x = ~jitter(Slot, 0.3),
                  y = ~FirstThickness,
                  color = ~Site,
                  type = 'scatter',
                  mode = 'lines+markers',
                  hoverinfo = 'text',
                  colors = pal,
                  opacity = 0.7,
                  size = 4,
                  line = list(width = 4),
                  text = ~paste('</br> Slot: ', Slot,
                                '</br> 1st TH: ', FirstThickness,
                                '</br> Position: ', Position,
                                '</br> Wafer Direction: ', WaferDirection,
                                '</br> Senior Loaded: ', SeniorLoaded)
        )  %>%
        layout(xaxis=list(tickvals = ~Slot, ticktext = ~Slot, title = "Slot"),
               legend = list( title = list(text = "Sites")))
    }
    
    #Plot function for GOF data
    plotFunc2 <- function(){
      
      #Colors representing the sites and mean, respectively
      pal <- c("red", "blue", "green", "orange", "magenta", "cyan")
      
      #Plot the file data and include textual information for the graphed points
      fig <- plot_ly(data = filteredData()) %>%
        add_trace(x = ~jitter(Slot, 0.3),
                  y = ~GOF,
                  color = ~Site,
                  type = 'scatter',
                  mode = 'lines+markers',
                  hoverinfo = 'text',
                  colors = pal,
                  opacity = 0.7,
                  size = 4,
                  line = list(width = 4),
                  text = ~paste('</br> Slot: ', Slot,
                                '</br> GOF: ', GOF,
                                '</br> Position: ', Position,
                                '</br> WaferDirection: ', WaferDirection,
                                '</br> Senior Loaded: ', SeniorLoaded)
        )  %>%
        layout(xaxis=list(tickvals = ~Slot, ticktext = ~Slot, title = "Slot"),
               legend = list( title = list(text = "Sites")))
    }
    
    
    #Require file input and load plot action button for 1st TH plot load
    output$plot1 <- renderPlotly({
      plotFunc1()
    })
    
    #Require file input and load plot action button for GOF plot load
    output$plot2 <- renderPlotly({
      plotFunc2()
    })
    
