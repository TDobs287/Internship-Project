# Internship-Project
Data manipulation and visualization application.


renv::activate()
renv::restore()

# Libraries ----
library(shiny)

library(plotly)

library(DT) #output of datatables

library(tidyverse) #add_row function

library(data.table) #setorder function

library(reshape2) #address the warnings given regarding the melt function

library(shinymanager) #password authentication

library(yaml)

library(DBI) #database library

library(RODBC)  #compute datetime for DB input

library(yaml)

library(odbc)

library(glue)


#Password Authentication
credentials <- data.frame(
  user = c("#######"), # mandatory
  password = c("#######") # mandatory
)


config <- yaml::read_yaml("~/ShinyApps/.config/config.yml")

con <- DBI::dbConnect(odbc::odbc(), .connection_string = config$sql_connection$shinydata)

dbIsValid(con)





# UI ----
ui <- fluidPage(
  
  
  #### Sidebar ----
  sidebarLayout(
    
    sidebarPanel(
      
      width = 2,
      
      br(),
      
      actionButton("reset",label = "Reset"),
      
      br(),
      br(),
      br(),
      br(),
      
      #Site Input ----
      checkboxGroupInput(
        inputId = "site_info",
        label = strong("Select Data:", style = "font-family: 'arial'; font-size: 12px"),
        choices = list("Mean" = 'Mean',
                       "Site 1" = 1,
                       "Site 2" = 2,
                       "Site 3" = 3,
                       "Site 4" = 4,
                       "Site 5" = 5),
        selected = c('Mean', 1, 2, 3, 4, 5)
      ),
      
    ),
    
    
    
    #### Body ----
    mainPanel(
      tabsetPanel(
        type = "tabs",
        id = "tab_selected",
        
        
        tabPanel(
          title = "Projects Table",
          
          DTOutput("projects_table"),
          
        ),
        
        
        tabPanel(
          title = "Data Entry",
          
          textInput(inputId = "title",
                    label = strong("Title:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          textAreaInput(inputId = "notes",
                        label = strong("Notes:", style = "font-family: 'arial'; font-size: 12px"),
                        value = "",
                        width = '1000px',
                        height = '100px',
                        placeholder = NULL),
          
          textInput(inputId = "creator",
                    label = strong("Creator:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          
          actionButton("save_project",label = "Save Project"),
          
          br(),
          br(),
          
          tags$h4("Project ID:"),
          textOutput("projID_output"),
          
          br(),
          br(),
          
          
          tags$h3("Test Runs"),
          DTOutput("save_TR_table"),
          
          tags$h5("Completed Date Format: YYYY-MM-DD HH:MI:SS"),
          actionButton("save_testrun",label = "Save Test"),
          
          br(),
          br(),
          
          tags$h4("Test Run ID:"),
          textOutput("TR_ID_output"),
          
          br(),
          br(),
          
          #Number of Wafers Input
          numericInput(
            inputId = "wafers",
            label = strong("Number of Wafers:", style = "font-family: 'arial'; font-size: 12px"),
            value = NA,
            min = 1,
            max = 50,
            step = 1
          ),
          
          actionButton("createTable",label = "Create Position DataTable"),
          
          br(),
          br(),
          
          tags$h3("Position Data"),
          DTOutput("new_positions_table"),
          
          actionButton("save_pos",label = "Save Position Data"),
          
          br(),
          br(),
          br(),
          
        ),
        
        tabPanel(
          title = "Load Project",  
          
          textInput(inputId = "title2",
                    label = strong("Title:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          actionButton("load_project",label = "Load Project"),
          
          br(),
          br(),
          br(),
          
          textAreaInput(inputId = "notes2",
                        label = strong("Notes:", style = "font-family: 'arial'; font-size: 12px"),
                        value = "",
                        width = '1000px',
                        height = '100px',
                        placeholder = NULL),
          
          textInput(inputId = "creator2",
                    label = strong("Creator:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          br(),
          
          tags$h4("Project ID:"),
          textOutput("projID_output2"),
          
          br(),
          br(),
          
          tags$h3("Test Runs"),
          br(),
          DTOutput("load_TR_table"),
          
          br(),
          br(),
          
          textInput(inputId = "testrun",
                    label = strong("Test Run ID:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          actionButton("load_pos_data",label = "Load Position Data"),
          
          br(),
          br(),
          
          tags$h3("Position Data"),
          
          br(),
          
          DTOutput("load_positions_table"),
          
          br(),
          
          actionButton("delete_pos", label = "Delete Position Data"),
          
          br(),
        ),
        
        
        tabPanel(
          title = "File Input",
          
          textInput(inputId = "TR_DT",
                    label = strong("Test Run ID:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          br(),
          
          #File Input ----
          fileInput(
            inputId = "file",
            label = strong("Choose File:", style = "font-family: 'arial'; font-size: 12px"),
            accept = c(".csv", ".txt"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          
          #Button to load the plotted data
          actionButton("load_plot_data",label = "Load Data"),
          
          br(),
          br(),
          br(),
          
          DTOutput("my_datatable2"),
          
          br(),
          
          actionButton("save_plot_data",label = "Save Data"),
        ),
        
        tabPanel(
          title = "Plot Results",
          
          textInput(inputId = "Plot_data",
                    label = strong("Test Run ID:", style = "font-family: 'arial'; font-size: 12px"),
                    value = "",
                    placeholder = NULL),
          
          actionButton("delete_data", label = "Delete Data"),
          
          br(),
          br(),
          
          DTOutput("my_datatable3"),
          
          plotlyOutput("plot1"),
          
          plotlyOutput("plot2")
          
        )
      )
    )
  )
)

ui <- secure_app(ui)


server <- function(input, output, session) {
  
  
  #Check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #Authentication output
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  #resets app for new project input
  observeEvent(input$reset, {
    session$reload()
  })
  
  
  ## Projects Table Display
  
  DB_query <- glue_sql("SELECT *
                        FROM   RecOpt_Projects")
  
  proj_table <- as.data.table(dbGetQuery(con, DB_query))
  
  output$projects_table <- renderDataTable({
    DT::datatable(proj_table)
  })
  
  
  
  #### SAVE PROJECT ----
  
  observeEvent(input$save_project, {
    Title <- input$title
    Notes <- input$notes
    Creator <- input$creator
    CreationDate <- as.POSIXct(Sys.time())
    
    
    projectsQuery <- glue_sql("INSERT INTO RecOpt_Projects (Title, Notes, Creator, CreationDate)
                               VALUES ({Title}, {Notes}, {Creator}, {CreationDate})", .con=con)
    
    dbExecute(con, projectsQuery)
    
    
    ## Acquire Project ID and Display
    formatted <- dbQuoteLiteral(con, Title)
    
    DB_query <- glue_sql("SELECT IDCol
                          FROM   RecOpt_Projects
                          WHERE  Title = {formatted}")
    
    results <- dbGetQuery(con, DB_query)
    
    
    #output ProjectID text
    projID <- reactive({
      results$IDCol
    })
    
    output$projID_output <- renderText({
      paste0(projID())
    })
    
  })
  
  
  
  
  #### SAVE TEST RUNS ----
  
  TR_table <- reactiveValues(data = {
    data.frame(ProjectID = integer(), Title = character(), Notes = character(), Tool = character(), Recipe = character(), TargetVal = numeric(), TargetUnit = character(), DelayTimeRequired = character(), IsDuplicate = integer(), PilotType = character(), PositionCount = integer(), CompletedDate = character(), Requester = character(), stringsAsFactors = FALSE) %>% 
      add_row(ProjectID = 0, Title = rep("",1), Notes = rep("",1), Tool = rep("",1), Recipe = rep("",1), TargetVal = rep(0,1), TargetUnit = rep("",1), DelayTimeRequired = rep("",1), IsDuplicate = rep(0,1), PilotType = rep("",1), PositionCount = rep(0,1), Requester = rep("",1))
  })
  
  output$save_TR_table <- renderDataTable({
    datatable(TR_table$data, editable = TRUE)
  })
  
  
  #When there is any edit to a cell, write that edit to the initial data frame
  observeEvent(input$save_TR_table_cell_edit, {
    #Get values
    info = input$save_TR_table_cell_edit
    i = info$row
    j = info$col
    k = info$value
    
    #Write values to reactive
    TR_table$data[i,j] <- k
  })
  
  
  observeEvent(input$save_testrun, {
    
    IsDuplicate <- 0
    RequestDate <- as.POSIXct(Sys.time())
    
    TR_table2 <- data.frame(IsDuplicate, RequestDate)
    
    TR_table3 <- merge(TR_table$data, TR_table2)
    
    col_order <- c("ProjectID", "Title", "Notes", "Tool", "Recipe", "TargetVal", "TargetUnit", "DelayTimeRequired", "IsDuplicate", "PilotType", "PositionCount", "CompletedDate", "Requester", "RequestDate")
    
    TR_table3 <- TR_table3[, col_order]
    
    str(TR_table3)
    
    dbWriteTable(con, "RecOpt_TestRuns", TR_table3, overwrite = FALSE, append = TRUE)
    
    
    
    ## Acquire TR ID and Display
    Title <- TR_table$data$Title
    
    formatted <- dbQuoteLiteral(con, Title)
    
    DB_query <- glue_sql("SELECT IDCol
                           FROM   RecOpt_TestRuns
                           WHERE  Title = {formatted}")
    
    results <- dbGetQuery(con, DB_query)
    
    
    #output ProjectID text
    TR_ID <- reactive({
      results$IDCol
    })
    
    output$TR_ID_output <- renderText({
      paste0(TR_ID())
    })
    
    
  })
  
  
  
  #### CREATE/SAVE POSITIONS DATA TABLE ----
  
  #Change rows (Outputs pos_table data and datatable with new row amount)
  observeEvent(input$createTable, {
    
    rowNum <- input$wafers
    
    
    #Initialize a blank data frame for the user to interact with
    pos_table <- reactiveValues(data = {
      data.frame(TestRunID = integer(), Position = integer(), Slot = integer(), WaferDirection = character(), SeniorLoaded = integer(), stringsAsFactors=FALSE) %>%
        add_row(TestRunID = 0, Position = rep(0,rowNum), Slot = rep(0,rowNum), WaferDirection = rep("",rowNum), SeniorLoaded = rep(0,rowNum))
    })
    
    
    #Output the data table based on the data frame and make it editable
    output$new_positions_table <- renderDataTable({
      datatable(pos_table$data, editable = TRUE)
    })
    
    
    #When there is any edit to a cell, write that edit to the initial data frame
    observeEvent(input$new_positions_table_cell_edit, {
      #Get values
      info = input$new_positions_table_cell_edit
      i = info$row
      j = info$col
      k = info$value
      
      #Write values to reactive
      pos_table$data[i,j] <- k
    })
    
    
    observeEvent(input$save_pos, {
      
      dbWriteTable(con, "RecOpt_Positions", pos_table$data, overwrite = FALSE, append = TRUE)
      
    })
    
  })
  
  
  #### LOAD PROJECT & TEST RUNS ----
  
  observeEvent(input$load_project, {
    
    Project <- input$title2
    
    formatted <- dbQuoteLiteral(con, Project)
    
    DB_query <- glue_sql("SELECT IDCol, Title, Notes, Creator
                           FROM   RecOpt_Projects
                           WHERE  Title = {formatted}")
    
    results <- dbGetQuery(con, DB_query)
    
    updateTextInput(session, "notes2", value = results$Notes)
    updateTextInput(session, "creator2", value = results$Creator)
    
    
    
    #output ProjectID text
    projID <- reactive({
      results$IDCol
    })
    
    output$projID_output2 <- renderText({
      paste0(projID())
    })
    
    
    #Load/Modify Test Runs ----
    
    formatted2 <- dbQuoteLiteral(con, results$IDCol)
    
    DB_query2 <- glue_sql("SELECT *
                            FROM   RecOpt_TestRuns
                            WHERE  ProjectID = {formatted2}")
    
    
    DB_data <- as.data.table(dbGetQuery(con, DB_query2))
    
    output$load_TR_table <- DT::renderDataTable({
      datatable(DB_data, editable = TRUE)
    })
    
    observeEvent(input$load_TR_table_cell_edit, {
      
      info = input$load_TR_table_cell_edit
      row = info$row
      col = info$col
      value = info$value
      
      column_name = colnames(DB_data)[col]
      query = paste0("UPDATE RecOpt_TestRuns SET ", column_name, " = '", value, "' WHERE IDCol = ", DB_data[row, "IDCol"])
      dbSendQuery(con, query)
      
    })
    
    
  })
  
  
  #### LOAD POSITIONS TABLE ----
  
  observeEvent(input$load_pos_data, {
    
    TR_ID <- input$testrun
    
    formatted <- dbQuoteLiteral(con, TR_ID)
    
    
    #Load/Modify Positions ----
    
    DB_query <- glue_sql("SELECT *
                            FROM   RecOpt_Positions
                            WHERE  TestRunID = {formatted}")
    
    DB_data <- as.data.table(dbGetQuery(con, DB_query))
    
    output$load_positions_table <- renderDataTable({
      DT::datatable(DB_data, editable = TRUE)
    })
    
    observeEvent(input$load_positions_table_cell_edit, {
      
      info = input$load_positions_table_cell_edit
      row = info$row
      col = info$col
      value = info$value
      
      column_name = colnames(DB_data)[col]
      query = paste0("UPDATE RecOpt_Positions SET ", column_name, " = '", value, "' WHERE IDCol = ", DB_data[row, "IDCol"])
      dbSendQuery(con, query)
      
    })
    
    observeEvent(input$delete_pos, {
      
      delete <- glue_sql("DELETE FROM RecOpt_Positions
                           WHERE TestRunID = {formatted}")
      
      results <- dbGetQuery(con, delete)
      
    })
    
    
  })
  
  
  #### FILE INPUT TAB ----
  
  observeEvent(input$load_plot_data, {
    
    req(input$file)
    
    TR_ID <- input$TR_DT
    
    formatted <- dbQuoteLiteral(con, TR_ID)
    
    DB_query <- glue_sql("SELECT TestRunID, Position, Slot, WaferDirection, SeniorLoaded
                            FROM   RecOpt_Positions
                            WHERE  TestRunID = {formatted}")
    
    DB_data <- as.data.table(dbGetQuery(con, DB_query))
    
    
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
    
    
    
    #Output the data table based on the data frame and make it editable
    output$my_datatable2 <- renderDataTable({
      DT::datatable(file_data())
    })
    
    
    #### Save file data ----
    
    observeEvent(input$save_plot_data, {
      
      # TR_ID <- input$TR_DT
      # 
      # formatted <- dbQuoteLiteral(con, TR_ID)
      
      DB_query2 <- glue_sql("SELECT IDCol
                           FROM   RecOpt_TestRuns
                           WHERE  IDCol = {formatted}")
      
      DB_data2 <- dbGetQuery(con, DB_query2)
      
      
      modifyFileData <- reactive({
        
        df <- file_data()
        
        df <- df %>%
          mutate(TestRunID = DB_data2$IDCol) %>%
          mutate(row_names = NULL)
        
        rownames(df) <- NULL
        
        return(df)
        
      })
      
      
      observeEvent(modifyFileData(), {
        
        dbWriteTable(con, "RecOpt_Data", modifyFileData(), overwrite = FALSE, append = TRUE)
        
      })
      
    })
    
  })
  
  
  #### PLOT RESULTS ----
  
  observeEvent(input$Plot_data, {
    
    #Allows the plotly graph to reactively update based on selected sites in the side panel
    fetchFilteredData <- function(selected_sites) {
      
      TR_ID <- input$Plot_data
      
      formatted <- dbQuoteLiteral(con, TR_ID)
      
      #Formats the selected sites for the SQL query to properly process
      sites_quoted <- paste0("'", selected_sites, "'", collapse = ", ")
      #print(sites_quoted)
      
      DB_query <- glue_sql("SELECT *
                            FROM   RecOpt_Data
                            WHERE  TestRunID = {formatted}
                            AND    Site IN (", sites_quoted, ")", .con = con)
      
      DB_data <- as.data.table(dbGetQuery(con, DB_query))
      #print(DB_data)
      
      return(DB_data)
      
    }
    
    
    filteredData <- reactive({
      
      selected_sites <- input$site_info
      
      fetchFilteredData(selected_sites)
      
    })
    
    
    observeEvent(input$delete_data, {
      
      TR_ID <- input$Plot_data
      
      formatted <- dbQuoteLiteral(con, TR_ID)
      
      delete <- glue_sql("DELETE FROM RecOpt_Data
                          WHERE TestRunID = {formatted}")
      
      results <- dbGetQuery(con, delete)
      
    })
    
    
    output$my_datatable3 <- renderDataTable({
      DT::datatable(filteredData())
    })
    
    
    
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
    
  })
  
  ##### DELETE PROJECT ----
  # (This is not implemented)
  # 
  # observeEvent(input$delete, {
  # 
  #   ProjID <- input$delete_Project
  # 
  #   formatted <- dbQuoteLiteral(con, ProjID)
  # 
  # 
  #   query1 <- glue_sql("SELECT IDCol
  #                       FROM   RecOpt_Projects
  #                       WHERE  Title = {formatted}")
  #   results <- dbGetQuery(con, query1)
  #   formatted1 <- dbQuoteLiteral(con, results$IDCol)
  #   
  #   
  #   
  #   query2 <- glue_sql("SELECT IDCol
  #                   FROM   RecOpt_TestRuns
  #                   WHERE  ProjectID = {formatted1}")
  #   results <- dbGetQuery(con, query2)
  #   formatted2 <- dbQuoteLiteral(con, results$IDCol)
  #   
  #   
  #   
  #   delete1 <- glue_sql("DELETE FROM RecOpt_Positions
  #                        WHERE TestRunID = {formatted2}")
  #   results <- dbGetQuery(con, delete1)
  #   
  #   
  #   delete2 <- glue_sql("DELETE FROM RecOpt_Data
  #                        WHERE TestRunID = {formatted2}")
  #   results <- dbGetQuery(con, delete2)
  #   
  #   
  #   delete3 <- glue_sql("DELETE FROM RecOpt_TestRuns
  #                        WHERE IDCol = {formatted2}")
  #   results <- dbGetQuery(con, delete3)
  #   
  # })
  
}

shinyApp(ui, server)

