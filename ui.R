library(markdown)
library(shiny)
library(shinydashboard)
library(shinyjs)
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

navbarPage("AUTO-CRISP",
           tabPanel("Welcome",
                    column(10, offset = 1,
                           
                    tags$h1 ("Auto-Crisp!"),
                    tags$hr(),
                    "Welcome to Auto-Crisp. In this application we will guide you through a data mining project aiming at event predicting. 
                    To complete a full data mining project it is recommended to go through all the tabs in the tools starting from data configuration to deployment.",
                    column(10, align = "center",img(src='CRISP.png', width = 600, heigth = 600))    
                    
                    )),
           navbarMenu("Data Configuration",

           tabPanel("Set Up Configuration",
                              column(10, offset = 1,
                              tags$h1("Upload and configure your medical datasets"),
                              tags$hr(),
                              tags$h5("clear old environment (this will remove previously loaded datasets)"),
                              actionButton("clear", "Clear"),
                              tags$hr(),
                              fluidRow(column(6,
                              fileInput("TPD", "Upload Time Point Data",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                              tags$hr(),

                              # Input: Checkbox if file has header ----
                              checkboxInput("headerTP", "Header", TRUE),
                              
                              # Input: Select separator ----
                              radioButtons("sepTP", "Separator", inline = TRUE,
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ";"),
                              textInput("nameTP", "Please name your dataset", value = "Time Point data"),       
                              
                              actionButton("loadTP", "Load time point data"),
                              tags$hr(),
                              selectInput("ITP_ID", "Select ID Column", c("None")),
                              selectInput("ITP_Date", "Select Date of Timepoint Column", c("None")),
                              selectInput("ITP_Time", "Select Time of Timepoint Column (optional)", c("None")),
                              selectInput("ITP_Type", "Select Type Column", c("None")),
                              selectInput("ITP_Outcome", "Select Outcome Column", c("None"))
                              )
                             

                              # Input: Select quotes ----

                              ,
                              column(6,
                                     fileInput("TID", "Upload Time Interval Data",
                                               multiple = TRUE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     tags$hr(),
                                     
                                     # Input: Checkbox if file has header ----
                                     checkboxInput("headerTI", "Header", TRUE),
                                     
                                     # Input: Select separator ----
                                     radioButtons("sepTI", "Separator",inline = TRUE,
                                                  choices = c(Comma = ",",
                                                              Semicolon = ";",
                                                              Tab = "\t"),
                                                  selected = ";"),
                                     textInput("nameTI", "Please name your dataset", value = "Time Interval data"),  
                                     actionButton("loadTI", "Load time interval data"),
                                     tags$hr(),
                                     
                                     
                                     column(10,
                                     selectInput("ITI_ID", "Select ID Column", c("None")),
                                     selectInput("ITI_SD", "Select Start Date Column", c("None")),
                                     selectInput("ITI_ED", "Select End Date Column", c("None")),
                                     selectInput("ITI_EDD", "Select End Date 2 Column (optional)", c("None")),
                                     selectInput("ITI_Type1", "Select Type 1 Column", c("None")),
                                     selectInput("ITI_Type2", "Select Type 2 Column (optional)", c("None")))
                      
                              )
                              ),
                    tags$hr(),
                    fluidRow(column(6,
                                    fileInput("COMPD", "Upload event data",
                                              multiple = TRUE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    tags$hr(),
                                    
                                    # Input: Checkbox if file has header ----
                                    checkboxInput("headerComp", "Header", TRUE),
                                    
                                    # Input: Select separator ----
                                    radioButtons("sepComp", "Separator",inline = TRUE,
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ";"),
                                    textInput("nameComp", "Please name your dataset", value = "event data"),  
                                    actionButton("loadcomp", "Load event data"),
                                    tags$hr(),
                                    selectInput("ICOMP_ID", "Select ID Column", c("None")),
                                    selectInput("ICOMP_Date", "Select Date Column", c("None")),
                                    selectInput("ICOMP_Type", "Select Type Column", c("None")),
                                    selectInput("ICOMP_NDays", "Select intervention days Column", c("None"))
                                    
                    )
                             ,
                    column(6,
                           fileInput("BASED", "Upload Baseline Data",
                                     multiple = TRUE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           tags$hr(),
                           
                           # Input: Checkbox if file has header ----
                           checkboxInput("headerBase", "Header", TRUE),
                           
                           # Input: Select separator ----
                           radioButtons("sepBase", "Separator",inline = TRUE,
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = ";"),
                           textInput("nameBase", "Please name your dataset", value = "Baseline data"),  
                           actionButton("loadBase", "Load baseline data"),
                           tags$hr(),
                           
                           # Input: Select quotes ----
                           selectInput("IBASE_ID", "Select ID Column", c("None")),
                           selectInput("IBASE_Start", "Select Start Column", c("None")),
                           selectInput("IBASE_End", "Select End Column", c("None")),
                           selectInput("IBASE_Age", "Select Age Column", c("None")),
                           selectInput("IBASE_Gender", "Select Gender Column", c("None")),
                           selectInput("IBASE_Mort", "Select Mortality date Column", c("None"))),

                    tags$br()
                    
                                     
            
                    ))
                    ,tags$hr(), 
                    column(12, align = "center",
                    tags$h5("Based on your inputs, we have got some work to do. Press the RUN button below and we will upload your datasets and prepare them for futher analysis"
                    ),
                    actionButton("run", "RUN", width = 500), tags$hr()
                    )
                    
                    ),
           tabPanel("Event Configuration", tags$head(tags$script(src = "message-handler.js")),
                      column(10, offset = 1, tags$h2("Select your class labels"),tags$hr()),
                      
  
                      fluidRow(  
                        column(5, offset = 1,
                               checkboxGroupInput("comptypes1", "", choices = NULL)),
                        column(4, checkboxGroupInput("comptypes2", "", choices = NULL)
                      )
                      
                      ),
                    fluidRow(column(10, offset = 1, tags$hr(),
                      numericInput("compdays", "Please specify the minimal days after intervention that should be included.", 30, min= 0, max = NA),
                      actionButton("preparecomp", "Set class label!", icon("refresh")),
                      checkboxInput("demo", "Demo Mode?", value = TRUE),
                
                      verbatimTextOutput("preparecompbut")
                    ))
                    )),
           
           navbarMenu("Data Understanding",
                  tabPanel("Data Description",
                    sidebarLayout(
                      sidebarPanel(
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("database", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("counterr", inline = TRUE)," Datasets"))
                          ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("ambulance", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("Neventss", inline = TRUE)," Medical Events"))
                        ),
                        tags$hr(),
                        
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("users", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("Npatientss", inline = TRUE)," Patients"))
                        ),
                        
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("medkit", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("PercentageComm", inline = TRUE)," % targeted"))
                        ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("heartbeat", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("Ntimepointss", inline = TRUE)," Time points"))
                        ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("medkit", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("AverTimee", inline = TRUE)," Time points per patients"))
                        ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("medkit", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("NvarTPp", inline = TRUE)," Time point variables"))
                        ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("medkit", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("Nintervall", inline = TRUE),"  Time intervals"))
                        ),
                        tags$hr(),
                        fluidRow(
                          column(2, align = "left", tags$h4(icon("medkit", "fa-2x"))),
                          column(7, align = "left", tags$h4 (textOutput("NvarTIi", inline = TRUE),"  Time interval variables"))
                        )
                        
                      ),
                      mainPanel(tags$h1("Data Description Report"),tags$hr(),
                              actionButton("createdatades", "Create data description report"),
                             
                              
                              
                              conditionalPanel(condition = "input.createdatades == true",
                              "This is your automated Data Description report. I would like to provide you with just some basic descriptions of your data before we dive deeper into the data. This reports has mainly two goals:",
                              tags$br(),
                              tags$ul(
                              tags$li( "Getting familair with the data"),
                              tags$li( "verifying the data quality")
                             ),
                             "Because these discriptions are directly based on your raw data, it is important to check if everything is correct and seems to make sense. If not, something probably went wrong with loading the data or specifying its characteristics.",
                             tags$br(),
                             tags$br(),
                             textOutput("datadesc"),
                             tags$br(),
                             textOutput("classdesc"),
                             tags$br(),
                             textOutput("tpdesc"),
                             tags$br(),
                             #tags$br(),
                             textOutput("tidesc"),
                             tags$br(),
                             textOutput("doubleBase"),
                             tags$br(),
                             textOutput("doubleBase1"),
                            tags$br(),
                            plotOutput("histoplot")
                      ))
                      )
                    
           ),
           tabPanel("Data Quality Verification", 
                    fluidRow(column(10, offset = 1,
  tags$div(tags$h1("Data Quality Verification"),
           tags$hr(),
           "In this report our goal is to verify the data quality of the loaded datasets. We will perform the folowing quality checks:", 
           tags$br(),
           tags$br(),

           tags$ul(
             tags$li( "Data Simularity Check"),
             tags$li( "Quality check time point variables"),
             tags$li( "Quality check time interval variables")
             
           ),
           #"The creation of your Data Qaulity report requires some work, press the button below to create your report and wait untill its ready",
          # tags$br(),
           tags$br(),
           column(12, align = "center", 
           actionButton("DQRep", "Create Data Quality Report")),
           conditionalPanel(condition = "input.DQRep > 0",
          column (10,
           tags$h2("Data Simularity"),
           htmlOutput("datasimu"),
           plotOutput("missplot"),
           tags$h2(textOutput("TP_name")),
           "The previous plot showed us how many patients occured in the time point dataset, nonetheless it only shows us that a certain patient has been observed in the full dataset. To verify the quality the next plot shows all the different timepoint variables versus the patients that have at least one measurement for that variable.",
           fluidRow(
             splitLayout(cellWidths = c("70%", "30%"),cellArgs = list(style = "padding: 6px"),
                         plotOutput("labpatientplot", height = 600),
                         DT::dataTableOutput("TP_table")
           
           )),
           "The plot above tells us something about the data quality, we can't fully determine the quality based on a 2 dimensional plot. Our goal with time point data is to show the sequential development by combining it into a time serie. This requires multiple timepoints, idealy we would have for all patients, for all variables, a great amount of timepoints. Unfortunately this often not the case in reality, to visualise this we will create a 3D plot, with the amount of patients, variables and timepoints of the axes",
           tags$br(),
           "The creation of this plot requires some work, press the following button to create the 3D plot.",
           tags$br(),
           actionButton("DDDplot", "Create 3D plot"),
           conditionalPanel(condition = "input.DDDplot > 0",
           plotlyOutput("Dplot")),
           tags$br(),
           tags$h2(textOutput("TI_name")),
           plotOutput("TIperPat"),
           "Time interval data differs from time points, it basically covers binary information about a time serie, stating for example if a patient is on a certain medication. Therefore, its harder to claim that data is missing. We can still get a sense of missing data by creating a special kind of a heatmap. This following plot will represent the time span of all registered patients. By selecting some time interval variables in the following table, we plot these interval on top of the patient periods. To verify the quality we suggest to select a subset of variables (for example blood thinners in case of medication), that should be registered mostly all the time. If the quality is high, it should show that the medication is always registered when a patient is being registered.",
          tags$br(),
          "Now first select the variables in the table. Than press the tab Show Plot and by pressing the button Create TI Plot, it will start creating it which could take some time. The colors indicate the following:", 
          tags$br(),tags$ul(
            tags$li( "Green: patient is at that time not registered within your cohort"),
            tags$li( "Purple: patient is being registered in your cohort but not within time interval"),
            tags$li( "Blue: selected time interval variable found for that patient"),
            tags$li( "Yellow: event occured")),
            
          
          tabsetPanel(type = "tabs",
                       tabPanel("Select TI variable", DT::dataTableOutput("medtable")),
                       tabPanel("Show Plot", 
                                actionButton("Cmedplot", "Create TI Plot"),
                                plotlyOutput("medplot"),
                                textOutput("testrows")
                              )) )
           
         )))))
          ,
  
           tabPanel("Data Exploration",
                    
                    column(10, offset =  1,
                    tags$h1("Exploratory Data Analysis"),
                    tags$hr(),
                    tags$h2("Kaplan Meier Curve"),
                    plotOutput("kaplan"),
                    tags$br(),
                    plotOutput("kaplann"),
                    
                    "Now that we have a sense of the data quality, we continue with the exploratory data analysis phase. Ofcourse, this can be as extensive as one would like but we stick to a few relevant plots and tables. Our goal in this phase is to get first insights into the data. We will first plot a histogram of the days after interventation when a event occured",
                    tags$br(),
                    plotOutput("eventplot"),
                    tags$br(),

                    tags$h2("Event statistics"),
                    "We will with start some very basic statistics about the events, this requires some calculation, press the button below to perform these calculations. Mean overal is based on the mean of all patients of the mean of all time. The mean events is based on calculating the mean of the variable of the day of event.",
                    tags$br(),
                    column(10, align = "center", tags$br(), actionButton("calstats", "Calculate event statistics")), tags$br(),tags$br(),
                    conditionalPanel(condition = "input.calstats > 0",
                    column(10, align = "center",
                    tableOutput("eventstat"))),
                    tags$br(),
                    tags$br(),
                    fluidRow(column(12,
                    tags$h2("Mean event plot"),
                    column(10, align = "center", tags$br(), actionButton("CMeanComPlot", "prepare dataset for mean event plot")),
                    conditionalPanel(condition = "input.CMeanComPlot > 0",
                                     
                    if(exists("TPVariables"))selectInput("TPselection", "Select your variable", choices = TPVariables ),
                    fluidRow(
                      column(4,
                             sliderInput("Bdaycompp", "Days before event", min = 10, max = 100, value = 50)),
                      column(4, 
                             sliderInput("Adaycompp", "Days after event", min = 0, max = 20, value = 10))),
                    checkboxGroupInput("sellines", "Select the time series", choices = c("Mean by day", "Rolling Mean", "Amount Patients"), inline = TRUE, selected =  c("Mean by day", "Rolling Mean", "Amount Patients")),
                    tags$br(),
                    radioButtons("devType", "Select deviation type", choices = c("Standard Error", "Standard Deviation")),
                    plotOutput("MeanComPlot")),
                    tags$br(),
                    tags$br(),
                    tags$h2("Patient event plot"),
                    "In the following plot we present multiple time series in which an event occured. The black line indicates one the selected events. The coloured lines depicts all scaled time point variables. Press next to view the next event. The slider input allows to select how many days before and after the event should included in the plot (if available). The checkboxes allows you to select the time series of the variables that should be presented.",
                    tags$br(),
                    tags$br(),
                    column(4, offset = 1,
                    actionButton("nextcompplot", "Show next event")),
                    column(4,
                    actionButton("prevcompplot", "Show previous event")))),
                    tags$br(),
                    fluidRow(
                      column(4, offset = 1,
                    sliderInput("Bdaycomp", "Days before event", min = 10, max = 100, value = 50)),
                    column(4, 
                    sliderInput("Adaycomp", "Days after event", min = 0, max = 20, value = 10))),
                    fluidRow(
                    actionLink("selectall","Select All"),
                    column(10,
                    if(exists("TPVariables"))checkboxGroupInput("TPselected", choices = TPVariables, inline = TRUE, label = "please select the TP variables", selected = TPVariables),
                    plotOutput("compplot"))),
                    
                    fluidRow(
                  tags$br(),
                  tags$h2("Correlation matrix"),
                  "In data mining our goal is to find predictive patterns, these patterns could be very complex and not understable but it may also be a simple correlation between a certain value. To visualize this, we could create a correlation matrix. Bear in mind that the following matrix is build from a matrix which consists out of mean values for each variable per patient and thus doesn't take any temporal aspect into account. Press the button to create the correlation matrix.",
                  tags$br(),
                  column(10, align = "center",
                  actionButton("Ccorrplot", "Create correlation matrix")),tags$br(),
                  
                  conditionalPanel(condition = "input.Ccorrplot > 0",
                  plotOutput("corrplot", height = 900))),
                  
                  tags$br(),
                  tags$br()
          
                  
           ))
           ),
  navbarMenu("Data Preparation",
             
          tabPanel("Data selection",
                   column(10, offset = 1,
                           tags$h1("Data Selection"),
                           tags$hr(),
                          tags$h2("class label selection"),
                          "We make a distinction between two types predictions, baseline predictions and time serie predictions. Baseline prediction relate to the task of classifying patients that will be targeted by an event anywhere in the future whereas with time series prediction we try to predict at certain time point if a patient will be targeted by event within the near future.",
                          radioButtons("classSel", "select type of prediction", choices = c("Baseline Prediction", "Time serie prediction"), selected = "Time serie prediction" , inline = TRUE),
                          conditionalPanel(condition = "input.classSel == 'Baseline Prediction' ",
                                           "Our goal with a baseline prediction is to identify the patients that are more likely to be targeted by an event. Because we have sequential data, it could be usefull to include a sample of the first sequential data. The sample size is specified by setting the maximum number of days after the intervention that should be used to create features for each patient.",
                                           numericInput("basemax", "please specify max amount days from intervention to include", value = 60, min = 0, max = 100)),
                          
                          conditionalPanel(condition = "input.classSel == 'Time serie prediction' ",
                                           fluidRow(
                                             column(4,
                                             numericInput("classmin", "please specify the minimun days of your classlabel window", value = 1, min = 0, max = 30)),
                                             column(4,
                                             numericInput("classmax", "please specify the maximum days of your classlabel window", value = 7, min = 0, max= 30))),
                                           
                                           numericInput("delvec", "please specify the amount of days after the event that should be removed from the dataset", value = 7, min = 0, max= 30),
                          tags$h2(textOutput("TIcheck")),
                          htmlOutput("checkTI"),
                          conditionalPanel(condition = "output.TIcheck == 'Dataset subsetting'",
                                           plotOutput("plot"),
                                           textOutput("TIcheck2"),
                                           tags$br(),
                                           textOutput("TIReg"),
                                           tags$br(),
                                           conditionalPanel(condition = "output.TIcheck2 == '-'",
                                            actionButton("cleanTI", "Clean TI Dataset"),
                                            tags$br(),
                                            conditionalPanel(condition = "input.cleanTI > 0",
                                           plotOutput("medplott"))),
                                           tags$br(),
                                           dateInput("cutTI", "Specify cutt of point", value = NULL))),
                  
                  tags$h2("Rolling Window Configuration"),
                  "We will use a rolling window method to create the fundament of our dataset. Each object will be a specific timepoint, that contains multiple temporal features that summarizes the history of that patient. The class label is specified days from first step. For example:
                  if the class label window is specified from 1 to 7, this means that we will create a dataset in which an object represents a specific time point of patient with a corresponding class label stating wether that patient on that time point will have an event within the coming 7 days. 
                  Because EHR data is irregularly sampled, a simple rolling window where each object represent for example a day, wouldn't make sense because the majority of the dataset would be missing. Therefore, each objects represent a time point .
                ",
                  tags$br(),
                  "First we need to specify which timepoints should be included in the model. This is a trade off, because when variable x is measured this doesn't mean variable y is specified. Including less measured time points means that this data needs to be imputed. More data that needs to be imputed increases the uncertainty and could potentially create more noise in the data. Therefore we suggest to select only most frequent variables",
                  tags$br(),
                  tags$br(),
                  "Please select the time point variables that you would like to include in the model",
                  DT:: dataTableOutput("DPTPsel"),
                  "The time interval data will be used to create a binary vector for each object (time point) stating whether a it falls within a certain time interval or not. Please select the time interval variables that you would like to include in the dataset",
                  selectInput("classBaseWindow", "Select Time point variable for structuring rolling window", choices = ""),
                  
                  DT:: dataTableOutput("DPTIsel"),
                  tags$h2("Combining Time Interval features"),
                  "Are there any Time interval variables that you would like to combine into one feature. Combining multiple variables results in a binary feature thats is positive in case a patient falls within a one or multiple time intervals of the specified variables.",
                  DT:: dataTableOutput("DPTIsell"),
                  checkboxInput("newTICombo", "Create another feature by combining time interval variables", value = FALSE),
                  conditionalPanel(condition = "input.newTICombo == true",
                  DT:: dataTableOutput("DPTIselll"),                 
                  checkboxInput("newTIComboo", "Create another feature by combining time interval variables", value = FALSE)),
                  conditionalPanel(condition = "input.newTIComboo == true",
                  DT:: dataTableOutput("DPTIsellll")) 

                   
                            
                      )),
          tabPanel("Data engineering",
                   column(10, offset = 1,
                   tags$h1("Data Engineering"),
                   "In this phase we a goal is to integrate and format the data in way that enables it to be handled by classification algorithms. Different design choices must be made that affects the predictive performance of the classifier. These design choices are related to the following data preparation tasks:",
                   tags$br(),tags$ul(
                     tags$li( "Data Imputation"),
                     tags$li( "Feature Extraction"),
                   tags$hr(),
                   tags$h2("Data Imputation"),
                   radioButtons("dataimp", "Select imputation method", choices = c("Mean", "Amelia", "MissForest", "MICE"), inline = TRUE),
                   textOutput("dataimpu"),
                   tags$h2("Feature Extraction"),
                   radioButtons("featex", "Select feature extraction method", choices = c("Last Values", "Statistics", "Summary", "Trend Value Abstraction", "Frequent Pattern Mining"), inline = TRUE),
                   textOutput("featexx"),
                   actionButton("dataprep", "Create dataset and impute missing data"),
                   verbatimTextOutput("prepareDat"))
                   
          ))),
          tabPanel("Modeling & Evaluation",
                   column(10, offset =1,
                          tags$h1("Modeling"),
                        tags$hr(),
                          tags$h2("Feature Selection"),
                          radioButtons("featsel", "Select feature selection method", choices = c("None","Boruta", "Recursive Feature Elimination", "Filter", "Rank by Importance (RandomForest)"), inline = TRUE),
                          textOutput("featsell"),
                          tags$br(),
                          tags$h2("Data Sampling"),
                          radioButtons("datasam", "Select data sampling method", choices = c("None","Over", "Under", "Rose"), inline = TRUE),
                          textOutput("datasamm"),
                          tags$hr(),
                          tags$h2("Select Algorithm"),
                          radioButtons("algos", "Select classification algorithm", choices = c("RandomForest", "NaiveBayes", "C5.0",  "Support Vector Machine"  , "Logistic Regression", "Decision Tree")),
                          tags$br(),
                          numericInput("seed", "Set Seed", value = 123),
                          tags$br(),
                          fluidRow(actionButton("cv", "Perform Cross-Validation"),
                                   actionButton("finMod", "Build my final model!")
                          ),
                          conditionalPanel(condition = "(input.featsel == 'Boruta' || input.featsel == 'Recursive Feature Elimination') && input.cv > 0",
                                           plotOutput("FSelPlot",height = 600)
                          ),
                        tags$br(),
                          conditionalPanel(condition = "(input.cv >0 || input.finMod >0)  && (input.featsel == 'Boruta' || input.featsel == 'Recursive Feature Elimination' || input.featsel == 'Filter' || input.featsel == 'Rank by Importance (RandomForest)') ", 
                                        tableOutput("varIMP")),
                        conditionalPanel(condition = "input.cv > 0",
                                        verbatimTextOutput("rocinfo"),
                                        plotOutput("roccurve"),
                                        sliderInput("thresh", "Threshold", 0, 1, 0.5),
                                        verbatimTextOutput("rocinfo1")
                                        ) ,                                 
                        conditionalPanel(condition = "input.finMod > 0",   
                                        verbatimTextOutput("compfin"),
                        conditionalPanel(condition = "input.algos == 'Logistic Regression'",
                                        verbatimTextOutput("raw_summary")),
                        conditionalPanel(condition = "input.algos == 'Decision Tree'",
                                         plotOutput("Tree", height = 700))
                        ),
                          tags$hr(),
                          tags$h2("Evaluate final model on new dataset"),
                          fluidRow(column(6,
                                          fileInput("TPDN", "Upload Time Point Data",
                                                    multiple = TRUE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          tags$hr(),
                                          
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("headerTPN", "Header", TRUE),
                                          
                                          # Input: Select separator ----
                                          radioButtons("sepTPN", "Separator", inline = TRUE,
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ";"),
                                          actionButton("loadTPN", "Load time point data"),
                                          radioButtons("newcolTPN", "", choices = c("Use previously specified column", "Specify new colums")),
                                          conditionalPanel(condition = "input.newcolTPN == 'Specify new colums' ",
                                          tags$hr(),
                                          selectInput("ITPN_ID", "Select ID Column", c("None")),
                                          selectInput("ITPN_Date", "Select Date of Timepoint Column", c("None")),
                                          selectInput("ITPN_Time", "Select Time of Timepoint Column (optional)", c("None")),
                                          selectInput("ITPN_Type", "Select Type Column", c("None")),
                                          selectInput("ITPN_Outcome", "Select Outcome Column", c("None")))
                          )
                          
                          
                          # Input: Select quotes ----
                          
                          ,
                          column(6,
                                 fileInput("TIDN", "Upload Time Interval Data",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 tags$hr(),
                                 
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("headerTIN", "Header", TRUE),
                                 
                                 # Input: Select separator ----
                                 radioButtons("sepTIN", "Separator",inline = TRUE,
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ";"),
                                 actionButton("loadTIN", "Load time interval data"),
                                 radioButtons("newcolTI", "", choices = c("Use previously specified column", "Specify new colums")),
                                 conditionalPanel(condition = "input.newcolTI == 'Specify new colums' ",
                                 tags$hr(),
                                 
                                 
                                 column(10,
                                        selectInput("ITIN_ID", "Select ID Column", c("None")),
                                        selectInput("ITIN_SD", "Select Start Date Column", c("None")),
                                        selectInput("ITIN_ED", "Select End Date Column", c("None")),
                                        selectInput("ITIN_EDD", "Select End Date 2 Column (optional)", c("None")),
                                        selectInput("ITIN_Type1", "Select Type 1 Column", c("None")),
                                        selectInput("ITIN_Type2", "Select Type 2 Column (optional)", c("None"))))
                                 
                          )
                          ),
                          tags$hr(),
                          fluidRow(column(6,
                                          fileInput("COMPDN", "Upload event data",
                                                    multiple = TRUE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          tags$hr(),
                                          
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("headerCompN", "Header", TRUE),
                                          
                                          # Input: Select separator ----
                                          radioButtons("sepCompN", "Separator",inline = TRUE,
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ";"),
                                          actionButton("loadcompN", "Load event data"),
                                          radioButtons("newcolcomp", "", choices = c("Use previously specified column", "Specify new colums")),
                                          conditionalPanel(condition = "input.newcolcomp == 'Specify new colums' ",
                                          tags$hr(),
                                          selectInput("ICOMP_IDN", "Select ID Column", c("None")),
                                          selectInput("ICOMP_DateN", "Select Date Column", c("None")),
                                          selectInput("ICOMP_TypeN", "Select Type Column", c("None")),
                                          selectInput("ICOMP_NDaysN", "Select intervention days Column", c("None")))
                                          
                          )
                          ,
                          column(6,
                                 fileInput("BASEDN", "Upload Baseline Data",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 tags$hr(),
                                 
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("headerBaseN", "Header", TRUE),
                                 
                                 # Input: Select separator ----
                                 radioButtons("sepBaseN", "Separator",inline = TRUE,
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ";"),
                                 actionButton("loadBaseN", "Load baseline data"),
                                 radioButtons("newcolBase", "", choices = c("Use previously specified column", "Specify new colums")),
                                 conditionalPanel(condition = "input.newcolBase == 'Specify new colums' ",
                                 tags$hr(),
                                 
                                 # Input: Select quotes ----
                                 selectInput("IBASE_IDN", "Select ID Column", c("None")),
                                 selectInput("IBASE_StartN", "Select Start Column", c("None")),
                                 selectInput("IBASE_EndN", "Select End Column", c("None")),
                                 selectInput("IBASE_AgeN", "Select Age Column", c("None")),
                                 selectInput("IBASE_GenderN", "Select Gender Column", c("None"))))
                          
                          )),
                   tags$br(),
                   tags$hr(),
                   column(10, align = "center",
                   actionButton("test", "Test the final model on loaded data"))
                   
                   
                   ),
  
          tabPanel("Deployment",
                   column(10, offset = 1,
                          tags$h1("Deployment"),
                          tags$hr(),
                          "Now that you have created and tested your final model, you can deploy your model. It's required to input the same datasets as initially loaded. The model will be deployed; based on your set class label, the model will provide prediction each patient that is within the dataset for latest timepoint found. In case your class label is set to prediction of event 1-7 days, it will provide a prediction for the following 1-7 days relative to the last inputted data.",
                          fluidRow(column(6,
                                          fileInput("TPDD", "Upload Time Point Data",
                                                    multiple = TRUE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          tags$hr(),
                                          
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("headerTPD", "Header", TRUE),
                                          
                                          # Input: Select separator ----
                                          radioButtons("sepTPD", "Separator", inline = TRUE,
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ";"),
                                          actionButton("loadTPD", "Load time point data"),
                                          radioButtons("newcolTPD", "", choices = c("Use previously specified column", "Specify new colums")),
                                          conditionalPanel(condition = "input.newcolTPD == 'Specify new colums' ",
                                                           tags$hr(),
                                                           selectInput("ITPD_ID", "Select ID Column", c("None")),
                                                           selectInput("ITPD_Date", "Select Date of Timepoint Column", c("None")),
                                                           selectInput("ITPD_Time", "Select Time of Timepoint Column (optional)", c("None")),
                                                           selectInput("ITPD_Type", "Select Type Column", c("None")),
                                                           selectInput("ITPD_Outcome", "Select Outcome Column", c("None")))
                          )
                          
                          
                          # Input: Select quotes ----
                          
                          ,
                          column(6,
                                 fileInput("TIDD", "Upload Time Interval Data",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 tags$hr(),
                                 
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("headerTID", "Header", TRUE),
                                 
                                 # Input: Select separator ----
                                 radioButtons("sepTID", "Separator",inline = TRUE,
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ";"),
                                 actionButton("loadTID", "Load time interval data"),
                                 radioButtons("newcolTID", "", choices = c("Use previously specified column", "Specify new colums")),
                                 conditionalPanel(condition = "input.newcolTID == 'Specify new colums' ",
                                                  tags$hr(),
                                                  
                                                  
                                                  column(10,
                                                         selectInput("ITID_ID", "Select ID Column", c("None")),
                                                         selectInput("ITID_SD", "Select Start Date Column", c("None")),
                                                         selectInput("ITID_ED", "Select End Date Column", c("None")),
                                                         selectInput("ITID_EDD", "Select End Date 2 Column (optional)", c("None")),
                                                         selectInput("ITID_Type1", "Select Type 1 Column", c("None")),
                                                         selectInput("ITID_Type2", "Select Type 2 Column (optional)", c("None"))))
                                 
                          )
                          ),
                          tags$hr(),
                          fluidRow(

                          column(6, 
                                 fileInput("BASEDD", "Upload Baseline Data",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 tags$hr(),
                                 
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("headerBaseD", "Header", TRUE),
                                 
                                 # Input: Select separator ----
                                 radioButtons("sepBaseD", "Separator",inline = TRUE,
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ";"),
                                 actionButton("loadBaseD", "Load baseline data"),
                                 radioButtons("newcolBaseD", "", choices = c("Use previously specified column", "Specify new colums")),
                                 conditionalPanel(condition = "input.newcolBaseD == 'Specify new colums' ",
                                                  tags$hr(),
                                                  
                                                  # Input: Select quotes ----
                                                  selectInput("IBASE_IDD", "Select ID Column", c("None")),
                                                  selectInput("IBASE_StartD", "Select Start Column", c("None")),
                                                  selectInput("IBASE_EndD", "Select End Column", c("None")),
                                                  selectInput("IBASE_AgeD", "Select Age Column", c("None")),
                                                  selectInput("IBASE_GenderD", "Select Gender Column", c("None")))),
                          
                          column(6, 
                                 actionButton("deploydata", "Create predictions based on final model"))
                          
                          )
                        
                          )

                      )
           
)