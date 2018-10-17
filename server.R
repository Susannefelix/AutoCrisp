{library(shinydashboard)
library(reshape2)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(chron)
library(stringr)
library(corrplot)
library(survival)
library(survminer)
library(randomForest)
library(e1071) 
library(TSclust)
library(mice)
library(Amelia)
library(missForest)
library(Boruta)
library(pROC)
library(caret)
library(ROSE)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(unbalanced)
library(varSelRF)

}
{
  remove_missing_levels <- function(fit, test_data) {
    
    # https://stackoverflow.com/a/39495480/4185785
    
    # drop empty factor levels in test data
    test_data %>%
      droplevels() %>%
      as.data.frame() -> test_data
    
    # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
    # account for it
    if (any(class(fit) == "glmmPQL")) {
      # Obtain factor predictors in the model and their levels
      factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                       names(unlist(fit$contrasts))))
      # do nothing if no factors are present
      if (length(factors) == 0) {
        return(test_data)
      }
      
      map(fit$contrasts, function(x) names(unmatrix(x))) %>%
        unlist() -> factor_levels
      factor_levels %>% str_split(":", simplify = TRUE) %>%
        extract(, 1) -> factor_levels
      
      model_factors <- as.data.frame(cbind(factors, factor_levels))
    } else {
      # Obtain factor predictors in the model and their levels
      factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                       names(unlist(fit$xlevels))))
      # do nothing if no factors are present
      if (length(factors) == 0) {
        return(test_data)
      }
      
      factor_levels <- unname(unlist(fit$xlevels))
      model_factors <- as.data.frame(cbind(factors, factor_levels))
    }
    
    # Select column names in test data that are factor predictors in
    # trained model
    
    predictors <- names(test_data[names(test_data) %in% factors])
    
    # For each factor predictor in your data, if the level is not in the model,
    # set the value to NA
    
    for (i in 1:length(predictors)) {
      found <- test_data[, predictors[i]] %in% model_factors[
        model_factors$factors == predictors[i], ]$factor_levels
      if (any(!found)) {
        # track which variable
        var <- predictors[i]
        # set to NA
        test_data[!found, predictors[i]] <- NA
        # drop empty factor levels in test data
        test_data %>%
          droplevels() -> test_data
        # issue warning to console
        message(sprintf(paste0("Setting missing levels in '%s', only present",
                               " in test data but missing in train data,",
                               " to 'NA'."),
                        var))
      }
    }
    return(test_data)
  }
} #extra functions


{ExVec <- 1
TPname <<- "Time Point data"
TIname <<- "Time Interval data"
Basename <<- "Baseline data"
Compname <<- "Complication data"
counter <<- 0
TPsets <<- 0
TIsets <<- 0
Basesets <<- 0
Compsets <<- 0
Nevents <<- 0
windowsize <<- 10
Neventpatients <<- 0
Npatients <<- 0
Ntimepoints <<- 0
PercentageCom <<- as.integer((Neventpatients/Npatients)*100)
AverTime <<- as.integer(Ntimepoints/Npatients)
NvarTP <<- 0
Ninterval <<- 0
NvarTI <<- 0
eventyear <<- data.frame(vector = numeric(), Freq = numeric())
IDS <<- 0
namecol <<- vector()
showrep <<- "no"
if (exists("comp_data") == FALSE)
{types<- c("", "")
typeslength <<- 0
compcol1 <<- 0}
if (exists("TPtable") == FALSE)
{ TPtable <<- data.frame()
}


TPDDate <<- 0
TIDDate <<- 0
compDDate <<- 0
baseDDate <<- 0
TP_NAID <- vector()      
TI_NAID <- vector()
Comp_NAID <- vector()
Base_NAID <- vector()
i <- 1 
Sys.setlocale("LC_TIME","English")

if (exists("TP_data1") == FALSE)
{ TPVariables <<- c("")}


  
if (exists("comp_data") == TRUE)
{ if (exists("Comp_type"))
  {types <<- unique(comp_data[,Comp_type])
  typeslength <<- length(types)
  compcol1 <<- typeslength-as.integer(typeslength/2)}
}
} #Basic Settings


function(input, output, session) {

  options(shiny.maxRequestSize=400*1024^2)
  
  output$summary <- renderPrint({
    summary(cars)
  })

  
  observeEvent(input$clear, {
    rm(comp_data, pos = ".GlobalEnv")
    rm(TP_data1,  pos = ".GlobalEnv")
    rm(TI_data1,  pos = ".GlobalEnv")
    rm(base_data,  pos = ".GlobalEnv")
    counter <<- 0
    TPsets <<- 0
    TIsets <<- 0
    Basesets <<- 0
    Compsets <<- 0
    Nevents <<- 0
    Neventpatients <<- 0
    Npatients <<- 0
    Ntimepoints <<- 0
    PercentageCom <<- as.integer((Neventpatients/Npatients)*100)
    AverTime <<- as.integer(Ntimepoints/Npatients)
    NvarTP <<- 0
    Ninterval <<- 0
    NvarTI <<- 0
    eventyear <<- data.frame(vector = numeric(), Freq = numeric())
    IDS <<- 0
    types <<- c("", "")
    typeslength <<- 0
    session$reload();
  })


  observeEvent(input$loadTP, {
      TPname <<- input$nameTP
    if(is.null(input$TPD) == FALSE)
{    TP_data1 <<- read.csv(input$TPD$datapath,
                 header = input$headerTP,
                 sep = input$sepTP)
    namecolTP <<- names(TP_data1)
    updateSelectInput(session, "ITP_ID",
                      label = paste("Select ID column"),
                      choices = namecolTP,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITP_Date",
                      label = paste("Select Date of Timepoint Column"),
                      choices = namecolTP,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITP_Time",
                      label = paste("Select Time of Timepoint Column (optional)"),
                      choices = namecolTP,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITP_Type",
                      label = paste("Select Type column"),
                      choices = namecolTP,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITP_Outcome",
                      label = paste("Select Outcome column"),
                      choices = namecolTP,
                      selected = tail(1, 1))

    TPDDate <<- 1
    TPsets <<- 1}
    
    })
  
  observeEvent(input$loadTI, {
    TIname <<- input$nameTI
    if(is.null(input$TID) == FALSE)
     {TI_data1 <<- read.csv(input$TID$datapath,
                          header = input$headerTI,
                          sep = input$sepTI)
    namecolTI <<- names(TI_data1)
    updateSelectInput(session, "ITI_ID",
                      label = paste("Select ID column"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITI_SD",
                      label = paste("Select Start date column"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITI_ED",
                      label = paste("Select End date column"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITI_EDD",
                      label = paste("Select End Date 2 Column (optional)"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITI_Type1",
                      label = paste("Select Type1 column"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    updateSelectInput(session, "ITI_Type2",
                      label = paste("Select Type2 column (optional)"),
                      choices = namecolTI,
                      selected = tail(1, 1))
    TIDDate <<- 1
    TIsets <<- 1}
  })

  observeEvent(input$loadcomp, {
    Compname <<- input$nameComp
    if(is.null(input$COMPD) == FALSE)
    {
      comp_data <<- read.csv(input$COMPD$datapath,
                          header = input$headerComp,
                          sep = input$sepComp)
 
    namecolComp <<- names(comp_data)
    
    updateSelectInput(session, "ICOMP_ID",
                      label = paste("Select ID column"),
                      choices = namecolComp,
                      selected = tail(1, 1))
    updateSelectInput(session, "ICOMP_Date",
                      label = paste("Select Date Column"),
                      choices = namecolComp,
                      selected = tail(1, 1))
    updateSelectInput(session, "ICOMP_NDays",
                      label = paste("Select intervention days Column"),
                      choices = namecolComp,
                      selected = tail(1, 1))
    updateSelectInput(session, "ICOMP_Type",
                      label = paste("Select Type Column"),
                      choices = namecolComp,
                      selected = tail(1, 1))
    compDDate <<- 1
    Compsets <<- 1
    }
  })
  
  observeEvent(input$loadBase, {
    Basename <<- input$nameBase
    if(is.null(input$BASED) == FALSE)
    {base_data <<- read.csv(input$BASED$datapath,
                          header = input$headerBase,
                          sep = input$sepBase)
    namecolBase <<- names(base_data)
    updateSelectInput(session, "IBASE_ID",
                      label = paste("Select ID column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    updateSelectInput(session, "IBASE_Start",
                      label = paste("Select Start Column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    updateSelectInput(session, "IBASE_End",
                      label = paste("Select End Column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    updateSelectInput(session, "IBASE_Age",
                      label = paste("Select Age Column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    updateSelectInput(session, "IBASE_Gender",
                      label = paste("Select Gender Column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    updateSelectInput(session, "IBASE_Mort",
                      label = paste("Select Mortality date Column"),
                      choices = namecolBase,
                      selected = tail(1, 1))
    baseDDate <<- 1
    Basesets <<- 1}
  })
  
  observeEvent(input$run, {
    withProgress(message = 'Preparing datasets!', value = 0, {
    if (exists("TP_data1") == TRUE)
    {if (TPDDate == 1)
    {TP_ID <<- which(names(TP_data1) == input$ITP_ID)
    TP_date <<-  which(names(TP_data1)== input$ITP_Date)
    TP_Type <<- which(names(TP_data1)== input$ITP_Type)
    TP_Outcome <<- which(names(TP_data1)== input$ITP_Outcome)
    #TP_time <<- which(names(TP_data1)== input$ITP_Time)
    TP_data1[,TP_Type] <<-str_replace_all(TP_data1[,TP_Type], "[^[:alnum:]]", "_")
        if (class(TP_data1[,input$ITP_Date]) != "Date")
        { if (!is.na(as.Date(TP_data1[1,TP_date], "%d%B%Y")))
          TP_data1[,TP_date] <<- as.Date(TP_data1[,TP_date], "%d%B%Y")
      else
        TP_data1[,TP_date] <<- as.Date(TP_data1[,TP_date])}

    TP_data1 <<- TP_data1[TP_data1[,TP_Outcome] != "-volgt-" ,]
    TP_data1 <<- TP_data1[TP_data1[,TP_Outcome] != "NEG" ,]
    TP_data1 <<- TP_data1[TP_data1[,TP_Outcome] != "POS" ,]
    TP_data1[,TP_Outcome] <<- gsub("<", "", TP_data1[,TP_Outcome])
    TP_data1[,TP_Outcome] <<- gsub(">", "", TP_data1[,TP_Outcome])
    TP_data1[,TP_Outcome] <<- as.numeric(as.character(TP_data1[,TP_Outcome]))
    
    TP_data1 <<- TP_data1[!is.na(TP_data1[,TP_Outcome]),]}
      TP_Nvariables <<- as.data.frame(table(TP_data1[,TP_Type]))
      TP_Nvariables <<- TP_Nvariables[order(-TP_Nvariables$Freq),]
      TPtable <<- TP_Nvariables[1:100,]
      NVar <- length(unique(TP_Nvariables$Var1))
      if (NVar > 20) {NVar <- 20}
      TPVariables <<- TP_Nvariables[1:NVar,1]
      
      Npatients <<- length(unique(TP_data1[,TP_ID]))
      Ntimepoints <<- nrow(TP_data1)
      NvarTP <<- length(unique(TP_data1[,TP_Type]))
      
      incProgress((5/100), detail = paste("Preparing Environment", "5% done"))
    
      patientID <- unique(TP_data1[,TP_ID])
      vec <- vector()
      for (i in 1:length(patientID))
      {subdf <- TP_data1[TP_data1[,TP_ID] == patientID[i],]
      subdf <- subdf[subdf[,TP_Type] == TPVariables[1], TP_date]
      subdf <- subdf[order(subdf)]
      subdf <- subdf[!duplicated(subdf)]
        for (j in 1:(length(subdf))-1)
        { x <- subdf[j+1]-subdf[j]
          vec <- c(vec, x)
        }
      }
      meanTP <<- mean(vec)
    }

    if (exists("comp_data") == TRUE)
    {   if(compDDate == 1)
     {Comp_ID <<- which(names(comp_data) == input$ICOMP_ID)
      Comp_date <<- which(names(comp_data) == input$ICOMP_Date)
      Comp_type <<- which(names(comp_data) ==input$ICOMP_Type)
      Comp_NDays <<- which(names(comp_data) ==input$ICOMP_NDays)
    if (class(comp_data[,Comp_date]) != "Date")
    {if (!is.na (as.Date(comp_data[1,Comp_date], "%d-%m-%Y")))
      {comp_data[,Comp_date] <<-as.Date(comp_data[,Comp_date], "%d-%m-%Y")
    vector <- format(as.Date(comp_data[,Comp_date], format="%d-%m-%Y"),"%Y")}
    
    else
      {comp_data[,Comp_date] <<-as.Date(comp_data[,Comp_date], "%Y-%m-%d")
      vector <- format(as.Date(comp_data[,Comp_date], format="%Y-%m-%d"),"%Y")
      }
    }}
    Nevents <<- nrow(comp_data)
    Neventpatients <<- length(unique(comp_data[,Comp_ID]))
    #vector <- format(as.Date(comp_data[,Comp_date], format="%d-%m-%Y"),"%Y")
    
    eventyear <<- as.data.frame(table(vector))
      types <<- unique(comp_data[,Comp_type])
      typeslength <<- length(types)
      compcol1 <<- typeslength-as.integer(typeslength/2)
      incProgress((5/100), detail = paste("Preparing Environment", "10% done"))
      
      }
    
    if (exists("base_data") == TRUE)
    { if( baseDDate == 1)
    {  
    Base_ID <<- which(names(base_data)== input$IBASE_ID)
    Base_Start <<- which(names(base_data)== input$IBASE_Start)
    Base_End <<- which(names(base_data)== input$IBASE_End)
    Base_Age <<- which(names(base_data)== input$IBASE_Age)
    Base_Gender <<- which(names(base_data)== input$IBASE_Gender)
    Base_Mort <<- which(names(base_data)== input$IBASE_Mort)
    if (!is.na(as.Date(base_data[1,Base_Start], "%d-%m-%Y")))
    {base_data[,Base_Start] <<- as.Date(base_data[,Base_Start], "%d-%m-%Y")
    base_data[,Base_End] <<- as.Date(base_data[,Base_End], "%d-%m-%Y")}
    else
    {base_data[,Base_Start] <<- as.Date(base_data[,Base_Start], "%Y-%m-%d")
    base_data[,Base_End] <<- as.Date(base_data[,Base_End], "%Y-%m-%d")} 
    base_data$stat <<-0
    base_data[is.na(base_data[,Base_Mort]),ncol(base_data)] <<- 1
    Base_stat <<- ncol(base_data)
    base_data$stat1 <<-1
  #  base_data[is.na(base_data[,Base_Mort]),ncol(base_data)] <<- 0
   # base_data[(base_data[,Base_Mort]== "") & base_data[,Base_End]< as.Date("1-1-2018" ,"%d-%m-%Y"),ncol(base_data)] <<- 0
    Base_stat1 <<- ncol(base_data)
    base_data$NdayPat <<- base_data[,Base_End]-base_data[,Base_Start]
    Base_nday <<- ncol(base_data)
    base_data <<- base_data[,c(Base_ID, Base_Start, Base_End, Base_Age, Base_Gender, Base_stat, Base_nday, Base_stat1)]
    base_data <<- base_data[!duplicated(base_data),]
    Base_ID <<- 1
    Base_Start <<- 2
    Base_End <<- 3
    Base_Age <<- 4
    Base_Gender <<- 5
    Base_stat <<- 6
    Base_nday <<- 7
    Base_stat1 <<- 8
    base_data[,Base_Age] <<- as.numeric(gsub(",", ".", base_data[,Base_Age]))
    doubleID <<- which(duplicated(base_data[,Base_ID])== TRUE)
    IDS <<- base_data[,Base_ID]


    incProgress((5/100), detail = paste("Preparing Environment", "15% done"))
    }}
    
    if (exists("TI_data1") == TRUE)
    { if (TIDDate == 1)
    {
      TI_ID <<- which(names(TI_data1)== input$ITI_ID)
      TI_start <<- which(names(TI_data1)== input$ITI_SD) 
      TI_end <<- which(names(TI_data1)== input$ITI_ED)
      TI_Type1 <<- which(names(TI_data1)== input$ITI_Type1)
      TI_Type2 <<- which(names(TI_data1)== input$ITI_Type2)
      TI_endd <<-which(names(TI_data1)== input$ITI_EDD)
      if (class(TI_data1[,input$ITI_SD]) != "Date")
        #{if (!is.na(as.Date(TI_data1[1,TI_start], "%d%B%Y")))
        TI_data1[,TI_start] <<- as.Date(TI_data1[,TI_start], "%d%B%Y")
      #else
       # TI_data1[,TI_start] <<- as.Date(TI_data1[,TI_start],  "%Y-%m-%d")}
      if (class(TI_data1[,input$ITI_ED]) != "Date")
    #  {if (!is.na(as.Date(TI_data1[1,TI_end], "%d%B%Y")))
        TI_data1[,TI_end] <<- as.Date(TI_data1[,TI_end], "%d%B%Y")
     # else
      #  TI_data1[,TI_start] <<- as.Date(TI_data1[,TI_end],  "%Y-%m-%d")}
      }
      TI_data1 <<-TI_data1[complete.cases(TI_data1[,TI_start]),]
      
      
    #  if no end-date, new end-date
      if (exists("TI_endd"))
      {if (!is.na(as.Date(TI_data1[1,TI_endd], "%d-%m-%Y")))
      {TI_data1[,TI_endd] <<- as.Date(TI_data1[,TI_endd], "%d-%m-%Y")}
      else
      {TI_data1[,TI_endd] <<- as.Date(TI_data1[,TI_endd], "%Y-%m-%d")}
      
      for (i in 1:nrow(TI_data1))
      { if(is.na(TI_data1[i,TI_end]) == TRUE)
      {TI_data1[i,TI_end] <<- TI_data1[TI_data1[,TI_ID] == TI_data1[i,TI_ID],TI_endd][1]
      }}}
      TI_data1 <<- TI_data1[TI_data1[,TI_end] < Sys.Date(),]
      TI_data1$ID <<- seq(1:nrow(TI_data1))
      
      TItable <<- as.data.frame(table(TI_data1[,TI_Type1], TI_data1[,TI_Type2]))
      TItable <<- TItable[order(-TItable$Freq),]
      TItable <<- TItable[1:50,]
      incProgress((5/100), detail = paste("Preparing Environment", "20% done"))
      
    
      Ninterval <<- nrow(TI_data1)
      NvarTI <<- length(unique(TI_data1[,TI_Type1]))}

    Nsets <<- c("TP_data1", "TI_data1", "comp_data", "base_data")
    counter <<- 0
    for (i in 1:length(Nsets))
    {if (exists(Nsets[i]))
      counter <<- counter+1}
    PercentageCom <<- as.integer((Neventpatients/Npatients)*100)
    AverTime <<- as.integer(Ntimepoints/Npatients)
    compDDate <<-0
    baseDDate <<- 0
    TPDDate <<- 0
    TIDDate <<- 0
    MisdfNames <<- "All patients"
    incProgress((10/100), detail = paste("Preparing Environment", "30% done"))
    
     IDS <<- vector()
      if (exists("TP_data1"))
      {TP_unID <<- unique(TP_data1[,TP_ID])
      IDS <<- c(IDS, TP_unID)
      MisdfNames <<- c(MisdfNames,paste("Patients in", TPname))}
      if (exists("TI_data1"))  
      {TI_unID <<- unique(TI_data1[,TI_ID])
      IDS <<- c(IDS, TI_unID)
      MisdfNames <<- c(MisdfNames,paste("Patients in", TIname))}
      if(exists("base_data"))
      {Base_unID <<- unique(base_data[,Base_ID])
      IDS <<- c(IDS, Base_unID)
      MisdfNames <<- c(MisdfNames,paste("Patients in", Basename))}
     if(exists("comp_data"))
     {Comp_unID <<- unique(comp_data[,Comp_ID])
     IDS <<- c(IDS, Comp_unID)
     MisdfNames <<- c(MisdfNames,paste("Patients in", Compname))}
      IDS <<- unique(IDS)
      
      MisDF <<- data.frame()
      
      incProgress((5/100), detail = paste("Preparing Environment", "35% done"))
      
      for(i in 1:length(IDS))
      { TPX <- NA
      TIX <- NA
      COMPX <- NA
      BASEX <- NA
      misrow <- 1
      incProgress( 0.65/length(IDS), detail = paste("Preparing Environment", (0.65/length(IDS)*i*100)+65, "% Done"))
      if(exists("TP_data1"))
      { if(IDS[i] %in% TP_unID) 
        TPX <- 1
        misrow <- c(misrow, TPX)}
      if(exists("TI_data1"))
      { if(IDS[i] %in% TI_unID) 
        TIX <- 1
      misrow <- c(misrow, TIX)}
      if(exists("base_data"))
      {if(IDS[i] %in% Base_unID)
        BASEX <- 1
      misrow <- c(misrow, BASEX)}
      if(exists("comp_data"))
      {  if(IDS[i] %in% Comp_unID)
        COMPX <- 1
      misrow <- c(misrow, COMPX)}
      MisDF <<- rbind(MisDF, misrow)}
      names(MisDF)<<- MisdfNames})
       # All code for MISDF
    session$reload();
     })

  observe({
    if (typeslength > 1)
   {updateCheckboxGroupInput(session, "comptypes1",
                             label = "please select your class label",
                             choices = types[1:compcol1])
    updateCheckboxGroupInput(session, "comptypes2",
                             label = NULL,
                             choices = types[(compcol1+1):typeslength])}
    
  })
  
  observeEvent(input$DQRep,{
    showrep <<- "yes"
    if(exists("TP_data1"))
    {TP_missing <<- sum(is.na(MisDF$`Patients in TP`))}
    if(exists("TI_data1"))
    TI_missing <<- sum(is.na(MisDF$`Patients in TI`))
    if(exists("comp_data"))
    Comp_missing <<- sum(is.na(MisDF$`Patients in Comp`))
    if(exists("base_data"))
    Base_missing <<- sum(is.na(MisDF$`Patients in Base`))
  })

  observeEvent(input$preparecomp, {
    classlabels1 <<- input$comptypes1
    classlabels2 <<- input$comptypes2
    classlabels <<- c(classlabels1, classlabels2)
    comp_data <<- comp_data[comp_data[,Comp_type] %in% classlabels,]
    comp_data <<- comp_data[comp_data[,Comp_NDays] > 30,]
    Neventpatients <<- length(unique(comp_data[,Comp_ID]))
    vector <- format(as.Date(comp_data[,Comp_date], format="%d-%m-%Y"),"%Y")
    eventyear <<- as.data.frame(table(vector))
    Nevents <<- nrow(comp_data)
    PercentageCom <<- as.integer((Neventpatients/Npatients)*100)
    output$preparecompbut <- renderText({ "Ready!" })
    
  })
  
  output$showrepp <- renderText({showrep})
  
  output$datadesc <- renderText({
  paste("In total we are working with", counter, "datasets,", TPsets, "time points dataset(s),", TIsets, "time interval dataset(s)", Compsets, "event dataset(s) and ", Basesets, " baseline dataset(s)")
  })
   
  output$classdesc <- renderText({
    paste("Let's start with the", input$nameComp, ". In total we have ", Nevents, "medical events. These events occured at ", Neventpatients, "patients. In your datasets you have ", Npatients , "unique patients, which thus means that ", PercentageCom, "of the patient was targeted by that event.") 
  })
   
   output$tpdesc <- renderText({
    paste("Now let's take a look at your",input$nameTP, ", in total you have ", Ntimepoints, "datapoints. This means that the average amount of time points per patient is ", AverTime, ". Obviously this is not from one variable, in total we have ", NvarTP, "variables. The average time between the time points is", as.integer(meanTP), " days.") 
   })

  output$tidesc <- renderText({
   paste("Your", input$nameTI,  "has ", Ninterval,  "intervals. These intervals are distributed over ", NvarTI, "variables.") 
  })
  
  output$doubleBase <- renderText({if(exists("doubleID"))
  {paste(c("It seems that you have patients appearing multiple times in your in your", input$nameBase ,"please check if this is correct. I found in total", as.character(length(double)), "patient(s)."), sep = " ")
  }
  })
  
  output$doubleBase1 <- renderText({ if(length(doubleID)< 5) paste(c("The following patientID's appeared multiple times:", IDS[doubleID]), sep = ":" )
  }) 

  output$datasimu <- renderUI({
    str1 <-     paste("")
    strTP <- paste("")
    strTI <- paste("")
    strCOMP <- paste("")
    strBASE <- paste("")
    if(counter > 0)
    str1 <- paste("Before we can integrate the datasets into one dataset, we need check wether all patients are present in all datasets. In figure 1 we first plotted the ", Npatients, "patients in the first column and is followed by the other ", counter, "datasets. These patients are chronologically ordered (patient 1 is the first patients, patient ", Npatients, "the most recent) So we can conclude that")
    if(exists("TP_data1"))
    strTP <- paste(" the time point dataset has", TP_missing,  "missing patients" )
    if(exists("TI_data1"))
    strTI <- paste(", the time interval dataset has", TI_missing,  "missing patients" )
    if(exists("comp_data"))
    strCOMP <- paste(", the complication dataset has", Comp_missing,  "missing patients" )
    if(exists("base_data"))
    strBASE <- paste(", the baseline dataset has", Base_missing,  "missing patients" )
    HTML(paste(str1, strTP, strTI, strCOMP, strBASE, sep = ''))
  })
  
  output$TP_name <- renderText({TPname})
  output$TI_name <- renderText({TIname})
  output$COmp_name <- renderText({Compname})
  output$Base_name <- renderText({Basename})
  output$counterr <- renderText({counter})
  output$Neventss <- renderText({Nevents})
  output$Neventpatientss <- renderText({Neventpatients})
  output$Npatientss <- renderText({Npatients})
  output$PercentageComm <- renderText({PercentageCom})
  output$Ntimepointss <- renderText({Ntimepoints})
  output$AverTimee <- renderText({AverTime})
  output$NvarTPp <- renderText({NvarTP})
  output$Nintervall <- renderText({Ninterval})
  output$NvarTIi <- renderText({NvarTI})
  
  output$histoplot <- renderPlot({barplot(eventyear$Freq, names.arg = eventyear$vector)})
  
  output$missplot <- renderPlot({MisDF %>% is.na %>% melt %>% 
      ggplot(data = .,aes(x = Var2,y = Var1)) +
      geom_raster(aes(fill = value)) +
      scale_fill_grey(name = "", labels = c("Observed","Missing")) +
      theme_minimal() + 
      theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=10)) + 
      labs(x = "Datasets",y = "Patients")})
  
  
  output$labpatientplot <- renderPlot({
    if(exists("TP_data1"))
    {TP_Npatients <- as.data.frame(table(TP_data1[,TP_ID], TP_data1[,TP_Type]))
    TP_Npatients <- TP_Npatients[TP_Npatients$Freq > 0,]
    TP_Nvariables <- as.data.frame(table(TP_Npatients$Var2))
    TP_Nvariables <- TP_Nvariables[order(-TP_Nvariables$Freq),]
    TP_Nvariables$ID <- seq.int(nrow(TP_Nvariables))
    TP_Nvariables <- TP_Nvariables[order(TP_Nvariables$Var1),]
    ggplot(TP_Nvariables, aes(x= ID, y=Freq))+ ggtitle ("Amount Variables vs amount patients") + geom_line() + xlab("N Variables") + ylab("N Patients")
    }})
  
  output$Dplot <- renderPlotly({

    if (input$DDDplot == FALSE) return()
    if (input$demo == TRUE)
    {yyy <- seq(1:length(Demo_Patients))
    xxx <-  Demo_variables
    zzz <- as.matrix(Demo_TP3D)}
    else
    {
    TP_Npatients <- as.data.frame(table(TP_data1[,TP_ID], TP_data1[,TP_Type]))
    TP_Npatients <- TP_Npatients[TP_Npatients$Freq > 0,]
    TP_Nvariables <- as.data.frame(table(TP_Npatients$Var2))
    TP_Nvariables <- TP_Nvariables[order(-TP_Nvariables$Freq),]
    TP_Nvariables$ID <- seq.int(nrow(TP_Nvariables))
    TP_Nvariables <- TP_Nvariables[order(TP_Nvariables$Var1),]
    TP_Nvariables1 <-  as.data.frame(table(TP_data1[,TP_Type]))
    TP_Nvariables1 <- TP_Nvariables1[order(TP_Nvariables1$Var1),]
    TP_Nvariables$TPs <- TP_Nvariables1$Freq/TP_Nvariables$Freq
    TP_Nvariables1 <- TP_Nvariables1[order(-TP_Nvariables1$Freq),]
    variables <- as.character(TP_Nvariables1$Var1)
    if (nrow(TP_Nvariables1) > 150)
    {variables <- as.character(TP_Nvariables1$Var1[1:150])}
    for (i in 1:length(variables))
    {variables[i] <- paste(as.character(i), variables[i])}

    TP_patientTimepoints <- as.data.frame(table(TP_data1[,TP_ID]))
    TP_patientTimepoints <- TP_patientTimepoints[order(-TP_patientTimepoints$Freq),]
    patients <- TP_patientTimepoints[,1]
  
    TP3D <- data.frame()
    withProgress(message = 'Making plot', value = 0, {
    for(i in 1:length(patients))
    { patient_subset <- TP_data1[TP_data1[, TP_ID] == patients[i],]
    rowdata <- vector()
    incProgress(100/length(patients), detail = paste("Doing part", i))
    for(j in 1:length(variables))
    {x <- sum(patient_subset[,TP_Type] == variables[j])
    rowdata <- c(rowdata, x)}
    TP3D <- rbind(TP3D, rowdata)}
    yyy <- seq(1:length(patients))
    xxx <- variables
    zzz <- as.matrix(TP3D)})}
    plot_ly(x = xxx, y = yyy, z = zzz, colors = c("green", "blue"), type = "surface", width = 1200, height = 500) %>% layout(title = "3D Plot", scene = list( xaxis = list(title = "variables"), yaxis = list(title = "patients"), zaxis = list(title = "timepoints")))
    })
  
  output$TP_table <- renderDataTable({
  if(exists("TPtable"))datatable(TPtable,rownames= FALSE)
  })
  
  output$DPTPsel <- renderDataTable({
    if(exists("TPtable"))datatable(TPtable,rownames= FALSE, selection = list(target = 'row', selected = c(seq(1:1))) )
  })
  
  output$medtable <- renderDataTable({
    if(exists("TItable"))datatable(TItable, rownames = FALSE)
  })
  
  output$DPTIsel <- renderDataTable({
    if(exists("TItable"))datatable(TItable,rownames= FALSE, selection = list(target = 'row', selected = c(seq(1:5))) )
  }) 
  
  output$DPTIsell <- renderDataTable({
    if(exists("TItable")){datatable(TItable[input$DPTIsel_rows_selected,])
}  }) 
  
  output$DPTIselll <- renderDataTable({
    if(exists("TItable")){datatable(TItable[input$DPTIsel_rows_selected,])
    }  }) 
  
  output$DPTIsellll <- renderDataTable({
    if(exists("TItable")){datatable(TItable[input$DPTIsel_rows_selected,])
    }  }) 
  
  outVar = reactive({
    mydata = (input$DPTPsel_rows_selected)
    
  })
  
  observe({
  updateSelectInput(session, "classBaseWindow",
                    label = paste("Select Time point variable for structuring rolling window"),
                    choices = TPtable[outVar(),1],
                    selected = TPtable[5,1])})
  
  output$testrows <- renderText({input$medtable_rows_selected})

  output$medplot <- renderPlotly({
    if (input$Cmedplot == FALSE) return()
    
    if (input$demo == TRUE)
    {
      medresult <- Demo_medresult
      medtext <- Demo_medtext
      datess <- Demo_datess}
    
    else
    {
    selectedrows <<- input$medtable_rows_selected
    
    if (length (selectedrows) == 0) return()
    
    selectedmeds <<- TItable[selectedrows,1] 
    
    totstart <-as.numeric(min(base_data[,Base_Start]))-10
    totend <- as.numeric( max(base_data[,Base_End]))
    dates <- vector()
    
    medresult <- data.frame()
    medtext <- data.frame()
    
    medication <<- TI_data1[TI_data1[,TI_Type1] %in% selectedmeds,]
    ID <- unique(base_data[,Base_ID])
    

    withProgress(message = 'Making plot', value = 0, {
      n <- length(ID)
    for (i in 1:length(ID))
    { 
      selectmed <- medication[grep(ID[i], medication[,TI_ID]),]
     incProgress(1/length(ID), detail = paste("Doing part", i))
      
      # selectlab <- df5[grep(ID[i], df5$PseudoID_voorkeur),]
      #  selectlab <- selectlab[order(selectlab$AfnameDatum_LAB),]
      #  start <- selectlab[1,82]
      #start <- selectmed[1,2]
      selectlab <- base_data[grep(ID[i], base_data[,Base_ID]),]
      start <- selectlab[,Base_Start]
      start <- as.numeric(start)
      end <- selectlab[,Base_End]
      selectmed$Einddatum[is.na(selectmed[,TI_end])] <- end
      end <- as.numeric(end)
      framerange <- 7
      dates <- vector()
      complication <- comp_data[grep(ID[i], comp_data[,Comp_ID]),]
      compdates <- complication[,Comp_date]
      compdates <- as.numeric(compdates)
      for (j in 1:611)
      {date <- (framerange*j)+totstart
      medresult [i,j] <- -1
      medtext [i, j] <- paste(as.Date(date, origin = "1970-01-01"),"Patient:", ID[i], "Status: in cohort, not within selected time interval")
      dates <- c(dates, date)
      
      if (nrow(selectmed) != 0){
        for (k in 1:nrow(selectmed)){
          if (date > selectmed[k,TI_start] & date < selectmed[k, TI_end])
          { medresult[i,j] <- 0
          medtext [i, j] <- paste(as.Date(date, origin = "1970-01-01"),"Patient:", ID[i], "Status: in cohort, within selected time interval")
          }} #gebruik Med
      }
      if (date < start)
      {
        medresult[i,j] <- 1 # datum voor LVAD patient
        medtext [i, j] <- paste(as.Date(date, origin = "1970-01-01"),"Patient:", ID[i], "Status: Not in cohort")
      }
      
      if (date > end)
      {
        medresult[i,j] <- 1 # datum na LVAD patient
        medtext [i, j] <- paste(as.Date(date, origin = "1970-01-01"),"Patient:", ID[i], "Status: Not in cohort")
      }
      if (length(compdates) != 0  )   
      {for (l in 1:length(compdates))
      {if( compdates[l] >= date & compdates[l] <= (date+6))
      {medresult [i,j] <- 2
      medtext [i, j] <- paste("Date", as.Date(compdates[l], origin = "1970-01-01"),"Patient:", ID[i], "Status: event present")
      }}}
      }}
    })
    medresult <- as.matrix(medresult)
    medtext <- as.matrix(medtext)
    framerange <- 7
    dates <- vector()
    for (j in 1:611)
    {date <- (framerange*j)+totstart
    dates <- c(dates,date)
    }
    datess <-as.Date(dates, origin = "1970-01-01")
    datess <- as.data.frame(datess)}
    plot_ly(x = datess[,1], y = rownames(medresult), z = medresult, type = "heatmap", height = 900, hoverinfo='text', text = medtext,  showscale=FALSE)})
  
  output$eventplot <- renderPlot({
    x <- Comp_NDays
    for(i in 1:ncol(comp_data))
    {if ( is.factor(comp_data[,i]))
      {if (length (levels(comp_data[,i])) > 1 & length(levels(comp_data[,i])) < 6)
        x <- i}
    }
    ggplot(data=comp_data, aes(comp_data[,Comp_NDays]))+ geom_histogram( col = "black", aes(fill = comp_data[,x]), alpha=.2, binwidth = 100)+ labs(title="Histogram: days of event after intervention", x="Days", y="Count", fill = names(comp_data)[x])
  })
  
  output$cohortdays <- renderPlot({
    base_data$NdayPat <- base_data[,Base_End] - base_data[,Base_Start] 
    base_data$NdayPat <- as.numeric(base_data[,ncol(base_data)])
    yearspatient <- data.frame(numeric(), numeric())
    yearspatient<- rbind(yearspatient, c(0, 274))
    years <- as.integer(max(base_data[,ncol(base_data)]) / 356)+1
    for(i in 1:(years*2))
    { x <- sum(base_data$NdayPat > ((i/2) * 356))
    yearspatient <- rbind(yearspatient, c((i/2), x))
    }
    names(yearspatient) <- c("years", "patients")
    ggplot(data=yearspatient, aes(x = yearspatient$years , y =yearspatient$patients)) + geom_bar(stat = "identity", col = "black", fill = "blue", alpha=.2)+ labs(title="Patients in cohort", x="Years", y="Patients")
  })
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"TPselected","please select the TP variables",choices=TPVariables, inline = TRUE)
    }
    else
    {
      updateCheckboxGroupInput(session,"TPselected","please select the TP variables",choices=TPVariables,selected=TPVariables, inline = TRUE)
    }
  })
 
  output$kaplan <- renderPlot({
    base_data$SurvObj <- with(base_data, Surv(NdayPat, stat ==0)) 
    km.as.one <- survfit(SurvObj ~ 1, data =  base_data, conf.type = "log-log")
    ggsurvplot(km.as.one, data = base_data, risk.table = TRUE,ggtheme = theme_light())
  })
  
  output$kaplann <- renderPlot({
    base_data$stat1 <- 1
    base_data$NdayPat1 <- base_data$NdayPat
    for (i in 1:nrow(base_data))
    { if(base_data[i,Base_ID] %in% comp_data[,Comp_ID])
      base_data$stat1[i] <- 0
    subdf <- comp_data[comp_data[,Comp_ID] == base_data[i,Base_ID],]
    subdf <- subdf[order(subdf[,Comp_date]),]
    Nday <- as.numeric(subdf[i,Comp_date]- base_data[i,Base_Start])
    }
    
    base_data$SurvObjj <- with(base_data, Surv(NdayPat1, stat1 ==0)) 
    km.as.one <- survfit(SurvObjj ~ 1, data =  base_data, conf.type = "log-log")
    ggsurvplot(km.as.one, data = base_data, risk.table = TRUE,ggtheme = theme_light())
  })
  
  output$MeanComPlot <- renderPlot({
    if(input$demo == TRUE)
    {
      patientPLN <- Demo_patientPLN
    }
    else
    {
    patientPLN <- globals$mydf}

    if(nrow(patientPLN) > 3)
      {
      patientPLN <- patientPLN[patientPLN[,3] == input$TPselection ,]
     # patientPLN <- patientPLN[patientPLN[,3] == "Glucose" ,]
      patientPLN <- patientPLN[patientPLN[,5] > -input$Bdaycompp,]
      patientPLN <- patientPLN[patientPLN[,5] < input$Adaycompp,]
      
      x <- mean(patientPLN$size)/mean(patientPLN$tpoutcome)
      
      if (input$devType == "Standard Error")
      {if (all(input$sellines == c("Mean by day","Rolling Mean", "Amount Patients")))     
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +  stat_summary(fun.data = "mean_se",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("indianred2", "lightsteelblue", "deepskyblue3"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients"))
      else if (all(input$sellines == c("Rolling Mean", "Amount Patients")))  
        p <- ggplot(data=patientPLN)+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("lightsteelblue", "deepskyblue3"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients")) 
      else if (all(input$sellines == c("Mean by day", "Amount Patients")))  
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  + stat_summary(fun.data = "mean_se",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("indianred2", "lightsteelblue"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients")) 
      else if (all(input$sellines == c("Mean by day", "Rolling Mean")))
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +   stat_summary(fun.data = "mean_se",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ scale_color_manual(values=c("indianred2", "deepskyblue3")) 
      else if (input$sellines == ("Amount Patients"))
        p <- ggplot(data=patientPLN)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("lightsteelblue"))+ geom_vline(xintercept=0)
      else if (input$sellines == ("Rolling Mean"))
        p <- ggplot(data=patientPLN)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ scale_color_manual(values=c("deepskyblue3"))+ geom_vline(xintercept=0) 
      else if (input$sellines == ("Mean by day"))
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +   stat_summary(fun.data = "mean_se",geom = "smooth")+ geom_vline(xintercept=0) + scale_color_manual(values=c("indianred2")) 
      }
      if (input$devType == "Standard Deviation")
      {if (all(input$sellines == c("Mean by day","Rolling Mean", "Amount Patients")))     
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +  stat_summary(fun.data = "mean_sd",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("indianred2", "lightsteelblue", "deepskyblue3"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients"))
      else if (all(input$sellines == c("Rolling Mean", "Amount Patients")))  
        p <- ggplot(data=patientPLN)+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("lightsteelblue", "deepskyblue3"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients")) 
      else if (all(input$sellines == c("Mean by day", "Amount Patients")))  
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  + stat_summary(fun.data = "mean_sd",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size/x ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("indianred2", "lightsteelblue"))+ scale_y_continuous(sec.axis = sec_axis(~.*x, name = "N Patients")) 
      else if (all(input$sellines == c("Mean by day", "Rolling Mean")))
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +   stat_summary(fun.data = "mean_sd",geom = "smooth")+ geom_vline(xintercept=0) + geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ scale_color_manual(values=c("indianred2", "deepskyblue3")) 
      else if (input$sellines == ("Amount Patients"))
        p <- ggplot(data=patientPLN)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$size ,colour = "N Patients"), size =1)+ scale_color_manual(values=c("lightsteelblue"))+ geom_vline(xintercept=0)
      else if (input$sellines == ("Rolling Mean"))
        p <- ggplot(data=patientPLN)+ geom_line(data = patientPLN, aes(x=patientPLN$dte, y = patientPLN$rollmean ,colour = "Rolling mean"), size =1)+ scale_color_manual(values=c("deepskyblue3")) + geom_vline(xintercept=0)
      else if (input$sellines == ("Mean by day"))
        p <- ggplot(data=patientPLN,aes(x=patientPLN$dte, y= patientPLN$tpoutcome, colour = "Mean by day"))  +   stat_summary(fun.data = "mean_sd",geom = "smooth")+ geom_vline(xintercept=0) + scale_color_manual(values=c("indianred2")) }
      p + labs(x = "days", y =input$TPselection) 
    }
  })

  globals <- reactiveValues(mydf = data.frame())
  
  observeEvent (input$CMeanComPlot, {
       if (input$demo == FALSE)
      {patientPLN <- data.frame()
       withProgress(message = 'Calculating mean TP values before complication', value = 0, {
         
       for(i in 1:nrow(comp_data))
       {print(i)
         incProgress((1/nrow(comp_data))/2, detail =  paste((((i/nrow(comp_data))/2)*100),"% Completed"))
         
         patientPL <- TP_data1[grep(comp_data[i,Comp_ID], TP_data1[,TP_ID]),]
         patientPL <- patientPL[patientPL[,TP_Type] %in% TPVariables,]
         names(patientPL)[TP_date] <- "tpdate"
         names(patientPL)[TP_ID] <- "tpid"
         names(patientPL)[TP_Outcome] <- "tpoutcome"
         names(patientPL)[TP_Type] <- "tptype"

         patientPL$tpoutcome <- as.numeric(as.character(patientPL$tpoutcome))
         patientPL <- patientPL[complete.cases(patientPL),]
         patientPL <- patientPL[,c(TP_date, TP_ID, TP_Type, TP_Outcome)]
         if(nrow(patientPL) == 0) next 
         patientPL <- aggregate(tpoutcome ~ tpdate + tpid +tptype, patientPL, mean)
         ddate <- comp_data[i,Comp_date]
         patientPL$dte <- patientPL$tpdate - ddate
         start_ddate <- ddate - input$Bdaycompp
        end_ddate <- ddate + input$Adaycompp
         start_ddate <- ddate - 100
         end_ddate <- ddate + 20
         patientPL <- patientPL[patientPL$tpdate > start_ddate,]
         patientPL <- patientPL[patientPL$tpdate < end_ddate,]
        if(nrow(patientPL)>5)
         patientPLN <- rbind(patientPLN, patientPL)
       }
      patientPLN$rollmean <- 0
       patientPLN$dte <- as.numeric(as.character(patientPLN$dte))
       for (i in 1:nrow(patientPLN))
       {
         incProgress((1/nrow(patientPLN))/4, detail =  paste(((((i/nrow(patientPLN))/4)*100)+50) ,"% Completed"))
         
         startdate <- patientPLN$dte[i]-1
         enddate <- patientPLN$dte[i] + 1
         interval <- seq(startdate,enddate,1)
         subpatientPLN <- patientPLN[patientPLN$tptype == patientPLN$tptype[i],]
         patientPLN$rollmean[i] <- mean(subpatientPLN[subpatientPLN$dte %in% interval,4])
       }
       
       patientPLN$size = 0
       for (i in 1:nrow(patientPLN))
       {incProgress((1/nrow(patientPLN))/4, detail =  paste(((((i/nrow(patientPLN))/4)*100)+75) ,"% Completed"))

         subpatientPLN <- patientPLN[patientPLN$tptype == patientPLN$tptype[i],]
       patientPLN$size[i] <- sum(subpatientPLN$dte == patientPLN$dte[i])}})
       globals$mydf <- patientPLN}
     })
   
  output$compplot <- renderPlot({
    
  i <-  input$nextcompplot +1
  if (i != 1)
  {i <- i - input$prevcompplot}
    SelTPVariables <- input$TPselected
   if (length(SelTPVariables) != 0)
    {patientPL <- TP_data1[grep(comp_data[i,Comp_ID], TP_data1[,TP_ID]),]
    #patientPL[,TP_date] <- as.Date(patientPL[,TP_date], "%d-%m-%Y")
    patientPL <- patientPL[patientPL[,TP_Type] %in% SelTPVariables,]
    #patientPL[,TP_Outcome] <- gsub("<", "", patientPL[,TP_Outcome])
    #patientPL[,TP_Outcome] <- gsub(">", "", patientPL[,TP_Outcome])          
    #patientPL[,TP_Outcome] <- as.numeric(as.character(patientPL[,TP_Outcome]))
    patientPL <- patientPL[,c(TP_date, TP_Type, TP_Outcome)]
   # patientPL <- patientPL[complete.cases(patientPL),]
    
    ddate <- comp_data[i,Comp_date]
    start_ddate <- ddate - input$Bdaycomp
    end_ddate <- ddate + input$Adaycomp
    
    
    patientPL_Scaled <- patientPL[0,]
    for (j in 1:length(SelTPVariables))
    { patientPL_subset <- patientPL[grep(SelTPVariables[j], patientPL[,2]),]
    patientPL_subset[,3] <- scale(patientPL_subset[,3])
    patientPL_Scaled <- rbind(patientPL_Scaled, patientPL_subset)
    }
    patientPL_Scaled <- patientPL_Scaled[patientPL_Scaled[,1] > start_ddate,]
    patientPL_Scaled <- patientPL_Scaled[patientPL_Scaled[,1] < end_ddate,]
    
    ggplot(data=patientPL_Scaled,aes(x=patientPL_Scaled[,1], y= patientPL_Scaled[,3], colour= patientPL_Scaled[,2]), heigth = 10) + geom_line(size = 1)+ geom_vline(xintercept=(comp_data[i,Comp_date]))
  }})

  output$eventstat <- renderTable({ 
    if (input$demo == TRUE)
    {meanframe <- Demo_meanframe}
    
    else
    {
      withProgress(message = 'Calculating event stats', value = 0, {
        
        meanframe <- data.frame(as.character(), as.numeric(), as.numeric(), as.numeric(), as.numeric(), as.numeric())
        basevec1 <- vector()
        basevec2 <- vector()
        NVar <- 10
        IDvec <- unique(comp_data[,Comp_ID])
        x1 <- as.factor(base_data[(base_data[,Base_ID] %in% IDvec), Base_Gender])
        x2 <- base_data[(base_data[,Base_ID] %in% IDvec), Base_Age]
        
        basevecOveralGender <- as.factor(base_data[,Base_Gender])
        basevecOveralAge <- base_data[,Base_Age]
        
        propTestGen<-prop.test(matrix(c(table(x1),table(basevecOveralGender)),ncol=2,byrow=T))
        wilTestAge <- wilcox.test(x2, basevecOveralAge)
        
        meanframe <- rbind(meanframe, c(as.character("Gender"), round(mean(as.numeric(as.character(basevecOveralGender))),2), NA, round(mean(as.numeric(as.character(x1))),2), NA, round(propTestGen$p.value,4)))
        meanframe[,1] <- as.character(meanframe[,1])
        meanframe[,2] <- as.numeric(as.character(meanframe[,2]))
        meanframe[,3] <- as.numeric(as.character(meanframe[,3]))
        meanframe[,4] <- as.numeric(as.character(meanframe[,4]))
        meanframe[,5] <- as.numeric(as.character(meanframe[,5]))
        meanframe[,6] <- as.numeric(as.character(meanframe[,6]))
        meanframe <- rbind(meanframe, c(as.character("Age"), round(mean(basevecOveralAge),2), round(sd(basevecOveralAge),2),round(mean(x2),2), round(sd(x2),2), round(as.numeric(wilTestAge$p.value),4)))
        
        
        TP_Nvariables <- as.data.frame(table(TP_data1[,TP_Type]))
        TP_Nvariables <- TP_Nvariables[order(-TP_Nvariables$Freq),]
        if (nrow(TP_Nvariables) > 10)
        topVariables <- TP_Nvariables[1:10,1]
        else
        {topVariables <- TP_Nvariables[,1]}
        
        for (j in 1:length(topVariables))
        {laboutvec <- vector()
        laboutvec1 <- vector()
        laboutvec1 <- (TP_data1[TP_data1[,TP_Type] == topVariables[j],TP_Outcome])
        incProgress(1/NVar, detail =  paste((j*NVar),"% Completed"))
        
        for (i in 1:nrow(comp_data))
        { selectlab <- TP_data1[ (TP_data1[,TP_ID] == comp_data[i, Comp_ID] & TP_data1[,TP_Type] == topVariables[j]),]
        selectlab <- selectlab[selectlab[,TP_date] == comp_data[i, Comp_date],]
        #x <- selectlab[which.min(selectlab[,TP_time]),TP_Outcome]
        x <- selectlab[1,TP_Outcome]
        laboutvec <- c(laboutvec,x)}
        laboutvec <- laboutvec[!is.na(laboutvec)]
        WilTest <- wilcox.test(laboutvec, laboutvec1)
        SD1 <- sd(laboutvec)
        SD2 <- sd(laboutvec1)
      
        meanframe[,2] <- as.numeric(as.character(meanframe[,2]))
        meanframe[,3] <- as.numeric(as.character(meanframe[,3]))
        meanframe[,4] <- as.numeric(as.character(meanframe[,4]))
        meanframe[,5] <- as.numeric(as.character(meanframe[,5]))
        meanframe[,6] <- as.numeric(as.character(meanframe[,6]))
        
        meanframe <- rbind(meanframe, c(as.character(topVariables[j]), round(mean(laboutvec1),2), round(sd(laboutvec1),2), round(mean(laboutvec),2), round(sd(laboutvec),2), round(WilTest$p.value,4)), stringsAsFactors=FALSE)
        }})
        names(meanframe) <- c("Variable", "Mean overal","Standard Deviation1", "Mean event", "Standard Deviation2", "P-Value")
}
    
    meanframe
  })  
  
  output$corrplot <- renderPlot({
    if (input$demo == TRUE)
    {
      corrmat <- Demo_corrmat
    }
    else
    {withProgress(message = 'Calculating correlations', value = 0, {
      
    TP_Nvariables <- as.data.frame(table(TP_data1[,TP_Type]))
    TP_Nvariables <- TP_Nvariables[order(-TP_Nvariables$Freq),]
    NVar <- length(TP_Nvariables$Var1)
    if (NVar > 20) NVar <- 20
    topVariables <- TP_Nvariables[1:NVar,1]
    ID <- unique(TP_data1[,TP_ID])    
    cordf <- data.frame()
    for (i in 1:length(ID))
    { x1 <- ID[i]
    x2 <- base_data[base_data[,Base_ID]== ID[i], Base_Age]
    x3 <- base_data[base_data[,Base_ID]== ID[i], Base_Gender]
    TPvec <- c(x1,x2,x3)
    incProgress(1/length(ID), detail = paste((i/length(ID)),"% Completed"))
    for (j in 1:length(topVariables))
    { selectlab <- TP_data1[ (TP_data1[,TP_ID] == ID[i] & TP_data1[,TP_Type] == topVariables[j]),]
    x <- mean(selectlab[,TP_Outcome], na.rm = TRUE)
    TPvec <- c(TPvec, x) 
    }
    cordf <- rbind(cordf, TPvec)
    }
    
    eventvec <- vector()
    for (i in 1:nrow(cordf))
    { x<- 0
    if((cordf[i,1] %in% comp_data[,Comp_ID]))
      x <- 1
    eventvec <- c(eventvec, x)
    }
    cordf$event <- eventvec
    
    names(cordf) <- c("ID", "age", "gender",as.character(topVariables), "event")
    
    cordf$gender <- as.factor(as.character(cordf$gender))
    
    for(i in 1:ncol(cordf)){
      cordf[is.nan(cordf[,i]),i] <- NA
      cordf[is.na(cordf[,i]), i] <- mean(cordf[,i], na.rm = TRUE)
    }
    })
  
    cordf$ID <- NULL
    df <- cordf
    
    cor_fun <- function(pos_1, pos_2){
      
      # both are numeric
      if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
         class(df[[pos_2]]) %in% c("integer", "numeric")){
        r <- stats::cor(df[[pos_1]]
                        , df[[pos_2]]
                        , use = "pairwise.complete.obs"
        )
      }
      
      # one is numeric and other is a factor/character
      if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
         class(df[[pos_2]]) %in% c("factor", "character")){
        r <- sqrt(
          summary(
            stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
      }
      
      if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
         class(df[[pos_1]]) %in% c("factor", "character")){
        r <- sqrt(
          summary(
            stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
      }
      
      # both are factor/character
      if(class(df[[pos_1]]) %in% c("factor", "character") &&
         class(df[[pos_2]]) %in% c("factor", "character")){
        r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
      }
      
      return(r)
    } 
    
    cor_fun <- Vectorize(cor_fun)
    
    # now compute corr matrix
    corrmat <- outer(1:ncol(df)
                     , 1:ncol(df)
                     , function(x, y) cor_fun(x, y)
    )
    
    rownames(corrmat) <- colnames(df)
    colnames(corrmat) <- colnames(df)}

    corrplot(corrmat, type="upper")

  })
  
  output$dataimpu <- renderText({
    #paste(input$dataimp)
    if (input$dataimp == "Mean")
    tekst <- "Using the mean as a data imputation method is a single imputation method. Imputating the mean is a simple method and therefore very fast. Because of simplicity, it mostly comes at costs for reliability and often under performs relative to the more complex ones."
    if (input$dataimp == "Amelia")
    tekst<- "Amelia is a method that uses multiple imputation (generate imputed data sets) to deal with missing values. Multiple imputation helps to reduce bias and increase efficiency.  It is enabled with bootstrap based EMB algorithm which makes it faster and robust to impute many variables including cross sectional, time series data etc. Also, it is enabled with parallel imputation feature using multicore CPUs."
    if (input$dataimp == "MissForest")
      tekst<- "MissForest is a commonly used multiple imputation method that makes use of the random forest algorithm. It builds a random forest model for each variable and use out of bag imputation error estimate. Many columns require relatively a lot work to perform and therefore is less fast than the simple ones."
    if (input$dataimp == "MICE")
      tekst<- "MICE (Multivariate Imputation via Chained Equations) is a data imputation method that uses 4 four different methods for predicting the missing values. By using for example logistic regression and predictive mean matching, it assumes that the missing data are Missing at Random."
    tekst
  })

  output$featexx <- renderText({
    if (input$featex == "Last Values")
    {tekst <- "Based on the sliding window approach the last 3 available time points are used as features. The advantages is its simplicity and therefore interpretability. The features only represent a snapshot of that moment, in case a lot of historical data is gathered, it might loose important information and result in lower accuracy."}
    if (input$featex == "Statistics")
    {tekst <- "Using statistical features is also relatively simple approach of extracting static features from temporal data. Based on a research of Nanopolous, we calculate the the mean, median, skewness and kurtosis of the first and second features."}
    if (input$featex == "Summary")
    {tekst <- "Based on research of Hauskrecht et al. we create a set of features that summarizes a time series. Features used in this approach are 'last slope', '% of change in the windowframe', 'last value difference', 'minimun' and maximum'. "}
    if (input$featex == "Trend Value Abstraction")
    {tekst <- "Based on a research of Esmael et al. we create of temporal abstraction of a time series. We use a SAX algoritm to develop value-based approximations and fit a least sequares to line to determine the trend."}
    if (input$featex == "Frequent Pattern Mining")
    {tekst <- "Based on a research of Batal et al. we use a frequent sequential pattern mining alogrithm to mine the most frequently occuring pattern. Based on the 100 most predicitive patterns we create binary features stating wether a certain pattern occured for that window frame."}
    tekst
  })
  
  output$TIperPat <- renderPlot({
    
    if (exists("TI_data1"))
    {
      test <- TI_data1
      test$cou <-1
      
      library(dplyr)
      test1<- test %>% mutate(monthyear = as.character(format(Startdatum, "%m-%Y"))) %>%
        arrange(Startdatum) %>% 
        group_by(monthyear) %>%
        summarise( flow = sum(cou))
      
      test1$monthyear1 <- as.Date(paste("01-",test1$monthyear),format = "%d-%m-%Y")
      
      test1$count <- 0
      for(i in 1:nrow(test1))
      { x <- base_data[base_data[,Base_Start]<test1$monthyear1[i],]
      x <- x[x[,Base_End]>test1$monthyear1[i],]
      test1$count[i] <- nrow(x)
      }
      test1 <- data.frame(test1)
      test1 <- test1[test1$count >0,]
      test1 <- test1[order(test1$monthyear1),]
      test1$newcount <- 0
      for(i in 1:nrow(test1))
      { 
        subdf1 <- test[test[,TI_start] < test1[i,3] ,]
        subdf1 <- subdf1[subdf1[,TI_end] > (test1[i,3]) ,]
        test1$newcount[i] <- nrow(subdf1)
      }
      
      test1$mean <- test1$newcount/test1$count
      ggplot(test1, aes(test1$monthyear1, test1$mean)) + geom_line() + xlab("Date") + ylab("Interval active per patient per month")
    
    }
  })
  
  output$checkTI <- renderUI({
    if (exists("TI_data1"))
    {
    test <- TI_data1
    #test <- TI_data21
    test$cou <-1
    
    library(dplyr)
    test1<- test %>% mutate(monthyear = as.character(format(Startdatum, "%m-%Y"))) %>%
      arrange(Startdatum) %>% 
      group_by(monthyear) %>%
      summarise( flow = sum(cou))
    
    test1$monthyear1 <- as.Date(paste("01-",test1$monthyear),format = "%d-%m-%Y")
    
    test1$count <- 0
    for(i in 1:nrow(test1))
    { x <- base_data[base_data[,Base_Start]<test1$monthyear1[i],]
    x <- x[x[,Base_End]>test1$monthyear1[i],]
    test1$count[i] <- nrow(x)
    }
    test1 <- data.frame(test1)
    test1 <- test1[test1$count >0,]
    test1 <- test1[order(test1$monthyear1),]
    test1$newcount <- 0
    for(i in 1:nrow(test1))
    { 
      subdf1 <- test[test[,TI_start] < test1[i,3] ,]
      subdf1 <- subdf1[subdf1[,TI_end] > (test1[i,3]) ,]
      test1$newcount[i] <- nrow(subdf1)
    }
    
    test1$mean <- test1$newcount/test1$count
    
    test1[is.infinite(test1$mean),5] <- NA
    test1 <- test1[complete.cases(test1),]
    
    test1 <- test1[order(test1$monthyear1),]
    cutoffsug <- test1[test1$mean > mean(test1$mean),]
    x <- cutoffsug$monthyear1[1]
    TimeSpan <- max(base_data$INT_date) - min(base_data$INT_date)
    Timedif <- cutoffsug$monthyear1[1] - min(base_data$INT_date)
    percTime <- as.numeric(Timedif)/as.numeric(TimeSpan)
   # globals$prTime <- percTime

    TI_data2 <- TI_data1
    vect <-vector()
    medvec <- data.frame(table(TI_data2$ATC_code))
    medvec <- medvec[order(-medvec$Freq),]
    #medvec <- medvec[1:60,1]
    medvec <- medvec[1,1]
    TI_data2 <- TI_data2[TI_data2$ATC_code %in% medvec,]
    PIDS <- unique(TI_data2$PseudoID_voorkeur)
    if (length (PIDS) > 50)
     { PIDS <- PIDS[1:50]}
    
    TI_data2$ID <- seq(1:nrow(TI_data2))
    for (i in 1:length(medvec))
    {print(i)
      subdf <- TI_data2[TI_data2$ATC_code == medvec[i],]
      for( j in 1:length(PIDS))
      {print(j)
        subdff <- subdf[subdf$PseudoID_voorkeur == PIDS[j],]
        for (k in 1:nrow(subdff))
        { xx <- ""
        xx<- subdff[subdff$Startdatum < subdff$Startdatum[k],]
        xx <- xx[xx$Einddatum >  subdff$Startdatum[k],]
        if (nrow(xx)> 0)
          vect <- c(vect, xx$ID)
        }
      }
    }
    
    if (length (vect) > 0)
      {globals$tt <- "Multiple patients have multiple time interval registrations occuring  at the same time. To remove all time interval registrations occuring multiple times, please press the button below. A new plot of the cleaned dataset will be created."
    output$TIcheck2 <- renderText("-")}
  
    if (percTime> 0.1)
     { output$TIcheck <- renderText("Dataset subsetting")
     updateDateInput(session, "cutTI","Specify cutt of point", value = x )
     output$plot <- renderPlot({
       ggplot(test1, aes(test1$monthyear1, test1$mean)) + geom_line() + xlab("Date") + ylab("Interval active per patient per month")+ geom_vline(xintercept = input$cutTI, color = "red")})
   tekst <- "It seems that the quality of the first period is low, therefore we would suggest to subset the dataset. The plot below shows the avarage amount of Time interval registrations per patient per month. The red vertical line is our suggestion as cutoff point. Our suggestion is filled in the date input below, if you would like to change this, please specify a new date."}}
    if(percTime<0.1)
    {output$TIcheck <- renderText("Class label selection")
      tekst <-  ""
    }
    
   HTML(tekst)
    
    
  })
  
  output$TIReg <- renderText(globals$tt)
  
  observeEvent(input$cleanTI,{
    if(input$demo == FALSE)
    {TI_data2 <- TI_data1
    vect <-vector()
    medvec <- data.frame(table(TI_data2$ATC_code))
    medvec <- medvec[order(-medvec$Freq),]
    withProgress(message = 'cleaning TI dataset', value = 0, {
    medvec <- medvec[1:60,1]
    TI_data2 <- TI_data2[TI_data2$ATC_code %in% medvec,]
    PIDS <- unique(TI_data2$PseudoID_voorkeur)
    
    for (i in 1:length(medvec))
    {incProgress((1/length(medvec)), detail =  paste(((i/length(medvec))*100),"% Completed"))
      subdf <- TI_data2[TI_data2$ATC_code == medvec[i],]
      for( j in 1:length(PIDS))
      {
        subdff <- subdf[subdf$PseudoID_voorkeur == PIDS[j],]
        for (k in 1:nrow(subdff))
        { x <- ""
        x<- subdff[subdff$Startdatum < subdff$Startdatum[k],]
        x <- x[x$Einddatum >  subdff$Startdatum[k],]
        if (nrow(x)> 0)
          vect <- c(vect, x$ID)
        }
      }
    }
    
    TI_data21 <- TI_data2[((TI_data2$ID %in% vect) ==FALSE),]
    globals$TI_data21 <- TI_data21
    globals$vect <- vect
    })}
    
  })
  
  output$medplott <- renderPlot({
    if (input$demo == TRUE)
    {test <- Demo_TI_data}
    else
    {test <- globals$TI_data21}
    if (nrow(test) > 1)
    {test$cou <-1
    
    library(dplyr)
    test1<- test %>% mutate(monthyear = as.character(format(Startdatum, "%m-%Y"))) %>%
      arrange(Startdatum) %>% 
      group_by(monthyear) %>%
      summarise( flow = sum(cou))
    
    test1$monthyear1 <- as.Date(paste("01-",test1$monthyear),format = "%d-%m-%Y")
    
    test1$count <- 0
    for(i in 1:nrow(test1))
    { x <- base_data[base_data[,Base_Start]<test1$monthyear1[i],]
    x <- x[x[,Base_End]>test1$monthyear1[i],]
    test1$count[i] <- nrow(x)
    }
    test1 <- data.frame(test1)
    test1 <- test1[test1$count >0,]
    test1 <- test1[order(test1$monthyear1),]
    test1$newcount <- 0
    for(i in 1:nrow(test1))
    { 
      subdf1 <- test[test[,TI_start] < test1[i,3] ,]
      subdf1 <- subdf1[subdf1[,TI_end] > (test1[i,3]) ,]
      test1$newcount[i] <- nrow(subdf1)
    }
    
    test1$mean <- test1$newcount/test1$count
    
    test1[is.infinite(test1$mean),5] <- NA
    test1 <- test1[complete.cases(test1),]
    
    test1 <- test1[order(test1$monthyear1),]
    cutoffsug <- test1[test1$mean > mean(test1$mean),]
    x <- cutoffsug$monthyear1[1]
    TimeSpan <- max(base_data$INT_date) - min(base_data$INT_date)
    Timedif <- cutoffsug$monthyear1[1] - min(base_data$INT_date)
    percTime <- as.numeric(Timedif)/as.numeric(TimeSpan)
    print(percTime)
    
   # if (percTime> 0.1)
  #  { output$TIcheck <- renderText("Dataset subsetting")
  #  updateDateInput(session, "cutTI","Specify cutt of point", value = x )}
   
    ggplot(test1, aes(test1$monthyear1, test1$mean)) + geom_line() + xlab("Date") + ylab("Interval active per patient per month")+ geom_vline(xintercept = input$cutTI, color = "red")}})
    
  globals <- reactiveValues(prepDataset = data.frame())
  globals <- reactiveValues(Boruta = list())
  globals <- reactiveValues(rfeSel = rfe)
  
  observeEvent(input$dataprep,{
    withProgress(message = 'Preparing dataset', value = 0, {
      route <- input$featex
      dataimputation <- input$dataimp
      
      labvec <<- TPtable[outVar(),1]
      
      TP_data2 <- TP_data1
      TP_data2 <- TP_data2[(TP_data2[,TP_Type] %in% labvec),]
      
      TP_data2[,TP_Outcome] <- as.numeric(TP_data2[,TP_Outcome])
      
      x <<- as.Date(input$cutTI)
      
      print (x)
      TP_data2 <- TP_data2[(TP_data2[,TP_date] > x) ,]
      maxDate <- max(comp_data[,Comp_date])
      TP_data2 <- TP_data2[TP_data2[,TP_date] < maxDate,]
      ID <- unique(TP_data2[,TP_ID])
      
      if(route == "Frequent Pattern Mining")
      {bound <- data.frame()
      
      print("here we go")
      for (i in 1:length(labvec))
      {
        dfsel <- TP_data2[grep(labvec[i], TP_data2[,TP_Type]),] #DF with all measurement of one labvalue
        
        bound[1,i] <- quantile(dfsel[,TP_Outcome], 0.1, na.rm = TRUE) # determining the boundaries, col 18 is value 
        bound[2,i]<- quantile(dfsel[,TP_Outcome], 0.25, na.rm = TRUE)
        bound[3,i]<- quantile(dfsel[,TP_Outcome], 0.75, na.rm = TRUE)
        bound[4,i]<- quantile(dfsel[,TP_Outcome], 0.9, na.rm = TRUE)}
      
      windowsize <- 6
      
      paste("done")
      TP_data2[,TP_date] <- as.numeric(TP_data2[,TP_date])
      colnames(TP_data2)[TP_date] <- "TPDate"
      TP_data2$ab <- "M"
      AbCol <- ncol(TP_data2)
      for ( i in 1:nrow(TP_data2)) # create abstractions of all datapoints
      {print(i)
        j <- which(TP_data2[i,TP_Type] == labvec) #j is to check which labmeasurement it is 
        TP_data2[i,AbCol] <- as.character( cut(TP_data2[i,TP_Outcome], 
                                               breaks = c(-Inf,bound[,j], Inf), 
                                               labels = c(
                                                 paste(labvec[j],"VL", sep = ""), 
                                                 paste(labvec[j],"L", sep = ""),
                                                 paste(labvec[j],"M", sep = ""), 
                                                 paste(labvec[j],"H", sep = ""),
                                                 paste(labvec[j],"VH", sep = "")))) #based on boundaries, it creates abstractions
      }
      print("done2")
      TP_data3 <- TP_data2[0,] # create a cleaned data with no multiple measurements on same day
      for ( j in 1:length(ID)) 
      { patientdf <- TP_data2[grep(ID[j], TP_data2[,TP_ID]),] #create a DF for each patient
      for (k in 1:length(labvec))
      {
        labdf <- patientdf[grep(labvec[k], patientdf[,TP_Type]),] #create df for each patient for each labmeasurement 
        labdf <- labdf %>% group_by(TPDate) %>% arrange(TPDate) %>% slice(1)# slices the multiple measurement on a day
        labdf <- as.data.frame(labdf) 
        TP_data3 <- rbind(TP_data3, labdf)# bind it
      }
      print(j)
      }
      
      
      TP_data3$bl <- "0" 
      blCol <- ncol(TP_data3)
      TP_data3$blW <- "0"
      blwCol <- ncol(TP_data3)
      TP_data3$IDseq <- seq(1:nrow(TP_data3)) 
      IDseqCol <- ncol(TP_data3)
      print("starting with iterationproces 1 out of 3")
      Imax <- nrow(comp_data)
      
      for(i in 1: Imax){
        
        
        datevec <- ""
        IDvec <- ""
        ID1 <- ""
        patient1 <- TP_data3[grep(comp_data[i,1], TP_data3[,TP_ID]),]#dataframe of that patient
        ddate <- comp_data[i,Comp_date] # date of complication
        compmin <- input$classmin
        compmax <- input$classmax
        ddate <- ddate-compmin
        class.date.vec <- ddate
        if (compmax - compmin != 0)
        {class.date.vec <- c(ddate, (ddate-seq(1:(compmax-compmin))))}# all dates that fall within the complication windowframe (compmax - compmin)
        del <- ""
        if (compmin != 0)
        {del <- ((ddate - (seq(1:compmin)-1))+compmin)}
        delvec <- patient1$IDseq[patient1[,TP_date] %in% del]
        IDvec <- patient1$IDseq[patient1[,TP_date] %in% class.date.vec]
        TP_data3$bl[TP_data3$IDseq %in% IDvec] <- "1" # indicator of bleeding occured
        TP_data3$blW[TP_data3$IDseq %in% IDvec] <- i # indicator of bleeding window
        TP_data3 <- TP_data3[(!(TP_data3$IDseq %in% delvec)),]
      }
      
      patternset <- data.frame() # the abstractions dataset
      teller <- 0
      ID <- unique(TP_data3[,TP_ID])
      print("starting with iterationproces 2 out of 3")
      Imax <- length(ID)
      patientdf <- TP_data3[TP_data3[,TP_ID] == ID[1],] # df of patient with all abstractions 
      dates <- unique(patientdf[TP_date]) # vec with all the unique measurement dates for that patient
      dates <- (dates[,1])
      baseSel <- base_data[base_data[,Base_ID] == ID[1],]
      stava<- c(baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
      
      for (i in 1:Imax) 
      {
        print("yes")
        patientdf <- TP_data3[TP_data3[,TP_ID] == ID[i],] # df of patient with all abstractions 
        dates <- unique(patientdf[TP_date]) # vec with all the unique measurement dates for that patient
        dates <- (dates[,1])
        baseSel <- base_data[base_data[,Base_ID] == ID[1],]
        stava<- c(baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
        
        for (j in 1:length(dates))
        {subdf <- patientdf[grep(dates[j], patientdf[,TP_date]),] # create df with all measurements on that day for that patient
        Nday <- as.numeric(dates[j])-as.numeric(baseSel[1,Base_Start])
        x <- paste(subdf[,AbCol], collapse = " ") # paste all abstractions seperated with witespaces
        rij <- c((subdf[1,TP_ID]),( subdf[1,TP_date]), (nrow(subdf)), subdf[1,blCol], (x), subdf[1,blwCol], Nday, stava) # create a row that contains all information  HIER MEEGEVEN
        patternset <- rbind(patternset, rij, stringsAsFactors=FALSE)}
      }
      
      
      patternset$ID <- seq(1:nrow(patternset))
      colnames(patternset) <- c("Pseudo_ID", "event", "size", "bl", "pattern", "classwin","Days", "gender","age", "ID")
      patternset$event <- as.numeric(as.character(patternset$event))
      print("starting with iterationproces 3 out of 3")
      Imax <- nrow(patternset)
      pb <- winProgressBar(title="Progress bar iteration process 3", label="0% done", min=0, max=Imax, initial=0)
      complete.patternset <- patternset[0,]
      for (i in 1:Imax)
      { date <- patternset$event[i]
      info <- sprintf("%d%% done", round((i/Imax)*100)) 
      setWinProgressBar(pb, i/(Imax)*Imax, label=info)   
      datevec <- c(date, (date-seq(1:windowsize)))
      subdf <- patternset[which(patternset$Pseudo_ID == patternset$Pseudo_ID[i]),]
      subdf <- subdf[which(subdf$event %in% datevec),]
      if(grepl("Hemoglobine", subdf$pattern[1]))
      { subdf$ID <- i
      subdf$bl <- patternset$bl[i]
      if(patternset$bl[i] == 1)
        rij <- c(subdf[1,1], (as.numeric(as.character(patternset[i,2]))+0.5), 1, 1, "bleeding", subdf[nrow(subdf), 6], subdf[1,7], subdf[1,8], subdf[1,9], i )
      if(patternset$bl[i] == 0)
        rij <- c(subdf[1,1], (as.numeric(as.character(patternset[i,2]))+0.5), 1, 0, "no_bleeding", subdf[nrow(subdf), 6],subdf[1,7], subdf[1,8], subdf[1,9], i)
      subdf <- rbind(subdf, rij)
      complete.patternset <- rbind(complete.patternset, subdf)
      }}
      
      globals$prepDataset <- complete.patternset
      dfff <<- complete.patternset
      }
      
      
      if(route == "Last Values")
      {
        df_lastvalues <- data.frame()
        j <- 1
        for(k in 1:length(ID))
        {
          patient1 <- TP_data2[grep(ID[k], TP_data2[,TP_ID]),]
          hemo <- patient1[patient1[,TP_Type] == input$classBaseWindow,]
          hemo <- hemo[order(hemo[,TP_date]),]
          
          
          IntDate <- base_data[base_data[,Base_ID]==ID[k],Base_Start]
          hemo <- hemo[hemo[,TP_date]> IntDate+30,]
          
          print(k)
          baseSel <- base_data[base_data[,Base_ID] == ID[k],]
          stava<- c(baseSel[1,Base_ID],baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
          incProgress((1/length(ID)*0.8), detail = paste(((k/length(ID))*100),"% Completed"))
          
          if(nrow(hemo) > 3)
          {for (i in 3: nrow(hemo))
          {
            ll <- list()
            lln <- list()
            if (hemo[i, TP_date] != hemo[(i-1),TP_date])
            {
              for(j in 1:length(labvec)){
                xdf <- patient1[grep(labvec[j], patient1[,TP_Type]),]
                windowmin <- hemo[i,TP_date]-windowsize
                windowmax <- hemo[i,TP_date]
                xdf<- xdf[xdf[,TP_date] <= hemo[i,TP_date],]
                xdf <- xdf[xdf[,TP_date] >= (hemo[i,TP_date]-windowsize),]
                
                xdf <- patient1[grep(labvec[j], patient1[,TP_Type]),]
                LD1 <- which(abs(xdf[,TP_date] - hemo[(i),TP_date]) == min(abs(xdf[,TP_date] - hemo[(i),TP_date])))
                x1 <- xdf[LD1[1], TP_Outcome]
                LD2 <- which(abs(xdf[,TP_date] - hemo[(i-1),TP_date]) == min(abs(xdf[,TP_date] - hemo[(i-1),TP_date])))
                x2 <- xdf[LD2[1], TP_Outcome]
                LD3 <- which(abs(xdf[,TP_date] - hemo[(i-2),TP_date]) == min(abs(xdf[,TP_date] - hemo[(i-2),TP_date])))
                x3 <- xdf[LD3[1], TP_Outcome]
                de <- c(x1, x2, x3)
                ll <- c(ll, de)  # list of 3 most recent values closest to hemo 3 values
                varna <- c(paste(labvec[j],"1", sep = ""), paste(labvec[j], "2", sep = ""), paste(labvec[j], "3", sep = "")) #names variables 
              }
              fin <- unlist(ll)
              dates <- c(dates, as.character(hemo[i,TP_date]))
              date <-  hemo[i,TP_date]
              Nday <- date-baseSel[1,Base_Start]
              rij <- c(stava, fin, date, Nday)
              df_lastvalues <- rbind(df_lastvalues, rij)
              
            }
          }
          }}
        df_date <- ncol(df_lastvalues)-1
        df_lastvalues <- df_lastvalues[-1,]
        
        for(j in 1:length(labvec)){
          varna <- c(paste(labvec[j],"1", sep = ""), paste(labvec[j], "2", sep = ""), paste(labvec[j], "3", sep = "")) #names variables 
          lln <- c(lln, varna)}
        
        varlist <- unlist(lln)
        
        df_prep <- df_lastvalues}
      
      if(route == "Summary")
      {df_sumtimeserie <- data.frame()
      
      for(k in 1:length(ID))
      {
        patient1 <- TP_data2[grep(ID[k], TP_data2[,TP_ID]),]
        patient1 <- patient1[order(patient1[,TP_date]),]
        hemo <- patient1[patient1[,TP_Type] == input$classBaseWindow,]
        hemo <- hemo[order(hemo[,TP_date]),]
        
        IntDate <- base_data[base_data[,Base_ID]==ID[k],Base_Start]
        hemo <- hemo[hemo[,TP_date]> IntDate+30,]
        
        print(k)
        baseSel <- base_data[base_data[,Base_ID] == ID[k],]
        stava<- c(baseSel[1,Base_ID],baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
        if(nrow(hemo) > 3)
        {for (i in 3: nrow(hemo))
        {ll <- list()
        lln <- list()
        
        if (hemo[i, TP_date] != hemo[(i-1),TP_date])
        {
          for(j in 1:length(labvec)){
            xdf <- patient1[grep(labvec[j], patient1[,TP_Type]),]
            valuevec <- vector()
            windowmin <- hemo[i,TP_date]-windowsize
            windowmax <- hemo[i,TP_date]
            xdf1 <- xdf[xdf[,TP_date] <= hemo[i,TP_date],]
            xdf1 <- xdf1[xdf1[,TP_date] >= (hemo[i,TP_date]-windowsize),]
            valuevec <- xdf1[,TP_Outcome]
            x1 <- valuevec[1]
            
            if (length(valuevec) > 1)
            { x2 <- valuevec[1]-valuevec[2]
            x3 <- min(valuevec)
            x4 <- max(valuevec)
            x5 <- (x1-x3)/x3
            x6 <- (x1-x4)/x4}
            else
            {x2 <- 0
            x3 <- x1
            x4 <- x1
            x5 <- (x1-x3)/x3
            x6 <- (x1-x4)/x4}
            
            de <- c(x1, x2, x3, x4,x5,x6)
            ll <- c(ll, de) 
            varna <- c(paste(labvec[j],"last", sep = ""), paste(labvec[j], "lastdif", sep = ""), paste(labvec[j], "nadir", sep = ""), paste(labvec[j], "apex", sep = ""), paste(labvec[j], "nadirdif", sep = ""), paste(labvec[j], "apexdif", sep = "")) #names variables 
            lln <- c(lln, varna)}
          fin <- unlist(ll)
          date <-  hemo[i,TP_date]
          Nday <- date-baseSel[1,Base_Start]
          rij <- c(stava, fin, date, Nday)
          df_sumtimeserie <- rbind(df_sumtimeserie, rij)}
        
        }}}
      df_date <- ncol(df_sumtimeserie)-1
      df_sumtimeserie[,df_date] <- as.Date(df_sumtimeserie[,df_date], origin = "1970-1-1")
      
      varlist <- unlist(lln)
      df_prep <- df_sumtimeserie}
      
      if(route == "Statistics")
      {df_statistics <- data.frame()
      j <- 1
      
      for(k in 1:length(ID))
      {
        patient1 <- TP_data2[grep(ID[k], TP_data2[,TP_ID]),]
        patient1 <- patient1[order(patient1[,TP_date]),]
        hemo <- patient1[patient1[,TP_Type] == input$classBaseWindow,]
        hemo <- hemo[order(hemo[,TP_date]),]
        
        IntDate <- base_data[base_data[,Base_ID]==ID[k],Base_Start]
        hemo <- hemo[hemo[,TP_date]> IntDate+30,]
        
        print(k)
        baseSel <- base_data[base_data[,Base_ID] == ID[k],]
        stava<- c(baseSel[1,Base_ID],baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
        incProgress((1/length(ID)*0.8), detail = paste(((k/length(ID))*100),"% Completed"))
        if(nrow(hemo) > 3)
        {for (i in 3: nrow(hemo))
        {ll <- list()
        lln <- list()
        
        if (hemo[i, TP_date] != hemo[(i-1),TP_date])
        {
          for(j in 1:length(labvec)){
            xdf <- patient1[patient1[,TP_Type] == labvec[j],]
            
            valuevec <- vector()
            valuevec1 <- vector() # second order features
            
            windowmin <- hemo[i,TP_date]-windowsize
            windowmax <- hemo[i,TP_date]
            xdf1 <- xdf[xdf[,TP_date] <= hemo[i,TP_date],]
            xdf1 <- xdf1[xdf1[,TP_date] >= (hemo[i,TP_date]-windowsize),]
            valuevec <- xdf1[,TP_Outcome]
            
            x1 <- mean(valuevec)
            x2 <- var(valuevec)     
            if(is.na(x2) == TRUE)
            {x2 <- 0}
            x3 <- skewness(valuevec)
            if (is.nan(x3) == TRUE)
              x3 <- 0
            x4 <- kurtosis(valuevec)
            if (is.nan(x4) == TRUE)
              x4 <- 0
            
            for(m in 1:length(valuevec)-1)
            {xx <- valuevec[m] - valuevec[m+1]
            valuevec1 <- c(valuevec1, xx)}
            
            x11 <- mean(valuevec1)
            if(is.na(x11) == TRUE)
            { x11 <- 0}
            
            x22 <- var(valuevec1)
            if(is.na(x22) == TRUE)
            {x22 <- 0}
            
            x33 <- skewness(valuevec1)
            if (is.nan(x33) == TRUE)
              x33 <- 0
            x44 <- kurtosis(valuevec1)
            if (is.nan(x44) == TRUE)
              x44 <- 0
            
            de <- c(x1, x2, x3, x4, x11, x22, x33, x44)
            ll <- c(ll, de) 
            varna <- c(paste(labvec[j],"mean_first", sep = ""), paste(labvec[j], "var_first", sep = ""),paste(labvec[j],"skew_first", sep = ""), paste(labvec[j], "kurt_first", sep = ""), paste(labvec[j], "mean_second", sep = ""), paste(labvec[j], "var_second", sep = ""),paste(labvec[j],"skew_second", sep = ""), paste(labvec[j], "kurt_second", sep = "")) #names variables 
            lln <- c(lln, varna)
          }
          fin <- unlist(ll)
          date <-  hemo[i,TP_date]
          Nday <- date-baseSel[1,Base_Start]
          rij <- c(stava, fin, date, Nday)
          df_statistics <- rbind(df_statistics, rij)}
        }}}
      df_date <- ncol(df_statistics)-1
      df_statistics[,df_date] <- as.Date(df_statistics[,df_date], origin = "1970-1-1")
      
      varlist <- unlist(lln)
      
      varlists <<- unlist(lln)
      df_prep <<- df_statistics}  
      
      if(route == "Trend Value Abstraction")
      {df_TVA <- data.frame()
      j <- 1
      for(k in 1:length(ID))
      {
        patient1 <- TP_data2[grep(ID[k], TP_data2[,TP_ID]),]
        patient1 <- patient1[order(patient1[,TP_date]),]
        hemo <- patient1[patient1[,TP_Type] == input$classBaseWindow,]
        hemo <- hemo[order(hemo[,TP_date]),]
        
        IntDate <- base_data[base_data[,Base_ID]==ID[k],Base_Start]
        hemo <- hemo[hemo[,TP_date]> IntDate+30,]
        
        print(k)
        baseSel <- base_data[base_data[,Base_ID] == ID[k],]
        stava<- c(baseSel[1,Base_ID],baseSel[1,Base_Gender],as.numeric(baseSel[1,Base_Age]))#static variables
        incProgress((1/length(ID)*0.8), detail = paste(((k/length(ID))*100),"% Completed"))
        if(nrow(hemo) > 3)
        {for (i in 3: nrow(hemo))
        {ll <- list()
        lln <- list()
        
        if (hemo[i, TP_date] != hemo[(i-1),TP_date])
        {
          for(j in 1:length(labvec)){
            xdf <- patient1[patient1[,TP_Type] == labvec[j],]
            valuevec <- vector()
            windowmin <- hemo[i,TP_date]-windowsize
            windowmax <- hemo[i,TP_date]
            xdf1 <- xdf[xdf[,TP_date] <= hemo[i,TP_date],]
            xdf1 <- xdf1[xdf1[,TP_date] >= (hemo[i,TP_date]-windowsize),]
            valuevec <- xdf1[,TP_Outcome]
            x1 <- mean(valuevec)
            max <- -(x1/20)
            min <- x1/20
            
            if (length(valuevec) == 1)
            { LD <- which(abs(xdf[,TP_date] - hemo[(i),TP_date]) == min(abs(xdf[,TP_date] - hemo[(i),TP_date])))
            if (LD[1] > 2)
            { xx1 <- xdf[(LD[1]-1), TP_Outcome]
            xx2 <- xdf[(LD[1]-2), TP_Outcome]
            valuevec <- c(xx1,xx2, valuevec)}
            }
            if(is.na(x1) == TRUE)
            {x1 <- mean(xdf$UitslagWaarde_LAB, na.rm = TRUE)}
            timevec <- seq(1:length(valuevec))
            
            if((all(is.na(valuevec))== TRUE)== FALSE & length(valuevec) > 1 & (min != max))
            {fit<- lm(valuevec ~ timevec)
            x <- fit$coefficients[2]
            x2 <- as.character( cut(x, breaks = c(-Inf,min, max, Inf), labels = c("down", "stable", "up")))} #based on boundaries, it creates trendabstractions
            else
            { x2 <- "stable"}
            de <- c(x1, x2)
            ll <- c(ll, de) 
            varna <- c(paste(labvec[j],"value", sep = ""), paste(labvec[j], "trend", sep = "")) #names variables 
            lln <- c(lln, varna)
          }  
          fin <- unlist(ll)
          date <-  hemo[i,TP_date]
          Nday <- date-baseSel[1,Base_Start]
          rij <- c(stava, fin, date, Nday)
          df_TVA <- rbind(df_TVA, rij, stringsAsFactors=FALSE)}
        }}}
      df_date <- ncol(df_TVA)-1
      print(df_TVA[1,df_date])
      df_TVA[,df_date] <- as.numeric (df_TVA[,df_date])
      
      varlist <- unlist(lln)
      
      for (i in 1:length(labvec))
      { col <- (i*2)+2
      print(col)
      df_TVA[,col]<- as.numeric(df_TVA[,col])
      sx <- convert.to.SAX.symbol(z.normalize(df_TVA[,col]), alpha = 5)
      df_TVA[,col] <- sx
      df_TVA[,col] <- as.factor(df_TVA[,col])
      df_TVA[,col+1] <- as.factor(df_TVA[,col+1])
      }
      
      for (i in 1:length(labvec))
      { col <- (i*2)+2
      print(col)
      df_TVA[which(is.na(df_TVA[,col])==TRUE), col] <- as.factor(3)
      }
      df_prep <- df_TVA
      }
      
      if (route != "Frequent Pattern Mining")
      {
        for (i in 1:ncol(df_prep)){df_prep[,i][is.infinite(df_prep[,i])] = NA}
        for (i in 1:ncol(df_prep)){df_prep[,i][is.nan(df_prep[,i])] = NA}
        
        finn <- c("Patient_ID", "Gender", "Age",varlist, "Date", "daysPat")
        colnames(df_prep) <- finn
        df_prep$daysPat <- as.numeric(df_prep$daysPat)
        if(dataimputation == "Mean")
        { for (i in 1:ncol(df_prep))
        {if(is.numeric(df_prep[,i])==TRUE)
          df_prep[is.na(df_prep[,i]), i] <- mean(df_prep[,i], na.rm = TRUE)}}
        
        
        if(dataimputation == "MICE" )
        {imputed_Data <- mice(df_prep, m=1, maxit = 20, method = 'cart', seed = 123)
        df_prep <- complete(imputed_Data,1)}
        
        if(dataimputation == "Amelia")
        {amelia_fit <- amelia(df_prep, m=1, parallel = "multicore")
        df_prep <- amelia_fit$imputations[[1]]}
        df_back <<- df_prep
        if(dataimputation=="MissForest")
        {df_prep.imp <- missForest(data.matrix(df_prep))
        df_prep <-as.data.frame(df_prep.imp$ximp)
        }
        incProgress(0.1, detail = paste(90,"% Completed"))
        
        medvec <-  TItable[input$DPTIsel_rows_selected,1]
        
        medvec <- c("B01AA07", "B01AB04", "C01BD01", "A02BC02", "B01AC08", "C03CA", "G04BE03", "B01AC04", "B01AA04", "B01AA", "B01AC", "J01")
        vect <- globals$vect
        if (length (vect) > 1)
        {TI_data1 <- TI_data1[((TI_data1$ID %in% vect) ==FALSE),]}
        
        cutcol <- ncol(df_prep)
        df_prep[,(cutcol+1):((length(medvec))+cutcol)] <- as.factor(0)
        names(df_prep)[((cutcol)+1): ((length(medvec))+cutcol)] <- as.character(medvec)
        patient11 <- TI_data1[grep(df_prep[1,1], TI_data1[,TI_ID]),]
        for (i in 2: nrow(df_prep)){
          ll <- list()
          lln <- list()
          if( df_prep[i,1] != df_prep[(i-1),1])
          {patient11 <- TI_data1[grep(df_prep[i,1], TI_data1[,TI_ID]),]}
          
          for(j in 1:length(medvec)){
            xdf <- patient11[grep(medvec[j], patient11[,TI_Type1]),]
            if (nrow(xdf) != 0){
              for(k in 1:nrow(xdf))
              {
                if(xdf[k,TI_start] <= df_prep[i,df_date] & xdf[k,TI_end] >= df_prep[i,df_date])
                {
                  df_prep[i, (cutcol+j)] <- as.factor(1)
                }
              }
            }
          }
        }
        
        for(i in 1:length(medvec))
        { df_prep[,(cutcol+i)] <- as.factor(df_prep[,(cutcol+i)])}
        df_prep$daysPat <- as.numeric(as.character(df_prep$daysPat))
        
        for (i in 1:ncol(df_prep))
          df_prep[is.na(df_prep[,i]), i] <- mean(df_prep[,i], na.rm = TRUE)
        
        dff <<- df_prep
        globals$prepDataset <- df_prep}
      output$prepareDat <- renderText({ "Ready!" })
    })
  })
  
  output$FSelPlot <- renderPlot({
  if(input$featsel == "Boruta")
   { boruta.ATT <- globals$Boruta
   p<- plot(boruta.ATT, xlab = "", xaxt = "n")
    lz<-lapply(1:ncol(boruta.ATT$ImpHistory),function(i)
      boruta.ATT$ImpHistory[is.finite(boruta.ATT$ImpHistory[,i]),i])
    names(lz) <- colnames(boruta.ATT$ImpHistory)
    Labels <- sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(boruta.ATT$ImpHistory), cex.axis = 0.7)}
  if(input$featsel == "Recursive Feature Elimination")
  {
    rfe.train <- globals$rfeSel
  p<-  plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)
  }
    p
  })
  
  globals <- reactiveValues(selVars = vector())
  globals <- reactiveValues(FullRoute = list)
  globals <- reactiveValues(fitt = list)
  globals <- reactiveValues(train = data.frame())
  globals <- reactiveValues(testbl = vector())
  globals <- reactiveValues(predbl = vector())

  observeEvent(input$cv,{
    if (input$demo == TRUE)
    {
      if (input$classSel == "Baseline Prediction") 
      {if (input$featex == "Last Values")
      {dfMod <- Demo_BaseLastValues}
        if (input$featex == "Summary")
        {dfMod <- Demo_BaseSum}
        if (input$featex == "Statistics")
        {dfMod <- Demo_BaseStat}
        if (input$featex == "Trend Value Abstraction")
        {dfMod <- Demo_BaseTVA}}
      
      if (input$classSel == "Time serie prediction")
      {if (input$featex == "Last Values")
      {
        if(input$dataimp == "Mean")
        {df_prep <- DemoLastvalues}
        if (input$dataimp == "MICE" )
        {df_prep <- DemoLastvaluesMice}
        if (input$dataimp == "Amelia")
        {df_prep <- DemoLastvaluesAm
        print("yes")}
        if (input$dataimp == "MissForest")
        {df_prep <- DemoLastvaluesMis}}
        
        
        if (input$featex == "Summary")
        {
          if(input$dataimp == "Mean")
          {df_prep <- DemoSum}
          if (input$dataimp == "MICE" )
          {df_prep <- DemoSumMice}
          if (input$dataimp == "Amelia")
          {df_prep <- DemoSumAm
          print("yes")}
          if (input$dataimp == "MissForest")
          {df_prep <- DemoSumMis}}
        
        
        
        if (input$featex == "Statistics")
        {df_prep <- DemoStat}
        if (input$featex == "Trend Value Abstraction")
        {df_prep <- DemoTVA}
        
        
      }
    }
    if (input$demo != TRUE)
    {df_prep <- dff
    #df_prep <- globals$prepDataset
    }
    
    x<-0
    df_prep$ID <- seq(1:nrow(df_prep))
    df_prep$Nbl <- 0
    df_prep$bl <- 0
    IDvec1 <- vector()
    for(i in 1: nrow(comp_data)){
      patient1 <- df_prep[grep(comp_data[i,1], df_prep[,1]),]
      patient11 <- patient1
      ddate <- comp_data[i,Comp_date]
      compmin.date <- ddate- input$classmin
      compmax.date <- ddate-input$classmax
      compmax.del <- compmin.date+input$delvec
      compmin.del <- compmin.date+1
      patient1 <- patient1[which(patient1$Date >= compmax.date),]
      patient1 <- patient1[which(patient1$Date <= compmin.date),]
      patient11 <- patient11[which(patient11$Date <= compmax.del),]
      patient11 <- patient11[which(patient11$Date >= compmin.del),]
      IDvec <- patient1$ID
      IDvec1 <- c(IDvec1, patient11$ID)
      
      if(length(IDvec) > 0)
      {x<- x+1
      df_prep$bl[df_prep$ID %in% IDvec] <- 1
      df_prep$Nbl[df_prep$ID %in% IDvec] <- x}
    }
    if (input$delvec>0)
    {df_prep <- df_prep[!df_prep$ID %in% IDvec1,]}
    df_prep$Nbl <- as.numeric(df_prep$Nbl)
    df_prep <- df_prep[order(df_prep$Date),]
    
    dfMod <<- df_prep
    
    withProgress(message = 'Performing 5-fold cross validation', value = 0, {
      if(input$featex != "Frequent Pattern Mining")
      {
        if (input$classSel == "Time serie prediction")
          dfMod$daysPat <- as.numeric(as.character(dfMod$daysPat))
        dfMod$Patient_ID <- NULL
        dfMod$Date <- NULL
        dfMod$ID <- NULL
        dfMod$Nbl <- NULL
        dfMod$bl <- as.numeric(as.character(dfMod$bl))
        dfMod$Gender <- as.factor(as.character(dfMod$Gender))
        dfMod$Age <- as.numeric(dfMod$Age)
        
        for (i in 1:ncol(dfMod)){dfMod[,i][is.infinite(dfMod[,i])] = NA}
        
        
        for (i in 1:ncol(dfMod))
          dfMod[is.na(dfMod[,i]), i] <- mean(dfMod[,i], na.rm = TRUE)
        
        
        dfMod <- dfMod[complete.cases(dfMod),]}
      meanvec <- vector()
      globals$test <- vector()
      globals$predbl <- vector()
      for (f in 1:5)
      {    set.seed(123)
        if(input$featex == "Frequent Pattern Mining")
        {
          patternset <- complete.patternset
          split <- (as.integer(length(unique(patternset$ID))/5))
          max <- f * split
          min <- max - split
          test <- seq(min:max)
          train <- patternset[!patternset$ID %in% test,]
          train <- train[order(train[,1], train[,2]),]
          
          patternset <- train
          patternset1 <- patternset[which(patternset$bl == 1),] # one dataset with only bleedings
          patternset2 <- patternset[which(patternset$bl == 0),] # one dataset without any bleedings
          patternset2$ID <- as.numeric(as.character(patternset2$ID))
          
          windowfreq <- as.data.frame(table(patternset2$ID))
          frequentwindows <- windowfreq[which(windowfreq$Freq > 1), 1]
          ncomp <- (sum(patternset1$pattern == "bleeding")*1)
          vec <- sample(frequentwindows, ncomp) # select ncomp random ID's to create non bleeding patterns
          patternset2 <- patternset2[patternset2$ID %in% vec,]
          
          patterns <- rbind(patternset2, patternset1)
          patterns$event <- as.numeric(as.character(patterns$event))
          patterns$ID <- as.numeric(as.character(patterns$ID))
          patterns <- patterns[order(patterns[,1], patterns[,2]),]
          
          patternset <- patterns
          patternset$Pseudo_ID <- patternset$ID
          patternset$ID <- NULL
          patternset$classwin <- NULL
          patternset$Pseudo_ID <- as.numeric(as.character(patternset$Pseudo_ID))
          patternset$event <- as.numeric(as.character(patternset$event))
          patternset$age <- NULL
          patternset$gender <- NULL
          patternset$Days <- NULL
          patternset$bl <- NULL
          colnames(patternset) <- c("ID", "event", "size", "pattern")
          patternset <- patternset[order(patternset[,1], patternset[,2]),]
          
          write.table(patternset, "C:\\patterns.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
          data <- read_baskets(con = "C:\\patterns.txt", info = c("sequenceID","eventID","SIZE"))
          transactionInfo(data)$eventID <- as.numeric(as.character(transactionInfo(data)$eventID))
          s1 <- cspade(data, parameter = list(support = 0.10), control = list(verbose = TRUE, numpart=1))
          r1<-ruleInduction(s1, confidence = 0.75)
          finalSeq <- subset(r1, rhs(x) %in% c("bleeding", "no_bleeding"))
          red <- is.redundant(finalSeq, measure = "lift")
          finalSeq1 <- finalSeq[!red]
          selRules <<-  as(finalSeq1, "data.frame")
          test1 <- as(finalSeq1, "data.frame")
          test1$rule <- as.character(test1$rule)
          testt1 <- test1
          testt2 <- testt1[grepl("no_bleeding", testt1$rule),]
          testt3 <- testt1[(!grepl("no_bleeding", testt1$rule)),]
          testt2 <- testt2[order(-testt2$confidence),]
          if (nrow(testt2) > 25)
            testt2 <- testt2[1:25,]
          testt3 <- testt3[order(-testt3$confidence),]
          if (nrow(testt3) > 25)
            testt3 <- testt3[1:25,]
          testt11 <- rbind(testt2, testt3)
          for (i in 1:nrow(testt11)){
            testt11[i,1] <- gsub(" => <{bleeding}>", "", testt11[i,1], fixed = TRUE)
            testt11[i,1] <- gsub(" => <{no_bleeding}>", "", testt11[i,1], fixed = TRUE)}
          
          
          predictive_patterns <- testt11
          patternset <- complete.patternset
          patternset$event <- as.numeric(as.character(patternset$event))
          patternset$ID <- as.numeric(as.character(patternset$ID))
          
          patternset<- patternset[which(patternset$pattern != "bleeding"),] # exclude bleeding indicators
          patternset<- patternset[which(patternset$pattern != "no_bleeding"),] 
          df_ftpm <- data.frame()
          occvec <- ""
          ids <- unique(patternset$ID) 
          imax <- length(ids)
          pb <- txtProgressBar(min = 0, max = imax, style = 3)
          for (i in 1:length(ids))
          {selectdf <- patternset[patternset$ID == ids[i],] # create df of patternset of that patient
          setTxtProgressBar(pb, i)
          occurence  <- ""
          occvec <- vector()
          for (k in 1:nrow(predictive_patterns)) # for every pattern
          {  
            pattern <- predictive_patterns[k,1]
            patient.patterns <-  selectdf  
            test <- as.list(str_extract_all(string = pattern, pattern = "\\{.*?\\}")[[1]])
            test <- as.list(str_replace_all(string = (test), pattern = "\\{|\\}", replacement = ""))
            pattern1 <- as.list(str_replace_all(string = (test), pattern = ",", replacement = " "))
            
            check <- 1
            occurence <- 1
            for (k in 1:length(pattern1))
            {if (occurence == 1){ 
              str <- pattern1[[k]]
              str <- unlist(strsplit(str, " "))
              if (nrow(patient.patterns) == check)
              {x <- ifelse(test = all(sapply(str, grepl, patient.patterns[1,5]))== TRUE, yes = 1, no = 0) }
              if (nrow(patient.patterns) > check)
              {x <- ifelse(test = apply(sapply(str, grepl, patient.patterns[(check:nrow(patient.patterns)),5]), 1, all) == TRUE, yes =  1, no = 0)}
              if (!(1 %in% x))
              {occurence <- 0
              break}
              if (1 %in% x)
              {check <- check + which.max(x)
              occurence <- 1}
            }}
            
            occvec <- c(occvec, as.character(occurence))}
          rij <- c(as.character(selectdf[1,10]),as.character(selectdf[1,1]), as.character(selectdf[nrow(selectdf),2]), occvec, as.character(selectdf[1,4]), as.character(selectdf[1,7]),as.character(selectdf[1,8]),as.character(selectdf[1,9]))
          rij <- data.frame(t(rij))
          df_ftpm <- rbind(df_ftpm, rij)}
          varna <- vector()
          
          for(i in 1:nrow(predictive_patterns))
          {x <- paste("pattern", i, sep = "")
          varna <- c(varna, x)}
          colnames(df_ftpm)<- c("ID","Pseudo_ID", "EventID", varna, "bl", "days", "gender", "age")
          
          df_ftpm$age <- as.numeric(as.character(df_ftpm$age))
          df_ftpm$days <- as.numeric(as.character(df_ftpm$days))
          df_ftpm$gender <- as.factor(df_ftpm$gender)
          df_ftpm$ID <- NULL
          df_ftpm$Pseudo_ID <- NULL
          df_ftpm$EventID <- NULL
          dff <<- df_ftpm
          dfMod <- df_ftpm
        }
        
        
        min <- as.integer((nrow(dfMod)/5)*f-(nrow(dfMod)/5))
        max <- as.integer((nrow(dfMod)/5)*f)
        print(min)
        print(max)
        trainn <- dfMod[-(min:max),]
        test <- dfMod[min:max,]
        print(table(trainn$bl))
        
        
        
        if(input$featsel == "Boruta")
        {boruta.trainn <- Boruta(bl~., data = trainn, doTrace = 2)
        boruta.ATT <- TentativeRoughFix(boruta.trainn)
        cols <- getSelectedAttributes(boruta.ATT, withTentative = F)
        trainn <- trainn[,c(cols, "bl")]
        test <- test[,c(cols, "bl")]
        print(names(trainn))
        globals$selVars <- cols
        globals$Boruta <- boruta.ATT}
        
        if(input$featsel == "Recursive Feature Elimination")
        {control <- rfeControl(functions=rfFuncs, method="cv", number=5)
        rfe.trainn <- rfe(trainn[, -which(names(trainn) %in% c("bl"))], trainn$bl , rfeControl=control)
        globals$rfeSel <- rfe.trainn
        trainn <- trainn[,c("bl", rfe.trainn$optVariables)]
        test <- test[,c("bl", rfe.trainn$optVariables)]
        globals$selVars <- rfe.trainn$optVariables}
        trainn[,(sapply(trainn, function(x) length(levels(x))) == 1)] <- NULL
        
        if(input$featsel == "Filter")
        {df <- trainn
        cor_fun <- function(pos_1, pos_2){
          
          # both are numeric
          if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
             class(df[[pos_2]]) %in% c("integer", "numeric")){
            r <- stats::cor(df[[pos_1]]
                            , df[[pos_2]]
                            , use = "pairwise.complete.obs"
            )
          }
          
          # one is numeric and other is a factor/character
          if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
             class(df[[pos_2]]) %in% c("factor", "character")){
            r <- sqrt(
              summary(
                stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
          }
          
          if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
             class(df[[pos_1]]) %in% c("factor", "character")){
            r <- sqrt(
              summary(
                stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
          }
          
          # both are factor/character
          if(class(df[[pos_1]]) %in% c("factor", "character") &&
             class(df[[pos_2]]) %in% c("factor", "character")){
            r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
          }
          
          return(r)
        } 
        
        cor_fun <- Vectorize(cor_fun)
        
        # now compute corr matrix
        cormat <- outer(1:ncol(df)
                        , 1:ncol(df)
                        , function(x, y) cor_fun(x, y)
        )
        
        cormat <- as.data.frame(cormat)
        rownames(cormat) <- colnames(df)
        colnames(cormat) <- colnames(df)
        cormat$bl <- abs(cormat$bl)
        cormat <- cormat[order(-cormat$bl),]
        SelVar <- rownames(cormat)[2:16]
        globals$selVars <- SelVar
        print(SelVar)
        trainn <- trainn[,c("bl", SelVar)]
        test <- test[,c("bl", SelVar)]
        print("Done")}
        
        if(input$featsel == "Rank by Importance (RandomForest)")
        {SelVar <- varSelRF( trainn[, -which(names(trainn) %in% c( "bl"))], as.factor(trainn$bl), ntree = 500)
        trainn <- trainn[,c("bl", SelVar$selected.vars)]
        test <- test[,c("bl", SelVar$selected.vars)]
        x <- (SelVar$selected.vars)
        globals$selVars <- SelVar$selected.vars
        }
        
        set.seed(input$seed)
        
        if(input$datasam == "Under")
        {
          x =ubUnder(trainn[, -which(names(trainn) %in% c( "bl"))], trainn$bl)
          trainn <- cbind(x$X,bl = x$Y)
        }
        if(input$datasam == "Over")
        {  
          x =ubOver(trainn[, -which(names(trainn) %in% c( "bl"))], trainn$bl)
          trainn <- cbind(x$X,bl = x$Y)
        }
        if(input$datasam == "Rose")  
        {trainn <- ROSE(bl ~ ., data = trainn, seed = 1)$data
        trainn[,(sapply(trainn, function(x) length(levels(x))) == 1)] <- NULL}
        set.seed(123)
        globals$test <- c(globals$test, test$bl)
        
        if(input$algos == "RandomForest")
        { print("go")
          mod1 <- randomForest(as.factor(bl) ~., data = trainn, ntree = 1000, mtry =5, seed = 123)
          pred <- predict(mod1, newdata = test, type = "prob")
          
          print("Random Forest does")
          print(roc(test$bl, pred[,1]))
          x = roc(test$bl, pred[,1])
          meanvec <- c(meanvec, x$auc)
          globals$predbl <-  c(globals$predbl, pred[,2])
        }
        if(input$algos == "NaiveBayes")
        {model <- naiveBayes(as.factor(bl) ~., data = trainn)
        pred <- predict(model, newdata = remove_missing_levels( fit= model, test_data =  test), type = "raw")
        print("NaiveBayes does")
        print(roc(test$bl, pred[,1]))
        x = roc(test$bl, pred[,1])
        meanvec <- c(meanvec, x$auc)
        globals$predbl <-  c(globals$predbl, pred[,2])
        }
        
        if(input$algos == "Logistic Regression")
        {model <- glm(as.factor(bl) ~.,family=binomial(link='logit'),data=trainn)
        pred <- predict(model, newdata = remove_missing_levels( fit= model, test_data =  test), type = "response")
        print(roc(test$bl, pred))
        x = roc(test$bl, pred)
        meanvec <- c(meanvec, x$auc)
        globals$predbl <-  c(globals$predbl, pred)
        }
        
        if(input$algos == "Decision Tree")      
        {modDT <- rpart(as.factor(bl)  ~., data = trainn, method= "class")
        pred <- predict(modDT, newdata = remove_missing_levels( fit= modDT, test_data =  test), type = "prob")
        print(roc(test$bl, pred[,1]))
        x = roc(test$bl, pred[,1])
        meanvec <- c(meanvec, x$auc)
        globals$predbl <-  c(globals$predbl, pred[,2])
        }
        
        if(input$algos == "Support Vector Machine")
        {  
          model_svm <- svm(as.factor(bl) ~. , trainn ,probability=TRUE)
          pred <- predict(model_svm, test,  probability=TRUE)
          print(roc(test$bl, attr(pred, "probabilities")[,1]))
          pred <- attr(pred, "probabilities")
          x = roc(test$bl, pred[,1])
          meanvec <- c(meanvec, x$auc)
          globals$predbl <-  c(globals$predbl, pred[,2])
        }
        
        if(input$algos == "C5.0")
        { fit <- C5.0(as.factor(bl)~., data=trainn)
        pred <- predict(fit, test, type= "prob")
        print(roc(test$bl, pred[,1]))
        x = roc(test$bl, pred[,1])
        meanvec <- c(meanvec, x$auc)
        globals$predbl <-  c(globals$predbl, pred[,2])
        
        }
        incProgress(1/5, detail = paste((f*20),"% Completed"))
        
        globals$testbl <- test$bl
      }
      globals$mean <- paste("Average AUC after 5 iterations is",  mean(meanvec))
    })
  })
  
  output$roccurve <- renderPlot({
    
    p <- roc.curve(globals$test, globals$predbl)
    p
  })
  
  output$rocinfo1 <- renderPrint({
    predClass1 <-(ifelse(globals$predbl >  input$thresh,  1,0))
    print(confusionMatrix(as.factor(predClass1),as.factor(globals$test), positive =  "1"))
    
  })
  
   output$varIMP <- renderTable({
     t <- data.frame(Rank = seq(1:length(globals$selVars)) ,Selected_Variables = globals$selVars)
     t
   })
    
   output$Tree <- renderPlot({
     p <- globals$fitt
     fancyRpartPlot(p)
   })                            
                               
   output$raw_summary <- renderPrint({
     d <- globals$dff
     model <- glm(as.factor(bl) ~.,family=binomial(link='logit'),data=d)
     print(summary(model))
   })
   
   output$rocinfo <- renderText({
     globals$mean
   })

   observeEvent(input$finMod,{
     if (input$demo == TRUE)
     {
       if (input$classSel == "Baseline Prediction") 
       {if (input$featex == "Last Values")
       {dfMod <- Demo_BaseLastValues}
         if (input$featex == "Summary")
         {dfMod <- Demo_BaseSum}
         if (input$featex == "Statistics")
         {dfMod <- Demo_BaseStat}
         if (input$featex == "Trend Value Abstraction")
         {dfMod <- Demo_BaseTVA}}
       
       if (input$classSel == "Time serie prediction")
       {if (input$featex == "Last Values")
       {
         
         if(input$dataimp == "Mean")
         {df_prep <- DemoLastvalues}
         if (input$dataimp == "Mice" )
         {df_prep <- DemoLastvaluesMice}
         if (input$dataimp == "Amelia")
         {df_prep <- DemoLastvaluesAm
         print("yes")}
         if (input$dataimp == "MissForest")
         {df_prep <- DemoLastvaluesMis}}
         
         
         if (input$featex == "Summary")
         {df_prep <- DemoSum
         if(input$dataimp == "Mean")
         {df_prep <- DemoSum}
         if (input$dataimp == "Mice" )
         {df_prep <- DemoSumMice}
         if (input$dataimp == "Amelia")
         {df_prep <- DemoSumAm
         print("yes")}
         if (input$dataimp == "MissForest")
         {df_prep <- DemoSumMis}}
         
         if (input$featex == "Statistics")
         {df_prep <- DemoStat}
         if (input$featex == "Trend Value Abstraction")
         {df_prep <- DemoTVA}}}
       df_prep <- dff
         x<-0
         df_prep$ID <- seq(1:nrow(df_prep))
         df_prep$Nbl <- 0
         df_prep$bl <- 0
         IDvec1 <- vector()
         for(i in 1: nrow(comp_data)){
           print(i)
           patient1 <- df_prep[grep(comp_data[i,1], df_prep[,1]),]
           patient11 <- patient1
           ddate <- comp_data[i,Comp_date]
           compmin.date <- ddate- input$classmin
           compmax.date <- ddate-input$classmax
           compmax.del <- compmin.date+input$delvec
           compmin.del <- compmin.date+1
           patient1 <- patient1[which(patient1$Date >= compmax.date),]
           patient1 <- patient1[which(patient1$Date <= compmin.date),]
           patient11 <- patient11[which(patient11$Date <= compmax.del),]
           patient11 <- patient11[which(patient11$Date >= compmin.del),]
           IDvec <- patient1$ID
           IDvec1 <- c(IDvec1, patient11$ID)
           
           if(length(IDvec) > 0)
           {x<- x+1
           df_prep$bl[df_prep$ID %in% IDvec] <- 1
           print("blood")
           df_prep$Nbl[df_prep$ID %in% IDvec] <- x}
         }
         if (input$delvec>0)
         {df_prep <- df_prep[!df_prep$ID %in% IDvec1,]}
         df_prep$Nbl <- as.numeric(df_prep$Nbl)
         df_prep <- df_prep[order(df_prep$Date),]
         dfMod <- df_prep
         
       
     
    # dfMod <- dff
     print("work")
     if (input$classSel == "Time serie prediction")
       dfMod$daysPat <- as.numeric(as.character(dfMod$daysPat))
     dfMod$Patient_ID <- NULL
     dfMod$Date <- NULL
     dfMod$ID <- NULL
     dfMod$Nbl <- NULL
     dfMod$bl <- as.numeric(as.character(dfMod$bl))
     dfMod$Gender <- as.factor(as.character(dfMod$Gender))
     dfMod$Age <- as.numeric(dfMod$Age)
     print ("yes")
     
     for (i in 1:ncol(dfMod)){dfMod[,i][is.infinite(dfMod[,i])] = NA}
     
     
     for (i in 1:ncol(dfMod))
       dfMod[is.na(dfMod[,i]), i] <- mean(dfMod[,i], na.rm = TRUE)
     
     
     dfMod <- dfMod[complete.cases(dfMod),]
     
     if(input$featsel == "Boruta")
     {boruta.dfMod <- Boruta(bl~., data = dfMod, doTrace = 2)
     boruta.ATT <- TentativeRoughFix(boruta.dfMod)
     cols <- getSelectedAttributes(boruta.ATT, withTentative = F)
     dfMod <- dfMod[,c(cols, "bl")]
     test <- test[,c(cols, "bl")]
     print(names(dfMod))
     globals$selVars <- cols
     globals$Boruta <- boruta.ATT}
     
     if(input$featsel == "Recursive Feature Elimination")
     {control <- rfeControl(functions=rfFuncs, method="cv", number=5)
     rfe.dfMod <- rfe(dfMod[, -which(names(dfMod) %in% c("bl"))], dfMod$bl , rfeControl=control)
     globals$rfeSel <- rfe.dfMod
     dfMod <- dfMod[,c("bl", rfe.dfMod$optVariables)]
     test <- test[,c("bl", rfe.dfMod$optVariables)]
     globals$selVars <- rfe.dfMod$optVariables}
     dfMod[,(sapply(dfMod, function(x) length(levels(x))) == 1)] <- NULL
     
     if(input$featsel == "Filter")
     {cormat <- as.data.frame(cor2(dfMod))
     cormat <- cormat[,order(cormat$bl)]
     SelVar <- rownames(cormat)[1:15]
     globals$selVars <- SelVar
     print(SelVar)
     dfMod <- dfMod[,c("bl", SelVar)]
     test <- test[,c("bl", SelVar)]
     print("Done")}
     
     if(input$featsel == "Rank by Importance (RandomForest)")
     {SelVar <- varSelRF( dfMod[, -which(names(dfMod) %in% c( "bl"))], as.factor(dfMod$bl), ntree = 500)
     dfMod <- dfMod[,c("bl", SelVar$selected.vars)]
     test <- test[,c("bl", SelVar$selected.vars)]
     x <- (SelVar$selected.vars)
     globals$selVars <- SelVar$selected.vars
     }
     
     set.seed(input$seed)
     
     if(input$datasam == "Under")
     {  x =ubUnder(dfMod[, -which(names(dfMod) %in% c( "bl"))], dfMod$bl)
     dfMod <- cbind(x$X,bl = x$Y)
     }
     if(input$datasam == "Over")
     {  
       x =ubOver(dfMod[, -which(names(dfMod) %in% c( "bl"))], dfMod$bl)
       dfMod <- cbind(x$X,bl = x$Y)
     }
     if(input$datasam == "Rose")  
     {dfMod <- ROSE(bl ~ ., data = dfMod, seed = 1)$data
     dfMod[,(sapply(dfMod, function(x) length(levels(x))) == 1)] <- NULL}
     
     
     if(input$algos == "RandomForest")
     { finalModel <- randomForest(as.factor(bl) ~., data = dfMod, ntree = 1000, mtry =5, seed= input$seed)
       finmod <<- finalModel
     }
     if(input$algos == "NaiveBayes")
     {finalModel <- naiveBayes(as.factor(bl) ~., data = dfMod)}
     if(input$algos == "Logistic Regression")
     {finalModel <- glm(as.factor(bl) ~.,family=binomial(link='logit'),data=dfMod)
     globals$dff <- dfMod}
     if(input$algos == "Decision Tree")      
     {finalModel <- rpart(as.factor(bl)  ~., data = dfMod, method= "class")
     globals$fitt <- finalModel}
     
     globals$FullRoute$DI <- input$dataimp
     globals$FullRoute$FE <- input$featex
     globals$FullRoute$FS <- input$featsel
     globals$FullRoute$DS <- input$datasam
     globals$FullRoute$AL <- input$algos
     
     fullRoute <- list()
     fullRoute$classmin <- input$classmin
     fullRoute$classmax <- input$classmax
     fullRoute$BaseWindow <- input$classBaseWindow
     fullRoute$DI <- input$dataimp
     fullRoute$FE <- input$featex
     fullRoute$FS <- input$featsel
     fullRoute$DS <- input$datasam
     fullRoute$AL <- input$algos
   })
   
     
   output$compfin <- renderText({ 
     "Your final model is has been build on the complete dataset and is saved as 'finalModel' in your global environment. All chosen settings have been saved as 'fullRoute' in your global environment, I you wish you can evaluate this model on new data by loading the data below. In case you would like to use the model later, make you save your global environment." })
   
   observeEvent(input$loadTPN, {
     if(is.null(input$TPDN) == FALSE)
     {    TP_dataN <<- read.csv(input$TPDN$datapath,
                                header = input$headerTPN,
                                sep = input$sepTPN)
     namecolTPN <<- names(TP_dataN)
     updateSelectInput(session, "ITPN_ID",
                       label = paste("Select ID column"),
                       choices = namecolTPN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITPN_Date",
                       label = paste("Select Date of Timepoint Column"),
                       choices = namecolTPN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITPN_Time",
                       label = paste("Select Time of Timepoint Column (optional)"),
                       choices = namecolTPN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITPN_Type",
                       label = paste("Select Type column"),
                       choices = namecolTPN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITPN_Outcome",
                       label = paste("Select Outcome column"),
                       choices = namecolTPN,
                       selected = tail(1, 1))
     
     TPDDateN <<- 1
     TPsetsN <<- 1}
     
   })
   
   observeEvent(input$loadTIN, {
     if(is.null(input$TIDN) == FALSE)
     {TI_dataN <<- read.csv(input$TIDN$datapath,
                            header = input$headerTIN,
                            sep = input$sepTIN)
     namecolTIN <<- names(TI_dataN)
     updateSelectInput(session, "ITIN_ID",
                       label = paste("Select ID column"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITIN_SD",
                       label = paste("Select Start date column"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITIN_ED",
                       label = paste("Select End date column"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITIN_EDD",
                       label = paste("Select End Date 2 Column (optional)"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITIN_Type1",
                       label = paste("Select Type1 column"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     updateSelectInput(session, "ITIN_Type2",
                       label = paste("Select Type2 column (optional)"),
                       choices = namecolTIN,
                       selected = tail(1, 1))
     TIDDateN <<- 1
     TIsetsN <<- 1}
   })
   
   observeEvent(input$loadcompN, {
     
     if(is.null(input$COMPDN) == FALSE)
     {
       comp_dataN <<- read.csv(input$COMPDN$datapath,
                              header = input$headerCompN,
                              sep = input$sepCompN)
       namecolCompN <<- names(comp_dataN)
       
       updateSelectInput(session, "ICOMP_IDN",
                         label = paste("Select ID column"),
                         choices = namecolCompN,
                         selected = tail(1, 1))
       updateSelectInput(session, "ICOMP_DateN",
                         label = paste("Select Date Column"),
                         choices = namecolCompN,
                         selected = tail(1, 1))
       updateSelectInput(session, "ICOMP_NDaysN",
                         label = paste("Select intervention days Column"),
                         choices = namecolCompN,
                         selected = tail(1, 1))
       updateSelectInput(session, "ICOMP_TypeN",
                         label = paste("Select Type Column"),
                         choices = namecolCompN,
                         selected = tail(1, 1))
       compDDateN <<- 1
       CompsetsN <<- 1
     }
   })
   
   observeEvent(input$loadBaseN, {
     if(is.null(input$BASED) == FALSE)
     {base_dataN <<- read.csv(input$BASEDN$datapath,
                             header = input$headerBaseN,
                             sep = input$sepBaseN)
     namecolBaseN <<- names(base_dataN)
     updateSelectInput(session, "IBASE_IDN",
                       label = paste("Select ID column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     updateSelectInput(session, "IBASE_StartN",
                       label = paste("Select Start Column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     updateSelectInput(session, "IBASE_EndN",
                       label = paste("Select End Column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     updateSelectInput(session, "IBASE_AgeN",
                       label = paste("Select Age Column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     updateSelectInput(session, "IBASE_GenderN",
                       label = paste("Select Gender Column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     updateSelectInput(session, "IBASE_MortN",
                       label = paste("Select Mortality date Column"),
                       choices = namecolBaseN,
                       selected = tail(1, 1))
     baseDDateN <<- 1
     BasesetsN <<- 1}
   })
  
}