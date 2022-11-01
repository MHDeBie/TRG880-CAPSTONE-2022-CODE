############################################
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

# Import libraries
library(shiny)
library(data.table)
library(randomForest)
library(gbm)
library(shinydashboard)

# Read in the model
load("C:/Users/Latitude 5580/OneDrive/Documents/1 UNI STUFF/TRG880/TRG_ASG7 CAPSTONE/APPLICATION/randomForest.rda")
load("C:/Users/Latitude 5580/OneDrive/Documents/1 UNI STUFF/TRG880/TRG_ASG7 CAPSTONE/APPLICATION/gbm.rda")


####################################
# User interface                   #
####################################

ui <- dashboardPage(
  
  # Page header
  dashboardHeader(title='Insurance Predictor'),
  
  # Input values
  dashboardSidebar(
    #HTML("<h3>Input parameters</h3>"),
    #tags$label(h3('Please enter the following')),
    tags$head(
      tags$style(
        HTML(
          "
          .form-group {
            margin-bottom: 0;
          }
        "
        )
      )
    ),
    dateInput("STATUS_DATE", 
                 label = "Status Date", 
                 value = "2008-09-01"),
    
    dateInput("COMMENCEMENT_DATE", 
                 label = "Commencement Date", 
                 value = "2008-09-01"),
    
    numericInput("PAYPOINT_NAME", 
                 label = "Paypoint Name", 
                 min=1,
                 max=428,
                 value = 21),
    
    numericInput("INCOME", 
                 label = "Income",
                 min=0,
                 value = 8415),
    
    selectInput("PAYMENT_MODE", 
                 label = "Payement Mode", 
                  choices = c("ADD","ASO","DSO","RSO"),
                 selected = "ADD"),
    
    numericInput("PREMIUM", 
                 label = "Premium", 
                 min=0,
                 value = 387.32),
    
    dateInput("PAYER_DATE_OF_BIRTH", 
                 label = "Payer Date of Birth", 
                 value = "1950-12-25"),
    
    dateInput("INSURED_DATE_OF_BIRTH", 
                 label = "Insured Date of Birth", 
                 value = "1950-12-25"),
    
    numericInput("TERM", 
                 label = "Term", 
                 min=0,
                 max=99,
                 value = 10),
    
    selectInput("PRODUCT_CODE", label = "Product Code", 
                choices = list("A1"="A1","A2"="A2","A3"="A3","A4"="A4",
                               "A5"="A5","A6"="A6","A7"="A7","A8"="A8",
                               "A9"="A9","A10"="A10","A11"="A11","A12"="A12",
                               "A13"="A13","A14"="A14","A15"="A15","A16"="A16",
                               "A17"="A17","A18"="A18","A19"="A19","A20"="A20",
                               "A21"="A21","A22"="A22","A23"="A23","A24"="24",
                               "A25"="A25","A26"="A26","A27"="A27"), 
                selected = "A23"),
    
        actionButton("submitbutton", "Calculate probabilities", 
                 class = "btn btn-primary")
  ),
  
  dashboardBody(
    tags$label('Results'), # Status/Output Text Box
    verbatimTextOutput('contents'),
    box(title = "Prediction of multi-class case",status = "primary",
        tableOutput('tabledata1'),height = 120, width = 400),
    box(title = "Pie chart of Status",status = "primary",
        plotOutput('plot1'),height = 180, width = 400),
    box(title = "Prediction of binary case",status = "primary",
    tableOutput('tabledata2'),height = 120, width = 400),
    box(title = "Pie chart of Retention",status = "primary",
    plotOutput('plot2'),height = 180, width = 400)
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  dataset1 <- reactive({  
    
    df <- data.frame(
      STATUS_DATE=as.Date(input$STATUS_DATE) ,
        COMMENCEMENT_DATE=as.Date(input$COMMENCEMENT_DATE) ,
        PAYPOINT_NAME=as.integer(input$PAYPOINT_NAME) ,
        INCOME=as.double(input$INCOME) ,
        PAYMENT_MODE=factor(input$PAYMENT_MODE,
                            levels = c("ADD","ASO","DSO","RSO")) ,
        PREMIUM=as.double(input$PREMIUM) ,
        PAYER_DATE_OF_BIRTH=as.Date(input$PAYER_DATE_OF_BIRTH) ,
        INSURED_DATE_OF_BIRTH=as.Date(input$INSURED_DATE_OF_BIRTH) ,
        TERM=as.integer(input$TERM) ,
        PRODUCT_CODE=factor(input$PRODUCT_CODE,
                            levels = c(paste("A",1:27,sep = ""))))
    
    class <- predict(model,df,type = "class")
    if(class=="ACT"){
      class <- "Active"
    }else if(class=="CAN"){
      class <- "Cancelled"
    }else if(class=="LAP"){
      class <- "Lapsed"
    }else{
      class <- "Surrendered"
    }
    pred <- predict(model,df,type = "prob")
    colnames(pred) <- c("Active","Cancelled","Lapsed","Surrendered")
    Table <- data.frame(Prediction=class,pred)
  })
  
  dataset2 <- reactive({
    df2 <- data.frame(
      STATUS_DATE=as.numeric(input$STATUS_DATE-as.Date("1900-01-01")) ,
      COMMENCEMENT_DATE=as.numeric(input$COMMENCEMENT_DATE-
                                     as.Date("1900-01-01")) ,
      PAYPOINT_NAME=as.integer(input$PAYPOINT_NAME) ,
      INCOME=as.double(input$INCOME) ,
      PAYMENT_MODE=as.numeric(factor(input$PAYMENT_MODE,
                          levels = c("ADD","ASO","DSO","RSO")))-1 ,
      PREMIUM=as.double(input$PREMIUM) ,
      PAYER_DATE_OF_BIRTH=as.numeric(input$PAYER_DATE_OF_BIRTH-
                                       as.Date("1900-01-01")) ,
      INSURED_DATE_OF_BIRTH=as.numeric(input$INSURED_DATE_OF_BIRTH-
                                         as.Date("1900-01-01")) ,
      TERM=as.integer(input$TERM) ,
      PRODUCT_CODE=as.numeric(factor(input$PRODUCT_CODE,
                          levels = c(paste("A",1:27,sep = ""))))-1)

    pred <- predict.gbm(object = model_gbm, newdata = df2, n.trees = 500,
                         type = "response")
    class <- ifelse(pred>0.5,1,0)
    retain <- ifelse(class==0,"Retained","Not retained")
    prob <- matrix(c(1-pred,pred),nrow = 1)
    colnames(prob) <- c("Retained","Not retained")
    Table <- data.frame(Prediction=retain,prob)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      validate(
        # need(input$STATUS_DATE != '','Specify a Status Date'),
        # need(input$COMMENCEMENT_DATE != '','Specify a Commencement Date'),
        need(input$PAYPOINT_NAME != '','Specify a Paypoint Name'),
        need(input$INCOME != '','Specify an Income Amount'),
        need(input$PAYMENT_MODE != '','Choose a Payment Method'),
        need(input$PREMIUM != '','Specify a Premium Amount'),
        # need(input$PAYER_DATE_OF_BIRTH != '','Specify the Payer Date of Birth'),
        # need(input$INSURED_DATE_OF_BIRTH != '',
        #      'Specify the Insured Date of birth'),
        need(input$TERM != '','Specify a Term'),
        need(input$PRODUCT_CODE != '','Choose a Product Code'),
        need(input$COMMENCEMENT_DATE<=input$STATUS_DATE,'Check Status Date'),
        need(input$COMMENCEMENT_DATE>input$PAYER_DATE_OF_BIRTH,
             'Check Payer Date of Birth'),
        need(input$COMMENCEMENT_DATE>input$INSURED_DATE_OF_BIRTH,
             'Check Insured Date of Birth'),
        need(input$PAYPOINT_NAME>=1 & input$PAYPOINT_NAME<=428,
             'Paypoint Name must be between 1 and 428'),
        need(input$INCOME>=0,'Income must be positive'),
        need(input$PREMIUM>=0,'Premium must be positive'),
        need(input$TERM>=0 & input$TERM<=99,
             'Term must be between 0 and 99')
      )
    }
  })
  
  # Prediction results table
  output$tabledata1 <- renderTable({
    if (input$submitbutton>0) { 
      validate(
        # need(input$STATUS_DATE != '',''),
        # need(input$COMMENCEMENT_DATE != '',''),
        need(input$PAYPOINT_NAME != '',''),
        need(input$INCOME != '',''),
        need(input$PAYMENT_MODE != '',''),
        need(input$PREMIUM != '',''),
        # need(input$PAYER_DATE_OF_BIRTH != '',''),
        # need(input$INSURED_DATE_OF_BIRTH != '',
        #      ''),
        need(input$TERM != '',''),
        need(input$PRODUCT_CODE != '',''),
        need(input$COMMENCEMENT_DATE<=input$STATUS_DATE,''),
        need(input$COMMENCEMENT_DATE>input$PAYER_DATE_OF_BIRTH,
             ''),
        need(input$COMMENCEMENT_DATE>input$INSURED_DATE_OF_BIRTH,
             ''),
        need(input$PAYPOINT_NAME>=1 & input$PAYPOINT_NAME<=428,
             ''),
        need(input$INCOME>=0,''),
        need(input$PREMIUM>=0,''),
        need(input$TERM>=0 & input$TERM<=99,
             '')
      )
      t <- dataset1()
      par(mar = rep(0,4))
      print(t)
    } 
  })
  
  output$plot1 <- renderPlot({
    if (input$submitbutton>0) {
      validate(
        # need(input$STATUS_DATE != '',''),
        # need(input$COMMENCEMENT_DATE != '',''),
        need(input$PAYPOINT_NAME != '',''),
        need(input$INCOME != '',''),
        need(input$PAYMENT_MODE != '',''),
        need(input$PREMIUM != '',''),
        # need(input$PAYER_DATE_OF_BIRTH != '',''),
        # need(input$INSURED_DATE_OF_BIRTH != '',
        #      ''),
        need(input$TERM != '',''),
        need(input$PRODUCT_CODE != '',''),
        need(input$COMMENCEMENT_DATE<=input$STATUS_DATE,''),
        need(input$COMMENCEMENT_DATE>input$PAYER_DATE_OF_BIRTH,
             ''),
        need(input$COMMENCEMENT_DATE>input$INSURED_DATE_OF_BIRTH,
             ''),
        need(input$PAYPOINT_NAME>=1 & input$PAYPOINT_NAME<=428,
             ''),
        need(input$INCOME>=0,''),
        need(input$PREMIUM>=0,''),
        need(input$TERM>=0 & input$TERM<=99,
             '')
      )
      p <- dataset1()
      p <- p[,-1]
      p <- as.matrix(p)
      par(mar = rep(0,4))
      pie(p,labels = paste(p*100,"%",sep = ""),
          col=rainbow(length(p)))
      legend("topright",colnames(p),cex = 0.8,
             fill = rainbow(length(p)))
    }
  },height = 110, width = 400)
  
  output$tabledata2 <- renderTable({
    if (input$submitbutton>0) {
      validate(
        # need(input$STATUS_DATE != '',''),
        # need(input$COMMENCEMENT_DATE != '',''),
        need(input$PAYPOINT_NAME != '',''),
        need(input$INCOME != '',''),
        need(input$PAYMENT_MODE != '',''),
        need(input$PREMIUM != '',''),
        # need(input$PAYER_DATE_OF_BIRTH != '',''),
        # need(input$INSURED_DATE_OF_BIRTH != '',
        #      ''),
        need(input$TERM != '',''),
        need(input$PRODUCT_CODE != '',''),
        need(input$COMMENCEMENT_DATE<=input$STATUS_DATE,''),
        need(input$COMMENCEMENT_DATE>input$PAYER_DATE_OF_BIRTH,
             ''),
        need(input$COMMENCEMENT_DATE>input$INSURED_DATE_OF_BIRTH,
             ''),
        need(input$PAYPOINT_NAME>=1 & input$PAYPOINT_NAME<=428,
             ''),
        need(input$INCOME>=0,''),
        need(input$PREMIUM>=0,''),
        need(input$TERM>=0 & input$TERM<=99,
             '')
      )
      t <- dataset2()
      print(t)
    }
  })

  output$plot2 <- renderPlot({
    if (input$submitbutton>0) {
      validate(
        # need(input$STATUS_DATE != '',''),
        # need(input$COMMENCEMENT_DATE != '',''),
        need(input$PAYPOINT_NAME != '',''),
        need(input$INCOME != '',''),
        need(input$PAYMENT_MODE != '',''),
        need(input$PREMIUM != '',''),
        # need(input$PAYER_DATE_OF_BIRTH != '',''),
        # need(input$INSURED_DATE_OF_BIRTH != '',
        #      ''),
        need(input$TERM != '',''),
        need(input$PRODUCT_CODE != '',''),
        need(input$COMMENCEMENT_DATE<=input$STATUS_DATE,''),
        need(input$COMMENCEMENT_DATE>input$PAYER_DATE_OF_BIRTH,
             ''),
        need(input$COMMENCEMENT_DATE>input$INSURED_DATE_OF_BIRTH,
             ''),
        need(input$PAYPOINT_NAME>=1 & input$PAYPOINT_NAME<=428,
             ''),
        need(input$INCOME>=0,''),
        need(input$PREMIUM>=0,''),
        need(input$TERM>=0 & input$TERM<=99,
             '')
      )
      p <- dataset2()
      p <- p[,-1]
      p <- as.matrix(p)
      par(mar = rep(0,4))
      pie(p,labels = paste(round(p*100,1),"%",sep = ""),
          col=rainbow(length(p)))
      legend("topright",colnames(p),cex = 0.8,
             fill = rainbow(length(p)))
    }
  },height = 110, width = 400)
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

