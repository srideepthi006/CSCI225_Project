# Slider App

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

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)

TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Stroke Prdeictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    
    selectInput("gender", label = "Gender:", 
                choices = list("Male" = "Male", "Female" = "Female"), 
                selected = "Female"),
    
    sliderInput("age", label = "Age", value = 40,
                min = min(TrainSet$age),
                max = max(TrainSet$age)
    ),
    
    sliderInput("avg_glucose_level", label = "Glucose", value = 73,
                min = min(TrainSet$avg_glucose_level),
                max = max(TrainSet$avg_glucose_level)),
    
    sliderInput("bmi", label = "BMI", value = 17,
                min = min(TrainSet$bmi),
                max = max(TrainSet$bmi)),
    
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Gender",
               "Age",
               "Glucose",
               "BMI"),
      Value = as.character(c(input$gender,
                             input$age,
                             input$avg_glucose_level,
                             input$bmi)),
      stringsAsFactors = FALSE)
    
    Stroke <- 0
    df <- rbind(df, Stroke)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)