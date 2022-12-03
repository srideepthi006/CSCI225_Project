# Slider App



# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Training set (new lines)
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
    
    selectInput("gender", label = "gender", 
                choices = list("Male" = "Male", "Female" = "Female"), 
                selected = "Female"),
    
    sliderInput("age", label = "age", value = 40,
                min = min(TrainSet$age),
                max = max(TrainSet$age)),
    
    selectInput("hypertension", label = "hypertension", 
                choices = list("Hypertension" = "Hypertension", "No Hypertension" = "No Hypertension"), 
                selected = "No Hypertension"),
    
    selectInput("heart_disease", label = "heart_disease", 
                choices = list("Heart Disease" = "Heart Disease", "No Heart Disease" = "No Heart Disease"), 
                selected = "No Heart Disease"),
    
    selectInput("ever_married", label = "ever_married", 
                choices = list("Yes" = "Yes", "No" = "No"), 
                selected = "Yes"),
    
    sliderInput("avg_glucose_level", label = "avg_glucose_levele", value = 73,
                min = min(TrainSet$avg_glucose_level),
                max = max(TrainSet$avg_glucose_level)),
    
    sliderInput("bmi", label = "bmi", value = 17,
                min = min(TrainSet$bmi),
                max = max(TrainSet$bmi)),
    
    selectInput("smoking_status", label = "smoking_status", 
                choices = list("Smokes" = "Smokes", "Never Smoked" = "Never Smoked",
                               "Formerly Smoked" = "Formerly Smoked"), 
                selected = "Never Smoked"), 
    
    
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
      Name = c("gender",
               "age",
               "hypertension",
               "heart_disease",
               "ever_married",
               "avg_glucose_level",
               "bmi",
               "smoking_status"),
      Value = as.character(c(input$gender,
                             input$age,
                             input$hypertension,
                             input$heart_disease,
                             input$ever_married,
                             input$avg_glucose_level,
                             input$bmi,
                             input$smoking_status)),
      stringsAsFactors = FALSE)
    
    stroke <- 0
    df <- rbind(df, stroke)
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