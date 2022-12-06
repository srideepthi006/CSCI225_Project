library(RCurl) # for downloading CSV file
library(randomForest)
library(caret)
library(ROSE)
library(rlang)
library(shiny)
library(data.table)
library(rsconnect)
library(shinythemes)

#clean_data <- read.csv(text = getURL("https://raw.githubusercontent.com/srideepthi006/CSCI225_Project/main/stroke_data.csv"))

#clean_data <- subset(clean_data, select = -c(id,Residence_type,Age_Category))


#set.seed(666)

#clean_data$gender <- as.factor(clean_data$gender)
#clean_data$ever_married <- as.factor(clean_data$ever_married)
#clean_data$work_type <- as.factor(clean_data$work_type)
#clean_data$smoking_status <- as.factor(clean_data$smoking_status)
#clean_data$stroke <- as.factor(clean_data$stroke)
#clean_data$heart_disease <- as.factor(clean_data$heart_disease)
#clean_data$hypertension <- as.factor(clean_data$hypertension)
#clean_data$bmi <- as.numeric(clean_data$bmi)


#oversampled_data <- ovun.sample(as.factor(stroke)~.,data = clean_data, method = 'over',p = 0.3)$data
#sample_index <- createDataPartition(oversampled_data$stroke, p = 0.7, 
 #                                  list = FALSE, 
  #                               times = 1)
#Performs stratified random split of the data set

#TrainingSet <- oversampled_data[sample_index,] # Training Set
#TestingSet <- oversampled_data[-sample_index,] # Test Set

#write.csv(TrainingSet, "training.csv")
#write.csv(TestingSet, "testing.csv")

#TrainSet <- read.csv("training.csv", header = TRUE)

#TrainSet <- TrainSet[,-1]


#TrainSet[,"stroke"] <- as.factor(TrainSet[,"stroke"])
#Building Random forest model

#model <- randomForest(stroke ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

#pred_train <- predict(model)

#result_train <- confusionMatrix(pred_train, TrainingSet$stroke)
#(result_train$overall)['Accuracy']

#Save model to RDS file
#saveRDS(model, "model.rds")

#TrainSet <- read.csv("https://raw.githubusercontent.com/srideepthi006/CSCI225_Project/main/trainingnew.csv", header = TRUE)
#TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                navbarPage("Stroke Data Analysis & Prediction",
                           
                           tabPanel("Stoke predictor",   
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h4>"),
                                      
                                      selectInput("gender", label = "gender", 
                                                  choices = list("Male" = "Male", "Female" = "Female"), 
                                                  selected = "Female"),
                                      
                                      sliderInput("age", label = "age", value = 40,
                                                  min = min(TrainSet$age),
                                                  max = max(TrainSet$age)),
                                      
                                      selectInput("ever_married", label = "ever_married", 
                                                  choices = list("Yes" = "Yes", "No" = "No"), 
                                                  selected = "Yes"),
                                      
                                      selectInput("work_type", label = "work_type", 
                                                  choices = list("Private" = "Private", "Self Employed" = "Self Employed", "Govt Job" = "Govt Job"), 
                                                  selected = "Private"),
                                      
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
                                      
                                      selectInput("hypertension", label = "hypertension", 
                                                  choices = list("Hypertension" = "Hypertension", "No Hypertension" = "No Hypertension"), 
                                                  selected = "No Hypertension"),
                                      
                                      selectInput("heart_disease", label = "heart_disease", 
                                                  choices = list("Heart Disease" = "Heart Disease", "No Heart Disease" = "No Heart Disease"), 
                                                  selected = "No Heart Disease"),
                                      
                                      
                                      actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Prediction results table
                                      
                                    )
                           )
                           
                )
)

####################################
# Server                           #
####################################

model <- readRDS("model.rds")

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
               "smoking_status",
               "work_type"),
      Value = as.character(c(input$gender,
                             input$age,
                             input$hypertension,
                             input$heart_disease,
                             input$ever_married,
                             input$avg_glucose_level,
                             input$bmi,
                             input$smoking_status,
                             input$work_type)),
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
