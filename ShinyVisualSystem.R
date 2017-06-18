library(shiny)
library(DT)
library(rpart)
library(ggplot2)
library(Cairo)
library("reshape")
library("varhandle")
library("scales")

#setwd("G:/UNCC/Documents/VA/Project")
setwd("C:/Users/pooja/Desktop/Spring 17/Visual Analytics/Project")
db <- read.csv("db_data_cleaned_temp.csv", header = TRUE)
db2 <- read.csv("drug_No.csv", header = TRUE)
str(db)

db$REP_admission_source_id <- as.factor(db$REP_admission_source_id)
db$REP_discharge_disposition_id <- as.factor(db$REP_discharge_disposition_id)
db$REP_admission_type_id <- as.factor(db$REP_admission_type_id)
db$REP_diag_1 <- as.factor(db$REP_diag_1)
db$REP_diag_2 <- as.factor(db$REP_diag_2)
db$REP_diag_3 <- as.factor(db$REP_diag_3)

ui <- fluidPage(
  headerPanel("Diabetes Readmission Calculator"),
  navlistPanel(widths = c(2,10),
    tabPanel("Home", dataTableOutput("mytable")),
    tabPanel("Drugs and Demographics",
             fluidRow(
               column(12, align = "center",
               selectInput(inputId = "num7", label = "Select readmission rate", choices = as.list(db$REP_readmitted), multiple = FALSE)
               )
            ),
            
             fluidRow(
               column(width = 12, align = "center",
                      h2("Drug Ecosystem"),
                      plotOutput("chart1")
               )
            ),
              
            fluidRow(
              column(12, align = "center",
                     h2("Patient Demographics")
                     )
            ),
             
            fluidRow(
                column(6, align = "right",
                      plotOutput("plot2", height = 442,width = 500,brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE))
                ),                 
                    
                column(6, align = "left",
                      plotOutput("plot3", height = 400, width = 500)
                )
            )
             
    ),
    tabPanel("Readmission calculator",
             fluidRow(
               column(12, align = "center",
                      h2("Please select inputs")
               )
             ),
             fluidRow(
               column(4, 
                      sliderInput(inputId = "num1",label = "Number of days admitted in hospital", step = 1,
                                      min = min(db$time_in_hospital), max = max(db$time_in_hospital), value = mean(db$time_in_hospital)),
                      selectInput(inputId = "num2", label = "Age group", choices = as.list(db$age) ,multiple = FALSE)
                      
              ),
              column(4,
                    selectInput(inputId = "num3", label = "Race", choices = as.list(db$IMP_race), multiple = FALSE),
                    br(),
                    selectInput(inputId = "num4", label = "Gender", choices = as.list(db$gender), multiple = FALSE)
              ),     
              column(4,
                    selectInput(inputId = "num5", label = "Primary diagnosis", choices = as.list(db$REP_diag_1), multiple = FALSE),
                    br(),
                    sliderInput(inputId = "num6", label = "Number of inpatient visits in the past year", step = 1,
                                      min = min(db$number_inpatient), max = max(db$number_inpatient), value = mean(db$number_inpatient))
              )
             ),
             fluidRow(align = "center",actionButton("actionbutton","Predict!",align = "center", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ),
             fluidRow(
               column(12,
                      h2("Your probability of readmission"), 
                      textOutput("readmissionprob1"),
                      textOutput("readmissionprob2"),
                      textOutput("readmissionprob3")
              ),
               tags$head(tags$style("#readmissionprob1{color: red;font-size: 20px; font-style: bold; text-align:left;}"),
                       tags$style("#readmissionprob2{color: red;font-size: 20px; font-style: bold; text-align:left;}"), 
                       tags$style("#readmissionprob3{color: green;font-size: 20px; font-style: bold; text-align:left;}")
               ),
               tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
               )
             )
            
    ) # tabPanel
  )# navlistPanel
) # fluidpage

server <- function(input, output){
  
  rpart.fit <- rpart(REP_readmitted ~ age + gender + IMP_race + time_in_hospital + REP_diag_1 + number_inpatient, data = db)
  
  output$mytable <- DT::renderDataTable(db, filter = 'top', options = list(
    pageLength = 20, autoWidth = TRUE, columnDefs = list(list(width = '20000px', targets = c(3,4)))))
    pred <- eventReactive(input$actionbutton,{
    time_in_hospital_input <- input$num1
    age_input <- input$num2
    race_input <- input$num3
    gender_input <- input$num4
    diagnosis_input <- input$num5
    inpatient_input <- input$num6
    predict(rpart.fit, newdata = data.frame(age = age_input, gender = gender_input, IMP_race = race_input, 
                                            time_in_hospital = time_in_hospital_input, REP_diag_1 = diagnosis_input, number_inpatient = inpatient_input))
  })
    

    hist1 <- reactive({
      d <- db2[which(db2$Readmit == input$num7),]
      my <- ggplot(d, aes(x = Drugs, y = No_patients, fill = Drug_dosage))+geom_bar(stat = "identity")+coord_flip()+scale_y_continuous(limits = c(0, 18000))+labs(y = "Number of patients")+ scale_fill_discrete(name = "Drug dosage") #TODO 
      #mdfr <- melt(a, id=c("Readmit","Drug.dosage"))
      # my <- ggplot(mdfr, aes(variable,value,fill = Drug.dosage, width = 0.6)) +
      #   geom_bar(position = "fill", stat = "identity") +
      #   ylab("Percentage of patients") + xlab("Medicines")+
      #   scale_y_continuous(labels = percent) + coord_flip()
      my + theme(panel.background = element_rect(fill = 'slategray1'),
                 panel.border = element_rect(colour = "black", fill=NA, size=1),
                 axis.text.x = element_text(size = 13),
                 axis.text.y = element_text(size = 13),
                 axis.title.x = element_text(size = 16, face = "bold"),
                 axis.title.y = element_text(size = 16, face = "bold"),
                 legend.text=element_text(size = 12),
                 legend.title=element_text(size = 14, face = "bold")
                 )
    })
    
    output$chart1 <- renderPlot({
       
     hist1()
      })
  
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  fancy <- TRUE
 
  output$plot2 <- renderPlot({
    a <- db[which(db$REP_readmitted == input$num7),]
    myplot <- ggplot(a, aes(a$age, a$time_in_hospital)) +geom_point() + xlab("Patient Age") + ylab("Time in Hospital")
    myplot + theme(panel.background = element_rect(fill = 'slategray1'), 
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   axis.text.x = element_text(size = 13, angle = 90),
                   axis.text.y = element_text(size = 13),
                   axis.title.x = element_text(size = 16, face = "bold"),
                   axis.title.y = element_text(size = 16, face = "bold")
              )
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
output$plot3 <- renderPlot({
      if (!is.null(input$plot2_brush)) {
      # subselection
      time_low <- input$plot2_brush[[3]]
      time_high <- input$plot2_brush[[4]]
      age_low <- input$plot2_brush[[1]]
      age_high <- input$plot2_brush[[2]]

      a <- db[((db$time_in_hospital>= time_low)
              & (db$time_in_hospital<= time_high)
              & (db$age %in% levels(db$age[ceiling(age_low):floor(age_high)]))),]
     
      } else {
      # noselection
      a <- db[which(db$REP_readmitted == input$num7),]
      }
   
      myplot1 <- ggplot(a, aes(a$IMP_race, a$time_in_hospital)) + geom_boxplot(fill="#FF9999", colour="black") + xlab("Patient Race") + ylab("Time in Hospital")
      myplot1 + theme(panel.background = element_rect(fill = 'slategray1'), 
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      axis.text.x = element_text(size = 13),
                      axis.text.y = element_text(size = 13),
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold")
                )
      })

      output$readmissionprob1 <- renderText({paste("* In less than 30 days - ", round(pred()[1]*100,2),"%")})
      output$readmissionprob2 <- renderText({paste("* In 30-90 days - ", round(pred()[2]*100,2),"%")})
      output$readmissionprob3 <- renderText({paste("* No readmission - ", round(pred()[3]*100,2),"%")})
      }

shinyApp(ui = ui, server = server)

