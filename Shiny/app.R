library(shiny)
ui <- fluidPage(
  fluidRow(
    column(12, align="center",
           div(style="display: inline-block;",img(src="body-fat-percentage-men-1.jpg", height=150, width=450))
    )
  ),
  tags$hr(),
  
  
  sidebarLayout(
    sidebarPanel(
      width=4,
      h2("BODY FAT PERCENTAGE",align="center"),
      h2("CALCULATOR FOR MEN ",align="center"),
      helpText("Note:"),
      helpText("1. Abdomen 2 circumference is measured laterally, at the level of the iliac crests, and anteriorly, at the umbilicus. Abdomen 2 circumference is necessary for the calculation, so please do not skip it, and try to make it as accurate as possible.",align="left"),
      helpText("2. Providing weight, which makes the result more accurate, is highly recommended.",align="left"),
      helpText("3. If you are not sure or unwilling to provide the other items, leave them empty.",align="left"),
      helpText("4. Please make sure your inputs are correct. If a input is not numeric value, it will be treated as no input",align="left")
    ),
    mainPanel(
      fluidRow(
        column(6, 
               numericInput("abdomen", 
                            h4("Abdomen 2 circumference (cm)"), 
                            value = 90))
      ),
      fluidRow(
        column(6,
               numericInput("weight", 
                            h4("Weight (lbs)"), 
                            value = NA))
      ),
      fluidRow(
        column(6, 
               numericInput("wrist", 
                            h4("Wrist circumference (cm)"), 
                            value = NA))
      ),
      fluidRow(
        column(6, 
               numericInput("forearm", 
                            h4("Forearm circumference (cm)"), 
                            value = NA))
      ),
      fluidRow(
        column(4, 
               actionButton("gobutton","Calculate"))
      ),
      fluidRow(
        column(11,
               h3("RESULT"),
               tags$style(HTML("
                               #summary{
                               font-size: 20px;
                               background-color: rgba(255,255,0,0.40); 
                               color: black;
                               }
                               ")),
               verbatimTextOutput("summary"),align="center")
               )
    )
  ),
  
  tags$hr(),
 
  fluidRow(
    column(12, align="center",
           div(style="display: inline-block;",img(src="Body-Fat-Ranges.jpg", height=180, width=950)))
  ),
  h4("Left to right: UNDERFAT, HEALTHY, OVERFAT, OBESE ",align = "center"),
  p("Compare you body fat percentage with the table above"),
  p("The result is only an ", strong("ESTIMATE"), " of your body fat percentage and could be inaccurate. If you're concerned with your health, please seek professional help. "),
  tags$hr(),
  
  helpText("Reference link:"),
  helpText("https://www.healthstatus.com/measuring-body-fat-percentage-home/"),
  helpText("https://www.medicalnewstoday.com/articles/body-fat-percentage-chart"),
  helpText("https://totalrowfitness.com/know-your-body-fat-total-row-explains-why-it-matters/"),
  tags$hr(),
  helpText("Created by Jiayi Xu (jxu273@wisc.edu) and Hua Tong (htong24@wisc.edu)"),
  helpText("Should you have any questions to our app, please contact us through email")
  
  
) 
  

server <- function(input, output) {
  
  calculate <- reactive({
    weight=input$weight
    abdomen=input$abdomen
    wrist=input$wrist
    forearm=input$forearm
    
    if(is.na(weight)){
      bodyfat=-34.7695+0.5805*abdomen
    }else if(!is.na(weight) & sum(is.na(c(wrist,forearm)))>0){
      bodyfat=-40.8999+0.9117*abdomen-0.1370*weight
    }else{
      bodyfat=-30.3331+0.9171*abdomen-0.1249*weight-1.4053*wrist+0.4317*forearm
    }
    
    bodyfat <- round(bodyfat,2)
    if(bodyfat<0 | bodyfat>50){
      print('Please check your input (it seems impossible).')
    }else{
      print(paste("YOUR BODY FAT PERCENTAGE IS: ",bodyfat,'%'))
    }
  })
  x <- eventReactive(input$gobutton, {
    if(is.na(input$abdomen)){
      paste('Please enter your abdomen 2 circumference.')
    }else if(sum(c(input$abdomen,input$weight,input$forearm,input$wrist)<c(60,0,15,12),na.rm=T)>0
             | sum(c(input$abdomen,input$weight,input$forearm,input$wrist)>c(170,500,40,25),na.rm=T)>0){
      paste('Please check your input (some numbers are anomalous).')
    }else{
      calculate()
    }
  })
  
  output$summary <- renderText({
    x()
  })
  
}

  
shinyApp(ui = ui, server = server)

#Contribution:
#Hua Tong: added algorithms to improve robustness, modified the notes.
#Jiayi Xu: created and built the main frame of the app; added notes and finalized the app.

