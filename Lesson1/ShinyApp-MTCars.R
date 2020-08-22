library(shiny)
library(corrplot)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(expss)
library(plotly)
library(shinythemes)
ui <- fluidPage(
  titlePanel("MTCars"),
  theme = shinythemes::shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter Name:'),
      actionButton("show_greeting", "Show Greeting"),
      selectInput("ctufl", "Choose Type (Corrplot Only!)", choices = c('upper', 'full', 'lower'), 'full')
    ),
    mainPanel(
      textOutput("caption"),
      tabsetPanel(
        tabPanel("Corrplot", plotOutput("correlation")),
        tabPanel("Scatterplot", plotly::plotlyOutput("scatterplot")),
        tabPanel("Pie Chart", plotOutput("piechart")),
        tabPanel("Table", DT::DTOutput('table'))
      )
    )  
  )  
) 
server <- function(input, output, session){
  observeEvent(input$show_greeting, {
    showModal(modalDialog(paste("Hello,", input$name)))
  })
  output$caption <- renderText({
    paste("This is merely for fun")
  })
  output$correlation <- renderPlot({
    corrplot(cor(mtcars), type=input$ctufl)
  })
  Scatterplot_MTCars <- function(){
    use_labels(mtcars, {
      ggplot(mtcars) +
        geom_point(aes(y = mpg, x = wt, color = qsec)) +
        facet_grid(factor(am, labels = c("automatic", "manual")) ~ factor(vs, labels = c("V-Engine", "Straight Engine")))
    })
  }
  output$scatterplot <- plotly::renderPlotly(
    Scatterplot_MTCars()
  )
  PieChart_MTCars <- function(){
    mtcarsgear = table(mtcars$gear)
    percentlabels<- round(100*mtcarsgear/sum(mtcarsgear), 1)
    pielabels<- paste(percentlabels, "%", sep="")
    pie(mtcarsgear,col = rainbow(length(mtcarsgear)), labels = pielabels , main = 'Pie Chart for MTCars Number of forward gears', cex = 0.8)
    legend("topright", c("3","4","5"), cex=0.6, fill=  rainbow(length(
      mtcarsgear)
     )
    )
  }
  output$piechart <- renderPlot(PieChart_MTCars())
  output$table <- DT::renderDT({
    mtcars 
  })
  
}

shinyApp(ui = ui, server = server)