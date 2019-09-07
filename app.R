library(ggplot2)
library(shiny)

source("scoremodel.R")

<<<<<<< HEAD

=======
>>>>>>> 9805e0d6c0664d702579ea3f25f10ad8e37233ba
ui = fluidPage(titlePanel("Score Modeler"),
               
               sidebarLayout(
                 sidebarPanel(
                   numericInput(
                     inputId = "max.score",
                     label = "Max Possible Score",
                     min = 0,
                     value = 100
                   ),
                   
                   checkboxInput(
                     inputId = "ec.possible",
                     label = "Extra Credit Possible? (Accuracy may be reduced)",
                     value = FALSE
                   ),
                   
                   numericInput(
                     inputId = "mean.score",
                     label = "Mean Score",
                     min = 0,
                     value = 70
                   ),
                   
                   numericInput(
                     inputId = "sd.score",
                     label = "Standard Deviation",
                     min = 0,
                     value = 10
                   ),
                   
                   numericInput(
                     inputId = "my.score",
                     label = "Your Score",
                     min = 0,
                     value = 0
                   ),
                   
                   textOutput(outputId = "percentile.text")
                 ),
                 
                 mainPanel(
                   h4("See how well you did on your exam!"),
                   
                   plotOutput(outputId = "dist.plot", click = "my.score.click")
                 )
               ))
<<<<<<< HEAD

=======
>>>>>>> 9805e0d6c0664d702579ea3f25f10ad8e37233ba

server = function(input, output, session)
{
  observeEvent(input$max.score, {
    updateNumericInput(session, "mean.score", max = input$max.score)
    updateNumericInput(session, "sd.score", max = input$max.score)
    updateNumericInput(session, "my.score", max = input$max.score)
  })
  
  observeEvent(input$my.score.click, {
    updateNumericInput(session, "my.score", value = round(input$my.score.click$x))
  })
  
  output$dist.plot = renderPlot({
    model = score.model(input$max.score,
                        input$mean.score,
                        input$sd.score,
                        input$ec.possible)
    
    my.score = ifelse(is.na(input$my.score), 0, input$my.score)
    
    score.plot(model[[1]], model[[2]], my.score)
    
  })
  
  output$percentile.text = renderText({
    if (is.na(input$my.score) || input$my.score == 0)
      return("")
    
    model = score.model(input$max.score,
                        input$mean.score,
                        input$sd.score,
                        input$ec.possible)
    
    percentile = score.percentile(model[[1]], input$my.score)
    
    paste("Your score is above ", round(percentile), "% of scores.", sep =
            "")
    
  })
}


shinyApp(ui, server)
