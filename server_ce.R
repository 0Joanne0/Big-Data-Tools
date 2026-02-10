#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #we change the color
  COLOR="darkgreen"
  observeEvent(input$colorBtn,{
    COLOR<<-sample(8,1)
  })
  
  output$distPlot <- renderPlot({
    input$updtBtn
    input$colorBtn
    
    #input$action #quand cet input est modifier cette fct est applee dans render
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = COLOR, border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
    
  })
  
  output$dataTab <- renderDataTable({faithful})
  output$text <- renderText(paste0("There are ",nrow(faithful)," observations."))
  
  observeEvent(input$action2,{
    
    output$distPlot2 <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Second graph (observe event)')
      
    })
    
  })
  
  get_bins=eventReactive(input$action3,{
    input$bins
  })
  
  output$distPlot3 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = get_bins() + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Graph 3 (event reactive)')
    
  })
  output$dataTab <- renderTable({faithful})
  
  insertUI(
    selector= "#container",
    ui = tags$div(
      tags$h4("More inputs"),
      textInput("in_text", "Insert text"),
      actionButton("new_btn", "New button")
    )
  )
  
}