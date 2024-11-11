server <- function(input, output, session) {
    # Server logic here
    output$plot1 <- renderPlot({
        # Your plot code here
    })
    
    output$plot2 <- renderPlot({
        # Your plot code here
    })
    
    output$table <- renderDT({
        # Your table code here
    })
}