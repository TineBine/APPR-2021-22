library(shiny)

server <- function(input, output) {
  
  output$outplot <- renderPlot({
    povprecje_ekip %>%
      filter(Sezona == input$z) %>%
      ggplot(aes_string(x=input$x, y=input$y)) + geom_point() + geom_text(aes(label=Ekipa),hjust=0, vjust=0, size=3) + xlab("") + ylab("")
  })
}