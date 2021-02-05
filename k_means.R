# Shiny app for plotting some probability distributions

library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Clustering"),
  br(),
  
  # selectInput("selectDistr", "Select a probability distribution", 
  #                      choices = c("Poisson" = "r_pois", 
  #                                  "Chi-squared" = "r_chisq", 
  #                                  "Normal" = "r_norm", 
  #                                  "Gamma" = "r_gamma",
  #                                  "Beta" = "r_beta")),
  
  selectInput("k", "Select the number of clusters", choices = 2:7, selected = 3),
  
  br(),
  plotOutput(outputId = "plt")
)


server <- function(input, output, session) {
  observeEvent(c(input$k), {
    k <- input$k
    ir <- iris[,1:2]
    k_means <- kmeans(ir, centers = k)
    #mt <- mtcars[,c(1,3)]; k_means <- kmeans(mt, centers = k)
    output$plt <- renderPlot(ggplot(ir, aes(Sepal.Length, Sepal.Width)) + 
                               geom_point(colour = k_means$cluster) +
                               theme_bw() +
                               xlab("Sepal Length") +
                               ylab("Sepal Width") +
                               ggtitle(paste("k-means clustering of iris dataset with k =", k)))
  })
}


shinyApp(ui = ui, server = server)
