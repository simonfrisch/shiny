# Shiny app for plotting kernel density estimation

library(shiny)

x <- c(2,4,5,6,8,9,10)


ui <- fluidPage(
  titlePanel("Kernel Density Estimation"),
  br(),
  br(),
  #numericInput("obs", "Number of observations:", 3, min = 2, max = 7), # free-text field
  selectInput("obs", "Select the number of observations:", choices = 2:7, selected = 3),
  
  selectInput("selectkernel", "Select a kernel", 
                       choices = c("Gaussian" = "gauss", 
                                   "Epanechnikov" = "epanech", 
                                   "Rectangular" = "rectang")),
  
  br(),
  plotOutput(outputId = "density_plot")
)


server <- function(input, output, session) {
  observeEvent(c(input$selectkernel,input$obs), {
    distr <- input$selectkernel
    n <- input$obs
    x <- x[1:n]
    if(distr == "gauss"){
      output$density_plot <- renderPlot(plot(density(x, kernel="gaussian"), xlab="", ylab="", main="Density Estimation using Gaussian Kernel", font.main=1, lwd=2, col="lightblue"))
    } else if(distr == "epanech"){
      output$density_plot <- renderPlot(plot(density(x, kernel="epanechnikov"), xlab="", ylab="", main="Density Estimation using Epanechnikov Kernel", font.main=1, lwd=2, col="lightblue"))
    } else if(distr == "rectang"){
      output$density_plot <- renderPlot(plot(density(x, kernel="rectangular"), xlab="", ylab="", main="Density Estimation using Rectangular Kernel", font.main=1, lwd=2, col="lightblue"))
    }
  })
}


shinyApp(ui = ui, server = server)
