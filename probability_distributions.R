# Shiny app for plotting common probability distributions

### global
library(shiny)

### server
server <- function(input, output, session) {
  observeEvent(input$selectDistr, {
    distr <- isolate(input$selectDistr)
    if(distr == "rpois"){
      output$hist <- renderPlot(hist(rpois(1000, 2), prob=TRUE, xlab="", ylab="", main="Poisson distribution", col="lightblue", border="lightblue"))
    } else if(distr == "rchisq"){
      output$hist <- renderPlot(hist(rchisq(500, 2), prob=TRUE, xlab="", ylab="", main="Chi-Squared distribution", col="lightblue", border="lightblue"))
    } else if(distr == "rnorm"){
      output$hist <- renderPlot(hist(rnorm(2000), prob=TRUE, xlab="", ylab="", main="Normal distribution", col="lightblue", border="lightblue"))
    } else if(distr == "rgamma"){
      output$hist <- renderPlot(hist(rgamma(500, 2), prob=TRUE, xlab="", ylab="", main="Gamma distribution", col="lightblue", border="lightblue"))
    } else if(distr == "rbeta"){
      output$hist <- renderPlot(hist(rbeta(300, 0.5, 0.5), prob=TRUE, xlab="", ylab="", main="Gamma distribution", col="lightblue", border="lightblue"))
    }
  })
}

### ui
ui <- fluidPage(
  titlePanel("Probability Distributions"),
  br(),
  fluidRow(
    column(4,
           selectInput("selectDistr", "Select a probability distribution", 
                       choices = c("Poisson" = "rpois", 
                                   "Chi-squared" = "rchisq", 
                                   "Normal" = "rnorm", 
                                   "Gamma" = "rgamma",
                                   "Beta" = "rbeta")))),
  br(),
  plotOutput(outputId = "hist")
)

shinyApp(ui = ui, server = server)
