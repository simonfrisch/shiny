# Shiny app for plotting some probability distributions

library(shiny)


ui <- fluidPage(
  titlePanel("Probability Distributions"),
  br(),
  
  selectInput("selectDistr", "Select a probability distribution", 
                       choices = c("Poisson" = "r_pois", 
                                   "Chi-squared" = "r_chisq", 
                                   "Normal" = "r_norm", 
                                   "Gamma" = "r_gamma",
                                   "Beta" = "r_beta")),
  
  selectInput("breaks", "Method to compute breakpoints", choices = c("sturges", "fd", "freedman-diaconis", "scott")),
  
  br(),
  plotOutput(outputId = "hist")
)


server <- function(input, output, session) {
  observeEvent(c(input$selectDistr, input$breaks), {
    distr <- input$selectDistr
    n_breaks <- input$breaks
    
    dist_out <- function(distr){
      switch(distr,
             r_pois = rpois(1000, 2),
             r_chisq = rchisq(500, 2),
             r_norm = rnorm(1000),
             r_gamma = rgamma(500,2),
             r_beta = rbeta(300, 0.5, 0.5))
    }
    d <- dist_out(distr)
    
    plot_title <- function(distr){
      switch(distr,
             r_pois = "Poisson",
             r_chisq = "Chi-squared",
             r_norm = "Normal",
             r_gamma = "Gamma",
             r_beta = "Beta")
    }
    plt_title <- plot_title(distr)
    
    output$hist <- renderPlot(hist(d, breaks = n_breaks, probability = TRUE, xlab = "", ylab = "", main = paste(plt_title, "distribution"), col = "lightblue", border = "lightblue"))
  })
}


shinyApp(ui = ui, server = server)
