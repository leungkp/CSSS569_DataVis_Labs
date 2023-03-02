library(shiny)

# One thing, I recommend turning on 
# Preferences :: Code :: Display :: Show indent guides

ui <- fluidPage(
  
  titlePanel("Histogram of a random variable"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "num",
        label = "Number of observations",
        min = 1,
        max = 500,
        value = 10
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot Histogram", plotOutput("histogram")),
        tabPanel("Summary table", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  random_numbers <- reactive(
    rnorm(input$num)
  )

  output$histogram <- renderPlot({
    hist(random_numbers())
    abline(v = mean(random_numbers()), col = "red")
  })

  output$summary <- renderPrint(
    summary(random_numbers())
  )
  
  ### Why not the following code? 
  # output$histogram <- renderPlot({
  #   x <- rnorm(input$num, sd = 10)
  #   hist(x)
  #   abline(v = mean(x), col = "blue", lwd = 2)
  # })
  # 
  # output$summary <- renderPrint({
  #   x <- rnorm(input$num, sd = 10)
  #   summary(x)
  # })
  
  ### And you cannot retrieve input outside of reactive context
  # x <- rnorm(input$num, sd = 10)
}


shinyApp(ui = ui, server = server)


