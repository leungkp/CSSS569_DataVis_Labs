library(shiny)
library(tidyverse)
library(gapminder)

ui <- fluidPage(
  
  titlePanel("Gapminder data"),
  
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput(
        "continent", 
        "Continent(s) to include",
        choices = unique(gapminder$continent),
        selected = "Asia"
      ),
      actionButton("update", "Update"),
    ),
    mainPanel(
      plotOutput("GMplot")
    )
  )
)

server <- function(input, output) {
  
  update_data <- eventReactive(input$update, {
      gapminder %>% 
      filter(continent %in% input$continent)
  })
  
  output$GMplot <- renderPlot({
    ## Plot data according to input$yvar
    update_data() %>%
      ggplot(aes(x = year, y = gdpPercap, color = continent)) +
      geom_smooth() +
      geom_jitter(alpha = 0.2) +
      scale_y_log10() +
      theme_bw()
  })

}


shinyApp(ui = ui, server = server)