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
      selectInput(
        'yvar', 
        'Outcome you want to show', 
        choices = c("Life expectancy" = "lifeExp", 
                    "Population" = "pop", 
                    "GDP per capita" = "gdpPercap"), 
        selected = "Life expectancy"
      ),
      selectInput(
        'smallM', 
        'Small Multiples?', 
        choices = c("Yes", "No"), 
        selected = "No"
      ),
      selectInput(
        'smallM_cat', 
        'Small Multiples by?', 
        choices = c("continent"), 
        selected = "continent"
      ),
    ),
    mainPanel(
      plotOutput("GMplot")
    )
  )
)

server <- function(input, output) {
  output$GMplot <- renderPlot({
    
    ## Extract data according to input$continent
    data <- 
      gapminder %>% 
      filter(continent %in% input$continent)
    
    ## Plot data according to input$yvar
    p <- ggplot(data, aes(x = year, color = continent)) +
      geom_smooth(aes_string(y = input$yvar)) +
      geom_jitter(aes_string(y = input$yvar), alpha = 0.2) +
      scale_y_log10() +
      theme_bw()
    
    ## Decide if small multiple based on input$smallM
    if (input$smallM == "Yes") {
      p + facet_wrap(~ get(input$smallM_cat)) + labs(title = paste0("Some text here", input$smallM_cat))
    } else {
      p
    }
    
  })

}


shinyApp(ui = ui, server = server)