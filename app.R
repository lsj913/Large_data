library(shiny)
library(tidyverse)

release <- read.csv('Data/release.csv')
release_type <- read.csv('Data/release_type.csv')
release_type_unique <- read.csv('Data/release_type_unique.csv')

ui <- fluidPage(
  titlePanel("The number of film"),

  sidebarLayout(
    sidebarPanel(
      helpText('Creat the number of release film or film in boxoffice per year(2019 ~ 2020)'),
    selectInput('var',
                 label = 'choose a case to display',
                 choices = c('release film' , 'film in boxoffice', 'unique film in boxoffice'),
                 selected = 'release film')
    ),
    
    mainPanel(plotOutput('N_film'))
  )
)

server <- function(input, output) {
  output$N_film <- renderPlot({
    data <- switch(input$var,
                   'release film' = release,
                   'film in boxoffice' = release_type,
                   'unique film in boxoffice' = release_type_unique)
    X <- switch(input$var,
                   'release film' = release$year,
                   'film in boxoffice' = release_type$year,
                   'unique film in boxoffice' = release_type_unique$year)
    Y <- switch(input$var,
                    'release film' = release$N,
                    'film in boxoffice' = release_type$N,
                    'unique film in boxoffice' = release_type_unique$N)
    Fill <- switch(input$var,
                   'release film' = release$type,
                   'film in boxoffice' = release_type$type,
                   'unique film in boxoffice' = release_type_unique$type)
    data %>% ggplot(mapping = aes(x = X, y = Y, fill = Fill)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette = "Pastel1") + ylim(0, 140) + labs(x = 'Year', y = 'N') +
      geom_text(aes(label = Y), vjust = 1.5, colour = "black", position = position_dodge(1), size = 5) +
      theme(axis.title = element_text(size = 13), axis.text = element_text(size = 13), legend.title = element_text(size = 13), legend.text = element_text(size = 12))
  })
  }
  

shinyApp(ui = ui, server = server)
