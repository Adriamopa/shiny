# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel("Barley Yield"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h3("Inputs for histogram"),
      selectInput(inputId = "gen",
                  label = "1. Select genotype",
                  choices = c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e", "F" = "f", "G" = "g", "H" = "h"), # also choices = unique(Barley$gen)
                  selected = "a"),
      br(),
      selectInput(inputId = "color",
                  label = "2. Select histogram color",
                  choices = c("blue" = "#22bbff", "green" = "#20cc40", "red" = "#f02222", "purple" = "#6111fc", "grey" = "grey"),
                  selected = "grey"),
      br(),
      sliderInput(inputId = "bin",
                  label = "3. Select number of histogram bins",
                  min = 1, max = 25, value = c(10)),
      br(),
      textInput(inputId = "text",
                label = "4. Enter some text to be displayed",
                value = "")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table"),
      textOutput("text"),
    )
  )
)

# server.R ----
server <- function(input, output) {
  output$plot <- renderPlot(ggplot(Barley, aes(x = yield)) +
                              geom_histogram(bins = input$bin,
                                             fill = input$color,
                                             data = Barley[Barley$gen == input$gen,],
                                             group = input$gen,
                                             color = "black"))
  output$table <- renderTable(Barley %>%
                                filter(gen == input$gen) %>%
                                summarise("Mean" = mean(yield),
                                          "Median" = median(yield),
                                          "STDEV" = sd(yield), 
                                          "Min" = min(yield),
                                          "Max" = max(yield)))
  output$text <- renderText(input$text)
}

# Run the app ----
shinyApp(ui = ui, server = server)