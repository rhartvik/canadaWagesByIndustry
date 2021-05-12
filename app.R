library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

wages <- read.csv(file = 'data/115 Wage rates by industry by region UTF-8.csv')
wages[, c(2:4)] <- sapply(wages[, c(2:4)], as.factor)
wages$Value <- sapply(wages$Value, as.numeric)

industries <- wages %>% select("Industry") %>% distinct()
metrics <- c("Total employees",
             "Average hourly wage rate",
             "Average weekly wage rate",
             "Median hourly wage rate",
             "Median weekly wage rate")
regions <- c("Central region",
             "Eastern region",
             "Northern region",
             "Western region",
             "Total, Ontario regions")

ui <- fluidPage(

    titlePanel("Wages in Canada by Industry"),
    p("Explore wage statistics for Canadian regions from 2001 through 2015."),
    br(),

    sidebarLayout(
        sidebarPanel(
            p("Select the industry and regions that you are interested in seeing."),

            selectInput("industries", "Industry:",
                        industries, "Total employees"),
            checkboxGroupInput("regions", "Regions:",
                               regions, "Western region"),
            br(),
            p("Select the wage metric to display in the graph."),
            radioButtons("metrics", "Wage metric:",
                         metrics, "Average hourly wage rate")
        ),

        mainPanel(
            p("Hover your cursor above the graph to see the interactive action bar."),
            p("Hover your cursor over points to see their exact co-ordinates."),
            plotly::plotlyOutput("wagePlot")
        )
    )
)

server <- function(input, output) {

    dataInput <- reactive({
        filteredWages <- wages %>% filter(Industry %in% input$industries)
        filteredWages <- filteredWages %>% filter(Wage.rates %in% input$metrics)
        filteredWages <- filteredWages %>% spread(Geography, Value)
        filteredWages
    })

    output$wagePlot <- plotly::renderPlotly({
        # filter out the industries as selected
        data <- dataInput()
        fig <- plot_ly(data, x=~Year)

        for (region in input$regions) {
            fig <- fig %>% add_trace(y = data[[region]], name = region, type="scatter", mode = 'lines+markers')
        }
        fig %>% layout(title = 'Wages in Canada', showlegend = TRUE)
    })
}

shinyApp(ui = ui, server = server)
