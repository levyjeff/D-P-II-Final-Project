library(tidyverse)
library(shiny)
library(plotly)
library(janitor)
library(scales)
library(countrycode)
library(StandardizeText)
library(RColorBrewer)
library(classInt)
library(scales)
library(grid)
library(directlabels)
library(ggthemes)
library(wesanderson)

us_china_totals <- read_csv("us_china_totals.csv")

# Interactive Plot of Trade Volume Between the US and China, using US--China trade flow data

ui <- fluidPage(
  selectInput(
    inputId = "category",
    label = "Choose an Import/Export Category",
    choices = unique(us_china_totals[["section"]])
  ),
  plotlyOutput("trade_table"),
  tableOutput("cat_disp"),
)

server <- function(input, output) {
  df <- us_china_totals %>%
    group_by(section, time, type) %>%
    summarise(total_value = sum(trade_value))
  
  data <- reactive({
    d <- filter(df, section == input$category)
    return(d)
  })
  
  output$cat_disp <- renderTable({
    data()
  })
  
  output$trade_table <- renderPlotly({
    plt <- ggplot(data = data()) +
      geom_bar(aes(time, total_value, fill = type), stat = "identity") +
      theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme_economist() +
      labs(title = input$category, x = "Year", y = "Trade Volume") +
      ggtitle("US-China Trade Flows by Product Category, 2017-2019") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))
    ggplotly(plt)
  })
}

shinyApp(ui = ui, server = server)




