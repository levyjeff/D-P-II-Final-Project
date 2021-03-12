git remote add origin https://github.com/NAJacobs1/D-P-II-Final-Project.git


library(tidyverse)
library(rworldmap)
library(fredr)
library(shiny)
library(plotly)
library(wesanderson)
library(tidytext)
library(textdata)

setwd("/Users/Nate/Desktop/Graduate School/Courses/Second Year/Winter Quarter/Data and Programming II/Final Project/D-P-II-Final-Project")

#Reading in trade data
us_fdi <- read_csv("US_FdiFlowsStock_ST202007161100_v1.csv")
us_ex_to_china_2018 <- read_csv("US_Ex_to_China_OEC_2018.csv")
us_ex_to_china_2019 <- read_csv("US_Ex_to_China_OEC_2019.csv")
us_ex_to_china_2020 <- read_csv("US_Ex_to_China_OEC_2020.csv")
us_im_from_china_2018 <- read_csv("US_Im_from_China_OEC_2018.csv")
us_im_from_china_2019 <- read_csv("US_Im_from_China_OEC_2019.csv")
us_im_from_china_2020 <- read_csv("US_Im_from_China_OEC_2020.csv")

#Some EDA
unique(us_fdi$Year)


#Tidying trade data
str(us_fdi)

us_fdi <- us_fdi %>% 
  subset(select = -c(9, 11, 13, 15, 17))

#Citation: https://medium.com/coinmonks/merging-multiple-dataframes-in-r-72629c4632a3
exports_merged <- do.call("rbind", list(us_ex_to_china_2018, us_ex_to_china_2019, us_ex_to_china_2020))
imports_merged <- do.call("rbind", list(us_im_from_china_2018, us_im_from_china_2019, us_im_from_china_2020))

exports_merged <- exports_merged %>% 
  mutate(type = "Export--US to China")

imports_merged <- imports_merged %>% 
  mutate(type = "Import--US from China")

us_china_totals <- rbind(exports_merged, imports_merged)

shiny_test <- us_china_totals %>% 
  group_by(Section, Time) %>% 
  summarise(total_trade = sum(`Trade Value`))

write.csv(shiny_test, file = 'shiny_test.csv')

#Creating a Shiny app of trade value data
ui <- fluidPage(
  selectInput(inputId = "category",
              label = "Choose a Category",
              choices = unique(shiny_test[['Section']])),
  plotlyOutput("trade_table"),
  tableOutput("cat_disp")
)

server <- function(input, output) {
  path <- "/Users/Nate/Desktop/Graduate School/Courses/Second Year/Winter Quarter/Data and Programming II/Final Project/D-P-II-Final-Project/"
  df <- read_csv(paste0(path, "shiny_test.csv"))
  
  data <- reactive({
    d <- filter(df, Section == input$category)
    return(d)
  })
  
  output$cat_disp <- renderTable({data()})
  
  output$trade_table <- renderPlotly({
    plt <- ggplot(data = data()) +
      geom_bar(aes(Time, total_trade), stat = "identity") +
      scale_fill_manual(values = wes_palette(4, name = "GrandBudapest1", type = "continuous"), name = "") +
      scale_y_continuous(labels = NULL) +
      labs(title = input$category, x = "Year", y = "Trade Volume")
    ggplotly(plt)
  })
}

shinyApp(ui = ui, server = server)


#Conducting NLP
nyt2021 <- read_file("NYT2021.txt")
dec_bis <- read_file("december_2020_bis.txt")

text_df <- tibble(text = nyt2021)
word_tokens_df <- unnest_tokens(text_df, word_tokens, text, token = "words")
word_tokens_df_nsw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

count(word_tokens_df_nsw, word_tokens, sort = TRUE)

for (s in c("afinn", "bing", "nrc")) {
  word_tokens_df_nsw <- word_tokens_df_nsw %>% 
    left_join(get_sentiments(s), by = c("word_tokens", "word")) %>% 
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}



#GENERALIZING: Writing a function to produce a plot of sentiments
sentiments_function <- function(article) {
  
  text_data <- tibble(text = article)
  word_tokens_df <- unnest_tokens(text_data, word_tokens, text, token = "words")
  word_tokens_df_nsw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))
  
  for (s in c("nrc", "afinn", "bing")) {
    word_tokens_df_nsw <- word_tokens_df_nsw %>% 
      left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>% 
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
    
    sentiment_plot <- ggplot(data = filter(word_tokens_df_nsw, !is.na(nrc))) +
      geom_histogram(aes(nrc), stat = "count") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(title = ("Sentiments"))
    
    return(sentiment_plot)
  }
}

sentiments_function(nyt2021)
sentiments_function(dec_bis)









