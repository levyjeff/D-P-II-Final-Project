library(tidyverse)
library(rworldmap)
library(fredr)
library(shiny)
library(plotly)
library(wesanderson)
library(tidytext)
library(textdata)
library(SnowballC)
library(udpipe)
library(ggraph)
library(igraph)
library(fredr)
library(MASS)

setwd("/Users/Nate/Desktop/Graduate School/Courses/Second Year/Winter Quarter/Data and Programming II/Final Project/D-P-II-Final-Project")

# Reading in trade data
us_fdi <- read_csv("US_FdiFlowsStock_ST202007161100_v1.csv")
us_ex_to_china_2018 <- read_csv("US_Ex_to_China_OEC_2018.csv")
us_ex_to_china_2019 <- read_csv("US_Ex_to_China_OEC_2019.csv")
us_ex_to_china_2020 <- read_csv("US_Ex_to_China_OEC_2020.csv")
us_im_from_china_2018 <- read_csv("US_Im_from_China_OEC_2018.csv")
us_im_from_china_2019 <- read_csv("US_Im_from_China_OEC_2019.csv")
us_im_from_china_2020 <- read_csv("US_Im_from_China_OEC_2020.csv")

# Some EDA
unique(us_fdi$Year)


# Tidying trade data
str(us_fdi)

us_fdi <- us_fdi %>%
  subset(select = -c(9, 11, 13, 15, 17))

# Citation: https://medium.com/coinmonks/merging-multiple-dataframes-in-r-72629c4632a3
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

write.csv(shiny_test, file = "shiny_test.csv")

# Creating a Shiny app of trade value data
ui <- fluidPage(
  selectInput(
    inputId = "category",
    label = "Choose a Category",
    choices = unique(shiny_test[["Section"]])
  ),
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

  output$cat_disp <- renderTable({
    data()
  })

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

## NLP Section

# Reading in articles, and conducting sentiment analysis on the first NYT article
nyt2021 <- read_file("NYT2021.txt")
peoplesdailymarch4_21 <- read_file("PeoplesDailyMarch4.txt")
peoplesdailysept28_18 <- read_file("PeoplesDailySept28_18.txt")
nytsep25_18 <- read_file("NYTSept25_18.txt")


text_df <- tibble(text = nyt2021)
word_tokens_df <- unnest_tokens(text_df, word_tokens, text, token = "words")
word_tokens_df_nsw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

count(word_tokens_df_nsw, word_tokens, sort = TRUE)

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
sentiments_function(peoplesdailymarch4_21)
sentiments_function(peoplesdailysept28_18)
sentiments_function(nytsep25_18)

#Conducting more advanced NLP
word_tokens_df_nsw$stem <- wordStem(word_tokens_df_nsw$word_tokens, language = "porter")

word_tokens_df_nsw %>%
  dplyr::group_by(stem) %>%
  count(sort = TRUE)

parsed_nyt <- udpipe(nyt2021, "english")

parsed_nyt$stem <- wordStem(parsed_nyt$token, language = "porter")

view(select(parsed_nyt, "token", "stem", "lemma", "upos"))

#Conducting dependency parsing on the first NYT article
nyt_deps <- cbind_dependencies(parsed_nyt, type = "parent_rowid", recursive = TRUE)

nyt_deps %>%
  select(c(token_id, token, upos, dep_rel, parent_rowid, parent_rowids)) %>%
  View()

#Plotting
nyt_deps <- nyt_deps %>%
  select(-"stem")

one_nyt <- filter(nyt_deps, doc_id == "doc1", sentence_id == 1)

edges <- subset(one_nyt, head_token_id != 0, select = c("token_id", "head_token_id", "dep_rel"))

edges$label <- edges$dep_rel

g <- graph_from_data_frame(edges,
  vertices = one_nyt[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
  directed = TRUE
)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(label = dep_rel), arrow = arrow(length = unit(4, "mm")), end_cap = circle(3, "mm")) +
  geom_node_point(color = "lightblue", size = 5) +
  theme_void(base_family = "") +
  geom_node_text(aes(label = token), vjust = 1.8) +
  ggtitle("Showing dependencies")


#GENERALIZING: Writing a function to do dependency parsing on any given sentence in any given article.
#The point here is to determine whether there are differences in the sentence structures of the NYT and People's Daily articles. 
dependency_parsing_func <- function(article, sentence_num) {
  text_data <- tibble(text = article)
  word_tokens_df <- unnest_tokens(text_data, word_tokens, text, token = "words")
  word_tokens_df_nsw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

  for (s in c("nrc", "afinn", "bing")) {
    word_tokens_df_nsw <- word_tokens_df_nsw %>%
      left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)

    word_tokens_df_nsw$stem <- wordStem(word_tokens_df_nsw$word_tokens, language = "porter")

    parsed <- udpipe(article, "english")
    parsed$stem <- wordStem(parsed$token, language = "porter")

    deps <- cbind_dependencies(parsed, type = "parent_rowid", recursive = TRUE)

    one_sentence <- filter(deps, doc_id == "doc1", sentence_id == sentence_num)

    edges <- subset(one_sentence, head_token_id != 0, select = c("token_id", "head_token_id", "dep_rel"))

    edges$label <- edges$dep_rel

    g <- graph_from_data_frame(edges,
      vertices = one_sentence[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
      directed = TRUE
    )

    g_graph <- ggraph(g, layout = "fr") +
      geom_edge_link(aes(label = dep_rel), arrow = arrow(length = unit(2, "mm")), end_cap = circle(3, "mm")) +
      geom_node_point(color = "darkred", size = 3) +
      theme_void(base_family = "") +
      geom_node_text(aes(label = token), vjust = 1.8) +
      ggtitle("Showing Dependencies for Given Sentence of Given Article")

    return(g_graph)
  }
}

dependency_parsing_func(peoplesdailymarch4, 1)
dependency_parsing_func(nyt2021, 1)
dependency_parsing_func(peoplesdailysept28_18, 1)
dependency_parsing_func(nytsep25_18, 4)














