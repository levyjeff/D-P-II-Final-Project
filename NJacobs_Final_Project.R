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
library(janitor)

setwd("/Users/Nate/Desktop/Graduate School/Courses/Second Year/Winter Quarter/Data and Programming II/Final Project/D-P-II-Final-Project")

rm(list = ls())

options(scipen = 999)

#PART 1: Data wrangling

# Reading in trade data
us_fdi <- read_csv("US_FdiFlowsStock_ST202007161100_v1.csv")
us_ex_to_china_2018 <- read_csv("US_Ex_to_China_OEC_2018.csv")
us_ex_to_china_2019 <- read_csv("US_Ex_to_China_OEC_2019.csv")
us_ex_to_china_2020 <- read_csv("US_Ex_to_China_OEC_2020.csv")
us_im_from_china_2018 <- read_csv("US_Im_from_China_OEC_2018.csv")
us_im_from_china_2019 <- read_csv("US_Im_from_China_OEC_2019.csv")
us_im_from_china_2020 <- read_csv("US_Im_from_China_OEC_2020.csv")
world_trade_flows <- read_csv("BACI_HS17_Y2019_V202102.csv")
country_codes <- read_csv("country_codes_V202102.csv")
product_codes <- read_csv("product_codes_HS17_V202102.csv")
gdp_data <- read_csv("2b68fee6-4c0f-4592-a63f-ff85acd68db3_Data.csv")
us_asia_world_flows <- read_csv("comtrade.csv")
world_ex_im <- read_csv("f4dc9c88-9144-422d-aa34-42550a9c844f_Data.csv")

# Tidying trade data
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

us_china_totals <- clean_names(us_china_totals)

str(us_china_totals)

us_china_totals$time <- as.character(us_china_totals$time)

#World trade flows data--for more info, see documentation here: http://www.cepii.fr/DATA_DOWNLOAD/baci/doc/DescriptionBACI.html
world_trade_flows <- world_trade_flows %>% 
  rename(year = "t", product_cat = "k", exporter = "i", importer = "j", trade_flow = "v", quantity_tons = "q")
  
world_trade_flows <- world_trade_flows %>% 
  inner_join(country_codes, by = c("exporter" = "country_code"))

world_trade_flows <- world_trade_flows %>% 
  rename(exporter_name = country_name_full)

world_trade_flows <- world_trade_flows %>% 
  select(-country_name_abbreviation)

world_trade_flows <- world_trade_flows %>% 
  select(-c(iso_2digit_alpha, iso_3digit_alpha))

world_trade_flows <- world_trade_flows %>% 
  inner_join(country_codes, by = c("importer" = "country_code"))

world_trade_flows <- world_trade_flows %>% 
  rename(importer_name = country_name_full)

world_trade_flows <- world_trade_flows %>% 
  select(-country_name_abbreviation)

world_trade_flows <- world_trade_flows %>% 
  select(-c(iso_2digit_alpha, iso_3digit_alpha))

product_codes$code <- as.character(product_codes$code)

world_trade_flows <- world_trade_flows %>% 
  inner_join(product_codes, by = c("product_cat" = "code"))

world_trade_flows <- world_trade_flows %>% #Restricting the dataset from the whole world to just the US and Asian trade partners reduces the size from over 10 million rows to about 44000.
  filter(year == 2019, importer_name %in% c("China", "Japan", "USA", "Puerto Rico and US Virgin Islands", "India", "Republic of Korea"), 
         exporter_name %in% c("China", "Japan", "USA", "Puerto Rico and US Virgin Islands", "India", "Republic of Korea"))

#Tidying and reshaping GDP data
gdp_data_tidy <- gdp_data %>% 
  select(-'Series Code')

gdp_data_tidy <- gdp_data_tidy %>% 
  clean_names()

gdp_data_tidy <- gdp_data_tidy %>% #Citation: https://stackoverflow.com/questions/58837773/pivot-wider-issue-values-in-values-from-are-not-uniquely-identified-output-w
  group_by(series_name) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = series_name, values_from = c(x2017_yr2017, x2018_yr2018, x2019_yr2019, x2020_yr2020)) %>% 
  select(-row)

gdp_data_tidy <- gdp_data_tidy %>% 
  select(-c(8, 14, 20:26))

gdp_data_tidy <- gdp_data_tidy %>% 
  rename('2017_gdp' = 3, '2017_gdpgrowth' = 4,
         '2017_gdppercap2010usd' = 5, '2017_gdppercapcurrentusd' = 6,
         '2017_percenttradegpd' = 7, 
         '2018_gdp' = 8, '2018_gdpgrowth' = 9,
         '2018_gdppercap2010usd' = 10, '2018_gdppercapcurrentusd' = 11,
         '2018_percenttradegpd' = 12, 
         '2019_gdp' = 13, '2019_gdpgrowth' = 14,
         '2019_gdppercap2010usd' = 15, '2019_gdppercapcurrentusd' = 16,
         '2019_percenttradegpd' = 17) 

gdp_part <- gdp_data_tidy %>% pivot_longer(
  cols = starts_with(c("2017", "2018", "2019")),
  names_to = c("year"),
  values_to = c("value"),
  values_drop_na = TRUE
)

gdp_part$value <- as.numeric(gdp_part$value)

gdp_part <- gdp_part %>% 
  separate(col = year,
           into = c("year", "measure"),
           sep = "_") %>% 
  filter(!is.na(value))

gdp_part <- gdp_part %>% 
group_by(country_name, year, measure) %>% 
summarise(value = sum(value))

gdp_part <- gdp_part %>% 
  pivot_wider(names_from = measure, values_from = value)

gdp_part[complete.cases(gdp_part), ] #Checking for NAs

gdp_final <- na.omit(gdp_part) #Omitting NAs, because it is unclear what to impute for them

total_trade_flows <- world_trade_flows %>% 
  group_by(exporter_name, importer_name) %>% 
  summarise(total = sum(trade_flow))

#Reshaping and merging export-import data
world_ex_im_tidy <- world_ex_im %>% 
  pivot_longer(cols = starts_with(c("2017", "2018", "2019")),
               names_to = c("year"),
               values_to = c("value"),
               values_drop_na = TRUE
)

world_ex_im_tidy <- world_ex_im_tidy %>% 
  select(-`Series Code`)

world_ex_im_tidy <- clean_names(world_ex_im_tidy)

gdp_data_tidy <- gdp_data_tidy %>% #Citation: https://stackoverflow.com/questions/58837773/pivot-wider-issue-values-in-values-from-are-not-uniquely-identified-output-w
  group_by(series_name) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = series_name, values_from = c(x2017_yr2017, x2018_yr2018, x2019_yr2019, x2020_yr2020)) %>% 
  select(-row)

world_ex_im_tidy <- world_ex_im_tidy %>% 
  pivot_wider(names_from = series_name, values_from = value)
  
world_ex_im_tidy$year <- str_extract(world_ex_im_tidy$year,"20..")

world_ex_im_tidy$exports_of_goods_and_services_current_us <- as.numeric(world_ex_im_tidy$exports_of_goods_and_services_current_us)
world_ex_im_tidy$imports_of_goods_and_services_bo_p_current_us <- as.numeric(world_ex_im_tidy$imports_of_goods_and_services_bo_p_current_us)

world_ex_im_tidy <- na.omit(world_ex_im_tidy) #Omitting NAs, because it is unclear what to impute for them

#Merg

#names(gdp_and_trade_flows)

#gdp_and_trade_flows <- gdp_and_trade_flows %>% 
 # select(-c(9:14, 16:18, 20, 21:35, 41))

#gdp_and_trade_flows <- gdp_and_trade_flows %>% 
#  select(-c(11, 12, 14, 15))

 #gdp_and_trade_flows %>% 
  #group_by(country_name, year.x, trade_flow, partner, gdp, gdpgrowth, gdppercap2010usd, gdppercapcurrentusd) %>% 
  #dplyr::summarise(trade_value = max(trade_value_us))
 
#PART 2: Plotting data
#Interactive Plot of Trade Volume Between the US and China
  ui <- fluidPage(
    selectInput(
      inputId = "category",
      label = "Choose an Import/Export Category",
      choices = unique(us_china_totals[["section"]])),
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
        labs(title = input$category, x = "Year", y = "Trade Volume") 
      ggplotly(plt) 
       
    })
  } 

  shinyApp(ui = ui, server = server)
  
##PART 3: NLP Section

#Reading in articles, and conducting sentiment analysis on the first NYT article
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
      geom_histogram(aes(nrc, fill = nrc), stat = "count") +
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

#view(select(parsed_nyt, "token", "stem", "lemma", "upos")) commenting out the view() command

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

#Examining the first sentence of each article
dependency_parsing_func(peoplesdailymarch4_21, 1)
dependency_parsing_func(nyt2021, 1)
dependency_parsing_func(peoplesdailysept28_18, 1)
dependency_parsing_func(nytsep25_18, 4)


# PART 4: Fitting a Model




