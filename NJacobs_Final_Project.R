library(tidyverse)
library(rworldmap)
library(shiny)
library(plotly)
library(tidytext)
library(textdata)
library(SnowballC)
library(udpipe)
library(ggraph)
library(igraph)
library(janitor)
library(stargazer)
library(modelr)
library(spData)
library(scales)
library(countrycode)
library(StandardizeText)
library(sf)
library(fastDummies)
library(plm)
library(RColorBrewer)
library(classInt)
library(scales)
library(grid)
library(directlabels)
library(ggthemes)
library(wesanderson)
library(kableExtra)

#Grading comments:
# - Good use of tidyverse and dplyr
# - Good use of comments that explain why things are being done
# - I like your sentiments function


setwd("/Users/Nate/Desktop/Graduate School/Courses/Second Year/Winter Quarter/Data and Programming II/Final Project/D-P-II-Final-Project")

rm(list = ls())

options(scipen = 999)

# PART 1: Data wrangling

# Reading in trade data
us_fdi <- read_csv("US_FdiFlowsStock_ST202007161100_v1.csv")
us_ex_to_china_2017 <- read_csv("US_Ex_to_China_OEC_2017.csv")
us_ex_to_china_2018 <- read_csv("US_Ex_to_China_OEC_2018.csv")
us_ex_to_china_2019 <- read_csv("US_Ex_to_China_OEC_2019.csv")
us_ex_to_china_2020 <- read_csv("US_Ex_to_China_OEC_2020.csv")
us_im_from_china_2017 <- read_csv("US_Im_from_China_OEC_2017.csv")
us_im_from_china_2018 <- read_csv("US_Im_from_China_OEC_2018.csv")
us_im_from_china_2019 <- read_csv("US_Im_from_China_OEC_2019.csv")
us_im_from_china_2020 <- read_csv("US_Im_from_China_OEC_2020.csv")
country_codes <- read_csv("country_codes_V202102.csv")
product_codes <- read_csv("product_codes_HS17_V202102.csv")
gdp_data <- read_csv("2b68fee6-4c0f-4592-a63f-ff85acd68db3_Data.csv")
world_ex_im <- read_csv("f4dc9c88-9144-422d-aa34-42550a9c844f_Data.csv")

# Tidying data on Foreign Direct Investment into and out of the US
str(us_fdi)

us_fdi <- us_fdi %>%
  subset(select = -c(9, 11, 13, 15, 17))

us_fdi <- clean_names(us_fdi)

us_fdi$economy_label <- standardize.countrynames(us_fdi$economy_label, standard = "iso", suggest = "auto") # Standardizing country names. Citation: https://cran.r-project.org/web/packages/StandardizeText/StandardizeText.pdf

#write_csv(us_fdi, "us_fdi.csv")

# Citation: https://medium.com/coinmonks/merging-multiple-dataframes-in-r-72629c4632a3
exports_merged <- do.call("rbind", list(us_ex_to_china_2017, us_ex_to_china_2018, us_ex_to_china_2019, us_ex_to_china_2020))
imports_merged <- do.call("rbind", list(us_im_from_china_2017, us_im_from_china_2018, us_im_from_china_2019, us_im_from_china_2020))

exports_merged <- exports_merged %>%
  mutate(type = "Export--US to China")

imports_merged <- imports_merged %>%
  mutate(type = "Import--US from China")

us_china_totals <- rbind(exports_merged, imports_merged)

us_china_totals <- clean_names(us_china_totals)

str(us_china_totals)

us_china_totals$time <- as.character(us_china_totals$time)

#write_csv(us_china_totals,"us_china_totals.csv")

# Tidying and reshaping GDP data
gdp_data_tidy <- gdp_data %>%
  select(-"Series Code")

gdp_data_tidy <- gdp_data_tidy %>%
  clean_names()

gdp_data_tidy$country_name <- standardize.countrynames(gdp_data_tidy$country_name, standard = "iso", suggest = "auto")

gdp_data_tidy <- gdp_data_tidy %>% # Citation: https://stackoverflow.com/questions/58837773/pivot-wider-issue-values-in-values-from-are-not-uniquely-identified-output-w
  group_by(series_name) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = series_name, values_from = c(x2017_yr2017, x2018_yr2018, x2019_yr2019, x2020_yr2020)) %>%
  select(-row)

gdp_data_tidy <- gdp_data_tidy %>%
  select(-c(8, 14, 20:26))

gdp_data_tidy <- gdp_data_tidy %>%
  rename(
    "2017_gdp" = 3, "2017_gdpgrowth" = 4,
    "2017_gdppercap2010usd" = 5, "2017_gdppercapcurrentusd" = 6,
    "2017_percenttradegpd" = 7,
    "2018_gdp" = 8, "2018_gdpgrowth" = 9,
    "2018_gdppercap2010usd" = 10, "2018_gdppercapcurrentusd" = 11,
    "2018_percenttradegpd" = 12,
    "2019_gdp" = 13, "2019_gdpgrowth" = 14,
    "2019_gdppercap2010usd" = 15, "2019_gdppercapcurrentusd" = 16,
    "2019_percenttradegpd" = 17
  )

gdp_part <- gdp_data_tidy %>% pivot_longer(
  cols = starts_with(c("2017", "2018", "2019")),
  names_to = c("year"),
  values_to = c("value"),
  values_drop_na = TRUE
)

gdp_part$value <- as.numeric(gdp_part$value)

gdp_part <- gdp_part %>%
  separate(
    col = year,
    into = c("year", "measure"),
    sep = "_"
  ) %>%
  filter(!is.na(value))

gdp_part <- gdp_part %>%
  group_by(country_name, year, measure) %>%
  summarise(value = sum(value))

gdp_part <- gdp_part %>%
  pivot_wider(names_from = measure, values_from = value)

gdp_part[complete.cases(gdp_part), ] # Checking for NAs

gdp_part[is.na(gdp_part)] <- 0

gdp_final <- gdp_part

gdp_final$country_name <- standardize.countrynames(gdp_final$country_name, standard = "iso", suggest = "auto")

#write_csv(gdp_final, "gdp_final.csv")

# Reshaping export-import data
world_ex_im_tidy <- world_ex_im %>%
  pivot_longer(
    cols = starts_with(c("2017", "2018", "2019")),
    names_to = c("year"),
    values_to = c("value"),
    values_drop_na = TRUE
  )

world_ex_im_tidy <- clean_names(world_ex_im_tidy)

world_ex_im_tidy <- world_ex_im_tidy %>%
  select(-series_code)

world_ex_im_tidy$country_name <- standardize.countrynames(world_ex_im_tidy$country_name, standard = "iso", suggest = "auto")

world_ex_im_tidy$year <- str_extract(world_ex_im_tidy$year, "20..")

world_ex_im_tidy <- world_ex_im_tidy %>%
  pivot_wider(names_from = series_name, values_from = value)

world_ex_im_tidy <- clean_names(world_ex_im_tidy)

world_ex_im_tidy$exports_of_goods_and_services_current_us <- as.numeric(world_ex_im_tidy$exports_of_goods_and_services_current_us)
world_ex_im_tidy$imports_of_goods_and_services_bo_p_current_us <- as.numeric(world_ex_im_tidy$imports_of_goods_and_services_bo_p_current_us)

# world_ex_im_tidy <- na.omit(world_ex_im_tidy) # Omitting NAs, because it is unclear what to impute for them
world_ex_im_tidy[is.na(world_ex_im_tidy)] <- 0

# Joining world export/import data to GDP data, on country-year
world_trade_final <- gdp_final %>%
  inner_join(world_ex_im_tidy, by = c("country_name" = "country_name", "year" = "year"))

# Computing trade openness, a measure of trade volume normalized by GDP
world_trade_final <- world_trade_final %>%
  mutate(trade_openness = ((exports_of_goods_and_services_current_us + imports_of_goods_and_services_bo_p_current_us) / gdp))

# Adding dummies for years, to account for year fixed effects in the model
world_trade_final <- world_trade_final %>%
  mutate(is_2017 = ifelse(year == 2017, 1, 0)) %>%
  mutate(is_2018 = ifelse(year == 2018, 1, 0)) %>%
  mutate(is_2019 = ifelse(year == 2019, 1, 0))

# Joining FDI data to tidied export-import data, again on country-year
us_fdi$year <- as.character(us_fdi$year)
us_fdi <- us_fdi %>%
  filter(direction_label == "Inward", mode_label == "Flow", year %in% c("2017", "2018", "2019")) # Limiting to inbound FDI for 2017-2019

us_fdi_for_join <- us_fdi %>% # Eliminating unnecessary columns
  select(1, 3, 8:11)

world_trade_final <- world_trade_final %>%
  inner_join(us_fdi_for_join, by = c("country_name" = "economy_label", "year" = "year"))

world_trade_final <- world_trade_final %>%
  filter(country_name != "World")

# Adding log-transformations to some of the variables, for model interpretation purposes
world_trade_final <- world_trade_final %>%
  mutate(ln_gdp = log(gdp), ln_gdp_per_cap_current = log(gdppercapcurrentusd), ln_gdp_per_cap2010 = log(gdppercap2010usd))

# Adding quadratic terms to some of the variables, again for model interpretation purposes
world_trade_final <- world_trade_final %>%
  mutate(gdppercapcurrentusd_sq = gdppercapcurrentusd^2, gdppercap2010usd = gdppercap2010usd^2)

world_trade_final <- world_trade_final %>%
  select(-c(17:18))

# Updating some column names to make sure they're understandable
world_trade_final <- world_trade_final %>%
  rename(value_of_inbound_fdi_mil_usd = us_dollars_at_current_prices_in_millions) %>%
  rename(value_of_inbound_fdi_usd_per_cap = us_dollars_at_current_prices_per_capita)

# Removing non-countries from the data
world_trade_final <- world_trade_final %>%
  filter(country_name != "Euro area" & country_name != "Sub-Saharan Africa")

is.na(world_trade_final) <- sapply(world_trade_final, is.infinite) # Imputing 0's for NaN
world_trade_final[is.na(world_trade_final)] <- 0

# Adding dummy columns for entity fixed effects--panel data set complete
world_trade_final <- dummy_cols(world_trade_final, select_columns = c("country_name"))

world_trade_final <- clean_names(world_trade_final)

#write_csv(world_trade_final, "world_trade_final.csv")

# Adding rows for missing countries
tribble(
  ~country_name, ~year, ~gdp, ~gdpgrowth, ~gdppercap2010usd, ~gdppercapcurrentusd, ~percenttradegdp, ~country_code, ~exports_of_goods_and_services_current_us, ~imports_of_goods_and_services_bo_p_current_us,
  ~trade_openness,
  "Libya", 2017, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

# PART 2: Plotting data

# STATIC PLOTS--interactive plot in app.R file
# A choropleth showing inbound FDI in 2017. Citation: https://journal.r-project.org/archive/2011-1/RJournal_2011-1_South.pdf
world2 <- world

world2$name_long <- standardize.countrynames(world2$name_long, standard = "iso", suggest = "auto")

world_trade_mapping_17 <- world2 %>%
  left_join(world_trade_final, by = c("name_long" = "country_name")) %>%
  filter(year == "2017")

world_trade_mapping_18 <- world2 %>%
  inner_join(world_trade_final, by = c("name_long" = "country_name")) %>%
  filter(year == "2018")

world_trade_mapping_19 <- world2 %>%
  inner_join(world_trade_final, by = c("name_long" = "country_name")) %>%
  filter(year == "2019")

inbound_fdi_2017 <- ggplot() +
  geom_sf(data = world_trade_mapping_17, aes(fill = value_of_inbound_fdi_mil_usd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(low = "red", high = "green", labels = dollar_format()) +
  ggtitle("Inbound FDI in 2017, Millions of USD") +
  theme(legend.text = element_text(size = 5), legend.title = element_blank(), legend.position = "top", panel.background = element_rect(fill = "lightblue"))

print(inbound_fdi_2017) #Could not add missing countries in time


# Another plot, showing trade openness
world_avg <- grobTree(textGrob("World Average = 0.79", vjust = -10, gp = gpar(col = "dodgerblue4", fontsize = 14, fontface = "bold")))

us_china_trade_openness_plot <- world_trade_final %>%
  filter(country_name == "United States" | country_name == "China") %>%
  ggplot() +
  geom_col(aes(x = year, y = trade_openness, fill = trade_openness)) +
  geom_text(aes(x = year, y = trade_openness, label = round(trade_openness, 2), vjust = -1), color = "chocolate4") +
  scale_fill_gradient(low = "red", high = "orange") +
  xlab("Year") +
  ylab("Ratio of Trade Volume to GDP") +
  ylim(0, 0.5) +
  ggtitle("Comparing US and Chinese Trade Openness, 2017-19") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "linen", color = "linen"), legend.position = "none") +
  facet_wrap(~country_name) +
  annotation_custom(world_avg)

print(us_china_trade_openness_plot)

# Another plot: Product categories
us_china_totals %>%
  filter(type == "Export--US to China", time == "2017") %>%
  group_by(section) %>%
  summarise(total_volume = sum(trade_value)) %>%
  arrange(desc(total_volume)) %>%
  head(10) # Determining the ten most heavily-exported product categories from US to China in 2017, the baseline year

us_china_totals_plotting <- us_china_totals %>% # Isolating those product categories to prepare them for ggplot
  filter(type == "Export--US to China", time != "2020", section %in% c(
    "Transportation", "Machines", "Vegetable Products", "Chemical Products", "Mineral Products",
    "Instruments", "Plastics and Rubbers", "Metals", "Paper Goods", "Wood Products"
  )) %>%
  group_by(section, time) %>%
  summarise(total_volume = sum(trade_value))

us_exports_product_categories_17_19 <- us_china_totals_plotting %>% # Citation for data labeling: https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines
  ggplot() +
  geom_line(aes(x = time, y = total_volume, group = section, color = section)) +
  geom_point(aes(x = time, y = total_volume, fill = total_volume)) +
  geom_dl(aes(x = time, y = total_volume, label = section), method = list(dl.combine("last.points"), cex = 0.50, vjust = -0.75)) +
  xlab("Year") +
  ylab("Total Value of US Exports to China") +
  theme_bw() +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_brewer(palette = "Spectral") +
  ggtitle("Value of US Exports to China, Top 10 2017 Categories (USD)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "linen"), legend.position = "none")

print(us_exports_product_categories_17_19)

## PART 3: NLP Section
# Reading in articles, and conducting sentiment analysis on the first NYT article
nyt2021 <- read_file("NYT2021.txt")
peoplesdailymarch4_21 <- read_file("PeoplesDailyMarch4.txt")
peoplesdailysept28_18 <- read_file("PeoplesDailySept28_18.txt")
nytsep25_18 <- read_file("NYTSept25_18.txt")


text_df <- tibble(text = nyt2021)
word_tokens_df <- unnest_tokens(text_df, word_tokens, text, token = "words")
word_tokens_df_nsw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

count(word_tokens_df_nsw, word_tokens, sort = TRUE)

# GENERALIZING: Writing a function to produce a plot of sentiments
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
      geom_text(stat = "count", aes(nrc, label = ..count..), vjust = -0.65, size = 2.5) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(title = ("Sentiments Expressed by NYT and People's Daily")) +
      xlab("Sentiment") +
      ylab("Count of Words") +
      theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    return(sentiment_plot)
  }
}

sentiments_function(nyt2021)
sentiments_function(peoplesdailymarch4_21)
sentiments_function(peoplesdailysept28_18)
sentiments_function(nytsep25_18)

# Conducting more advanced NLP
word_tokens_df_nsw$stem <- wordStem(word_tokens_df_nsw$word_tokens, language = "porter")

word_tokens_df_nsw %>%
  dplyr::group_by(stem) %>%
  count(sort = TRUE)

parsed_nyt <- udpipe(nyt2021, "english")

parsed_nyt$stem <- wordStem(parsed_nyt$token, language = "porter")

# view(select(parsed_nyt, "token", "stem", "lemma", "upos")) commenting out the view() command

# Conducting dependency parsing on the first NYT article
nyt_deps <- cbind_dependencies(parsed_nyt, type = "parent_rowid", recursive = TRUE)

#nyt_deps %>%
 # select(c(token_id, token, upos, dep_rel, parent_rowid, parent_rowids)) %>%
  #View()

# Plotting
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


# GENERALIZING: Writing a function to do dependency parsing on any given sentence in any given article.
# The point here is to determine whether there are differences in the sentence structures of the NYT and People's Daily articles.
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

# Examining the first sentence of each article
dependency_parsing_func(peoplesdailymarch4_21, 1)
dependency_parsing_func(nyt2021, 1)
dependency_parsing_func(peoplesdailysept28_18, 1)
dependency_parsing_func(nytsep25_18, 1)


# PART 4: Fitting a Model: How did US FDI change for China and the US from before and after the launch of the trade war, controlling for GDP?
# prelim_model <- lm(value_of_inbound_fdi_mil_usd ~ gdp + gdppercapcurrentusd + ln_gdp + ln_gdp_per_cap_current + gdppercapcurrentusd_sq + is_2018 + is_2019 + trade_openness, data = world_trade_final)
trade_fixed_effects <- lm(trade_openness ~ . + country_name_china * is_2018 + country_name_united_states*is_2018 + country_name_china * is_2019 + country_name_united_states * is_2019 - is_2018 -
  country_name_zimbabwe - country_name - year - exports_of_goods_and_services_current_us -
  imports_of_goods_and_services_bo_p_current_us - country_code - ln_gdp -
  ln_gdp_per_cap_current - gdp - gdppercap2010usd - ln_gdp_per_cap2010 - gdppercapcurrentusd, data = world_trade_final)

stargazer(trade_fixed_effects, type = "text", order = c("is_2019:country_name_united_states", "is_2019:country_name_china", "country_name_united_states", "country_name_china"), omit = c(10:549)) # Individual countries other than the US and China omitted to keep output table manageable
