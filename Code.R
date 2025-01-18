
install.packages(c("dplyr","lubridate","ggplot2","readr","sentimentr",
  "skimr","DataExplorer","jplotly","tidyr","tidytext","tm",
  "wordcloud2","text","RColorBrewer","viridis"
))


library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(sentimentr)
library(skimr)
library(DataExplorer)
library(jsonlite)
library(plotly)
library(tidyr)
library(tidytext)
library(tm)
library(wordcloud2)
library(text)
library(RColorBrewer)
library(viridis)

# Import the CSV file
news <- read_csv("news.csv")

# Check the first few rows
head(news)
# Check the structure of the dataset
str(news)
# Check the structure of the dataset
str(news)
# Convert the 'date' column to Date format
news <- news %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Remove rows with missing values in critical columns
news <- news %>%
  filter(!is.na(headline), !is.na(category))

# Convert all text in 'headline' to lowercase
news <- news %>%
  mutate(headline = tolower(headline))

# Check for missing values after cleaning
colSums(is.na(news))

# Preview the cleaned dataset
head(news)

##==========================
# EDA
#====================

# Counting the number of articles in each category
category_count <- news %>%
  count(category, sort = TRUE)
print(category_count)

# Visualize the distribution of articles per category
ggplot(category_count, aes(x = reorder(category, n), y = n, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of Articles per Category", x = "Category", y = "Number of Articles") +
  theme_minimal()

# Calculating the length of each headline
news <- news %>%
  mutate(headline_length = nchar(headline))

# Visualizing the distribution of headline lengths
ggplot(news, aes(x = headline_length)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Headline Lengths", x = "Headline Length (Characters)", y = "Count") +
  theme_minimal()
# Checking the earliest and latest dates
summary(news$date)

# Visualizing the number of articles published over time
ggplot(news, aes(x = date)) +
  geom_histogram(binwidth = 30, fill = "lightgreen", color = "black") +
  labs(title = "Number of Articles Published Over Time", x = "Date", y = "Count") +
  theme_minimal()

colSums(is.na(news))
summary(news)
unique(news$category)

#========================
#recategory
#=============

news <- news %>%
  mutate(category = tolower(category))
category_count <- news %>%
  count(category, sort = TRUE)
total_articles <- sum(category_count$n)

# Calculating the percentage of articles in each category
category_count <- category_count %>%
  mutate(Percentage = (n / total_articles) * 100)

# Identifying categories with more than 5% and less than 1% of total articles
categories_above_5 <- category_count %>% filter(Percentage > 5) %>% pull(category)
categories_below_1 <- category_count %>% filter(Percentage < 1) %>% pull(category)

# Defining meaningful groups for recategorization
group_mapping <- c(
  "parenting" = "Health & Family",
  "healthy living" = "Health & Family",
  "queer voices" = "Social Justice",
  "food & drink" = "Lifestyle",
  "business" = "Economy & Business",
  "comedy" = "Entertainment & Media",
  "sports" = "Sports",
  "black voices" = "Social Justice",
  "home & living" = "Lifestyle",
  "parents" = "Health & Family",
  "the worldpost" = "World News",
  "weddings" = "Lifestyle",
  "women" = "Social Justice",
  "crime" = "Crime & Justice",
  "impact" = "Social Impact",
  "divorce" = "Lifestyle",
  "world news" = "World News",
  "media" = "Entertainment & Media",
  "weird news" = "Entertainment & Media",
  "green" = "Environment & Science",
  "worldpost" = "World News",
  "religion" = "Culture & Religion",
  "style" = "Arts & Culture",
  "science" = "Environment & Science",
  "tech" = "Technology",
  "taste" = "Lifestyle"
)

# Applying the group mapping to create a new grouped category column
news <- news %>%
  mutate(category_grouped = case_when(
    category %in% names(group_mapping) ~ group_mapping[category],
    category %in% categories_above_5 ~ category,
    category %in% categories_below_1 ~ "Remove",
    TRUE ~ "Other"
  )) %>%
  filter(category_grouped != "Remove")

category_grouped_count <- news %>%
  count(category_grouped, sort = TRUE)
print(category_grouped_count)

# Visualize the new grouped category
ggplot(category_grouped_count, aes(x = reorder(category_grouped, n), y = n, fill = category_grouped)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of Articles per Grouped Category", x = "Grouped Category", y = "Number of Articles") +
  theme_minimal()


#=============================================
# handle missing values for short description
#================================================
colSums(is.na(news))

# checking for missing values in headline and short_description
missing_headline <- sum(is.na(news$headline))
missing_description <- sum(is.na(news$short_description))

news <- news %>%
  mutate(short_description = ifelse(is.na(short_description), "", short_description))
colSums(is.na(news))

# Combining headline and short_description into a new column (full_text)
news <- news %>%
  mutate(full_text = paste(headline, short_description, sep = " "))

##================================
# sentimental analysis
#==============================

# Load necessary libraries
library(dplyr)
library(stringr)
library(tm)

# Create the 'full_text' column by combining 'headline' and 'short_description'
news <- news %>%
  mutate(full_text = paste(headline, short_description, sep = " "))

# Define a text cleaning function
clean_text <- function(text) {
  text %>%
    str_to_lower() %>%                         # Convert to lowercase
    str_replace_all("http\\S+|www\\S+", "") %>% # Remove URLs
    str_replace_all("[^a-z\\s]", "") %>%        # Remove special characters and numbers
    str_squish()                                # Remove extra whitespace
}

#Apply the cleaning function to the 'full_text' column
news <- news %>%
  mutate(cleaned_text = clean_text(full_text))


# VADER Sentiment Analysis on cleaned text
# Use get_sentences() to split text into sentences before applying sentiment_by()
news <- news %>%
  mutate(sentences = get_sentences(cleaned_text)) %>%
  mutate(vader_sentiment = sentiment_by(sentences)$ave_sentiment)

# Check the first few rows
head(news)


#
# Add an ID column to the original news dataframe
news <- news %>%
  mutate(id = row_number())
# Load the AFINN lexicon
afinn <- get_sentiments("afinn")

# Tokenize the cleaned text and calculate AFINN sentiment scores
news_afinn <- news %>%
  unnest_tokens(word, cleaned_text) %>%
  inner_join(afinn, by = "word") %>%
  group_by(id) %>%
  summarize(afinn_sentiment = sum(value, na.rm = TRUE))
# Merge the AFINN sentiment scores back to the original dataframe using the ID column
news <- news %>%
  left_join(news_afinn, by = "id")

# Check the first few rows
head(news)


# TextBlob Sentiment Analysis on cleaned text
# Install the syuzhet package
install.packages("syuzhet")

# Load the package
library(syuzhet)
# Perform TextBlob Sentiment Analysis using syuzhet
news <- news %>%
  mutate(textblob_sentiment = get_sentiment(cleaned_text, method = "syuzhet"))

# Check the first few rows
head(news)

#==================================================
#Comparing Sentiment Models
#=================================================
# Calculate correlations between sentiment scores
correlation_matrix <- cor(news %>% select(vader_sentiment, afinn_sentiment, textblob_sentiment), use = "complete.obs")
# Print the correlation matrix
print(correlation_matrix)


# Plot histogram for each sentiment model
ggplot(news, aes(x = vader_sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "VADER Sentiment Score Distribution", x = "Sentiment Score", y = "Count")

ggplot(news, aes(x = afinn_sentiment)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "AFINN Sentiment Score Distribution", x = "Sentiment Score", y = "Count")

ggplot(news, aes(x = textblob_sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "TextBlob Sentiment Score Distribution", x = "Sentiment Score", y = "Count")



# Calculate average sentiment over time for each model
sentiment_trends <- news %>%
  group_by(date) %>%
  summarize(
    avg_vader = mean(vader_sentiment, na.rm = TRUE),
    avg_afinn = mean(afinn_sentiment, na.rm = TRUE),
    avg_textblob = mean(textblob_sentiment, na.rm = TRUE)
  )

# Plot sentiment trends over time
ggplot(sentiment_trends, aes(x = date)) +
  geom_line(aes(y = avg_vader, color = "VADER")) +
  geom_line(aes(y = avg_afinn, color = "AFINN")) +
  geom_line(aes(y = avg_textblob, color = "TextBlob")) +
  labs(title = "Sentiment Trends Over Time", x = "Date", y = "Average Sentiment Score") +
  theme_minimal() +
  scale_color_manual(values = c("VADER" = "blue", "AFINN" = "orange", "TextBlob" = "green"))


# Boxplot of sentiment scores by category
ggplot(news, aes(x = category_grouped, y = vader_sentiment, fill = category_grouped)) +
  geom_boxplot() +
  labs(title = "VADER Sentiment Scores by Category", x = "Category", y = "Sentiment Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(news, aes(x = category_grouped, y = afinn_sentiment, fill = category_grouped)) +
  geom_boxplot() +
  labs(title = "AFINN Sentiment Scores by Category", x = "Category", y = "Sentiment Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(news, aes(x = category_grouped, y = textblob_sentiment, fill = category_grouped)) +
  geom_boxplot() +
  labs(title = "TextBlob Sentiment Scores by Category", x = "Category", y = "Sentiment Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate mean and standard deviation for each sentiment model
model_comparison <- news %>%
  summarise(
    mean_vader = mean(vader_sentiment, na.rm = TRUE),
    sd_vader = sd(vader_sentiment, na.rm = TRUE),
    mean_afinn = mean(afinn_sentiment, na.rm = TRUE),
    sd_afinn = sd(afinn_sentiment, na.rm = TRUE),
    mean_textblob = mean(textblob_sentiment, na.rm = TRUE),
    sd_textblob = sd(textblob_sentiment, na.rm = TRUE)
  )

print(model_comparison)

#=============================================
# sentiment score over time
#==============================================
news <- news %>% mutate(afinn_sentiment = ifelse(is.na(afinn_sentiment), 0, afinn_sentiment))

ggplot(news, aes(x = category_grouped, y = afinn_sentiment, fill = category_grouped)) +
  geom_boxplot() +
  geom_hline(yintercept = median(news$afinn_sentiment), linetype = "dashed", color = "red") +
  labs(title = "AFINN Sentiment Scores by Category",
       x = "Category",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##=========================================
#trend over time key event covid-19
#============================================

filtered_data <- news %>%
  filter(category_grouped %in% c("Health & Family", "Lifestyle"))
filtered_data <- filtered_data %>%
  mutate(afinn_sentiment = ifelse(is.na(afinn_sentiment), 0, afinn_sentiment))

# Grouping date and category for average sentiment
sentiment_trend <- filtered_data %>%
  group_by(date, category_grouped) %>%
  summarize(avg_afinn = mean(afinn_sentiment, na.rm = TRUE), .groups = "drop")

# Defining the COVID-19 key event
covid_event <- data.frame(
  event = "COVID-19 Pandemic",
  date = as.Date("2020-03-01")
)

ggplot(sentiment_trend, aes(x = date, y = avg_afinn, color = category_grouped)) +
  geom_line(size = 1) +
  geom_vline(data = covid_event, aes(xintercept = as.numeric(date)), linetype = "dashed", color = "red", size = 0.8) +  # Add vertical line for COVID-19
  geom_text(data = covid_event, aes(x = date, y = max(sentiment_trend$avg_afinn, na.rm = TRUE), label = event),
            angle = 90, hjust = -0.1, size = 3, color = "red") +  # Add event label
  labs(
    title = "Sentiment Trends Over Time for Health & Family and Lifestyle",
    x = "Year",
    y = "Average Sentiment Score",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

###=========================================
# Calculate Average Sentiment by Category
#============================================
average_sentiment <- news %>%
  group_by(category_grouped) %>%
  summarise(avg_sentiment = mean(afinn_sentiment, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(avg_sentiment))
print(average_sentiment)

ggplot(average_sentiment, aes(x = reorder(category_grouped, avg_sentiment), y = avg_sentiment, fill = avg_sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Sentiment by Category (AFINN)",
       x = "Category",
       y = "Average Sentiment Score") +
  theme_minimal()

#=======================================================================
# Topic modeling
#======================================================================

# Categorize in Positive and Negative
positive_articles <- news %>%
  filter(afinn_sentiment > 0)
negative_articles <- news %>%
  filter(afinn_sentiment < 0)

# DTM
positive_dtm <- positive_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)
negative_dtm <- negative_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)


install.packages("topicmodels")
library(topicmodels)

# LDA
positive_lda <- LDA(positive_dtm, k = 5, control = list(seed = 1234))
positive_topics <- tidy(positive_lda, matrix = "beta")
negative_lda <- LDA(negative_dtm, k = 5, control = list(seed = 1234))
negative_topics <- tidy(negative_lda, matrix = "beta")

# Top Words in Topics
positive_top_words <- positive_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()
negative_top_words <- negative_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

irrelevant_words <- c("photos", "video", "day","days","city" ,"week", "people", "trump","dont","iam","time")
positive_wordcloud_data <- positive_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% irrelevant_words) %>%
  count(word, sort = TRUE)

wordcloud2(positive_wordcloud_data, size = 0.7, color = "green", backgroundColor = "white")

# Negative Word Cloud
negative_wordcloud_data <- negative_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% irrelevant_words) %>%
  count(word, sort = TRUE)

wordcloud2(negative_wordcloud_data, size = 0.7, color = "red", backgroundColor = "white")

#==========================
# covid-19
#==============================
#Define COVID Period
covid_start <- as.Date("2020-01-01")
covid_end <- as.Date("2021-12-31")

# Filtering data for COVID period
covid_data <- news %>%
  filter(date >= covid_start & date <= covid_end)

#Extracting Negative Articles in "Health & Family" Category
negative_health_articles <- covid_data %>%
  filter(category_grouped == "Health & Family" & afinn_sentiment < 0)

#Creatting DTM for Negative Health Articles
#Creating Document-Term Matrix
negative_health_dtm <- negative_health_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

#Applying LDA for Topic Modeling
library(topicmodels)

# Apply LDA for Negative Health Articles
negative_health_lda <- LDA(negative_health_dtm, k = 5, control = list(seed = 1234))
negative_health_topics <- tidy(negative_health_lda, matrix = "beta")

# Extract Top Words in Negative Health Topics
negative_health_top_words <- negative_health_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

# Define irrelevant words to filter out
irrelevant_words <- c("photos", "video", "day", "week", "people", "trump","youre")

negative_health_wordcloud_data <- negative_health_articles %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% irrelevant_words) %>%
  count(word, sort = TRUE)

wordcloud2(negative_health_wordcloud_data, size = 0.7, color = "red", backgroundColor = "white")
library(ggplot2)


