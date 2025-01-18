# News Sentiment Analysis Project

## Overview
This project analyzes sentiment patterns in news headlines and articles using multiple sentiment analysis approaches (VADER, AFINN, and TextBlob). The analysis includes topic modeling, temporal trends, and special focus on COVID-19 related content in health and lifestyle categories.

## Features
- Multi-model sentiment analysis using VADER, AFINN, and TextBlob
- Category-based sentiment analysis and visualization
- Temporal trend analysis
- Topic modeling for positive and negative content
- Special focus on COVID-19 impact on health and lifestyle news
- Word cloud visualization for content analysis

## Dependencies
```R
- dplyr
- lubridate
- ggplot2
- readr
- sentimentr
- skimr
- DataExplorer
- jsonlite
- plotly
- tidyr
- tidytext
- tm
- wordcloud2
- text
- RColorBrewer
- viridis
- syuzhet
- topicmodels
```

## Data Preprocessing
The project includes several preprocessing steps:
1. Date format conversion
2. Missing value handling
3. Text cleaning and normalization
4. Category regrouping for better analysis
5. Text combination (headlines + descriptions)

## Analysis Components

### 1. Category Analysis
- Distribution of articles across categories
- Category regrouping for meaningful analysis
- Visualization of category distribution

### 2. Sentiment Analysis
- Implementation of three sentiment models:
  - VADER
  - AFINN
  - TextBlob
- Comparison of sentiment scores across models
- Temporal trend analysis of sentiment
- Category-wise sentiment distribution

### 3. COVID-19 Analysis
- Focused analysis of health and lifestyle content during COVID-19 period
- Sentiment trends before and after COVID-19
- Topic modeling of negative health-related articles

### 4. Topic Modeling
- Separate analysis for positive and negative content
- Word cloud visualization
- Key topic extraction and analysis

## Visualizations
The project includes various visualizations:
- Histogram of sentiment distributions
- Time series plots of sentiment trends
- Box plots of category-wise sentiment
- Word clouds for positive and negative content
- Category distribution plots

## Key Findings
1. Sentiment variation across news categories
2. Impact of COVID-19 on health and lifestyle news sentiment
3. Common topics in positive vs negative news
4. Temporal patterns in news sentiment

## Usage
1. Install required R packages:
```R
install.packages(c("dplyr", "lubridate", "ggplot2", "readr", "sentimentr",
                  "skimr", "DataExplorer", "jsonlite", "plotly", "tidyr",
                  "tidytext", "tm", "wordcloud2", "text", "RColorBrewer",
                  "viridis", "syuzhet", "topicmodels"))
```

2. Load your news data:
```R
news <- read_csv("path_to_your_news_data.csv")
```

3. Run the analysis scripts in sequence as provided in the main code file.

