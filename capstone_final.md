---
title: "MILESTONE"
author: "Jesus"
date: "27/1/2021"
output:
  html_document:
    
    keep_md: yes
  md_document: default
  pdf_document: default
---




# 1 - Introduction   
The goal of this project is just to display that you've gotten used to working with the data and that you are on track to create your prediction algorithm. Please submit a report on [R Pubs](http://rpubs.com/)^[You can create an account on [RPubs.com](http://rpubs.com/), [ShinyApps.io](http://www.shinyapps.io) and [®StudioConnect.com](https://beta.rstudioconnect.com/) to publish your reports or shiny apps] that explains your exploratory analysis and your goals for the eventual app and algorithm. This document should be concise and explain only the major features of the data you have identified and briefly summarize your plans for creating the prediction algorithm and [Shiny app](http://shiny.rstudio.com/) in a way that would be understandable to a non-data scientist manager. You should make use of tables and plots to illustrate important summaries of the data set. The motivation for this project is to:

  1. Demonstrate that you've downloaded the data and have successfully loaded it in.
  2. Create a basic report of summary statistics about the data sets.
  3. Report any interesting findings that you amassed so far.
  4. Get feedback on your plans for creating a prediction algorithm and Shiny app.

  Review criteria:

  1. Does the link lead to an HTML page describing the exploratory analysis of the training data set?
  2. Has the data scientist done basic summaries of the three files? Word counts, line counts and basic data tables?
  3. Has the data scientist made basic plots, such as histograms to illustrate features of the data?
  4. Was the report written in a brief, concise style, in a way that a non-data scientist manager could appreciate?




# 2 - Preparation of Data

## 2.1 -  Downloading File and Unzip

The file once is unziped , has an english folder that contais three files: **en_US.blogs.txt**, **en_US.news.txt**, and **en_US.twitter.txt**.


```r
if(!file.exists("Coursera-SwiftKey.zip")) {
      download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
      unzip("Coursera-SwiftKey.zip")
}
```

## 2.2 -  Extracting the lines

We extract the lines of each of the three files:


```r
# The name of the files Files:

blogs_file   <- "./final/en_US/en_US.blogs.txt"
news_file    <- "./final/en_US/en_US.news.txt"
twitter_file <- "./final/en_US/en_US.twitter.txt" 

#Lines extraction
blogs_lines   <- readLines(blogs_file, skipNul = TRUE)
news_lines    <- readLines(news_file,  skipNul = TRUE)
```

```
## Warning in readLines(news_file, skipNul = TRUE): incomplete final line found on
## './final/en_US/en_US.news.txt'
```

```r
twitter_lines <- readLines(twitter_file, skipNul = TRUE)

#Conversion into dataframe
blogs_lines   <- data_frame(text = blogs_lines)
```

```
## Warning: `data_frame()` was deprecated in tibble 1.1.0.
## Please use `tibble()` instead.
```

```r
news_lines    <- data_frame(text = news_lines)
twitter_lines <- data_frame(text = twitter_lines)
```

## 2.3 -  Sample an fusion of the data

In order to manipulate the data, we must sample it for each file using the sample_n function. Finally we  make one only file with the union of the three files :


```r
set.seed(1001)
pct <- 0.05 #sample reduction %

blogs_sample_lines <- blogs_lines %>% sample_n(., nrow(blogs_lines)*pct)
news_sample_lines <- news_lines %>% sample_n(., nrow(news_lines)*pct)
twitter_sample_lines <- twitter_lines %>% sample_n(., nrow(twitter_lines)*pct)

#Sample union
sample <- bind_rows(mutate(blogs_sample_lines, source = "blogs_lines"),
                         mutate(news_sample_lines,  source = "news_lines"),
                         mutate(twitter_sample_lines, source = "twitter_lines")) 
sample$source <- as.factor(sample$source)
```
## 2.4 -  Tidy and clean the information

We clean the information, eliminating/substituting non desired characters that won't help us in our prediction final purpose:


```r
urls <- "http[^[:space:]]*"
spaces <- "[^[:alpha:][:space:]]*"
rest <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  


final_sample <-  sample %>%
  mutate(text = str_replace_all(text, spaces, "")) %>%
  mutate(text = str_replace_all(text, urls, "")) %>%
  mutate(text = str_replace_all(text, rest, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT")) #English
```
## 2.5 -  NGrams information

We obtain from the final cleaned sample, the n grams information that will let us the prediction analysis, using  unnest_tokens funtion. In order to obtain a faster algorythm we use only the 4th fist grams (order 2 , 3 and 4) :


```r
#' 2 grams  
bigrams <- final_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' 3 grams  
trigrams <- final_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' 4 grams  
quadgrams <- final_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)
```
## 2.6 -  NGrams reduction

The N grams information is reduced in order to make fast the prediction algorythm. Only the upper half zone is taking into account:


```r
bigram_red <- bigrams %>% count(bigram) %>%  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  mutate(coverage = cumsum(proportion)) %>% filter(coverage <= 0.5)


trigram_red <- trigrams %>% count(trigram) %>%  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  mutate(coverage = cumsum(proportion)) %>% filter(coverage <= 0.5)


quadgram_red <- quadgrams %>% count(quadgram) %>%  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  mutate(coverage = cumsum(proportion)) %>% filter(coverage <= 0.5)
```
## 2.6 -  NGrams save

The information of the 2,3 4 Grams , is separated in words columns an saved in files (function saveRDS) in order to be used by the ShinyApp later:


```r
bigram_words <- bigram_red %>% separate(bigram, c("word1", "word2"), sep = " ")
trigram_words <- trigram_red %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
quadgram_words <- quadgram_red %>% separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

saveRDS(bigram_words, "bigram_words.rds")
saveRDS(trigram_words, "trigram_words.rds")
saveRDS(quadgram_words, "quadgram_words.rds")
```

