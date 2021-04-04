#prediction code

#Load into variables, repos files with NGram Information
bwords <- readRDS("bigram_words.rds")
twords  <- readRDS("trigram_words.rds")
qwords <- readRDS("quadgram_words.rds")

# Bigram match function
bigram <- function(input_words){
  num <- length(input_words)
  filter(bwords, 
         word1==input_words[num]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "NO WORD FOUND", return(out))
}

# Trigram match function
trigram <- function(input_words){
  num <- length(input_words)
  filter(twords, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

# Quadgram match function
quadgram <- function(input_words){
  num <- length(input_words)
  filter(qwords, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}

# 
prediction <- function(input){
  #Dataframe conversion
  input <- data_frame(text = input)
  
  # Space suppress
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  
  #Obtain the list of word suppied by user
  input_words <- unlist(str_split(input, boundary("word")))
  
  #We have the repos in lowe case
  input_words <- tolower(input_words)
  
  #Obtaining number of word supplied by user
  number_words <- str_count(input, boundary("word"))
  
  # call the functions in order to find the predicted word
  predict <- ifelse(number_words == 0, "You must put a sentence",
                ifelse(number_words == 3, quadgram(input_words),
                       ifelse(number_words == 2, trigram(input_words), bigram(input_words))))
  

  return(predict)
}

