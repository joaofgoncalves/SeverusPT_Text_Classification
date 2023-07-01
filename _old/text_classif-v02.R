


library(tidytext)
library(stringr)
library(dplyr)
library(caTools)
library(tidyr)
library(tidytext)
library(readr)
library(tidyverse)
library(randomForest)


## ------------------------------------------------------------------------------------------------ ##


count_common_words <- function(title, common_words) {
  # tokenize the title into individual words
  words <- str_split(title, "\\s+")[[1]]
  
  # count the number of occurrences of each common word
  counts <- sapply(common_words, function(word) as.integer(sum(words == word) > 0))
  
  # return the vector of counts, with zeros included for words not found
  return(ifelse(is.na(counts), 0, counts))
}

count_common_keywords <- function(title, common_words) {
  
  # tokenize the title into individual words
  words <- str_split(gsub(";","",title), "\\s+")[[1]]
  
  # count the number of occurrences of each common word
  counts <- sapply(common_words, function(word) as.integer(sum(words == word) > 0))
  
  # return the vector of counts, with zeros included for words not found
  return(ifelse(is.na(counts), 0, counts))
}


## ------------------------------------------------------------------------------------------------ ##


papers <- read_csv("C:/Users/JG/Downloads/pre_fire_title_screening_v2.csv") %>% 
  mutate(included = ifelse(screened_titles == "selected", 1, 0))

colnames(papers)

train_papers <- papers %>% 
  select(-1, -3) %>% 
  filter(!is.na(screened_titles)) %>% 
  filter(screened_titles %in% c("selected", "excluded")) %>% 
  mutate(included = ifelse(screened_titles == "selected", 1, 0))


tb <- table(train_papers$screened_titles)

(tb / sum(tb))*100




## ------------------------------------------------------------------------------------------------ ##
## Train data only ----
## ------------------------------------------------------------------------------------------------ ##



# tokenize each title into individual words
words_tibble <- train_papers %>%
  select(label, title, included) %>% 
  unnest_tokens(word, title) %>%
  anti_join(stop_words)


# group the words by acceptance and count their frequency
freq_tibble <- words_tibble %>%
  count(included, word, sort = TRUE)

# get the 30 most common words for acceptance == "YES"
common_words_selected_papers <- freq_tibble %>%
  filter(included == 1) %>% 
  slice(1:40)

common_words_unselected_papers <- freq_tibble %>%
  filter(included == 0) %>% 
  filter(!(word %in% common_words_selected_papers$word)) %>% 
  arrange(desc(n)) %>% 
  slice(1:40)


## ------------------------------------------------------------------------------------------------ ##

# tokenize each title into individual words
keywords_tibble <- train_papers %>%
  select(label, keywords, included) %>% 
  unnest_tokens(keyword, keywords) %>%
  anti_join(stop_words, by=c("keyword"="word"))


# group the words by acceptance and count their frequency
keywords_freq_tibble <- keywords_tibble %>%
  count(included, keyword, sort = TRUE)

# get the 30 most common words for acceptance == "YES"
common_keywords_selected_papers <- keywords_freq_tibble %>%
  filter(included == 1) %>% 
  slice(1:40)

common_keywords_unselected_papers <- keywords_freq_tibble %>%
  filter(included == 0) %>% 
  filter(!(keyword %in% common_keywords_selected_papers$keyword)) %>% 
  arrange(desc(n)) %>% 
  slice(1:40)




## ------------------------------------------------------------------------------------------------ ##

# create a new tibble with the counts for each of the top 30 words
counts_sel_words <- t(apply(as.data.frame(train_papers %>% select(title)), 1, count_common_words, 
                         common_words = common_words_selected_papers$word)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("words_sel_", colnames(.)))

# create a new tibble with the counts for each of the top 30 words
counts_unsel_words <- t(apply(as.data.frame(train_papers %>% select(title)), 1, count_common_words, 
                            common_words = common_words_unselected_papers$word)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("words_nsel_", colnames(.)))


## ------------------------------------------------------------------------------------------------ ##


# create a new tibble with the counts for each of the top 30 keywords
counts_sel_keywords <- t(apply(as.data.frame(train_papers %>% select(keywords)), 1, count_common_keywords, 
                            common_words = common_keywords_selected_papers$keyword)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("keywords_sel_", colnames(.)))

# create a new tibble with the counts for each of the top 30 keywords
counts_unsel_keywords <- t(apply(as.data.frame(train_papers %>% select(keywords)), 1, count_common_keywords, 
                              common_words = common_keywords_unselected_papers$keyword)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("keywords_nsel_", colnames(.)))



## ------------------------------------------------------------------------------------------------ ##
## All data ----
## ------------------------------------------------------------------------------------------------ ##

# create a new tibble with the counts for each of the top 30 words
counts_sel_words_all <- t(apply(as.data.frame(papers %>% select(title)), 1, count_common_words, 
                            common_words = common_words_selected_papers$word)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("words_sel_", colnames(.)))

# create a new tibble with the counts for each of the top 30 words
counts_unsel_words_all <- t(apply(as.data.frame(papers %>% select(title)), 1, count_common_words, 
                              common_words = common_words_unselected_papers$word)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("words_nsel_", colnames(.)))


## ------------------------------------------------------------------------------------------------ ##


# create a new tibble with the counts for each of the top 30 keywords
counts_sel_keywords_all <- t(apply(as.data.frame(papers %>% select(keywords)), 1, count_common_keywords, 
                               common_words = common_keywords_selected_papers$keyword)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("keywords_sel_", colnames(.)))

# create a new tibble with the counts for each of the top 30 keywords
counts_unsel_keywords_all <- t(apply(as.data.frame(papers %>% select(keywords)), 1, count_common_keywords, 
                                 common_words = common_keywords_unselected_papers$keyword)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("keywords_nsel_", colnames(.)))


## ------------------------------------------------------------------------------------------------ ##


# combine the counts with the original titles and labels
train_tb <- bind_cols(train_papers %>% select(label, included), 
                      counts_sel_words, 
                      counts_unsel_words,
                      counts_sel_keywords,
                      counts_unsel_keywords)

# combine the counts with the original titles and labels
pred_tb <- bind_cols(papers %>% select(label, included), 
                      counts_sel_words_all, 
                      counts_unsel_words_all,
                      counts_sel_keywords_all,
                      counts_unsel_keywords_all)



## ------------------------------------------------------------------------------------------------ ##
## RF models ----
## ------------------------------------------------------------------------------------------------ ##

## Full model
rf <- randomForest(x = train_tb %>% select(-label, -included),
             y = train_tb %>% pull(included) %>% as.factor(), ntree=1000)

print(rf)

vimp <- importance(rf) %>% 
  as.data.frame() %>% 
  mutate(var_name = rownames(.)) %>% 
  arrange(desc(MeanDecreaseGini))

imp_vars <- vimp$var_name[1:30]



## Selected best var set model
rf1 <- randomForest(x = train_tb %>% select(all_of(imp_vars)),
                   y = train_tb %>% pull(included) %>% as.factor(), ntree=1000)

print(rf1)

plot(rf1)


pred_class <- predict(rf, newdata = pred_tb %>% select(-label, -included))


