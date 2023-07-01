

library(tidytext)
library(stringr)
library(dplyr)
library(caTools)
library(tidyr)
library(tidytext)

count_common_words <- function(title, common_words) {
  # tokenize the title into individual words
  words <- str_split(title, "\\s+")[[1]]
  
  # count the number of occurrences of each common word
  counts <- sapply(common_words, function(word) sum(words == word))
  
  # return the vector of counts, with zeros included for words not found
  return(ifelse(is.na(counts), 0, counts))
}


# create a dataframe with the titles and their corresponding labels
titles_df <- data.frame(title= c("UAV routing by simulation-based optimization approaches for forest fire risk mitigation",
                          "Vulnerability assessment of storage tanks exposed to simultaneous fire and explosion hazards",
                          "Evaluation of geographically weighted logistic model and mixed effect model in forest fire prediction in northeast China",
                          "Urban fire emergency management based on big data intelligent processing system and Internet of Things",
                          "Wildland-urban interface fires in Norwegian coastal heathlands - Identifying risk reducing measures",
                          "Multifunctional MXene-based fire alarm wallpaper with sandwich-like structure for enhanced fire safety and prevention",
                          "Debris flow occurrence under changing climate and wildfire regimes: A southern California perspective",
                          "Fire hazard vulnerability assessment of shopping centers: Empirical insight from Rangpur City Corporation, Bangladesh",
                          "A comprehensive investigation of the impacts of discovery time and fire brigade response time on life safety and property protection in England",
                          "Coupling wildfire spread simulations and connectivity analysis for hazard assessment: a case study in Serra da Cabreira, Portugal",
                          "Phenolic resin microspheres surface-modified with sodium silicate for reducing fire hazard in TPU composites",
                          "Enhanced Anti-Freezing Heating Cable Standard for Fire Prevention",
                          "Geospatial Wildfire Risk Assessment from Social, Infrastructural and Environmental Perspectives: A Case Study in Queensland Australia",
                          "Fire risk assessment of bridges: from state of the art to structural vulnerability mitigation",
                          "Participatory multi-criteria evaluation of landscape values to inform wildfire management",
                          "Motion analysis of the fire-fighting robot and trajectory correction strategy"),
                        acceptance = rep(1, 16))

# create a tibble with the titles and their corresponding labels
titles_tibble <- tibble(titles_df)

# tokenize each title into individual words
words_tibble <- titles_tibble %>%
  unnest_tokens(word, title)

# remove stop words
words_tibble <- words_tibble %>%
  anti_join(stop_words)

# group the words by acceptance and count their frequency
freq_tibble <- words_tibble %>%
  count(acceptance, word, sort = TRUE)

# get the 30 most common words for acceptance == "YES"
common_words <- freq_tibble %>%
  slice(1:10)


count_common_words(titles_tibble$title[1], common_words$word)

# create a new tibble with the counts for each of the top 30 words
counts_tibble <- t(apply(as.data.frame(titles_tibble), 1, count_common_words, 
      common_words = common_words$word)) %>% 
  as.data.frame() %>% 
  `colnames<-`(paste0("words_in_", colnames(.))) %>% 
  cbind(titles_df, .)


# combine the counts with the original titles and labels
result_tibble <- bind_cols(titles_tibble, counts_tibble)
