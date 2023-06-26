library(jsonlite)
library(tidyverse)
library(tidytext)

# stream in 1 million randomly sampled reviews and all business data
review_data <- stream_in(file("/Users/lamhine/Downloads/archive/yelp-sampled.json"))
businesses <- stream_in(file("/Users/lamhine/Downloads/archive/yelp_academic_dataset_business.json"))

# filter for only restuarants containing American, Chinese, Mexican, or Italian
# in the `categories` variable

restaurants <- businesses %>% 
  filter(str_detect(categories, "Restaurants")) %>% 
  filter(str_detect(categories, "American|Chinese|Mexican|Italian"))

# inner_join with review data 
rest_revs <- inner_join(restaurants, review_data, by = "business_id")

# keep only vars of interest
rest_revs <- rest_revs %>% 
  select(business_id:review_count, 
         categories,
         review_id,
         stars.y,
         text,
         date)

# get counts of words for each cuisine
rest_revs <- rest_revs %>% 
  mutate(cuis_count = str_count(categories, "American|Chinese|Mexican|Italian"))

# filter out restaurants that overlap in multiple of these cuisine categories
rest_uniq <- rest_revs %>% 
  filter(cuis_count == 1)













# unnest tokens and remove stop words
data("stop_words")

tidy_words <- chinese_revs %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# get table of most commonly used words
top_words <- tidy_words %>%
  count(word, sort = TRUE) 

# get sentiments from nrc lexicon
nrc_sent <- get_sentiments("nrc") 

# join with top_words
top_sent <- left_join(top_words, nrc_sent, by = "word")

# plot
top_sent %>% 
  group_by(sentiment) %>% 
  summarize(n = sum(n)) %>% 
  mutate(sentiment = reorder(sentiment, n)) %>% 
  ggplot(aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = F)
