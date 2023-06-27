library(jsonlite)
library(tidyverse)
library(tidytext)

# stream in 1 million randomly sampled reviews and all business data
review_data <- stream_in(file("/Users/lamhine/Downloads/archive/yelp-sampled.json"))
businesses <- stream_in(file("/Users/lamhine/Downloads/archive/yelp_academic_dataset_business.json"))

# filter for only restuarants containing American, Chinese, Mexican, or Italian
# in the `categories` variable - note we need to overwrite "Latin American" 
# create a MECE cuisine category and
# remove all reviews that are blank (no text)

restaurants <- businesses %>% 
  mutate(categories = str_remove(categories, "Latin American")) %>% 
  filter(str_detect(categories, "Restaurants")) %>%
  filter(str_detect(categories, "American|Chinese|Mexican|Italian")) %>% 
  mutate(cuis_count = str_count(categories, "American|Chinese|Mexican|Italian")) %>% 
  filter(cuis_count == 1) %>% 
  mutate(cuis_cat = case_when(
    str_detect(categories, "American") ~ "American",
    str_detect(categories, "Chinese") ~ "Chinese",
    str_detect(categories, "Mexican") ~ "Mexican",
    str_detect(categories, "Italian") ~ "Italian"
  ))

# inner_join with review data and flatten 
rest_revs <- flatten(inner_join(restaurants, review_data, by = "business_id"))

# write out .csv
write_csv(rest_revs, 
          file.path(dir = "/Users/lamhine/Documents/GitHub/yelp/data/",
          "rest_revs.csv.gz"))

