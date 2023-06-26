library(tidyverse)
setwd("/Users/lamhine/Documents/GitHub/yelp/data")
rest_revs <- read_csv("rest_revs.csv")

freq_words <- rest_revs %>% 
  mutate(nasty = str_count(text, "nasty"),
         gross = str_count(text, "gross"),
         auth = str_count(text, "authentic"))
    
plot_freq <- freq_words %>% 
  ggplot(aes(x = cuis_cat)) 