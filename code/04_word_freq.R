library(tidyverse)
setwd("/Users/lamhine/Documents/GitHub/yelp/data")
rest_revs <- read_csv("rest_revs.csv")

freq_words <- rest_revs %>% 
  mutate(nasty = as.numeric(str_detect(text, "nasty")),
         gross = as.numeric(str_detect(text, "gross")),
         auth = as.numeric(str_detect(text, "authentic")))

freq_words %>% 
  group_by(cuis_cat) %>% 
  summarize(
    nasty_freq = sum(nasty) / n(),
    gross_freq = sum(gross) / n(),
    auth_freq = sum(auth) / n()
  )
    
plot_freq <- freq_words %>% 
  ggplot(aes(x = cuis_cat)) 