library(tidyverse)
setwd("/Users/lamhine/Documents/GitHub/yelp/data")
unzip("rest_revs.csv.zip")
rest_revs <- read_csv("rest_revs.csv")

freq_words <- rest_revs %>% 
  filter(!is.na(text)) %>% 
  mutate(nasty = as.numeric(str_detect(text, "nasty")),
         gross = as.numeric(str_detect(text, "gross")),
         auth = as.numeric(str_detect(text, "authentic")))

freq_sum <- freq_words %>% 
  group_by(cuis_cat) %>% 
  summarize(
    nasty = sum(nasty)*100 / n(),
    gross = sum(gross)*100 / n(),
    auth = sum(auth)*100 / n()
  ) %>% 
  pivot_longer(
    cols = c(2:4), 
    names_to = "word"
  )
    
plot_freq <- freq_sum %>% 
  ggplot(aes(x = cuis_cat)) + 
  geom_bar() + 
  facet_wrap(vars(word))
