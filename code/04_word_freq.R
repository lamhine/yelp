library(tidyverse)
setwd("/Users/lamhine/Documents/GitHub/yelp/data")
rest_revs <- read_csv("rest_revs.csv")

freq_words <- rest_revs %>% 
  filter(!is.na(text)) %>% 
  mutate(nasty = as.numeric(str_detect(text, "nasty")),
         gross = as.numeric(str_detect(text, "gross")),
         auth = as.numeric(str_detect(text, "authentic")))

freq_sum <- freq_words %>% 
  group_by(cuis_cat) %>% 
  summarize(
    nasty_freq = sum(nasty)*100 / n(),
    gross_freq = sum(gross)*100 / n(),
    auth_freq = sum(auth)*100 / n()
  ) %>% 
  
    
plot_freq <- freq_sum %>% 
  ggplot(aes(x = cuis_cat)) + 
  geom_bar(stat = "bin") + 
  facet_wrap(vars = )