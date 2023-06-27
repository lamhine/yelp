library(tidyverse)
setwd("/Users/lamhine/Documents/GitHub/yelp/data")
unzip("rest_revs.csv.zip")
rest_revs <- read_csv("rest_revs.csv")

freq_words <- rest_revs %>% 
  filter(!is.na(text)) %>% 
  mutate(nasty = as.numeric(str_detect(text, "nasty")),
         gross = as.numeric(str_detect(text, "gross")),
         dirty = as.numeric(str_detect(text, "dirty")),
         auth = as.numeric(str_detect(text, "authentic")))

freq_sum <- freq_words %>% 
  group_by(cuis_cat) %>% 
  summarize(
    nasty = sum(nasty)*100 / n(),
    gross = sum(gross)*100 / n(),
    dirty = sum(dirty)*100 / n(),
    auth = sum(auth)*100 / n()
  ) %>% 
  pivot_longer(
    cols = c(2:ncol(.)), 
    names_to = "word"
  )
    
plot_freq <- freq_sum %>% 
  ggplot(aes(x = cuis_cat, y = value, fill = cuis_cat)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(vars(word), scales="free")

plot_freq

ggsave("/Users/lamhine/Documents/GitHub/yelp/plots/plot_freq.png")
