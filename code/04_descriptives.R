# load packages
library(tidyverse)
library(data.table)
library(gtsummary)

# set working directory
setwd("/Users/lamhine/Documents/GitHub/yelp/data")

# load in data
rest_revs <- fread("rest_revs.csv.gz")

# create dummy variables for presence words of interest in each review
freq_words <- rest_revs %>% 
  mutate(nasty = as.numeric(str_detect(text, "nasty")),
         gross = as.numeric(str_detect(text, "gross")),
         dirty = as.numeric(str_detect(text, "dirty")),
         auth = as.numeric(str_detect(text, "authentic")))

# create table 1
tbl1 <- freq_words %>% 
  dplyr::rename(
    `Star rating` = stars.y,
    Price = attributes.RestaurantsPriceRange2,
    Nasty = nasty,
    Gross = gross,
    Dirty = dirty,
    Authentic = auth
         ) %>% 
  mutate(
    `Star rating` = case_when(
      `Star rating` == 1 ~ "*",
      `Star rating` == 2 ~ "**",
      `Star rating` == 3 ~ "***",
      `Star rating` == 4 ~ "****",
      `Star rating` == 5 ~ "*****"),
    Price = case_when(
      Price == 1 ~ "$",
      Price == 2 ~ "$$",
      Price == 3 ~ "$$$",
      Price == 4 ~ "$$$$",
      TRUE ~ "Missing")
    ) %>% 
  select(cuis_cat, `Star rating`, Price, 
         Nasty, Gross, Dirty, Authentic) %>% 
  tbl_summary(by = cuis_cat) %>% 
  add_overall()

tbl1

tbl1 %>% as_gt() %>% gt::gtsave("/Users/lamhine/Documents/GitHub/yelp/plots/tbl1.png")

# plot proportions of reviews by category that mention each word
# first group by and summarize, then ggplot
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

# stratify plots by star rating
freq_stars <- freq_words %>% 
  group_by(cuis_cat, stars.y) %>% 
  summarize(
    nasty = sum(nasty)*100 / n(),
    gross = sum(gross)*100 / n(),
    dirty = sum(dirty)*100 / n(),
    auth = sum(auth)*100 / n()
  ) %>% 
  pivot_longer(
    cols = c(3:ncol(.)), 
    names_to = "word"
  )

plot_stars <- freq_stars %>% 
  ggplot(aes(x = cuis_cat, y = value, fill = factor(stars.y))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(vars(word), scales="free")

plot_stars
ggsave("/Users/lamhine/Documents/GitHub/yelp/plots/plot_stars.png")

# stratify plots by price category, remove NA or "None"
freq_price <- freq_words %>% 
  group_by(cuis_cat, attributes.RestaurantsPriceRange2) %>% 
  summarize(
    nasty = sum(nasty)*100 / n(),
    gross = sum(gross)*100 / n(),
    dirty = sum(dirty)*100 / n(),
    auth = sum(auth)*100 / n()
  ) %>% 
  pivot_longer(
    cols = c(3:ncol(.)), 
    names_to = "word"
  ) %>% 
  filter(attributes.RestaurantsPriceRange2 %in% c(1:4))

plot_price <- freq_price %>% 
  ggplot(aes(x = cuis_cat, y = value, fill = factor(attributes.RestaurantsPriceRange2))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(vars(word), scales="free")

plot_price
ggsave("/Users/lamhine/Documents/GitHub/yelp/plots/plot_stars.png")