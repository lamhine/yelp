
# filtering
tidy_american_bad <- subset(restaurants, stars.x <= 1 & cuis_cat == "American")
View(tidy_american_bad)

# text cleaning
# 1. creating a corpus
tidy_american_bad <- tidy_american_bad %>%
  select(date,text) %>%
  unnest_tokens("word", text)
head(tidy_american_bad)

tidy_american_bad %>%
  count(word) %>%
  arrange(desc(n))

# 2. removing stopwords
tidy_american_bad <- tidy_american_bad %>%
  anti_join(stop_words)

tidy_american_bad %>%
  count(word) %>%
  arrange(desc(n))

# 3. removal of other elements, lowercasing, white spaces
tidy_american_bad <- tidy_american_bad[grep("\\b\\d+\\b", tidy_american_bad$word, invert=TRUE),] #remove numbers
tidy_american_bad$word <- gsub("\\s+","",tidy_american_bad$word) #remove white spaces
tidy_american_bad$word <- str_to_lower(tidy_american_bad$word) #lowercase

tidy_american_bad <- tidy_american_bad[-grep(foodstopwords, tidy_american_bad$word),]

tidy_american_bad %>%
  count(word) %>%
  arrange(desc(n))

otherstopwords = "food|mexican|italian|american|chinese|pizza|fries|burger|pasta|bread|meal|restaurant|egg|sushi|soup|meat|noodles|roll|crab|burrito|taco|margarita|chip"

tidy_american_bad <- tidy_american_bad[-grep(otherstopwords, tidy_american_bad$word),]

tidy_american_bad %>%
  count(word) %>%
  arrange(desc(n))

# visualizing top words
top_words<-
  tidy_american_bad %>%
  anti_join(stop_words) %>%
  filter(!(word=="food"|
             word=="restaurant"|
             word=="restaurants"|
             word=="bar"|
             word=="bars"|
             word=="time"|
             word=="service"|
             word=="order")) %>%
  count(word) %>%
  arrange(desc(n))

top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Negative American Restaurant Reviews")+
  guides(fill=FALSE)

# 4. stemming
tidy_american_bad <- tidy_american_bad %>%
  mutate_at("word", funs(wordStem((.), language="en")))

# 5. generating document term matrix
tidy_american_bad_dtm <-
  tidy_american_bad %>%
  count(date, word) %>%
  cast_dtm(date, word, n)

# 6. topic modeling
american_bad_topic_model <- LDA(tidy_american_bad_dtm, k=10, control = list(seed = 321))

american_bad_topics <- tidy(american_bad_topic_model, matrix = "beta")

american_bad_top_terms <- 
  american_bad_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

american_bad_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
