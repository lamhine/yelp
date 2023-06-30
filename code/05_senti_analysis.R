rm(list = ls())
library(jsonlite)
library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)
library(syuzhet)
library(parallel)
library(textnets)
library(network)
library(ggplot2)
library(gtsummary)
library(patchwork)
library(cowplot)
library(igraph)
library(jsonlite)
# stream in 1 million randomly sampled reviews and all business data
review <- read.csv("/Users/xiaoqianwan/Desktop/SICSS_UCLA/project/LIWC Analysis.csv")

review <- review %>%
  mutate(genre = if_else(str_detect(categories, "Chinese"), "Chinese",
                         if_else(str_detect(categories, "Mexican"), "Mexican",
                                 if_else(str_detect(categories, "American"), "American",
                                         "Italian"))))
businesses <- stream_in(file("/Users/xiaoqianwan/Desktop/SICSS_UCLA/project/yelp_academic_dataset_business.json"))


#############sentiment analysis 
senti_data <- review %>%
  select(genre,text,business_id,review_id,date)


# unnest tokens and remove stop words
data("stop_words")

tidy_words <- senti_data %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# get table of most commonly used words
top_words <- tidy_words %>%
  count(word, sort = TRUE) 


# Set the number of cores to be used
num_cores <- 4

cl <- makeCluster(num_cores)
# Perform sentiment analysis using parallel processing
sentiment_scores <- mclapply(tidy_words$word, get_sentiment)
stopCluster(cl)

# Add the sentiment scores to the 'score' column in 'tidy_words'
tidy_words$score <- sentiment_scores

#convert date to date format 
tidy_words$date <- as.Date(tidy_words$date)
tidy_words$score <-as.numeric(tidy_words$score)
#calculat the sentimen score 
tidy_words_totalscore <- tidy_words %>%
  group_by(date,business_id,review_id)%>%
  mutate(review_score=sum(score)) %>%
  select(business_id,date,review_score,genre)%>%
  unique()%>%
  mutate(year=as.numeric(substr(date, 1, 4)))

#mean review score per restaurant per year 
tidy_words_totalscore <- tidy_words_totalscore%>%
  group_by(business_id,year,genre)%>%
  mutate(mean_score=mean(review_score))%>%
  mutate(median_score=median(review_score))

tidy_words_totalscore<-tidy_words_totalscore%>%
  group_by(year)%>%
  mutate(mean_year=mean(review_score))

tidy_words_select <- tidy_words_totalscore%>%
  select(business_id,review_id,review_score,date,year,text)
review_text <-review %>%
  select(business_id,review_id,text)

tidy_words_text <- tidy_words_select %>%
  inner_join(review_text,by=c("business_id","review_id"))

tidy_words_text <- tidy_words_text%>% 
  arrange(review_score)

write_csv(tidy_words_select,"/Users/xiaoqianwan/Desktop/SICSS_UCLA/project/sentiment_score.csv")

#plot score and year
plot1<-ggplot(tidy_words_totalscore, aes(x = year, y = mean_score,color=genre)) +
  geom_smooth() +
  labs(x = "Year", y = "Score") +
  ggtitle("Sentiment score trend by restuarant genre")


plot2<-ggplot(tidy_words_totalscore, aes(x = year, y = mean_year)) +
  geom_smooth() +
  labs(x = "Year", y = "Score by year") +
  ggtitle("Overall sentiment score trend")

combined_plot <- plot_grid(plot1, plot2, align = "hv", nrow = 1)
print(combined_plot)


#put the review text back 
review_text <- review %>%
  select(business_id,review_id,text)

tidy_words_totalscore <- tidy_words_totalscore %>%
  inner_join(review_text,by=c("business_id","review_id"))

#mention/actual categories 
review_Chinese <- review%>%
  filter(str_detect(text,"Chinese")|str_detect(text,"chinese"))%>%
  select(business_id,genre,text)


chinese_bigrams <-review_Chinese %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
chinese_bigrams %>%
  count(bigram,sort=TRUE)


bigrams_separated <- chinese_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered <- bigrams_filtered %>%
  select(-business_id,-genre)


#bigram to network
network1 <- graph_from_data_frame(bigrams_filtered, directed = TRUE)
network.simple <- delete_vertices(simplify(network1), degree(network1) <= 500) #remove words with connection less than 100. adjust as deemed necessary
degree_centrality <- degree(network.simple)


plot.igraph(network.simple,
            vertex.label.cex = ifelse(grepl("Chinese",vertex_attr(network.simple,"name")),1,degree_centrality/100),
            layout = layout_with_fr,
            edge.arrow.size = .02,
            vertex.size = 0.2)


cluster_l<-cluster_walktrap(network.simple)
V(network.simple)$cluster_l <- cluster_l$membership
plot.igraph(network.simple,
            vertex.size = 0.2,
            layout = layout_with_fr,
            edge.arrow.size = .02,
            vertex.color=V(network.simple)$cluster_l,
            vertex.label.cex=degree_centrality/100)




