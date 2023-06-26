library(jsonlite)
library(dplyr)
library(stringr)

#read the random review sample and all business data
setwd("D:/Shiun/UCI/0Research Projects/Research Papers/SICSS/data/archive (1)")
review_data_1M <- stream_in(file("yelpdata2.json"))
head(review_data_1M)
colnames(review_data_1M)
businesses <- stream_in(file("yelp_academic_dataset_business.json"))
colnames(businesses)

#merge it with business id for more info
rest_s <- inner_join(businesses, review_data_1M, by = "business_id")
chinese <- rest_s %>% filter(str_detect(categories, "Chinese"))
#check frequency distribution of values containing "Chinese"
table_c <- table(chinese$categories)
head(sort(table_c, decreasing = T))
print(sort(table_c, decreasing = T)) #might contain other businesses such as massage 
chinese_r <- rest_s %>% filter(str_detect(categories, "Chinese") 
                               & str_detect(categories, "Restaurants"))
restaurants <- rest_s %>% filter(str_detect(categories, "Restaurants"))
table_all <- table(restaurants$categories)
head(sort(table_all, decreasing = T)) #other high frequency category includes: Mexican, Italian, Pizza
rm(chinese)
chinese <- chinese_r
rm(chinese_r)

#create Mexican, Italian restaurants for comparison
mexican <- restaurants %>% filter(str_detect(categories, "Mexican"))
italian <- restaurants %>% filter(str_detect(categories, "Italian"))

#output data as csv file
names(chinese)
head(chinese)
table(chinese$stars.y) #not so sure what is stars.x
chinese$catg <- "c" #create category indicator
mexican$catg <- "m"
italian$catg <- "i"
select_columns <- function(df) {
  df %>% select(business_id, stars.y, text,catg) 
}
chinese_text <- select_columns(chinese)
mexican_text <- select_columns(mexican)
italian_text <- select_columns(italian)
cmi_text <- bind_rows(chinese_text, mexican_text, italian_text)
write.csv(cmi_text, "cmi_text.csv", row.names = T)
rm(list = ls())

#read the LIWC results 
liwc <- read.csv("LIWC-22 Results - cmi_text - LIWC Analysis.csv")
names(liwc)
# Convert to factor
liwc$catg <- as.factor(liwc$catg)
# Use relevel
liwc$catg <- relevel(liwc$catg, ref = "m")

m1 <- lm(WC ~ stars.y, data = liwc)
summary(m1)
m2 <- lm(WC ~ catg, data = liwc)
summary(m2)
m3 <- lm(WC ~ catg + stars.y, data = liwc)
summary(m3)
m4 <- lm(WC ~ catg*stars.y, data = liwc)
summary(m4)

#run models for all the LIWC outcomes to see if there is any pattern
# Install the broom package if you haven't already
if (!require(broom)) {
  install.packages("broom")
}

# List of outcomes
outcome_vars <- names(liwc)[which(names(liwc) == "WC"):which(names(liwc) == "Emoji")]

# Apply the function to each outcome
results <- lapply(outcome_vars, function(var) {
  model <- lm(as.formula(paste(var, "~ catg")), data = liwc)
  res <- tidy(summary(model))
  
  # Round p-values to 4 decimal places
  res$p.value <- round(res$p.value, 4)
  
  # Add the outcome variable name to the result
  res$outcome_variable <- var
  res
})




results <- lapply(outcome_vars, function(var) {
  model <- lm(as.formula(paste(var, "~ catg")), data = liwc)
  tidy(summary(model))
})
# Round p-values to 4 decimal places
results$p.value <- round(results$p.value, 4)

# Combine the results into a single data frame
results_df <- do.call(rbind, results)
write.csv(results_df, "regression_results.csv", row.names = FALSE)