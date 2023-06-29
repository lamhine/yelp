# load packages
library(tidyverse)
library(data.table)
library(sandwich)
library(lmtest)

# set working directory
setwd("/Users/lamhine/Documents/GitHub/yelp/data")

# load in data
rest_revs <- fread("rest_revs.csv.gz")

# create dummy variables for presence words of interest in each review
freq_words <- rest_revs %>% 
  mutate(text = tolower(text)) %>% 
  mutate(ngdd = as.numeric(str_detect(text, "nasty|gross|disgust|disgusting")),
         dirty = as.numeric(str_detect(text, "dirty")),
         badhor = as.numeric(str_detect(text, "bad|horrible")),
         auth = as.numeric(str_detect(text, "authentic")), 
         sick = as.numeric(str_detect(text, "sick")),
         rude = as.numeric(str_detect(text, "rude")))

# collapse the $$$ and $$$$ categories because too few in the $$$$ group
freq_words <- freq_words %>% 
  mutate(
    price = case_when(
      attributes.RestaurantsPriceRange2 == "4" ~ "3",
      attributes.RestaurantsPriceRange2 == "None" ~ NA_character_,
      TRUE ~ attributes.RestaurantsPriceRange2),
    price = as.factor(price)
  ) 

# complete cases
freq_words_complete <- freq_words %>% filter(!is.na(price))

# Fit a regression model
model <- glm(rude ~ factor(cuis_cat) + factor(stars.y) + factor(price),
                  family = poisson("log"),
                  data = freq_words_complete)

# Estimate robust standard errors for clustered data
robust_se <- vcovHC(model, type = "HC1", cluster = "business_id")

# Extract robust standard errors
robust_se_values <- sqrt(diag(robust_se))

# Conduct hypothesis tests using robust standard errors
exp(coeftest(model, vcov = robust_se))

# Exponentiate the coefficient estimates and confidence intervals
exp_coefficients <- exp(coef(model))
exp_ci <- exp(confint(model))

# Print the exponentiated results
print(exp_coefficients)
print(exp_ci)



