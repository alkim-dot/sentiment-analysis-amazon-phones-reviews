##ADS531 Project
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(readr)
library(scales)
library(lubridate)
library(tidyverse)
library(viridis)
library(data.table)
library(patchwork)
library(fmsb)
library(stopwords)
library(tidytext)
library(stringr)
library(wordcloud)
library(broom)
library(reshape2)
library(wordcloud)
library(tm)
library(wordcloud2)
setwd("C:/Users/alkim/Dropbox/master/ADS531/Project")
items <- read_csv("Data/20191226-items.csv")
reviews <- read_csv("Data/20191226-reviews.csv")
str(items)
summary(items)


filtered_items$brand <- as.factor(filtered_items$brand)

##sentiment analysis 
#preparing data for sentiment analysis 

dataset <- items %>% inner_join(reviews, by = "asin") %>% select(-image, -url, -reviewUrl, -name) %>% na.omit(brand)

dataset <- dataset %>% rename(meanRating = rating.x,
                              commentRating = rating.y,
                              productTitle = title.x,
                              commentTitle = title.y) 

word_general <- dataset %>% unnest_tokens(word, body) %>% anti_join(stop_words)

word_general %>% count(word, sort = TRUE) %>% 
  top_n(15) %>%
  mutate(word = reorder(word, n))  %>% 
  ggplot(aes(word, n )) +
  geom_col() +
  xlab("WORD") +
  coord_flip() 
##obviously phoene is most used for 
custom_stop_words <- bind_rows(data_frame(word = c("phone","note","Samsung","Nokia","Apple","ASUS","OnePlus","Motorola","HUAWEI","Sony","Google","Xiaomi","great","like","good",
                                                   "samsung","nokia","iphone","apple","asus","oneplus","motorola","huawei","sony","google","xiaomi"), lexicon = c("custom")), stop_words)
#frequency calculation 
frequency_word <- word_general %>% anti_join(custom_stop_words) %>% count(word, sort = TRUE) %>%      
  mutate(proportion = n / sum(n)) %>%    
  select(-n)

frequency_word %>% inner_join(get_sentiments("bing")) %>%   
  count(word, sentiment, sort = TRUE) %>%   
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 210) +
  scale_color_continuous()

frequency_word %>% 
  top_n(15) %>% ggplot(aes(x= reorder(word, proportion), y = proportion)) +
  geom_col() +
  coord_flip()

frequency_word %>% 
  top_n(15) %>% ggplot(aes(x= reorder(word, proportion), y = proportion)) +
  geom_col() +
  coord_flip()

word_general %>% anti_join(custom_stop_words) %>% count(brand) %>%
  ggplot(aes(x = reorder(brand, n), y = n)) +
  geom_col()
  
frequency_by_brand <- word_general %>% anti_join(custom_stop_words) %>% 
  count(brand, word, sort =TRUE) %>% filter(nchar(word) > 3) %>% group_by(brand) %>% 
  mutate(proportion = n/sum(n))
  
##dikkat et 
frequency_by_brand %>% top_n(10) %>% select(-n) %>% 
  ggplot(aes(x = reorder(brand, -proportion), y = proportion)) +
  geom_col(aes(fill = brand), show.legend = F) +
  coord_flip() +
  xlab("Proportion of Words") +
  ylab("Brand") +
  ggtitle("Frequency of Total Words by Brand")

frequency_by_brand %>% top_n(n=3,n) %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=brand)) + geom_col(show.legend=F,col="black")+
  coord_flip()+ facet_wrap(~brand,ncol=3,scales="free")+xlab("") + ylab("Count")+ggtitle("Most words in reviews by brand")


frequency_by_rating <- word_general %>% anti_join(custom_stop_words) %>% 
  count(commentRating, word, sort =TRUE) %>% filter(nchar(word) > 3) %>% group_by(commentRating) %>% 
  mutate(proportion = n/sum(n))

frequency_by_rating %>% top_n(10) %>% select(-n) %>% 
  ggplot(aes(x = reorder(commentRating, -proportion), y = proportion)) +
  geom_col(aes(fill = commentRating), show.legend = F) +
  coord_flip() +
  ylab("Proportion of Words") +
  xlab("Rating") +
  ggtitle("Frequency of Total Words by Rating")

most_used_words <- c("screen", "battery", "camera", "time", "apps", "love", "life")

most_frequency_by_rating <- frequency_by_rating %>% filter(word %in% most_used_words)

most_frequency_by_rating %>% ggplot(aes(x = word, y = proportion)) + 
  geom_col(aes(fill = commentRating)) + 
  coord_flip() +
  facet_grid(~commentRating) +
  ggtitle("Frequency of Words by Ratings") 

frequency_by_rating_year <- dataset %>%
  mutate(date=mdy(date)) %>% mutate(year=year(date)) %>% unnest_tokens(word, body) %>%
  anti_join(custom_stop_words) %>% 
  count(commentRating, word, year, sort =TRUE) %>% 
  filter(nchar(word) > 3) %>% 
  group_by(commentRating) %>% 
  mutate(proportion = n/sum(n))

summary(frequency_by_rating_year)

most_frequency_by_rating_year <-frequency_by_rating_year %>% filter(word %in% most_used_words)

most_frequency_by_rating_year %>% group_by(word, year, commentRating) %>%  select(-n) %>% 
  filter(year > 2012) %>%
  ggplot(aes(year, proportion, fill = word)) +
  geom_col() +
  facet_grid(~word) +
  ggtitle("Disturbution of Words in years")

most_frequency_by_brand_year <- dataset %>%
  mutate(date=mdy(date)) %>% mutate(year=year(date)) %>% unnest_tokens(word, body) %>%
  anti_join(custom_stop_words) %>% 
  count(brand, word, year, sort =TRUE) %>% 
  filter(nchar(word) > 3) %>% 
  group_by(brand) %>% 
  mutate(proportion = n/sum(n)) 

bigram_general <- dataset %>% unnest_tokens(body, output = "bigram", token = "ngrams", n = 2)
 
bigram_general_seperated <- bigram_general %>% separate(bigram, c("word1", "word2"), sep = " ")
bigram_general_seperated <- bigram_general_seperated %>% filter(!word1 %in% custom_stop_words$word,
                                                                !word2 %in% custom_stop_words$word)

bigram_general_seperated %>%
  count(word1, word2, sort = TRUE)

bigram_general_seperated %>% filter(word1 == "battery") %>%
  count(word1, word2, sort = TRUE)
##it shows that battery life is the most important thing for customers

bigram_general_seperated %>% filter(word1 == "screen") %>%
  count(word1, word2, sort = TRUE)
##screen protector, size and resolution is very important

bing_Apple_battery <- bigram_general_seperated %>% filter(word1 == "battery", brand == "Apple") %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))

bing_Apple_battery %>% count(sentiment)

bing_Brands_battery <- bigram_general_seperated %>% filter(word1 == "battery") %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))

bing_Brands_battery %>%    
  count(word2, sentiment, sort = TRUE) %>%   
  acast(word2 ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 210) +
  scale_color_continuous()


p2 <- bing_Brands_battery %>% count(brand, sentiment) %>% group_by(brand) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x=brand, y=percent, fill = sentiment)) + geom_col(col="black") +
  scale_color_discrete(c("tomatoes", "red")) +
  theme_minimal()+
  coord_flip()+
  theme(panel.grid.minor = element_line(colour = "blue"))+
  theme(axis.text.x = element_text(face = "bold"))+
  ggtitle("Distrubition of Usage of Battery Words in Reviews")+
  labs(fill="Verified") +
  xlab(" ") +
  ylab(" ")

bing_Brands_screen <- bigram_general_seperated %>% filter(word1 == "screen") %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) 

bing_Brands_screen %>%    
  count(word2, sentiment, sort = TRUE) %>%   
  acast(word2 ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 210) +
  scale_color_continuous()



p <- bing_Brands_screen %>% count(brand, sentiment) %>% group_by(brand) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x=brand, y=percent, fill = sentiment)) + geom_col(col="black")+
  scale_fill_discrete() +
  theme_minimal()+
  coord_flip()+
  theme(panel.grid.minor = element_line(colour = "blue"))+
  theme(axis.text.x = element_text(face = "bold"))+
  ggtitle("Distrubition of Usage of Screen Words in Reviews")+
  labs(fill="Verified") +
  xlab(" ") +
  ylab(" ")

p + p2 

bing_Brands_camera <- bigram_general_seperated %>% filter(word1 == "camera") %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) 

bing_Brands_camera %>%    
  count(word2, sentiment, sort = TRUE) %>%   
  acast(word2 ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 210) +
  scale_color_continuous()

p3 <- bing_Brands_camera %>% count(brand, sentiment) %>% group_by(brand) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x=brand, y=percent, fill = sentiment)) + geom_col(col="black")+
  scale_fill_discrete() +
  theme_minimal()+
  coord_flip()+
  theme(panel.grid.minor = element_line(colour = "blue"))+
  theme(axis.text.x = element_text(face = "bold"))+
  ggtitle("Distrubition of Usage of Camera Words in Reviews")+
  labs(fill="Verified") +
  xlab(" ") +
  ylab(" ")

bing_Brands_price <- bigram_general_seperated %>% filter(word1 == "price") %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) 

p4 <- bing_Brands_price %>% count(brand, sentiment) %>% group_by(brand) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x=brand, y=percent, fill = sentiment)) + geom_col(col="black")+
  scale_fill_discrete() +
  theme_minimal()+
  coord_flip()+
  theme(panel.grid.minor = element_line(colour = "blue"))+
  theme(axis.text.x = element_text(face = "bold"))+
  ggtitle("Distrubition of Usage of Price Word in Reviews")+
  labs(fill="Verified") +
  xlab(" ") +
  ylab(" ")

p3+p4

##general review analysis for different brand, I want to focus on Samsung, Apple and Xiaomi
#because even Samsung and Apple have higher market share, Xiamomi have higher avg_rate and positive sentiment in general
 

year_word_sentiment <- dataset %>% mutate(date=mdy(date)) %>% mutate(year=year(date)) %>% 
  unnest_tokens(word, body) %>% inner_join(get_sentiments("afinn")) 

my_brands <- c("Apple", "Samsung", "Xiaomi", "Sony")

year_word_sentiment <- year_word_sentiment %>% filter(brand %in% my_brands)

year_word_sentiment %>% distinct(brand) #starts with 2009

year_word_sentiment %>% count(brand, year, value) %>% 
  group_by(brand, year) %>% mutate(sentiment = value * n) %>% count(brand, year, sentiment) %>%
  select(-n) %>% summarize(tsentiment = sum(sentiment)) %>% 
  mutate(proportion = tsentiment / sum(tsentiment)) %>%
  ggplot(aes(x = year, y = proportion)) +
  geom_col(aes(fill = brand)) +
  facet_grid(~brand) +
  xlab("Year") +
  ylab("Total Sentiment") +
  scale_x_continuous(breaks = c(2010, 2014, 2017, 2019))

bigram_general_united <- bigram_general_seperated %>% filter(brand %in% my_brands) %>% select(brand, word1, word2) %>%
  unite(biagram, word1, word2, sep = " ")

#highly recommend 

no_highly_recommed <- bigram_general_united %>% filter(biagram == "highly recommend") %>% count(brand)
  
no_brand <- bigram_general_united %>% count(brand)

no_highly_recommed <- no_highly_recommed %>% inner_join(no_brand, by ="brand") %>%
  mutate(percentage = n.x / n.y * 100)

no_highly_recommed %>% ggplot(aes(x = brand, y = percent, fill = brand)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Distrubution of 'highly recommend' word for brands") +
  xlab("BRAND") +
  ylab("Percentage of Word")

##battery life 
word3_data <- dataset %>% unnest_tokens(body, output = "word3", token = "ngrams", n = 3) %>%
  separate(word3, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% custom_stop_words$word,
         !word2 %in% custom_stop_words$word,
         !word3 %in% custom_stop_words$word)

battery_analysis <- word3_data %>% filter(word2 == "battery", word3 == "life") %>% 
  filter(brand %in% my_brands) %>% select(brand, commentRating, word1) %>%
  inner_join(get_sentiments("bing"), by = c(word1 = "word"))


p_battery <- battery_analysis %>% 
  count(word1, sentiment, sort = TRUE) %>% top_n(10) %>%
  ggplot(aes(x = reorder(word1,n), y = n, fill = sentiment)) +
  geom_col()
  

battery_analysis %>% count(commentRating, sentiment) %>% group_by(commentRating) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(commentRating, percent, fill = sentiment)) +
  geom_col(position = "dodge", show.legend = F) + 
  xlab("Ratings of Comments") +
  ylab("Distrubution of Sentiments") + 
  ggtitle("Effect of Batery Life on Ratings") +
  p_battery + coord_flip() + xlab("Word") + ylab("No. of Word")



 





