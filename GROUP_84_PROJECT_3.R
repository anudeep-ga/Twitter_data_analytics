
###Task 1) The objective of project 3 is to perform keyword network analysis and word frequency analysis.

##Ques 2) Write a code to extract keyword data from the above file and convert it to a weighted adjacency matrix.


#Loading all the necessary libraries
library(dplyr)  
library(magrittr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(stringr)
library(purrr)
library(readr)
library(tidyverse)


#Retrieving the dataset into dataframe
df = read.csv("Keyword_data - Keyword_data.csv", na.strings = c("","-"))
#na.strings indicates which values need to be read as NA in the body of the file

df$Title = toupper(df$Title)      #converting from lowercase to uppercase
df = df %>% drop_na(Title)        #dropping rows contaning missing values
x = str_which(df$Title,"/03$")    #identifying the location of the character strings containing the given pattern
df = df[-c(x),]
rm(x)                             #removing the variable x from our work space


#Removing NA values  
testing <- data.frame(col = c(df$Keyword.1, df$Keyword.2, df$Keyword.3, df$Keyword.4, df$Keyword.5, df$Keyword.6, df$Keyword.7, df$Keyword.8, df$Keyword.9, df$Keyword.10, df$Keyword.11, df$Keyword.12)) %>% drop_na(col)

df[is.na(df)] <- "undefined"                           #this is used to replace all the NA values to "undefined"

testing <- unique(testing$col) %>% as.data.frame()     #eliminating the duplicate values from the dataframe
colnames(testing) = "unique_keywords"                  #renaming the column to "unique_keywords"
testing


#Storing unique values into a vector
v = vector()
for (i in testing){
  v = i
}


m = matrix(0, nrow = 248,ncol = 248)
colnames(m) = v
rownames(m) = v


#Word Mapping
count = 1
list <- c()
i = 1
while (i <= nrow(testing)){
  list[[testing[i,]]] <- count
  count = count + 1
  i <- i + 1
}
list["undefined"] = 0

row.names(df) <- NULL


#Writing while loops for assigning weights
row = 1
while (row < 50) {
  col = 2
  while (col < 13){
    key1 = df[row, col]
    pos1 = list[key1]
    if (pos1!=0){
      col2 = col + 1
      while (col2 < 14 ){
        key2 = df[row, col2]
        pos2 = list[key2]
        if (pos2!=0){
          m[unlist(pos1),unlist(pos2)] = m[unlist(pos1),unlist(pos2)] + 1
          m[unlist(pos2),unlist(pos1)] = m[unlist(pos2),unlist(pos1)] + 1
        }
        col2 <- col2 + 1
      }
    }
    col <- col + 1
  }
  row <- row + 1
}


#The end result is stored in a matrix named "m"
rm(df,list,pos1,pos2,testing,col,col2,count,i,key1,key2,row,v)
m


##Ques 3) Reading the adjacency matrix and converting it into a weighted network
network = graph_from_adjacency_matrix(m,mode="undirected",weighted = TRUE)
network


##Ques 4 & 5) Computing the node degree and node strength; showing the top 10 nodes by degree and top 10 nodes by strength
degrees <- degree(network, mode='all')
Top_Ten_Degrees <- head(sort(degrees, decreasing = T), 10)
Top_Ten_Degrees



strengths <- strength(network, mode='all')
Top_Ten_Strengths <- head(sort(strengths, decreasing = T), 10)
Top_Ten_Strengths


##Ques 6) Showing the top 10 node pairs by weight
Top_Ten_Node = as_long_data_frame(network) %>%
  rename("Node 1" = "ver[el[, 1], ]", "Node 2" = "ver2[el[, 2], ]") %>%
  select("weight":"Node 2") %>%
  arrange(desc(weight)) %>%
  head(10)
Top_Ten_Node


##Ques 7) Plotting the average strength on y-axis and the degree on x-axis
Avg_Weight <- tapply(strengths, degrees, mean)
Avg_Weight <- data.frame(Avg_Weight)
AvgweightVDegree <- cbind(degree=rownames(Avg_Weight), Avg_Weight)
row.names(AvgweightVDegree) <- NULL

plot(AvgweightVDegree, type='p', pch = 1, main='Average Strength vs Degree', xlab='Degree', ylab='Average Strength')




###Task 2) Thelinkprovides the twitter data of Elon Musk from2010-2022. For analysis consider the years 2017-2022.                                 Each year has thousands of tweets. Assume each year to be a document (all the tweets in one year will be considered as a document).

##Ques 1) 1. Compute word frequencies for each year. Exclude the stop words.

#Reading the dataset - This dataset is a csv file compilation of all the tweets of elon musk from 2010 to 2022; 
#uploaded the csv file named "elon_tweets" along with the r files of task 1 and 2 in the portal.

df_elon = read.csv("elon_tweets.csv", na.strings = c("","-"))   #na.strings indicates which values need to be read as NA in the body of the file

#Retrieving data from 2017 to 2022
df_elon = df_elon %>%
  filter(date > "2016")   #using the filter function to get tweets starting from the year 2017

remove_reg <- "&amp;|&lt;|&gt"                                 #removing the special characters from the tweets
df_elon$tweet <- gsub("@\\w+", "", df_elon$tweet)              #removing @ taggings
df_elon$tweet <- gsub("[[:digit:]]", "", df_elon$tweet)        #removing all digits from 0 to 9
df_tweets <- df_elon %>% 
  mutate(tweet = str_remove_all(tweet, remove_reg)) %>%        #mutate is used for adding new variables and preserving existing ones
  unnest_tokens(word, tweet, token = "tweets") %>%             #splitting a column into tokens, flattening the table into one token per row
  filter(!word %in% stop_words$word,                           #removing the stop words
         !word %in% str_remove_all(stop_words$word, "'"),     
         !word %in% c("http", "https", "t.co", "amp"),         #removing urls and links
         str_detect(word, "[a-z]")) %>%
  count(date, word, sort = TRUE)                               #computing word count per year

total_tweet = df_tweets %>%                                    #computing the word frequencies
  group_by(date) %>%
  summarise(total = sum(n))

tweets = left_join(df_tweets,total_tweet)                      #joining based on common key, including all rows from df_tweets
tweets$Freq = tweets$n/tweets$total                            #calculating the word frequencies
tweets



##Ques 2) Show top 10 words (for each year) by the highest value of word frequency.
year_df <- data.frame(year=numeric(0), word=character(0), frequency=numeric(0))   #creating a dataframe with the columns year, word, and frequency
for (i in 2017:2022){
  temp_df <- head(subset(df_tweets, date==i),10)                          #storing the top 10 words with their frequencies in a temporary dataframe
  temp_df <- temp_df[, !names(temp_df) %in% c('n','total_words')]
  year_df <- rbind(year_df, temp_df)                                      #row binding; joining the two dataframes
}
year_df



##Ques 3) Plot histogram of word frequencies for each year.
histogram_data = tweets %>%
  filter(n > 5)

ggplot(histogram_data, aes(n/total, fill = date)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~date, ncol = 2, scales = "free_y")



##Ques 4) Use Zipf’s law and plot log-log plots of word frequencies and rank for each year.
Frequency_by_Rank <- tweets %>% 
  group_by(date) %>% 
  mutate(Rank = row_number()) %>%         #assigning numerical rankings to the data values in ascending order
  ungroup()                               #ungrouping after performing calculations on the grouped data

Frequency_by_Rank %>%                     #code for plotting the histograms for each year from 2017 to 2022
  ggplot(aes(Rank, Freq, color = date)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+
  facet_wrap(~date, ncol = 2, scales = "free_y")



##Ques 5) Create bigram network graphs for each year.
tweet_bigrams = df_elon %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

#separating the words
bigrams_separated = tweet_bigrams %>%
  separate(bigram, c("w1", "w2"), sep = " ")

#Removing all the stop words
bigrams_filtered = bigrams_separated %>%
  filter(!w1 %in% stop_words$word) %>%
  filter(!w2 %in% stop_words$word)

#Removing all the urls and links
bigrams_final = bigrams_filtered %>%
  filter(!w1 %in% c("http", "https", "t.co", "amp", NA))%>%
  filter(!w2 %in% c("http", "https", "t.co", "amp", NA))

#Counting the bigrams
bigram_counts <- bigrams_final %>% 
  count(w1, w2, sort = TRUE)

#Combining the words
bigrams_united = bigrams_final %>%
  unite(bigram, w1, w2, sep = " ")


#Visualization of the biagram for the year 2017
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2017") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkblue", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for Elon Musk's Tweets in the year 2017")


#Visualization of the bigram for the year 2018
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2018") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "red", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  ggtitle("Bigram for Elon Musk's Tweets in the year 2018")


#Visualization of the bigram for the year 2019
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2019") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "purple", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  ggtitle("Bigram for Elon Musk's Tweets in the year 2019")


#Visualization of the bigram for the year 2020
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2020") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "green", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  ggtitle("Bigram for Elon Musk's Tweets in the year 2020")


#Visualization of the bigram for the year 2021
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2021") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "brown", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  ggtitle("Bigram for Elon Musk's Tweets in the year 2021")


#Visualization of the bigram for the year 2022
library(ggraph)
bigram_graph <- bigrams_final %>% 
  filter(date == "2022") %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n > 3) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  ggtitle("Bigram for Elon Musk's Tweets in the year 2022")