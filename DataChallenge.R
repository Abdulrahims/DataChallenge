library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

collected_posts <- read.csv("collected_posts--2016-05-09_16-52-15-UTC.csv", sep = ";")
collections <- read.csv("~/Downloads/data/collections--2016-05-09_16-51-58-UTC.csv", sep = ";")
followings <- read.csv("followings--2016-05-09_16-50-41-UTC.csv", sep = ";")
posts <- read.csv("posts--2016-05-09_16-47-31-UTC.csv", sep = ";")
topic_posts <- read.csv("topic_posts_associations--2016-05-09_16-52-16-UTC.csv", sep = ";")
topics <- read.csv("topics--2016-05-09_16-52-16-UTC.csv", sep = ";")
users <- read.csv("users--2016-05-09_16-47-34-UTC.csv", sep = ";")
votes <- read.csv("votes--2016-05-09_16-48-43-UTC.csv", sep = ";")

user_headlines <- data.frame(users$headline, stringsAsFactors = FALSE)
user_headlines <- na.omit(user_headlines)
colnames(user_headlines)[1] <- "headlines"
user_headlines$headlines <- as.character(user_headlines$headlines)
user_headlines <- subset(user_headlines, headlines != "")
user_headlines <- subset(user_headlines, headlines != ".")

user_headlines2 <- user_headlines
counter = 1
#counter = 3142

row.names(user_headlines) <- 1:nrow(user_headlines)

#remove the given title

for(i in 1:nrow(user_headlines)){
  if(grepl("senior", user_headlines$headlines[i], ignore.case = TRUE) == TRUE){
    user_headlines$headlines[i] <- gsub("senior", "", user_headlines$headlines[i], ignore.case = TRUE)
  }
}

#Terms I went out of my way to standardize

#Account exec
#software engineer
#software developer
#web developer
#unemployed
#student
#CEO
#programmer
#android developer
#social media
#marketing
#UX
#sales
#president
#QA
#retired

#if contains the word, make the whole thing that word

for(i in 1:nrow(user_headlines)){
  if(grepl("mobile", user_headlines$headlines[i], ignore.case = TRUE) == TRUE){
    user_headlines$headlines[i] <- "mobile"
  }
}


#wordcloud construction

user_titles <- Corpus(VectorSource(user_headlines))

dtm_user_titles <- TermDocumentMatrix(user_titles)
m_user_titles <- as.matrix(dtm_user_titles)
v_user_titles <- sort(rowSums(m_user_titles),decreasing=TRUE)
d_user_titles <- data.frame(word = names(v_user_titles),freq=v_user_titles)
head(d_user_titles, 10)

wordcloud(words = d_user_titles$word, freq = d_user_titles$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9, "YlOrRd"))

#makex heatmap

explore_topics <- data.frame(topics$name, topics$followers_count, topics$posts_count)

final_df <- as.data.frame(t(explore_topics2), stringsAsFactors = FALSE)
test <- as.data.frame(t(final_df), stringsAsFactors = FALSE)
new_df <- data.frame(test)
new_df$names<-rownames(new_df)

row.names(new_df) <- 1:nrow(new_df)

new_df <- new_df[c(3,1,2)]

for(i in 1:length(final_df)){
  colnames(final_df)[i] <- final_df[1,i]
}

explore_topics <- subset(explore_topics, explore_topics$topics.posts_count > 200)

new_df$topics.followers_count <- as.integer(new_df$topics.followers_count)
new_df$topics.posts_count <- as.integer(new_df$topics.posts_count)

# Ordering
new_df <- new_df[order(new_df$topics.posts_count, decreasing=TRUE),]
new_df  <- new_df[order(new_df$topics.posts_count),]

row.names(new_df) <- new_df$names
topicmatrix <- data.matrix(new_df) #create the data matrix
topicmatrix <- topicmatrix[,-1]  #took off the first default row (terms)

#generate the heatmap
topicmatrixheat <- heatmap(topicmatrix, Rowv=NA, Colv=NA, col = brewer.pal(9, "Blues"), scale="column", margins=c(7,10), main = "Test")