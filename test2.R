library('ggplot2')
library('dplyr')
library('shiny')



install.packages('rmarkdown')

getwd()
setwd('~/R')

reviews = read.csv('reviews with diversity.csv')

reviews$words <- NULL
reviews$X <- NULL
reviews$Unnamed..0 <- NULL

ggplot(aes(x = diversity, y = length, color = pub_year), data = subset(reviews, diversity > 0))+
  geom_point(alpha = 1/3)
  
ggplot(aes(x = diversity, y = score, color = pub_year), data = subset(reviews, diversity > 0))+
  geom_point(alpha = 1/3)

cor.test(reviews$diversity, reviews$score)

length(reviews$score)

head(subset(reviews, diversity >.3 & diversity <.4 & length < 400))

head(subset(reviews, diversity > .85))

genre.lm <- lm(score ~ genre, reviews)

ggplot(aes(y = score, x = pub_date), data = reviews) +
  geom_point(alpha = 1/2)+
  facet_wrap(~genre)

genre_group <- group_by(reviews, genre)

genre_group <- summarise(genre_group,
                         median.score = median(score),
                         mean.score = mean(score))


model <- lm(score ~ genre + length + author, reviews)

predict(model)

lm(score ~pub_month, reviews)

genre_group

sd(reviews$score)

install.packages('plyr')

count(subset(reviews, genre == 'jazz' & score > 8.5)) / count(subset(reviews, genre == 'jazz'))

count(subset(reviews, genre == 'rock' & score > 8.5)) / count(subset(reviews, genre == 'rock'))

count(subset(reviews, genre == 'electronic' & score > 9)) / count(subset(reviews, genre == 'rap'))


?count
