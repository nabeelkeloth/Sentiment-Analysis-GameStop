#' Title:WallStreetBets Gamestop Analysis
#' Purpose: To evaluate the text and to identify any early indication in gamestop stock price.  
#' Author:Nabeel keloth
#' Date: March 10 2021


# Setting working directory
setwd("~/Desktop/NLP/hult_NLP_student/cases/session II/WallStreetBets")

# Loading required libraries 
library(tm) 
library(rtweet) 
library(mgsub) 
library(qdap) 
library(textclean) 
library(lubridate)
library(lexicon)
library(tidytext)
library(dplyr)
library(echarts4r)
library(tidyr)
library(corpus)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plotrix)
library(ggthemes)
library(ggalt)
library(readr)
library(pbapply)

# Custom Functions
source('~/Desktop/NLP/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Reading file 
case_df <- read.csv("CASE_gme.csv")
gme_df  <- read.csv("gme_HLOC.csv")

# Applying Options & Functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern="\u00A0", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern="\031", replacement="'")
  return(corpus)
}


# Remove: GSUB all non-ascii character; this removes all non-English too
gsub("[^\x01-\x7F]", " ", case_df$comment)

# Substituting emojis with the lexicon of all comments
mgsub(case_df$comment, emojis$code, emojis$description)

# Removing spaces between emojis
mgsub(case_df$comment, emojis$code, paste0(' ', emojis$description,' '))

# Stopwords
stops <- c(stopwords('smart'), 'price', 'gme', 'shares', 'share', 'gamestop', 
           'buy', 'money', 'stock')

# Cleaning text
gme_text <- VCorpus(VectorSource(case_df$comment))
gme_text <- cleanCorpus(gme_text, stops)


# DocumentTermMatrix
txtDTM <- DocumentTermMatrix(gme_text)
txtDTM <- as.matrix(txtDTM)

# WFM
txtWFM <- colSums(txtDTM)

# Examine & Organize
txtWFM <- data.frame(word = names(txtWFM), freq = txtWFM)
rownames(txtWFM) <- NULL
txtWFM <- txtWFM[order(txtWFM$freq, decreasing = T),]

# Plot WFM
barplot(txtWFM$freq[1:15], names.arg = txtWFM$word[1:15], las = 2)
dev.off()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Dark2")

# Word cloud
set.seed(1234)
wordcloud(txtWFM$word, 
          txtWFM$freq, 
          max.words    = 80,
          random.order = FALSE,
          colors       = pal,
          scale        = c(1.5,0.5))
dev.off()



# get polarity of comments as character 
pol <- polarity(as.character(case_df$comment))

# Subsetting to keep the columns needed
poltext <- subset(case_df, select = c(id, comment, comm_date_day, comm_date_weekday, comm_date_month, comm_date_yr))


# Now Organize the temporal and polarity info
timepol <- data.frame(gmepol   = pol$all$polarity, 
                      day      = poltext$comm_date_day,
                      week  = poltext$comm_date_weekday,
                      month    = poltext$comm_date_month,
                      year     = poltext$comm_date_yr)

# Examine
head(timepol)
mean(timepol$gmepol, na.rm = T) # to be compared with previous ones 

# NA to 0
timepol[is.na(timepol)] <- 0
dim(timepol)


# Daily average
dailypol <- aggregate(gmepol~day, timepol, mean)
dailypol

#plot
plot(dailypol$gmepol, type = 'l', col='red', 
     main= "Daily Polarity", ylab = "Polarity", 
     xlab = "Day")
dev.off()


# Weekday average
weeklypol <- aggregate(gmepol~week, timepol, mean)

# Tell R the ordinal nature of the factor levels
weeklypol$week <- c("First_week", "Second_week", "Third_week", "Forth_week")
weeklypol

#Plot
plot(weeklypol$gmepol, type = 'l', col='orange', 
     main= "Weekly Polarity", ylab = "Polarity", 
     xlab = "Week")
dev.off()


# Monthly average
monthlypol <- aggregate(gmepol~month, timepol, mean)

# Tell R the ordinal nature of the factor levels
monthlypol$month <- c("January_2021", "February_2021", "March_2020",
                    "April_2020", "June_2019", "July_2020",
                    "August_2020", "October_2020", "November_2020", "December_2020")
monthlypol
#Plot
plot(monthlypol$gmepol, type = 'l', col='navy', 
     main= "Monthly Polarity", ylab = "Polarity", 
     xlab = "Month")
dev.off()

#Yearly average 
yearlypol <- aggregate(gmepol~year, timepol, mean)
yearlypol

#Plot
plot(yearlypol$gmepol, type = 'l', col='blue', 
     main= "Yearly Polarity", ylab = "Polarity", 
     xlab = "Year")
dev.off()

#subsetting the data on years

first_year  <- subset(timepol, timepol$year ==2019)
second_year <- subset(timepol, timepol$year ==2020)
third_year  <- subset(timepol, timepol$year ==2021)


# Daily average first year
dailypol_fy <- aggregate(gmepol~day, first_year, mean)
dailypol_fy

#Plot
plot(dailypol_fy$gmepol, type = 'l', col='purple', 
     main= "Daily Polarity 2019", ylab = "Polarity", 
     xlab = "Days")
dev.off()


# Daily average second year
dailypol_sy <- aggregate(gmepol~day, second_year, mean)
dailypol_sy

#Plot
plot(dailypol_sy$gmepol, type = 'l', col='green', 
     main= "Daily Polarity 2020", ylab = "Polarity", 
     xlab = "Days")
dev.off()


# Daily average third year
dailypol_ty <- aggregate(gmepol~day, third_year, mean)
dailypol_ty

#Plot
plot(dailypol_ty$gmepol, type = 'l', col='brown', 
     main= "Daily Polarity 2021", ylab = "Polarity", 
     xlab = "Days")
dev.off()

#submetting GME.Adjusted on years
gme_fy <- subset(gme_df, gme_df$date > '2019-01-01' & gme_df$date < '2019-12-31')
gme_sy <- subset(gme_df, gme_df$date > '2020-01-01' & gme_df$date < '2020-12-31' )
gme_ty <- subset(gme_df, gme_df$date > '2021-01-01' & gme_df$date < '2021-12-31' )


#Plot Adjusted GME 2019
plot(gme_fy$GME.Adjusted, type = 'l', col='red', 
     main= "Adjusted GME 2019", ylab = "Adjusted GME", 
     xlab = "Text")
dev.off()

#Plot Adjusted GME 2020
plot(gme_sy$GME.Adjusted, type = 'l', col='green', 
     main= "Adjusted GME 2020", ylab = "Adjusted GME", 
     xlab = "Days")
dev.off()

#Plot Adjusted GME 2021
plot(gme_ty$GME.Adjusted, type = 'l', col='blue', 
     main= "Adjusted GME 2021", ylab = "Adjusted GME", 
     xlab = "Days")
dev.off()


#Sentiment Analysis


# Examine the emotional words used in the comment
txtDTM <- VCorpus(VectorSource(case_df$comment))
txtDTM <- cleanCorpus(txtDTM, stops)
txtDTM <- DocumentTermMatrix(txtDTM)

# Examine Tidy & Compare
tidyCorp <- tidy(txtDTM)
tidyCorp[100:110,]
dim(tidyCorp)


##bing lexicon

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)


# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent


# Quick Analysis
#tally ignoring count
table(bingSent$sentiment)

#counting the variables 
table(bingSent$sentiment, bingSent$count) 

#sum the varible
aggregate(count~sentiment,bingSent, sum) 


##Afinn Lexicon

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)


# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Examine the quantity
afinnSent$afinnAmt     <- afinnSent$count * afinnSent$value


# Compare w/polarity and bing
mean(afinnSent$afinnAmt)


# Example use case : i.e
afinnTemporal          <- aggregate(afinnAmt~document, afinnSent, sum)
afinnTemporal$document <- as.numeric(afinnTemporal$document)
afinnTemporal          <- afinnTemporal[order(afinnTemporal$document),]


# Quick plot
plot(afinnTemporal$afinnAmt, type="l", main="Quick Timeline of Identified Words",
     ylab = "Afin Amount", xlab = "Text") 
dev.off()

## NRC lexicon

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
nrc <- nrc_emotions
head(nrc)

# Tidy this up
nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
nrc <-subset(nrc, nrc$freq>0 )
head(nrc)

#no longer needed
nrc$freq <- NULL 

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'term'))
nrcSent

# Radar chart
table(nrcSent$emotion)
emos <- data.frame(table(nrcSent$emotion))
names(emos) <- c('emotion', 'termsCt')
emos %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos$termsCt), name = "Wall Street Bets Emotional Categories") %>%
  e_tooltip(trigger = "item") %>% e_theme("dark-mushroom")


#loughran Lexicons

# Other Emotion Lexicons Exist
emotionLex <- affect_wordnet
emotionLex
table(emotionLex$emotion)
table(emotionLex$category)

emotionLex <- subset(emotionLex, 
                     emotionLex$emotion=='Positive'|emotionLex$emotion=='Negative')


# More emotional categories, fewer terms
lexSent <- inner_join(tidyCorp,emotionLex, by=c('term' = 'term'))
lexSent
emotionID <- aggregate(count ~ category, lexSent, sum)
emotionID %>% 
  e_charts(category) %>% e_theme("dark-mushroom") %>%
  e_radar(count, max =max(emotionID$count), name = "Wall Street Bets Comments Emotional Categories") %>%
  e_tooltip() %>%
  e_theme("dark-mushroom") 


#End


