#' Title: Call of Duty E-Sport
#' Purpose: Data processing & exploration 
#' Author: Qi Yang
#' Date: Jan 24 2020
#' 

# Set wd 
setwd("~/Downloads/R/hult_NLP_student/cases/Call of Duty E-Sport")

# libs
library(stringr)
library(ggplot2)
library(ggthemes)
library(pbapply)
library(tm)
library(fst)
library(echarts4r)
library(plotrix)
library(ggalt)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(lexicon)
library(tidytext)
library(dplyr)
library(radarchart)


# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'),"lol","amp","good","post","bro","man","lmk","it","...","i","....","game",
           "person",)

#Read Data
emojis <- read.csv('emojis.csv')
playerFTl1 <- read.fst('student_2020-12-30_LAGuerrillas_323099683_playerFollowerTimelines.fst')
playerFTl2 <- read.fst('student_2020-12-30_LAThieves_831981685_playerFollowerTimelines.fst')
playerTl1 <- read.fst('student_2020-12-28_LAGuerrillas_player_timelines.fst')
playerTl2 <- read.fst('student_2020-12-28_LAThieves_player_timelines.fst')
teamFTl1 <- read.fst('student_2020-12-28_LAGuerrillas2_followers_timelines.fst',from = 1, to = 1000)
teamFTl2 <- read.fst('student_2020-12-28_LAThieves2_followers_timelines.fst',from = 1, to = 1000)
teamTl <- read.fst('student_TeamTimelines.fst')

# Bring in our supporting functions
source('~/Downloads/R/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Set Pyramid Plots for playerFTl(LAGuerrillas VS LAThieves).
playerFTl1  <- cleanMatrix(pth       = 'student_2020-12-30_LAGuerrillas_323099683_playerFollowerTimelines.fst',
                     columnName      = 'text',
                     collapse        = T, 
                     customStopwords = stops,
                     type = 'TDM',
                     wgt = 'weightTf') 

playerFTl2 <- cleanMatrix(pth    = 'student_2020-12-30_LAThieves_831981685_playerFollowerTimelines.fst' ,
                     columnName = 'text',
                     collapse   = T,
                     customStopwords = stops,
                     type = 'TDM', 
                     wgt = 'weightTf')

df        <- merge(playerFTl1, playerFTl2, by ='row.names')
names(df) <- c('terms', 'LAGuerrillas', 'LAThieves')

# Examine
df[6:10,]

# Calculate the absolute differences among in common terms
df$diff <- abs(df$LAGuerrillas - df$LAThieves)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top20 <- df[1:20, ]

# Pyarmid Plot
pyramid.plot(lx         = top20$LAGuerrillas, #left
             rx         = top20$LAThieves,    #right
             labels     = top20$terms,  
             top.labels = c('LAGuerrillas', 'Terms', 'LAThieves'), 
             gap        = 150, 
             main       = 'Words in Common(playerFTl)', # title
             unit       = 'wordFreq') 
top20

# Set Pyramid Plots for playerTl(LAGuerrillas VS LAThieves).
playerTl1  <- cleanMatrix(pth       = 'student_2020-12-28_LAGuerrillas_player_timelines.fst',
                           columnName      = 'text',
                           collapse        = T, 
                           customStopwords = stops,
                           type = 'TDM',
                           wgt = 'weightTf') 

playerTl2 <- cleanMatrix(pth    = 'student_2020-12-28_LAThieves_player_timelines.fst' ,
                         columnName = 'text',
                         collapse   = T,
                         customStopwords = stops,
                         type = 'TDM', 
                         wgt = 'weightTf')

df        <- merge(playerTl1, playerTl2, by ='row.names')
names(df) <- c('terms', 'LAGuerrillas', 'LAThieves')

# Examine
df[6:10,]

# Calculate the absolute differences among in common terms
df$diff <- abs(df$LAGuerrillas - df$LAThieves)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top20 <- df[1:20, ]

# Pyarmid Plot
pyramid.plot(lx         = top20$LAGuerrillas, #left
             rx         = top20$LAThieves,    #right
             labels     = top20$terms,  
             top.labels = c('LAGuerrillas', 'Terms', 'LAThieves'), 
             gap        = 100, 
             main       = 'Words in Common(playerTl)', # title
             unit       = 'wordFreq') 

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

teamTl <- read.fst('student_TeamTimelines.fst',from = 1, to = 100)

# As of tm version 0.7-3 tabular was deprecated
names(teamTl)[1] <-'doc_id' 

# Make a volatile corpus
teamTlCorpus <- VCorpus(VectorSource(teamTl))

# Preprocess the corpus
teamTlCorpus <- cleanCorpus(teamTlCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
teamTlTDM  <- TermDocumentMatrix(teamTlCorpus, 
                               control=list(tokenize=bigramTokens))
teamTlTDMm <- as.matrix(teamTlTDM)

# See a bi-gram
exampleTweet <- grep('teamTl', rownames(teamTlTDMm))
teamTlTDMm[(exampleTweet-2):(exampleTweet),870:871]

# Get Row Sums & organize
teamTlTDMv <- sort(rowSums(teamTlTDMm), decreasing = TRUE)
teamTlDF   <- data.frame(word = names(teamTlTDMv), freq = teamTlTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(teamTlDF$word,
          teamTlDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

#Sentiment Analysis for LAGuerrillas teamFTl
# Clean and Organize
teamFTl1DTM <- cleanMatrix(pth = 'student_2020-12-28_LAGuerrillas2_followers_timelines.fst',
                      columnName = 'text',
                      collapse        = T, 
                      customStopwords = stops, 
                      type            = 'DTM', 
                      wgt             = 'weightTf')

# Examine original 
teamFTl1DTM[,1:10]
dim(teamFTl1DTM)

# Examine Tidy 
tmp      <- as.DocumentTermMatrix(teamFTl1DTM, weighting = weightTf ) 
tidyCorp <- tidy(tmp)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis
table(bingSent$sentiment, bingSent$count)
aggregate(count~sentiment,bingSent, sum)

# Compare original with qdap::Polarity
polarity(read.fst('student_2020-12-28_LAGuerrillas2_followers_timelines.fst',from = 1, to = 100)$text)

install.packages('textdata')
library(textdata)
my_lexicon <-lexicon_afinn()

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Quick Analysis
teamFTl1DTM <- read.fst('student_2020-12-28_LAGuerrillas2_followers_timelines.fst',from = 1, to = 100)$text
teamFTl1Words <- data.frame(word = unlist(strsplit(teamFTl1DTM,' ')))
teamFTl1Words$word <- tolower(teamFTl1Words$word )
teamFTl1Words <- left_join(teamFTl1Words,afinn, by=c('word' = 'word'))
teamFTl1Words[is.na(teamFTl1Words$value),2] <- 0
plot(teamFTl1Words$value, type="l", main="Quick Timeline of teamFTl1")

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
nrc <- nrc_emotions
head(nrc)

# Clean this up
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Quick Analysis
table(nrcSent$sentiment)
emos <- data.frame(table(nrcSent$sentiment))

chartJSRadar(scores = emos, labelSize = 10, showLegend = F)

#Sentiment Analysis for LAThieves teamFTl
# Clean and Organize
teamFTl2DTM <- cleanMatrix(pth = 'student_2020-12-28_LAThieves2_followers_timelines.fst',
                           columnName = 'text',
                           collapse        = T, 
                           customStopwords = stops, 
                           type            = 'DTM', 
                           wgt             = 'weightTf')

# Examine original 
teamFTl2DTM[,1:10]
dim(teamFTl2DTM)

# Examine Tidy 
tmp      <- as.DocumentTermMatrix(teamFTl2DTM, weighting = weightTf ) 
tidyCorp <- tidy(tmp)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis
table(bingSent$sentiment, bingSent$count)
aggregate(count~sentiment,bingSent, sum)

# Compare original with qdap::Polarity
polarity(read.fst('student_2020-12-28_LAThieves2_followers_timelines.fst',from = 1, to = 100)$text)

install.packages('textdata')
library(textdata)
my_lexicon <-lexicon_afinn()

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Quick Analysis
teamFTl2DTM <- read.fst('student_2020-12-28_LAThieves2_followers_timelines.fst',from = 1, to = 100)$text
teamFTl2Words <- data.frame(word = unlist(strsplit(teamFTl2DTM,' ')))
teamFTl2Words$word <- tolower(teamFTl2Words$word )
teamFTl2Words <- left_join(teamFTl2Words,afinn, by=c('word' = 'word'))
teamFTl2Words[is.na(teamFTl2Words$value),2] <- 0
plot(teamFTl2Words$value, type="l", main="Quick Timeline of teamFTl2")

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
nrc <- nrc_emotions
head(nrc)

# Clean this up
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Quick Analysis
table(nrcSent$sentiment)
emos <- data.frame(table(nrcSent$sentiment))

chartJSRadar(scores = emos, labelSize = 10, showLegend = F)

# Make comparison cloud for(LAGuerrillas teamFTl & LAThieves2 teamFTl)
# Read in multiple files 
txtFiles <- list.files(pattern = 'teamFTl1|teamFTl2')

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Vector Corpus; omit the meta data
teamFTl1       <- VCorpus(VectorSource(read.fst('student_2020-12-28_LAGuerrillas2_followers_timelines.fst',from = 1, to = 100)$text))
teamFTl2  <- VCorpus(VectorSource(read.fst('student_2020-12-28_LAThieves2_followers_timelines.fst',from = 1, to = 100)$text))

# Clean up the data
teamFTl1      <- cleanCorpus(teamFTl1, stops)
teamFTl2 <- cleanCorpus(teamFTl2, stops)

teamFTl1       <- paste(teamFTl1 , collapse = ' ')
teamFTl2 <- paste(teamFTl2, collapse = ' ')

# Combine the subject documents into a corpus of *2* documents
allTeamFtls <- c(teamFTl1,teamFTl2)
allTeamFtls <- VCorpus((VectorSource(allTeamFtls)))

# Make TDM with a different control parameter
ctrl      <- list(weighting = weightTfIdf)
TeamFtlsTDM  <- TermDocumentMatrix(allTeamFtls, control = ctrl)
TeamFtlsTDMm <- as.matrix(TeamFtlsTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(TeamFtlsTDM) <- c('teamFTl1', 'teamFTl2')

# Examine
head(TeamFtlsTDMm)

# Make comparison cloud for(LAGuerrillas teamFTl & LAThieves2 teamFTl)
comparison.cloud(TeamFtlsTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(TeamFtlsTDMm),"Dark2"),
                 scale=c(3,0.1))

