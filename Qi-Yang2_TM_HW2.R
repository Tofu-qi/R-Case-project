#' Title: HW2
#' Purpose: modeling for classification
#' Author: Qi Yang
#' Date: Jan 31 2020
#'

# libs
library(text2vec)
library(caret)
library(glmnet)
library(plyr)
library(class)
library(tm)
library(yardstick)
library(ggplot2)
library(qdap)
library(plotrix)
library(ggthemes)
library(ggalt)
library(e1071)

# Bring in our supporting functions
source('~/Downloads/R/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Options & Functions
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale('LC_ALL','C')

# Set wd 
setwd("~/Downloads/R/hult_NLP_student/HW/HW2")

#Read Data
cs <- read.csv('student_tm_case_score_data.csv')
ct <- read.csv('student_tm_case_training_data.csv')

# Create custom stop words
stops <- c(stopwords('SMART'), 'movie', 'vampire','zombie','unicorn','dont','back','day','today','amp','cnn','shit')

#define the type of source
cs1 <- VectorSource(cs)
ct1 <- VectorSource(ct)

#create a corpus object
csCorp <- VCorpus(cs1)
ctCorp <- VCorpus(ct1)

# Clean each one
csCorp <- cleanCorpus(csCorp, stops)
ctCorp <- cleanCorpus(ctCorp, stops)

# Combine
allPosts <-  c(csCorp,ctCorp)
rm(csCorp)
rm(ctCorp)
gc()

# Construct the Target
yTarget <- c(rep(1,1000), rep(0,1000)) #1= about cs, 0 = ct

# Make TDMs 
allTDM <- TermDocumentMatrix(allPosts, 
                             control = list(weighting = weightTfIdf))
allTDM

length(allTDM$dimnames$Terms)
tdm_removed=removeSparseTerms(allTDM, 0.99)
length(tdm_removed$dimnames$Terms)
mat = as.matrix(tdm_removed)

# Extract the document LSA values
docVectors <- as.data.frame(mat)
head(docVectors)

# Sample (avoid overfitting)
set.seed(1234)
idx <- sample(0:nrow(docVectors),.6*nrow(docVectors))
csCorp   <- docVectors[idx,]
ctCorp <- docVectors[-idx,]

# Fit the Model
fit=naiveBayes(yTarget ~ .,  data = ctCorp)

# Training Preds
pred <- predict(fit, csCorp)
table(pred, csCorp$yTarget)

# Validation Preds
pred <- predict(fit, ctCorp)
(confMat <-table(pred, ctCorp$yTarget))

# Simple model evals
summary(conf_mat(confMat))
autoplot(conf_mat(confMat))

# End

##Substitute the test set into the model

#Read Data
cs=read.csv('student_tm_case_score_data.csv',header = T)
head(cs)
dim(cs)

# Remove emoji
clean_cs = as.character(cs$rawText)
clean_cs = gsub("[^\x01-\x7F]", "", clean_cs)

# Convert the text to lower case
clean_cs = unlist(lapply(clean_cs,tolower))


#New Data
csterms = tdm_removed$dimnames$Terms
n= nrow(cs)
mat1= matrix(NA,n,length(terms))
tcs = data.frame(mat1)
pred = predict(fit,newdata=tcs)
table(pred)
cs$pre_label=pred
head(cs)

write.csv(cs,'Qi-Yang_TM_scores.csv')

#End



