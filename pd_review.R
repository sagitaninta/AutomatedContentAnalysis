#-----------------------------
# Automated Content Analysis
#-----------------------------

library(tm)
library(stm)
library(lda)
library(topicmodels)
library(SnowballC)
library(dplyr)
library(pdftools)
install.packages("Rcampdf")

# Make sure your directory to know how you should get the reviewed documents
getwd()

# Save your directory path to save some time typing it when needed
phydiv<-"D:/Master/Thesis/PhylogeneticDiversity"
pd_files<-list.files("D:/Master/Thesis/PhylogeneticDiversity") # this is for list of file names

# Building a corpus of literature that are gonna be reviewed
pd_docs<-Corpus(DirSource(phydiv),readerControl = list(reader=readPDF))

pd_docs[[28]] # checking the individual documents
length(pd_docs) # return the number of documents
summary(pd_docs)
inspect(pd_docs[1])
summary(pd_docs[1])

# Checking metadata inside the corpus
pd_meta<-meta(pd_docs, type="local")

# Checking contents
pd_char<-writeLines(as.character(pd_docs[2])) # displaying the text
# Note that after they were changed into characters, metadata are not readable anymore

# Cleaning the content so we get only analytical text
pd_docs<-tm_map(pd_docs, removeWords, stopwords("SMART")) # removing stop-words with no analytical values
pd_docs<-tm_map(pd_docs, removePunctuation) # removing punctuation inside the documents
pd_docs<-tm_map(pd_docs, removeNumbers) # removing numbers inside the documents
pd_docs<-tm_map(pd_docs, stripWhitespace)

# Stemming document (not really useful in this analysis)
pd_stem<-tm_map(pd_docs, stemDocument)
pd_stem[1]

# Change to document term matrix for easy analysis (should still be in corpus mode)
pd_dtm<-DocumentTermMatrix(pd_docs)
str(pd_dtm)
glimpse(pd_dtm)
pd_dtm_terms<-pd_dtm$dimnames$Terms #get all the words in the document
pd_meta<-meta(pd_docs)

inspect(pd_dtm[, 740:743])
findFreqTerms(pd_dtm,300) # to get terms that occurs at least 300 times
findAssocs(pd_dtm, "phylogenetic", 0.8)

# Preparing data for stm
?stm
# term-document matrices are ingested using readCorpus()
# raw texts? you will want to start with textProcessor().
pd_stm<-readCorpus(pd_dtm,type = "slam")
glimpse(pd_stm)
pd_vocab<-pd_stm$vocab

plotRemoved(pd_stm, lower.thresh = seq(1, 200, by = 100)) # ERROR

# as the data is super big, we may want to remove terms that occurs les than 40% in the corpus
prepDocuments(pd_stm, pd_vocab, pd_meta)
?plotRemoved
?prepDocuments
inspect(removeSparseTerms(pd_dtm, 0.4))

#
inspect(DocumentTermMatrix(pd_dtm,
                             + list(dictionary = c("phylogenetic", "diversity", "community"))))

# using stm (the key innovation of stm compared to tm is the metadata so let's see)
meta(pd_docs) # the corpus itself doesn't have metadata
meta(pd_docs, type="local") # each document has their own metadata but only title, date, and not every documents get its author right

# a topic is defined as a mixture over words where each word has a probability of belonging to a topic.
# a document is a mixture over topics, meaning that a single document can be composed of multiple topics.


str(pd_stm)
glimpse(pd_stm)

head(gadarian)
#Process the data for analysis.
temp<-textProcessor(documents=gadarian$open.ended.response,metadata=gadarian)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta
out$meta
meta
glimpse(vocab)
