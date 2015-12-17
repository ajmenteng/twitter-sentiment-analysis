#TWITTER SENTIMENT ANALYSIS IN RELATION TO STOCK MARKET FLUCTUATION
#AUTHOR: JOHN CHANDRA (john@johnwesly.net)

#LIBRARIES
library(tm)
library(twitteR)
library(wordcloud)
library(dplyr)
library(stringr)

#TWITTER CREDENTIALS
t.api.key<-"[API Key]"
t.api.secret<-"[API Secret]"
t.acc.token<-"[Acc. Token]"
t.acc.secret<-"[Acc. Secret]"
setup_twitter_oauth(t.api.key, t.api.secret, t.acc.token, t.acc.secret)


#PREPARE THE MAIN DIRECTORY
mainDir<-"[Type you main directory here]"

#USE CURRENT HOUR AS THE NAME OF THE SUB DIRECTORY
subDir<-as.numeric(as.character.POSIXt(Sys.time(),format="%H"))

#CREATE A NEW FOLDER EVERY TIME THIS SCRIPT RUNS
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)

#SET THE WORKING DIRECTORY
setwd(file.path(mainDir, subDir))

#FUNCTION TO BREAK TWEETS INTO 5-MINUTE BUCKETS AND SAVE TO FILE
groupSave = function(sid,tweets){
  for (i in 1:length(tweets)){
    
    #H:HOUR; M:MINUTE
    h<-as.numeric(as.character.POSIXt(tweets[[i]]$created,format="%H"))
    m<-as.numeric(as.character.POSIXt(tweets[[i]]$created,format="%M"))
    
    #GM: GROUP-MINUTE
    gm<-floor(m/5)*5
    
    #PREPARE A FILENAME
    fname<-paste(sid,h,gm, sep="-")
    
    #WRITE TO FILE
    write(tweets[[i]]$text,fname, append = TRUE)
  }
}

#CREATE A FUNCTION FOR SEARCH TWEETS
#KEYWORDS: USE 'OR' FOR COMBINED SEARCH
getTweets = function(sid, keywords, n, start, end){
  tw<-searchTwitter(keywords, lang = "en", n = n, since = start, until = end)
  groupSave(sid, tw)
  print(length(tw))
}

#INITIALIZE THE PARAMETERS
sdate<-"2015-12-15"
udate<-"2015-12-16"
n<-1000

#GAINERS
getTweets("THST","Wineries OR Distillers", n, sdate, udate)
getTweets("MCUR","Biotechnology OR Biotech OR \"cell therapy\"", n, sdate, udate)
getTweets("MBLS","Polymer", n, sdate, udate)

#LOSERS
getTweets("AEY","Electronics", n, sdate, udate)
getTweets("NRX","drugs", n, sdate, udate)
getTweets("KMT","machine OR tools", n, sdate, udate)

#FUNCTION FOR REMOVING URLS FROM TWEETS
removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x)
  gsub("(https[^ ]*)", "", x)
}

#FUNCTION FOR REMOVING NUMBER FROM TWEETS
removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

#FUNCTION FOR TRANSFORMATION
transformCorpus = function(x, x.source) {
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, content_transformer(removeURL))
  x <- tm_map(x, content_transformer(removePunctuation))
  x <- tm_map(x, content_transformer(stemDocument))
  x <- tm_map(x, content_transformer(stripWhitespace))
  x <- tm_map(x, content_transformer(removeNumberWords))
  x <- tm_map(x, content_transformer(removeWords), c(stopwords("english"),"the", "from", "are", "and", "&amp;", "has", "will"))
  x <- tm_map(x, content_transformer(stripWhitespace))
}


#CORPUS TRANSFORMATION (GAINERS)
THST.source <- DirSource(paste(mainDir,"/Gainers/THST", sep=""))
THST.corpus <- Corpus(URISource(THST.source$filelist[1:length(THST.source)]),readerControl=list(reader=readPlain))
transformCorpus(THST.corpus, THST.source)

MCUR.source <- DirSource(paste(mainDir,"/Gainers/MCUR", sep=""))
MCUR.corpus <- Corpus(URISource(MCUR.source$filelist[1:length(MCUR.source)]),readerControl=list(reader=readPlain))
transformCorpus(MCUR.corpus, MCUR.source)

MBLX.source <- DirSource(paste(mainDir,"/Gainers/MBLX", sep=""))
MBLX.corpus <- Corpus(URISource(MBLX.source$filelist[1:length(MBLX.source)]),readerControl=list(reader=readPlain))
transformCorpus(MBLX.corpus, MBLX.source)


#CORPUS TRANSFORMATION (LOSERS)
AEY.source <- DirSource(paste(mainDir,"/Losers/AEY", sep=""))
AEY.corpus <- Corpus(URISource(AEY.source$filelist[1:length(AEY.source)]),readerControl=list(reader=readPlain))
transformCorpus(AEY.corpus, AEY.source)

NRX.source <- DirSource(paste(mainDir,"/Losers/NRX", sep=""))
NRX.corpus <- Corpus(URISource(NRX.source$filelist[1:length(NRX.source)]),readerControl=list(reader=readPlain))
transformCorpus(NRX.corpus, NRX.source)

KMT.source <- DirSource(paste(mainDir,"/Losers/KMT", sep=""))
KMT.corpus <- Corpus(URISource(KMT.source$filelist[1:length(KMT.source)]),readerControl=list(reader=readPlain))
transformCorpus(KMT.corpus, KMT.source)


#BUILD THE TERM DOCUMENT MATRIX
THST.tdm <- TermDocumentMatrix(THST.corpus)
MCUR.tdm <- TermDocumentMatrix(MCUR.corpus)
MBLX.tdm <- TermDocumentMatrix(MBLX.corpus)

AEY.tdm <- TermDocumentMatrix(AEY.corpus)
NRX.tdm <- TermDocumentMatrix(NRX.corpus)
KMT.tdm <- TermDocumentMatrix(KMT.corpus)

#KAGGLE
save(list = ("THST.tdm"), file = "THSTkaggleTDM.RData")
load(file = "THSTkaggleTDM.RData")

save(list = ("MCUR.tdm"), file = "MCURkaggleTDM.RData")
load(file = "MCURkaggleTDM.RData")

save(list = ("MBLX.tdm"), file = "MBLXkaggleTDM.RData")
load(file = "MBLXkaggleTDM.RData")

save(list = ("AEY.tdm"), file = "AEYkaggleTDM.RData")
load(file = "AEYkaggleTDM.RData")

save(list = ("NRX.tdm"), file = "NRXkaggleTDM.RData")
load(file = "NRXkaggleTDM.RData")

save(list = ("KMT.tdm"), file = "KMTkaggleTDM.RData")
load(file = "KMTkaggleTDM.RData")


#CONVERT TDM TO A MATRIX
THST.m <- as.matrix(THST.tdm)
MCUR.m <- as.matrix(MCUR.tdm)
MBLX.m <- as.matrix(MBLX.tdm)
AEY.m <- as.matrix(AEY.tdm)
NRX.m <- as.matrix(NRX.tdm)
KMT.m <- as.matrix(KMT.tdm)


#CALCULATE THE FREQUENCY OF WORDS
THST.wordFreq <- rowSums(THST.m)
MCUR.wordFreq <- rowSums(MCUR.m)
MBLX.wordFreq <- rowSums(MBLX.m)
AEY.wordFreq <- rowSums(AEY.m)
NRX.wordFreq <- rowSums(NRX.m)
KMT.wordFreq <- rowSums(KMT.m)


#SORT THE WORD BY DESCENDING ORDER OF FREQUENCY
THST.wordFreq <- sort(THST.wordFreq, decreasing=TRUE)
MCUR.wordFreq <- sort(MCUR.wordFreq, decreasing=TRUE)
MBLX.wordFreq <- sort(MBLX.wordFreq, decreasing=TRUE)
AEY.wordFreq <- sort(AEY.wordFreq, decreasing=TRUE)
NRX.wordFreq <- sort(NRX.wordFreq, decreasing=TRUE)
KMT.wordFreq <- sort(KMT.wordFreq, decreasing=TRUE)


#WORD CLOUD
palette <- brewer.pal(8, "Dark2")
palette
set.seed(137)
wordcloud(words = names(THST.wordFreq), freq = THST.wordFreq, min.freq = 20, random.order = F, colors = palette)
wordcloud(words = names(MCUR.wordFreq), freq = MCUR.wordFreq, min.freq = 40, random.order = F, colors = palette)
wordcloud(words = names(MBLX.wordFreq), freq = MBLX.wordFreq, min.freq = 20, random.order = F, colors = palette)

wordcloud(words = names(AEY.wordFreq), freq = AEY.wordFreq, min.freq = 100, random.order = F, colors = palette)
wordcloud(words = names(NRX.wordFreq), freq = NRX.wordFreq, min.freq = 200, random.order = F, colors = palette)
wordcloud(words = names(KMT.wordFreq), freq = KMT.wordFreq, min.freq = 20, random.order = F, colors = palette)


#PREPARE SENTIMENT WORDS REFERENCE
setwd(mainDir)
pos.words = scan('positive-words.txt',what='character',comment.char = ';')
neg.words = scan('negative-words.txt',what='character',comment.char = ';')


#FUNCTION FOR SENTIMENT CALCULATION
sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  
  # SPLIT THE TEXT INTO VECTOR
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  
  # CHECK FOR POSITIVE WORDS
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  # CHECK FOR NEGATIVE WORDS
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  # CALCULATE AND RETURN THE SCORE
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}


#FUNCTION FOR PLOTTING THE TWEETS' SCORES
graphPlot <- function(corpora, code, pos.w, neg.w){
  code.scores = c()
  code.id = c()
  code.tweets = c()
  mins = c()
  for(i in 1:length(corpora)){
    score <- sentiment(corpora[[i]]$content, pos.w, neg.w)
    code.scores <- c(code.scores, score)
    code.id <- c(code.id, corpora[[i]]$meta$id)
    code.tweets <- c(code.tweets, length(corpora[[i]]$content))
    mins <- c(mins, i)
  }
  plot(mins, code.scores, type = "h", lwd = 2, col = "red", xlab="5-Minute Time Unit")
  lines(mins, code.scores)
}

#PLOTTING THE SCORES
graphPlot(THST.corpus, THST, pos.words, neg.words)
graphPlot(MCUR.corpus, MCUR, pos.words, neg.words)
graphPlot(MBLX.corpus, MBLX, pos.words, neg.words)
graphPlot(AEY.corpus, AEY, pos.words, neg.words)
graphPlot(NRX.corpus, NRX, pos.words, neg.words)
graphPlot(KMT.corpus, KMT, pos.words, neg.words)