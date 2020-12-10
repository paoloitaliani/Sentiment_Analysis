setwd("C:/Users/paoit/Desktop/Università/Tesi")
 appname <- "nome applicazione"
 API_Key <- "06ePVShnnD2NH366Efmn79iwa"
 API_Secret <- "Lb8hPAyiZLmFr4veleU9nDy25OcY05M6BQ3KLcKsVRDd5DQV8k"
 Access_Token <- "1127511670472024064-maM425t5jRScyUZVoKtlClSOOZaddR"
 Access_Token_Secret <- "TgLVBZ0ysY8F698jmfWXW6vr19mS2KLqfTXtfBL6FFfRT"
 
 library(tm)
 library(wordcloud)
 library(rtweet)
 
 twitter_token <- create_token(
   app = appname,
   consumer_key = API_Key,
   consumer_secret = API_Secret)
 

source("textmFunctions.R")
source("clean_Tw_Fb.R")
load("dizFormeLemmi.Rdata")
source("SentimentFunctions.R")

tw2 <- search_tweets(q = "salvini",
                     n = 1,
                     include_rts = F,
                     type = "recent", lang="it")
tweet <- tw2$text

ongtxt<-preTrat(tweet, Clean= T,FUN = cleanTweets)
mystopwords<-c("salvini")
tdmong<-doWrdCld(x = ongtxt, stopw= mystopwords,
                 col = brewer.pal(6,"Dark2"))
m<-as.matrix(tdmong)
head(tdmong)
word_freqs= sort(rowSums(m), decreasing=TRUE)
tdmongl<-lemmatiz(x = tdmong, comparison= T)
dm = data.frame(word=names(word_freqs), freq=word_freqs)

head(dm,10)


ricTxtLem<-function(x = NULL){
  # x = data.framedella termdocumentmatrixlemmatizzata
  if(is.null(x)){return()}
  out <-character()
  nc<-dim(x)[2]-1
  nt <-0
  for(i in 3:nc){
    nt <-nt + 1
    out[nt] <-paste(x[x[,i] > 0, "lemma"], collapse= " ")
  }
  return(out)
}
ongtxtl<-ricTxtLem(tdmongl)
ongPol <- myClassPolarity(ongtxtl,algorithm="bayes", prior=0.5,
                          lexicon = "buonosx.csv")


table(ongPol$documenti$best_fit)
par(mar=c(3,4,1,1))
barplot(prop.table(table(ongPol[[1]][,4]))*100,ylim=c(0,60),
        ylab="valori %", cex.lab=0.8,cex.axis = 0.7,cex.names = 0.9)
grid(nx = 0,ny = 6,col = "gray50")


tweet_df = data.frame(text=ongtxtl,
                      polarity=ongPol[[1]][,4],
                
                      stringsAsFactors=FALSE)
polar = levels(factor(tweet_df$polarity))
polar

nemo = length(polar)
plr.docs = rep("", nemo)
for (i in 1:nemo){
  plr.docs[i] = paste(tweet_df[tweet_df$polarity==polar[i],"text"],
                      collapse = " ")
}
corpus = Corpus(VectorSource(plr.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = polar
mycol <- c("darkred","gray70","darkgreen")
tdc<-comparison.cloud(tdm, colors = mycol, max.words=150,
                 scale = c(3,.3), random.order = FALSE, title.size = 1.3)

