setwd("C:/Users/paoit/Desktop/Università/Tesi")



library(rtweet)
appname <- "nome applicazione"
API_Key <- "06ePVShnnD2NH366Efmn79iwa"
API_Secret <- "Lb8hPAyiZLmFr4veleU9nDy25OcY05M6BQ3KLcKsVRDd5DQV8k"
Access_Token <- "1127511670472024064-maM425t5jRScyUZVoKtlClSOOZaddR"
Access_Token_Secret <- "TgLVBZ0ysY8F698jmfWXW6vr19mS2KLqfTXtfBL6FFfRT"


twitter_token <- create_token(
  app = appname,
  consumer_key = API_Key,
  consumer_secret = API_Secret)

twitter_token <- create_token(
  app = appname,
  consumer_key = API_Key,
  consumer_secret = API_Secret,
  access_token = Access_Token,
  access_secret = Access_Token_Secret)

tw2 <- search_tweets(q = "salvini",
                     n = 1000,
                     include_rts = F,
                     type = "recent", lang="it")
tweet <- tw2$text
# ricodifica da UTF-8 a latin1
tweet = iconv(tweet, from = "UTF-8", to = "latin1", sub = "")
# rimuove i link
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
# i riferimenti nei retweet
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
tweet = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# punteggiatura esclusi # e @
tweet = gsub("([#@])|[[:punct:]]", " \\1", tweet)
# hashtag
tweet = gsub("#\\w+", " ", tweet)
# mention
tweet = gsub("@\\w+", " ", tweet)
# caratteri di controllo
tweet = gsub('[[:cntrl:]]', ' ', tweet)
# quelli che non sono caratteri grafici
#(quello che non è [[:alnum:][:punct:]])
tweet = gsub('[^[:graph:]]', ' ', tweet)
# tabulazioni e più spazi in mezzo al testo
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub(' +',' ',tweet)
# spazi all'inizio e alla fine
tweet = gsub("^\\s+|\\s+$", "", tweet)
# numeri
tweet = gsub("[[:digit:]]", " ", tweet)
# trasforma tutto in minuscolo
tweet = tolower(tweet)


library(ngram)
testo <- tweet
testo <- gsub("flat tax","",tweet)
# elimino gli spazi eccessivi
testo = gsub(' +',' ',testo)
testo = gsub("^\\s+|\\s+$", "", testo)
# conteggio del numero di parole presenti per tweet
# dichiarazione del vettore numerico in cui
# registrare il numero di parole di ciascun tweet
vnWrd <- numeric()
for(i in 1:length(testo)){
  vnWrd[i] <- wordcount(testo[i])
}
# n-gram con n=3 sequenze di 3 parole più frequenti
ng <- ngram(testo[vnWrd>2], n=3)
ngt <- get.phrasetable(ng)
ngt[ngt$freq>25,]


ng <- ngram(testo[vnWrd>1], n=2)
ngt <- get.phrasetable(ng)
ngt[ngt$freq>20,]
testo <- gsub("di maio", "dimaio",testo)


library(tm)
crp <- Corpus(VectorSource(testo))
# rimozione delle stopwords italiane
stopwrd <- iconv(stopwords("italian"),
                 from = "UTF-8", to = "latin1", sub = "")
crp <- tm_map(crp, removeWords, stopwrd)
# rimozione delle stopwords definite dall'utente
mystopw <- c("flat","tax","flattax")
crp <- tm_map(crp, removeWords, mystopw)

tdm <- TermDocumentMatrix(crp)
m = as.matrix(tdm)
mtdm <- as.data.frame(m)

load("./functions/formelemmi.RData")
vLem <- character()
vCGr <- character()
{pb <- txtProgressBar(min = 0,max = nrow(mtdm),style=3)
  for(i in 1:nrow(mtdm)){
    setTxtProgressBar(pb,i)
    prl <- rownames(mtdm)[i]
    ll <- formelemmi[formelemmi$forma == prl, c("lemma", "CatGrL")]
    if(nrow(ll)>0){
      vLem[i] <- ll[1, "lemma"]
      vCGr[i] <- ll[1, "CatGrL"]
    } else {
      vLem[i] <- prl
      vCGr[i] <- "unknown"
    }
  }
  close(pb)}
dftmp <- data.frame(forma = rownames(mtdm), lemma = vLem,
                    CatGr = vCGr, mtdm, Tot = rowSums(mtdm),
                    stringsAsFactors = F)
load("formelemmi.RData")
vLem <- character()
vCGr <- character()
{pb <- txtProgressBar(min = 0,max = nrow(mtdm),style=3)
  for(i in 1:nrow(mtdm)){
    setTxtProgressBar(pb,i)
    prl <- rownames(mtdm)[i]
    ll <- formelemmi[formelemmi$forma == prl, c("lemma", "CatGrL")]
    if(nrow(ll)>0){
      vLem[i] <- ll[1, "lemma"]
      vCGr[i] <- ll[1, "CatGrL"]
    } else {
      vLem[i] <- prl
      vCGr[i] <- "unknown"
    }
  }
  close(pb)}
dftmp <- data.frame(forma = rownames(mtdm), lemma = vLem,
                    CatGr = vCGr, mtdm, Tot = rowSums(mtdm),
                    stringsAsFactors = F)

library(dplyr)
rownames(dftmp) <- NULL
dfLemm <- dftmp[,-1] %>%
  group_by(lemma,CatGr) %>%
  summarise_all(sum)
dfLemm <- dfLemm[order(dfLemm$Tot,decreasing = T),]
dfLemm[1:10,c(1:5,1000:1001)]


