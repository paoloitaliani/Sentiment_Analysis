library(rvest)
library(RCurl)
library(XML)
library(dplyr)
library(purrr)
prod_code <- "B07G7MBP49"
url <- paste0("https://www.amazon.it/Xiaomi-Mi-Band-monitoraggio-frequenza/dp/B07G7MBP49?pd_rd_w=xvlmu&pf_rd_p=670dc11e-0ef0-48a1-a21e-5f8efa9ce613&pf_rd_r=6N6YJ51XW59FSQG1XK4G&pd_rd_r=c833000e-763c-460c-b9ad-ad909f5b112a&pd_rd_wg=QDqpt&ref_=pd_gw_cr_simh")
doc <- read_html(url)

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  trimws()




scrape_amazon <- function(url, throttle = 0){
  
  
  
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  # obtain HTML of URL
  doc <- read_html(url)
  
  # Parse relevant elements from HTML
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  review_format <- doc %>% 
    html_nodes(".review-format-strip") %>% 
    html_text() 
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric() 
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  suppressWarnings(n_helpful <- doc %>%
                     html_nodes(".a-expander-inline-container") %>%
                     html_text() %>%
                     gsub("\n\n \\s*|found this helpful.*", "", .) %>%
                     gsub("One", "1", .) %>%
                     map_chr(~ str_split(string = .x, pattern = " ")[[1]][1]) %>%
                     as.numeric())
  
  # Combine attributes into a single data frame
  df <- data.frame(title, author, date, review_format, stars, comments, n_helpful, stringsAsFactors = F)
  
  return(df)
}


url <- "https://www.amazon.it/Xiaomi-Mi-Band-monitoraggio-frequenza/product-reviews/B07G7MBP49?pageNumber=1"
reviews <- scrape_amazon(url)

pages <- 300

# create empty object to write data into
reviews_all <- NULL

# loop over pages
prod_code <- "B07G7MBP49"
pb <- txtProgressBar(min = 0, max = pages, style = 3)
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.it/Xiaomi-Mi-Band-monitoraggio-frequenza/product-reviews/B07G7MBP49/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
  setTxtProgressBar(pb, page_num)
  
}
close(pb)
##########ANALISI
setwd("C:/Users/paoit/Desktop/Università/Tesi")
source("textmFunctions.R")
source("clean_Tw_Fb.R")
load("dizFormeLemmi.Rdata")
source("SentimentFunctions.R")

library(tm)
library(wordcloud)


View(tweet[303:304])
tweet <- reviews_all$comments

ongtxt<-preTrat(tweet, Clean= T,FUN = cleanTweets)

tdmong<-doWrdCld(x = ongtxt,
                 col = brewer.pal(6,"Dark2"))
m<-as.matrix(tdmong)
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
                          lexicon = "DPLbuono.csv")

table(ongPol$documenti$best_fit)




tweet_df = data.frame(text=ongtxtl,
                      polarity=ongPol[[1]][,4],
                      
                      stringsAsFactors=FALSE)

dd<-cbind(ongPol$documenti,reviews_all$stars)
stars<-reviews_all$stars
dd<-cbind(dd,stars)
table(dd$rating)
########################à
dd$rating<-ifelse(dd$stars>3.5,"positivo",ifelse(dd$stars<= 2.5,"negativo","neutrale"))

ddd<-dplyr::mutate(dd, ID = row_number())


dpos<-ddd %>% filter(rating=="positivo")
dposs<-sample_n(dpos, 203,replace = T)
str(dposs)

dneu<-ddd %>% filter(rating=="neutrale")
dneus<-sample_n(dneu, 203)
str(dneus)

dneg<-ddd %>% filter(rating=="negativo")
str(dneg)
dnegs<-sample_n(dneg, 203)

drd.subj<-rbind(dneu,dnegs,dposs)
write.table(drd.subj,"drd.dpl2",sep=",")

tabConf<-table(drd.subj$rating,drd.subj$best_fit,dnn=c("Osservato","Previsto"))
fit1.Accuracy <-sum(diag(tabConf))/sum(tabConf)

 setwd("C:/Users/paoit/Desktop/Università/Tesi")

d.s2<-read.table("drd.subj2",sep=",")
d.d2<-read.table("drd.dpl2",sep=",")
d.sx2<-read.table("drd.sx2",sep=",")
d.i2<-read.table("drd.ital2",sep=",")


d.s1<-read.table("drd.subk1",sep=",")
d.d1<-read.table("drd.dpl1",sep=",")
d.sx1<-read.table("drd.sx1",sep=",")
d.i1<-read.table("drd.ital1",sep=",")

