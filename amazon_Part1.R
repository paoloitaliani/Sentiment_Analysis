library(rvest)
library(RCurl)
library(XML)
library(dplyr)
library(purrr)
library(stringr)
prod_code <- "B00G98EJHM"
url <- paste0("https://www.amazon.it/Nespresso-Citiz-macchina-caff%C3%A8-espresso/dp/B01FKITAWU?pd_rd_wg=84U5J&pd_rd_r=5025f3a7-678a-4cb8-a11d-de30305ab473&pd_rd_w=PQLQa&ref_=pd_gw_ri&pf_rd_r=X69XHJXQFEVE58VH20J7&pf_rd_p=965378cd-4c7f-58fb-bbad-2e4a12d136e6")
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

url <- "https://www.amazon.it/product-reviews/B00G98EJHM/?pageNumber=1"
reviews <- scrape_amazon(url)

pages <- 1000

# create empty object to write data into
reviews_all <- NULL

# loop over pages
prod_code <- "B00G98EJHM"
pb <- txtProgressBar(min = 0, max = pages, style = 3)
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.it/product-reviews/B00G98EJHM/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
  setTxtProgressBar(pb, page_num)
  
}

str(reviews_all)
##########ANALISI
setwd("C:/Users/paoit/Desktop/Università/Tesi")
source("textmFunctions.R")
source("clean_Tw_Fb.R")
load("dizFormeLemmi.Rdata")
source("SentimentFunctions.R")

library(tm)
library(wordcloud)




tweet <- reviews_all$comments

ongtxt<-preTrat(tweet, Clean= T,FUN = cleanTweets)

tdmong<-doWrdCld(x = ongtxt,
                 col = brewer.pal(6,"Dark2"))
m<-as.matrix(tdmong)

word_freqs= sort(rowSums(m), decreasing=TRUE)
tdmongl<-lemmatiz(x = tdmong, comparison= T)
dm = data.frame(word=names(word_freqs), freq=word_freqs)



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
ongPol <- myClassPolarity(ongtxtl,algorithm="count", prior=0.5,
                          lexicon = "DPLbuono.csv")

table(ongPol$documenti$best_fit)




tweet_df = data.frame(text=ongtxtl,
                      polarity=ongPol[[1]][,4],
                      
                      stringsAsFactors=FALSE)


dd<-cbind(ongPol$documenti,reviews_all$stars)
stars<-reviews_all$stars
dd<-cbind(dd,stars)

########################à
dd$rating<-ifelse(dd$stars>3.5,"positivo",ifelse(dd$stars<= 2.5,"negativo","neutrale"))

ddd<-dplyr::mutate(dd, ID = row_number())

dpos<-ddd %>% filter(rating=="positivo")
dposs<-sample_n(dpos, 265,replace = T)
str(dposs)

dneu<-ddd %>% filter(rating=="neutrale")
dneus<-sample_n(dneu, 265)
str(dneus)

dneg<-ddd %>% filter(rating=="negativo")
str(dneg)
dnegs<-sample_n(dneg, 265)

str(dnegs)
drd.ital<-rbind(dneus,dnegs,dposs)


write.table(drd.ital,"drd.dpl3",sep=",")


drd.dpl3<-read.table("drd.dpl3",sep=",")

drd.dpl3p<-subset(drd.dpl3, )
