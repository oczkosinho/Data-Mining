#Instalacja pakiet?W

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("ggplot2")
#install.packages("wordcloud")

###Corpus Preprocessing
#Start for preprocessing
#Loading (already installed) packages
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)

#Setting working directory
setwd("C:/Users/waocz/OneDrive/Pulpit/Studia/Semestr5/Data Mining in Business/projekt")

getwd()

library(tm)
Data<-read.csv("Opinie.csv",
               header = FALSE,
               sep = ",",  # or ";"
               strip.white = TRUE, 
               fill = TRUE, 
               comment.char = "#",
               stringsAsFactors = FALSE,
)

#View(Data)
#Matrix from Brand Database
MyData = as.data.frame.matrix(Data)
colnames(MyData)<-("Review")
#View(MyData)


MyData_1 <- MyData[,1] 
n<-length(MyData_1)
n
head(MyData_1) 


docs <-VCorpus(x = VectorSource(MyData_1),
               readerControl = list(reader=readPlain,
                                    language="en"))

#___________by sentences_____________________
library(tokenizers)
MyData_22<-tokenize_sentences(MyData_1)
#MyData_22<-tokenize_paragraphs(MyData_1)
length(MyData_22)
wf1=NULL
for (i in 1:n) {
  c <- data.frame(document=as.character(MyData_22[[i]][]))
  wf1=rbind(wf1,c)
}
wf1$document[2]
dim(wf1)
n<-length(wf1$document)
n
write.csv(wf1,'Sentences.csv', row.names=FALSE)


library(tm)
Data_Sentences<-read.csv("Sentences.csv",
                         header = TRUE,
                         sep = ",",  # or ";"
                         strip.white = TRUE, 
                         fill = TRUE, 
                         comment.char = "#",
                         stringsAsFactors = FALSE 
)
#head(Data_Sentences)

#Matrix from Brand Database
Data_Sentences = as.data.frame.matrix(Data_Sentences) 
dim(Data_Sentences)

docs <-VCorpus(x = VectorSource(Data_Sentences[,1]),
               readerControl = list(reader=readPlain,
                                    language="en"))

#Comments <- Corpus(VectorSource(MyData_1)) # for RStudio
#Comments <- VCorpus(VectorSource(MyData_1)) # for R 3.6.1
#_________________Preprocessing_____________________________

#Comments <- Corpus(VectorSource(MyData_1)) # for RStudio
#docs <- VCorpus(VectorSource(MyData_1)) # for R 3.6.1
docs

# number of terms in Corpus
docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)
dtm 
#write.csv(as.matrix(dtm),file="DTM.csv")

rownames(dtm)<-seq(1,n)
rownames(dtm)

#Text of Document 1
writeLines(as.character(docs [[1]]))


# comments length
doc_length <- as.data.frame(rowSums(as.matrix(dtm)))
#write.csv(as.matrix(doc_length),file="Doc_length.csv")

max_length<-max(doc_length)
max_length
min_length<-min(doc_length)
min_length
aver_length<-mean(rowSums(as.matrix(dtm)))
aver_length

#pre-processing

getTransformations()

docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers) 
writeLines(as.character(docs[[1]]))

for (j in seq(docs)) { 
  docs[[j]] <- gsub("/", " ", docs[[j]]) 
  docs[[j]] <- gsub("-", " ", docs[[j]]) 
  docs[[j]] <- gsub("â€", " ", docs[[j]]) 
  docs[[j]] <- gsub("’", " ", docs[[j]]) 
  docs[[j]] <- gsub("“", " ", docs[[j]]) 
  docs[[j]] <- gsub("…", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]]) 
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])  
}  

writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, tolower) 
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

length(stopwords("english")) 
stopwords("english")

docs <- tm_map(docs, removeWords, stopwords("English")) 
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

StW<-read.table("StopWords.txt") 
StW

StWW<-as.character(StW$V1) 
StWW

docs <- tm_map(docs, removeWords, StWW) 
docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, stripWhitespace) 
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

library(SnowballC) 

for (j in seq(docs)) { 
  docs[[j]]<-stemDocument(docs[[j]], language = "english") 
} 
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)
dtm 

rownames(dtm)<-seq(1,n)
rownames(dtm)
inspect(dtm[1:10, 1565:1570])

#write.csv(as.matrix(dtm),file="DocumentTermMatrix.csv")

#normalized matrix
xx<-rowSums(as.matrix(dtm))
xx
dtm_Norm<-dtm/xx
dtm_Norm
inspect(dtm_Norm[50:55, 1565:1570])
dtm_Norm_m<-as.data.frame(as.matrix(dtm_Norm))
#write.csv(dtm_Norm_m, file="DocumentTermMatrixNorm.csv")

#___________Histogtram of Frequency_________________
#__________________________________________________________

#___________Calculating the Frequency_______________________
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 30)
tail(freq, 15)   
d <- data.frame(word = names(freq),freq=freq)
head(d, 30)
mk<-min(head(freq, 30))
#mk<-1

#___________Building the Histogtram witn Frequency > mk_________

wf=data.frame(word=names(freq),freq=freq)  
p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Keywords for Opinions") + labs(x="Words",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1, size=16))
p


#Dorobic!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
findAssocs(dtm,"phone",0.2)

findAssocs(dtm,"room",0.2)

findAssocs(dtm,"stay",0.2)



#___________________Building Wordcloud___________________________

d <- data.frame(word = names(freq),freq=freq)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#_________________________________________________________________
#_____________________Bigrams_____________________________________
#_________________________________________________________________
NgramTokenizer = function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs, control = list(tokenize = NgramTokenizer))
dtm_n


#___________Histogtram of Bigrams__________________________________
#___________Calculating the Frequency______________________________

freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE)
head(freq_n, 15)
mk<-min(head(freq_n, 15))
tail(freq_n, 15)   
m<-as.matrix(dtm_n)
#write.csv(m, file="N_DocumentTermMatrix.csv")
#___________Building the Histogtram (zipf law)___________________
wf=data.frame(word=names(freq_n),freq=freq_n) 
wf 

dev.off()
dev.new(width = 100, height = 100, unit = "px") #could be useful
p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bi-grams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=10))
p


dev.off()
dev.new(width = 100, height = 100, unit = "px") #could be useful

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq_n), freq_n, max.words=100, rot.per=0.2, colors=dark2)   


#__________LDA Topic Modelling Prep______________________

#install.packages("topicmodels")


#___________topics modelling____________________________________
library(topicmodels) 
library(lsa)
#___________________________________________________
#check the presence of rows with a zero's sum

raw.sum=apply(dtm,1,FUN=sum) #sum by raw for each raw of the table
raw.sum

#number of rows with a zero's sum
mmm<-nrow(dtm[raw.sum==0,])
mmm

# if mmm=0, only create dtm2 and NN (number of rows in DTM)
# if mmm>0, delete the rows with zero's sum form corpus

if (mmm==0) {
  dtm2<-dtm
  NN<-nrow(dtm)
  NN
} else {
  dtm2<-dtm[raw.sum!=0,]
  NN<-nrow(dtm2)
}
#number of comments before deleting
n
#number of comments after deleting
NN
#new matrix dtm2
dtm2
#___________________________________________________
#__________LDA Topic Modelling______________________
# _____________LDA Setting__________________________
# @burnin : number of omitted Gibbs iterations at beginning
# @thin : number of omitted in-between @iter Gibbs iterations
# @nstart indicates the number of repeated runs with random initialization
# @ seed needs to have the length nstart
# If best=TRUE only the best model over all runs with respect to the log-likelihood is returned

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5

ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)
#setwd('C:\\Users\\ninar\\OneDrive\\Documents\\Dedaktyka\\DUAN\\")
#____topics keywords___________________________
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms
write.csv(ldaOut.terms,file="C:/Users/waocz/OneDrive/Pulpit/Studia/Semestr5/Data Mining in Business/projekt/TopicsToTerms_1.csv")

#___topics probability per document_____________
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities
#___Topic proportion over Corpus________________
col.sum=apply(topicProbabilities,2,FUN=sum) # 1 - rows sum, 2- columns sum
col.sum<-as.matrix(col.sum)
dim(col.sum)
sum.TP=col.sum/sum(col.sum)
sum.TP
write.csv(topicProbabilities,file="C:/Users/waocz/OneDrive/Pulpit/Studia/Semestr5/Data Mining in Business/projekt/TopicProbabilities_1.csv")

#___topics by Documents_________________________
ldaOut.topics <- as.matrix(topics(ldaOut))
View(rownames(ldaOut.topics))
View(rownames(dtm))
#error dtm -> dtm2
rownames(ldaOut.topics)<-as.character(rownames(dtm2)) 
ldaOut.topics
write.csv(ldaOut.topics,file="C:/Users/waocz/OneDrive/Pulpit/Studia/Semestr5/Data Mining in Business/projekt/DocsToTopics_1.csv")

#___________Topics Data Frame building_______________________________
nrow(ldaOut.topics)
Comment<-seq(1, NN, by=1)
Comment
wf=data.frame(Comment=Comment, Topics=ldaOut.topics)
wf
#________________________________________________________
#___________Building Sub-Corpus of Topic 1_________________
topic1<-wf[wf[2] == 1,]  #find Topic 1 po == sie jest numer 1/3
topic1$Comment
length(topic1$Comment)
kk1<-nrow(topic1)
kk1
########
topic2<-wf[wf[2] == 2,]
length(topic2$Comment)
kk1<-nrow(topic2)
kk1
###
topic3<-wf[wf[2] == 3,]
length(topic3$Comment)
kk1<-nrow(topic3)
kk1
########
topic4<-wf[wf[2] == 4,]
length(topic4$Comment)
kk1<-nrow(topic4)
kk1
########
topic5<-wf[wf[2] == 5,]
length(topic5$Comment)
kk1<-nrow(topic5)
kk1
########
kk<-nrow(dtm2)
kk

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==5) {               # Tu zmieniac XD
    list1<-c(list1,i)}
  i=i+1
}
list1
wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(Comment[[i]]))
      wf1=rbind(wf1,c)
    } 
  }
}
wf1
wf1$document[1]

#___________Corpus Creating_________________________________________
Topic_1_docs <- Corpus(VectorSource(as.character(wf1$document))) #Corpus for Topic 1
writeLines(as.character(Topic_1_docs[[1]])) #Corpus for Topic 1

#___________Writing in CSV File_ with Comments of Topic 1 __________

mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=TRUE)
help(data.frame)
mycorpus_dataframe

write.csv(mycorpus_dataframe,'Topic 1_Comments.csv', row.names=FALSE) #File with documents for Topic 1

#________________________________________________________________________
# Do the all steps above for Building Sub-Corpus of Topic, Creating the Corpus and
# Writing in CSV File_ with Comments of Topic 1 for each of 5 topic
# from part "Building Sub-Corpus of Topic 1"


#________________________________________________________________________
#________________________________________________________________________
#____________________SENTIMENT ANALYSIS__________________________________
#________________________________________________________________________
#________________________________________________________________________
# Do the all steps below for Sentiment Analysis for each of 5 Topics Corpora 
# using differen Topic_N_docs Corpora: 
#Topic_1_docs
#Topic_2_docs
#Topic_3_docs
#Topic_4_docs
#Topic_5_docs
#________________________________________________________________________

#_________________SENTIMENT_1st OPTION____________________________________
#install.packages("plyr")
#install.packages("stringr")
library("plyr")
library("stringr")
# Download the Folder Lexicon
# https://drive.google.com/open?id=1NVnThfr8ia-fLTvR2M0qkEpeVdYSRqGd
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English
# and set the Path to this Folder 

neg=scan("Lexicon/negative-words.txt", what="character", comment.char=";" )
pos=scan("Lexicon/positive-words.txt", what="character", comment.char=";" )

#Nasze

for (j in seq(neg)) { 
  neg[[j]]<-stemDocument(neg[[j]], language = "english") 
} 

for (j in seq(pos)) { 
  pos[[j]]<-stemDocument(pos[[j]], language = "english") 
} 

#__________Initialization of the Sentiment analysis Procedure_______________

V <- levels(mycorpus_dataframe$text)
X <- as.numeric(V)
X
docs_w <- Data_Sentences$document[X]
View(docs_w)
write.csv(docs_w,'Topic 1_CommentsXD.csv', row.names=FALSE)
score.sentiment = function(docs, pos.words, neg.words, .progress='none')
{
  scores = laply(docs_s, function(docs, pos.words, neg.words) {
    
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=docs)
  return(scores.df)
}

#___________________Topic 1 Sentiment Scoring _______________________                                                                                     

result=c()

docs<-docs_w # You need to replace it into Topic_2_docs, ...Topic_5_docs in the next steps
m1=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  #print(newRow1) 
  m1<- rbind(m1,newRow1)
  #print(m1) 
}
head(m1)
m1[1:3,]
#______________________Statistics______________________________________________
summary(m1$Score)
minn<-min(m1$Score)
minn
maxx<-max(m1$Score)
maxx
mmm<-maxx-minn
mmm
#__________Topic 1___Histograms_______________________________________________

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


tdmr<-removeSparseTerms(dtm1w, 0.35)
tdmr
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)


#______________________Histogram_1____________________________________________
h<-hist(m1$Score, 
        main="Histogram for the Sentiment by Topic 5", 
        xlab="Scores", 
        ylab="Number of of Opinions",
        right=FALSE,
        border="blue", 
        col="green",
        freq=TRUE,
        las=1,
        xlim=c(minn,maxx),
        breaks=mmm
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
m1$Score
h$count

#______________________Histogram_2____________________________________________
hist(m1$Score, 
     main="Histogram for the Sentiment by Topic 5", 
     xlab="Scores", 
     ylab="Probability", 
     border="blue", 
     col="green",
     prob = TRUE,
     right=FALSE,
     xlim=c(minn,maxx),
     breaks=mmm
)
lines(density(m1$Score))

m11<-as.matrix(m1)
m11
write.csv(m11, file="Sent_1.csv")



#____________Division into positive and negative opinions______________
#_________________________Topic 1_______________________________________________
#____________Positive - score >=1 _______________________________________________

pos1<-m1[m1$Score>=1,]

pos1$Documents
pos1$Score
length(pos1$Score)

#____________Neutral - score <1 and  >=0 __________________________________________

neu1<-m1[(m1$Score<1)&(m1$Score>=0),]

neu1$Documents
neu1$Score
length(neu1$Score)

#____________Negative - score <0 _______________________________________________
neg1<-m1[m1$Score<0,]

neg1$Score
length(neg1$Score)
neg1$Documents

#____________Topic 1: List of Positive, Neutral and Negative opinions______________

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
pos_docs_1
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neu_docs_1
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))
neg_docs_1

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1235)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#___________Writing in CSV File_ with Positive Comments of Topic 1 __________

pos_docs_1_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
pos_docs_1_dataframe
write.csv(pos_docs_1_dataframe,'Pos_Topic 1_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Negative Comments of Topic 1 __________

neg_docs_1_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
neg_docs_1_dataframe
write.csv(neg_docs_1_dataframe ,'Neg_Topic 1_Comments.csv', row.names=FALSE)


#___________Writing in CSV File_ with Neutral Comments of Topic 1 __________

neu_docs_1_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
neu_docs_1_dataframe
write.csv(neu_docs_1_dataframe ,'Neu_Topic 1_Comments.csv', row.names=FALSE)

###JEBAC




#________________________________________________________________________
# Do the same steps of Sentiment Analysis for each of 5 Topics Corpora 
# using different Topic_N_docs Corpora: 
#Topic_1_docs
#Topic_2_docs
#Topic_3_docs
#Topic_4_docs
#Topic_5_docs
###
#____SENTIMENT_2nd OPTION__________________________________
#______________________________________________
#___syuzhet____________________________________
#http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#https://www.springboard.com/blog/text-mining-in-r/

#install.packages("syuzhet", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(syuzhet)
#____________Read File with Topic 1___________________________________
Topic_1<-read.csv("Topic 1_CommentsXD.csv",
               header = TRUE,
               sep = ",",  # or ";"
               strip.white = TRUE, 
               fill = TRUE, 
               comment.char = "#",
               stringsAsFactors = FALSE 
)

View(Topic_1)
head(Topic_1)

#Matrix from Brand Database
Topic_1 = as.data.frame.matrix(Topic_1) 
mycorpus_dataframe1<- data.frame(text=Topic_1, stringsAsFactors=F)
mycorpus_dataframe1

#remove all non graphical characters 
usableText=str_replace_all(mycorpus_dataframe1$text,"[^[:graph:]]", " ")

d<-get_nrc_sentiment(usableText)
head(d)
d$anger

t(d)
td<-data.frame(t(d))
td[,5]

td_new <- data.frame(rowSums(td))
td_new

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new
td_new2<-td_new[1:10,]
td_new2

#Visualisation
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Opinion sentiments for Topic ___ by___Year and by____ City")

#________________________________________________________________________
# Do the same steps of Sentiment Analysis for each of 5 Topics Corpora 
# using different Topic Corpora: 
#Topic 1_Comments.csv
#Topic 2_Comments.csv
#Topic 3_Comments.csv
#Topic 4_Comments.csv
#Topic 5_Comments.csv
