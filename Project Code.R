setwd("C:/College/KDD/project/")
library(tm)
docs<-Corpus(DirSource("diabetes"))
writeLines(as.character(docs[[1]]))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeWords, stopwords("english"))
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("\"", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("~", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
  docs[[j]] <- gsub("\u0090", " ", docs[[j]])
  docs[[j]] <- gsub("Ãâ???zÂ", " ", docs[[j]])# This is an ascii character that did not translate, so it had to be removed.
}

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
docs <- tm_map(docs, content_transformer(removeNumPunct))


myStopwords <- c("can", "say","one","way","use","also","howev","tell","will",
                 "much","need","take","tend","even","like","particular","rather","said",
                 "get","well","make","ask","come","end","first","two","help","often","may",
                 "might","see","someth","thing","point","post","look","right","now","think",
                 "’ve ","’re ","remains","suggesting","rapidly","principal","involving",
                 "millions","publication","supporting","opportunity","dedicated","fully","continues","avoid","assessed","basis","plays","defined","causing","investigate","released","founded","examine","majority","allows","concluded","presence","respond","options","amounts","michael","continued","differ","reach","ensure","toward","april","demonstrate","studys","actually","getting","though","indicated","contains","course","variety","monitor","learn","instead","revealed","option","forms","larger","seven","estimates","create","march","suffer","properly","chair","necessary","comprehensive","generally","success","improves","performed","essential","activities","evaluate","notes","completed","explain","effectiveness","industry","officer","resulting","ongoing","raise","signs","normally","underlying","fewer","reduces","investigation","influence","burden","agents","short","promising","noted","clinicians","actual","twice","analyzed","existing","evaluated","regulation","processes","methods","helping","advance","progress","managing","whole","allow","great","controls","combined","protect","executive","difficult","leader","environment","controlling","approaches","prior","suggested","profile","thought","assess","effectively","indicate","maintain","resources","complete","network","regulate","status","highly","worlds","country","require","implications","promote","pathways","looked","environmental","third","simple","along","nations","successful","central","strong","explains","highest","responsible","stage","plans","lowering","occur","member","david","appear","design","direct","experienced","strategy","regarding","field","attacks","researcher","relationship","commonly","latest","usually","please","light","method","appropriate","tolerance","little","works","specifically","moderate","beneficial","versus","explained","impaired","initial","samples","offer","physician","forward","makes","experts","genetics","grant","offers","improvements","unique","canada","centre","achieved","recommendations","companies","written","various","clear","deaths","knowledge","boston","directly","persons","innovative","means","natural","issues","despite","awareness","independent","whose","excess","nature","approval","clinic","place","collaboration","molecule","associations","examined","efforts","start","address","followed","looking","secretion","leads","chemical","providing")
#remove custom stopwords
docs <- tm_map(docs, removeWords, myStopwords)

#dtm <- DocumentTermMatrix(docs)
#dtm <- removeSparseTerms(dtm, 0.95)
dtm3 <-DocumentTermMatrix(docs, control=list(wordLengths=c(5, 20)))

dtm3 

freq <- sort(colSums(as.matrix(removeSparseTerms(dtmss, 0.95))),decreasing = TRUE)

length(freq) 

head(table(freq), 20)

#ways to view term frequency
freq <- sort(colSums(as.matrix(dtm4)), decreasing=TRUE)   
head(freq, 14)
tail(freq, 14)

findFreqTerms(dtm4, lowfreq=50)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library("SnowballC")
library("wordcloud")
library("RColorBrewer")
set.seed(1234)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 2000,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

df_dis <- wf[wf$word == "sleep",]
df_dis
write.csv(wf, file = "df.csv")

dtm4 <- DocumentTermMatrix(docs, control = list(dictionary=c("sleep","brain","coffee","cancer","children","diabetes")))
#plotting the words
install.packages("ggplot2")
library(ggplot2)

p <- ggplot(subset(wf, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

p

#relationships between words

findAssocs(dtm3, c("brain" , "diabetes"), corlimit=0.05)

findAssocs(dtm, "university", corlimit=1)


#clustering

install.packages("cluster")
library(cluster)
dtmss <- removeSparseTerms(dtm3, 0.95)
dtmss
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   # for a different look try substituting: method="ward.D"
fit

plot(fit, hang=-1) 

#creating borders

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=2, border="red") 



library(fpc)   
d <- dist(t(dtm3), method="euclidian")   
kfit <- kmeans(d, 15)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, xlim = c(6.5,9), ylim = c(-2,2))

install.packages("infotheo")
library(infotheo)

entropy <- function(p) {
  # Assumes: p is a numeric vector
  if (sum(p) == 0) {
    return(0) 
  }
  p <- p/sum(p) # Normalize so it sums to 1
  p <- p[p > 0] # Discard zero entries (because 0 log 0 = 0)
  H = -sum(p*log(p,base=2))
  return(H)
}

m3<-as.matrix(dtmss)
grep('dis',m3, value=TRUE)
df3<-as.data.frame(m3)

entropy_dis<-df3[,"european"]

entropy(entropy_dis)

dtm_tfxidf <- weightTfIdf(dtm3)
m1 <- as.matrix(dtm_tfxidf)
m<-t(m1)
memory.limit(size=56000)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
num_cluster<-10
cl <- kmeans(t(m1), num_cluster)

cl

plot(m,col=cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)


install.packages("infotheo")
library(infotheo)


entropy_dis1<-df3[,"diabetes"]
entropy_dis2<-df3[,"sleep"]

mutinformation(entropy_dis1,entropy_dis2,method="emp")

#calculate correlation
m<-as.matrix(dtm3)
df<-as.data.frame(m)

diabetes_df<-df[,"diabetes"]
brain_df<-df[,"cancer"]

brain_df<-df[,c("disease","brain","sleep","children","coffee")]
cor.test(brain_df,diabetes_df , method = "pearson")$p.value
brain_df
cor.test()

#topic modeling****
install.packages("topicmodels")
library(topicmodels)


memory.limit(size=56000) 
rowTotals <- apply(dtm3 , 1, sum)
dtm.new   <- dtm3[rowTotals> 0, ]   


ap_lda <- LDA(dtm.new, k = 5, control = list(seed = 2345))
ap_lda

install.packages("tidytext")
library(tidytext)

install.packages("magrittr")
library(magrittr)

# Word-Topic probabilities
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

# Visualization of the top 10 words for each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(30, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

topics(ap_lda)

terms(ap_lda)
#The words with the greatest differences among the three topics are visualized in Figure. We filter out those rare words (<1/1000).

install.packages("tidyr")
library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001| topic3 > .001| topic4 > .001| topic5 > .001) %>%
  mutate(entropy = -topic1*log2(topic1)-topic2*log2(topic2)-topic3*log2(topic3)-topic4*log2(topic4)-topic5*log2(topic5))

beta_spread

beta_spread %>%
  top_n(-10, entropy) %>%
  mutate(term = reorder(term, entropy)) %>%
  ggplot(aes(term, entropy)) +
  geom_col() +
  labs(y = "entropy of terms over the five topics distribution") +
  coord_flip()

#Document-topic probabilities
library(tidyr)
install.packages("tidytext")
library(tidytext)
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

gamma_spread <- ap_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, gamma)
