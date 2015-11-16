## Installer des packages 
# pack <- c("twitteR","tm","cluster","FactoMineR","RColorBrewer","ggplot2","slam", "textcat", "koRpus")
# install.packages(pack, dependencies = TRUE)

rm(list=ls())

# load packages

library(tm)
library(cluster)
library(FactoMineR)
library(RColorBrewer)
library(ggplot2)
library(slam)
library(textcat)
library(koRpus)

setwd("~/Téléchargements/CorpusR/")
source("~/github/presentations/presentation_R/S4/scripts/lemmatisation.R")

#specifies the exact folder where my text file(s) is for analysis with tm.
myTxt <- Corpus(DirSource("."), readerControl = list(language="lat")) 


# remove stoprwords
corp <- tm_map(myTxt, removeWords, c(stopwords("french")))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, content_transformer(removeNumbers))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeWords, stopwords("french"))
corp <- tm_map(corp, stripWhitespace); #inspect(docs[1])
# corp <- tm_map(corp, stemDocument)
# remove extra white-spaces
#corp = tm_map(corp, stripWhitespace)


my.lemma <- lemmatisation(myTxt)


tdm = TermDocumentMatrix(corp)

m <- aggregate(.~lemma, merge(inspect(tdm), my.lemma[, c("token", "lemma")], 
                                                    by.x="row.names", by.y="token")[-1], sum)

rownames(m)<-m[,1]
m<-m[-1,c(2:21)]

# convert as matrix
m = as.matrix(m)



# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# distance matrix with binary distance
m1dist = dist(m1, method="binary")

# cluster with ward method
clus1 = hclust(m1dist, method="ward.D")

# plot dendrogram
plot(clus1, cex=0.7)

# correspondance analysis
rei_ca = CA(m1, graph=FALSE)


# partitioning around medoids iwth 6 clusters
k = 6

# pam clustering
rei_pam = pam(rei_ca$row$coord[,1:2], k)

# get clusters
clusters = rei_pam$clustering

# create data frame
rei_words_df = data.frame(
  words = rownames(m1),
  dim1 = rei_ca$row$coord[,1],
  dim2 = rei_ca$row$coord[,2],
  freq = rowSums(m1),
  cluster = as.factor(clusters))

# plot
ggplot(rei_words_df, aes(x=dim1, y=dim2, label=words)) +
  geom_text(aes(size=freq, colour=cluster), alpha=0.7) +
  scale_size_continuous(breaks=seq(20,80,by=10), range=c(3,8)) +
  scale_colour_manual(values=brewer.pal(8, "Dark2")) +
  labs(x="", y="") +
  theme_bw()

library(wordcloud)
# comparison cloud
comparison.cloud(m, random.order=FALSE, 
                 title.size=1.5, max.words=500)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
