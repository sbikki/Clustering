#hierarchical clustering
#Read the nytimes data
#A Dendogram is a tree like structure and putiing an edge between A and B
#Dendogram represents the hierarchial structure

nytimes1<-read.csv("nytimes.csv",h=FALSE)

View(nytimes1)
#Row bind all the files

obama<-rbind(nytimes1)
str(obama)
tweets<-data.frame(obama$V3)
View(tweets)
names(tweets)<-"Tweet_Text"
str(tweets)
View(tweets)

#Data Pre-processing using tm package
library(tm)

#Building a Text Corpus
tweets.corpus<-Corpus(VectorSource(tweets$Tweet_Text))
summary(tweets.corpus)
inspect(tweets.corpus[1:5]) #Inspecting elements in Corpus

#Removing stop words
#trying to remove the http which refer to the url's
#You can also try and remove frequent words like "Obama","Barack" that occur in the document
my_stopwords<-c(stopwords('english'),'available','http') #Can add more words apart from standard list
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)

#Data Transformations
tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'available','http') #Can add more words apart from standard list
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)

#Building term document matrix
#What is a tdm? 
tweets.tdm<-TermDocumentMatrix(tweets.corpus)
tweets.tdm
dim(tweets.tdm) #Dimensions of term document matrix
inspect(tweets.tdm[1:10,1:10]) #Inspecting the term document matrix

#---Hierarchical clustering --------
#We have already created the Term document matrix in the session Tm package with R--

#Remove sparse terms
#A sparse parameter of 0.9 means that we select from those terms which are less than 90% empty. 
tweets.rs<-removeSparseTerms(tweets.tdm,sparse=0.97)
tweets.matrix<-as.matrix(tweets.rs)
View(tweets.matrix)
class(tweets.matrix)


#Finding the Distance metric between terms in the document
distmatrix<-dist(scale(tweets.matrix),method="euclidean")
#Applying the hierarchcal clusterting algorithm
tweets.h<-hclust(distmatrix,method="ward")
tweets.h<-hclust(distmatrix,method="ward.D")
tweets.h<-hclust(distmatrix,method="ward.D2")

?hclust

#the cluster height is used in the Dendogram
tweets.h$height

#labels for each of the objects being clustered
tweets.h$labels

#distance measure used
tweets.h$dist.method

#to get flat clustering
k<-cutree(tweets.h, k = 3)
k

# install.packages('ggdendro')
library(ggdendro)

#plot dendogram
plot(tweets.h,cex=0.1,hang=-1,which.plots = 2,main="Word cluster Dendogram")

#Other packages 


# basic option
ggdendrogram(tweets.h,cex=0.9)
ggdendrogram(tweets.h, rotate = FALSE, size = 4, theme_dendro = FALSE, color = "tomato")

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(tweets.h, k = 3, cex=0.5,boxes = FALSE, col.up = "grey50", col.down = c("green", 
                                                                                "blue", "black","red","yellow","orange","brown"))
