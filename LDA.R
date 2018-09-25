library(tm)
library(pdftools)
library(SnowballC)
library(wordcloud)
library(Matrix)
library(NLP)
library(stringr)
library(tidytext)
library(topicmodels)
Filepath = "C:/Users/Hannan Qazi/Desktop/ClassDemonstration/"
setwd(Filepath)
dir(Filepath)

file1 = "HBO_NOW.txt"

file1 = file(file1,open="r")
text.decomposition = readLines(file1)
text.decomposition[1]

corpus_txt = Corpus(VectorSource(text.decomposition))
corpus_txt
corpus_HBO = tm_map(corpus_txt,PlainTextDocument)
corpus_HBO = tm_map(corpus_HBO,tolower)
corpus_HBO = tm_map(corpus_HBO,removeNumbers)
corpus_HBO = tm_map(corpus_HBO,removePunctuation)
corpus_HBO = tm_map(corpus_HBO,stemDocument) #stem the words.

stopwords("english")
mystopwords = c("hbo","now","app")
corpus_HBO_clean = tm_map(corpus_HBO,removeWords,c(stopwords("english"),mystopwords))

corpus.dtm = DocumentTermMatrix(corpus_HBO_clean)
write.csv(as.matrix(corpus.dtm),file=file.path("dtm-hbo.csv"))

#number of topics
candidate_k = c(2,3,4,5,10,20,40,60,100)

results = matrix(0,nrow=length(candidate_k),ncol=2)

colnames(results) = c('k',"perplexity")
results

for(j in 1:length(candidate_k)){
  k=candidate_k[j]
  SEED = 2010
  text.lda = LDA(corpus.dtm,k=k,method = "Gibbs",
                 control = list(seed=SEED,burnin=1000,
                                thin = 100,iter=1000))
  results[j,] = c(k,perplexity(text.lda,newdata = corpus.dtm))
}

results_df = as.data.frame(results)
results_df

plot(results_df)

#-------------------------------------------
k=10

SEED = 2010
text.lda2 = LDA(corpus.dtm,k=k,method = "Gibbs",
                control = list(seed=SEED,burnin=1000,
                               thin = 100,iter=1000))
#term topic probabilities
Term = posterior(text.lda2)$terms
write.csv(as.matrix(Term),file=file.path("term.csv"))

term2 = tidy(text.lda2,matrix="beta")
write.csv(as.matrix(term2),file=file.path("term2.csv"))

#-------------------------------------------
#document topic pobabilities
topic = posterior(text.lda2)$topics
write.csv(as.matrix(topic),file=file.path("topic.csv"))

topic2 = tidy(text.lda2,matrix="gamma")
write.csv(as.matrix(topic2),file=file.path("topic3.csv"))

#top 10 terms
top.terms=terms(text.lda2,10)

