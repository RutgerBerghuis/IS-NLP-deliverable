library(tm)
library(stringi)
library(sylly.en)
library(ggplot2)
library(wordcloud)
loc <- readline("What is location of your file?")
corpus <- Corpus(DirSource(loc), readerControl = list(language="lat"))
corpus=corpus[1:600]
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)

df <- data.frame(words = unlist(stri_extract_all_words(stri_trans_tolower(corpus))))
vector <- unique(df$words)
length(vector)
hyph.txt.en <- hyphen(vector, hyph.pattern="en")
hyp <- hyphenText(hyph.txt.en)
vector <- hyp$word
vector <- strsplit(vector, "-", perl=TRUE)
vector <- unlist(vector)
corpus <- Corpus(VectorSource(vector))
tdm <-  TermDocumentMatrix(corpus)
freq=rowSums(as.matrix(tdm))
plot(sort(freq, decreasing = T),col="blue",main="syllable frequencies", xlab="Frequency-based rank", ylab = "Frequency")
tail(sort(freq),n=20)
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
word.cloud=wordcloud(words=names(freq), freq=freq,
                     min.freq=80, random.order=F, colors=pal)
