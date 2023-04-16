library(tosca)

#-----------------------------------------------------------------------------
# LDA
#-----------------------------------------------------------------------------

# Define columns to be used from the CSV 
cols = c("Nr.", "Eingereichter.Text", "Einreichungsdatum", "Titel")

# generate textmeta object from the csv
textmeta <- readTextmeta(affairs, cols = cols, dateFormat =
                           "%d.%m.%Y",idCol = "Nr.", dateCol = "Einreichungsdatum", textCol =
                           "Eingereichter.Text", titleCol= "Titel")

# Preprocessing
corpusClean <- cleanTexts(object = textmeta, sw = c(tm::stopwords("de"),"schweiz","schweizer", "bundesrat", "dass", "bereits", 
                                                    "sowie", "wurden", "wurde", "insbesondere", "jahr", "gemäss", 
                                                    "mehr", "seit", "jedoch", "sr", "nr", "jahren", "zudem", "müssen", 
                                                    "dabei", "kantone", "kantonen", "beim", "deshalb", "weitere", "davon", 
                                                    "a", "b", "z", "somit", "sollen"), checkUTF8=F)
#-----------------------------------------------------------------------------
# Corpus Überblick
#-----------------------------------------------------------------------------
# Summary of Corpus
summary(corpusClean)

# plotting the corpus as time-series, other possible: month, weeks, bi-month, days
plotScot <- plotScot(corpusClean, curves = "both", unit="year")

# store timeseries as csv 
write.csv(plotScot, file = "plotScot_I.csv")


library(ggplot2)
plot_counts <- ggplot(data = plotScot,
         mapping = aes (x = date)) +
  geom_histogram(binwidth = 20) +
  labs(title = "Anzahl eingereichte Interpellationen", 
     caption = "Sept. 2011 bis Nov. 2021",
     x = "Height (cm)", y = "Weight (kg)")

# generate Wordtable 
wordtable <- makeWordlist(corpusClean)

# Frequency analysis
head(sort(wordtable$wordtable, decreasing = TRUE), 100)

# Preprocessing: eliminate rare occuring words
words5 <- wordtable$words[wordtable$wordtable > 5]

# LDA prepare and generate
pagesLDA <- LDAprep(text = corpusClean$text, vocab = words5)

dir.create("LDA") # creates folder lda

result1 <- LDAgen(documents = pagesLDA, K = 5L, vocab = words5, seed = 123, folder="./LDA/LDA5") # K 5, alpha=eta qua default 1/k
result2 <- LDAgen(documents = pagesLDA, K = 10L, vocab = words5, seed = 123, folder="./LDA/LDA10")
result3 <- LDAgen(documents = pagesLDA, K = 15L, vocab = words5, seed = 123, folder="./LDA/LDA15")
result4 <- LDAgen(documents = pagesLDA, K = 20L, vocab = words5, seed = 123, folder="./LDA/LDA20")

# load lda result
load("./LDA/LDA10-k10alpha0.1eta0.1i200b70s123.RData")

lda::top.topic.words(result$topics, by.score = TRUE, num.words = 10) # show top words

### ???

topicsInText(text = pagesLDA, ldaresult = result, ldaID = ldaID,
             id = "20204274", vocab = words5, wordOrder = "originaltext", originaltext= textmeta$text) # Topics-Assignments exempl. in einem Text


# create folder for top texts
dir.create("./LDA/K10")

# store top texts
topID <- topTexts(ldaresult = result, ldaID = ldaID, limit = 10)

topArt <- showTexts(textmeta, id = topID, file="./LDA/K10/")

# dendrogram
clustRes <- clusterTopics(ldaresult = result, xlab = "Topic", ylab = "Distance")

# topic frequency as time series
plotTopic <- plotTopic(object = textmeta, ldaresult = result, ldaID = ldaID, rel = TRUE,
                       curves = "smooth", smooth = 0.1, legend = "none", unit="year")
write.csv(plotTopic, "./LDA/LDA10_plotTopic.csv")

plotTopic2 <- read.csv(file = "./LDA/LDA10_plotTopic.csv")
plotTopic2 <- plotTopic2[,-1]
plotTopic2 <- plotTopic(object = textmeta, ldaresult = result, ldaID = ldaID, rel = TRUE,
                        curves = "smooth", smooth = 0.1, legend, unit="year")


# Heatmap - other viz of topic freq (deviations of topic mean)
plotHeat(object = textmeta, ldaresult = result, ldaID = ldaID, unit = "month")

# IntruderWord test - validation - here the "intruderwords", i.e. the words that do not belong, must be recognized.
# see: https://www.researchgate.net/publication/221618226_Reading_Tea_Leaves_How_Humans_Interpret_Topic_Models

intWords <- intruderWords(beta = result$topics, numIntruder = 1) # with 1 Intruder


names(result2)
head(result2$topic_sums)

topic_sums <- result2$topic_sums
document_sums <- result2$document_sums
