library(tosca)

## LDA ##

# Vorbereitung der Betrachtung einzelner Subcorpora und ihrer Topic-Anteile - s.u.

# Subcorpora pro Partei
corp_sp <- affairs$X[affairs$party == "SP"]
corp_fdp <- affairs$X[affairs$party == "FDP"]
corp_svp <- affairs$X[affairs$party == "SVP"]
corp_gps <- affairs$X[affairs$party == "GPS"]
corp_bdp <- affairs$X[affairs$party == "BDP"]
corp_cvp <- affairs$X[affairs$party == "CVP"]
corp_glp <- affairs$X[affairs$party == "GLP"]
corp_pda <- affairs$X[affairs$party == "PdA"]
corp_ldt <- affairs$X[affairs$party == "LdT"]
corp_evp <- affairs$X[affairs$party == "EVP"]
corp_csp <- affairs$X[affairs$party == "CSP"]
corp_edu <- affairs$X[affairs$party == "EDU"]
corp_ldp <- affairs$X[affairs$party == "LDP"]
corp_mcg <- affairs$X[affairs$party == "MCG"]
corp_mcr <- affairs$X[affairs$party == "MCR"]
corp_basta <- affairs$X[affairs$party == "BastA"]
corp_eag <- affairs$X[affairs$party == "EAG"]
corp_parteilos <- affairs$X[affairs$party == "parteilos"]

# Für alle Parteien - Liste mit IDs aller Parteien einzeln 
partei <- names(table(affairs$party))
ids_partei <- list()
for (i in 1:length(partei)) 
  ids_partei[[i]] <- affairs$X[affairs$party == partei[i]]


###############################################
# Define columns to be used from the CSV 
cols = c("Nr.", "Eingereichter.Text", "Einreichungsdatum", "Titel")

# generate textmeta object from the csv
textmeta <- readTextmeta(affairs, cols = cols, dateFormat =
                           "%d.%m.%Y",idCol = "X", dateCol = "Einreichungsdatum", textCol =
                           "Eingereichter.Text", titleCol= "Titel")


# Preprocessing

corpusClean <- cleanTexts(object = textmeta, sw = c(tm::stopwords("en"),"said", "will", "us", "new", "also"), checkUTF8=F)



# Summary of Corpus

summary(corpusClean)

# plotting the corpus as time-series, other possible: month, weeks, bi-month, days

plotScot <- plotScot(corpusClean, curves = "both", unit="days")

# store timeseries as csv 

write.csv(plotScot, file = "plotScot_I.csv")

# generate Wordtable 

wordtable <- makeWordlist(corpusClean)

# Frequency analysis

head(sort(wordtable$wordtable, decreasing = TRUE), 100)

# Preprocessing: eliminate rare occuring words

words5 <- wordtable$words[wordtable$wordtable > 5]

# LDA prepare and generate

pagesLDA <- LDAprep(text = corpusClean$text, vocab = words5)

dir.create("LDA") # creates folder lda

result <- LDAgen(documents = pagesLDA, K = 5L, vocab = words5, seed = 123, folder="./LDA/LDA5") # K 5, alpha=eta qua default 1/k
result <- LDAgen(documents = pagesLDA, K = 10L, vocab = words5, seed = 123, folder="./LDA/LDA10")
result <- LDAgen(documents = pagesLDA, K = 15L, vocab = words5, seed = 123, folder="./LDA/LDA15")
result <- LDAgen(documents = pagesLDA, K = 20L, vocab = words5, seed = 123, folder="./LDA/LDA20")

# load lda result

load("./LDA/LDA10-k10alpha0.1eta0.1i200b70s123.RData")

# Anteile des Guardian an den 10 Topics
rowSums(result$document_sums[,match(id_Guardian, names(textmeta$text))])/rowSums(result$document_sums)

# Anteile aller Parteien an den 10 Topics
topic_shares_partei <- list()
for (i in 1:length(ids_partei)) topic_shares_partei[[i]] <- rowSums(result$document_sums[,match(ids_partei[[i]], names(textmeta$text))])/rowSums(result$document_sums)

topic_shares_partei_df <- as.data.frame(matrix(unlist(topic_shares_partei), 10))
colnames(topic_shares_partei_df) <- partei
topic_shares_partei_df <- t(topic_shares_partei_df)
colnames(topic_shares_partei_df) <- paste("topic", 1:10, sep="")

topic_shares_partei_df



