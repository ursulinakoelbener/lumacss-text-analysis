library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
#-----------------------------------------------------------------------------
# Preprocessing
#-----------------------------------------------------------------------------

affairs <- read.csv2(file = "affairs_all_full.csv")

# leere Zeilen löschen
empty <- c(1, 2, 3, 4, 7, 8, 99, 147, 174, 181, 232, 255, 270, 301, 320, 355, 
           365, 370, 466, 539, 555, 559, 564, 587, 591, 593, 676, 705, 714, 726, 
           727, 757, 778, 789, 810, 897, 961, 1002, 1038, 1091, 1102, 1131, 1140, 
           1165, 1180, 1183, 1204, 1245, 1254, 1271, 1272, 1273, 1359, 1387, 1398, 
           1404, 1492, 1525, 1566, 1653, 1718, 1740, 1770, 1832, 1882, 1900, 1914, 
           2011, 2157, 2241, 2252, 2253, 2307, 2399, 2408, 2410, 2427, 2487, 2489, 
           2568, 2570, 2714, 2748, 2778, 2791, 2822, 2824, 2827, 2837, 2873, 3026, 
           3027, 3028, 3029, 3030, 3031, 3032, 3033, 3039, 3054, 3151, 3173, 3181, 
           3197, 3329, 3363, 3375, 3382, 3428, 3436, 3445, 3468, 3516, 3534, 3604, 
           3624, 3705, 3712, 3714, 3746, 3770, 3832, 3886, 3985, 3990, 3997, 4006, 
           4061, 4081, 4088, 4091, 4112, 4137, 4153, 4179, 4193, 4311, 4334, 4378, 
           4389, 4394, 4442, 4460, 4468, 4509, 4551, 4675, 4756, 4816, 4875, 4878, 
           5001, 5032, 5056, 5099, 5108, 5121, 5207, 5245, 5277, 5304, 5317, 5330, 
           5333, 5420, 5471, 5479, 5494, 5550, 5607, 5610, 5611, 5617, 5711, 5712, 
           5730, 5845, 5864, 5873, 5964, 6004, 6005, 6007, 6026, 6044, 6050, 6063, 
           6089, 6110, 6127, 6142, 6148, 6149, 6228, 6268, 6270, 6283, 6376, 6487, 
           6516, 6550, 6554, 6565, 6583, 6600, 6624, 6626, 6656, 6679, 6687, 6700, 
           6742, 6743, 6791, 6823, 6892, 6941, 6968, 6986, 7007, 7028, 7061, 7069, 
           7094, 7103, 7125, 7164, 7167, 7218, 7235)
affairs <- affairs[-empty,]

affairs$Nr. <- substr(affairs$Link,78,85)
affairs$Jahr <- substr(affairs$Einreichungsdatum, 7, 10)

affairs$party <- ifelse(affairs$Partei == 'FDP.Die Liberalen', "FDP",
                        ifelse(affairs$Partei == 'Schweizerische Volkspartei', "SVP",
                        ifelse(affairs$Partei == 'Alternative-die Grünen Kanton Zug', "GPS",
                        ifelse(affairs$Partei == 'Bürgerlich-Demokratische Partei Schweiz', "BDP",
                        ifelse(affairs$Partei == 'Christlichdemokratische Volkspartei der Schweiz', "CVP",
                        ifelse(affairs$Partei == 'Christlichdemokratische Volkspartei Oberwallis', "CVP",
                        ifelse(affairs$Partei == 'Ensemble à Gauche', "EAG",
                        ifelse(affairs$Partei == 'Evangelische Volkspartei der Schweiz', "EVP",
                        ifelse(affairs$Partei == 'Grüne (Basels starke Alternative)', "BastA",
                        ifelse(affairs$Partei == 'Grüne Partei der Schweiz', "GPS",
                        ifelse(affairs$Partei == 'Grünliberale Partei', "GLP",
                        ifelse(affairs$Partei == 'Lega dei Ticinesi', "LdT",
                        ifelse(affairs$Partei == 'Partei der Arbeit der Schweiz', "PdA",
                        ifelse(affairs$Partei == 'Sozialdemokratische Partei der Schweiz', "SP", 
                        ifelse(affairs$Partei == 'Christlich-soziale Partei Obwalden', 'CSP',
                        ifelse(affairs$Partei == 'Christlichsoziale Volkspartei Oberwallis', 'CSP',
                        ifelse(affairs$Partei == 'Eidgenössisch-Demokratische Union', 'EDU',
                        ifelse(affairs$Partei == 'Liberal-Demokratische Partei', 'LDP',
                        ifelse(affairs$Partei == 'Mouvement Citoyens Genevois', 'MCG',
                        ifelse(affairs$Partei == 'Mouvement Citoyens Romand', 'MCR',
                        ifelse(affairs$Partei == 'parteilos', 'parteilos', "")))))))))))))))))))))


# Corpus aus Dataframe erstellen
corpus <- corpus(affairs, docid_field = "Nr.", text_field = "Eingereichter.Text", meta = list(), unique_docnames = TRUE) 

# Summary
summary(corpus, n = 5)

#-----------------------------------------------------------------------------
# Token and Type / Tokensfrequenz
#-----------------------------------------------------------------------------

ntype_corp <- ntype(corpus) # Zwichenketten als Kategorie 
ntoken_corp <- ntoken(corpus) # Alle durch Leerzeichen getrennte Zeichenketten

docvars(corpus)

plot(x = factor(corpus$party), y = ntoken_corp, xlab = "", ylab = "Tokenzahl", las = 2)

barplot(ntoken_corp, names.arg = c(corpus$party), las = 2)

#-----------------------------------------------------------------------------
# Key Words
#-----------------------------------------------------------------------------

# Tokenize
toks_affairs <- tokens(corpus)

# durchstöbern mit Keywords
kw_vegan <-  kwic(toks_affairs, pattern = "vegan*", window = 10)
kw_gleichstellung <-  kwic(toks_affairs, pattern = "gleichstellung*", window = 10)
kw_deliberativ <-  kwic(toks_affairs, pattern = "deliberat*", window = 10)
kw_klima <-  kwic(toks_affairs, pattern = "klima*", window = 10)
kw_zucker <-  kwic(toks_affairs, pattern = "zucker*", window = 10)
kw_partizipation <-  kwic(toks_affairs, pattern = "partizipat*", window = 10)

# Punkte und Füllwörter entfernen
toks_affairs_reduced <- tokens(corpus,
                       remove_punct = TRUE,
                       remove_numbers = TRUE) %>%
  tokens_remove (c(stopwords("de"), "schweiz","schweizer", "bundesrat", "dass", "bereits", 
                   "sowie", "wurden", "wurde", "", "insbesondere", "jahr", "gemäss", 
                   "mehr", "seit", "jedoch", "sr", "nr", "jahren", "zudem", "müssen", 
                   "dabei", "kantone", "kantonen"))

#-----------------------------------------------------------------------------
# Document Feature Matrix
#-----------------------------------------------------------------------------

# document-feature matrix erstellen
dfm_mat_affairs <- dfm(toks_affairs_reduced) %>% 
  dfm_trim(min_termfreq = 20, verbose = FALSE)

set.seed(100)
textplot_wordcloud(dfm_mat_affairs)


#-----------------------------------------------------------------------------
# Custom function
#-----------------------------------------------------------------------------
get_keyness_per_year <- function(year, n = 30, nouns.only = TRUE, dfm) {
  
  # Install package
  if (!require(lubridate)) install.package("lubridate")
  
  # Feedback
  cat(paste0("processing: '", year, "'\n"))
  
  # Keywords
  res <- quanteda.textstats::textstat_keyness(
    x = dfm,
    target = lubridate::year(quanteda::docvars(dfm)[["Start"]]) == year
  )
  
  # Only nouns
  if (nouns.only) res <- res %>% filter(!feature == tolower(feature)) 
  
  # Top N
  res %>% 
    dplyr::slice(1:n) %>% 
    dplyr::select(feature, chi2) %>% 
    dplyr::mutate(year = year)
  
}

keywords <- purrr::map_dfr(
  c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021), 
  get_keyness_per_year, 
  dfm = dfm_mat_affairs
)

#-----------------------------------------------------------------------------
# ?
#-----------------------------------------------------------------------------

textstat_frequency(dfm_mat_affairs, n = 20)

# Eine Document feature matrix macht bei weniger Text mehr Sinn, also z.B. den Titeln
# Corpus aus Dataframe erstellen
corp_titel <- corpus(affairs, docid_field = "Nr.", text_field = "Titel", meta = list(), unique_docnames = TRUE)

# Punkte und Füllwörter entfernen
toks_titel <- tokens(corp_titel,
                     remove_punct = TRUE,
                     remove_numbers = TRUE) %>%
  tokens_remove (c(stopwords("de"), "schweiz", "schweizer", "bundesrat", "dass", "bereits", 
                   "sowie", "wurden", "wurde", "", "insbesondere", "jahr", "gemäss", 
                   "mehr", "seit", "jedoch", "sr", "nr", "jahren", "zudem", "müssen", 
                   "dabei", "kantone", "kantonen", "warum"))

# document-feature matrix erstellen
dfm_mat_titel <- dfm(toks_titel)

textstat_frequency(dfm_mat_titel, n = 20)

# Topic Model
install.packages("stm")
library(stm)

stm_topic <- convert(dfm_mat_affairs,
                     to = "stm")
