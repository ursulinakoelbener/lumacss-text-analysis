# Bibliotheken laden
library("RSelenium")
library("dplyr")

# Dataframe mit allen Links (Interpellationen)
affairs <- read.csv2("affairs_all.csv")

# RSelenium laden

driver <- rsDriver(browser=c("firefox"), port=6030L)

# Remot Driver als Client definieren
remote_driver <- driver[["client"]]

# For-Loop

for (a in 1:7280){
  tryCatch({

# Geschäft aufrufen
remote_driver$navigate(affairs[a,2])
    Sys.sleep(2)

#------------------------------------------------------------------------------
# Teaser/Titel
#------------------------------------------------------------------------------

# <h2 class="teaser-text business-title ng-binding ng-scope"###</h2>
teaser_el <- remote_driver$findElements(using = 'xpath', value = "//h2[@class='teaser-text business-title ng-binding ng-scope']")

teaser <- list()

for (i in 1:length(teaser_el)) {
  tryCatch({
    teaser[i] <- teaser_el[[i]]$getElementText()
  }, 
  error=function(e){})
}

teaser <- paste(unlist(teaser), collapse = "")

affairs[a,3] <- teaser

#------------------------------------------------------------------------------
# Eingereicht von
#------------------------------------------------------------------------------

# <a class="person-name gray-up-bold meta-value ng-binding" ###</a>
eingereicht_el <- remote_driver$findElements(using = 'xpath', value = "//a[@class='person-name gray-up-bold meta-value ng-binding']")

eingereicht <- list()

for (i in 1:length(eingereicht_el)) {
  tryCatch({
    eingereicht[i] <- eingereicht_el[[i]]$getElementText()
  }, 
  error=function(e){})
}

eingereicht <- paste(unlist(eingereicht), collapse = "")

affairs[a,4] <- eingereicht

#------------------------------------------------------------------------------
# Eingereicht Details (Datum, Rat, Stand der Beratung)
#------------------------------------------------------------------------------

# <div class="col-sm-8 meta-value ng-binding">###</div>
eingereicht_details_el <- remote_driver$findElements(using = 'xpath', value = "//div[@class='col-sm-8 meta-value ng-binding']")

eingereicht_details <- list()

for (i in 1:length(eingereicht_details_el)) {
  tryCatch({
    eingereicht_details[i] <- eingereicht_details_el[[i]]$getElementText()
  }, 
  error=function(e){})
}

eingereicht_am <- paste(unlist(eingereicht_details[[1]]), collapse = "")
eingereicht_im <- paste(unlist(eingereicht_details[[2]]), collapse = "")
status <- paste(unlist(eingereicht_details[3]), collapse = "")

affairs[a, 5] <- eingereicht_am
affairs[a, 6] <- eingereicht_im
affairs[a, 7] <- status

#------------------------------------------------------------------------------
# Eingereichter Text
#------------------------------------------------------------------------------

# <p class="ng-scope">###</p>
text_el <- remote_driver$findElements(using = 'xpath', value = "//p[@class='ng-scope']")

# Leere Liste für Texte erstellen
text <- list()

for (i in 1:length(text_el)) {
  tryCatch({
    text[i] <- text_el[[i]]$getElementText()
  }, 
  error=function(e){})
}

# Umwandlung in eine Liste von Texten
text <- paste(unlist(text), collapse = "")

affairs[a, 8] <- text

#------------------------------------------------------------------------------
# Partei & Fraktion
#------------------------------------------------------------------------------

# <p class="ng-binding ng-scope">###</p>
party_el <- remote_driver$findElements(using = 'xpath', value = "//p[@class='ng-binding ng-scope']")

# Leere Liste für Texte erstellen
party <- list()

for (i in 1:length(party_el)) {
  tryCatch({
    party[i] <- party_el[[i]]$getElementText()
  }, 
  error=function(e){})
}


fraktion <- paste(unlist(party[[2]]), collapse = "")
partei <- paste(unlist(party[[3]]), collapse = "")

affairs[a, 9] <- partei
affairs[a, 10] <- fraktion

#------------------------------------------------------------------------------
# Ende For-Loop
#------------------------------------------------------------------------------
  }, 
error=function(e){})
}

names(affairs) <- c("Nr.", "Link", "Titel", "Eingereicht von", "Einreichungsdatum",
                    "Eingereicht im", "Stand der Beratungen", "Eingereichter Text", "Partei", "Fraktion")

write.csv2(affairs, file = 'affairs_all_full.csv')
