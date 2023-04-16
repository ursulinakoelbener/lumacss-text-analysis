# RSelenium laden
library("RSelenium")
library("tidyverse")
driver <- rsDriver(browser=c("firefox"), port=6022L)

# Remote Driver als Client definieren
remote_driver <- driver[["client"]]

remote_driver$navigate("https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista?k=(PdAffairTypeId:8+OR+PdAffairTypeId:9)") # Öffnet Seite im Remote Browser

pages <- 728 # Anzahl Seiten, die gescraped werden müssen

# For-Loop Links scrapen
metaliste <- list()

for(e in 1:pages){

links_el <- remote_driver$findElements(using = 'xpath', value = "//a[@class='create-clickable-area']")

links <- list() # Leere Liste für Links erstellen

# Links der aktuellen Seite in die Liste schreiben
for (i in 1:length(links_el)) {
  tryCatch({
    links[i] <- links_el[[i]]$getElementAttribute("href")
  }, 
  error=function(e){})
}

links <- unlist(links)

links_export <- data.frame(links) # Links in Dataframe umwandeln

write.csv2(links_export, file = paste0('links',e,'.csv')) # Links als csv speichern

# Vektor in der Metaliste abspeichern
metaliste[e] <- links

# Wechsel auf die nächste Seite - Neue URL generieren und dorthin navigieren
new_url <- paste("https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista?k=(PdAffairTypeId:8+OR+PdAffairTypeId:9)#k=(PdAffairTypeId%3A8%20OR%20PdAffairTypeId%3A9)#s=", 
                 as.character(e), "1", sep = "")
remote_driver$navigate(new_url)

Sys.sleep(2)
}