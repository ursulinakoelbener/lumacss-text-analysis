# Get the files names
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

# Dataframe aller Geschäfte als csv abspeichern
write.csv2(myfiles, file = 'affairs_all.csv')

# Manuelle Bereinigung des csv nötig -> alle Zeilennummern entfernen.