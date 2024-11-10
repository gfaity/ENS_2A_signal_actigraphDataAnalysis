# Nettoyer le fichier de label

cleanDFlabel <- function(DFlabel){
    if (length(which(!is.na(DFlabel$notes)))>0){
        #do not run this if all NAs
        DFlabel <- subset(DFlabel, notes != "missing labels")
    }
    # Convertir la date et l'heure en un format de type POSIXct
    DFlabel$start <- as.POSIXct(
    paste(DFlabel$date, DFlabel$start), 
    format = "%d/%m/%Y %H:%M:%S", 
    tz = "GMT"
    )
    DFlabel$end <- as.POSIXct(
    paste(DFlabel$date, DFlabel$end), 
    format = "%d/%m/%Y %H:%M:%S", 
    tz = "GMT"
    )
    # supprimer la date devenue inutile
    DFlabel$date = NULL
	
	return(DFlabel)
}