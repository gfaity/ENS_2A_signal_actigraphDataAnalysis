# Clean data and filtering
cleanFilterAccelero <- function(thisDF, cut_freq){
    # Clean data and compute accelerometer norm

    # Filtrer les colonnes X, Y, Z avec le filtre Butterworth
    thisDF[, `:=`(
        X = bwfilter(X, f = sampling_rate, n = 2, to = cut_freq, output = "Sample"),
        Y = bwfilter(Y, f = sampling_rate, n = 2, to = cut_freq, output = "Sample"),
        Z = bwfilter(Z, f = sampling_rate, n = 2, to = cut_freq, output = "Sample")
    )]

    return(thisDF)
}