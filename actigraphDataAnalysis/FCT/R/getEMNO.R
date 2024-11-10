# Compute accelerometer norm
getEMNO <- function(thisDF){
    # Get Euclidean Norm Minus One (ENMO)
    thisDF[, ENMO := sqrt(X^2 + Y^2 + Z^2) - 1]

    # Remove X, Y and Z = we don't need it anymore
    thisDF[, c("X", "Y", "Z") := NULL]

    return(thisDF)
}