# Segment Data into Windows
segmentDataIntoWindows <- function(activity_data, sampling_rate, window_size_sec, overlap_fraction){
    # Calculate window parameters
    window_size <- window_size_sec * sampling_rate
    step_size <- window_size * (1 - overlap_fraction)

    # Create Sliding Windows
    
    # Calculate the number of windows
    num_samples <- nrow(activity_data)
    num_windows <- num_samples%/%window_size
    # Nombre de lignes restantes (last window)
    remainder <- num_samples %% num_windows

    # Créer les groupes de base avec 'gl()'
    activity_data$window <- gl(n = num_windows, k = window_size, length = num_samples)

    # Remove data without window IDs (edges) [do not keep because we always need same window size]
    if (remainder > 0) {
        activity_data$window[(num_samples - remainder + 1):num_samples] <- NA
    }
    activity_data <- activity_data[!is.na(window)]

    # Pour verifier son travail
    #activity_data %>% group_by(window) %>% summarize(n())
    
    return(activity_data)
}

