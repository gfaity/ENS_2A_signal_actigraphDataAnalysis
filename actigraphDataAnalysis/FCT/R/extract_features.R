# Définir la fonction d'extraction des caractéristiques
extract_features <- function(window_data, sampling_rate) {

    # Extraire le signal ENMO
    enmo <- window_data$ENMO
    
    # Vérifier s'il y a des données manquantes
    if (all(is.na(enmo))) {
        return(NULL)
    }
    
    ### **Caractéristiques Temporelles**

    # Initialiser un vecteur de caractéristiques
    features <- list(
        mean_ENMO = mean(enmo, na.rm = TRUE),
        sd_ENMO = sd(enmo, na.rm = TRUE),
        median_ENMO = median(enmo, na.rm = TRUE),
        mad_ENMO = mad(enmo, na.rm = TRUE),
        min_ENMO = min(enmo, na.rm = TRUE),
        max_ENMO = max(enmo, na.rm = TRUE),
        range_ENMO = max(enmo, na.rm = TRUE) - min(enmo, na.rm = TRUE)
    )
    #features$iqr_ENMO <- IQR(enmo, na.rm = TRUE)  # Intervalle interquartile
    #features$skewness_ENMO <- e1071::skewness(enmo, na.rm = TRUE)  # Asymétrie
    #features$kurtosis_ENMO <- e1071::kurtosis(enmo, na.rm = TRUE)  # Aplatissement
    
    # **Caractéristiques fréquentielles**
    # Centrer le signal pour supprimer la composante continue (DC)
    enmo_centered <- enmo - mean(enmo, na.rm = TRUE)
    
    # Appliquer la Transformée de Fourier Rapide (FFT)
    fft_enmo <- abs(fft(enmo_centered))
    n <- length(fft_enmo)
    freq <- seq(0, sampling_rate/2, length.out = n/2)
    fft_enmo <- fft_enmo[1:(n/2)]

    # Fréquence dominante
    dom_freq_idx <- which.max(fft_enmo)
    features$dom_freq <- freq[dom_freq_idx]

    # Calcul power spectral density
    psd <- fft_enmo^2
    psd_norm <- psd / sum(psd)

    # Entropie spectrale
    spectral_entropy <- -sum(psd_norm * log(psd_norm + 1e-12))  # Petite constante pour éviter log(0)
    features$spectral_entropy <- spectral_entropy

    # Energie dans différentes bandes
    # 0–0.5 Hz : Mouvements très lents (changements de posture, oscillations de faible intensité).
    # 0.5–3 Hz : Activités régulières comme la marche ou la montée d'escaliers.
    # 3–10 Hz : Mouvements plus rapides, comme la course ou des mouvements segmentaires intenses.
    # 10–30 Hz : Bruit mécanique ou vibrations des équipements (à exclure).
    band_limits <- c(0, 0.5, 3, 10)
    energy_bands <- numeric(length(band_limits) - 1)
    for (i in seq_along(energy_bands)) {
        energy_bands[i] <- sum(psd[freq > band_limits[i] & freq <= band_limits[i + 1]]) / sum(psd)
    }
    # Enregistrer l'énergie dans chaque bande
    features$energy_band_1 <- energy_bands[1]
    features$energy_band_2 <- energy_bands[2]
    features$energy_band_3 <- energy_bands[3]
    
    ### **Caractéristiques Non Linéaires**
    # Exposant de Lyapunov
    #features$lyapunov_exponent <- lyapunov_exp(enmo)
    
    # **Autres caractéristiques**
    # Taux de passage par zéro
    zero_crossings <- sum(diff(enmo_centered > 0) != 0)
    features$zero_crossing_rate <- zero_crossings / length(enmo_centered)
    
    # Autocorrélation à lag 1 (détecte des motifs répétitifs)
    #features$autocorr_ENMO <- acf(enmo, plot = FALSE)$acf[2]
    
    # Convertir la liste de caractéristiques en data frame
    features_df <- as.data.frame(features)
    
    return(features_df)
}