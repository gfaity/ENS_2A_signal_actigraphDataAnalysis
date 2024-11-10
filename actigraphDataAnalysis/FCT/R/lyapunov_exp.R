# Fonction pour calculer un exposant de Lyapunov de manière approximative
lyapunov_exp <- function(signal, embedding_dim = 2, time_lag = 1, num_nearest = 10) {

    # embedding_dim (Dimension d'Embedding)
    #   Définition : La dimension d'embedding est le nombre de variables nécessaires pour représenter correctement l'état du système dynamique. Dans l'analyse de séries temporelles, on utilise des dimensions d'embedding pour reconstruire l'espace de phase du signal à partir d'une seule dimension observée.
    #   Pourquoi embedding_dim = 2 ?
    #   Valeur Courante pour l'Exploration : embedding_dim = 2 est souvent utilisé comme un point de départ pour des analyses exploratoires simples, car il permet de visualiser les trajectoires du signal dans un espace bidimensionnel. Cela aide à capturer des structures simples dans le comportement dynamique du signal.
    #   Éventuels Ajustements : Si le signal montre des comportements plus complexes, tu pourrais augmenter embedding_dim (par exemple, à 3 ou plus) pour mieux capturer la dynamique du système. La théorie de l'embedding de Takens suggère que la dimension optimale peut être plus élevée, en particulier pour des systèmes complexes.
    #
    # time_lag (Décalage Temporel)
    #   Définition : Le décalage temporel est le nombre d'échantillons entre les points successifs utilisés pour créer l'espace de phase. Cela permet de capturer la dynamique temporelle du signal de manière plus précise.
    #   Pourquoi time_lag = 1 ?
    #   Simplification Initiale : Un time_lag de 1 signifie que chaque point successif de la série temporelle est utilisé sans saut d'échantillons. C'est une valeur simple qui permet de capturer rapidement la structure temporelle, mais elle peut être sous-optimale pour certains signaux.
    #   Optimisation Possible : Dans une analyse plus approfondie, tu pourrais utiliser des méthodes comme l'analyse de l'autocorrélation ou la fonction d'information mutuelle pour déterminer un time_lag plus approprié. Cela dépend de la période naturelle des oscillations du signal.
    # 
    # num_nearest (Nombre de Plus Proches Voisins)
    #   Définition : Le nombre de plus proches voisins est le nombre de points voisins dans l'espace de phase que l'on utilise pour mesurer la divergence exponentielle. Ces voisins aident à estimer comment les trajectoires du signal divergent avec le temps.
    #   Pourquoi num_nearest = 10 ?
    #   Compromis Entre Stabilité et Précision : Prendre 10 voisins permet d'obtenir une estimation plus stable de la divergence des trajectoires sans être trop influencé par le bruit. Un nombre trop petit (par exemple, 1 ou 2) rendrait l'estimation très sensible au bruit, tandis qu'un nombre trop grand pourrait lisser des détails importants de la dynamique.
    #   Ajustements : Tu pourrais ajuster ce nombre en fonction de la complexité de ton signal ou de la taille de ta série temporelle. Si tu as un signal très bruité, augmenter ce nombre peut améliorer la robustesse de l'estimation.

    # Vérifier que la longueur du signal est suffisante
    if (length(signal) < embedding_dim * time_lag + num_nearest) {
        return(NA)
    }
    
    # Créer un espace de phase en utilisant le signal (reconstruction de phase)
    embed_signal <- embed(signal, embedding_dim)
    n <- nrow(embed_signal)
    
    # Calculer les distances initiales entre les points dans l'espace de phase
    distances <- as.matrix(dist(embed_signal))
    
    # Exclure les distances avec soi-même (mettre les diagonales à Inf)
    diag(distances) <- Inf
    
    # Prendre les plus proches voisins pour chaque point
    nearest_distances <- apply(distances, 1, function(x) sort(x)[1:num_nearest])
    
    # Calculer la moyenne de la distance initiale
    mean_initial_distance <- mean(nearest_distances, na.rm = TRUE)
    
    # Si la distance initiale est trop petite, renvoyer NA pour éviter des problèmes numériques
    if (mean_initial_distance < .Machine$double.eps) {
        return(NA)
    }
    
    # Calculer l'évolution de la distance au cours du temps
    divergence <- log(nearest_distances / mean_initial_distance)
    
    # Calculer la pente moyenne de la divergence (exposant de Lyapunov approximatif)
    lyapunov_exponent <- mean(divergence, na.rm = TRUE)
    
    return(lyapunov_exponent)
}