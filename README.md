# Actigraph Data Analysis

Auteur du projet: Germain Faity - ENS Rennes 2024 (germain.faity@ens-rennes.fr).

## Objectif

L'objectif principal de ce projet est d'analyser des données actimétriques issues d'un accéléromètre Actigraph porté à la hanche dans deux conditions:
    - une première fois durant plusieurs jours (environ une semaine) pendant la vie quotidienne (TP1).
    - une seconde fois pendant plusieurs activités standardisées (marche, course, vélo...) avec les dates de début et de fin consignées pour chaque activités (TP2).

Une fois ces données récoltées et analysées, le but est d'en extraire des caractéristiques (features) pour chaque activités afin de pouvoir entraîner un modèle de machine learning sur les données standardisées dans le but de faire de la reconnaissance d'activités sur les données de la vie quotidienne.

## Organisation

- "actigraphDataAnalysis": Repertoire principal du projet.
    - "actigraphDataAnalysis/gt3x2csv.Rmd"
        Fichier de code R permettant de convertir des données actigraph du format gt3x au format csv. Ce programme n'est finalement pas utilisé.
    - "actigraphDataAnalysis/main_TP2_std.Rmd"
        Fichier de code R permettant de lire et analyser les données actigraph du TP2 (activités standardisées) ainsi que le fichier de labellisation (heures de début et fin de chaque activité) afin d'extraire des caractéristiques du signal accélérométrique intéressantes par activité (et par fenêtre temporelles, par exemple 5s). Des étapes supplémentaires permettent de sélectionner les variables les plus pertinentes (pour éviter redondance ou les variables peu discriminantes par exemple).
    - "actigraphDataAnalysis/activityRecognition_createModels.Qmd"
        Fichier de code python permettant de lire les caractéristiques sélectionnées du signal accélérométrique des activités standardisées et d'entraîner (et tester) un modèle de machine learning capable de reconnaitre les activités présentes.
    - "actigraphDataAnalysis/activityRecognition_createModels_13features.html"
        Fichier markdown présentant les résultats de l'analyse "activityRecognition_createModels.Qmd" lorsque les modèles de machine learning sont entraînés avec 13 features précedemment sélectionnées. Grande précision des modèles dans la reconnaissance d'activité.
    - "actigraphDataAnalysis/activityRecognition_createModels_4features.html"
        Fichier markdown présentant les résultats de l'analyse "activityRecognition_createModels.Qmd" lorsque les modèles de machine learning sont entraînés avec seulement les 4 features les plus pertinentes. On simplifie les modèles mais on perd sur la précision des modèles dans la reconnaissance d'activité.
    -  "actigraphDataAnalysis/extractFeaturesBySegment_TP1.Rmd"
        Fichier de code R permettant de lire et analyser les données actigraph du TP1 (vie quotidienne) afin d'extraire des caractéristiques du signal accélérométrique par fenêtre temporelles, par exemple 5s. Etant donnée la grande taille des fichiers accelerométrique de la vie quotidienne, ce code est optimisé pour minimiser le temps d'analyse.
    - "actigraphDataAnalysis/activityRecognition_applyModel.Qmd"
        Fichier de code python permettant de lire les caractéristiques du signal accélérométrique de la vie quotidienne et d'y appliquer le un modèle de machine learning précédemment entraîné, afin de reconnaître certaines activités (marche, course, vélo...).
    - "actigraphDataAnalysis/activityRecognition_applyModel_appliedToStdActivities_from5sTo5s.html"
        Résultats de l'analyse "activityRecognition_applyModel.Qmd" lorsque le modèle de machine learning entraîné sur des fenêtres de 5s est appliqué sur le fichier accélérométrique des activités standardisé (fenêtres temporelles de 5s). Il s'agit d'un fichier markdown contenant une figure générale par fichier analysé permettant de se faire une idée du succès (ou non) de la reconnaissance d'activité. Les résultats montrent un certain succès de l'application du modèle de machine learning pour reconnaitre les activités.
    - "actigraphDataAnalysis/activityRecognition_applyModel_appliedToAll7DaysData_from5sTo5s.html"
        Résultats de l'analyse "activityRecognition_applyModel.Qmd" lorsque le modèle de machine learning entraîné sur des fenêtres de 5s est appliqué sur le fichier accélérométrique de la vie quotidienne (fenêtres temporelles de 5s). Il s'agit d'un fichier markdown contenant une figure générale par fichier analysé permettant de se faire une idée du succès (ou non) de la reconnaissance d'activité. Les résultats sont relativement confus et montrent un mélange entre marche / montée d'escalier / vélo. Seule l'activité course à pied semble bien reconnue.

- "actigraphDataAnalysis/data"
    Répertoire contenant les données accélérométriques.
    - "actigraphDataAnalysis/data/TP1_freeliving"
        Répertoire contenant les données actigraph (.gt3x) des enregistrements de la vie quotidienne pour l'ensemble des 17 sujets.
        - "actigraphDataAnalysis/data/TP1_freeliving/TMP"
            Répertoire qui vise à accueillir des données temporaires permettant d'optimiser la vitesse d'execution du programme "extractFeaturesBySegment_TP1.Rmd".
    - "actigraphDataAnalysis/data/TP2_APstandard"
        Répertoire contenant les données actigraph (.gt3x) des enregistrements des activités standardisées pour l'ensemble des 17 sujets.
    - "actigraphDataAnalysis/data/2A_2024_hoursActivity.csv"
        Fichier contenant les informations de labellisation (heures de début et de fin de chaque activité standardisée par participant) pour les enregistrements des activités standardisées.
- "actigraphDataAnalysis/FCT"
    Répertoire contenant les fichiers de code contenant des fonctions utiles aux programmes principaux.
    - "actigraphDataAnalysis/FCT/R"
    Répertoire contenant les fichiers de code R contenant des fonctions utiles aux programmes principaux R.
    **A noter: La fonction "extract_features.R" contient le code permettant d'extraire les caractéristiques du signal accélérométrique précédemment nettoyé pour chaque fenêtre temporelle.**

- "actigraphDataAnalysis/RES"
    Répertoire contenant les fichiers de résultats (caractéristiques du signal accélérométrique extraites par fenêtres temporelles d'analyses).
    - "actigraphDataAnalysis/RES/TP1_freeliving"
        Répertoire contenant les fichiers de résultats pour les enregistrements de la vie quotidienne.
        - "actigraphDataAnalysis/RES/TP1_freeliving/windowSize_5s"
            **Répertoire contenant les fichiers de résultats (caractéristiques du signal accélérométrique extraites par fenêtres temporelles d'analyses) pour des fenêtres de 5 secondes (1 fichier par participant).**
            - "actigraphDataAnalysis/RES/TP1_freeliving/windowSize_5s/activityPredictions"
                Répertoire contenant les fichiers de résultats de la prédiction d'activité par l'algorithme de machine learning pour des fenêtres de 5 secondes (1 fichier par participant). Il s'agit des mêmes fichiers que dans le répertoire parent mais avec 1 colonne supplémentaire (dernière colonne): l'activité prédite.      
    - "actigraphDataAnalysis/RES/TP2_APstandard"
        Répertoire contenant les fichiers de résultats pour les enregistrements des activités standardisées.
        - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s"
            Répertoire contenant les fichiers de résultats (caractéristiques du signal accélérométrique extraites par fenêtres temporelles d'analyses) pour des fenêtres de 5 secondes (1 fichier au total pour l'ensemble des participants, par type d'analyse), mais également les modèles de machine learning entraînés.
            - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/ClassifierIA"
                Répertoire contenant des fichiers .pkl contenant les modèles de machine learning entraînés avec 13 features, et également les outils pour normaliser les données (scaler.pkl) et les encoder (label_encoder.pkl).
            - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/ClassifierIA_4features"
                Même chose que le dossier "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/ClassifierIA" mais contenant les modèles de machine learning entraînés avec seulement 4 features.
            - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/standardised_activity_features.csv"
                **Fichier de résultats contenant les caractéristiques du signal accélérométrique extraites par fenêtres temporelles d'analyses pour des fenêtres de 5 secondes pour les enregistrements des activités standardisées (13 features différentes).** Meilleure précision.
            - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/standardised_activity_features_onlySelectedFeatures.csv"
                **Même fichier que "standardised_activity_features.csv" mais avec seulement une selection encore plus faible de features (seulement 4 features différentes).** Précision de la reconnaissance d'activités inférieure.
            - "actigraphDataAnalysis/RES/TP2_APstandard/windowSize_5s/testActivityPredictionsFromAPstandard"
                Répertoire contenant la même chose que "actigraphDataAnalysis/RES/TP1_freeliving/windowSize_5s" mais appliqué aux enregistrements effectués pendant les activités standardisés. Servi pour tester les modèles de machine learning.