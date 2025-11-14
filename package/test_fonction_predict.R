# =================================================================
# SCRIPT DE TEST : Workflow et fonctionnalité $predict
# =================================================================
getwd()
# 1. Chargement des dépendances et des fichiers du package

# Installez R6 si ce n'est pas déjà fait : install.packages("R6")
library(R6)
cat("--- Étape 1 : Chargement des fichiers du package ---\n")

# L'ordre de chargement est important : Parent -> Classes Enfants -> Wrapper -> User Functions
tryCatch({
    source("ClusterAnalysis_parentclass.R")
    source("kmeans_class.R")
    source("CAH_class.R") 
    source("Kprototypes_class.R")
    source("ClustOfVar_class.R")
    source("wrapper.R")
    source("user_functions.R")
    cat("Chargement de tous les fichiers R réussi.\n\n")
}, error = function(e) {
    cat("ERREUR DE CHARGEMENT : Assurez-vous que tous les fichiers sont dans le répertoire de travail.\n")
    stop(e)
})

# 2. Préparation des données de test

set.seed(42)

# Données Quantitatives (pour Kmeans et CAH_Kmeans)
data_num <- as.data.frame(iris[1:4])
data_new_num <- data_num[sample(nrow(data_num), 5), ] # 5 nouvelles observations pour la prédiction
data_fit_num <- data_num[-sample(nrow(data_num), 5), ] # Données d'entraînement

# Données Mixtes (pour Kprototypes)
data_mixte <- iris
data_mixte$Species <- as.factor(data_mixte$Species)
# Ajout d'une variable catégorielle synthétique pour tester Kprototypes
data_mixte$Taille_Sepale <- cut(data_mixte$Sepal.Length, breaks = 3, labels = c("Petit", "Moyen", "Grand"))
data_mixte$Sepal.Length <- NULL # Retire la version numérique
data_fit_mixte <- data_mixte[1:140, ]
data_new_mixte <- data_mixte[141:150, ]

cat("--- Étape 2 : Données de test préparées ---\n")
cat("Set numérique :", nrow(data_fit_num), "obs.\n")
cat("Set mixte :", nrow(data_fit_mixte), "obs.\n\n")


# =================================================================
# 3. Test de la fonctionnalité $predict et du Workflow
# =================================================================

# -----------------------------------------------------------------
# TEST 3.1 : Workflow direct (Classe Enfant -> $fit -> $predict) - Kmeans
# -----------------------------------------------------------------
cat("### TEST 3.1 : Workflow Kmeans (Numérique) et $predict ###\n")

# 1. Instanciation de la classe enfant (hérite de ClusterAnalysis)
km_model <- Kmeans$new(k = 3, cr = TRUE, na_action = "omit")

# 2. Ajustement du modèle ($fit gère le nettoyage/standardisation et appelle kmeans())
cat("Ajustement du modèle Kmeans...\n")
km_model$fit(data_fit_num)

# 3. Prédiction sur de nouvelles données (test de $predict)
cat("Prédiction sur", nrow(data_new_num), "nouvelles observations...\n")
predictions_km <- km_model$predict(data_new_num)

cat("Prédictions obtenues (Kmeans) :\n")
print(predictions_km)
stopifnot(length(predictions_km) == nrow(data_new_num))
cat("-> VÉRIFICATION OK : Taille des prédictions correcte.\n")
cat("----------------------------------------------------\n\n")


# -----------------------------------------------------------------
# TEST 3.2 : Workflow direct (Classe Enfant) - Kprototypes
# -----------------------------------------------------------------
cat("### TEST 3.2 : Workflow Kprototypes (Mixte) et $predict ###\n")

# Kprototypes est un test crucial car il gère l'hétérogénéité des données.
kp_model <- Kprototypes$new(k = 3, cr = TRUE)

# Ajustement
cat("Ajustement du modèle Kprototypes...\n")
kp_model$fit(data_fit_mixte)

# Prédiction
cat("Prédiction sur", nrow(data_new_mixte), "nouvelles observations mixtes...\n")
predictions_kp <- kp_model$predict(data_new_mixte)

cat("Prédictions obtenues (Kprototypes) :\n")
print(predictions_kp)
stopifnot(length(predictions_kp) == nrow(data_new_mixte))
cat("-> VÉRIFICATION OK : Taille des prédictions correcte.\n")
cat("----------------------------------------------------\n\n")


# -----------------------------------------------------------------
# TEST 3.3 : Workflow via Factory (wrapper.R) et User Functions
# -----------------------------------------------------------------
cat("### TEST 3.3 : Workflow Complet via faire_clustering (CAH+Kmeans) ###\n")

# 1. Utilisation de la fonction utilisateur (utilise la Factory en interne)
cat("Appel de faire_clustering(method = 'cah_kmeans')...\n")
cah_model <- faire_clustering(data = data_fit_num, 
                              k = 4, 
                              method = "cah_kmeans", 
                              cr = TRUE)

# 2. Vérification du workflow : Est-ce que la Factory a retourné le bon objet ?
if (inherits(cah_model, "CAH_Kmeans")) {
    cat("-> VÉRIFICATION WORKFLOW OK : faire_clustering -> Factory -> CAH_Kmeans.\n")
} else {
    stop("ERREUR WORKFLOW : La Factory n'a pas retourné l'objet CAH_Kmeans attendu.")
}

# 3. Test de $predict sur l'objet retourné par le workflow
cat("Test de $predict sur le modèle CAH_Kmeans obtenu via faire_clustering...\n")
predictions_cah <- cah_model$predict(data_new_num)
cat("Prédictions obtenues (CAH_Kmeans) :\n")
print(predictions_cah)
stopifnot(length(predictions_cah) == nrow(data_new_num))
cat("-> VÉRIFICATION OK : $predict fonctionne sur l'objet Factory/User Function.\n")
cat("----------------------------------------------------\n\n")


# -----------------------------------------------------------------
# TEST 3.4 : Robustesse : $predict avec NA dans les nouvelles données
# -----------------------------------------------------------------
cat("### TEST 3.4 : $predict avec Données Manquantes (NA) ###\n")

data_new_num_na <- data_new_num
# Introduire des NA
data_new_num_na[1, 1] <- NA 
data_new_num_na[3, 4] <- NA

cat("Prédiction sur nouvelles données avec NA (Kmeans)...\n")
# $predict devrait renvoyer NA pour les observations ayant des NA non gérés.
predictions_na <- km_model$predict(data_new_num_na)

cat("Prédictions obtenues (NA) :\n")
print(predictions_na)
cat("Note : Si le $predict ne supporte pas nativement les NA, les valeurs correspondantes\n")
cat("       devraient être NA ou la fonction devrait lancer un avertissement/erreur.\n")
cat("----------------------------------------------------\n\n")

cat("====================================================\n")
cat("SCRIPT DE TEST TERMINÉ. Tous les aspects du workflow et $predict ont été appelés avec succès.\n")
cat("====================================================\n")