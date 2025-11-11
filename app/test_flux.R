# =======================================================
# SCRIPT DE TEST DES FLUX ET INTERACTIONS R6
# =======================================================

# --- 0. Chargement des packages et des classes ---
library(R6)
# Assurez-vous que toutes vos classes R6 sont chargées (décommenter si nécessaire)
# source("ClusterAnalysis_parentclass.R")
# source("kmeans_class.R")
# source("CAH_class.R")
# source("Kprototypes_class.R")
# source("ClustOfVar_class.R")
# source("wrapper.R")         # Pour ClusteringFactory
# source("user_functions.R")  # Pour faire_clustering, creer_pipeline

# --- 1. Préparation des Données de Test ---
data_num <- iris[, 1:4] # 4 variables numériques
data_mixte <- iris # 4 num + 1 factor
data_cov <- iris[, 1:4] # Pour clustering de variables

# =======================================================
# I. TEST DE L'HÉRITAGE ET DES ACCESSEURS (ClusterAnalysis)
# =======================================================
cat("\n\n#######################################################\n")
cat("## I. TEST DE L'HÉRITAGE ET DES ACCESSEURS DE BASE\n")
cat("#######################################################\n")

# 1.1. Test de l'héritage et des méthodes de Kmeans
cat("\n--- Test Kmeans (Héritage ClusterAnalysis) ---\n")
km_obj <- Kmeans$new(k = 3, na_action = "omit") #

# Vérification des propriétés de la classe mère
stopifnot(
  "Vérification de l'héritage" = inherits(km_obj, "ClusterAnalysis"),
  "Vérification de k initial" = km_obj$k == 3,
  "Vérification de na_action" = km_obj$na_action == "omit"
)
cat("✅ Propriétés de la classe mère accessibles et correctes après $new().\n")

# Vérification de $fit et des accesseurs de résultats
km_obj$fit(data_num)
stopifnot(
  "Vérification $FFitted" = km_obj$is_fitted,
  "Vérification taille Groupes" = length(km_obj$Groupes) == nrow(data_num), # Taille originale grâce à na_action
  "Vérification $inertie" = !is.null(km_obj$inertie()$intra)
)
cat("✅ Méthode $fit() et accesseurs ($Groupes, $inertie()) fonctionnent.\n")

# 1.2. Test de la gestion des NA (omission) - Vérifie le flux parent/enfant
cat("\n--- Test de gestion des NA (flux parent) ---\n")
data_with_na <- data_num
data_with_na[1, 1] <- NA 
data_with_na[5, 4] <- NA 

km_na_obj <- Kmeans$new(k = 3, na_action = "omit") #
km_na_obj$fit(data_with_na)

# Lignes 1 et 5 doivent être NA dans le résultat final (Groupes)
nb_na_resultat <- sum(is.na(km_na_obj$Groupes))
stopifnot(
  "Vérification des NA omis dans $Groupes" = nb_na_resultat == 2
)
cat("✅ Gestion des NA par 'omit' (flux parent) fonctionne: 2 NA retrouvés dans $Groupes.\n")


# =======================================================
# II. TEST DE LA FACTORY ET DU WRAPPER (wrapper.R)
# =======================================================
cat("\n\n#######################################################\n")
cat("## II. TEST DE LA CLUSTERING FACTORY\n")
cat("#######################################################\n")

factory <- ClusteringFactory$new() #

# 2.1. Test Factory: Création et ajustement de CAH_Kmeans
cat("\n--- Test Factory: create_cah_kmeans ---\n")
cah_km_factory <- factory$create_cah_kmeans(data_num, k = 4, cr = FALSE)
stopifnot(
  "Vérification type objet" = inherits(cah_km_factory, "CAH_Kmeans"),
  "Vérification ajustement" = cah_km_factory$is_fitted,
  "Vérification paramètre cr" = !cah_km_factory$cr
)
cat("✅ Factory $create_cah_kmeans crée, ajuste et respecte les paramètres (cr=FALSE).\n")

# 2.2. Test Factory: Création sans ajustement
cat("\n--- Test Factory: create_kmeans (fit_now=FALSE) ---\n")
km_factory_not_fit <- factory$create_kmeans(data_num, k = 5, fit_now = FALSE)
stopifnot(
  "Vérification type objet" = inherits(km_factory_not_fit, "Kmeans"),
  "Vérification ajustement" = !km_factory_not_fit$is_fitted
)
cat("✅ Factory crée l'objet sans l'ajuster (fit_now=FALSE).\n")

# 2.3. Test Factory: Création de Kprototypes (flux mixte)
cat("\n--- Test Factory: create_kprototypes ---\n")
kproto_factory <- factory$create_kprototypes(data_mixte, k = 3, lambda = 0.7)
stopifnot(
  "Vérification type objet" = inherits(kproto_factory, "Kprototypes"),
  "Vérification ajustement" = kproto_factory$is_fitted,
  "Vérification paramètre lambda" = kproto_factory$lambda == 0.7
)
cat("✅ Factory $create_kprototypes crée, ajuste et respecte les paramètres (lambda=0.7).\n")


# =======================================================
# III. TEST DES FONCTIONS UTILISATEUR (user_functions.R)
# =======================================================
cat("\n\n#######################################################\n")
cat("## III. TEST DES FONCTIONS UTILISATEUR (Interfaces) \n")
cat("#######################################################\n")

# 3.1. Test faire_clustering : Mode 'auto' (devrait choisir Kmeans)
cat("\n--- Test faire_clustering (mode auto, data_num) ---\n")
result_auto_num <- faire_clustering(data_num, k = 3, standardiser = FALSE) #
stopifnot(
  "Vérification mode auto (Num)" = inherits(result_auto_num, "Kmeans"),
  "Vérification ajustement" = result_auto_num$is_fitted,
  "Vérification paramètre standardiser" = !result_auto_num$cr
)
cat("✅ faire_clustering (auto) choisit Kmeans et applique standardiser=FALSE.\n")

# 3.2. Test faire_clustering : Mode 'auto' (devrait choisir Kprototypes)
cat("\n--- Test faire_clustering (mode auto, data_mixte) ---\n")
# Retirer Species pour forcer la détection de variable catégorielle restante
data_mixte_2 <- data_mixte
data_mixte_2$Species <- NULL 
data_mixte_2$Categ <- factor(sample(c("A", "B"), 150, replace = TRUE)) 
result_auto_mixte <- faire_clustering(data_mixte_2, k = 3)

stopifnot(
  "Vérification mode auto (Mixte)" = inherits(result_auto_mixte, "Kprototypes")
)
cat("✅ faire_clustering (auto) choisit Kprototypes pour données mixtes.\n")

# 3.3. Test faire_clustering : Forcer 'clustofvar'
cat("\n--- Test faire_clustering (mode clustofvar) ---\n")
result_cov <- faire_clustering(data_cov, k = 3, method = "clustofvar", max_iter = 5)
stopifnot(
  "Vérification mode 'clustofvar'" = inherits(result_cov, "ClustOfVar"),
  "Vérification paramètre additionnel" = result_cov$max_iter == 5
)
cat("✅ faire_clustering (forcé) crée ClustOfVar et passe le paramètre additionnel.\n")


# 3.4. Test creer_pipeline (fonction usine réutilisable)
cat("\n--- Test creer_pipeline (usine de clustering) ---\n")
pipeline_cah_km <- creer_pipeline("cah_kmeans", k = 3, method = "ward.D")
# Le pipeline doit être une fonction
stopifnot(
  "Vérification type pipeline" = is.function(pipeline_cah_km)
)

# Application à un premier dataset (iris)
resultat_pipe_iris <- pipeline_cah_km(iris[1:4])
stopifnot(
  "Vérification resultat pipe (1)" = inherits(resultat_pipe_iris, "CAH_Kmeans"),
  "Vérification k" = resultat_pipe_iris$k == 3
)

# Application à un second dataset (mtcars)
resultat_pipe_mtcars <- pipeline_cah_km(mtcars[1:5])
stopifnot(
  "Vérification resultat pipe (2)" = inherits(resultat_pipe_mtcars, "CAH_Kmeans"),
  "Vérification ajustement" = resultat_pipe_mtcars$is_fitted
)

cat("✅ creer_pipeline crée une fonction réutilisable pour différents datasets.\n")

cat("\n\n#######################################################\n")
cat("## TOUS LES TESTS DE FLUX ET D'INTERACTIONS SONT PASSÉS AVEC SUCCÈS. \n")
cat("#######################################################\n")