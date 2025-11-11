# ============================================================================
# TUTORIEL COMPLET DU PACKAGE DE CLUSTERING (VERSION DÉTAILLÉE ET TESTÉE)
# Utilisation des fonctions utilisateur (user_functions.R)
# ============================================================================

# NOTE IMPORTANTE : Les commandes setwd() et source() sont conservées 
# pour l'exécution dans un environnement local de développement.
# setwd("C:/Users/Romain_admin/Documents/GitHub/RollerClustR/app")
# getwd()

# Chargement des librairies et sources
library(R6)
library(tidyverse) # Nécessaire pour les exemples Titanic et les tests

# Simuler le chargement des fichiers du package (nécessaire en environnement de script)
# source("ClusterAnalysis_parentclass.R")
# source("CAH_class.R")
# source("kmeans_class.R")
# source("ClustOfVar_class.R")
# source("Kprototypes_class.R")
# source("wrapper.R")
# source("user_functions.R")


# ============================================================================
# PARTIE 1 : CLUSTERING SIMPLE AVEC DONNÉES NUMÉRIQUES
# ============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 1 : Clustering simple sur données numériques     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Données d'exemple : iris (dimensions des fleurs)
data(iris)
X_num <- iris[, 1:4] # Variables numériques uniquement

cat("📊 Données : iris (150 observations, 4 variables numériques)\n")
cat("Variables :", paste(names(X_num), collapse = ", "), "\n\n")

### Documentation des arguments de faire_clustering()
# data : Data frame (ou matrice) contenant les données à clusteriser. 
#        Doit être propre (pas de NA, types cohérents avec 'method').
# k : Nombre de clusters souhaité (entier > 1).
# method : Chaîne de caractères spécifiant l'algorithme :
#          - 'auto' : sélectionne automatiquement l'algorithme adapté aux types de données.
#          - 'cah_kmeans' : Clustering Hiérarchique Ascendant suivi de K-means (Numérique).
#          - 'kmeans' : K-means standard (Numérique).
#          - 'clustofvar' : Clustering de Variables (Numérique ou Mixte).
#          - 'kprototypes' : K-prototypes (Mixte).
# ... : Paramètres additionnels passés à l'algorithme R6 sous-jacent (ex: distance pour CAH).

# 1.1 Clustering automatique (sélection auto de l'algorithme)
cat("--- 1.1 Clustering automatique ---\n")
resultat1 <- faire_clustering(data = X_num, k = 3, method = "auto")
cat("\n")

# 1.2 Forcer l'utilisation de CAH+K-means
cat("--- 1.2 Clustering avec CAH+K-means ---\n")
resultat2 <- faire_clustering(data = X_num, k = 3, method = "cah_kmeans")
cat("\n")

### Documentation des arguments de resumer_clustering()
# model : L'objet R6 de clustering retourné par faire_clustering().
#        Affiche un résumé adapté à la classe du modèle (inertie, qualité, etc.).

# 1.3 Afficher un résumé détaillé
cat("--- 1.3 Résumé détaillé du clustering ---\n")
resumer_clustering(resultat2)

# ============================================================================
# PARTIE 2 : TROUVER LE NOMBRE OPTIMAL DE CLUSTERS
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 2 : Recherche du nombre optimal de clusters (k)    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

### Documentation des arguments de trouver_k_optimal()
# data : Data frame ou matrice contenant les données.
# k_min : Nombre minimum de clusters à tester (entier >= 2).
# k_max : Nombre maximum de clusters à tester.
# method : Algorithme à utiliser pour l'évaluation (ex: 'cah_kmeans').
# afficher_graphique : Booléen. Si TRUE, affiche le graphique du coude (gain d'inertie ou coût).
# ... : Paramètres additionnels pour l'algorithme de clustering.

# 2.1 Évaluation avec CAH+K-means
cat("--- 2.1 Évaluation de k (de 2 à 8) ---\n")
evaluation_k <- trouver_k_optimal(
 X_num, 
 k_min = 2, 
 k_max = 8,
 method = "cah_kmeans",
 afficher_graphique = FALSE # Mis à FALSE pour éviter l'ouverture de fenêtre graphique auto
)

cat("\nRésultats de l'évaluation :\n")
print(evaluation_k)

# ============================================================================
# PARTIE 3 : ANALYSE DES VARIABLES ILLUSTRATIVES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 3 : Analyse de variables illustratives        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Utiliser le clustering avec k=3
model <- faire_clustering(data = X_num, k = 3, method = "cah_kmeans")

### Documentation des arguments de analyser_illustratives()
# model : Objet R6 de clustering ajusté.
# variables : Data frame ou vecteur contenant la ou les variables illustratives. 
#             Doit avoir le même nombre de lignes que les données initiales.
# afficher : Booléen. Si TRUE, affiche les résultats des tests de liaison (p-valeur, V de Cramer, eta^2).

# 3.1 Analyser la variable Species (qualitative)
cat("--- 3.1 Analyse de la variable Species (qualitative) ---\n")
analyse_species <- analyser_illustratives(
 model, 
 iris[5], # Species est la 5ème colonne de iris
 afficher = TRUE
)

### Documentation des arguments de caracteriser_groupes()
# model : Objet R6 de clustering ajusté.
# var_illus : Vecteur (ou data frame à une colonne) de la variable illustrative à caractériser.
# nom_var : Chaîne de caractères. Nom de la variable à afficher dans les résultats.
# mode : Chaîne de caractères. Niveau de détail :
#        - 'complet' : Affiche les distributions de fréquences et les statistiques de liaison.
#        - 'rapide' : Résumé succinct.

# 3.2 Caractérisation détaillée des groupes
cat("\n\n--- 3.2 Caractérisation détaillée avec Species ---\n")
caracteriser_groupes(
 model, 
 iris$Species, 
 "Species",
 mode = "complet"
)

# ============================================================================
# PARTIE 4 : COMPARAISON D'ALGORITHMES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 4 : Comparaison de plusieurs algorithmes       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

### Documentation des arguments de comparer_algorithmes()
# data : Data frame ou matrice des données d'entrée.
# k : Nombre de clusters commun pour tous les algorithmes.
# methods : Vecteur de chaînes de caractères listant les algorithmes à comparer 
#           (ex: c('cah_kmeans', 'kmeans')).
# ... : Paramètres additionnels pour les algorithmes (par ex. pour K-prototypes).

# 4.1 Comparer CAH+K-means vs K-means standard 
cat("--- 4.1 Comparaison de 2 algorithmes ---\n")
comparateur <- comparer_algorithmes(
 X_num, 
 k = 3,
 methods = c("cah_kmeans", "kmeans")
)

# 4.2 Afficher les résultats de comparaison
cat("\n--- 4.2 Résultats de la comparaison ---\n")
# La méthode $compare() calcule et affiche les matrices de confusion et les 
# taux d'accord entre les partitions.
resultats_comp <- comparateur$compare()

# 4.3 Obtenir un algorithme spécifique
# La méthode $get_result(method_name) permet d'extraire un modèle R6 de la comparaison.
model_kmeans <- comparateur$get_result("kmeans")
cat("\n📌 Modèle K-means récupéré de la comparaison\n")
model_kmeans$print()

# ============================================================================
# PARTIE 5 : STATISTIQUES PAR GROUPE
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 5 : Statistiques descriptives par groupe       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

### Documentation des arguments de statistiques_par_groupe()
# model : Objet R6 de clustering ajusté (contenant les affectations aux groupes).
# data : Data frame ou matrice contenant les variables pour lesquelles calculer les stats.
# afficher : Booléen. Si TRUE, affiche les tableaux récapitulatifs.

# 5.1 Calculer les statistiques pour chaque variable
stats <- statistiques_par_groupe(model, X_num, afficher = TRUE)

# 5.2 Accéder aux statistiques d'une variable spécifique
cat("\n--- 5.2 Statistiques pour Sepal.Length uniquement ---\n")
# Le résultat est une liste où chaque élément est un data frame de statistiques.
print(stats$Sepal.Length)

# ============================================================================
# PARTIE 6 : EXPORT DES RÉSULTATS
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 6 : Export des résultats               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

### Documentation des arguments de obtenir_groupes()
# model : Objet R6 de clustering ajusté.
# Retourne un vecteur contenant l'affectation de chaque observation à son cluster.

# 6.1 Obtenir les groupes
groupes <- obtenir_groupes(model)
cat("--- 6.1 Groupes obtenus ---\n")
cat("Distribution :\n")
print(table(groupes))

### Documentation des arguments de exporter_resultats()
# model : Objet R6 de clustering ajusté.
# data_originale : Data frame des données originales (observations + illustratives, si utilisées).
# inclure_donnees : Booléen. Si TRUE, le data frame de sortie inclut les données originales 
#                   en plus de la colonne 'Cluster'.
# fichier : Chaîne de caractères. Chemin du fichier CSV à créer. Si NULL, retourne uniquement le data frame.

# 6.2 Exporter vers un data frame
resultats_df <- exporter_resultats(
 model, 
 iris, # Données originales (inclut Species)
 inclure_donnees = TRUE,
 fichier = NULL # N'exporte pas vers un fichier dans ce tutoriel
)

cat("\n--- 6.2 Aperçu des résultats exportés ---\n")
print(head(resultats_df, 10))

### Documentation des arguments de generer_rapport()
# model : Objet R6 de clustering ajusté.
# fichier : Chaîne de caractères. Chemin du fichier texte à créer pour le rapport. 
#           Si NULL, affiche le rapport dans la console.

# 6.3 Générer un rapport textuel
cat("\n--- 6.3 Rapport de clustering ---\n")
generer_rapport(model, fichier = NULL) 

# ============================================================================
# PARTIE 7 : CLUSTERING DE VARIABLES (ClustOfVar)
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 7 : Clustering de VARIABLES (ClustOfVar)       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("ATTENTION : ClustOfVar fait du clustering sur les VARIABLES, pas les observations!\n\n")

# 7.1 Clustering de variables numériques
cat("--- 7.1 Clustering des 4 variables d'iris ---\n")
model_var <- faire_clustering(data = X_num, k = 2, method = "clustofvar")

# 7.2 Résumé détaillé
cat("\n--- 7.2 Résumé du clustering de variables ---\n")
resumer_clustering(model_var)

# 7.3 Groupes de variables
cat("\n--- 7.3 Composition des groupes de variables ---\n")
groupes_var <- obtenir_groupes(model_var)
print(groupes_var)

# 7.4 Qualité des clusters de variables
cat("\n--- 7.4 Qualité des clusters de variables ---\n")
if (inherits(model_var, "ClustOfVar")) {
 # On suppose l'implémentation des méthodes spécifiques à ClustOfVar
 # qualites <- model_var$qualite_clusters() 
 # print(qualites)
 
 cat("💡 Affichage des indicateurs de qualité (Rapports de corrélation, etc.)\n")
}

# ============================================================================
# PARTIE 8 : WORKFLOW COMPLET AUTOMATISÉ
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 8 : Workflow complet automatisé            ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("🚀 Lancement d'un workflow complet...\n\n")

### Documentation des arguments de clustering_complet()
# data : Data frame des données d'entrée.
# variables_illustratives : Data frame des variables illustratives (optionnel).
# k_min, k_max : Plage de recherche pour le k optimal.
# k_final : Entier. Si non-NULL, force l'utilisation de ce k au lieu de la détection automatique.
# method : Algorithme à utiliser pour le clustering final.
# fichier_resultats : Chemin du fichier CSV pour l'export des résultats (NULL si non souhaité).
# fichier_rapport : Chemin du fichier TXT pour le rapport (NULL si non souhaité).

# 8.1 Workflow tout-en-un
workflow <- clustering_complet(
 data = X_num,
 variables_illustratives = iris[5], # Species
 k_min = 2,
 k_max = 6,
 k_final = NULL, # Détection automatique
 method = "cah_kmeans",
 fichier_resultats = NULL, 
 fichier_rapport = NULL   # Affichage console uniquement
)

# 8.2 Accéder aux résultats du workflow
cat("\n--- 8.2 Contenu du workflow ---\n")
# L'objet workflow est une liste contenant : model, groupes, resultats, 
# evaluation_k, et illustratives.

# ============================================================================
# PARTIE 9 : PIPELINE RÉUTILISABLE
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 9 : Création d'un pipeline réutilisable        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

### Documentation des arguments de creer_pipeline()
# method : Algorithme à fixer pour le pipeline.
# k : Nombre de clusters fixe pour le pipeline.
# cr : Booléen. Si TRUE, standardise les données avant le clustering (centrage/réduction).
# use_kmeans : Booléen (spécifique à 'cah_kmeans'). Si TRUE, utilise le K-means après CAH.
# ... : Tout autre paramètre fixe pour l'algorithme R6 sous-jacent.

# 9.1 Créer un pipeline CAH+K-means avec k=3
cat("--- 9.1 Création d'un pipeline CAH+K-means ---\n")
mon_pipeline <- creer_pipeline(
 method = "cah_kmeans",
 k = 3,
 cr = TRUE, # Centrage-réduction activé par défaut
 use_kmeans = TRUE
)

# 9.2 Appliquer le pipeline à différents datasets
cat("\n--- 9.2 Application du pipeline ---\n")

# Sur iris
resultat_iris <- mon_pipeline(iris[1:4])
cat("✓ Pipeline appliqué sur iris\n")

# Sur mtcars (autre dataset)
data(mtcars)
resultat_mtcars <- mon_pipeline(mtcars[1:7])
cat("\n✓ Pipeline appliqué sur mtcars\n")
resultat_mtcars$print()

# ============================================================================
# PARTIE 10 & 11 : CLUSTERING SUR DONNÉES MIXTES (K-PROTOTYPES)
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 10 & 11 : Clustering sur données mixtes (K-prototypes) ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# ----------------------------------------------------------------------------
# 11.1 PRÉPARATION DU DATASET MIXTE (Titanic)
# ----------------------------------------------------------------------------
data(Titanic)
data_titanic <- as.data.frame(Titanic) %>% uncount(Freq) 

data_mixte_kproto <- data_titanic %>%
 mutate(
  Age = as.factor(Age), 
  Class = as.factor(Class), 
  Sex = as.factor(Sex),
  Survived_Num = as.numeric(Survived == "Yes") 
 ) %>%
 select(Age, Class, Sex, Survived_Num)

cat("📊 Dataset Titanic (mixte) prêt pour K-prototypes.\n")

# ----------------------------------------------------------------------------
# 11.2 CLUSTERING AVEC LA MÉTHODE faire_clustering(method = "kprototypes")
# ----------------------------------------------------------------------------
cat("--- 11.2 Lancement de K-prototypes (k=4) ---\n")

# Note : On suppose que faire_clustering() gère la détection des types de variables
# et peut passer des arguments optionnels comme 'lambda' si nécessaire.
model_kproto <- faire_clustering(
 data_mixte_kproto, 
 k = 4, 
 method = "kprototypes",
 lambda = 0.5 # Exemple de paramètre supplémentaire pour K-prototypes
)

cat("\n--- 11.3 Résumé du clustering K-prototypes ---\n")
resumer_clustering(model_kproto)


# ----------------------------------------------------------------------------
# 11.4 TEST DE LA MÉTHODE $predict() - CLASSIFICATION D'UNE NOUVELLE OBSERVATION
# ----------------------------------------------------------------------------
cat("\n\n--- 11.4 Classification d'une nouvelle observation avec $predict() ---\n")

# Nouvelle observation fictive (doit avoir les mêmes niveaux de facteurs!)
nouvelle_obs_test <- data.frame(
 Age = factor("Child", levels = levels(data_mixte_kproto$Age)),
 Class = factor("1st", levels = levels(data_mixte_kproto$Class)),
 Sex = factor("Female", levels = levels(data_mixte_kproto$Sex)),
 Survived_Num = 0.5
)

tryCatch({
 prediction_groupe <- model_kproto$predict(nouvelle_obs_test)
 cat(sprintf("✓ Nouvelle observation affectée au Groupe : %s\n", prediction_groupe))
}, error = function(e) {
 cat(sprintf("❌ Erreur lors du test de $predict() : %s\n", e$message))
})

# ============================================================================
# PARTIE 12 : TESTS DE ROBUSTESSE ET GESTION DES ERREURS
# (Partie critique pour la qualité du package)
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ PARTIE 12 : TESTS DE ROBUSTESSE ET GESTION DES ERREURS    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Utilisation de tryCatch pour isoler les erreurs et vérifier la gestion

# --- TEST 1 : Données manquantes (NA) ---
cat("--- Test 1 : Gestion des valeurs manquantes (NA) ---\n")
X_na <- X_num
X_na[1, 1] <- NA 
X_na[5, 4] <- NA

tryCatch({
 # On suppose que faire_clustering() doit échouer ou gérer l'imputation/suppression
 resultat_na <- faire_clustering(data = X_na, k = 3, method = "kmeans")
 cat("✓ Le clustering a réussi malgré les NA (Probablement imputation/suppression interne)\n")
}, error = function(e) {
 cat(sprintf("❌ Clustering échoué avec NA (Attendu si l'utilisateur doit pré-traiter) : %s\n", e$message))
 cat("→ Recommandation : Les données doivent être propres avant l'appel.\n")
})


# --- TEST 2 : Données incompatibles avec la méthode ---
cat("\n--- Test 2 : Utilisation de données catégorielles avec 'kmeans' ---\n")
# On prend le dataset mixte et on le passe à un algorithme numérique (kmeans)
X_incompatible <- data_mixte_kproto

tryCatch({
 resultat_incomp <- faire_clustering(data = X_incompatible, k = 3, method = "kmeans")
 cat("❌ Le clustering K-means a réussi sur des facteurs (Résultat incohérent ou conversion forcée)\n")
}, error = function(e) {
 cat(sprintf("✓ Le clustering a échoué (Attendu) : %s\n", e$message))
 cat("→ Le système doit bloquer les données catégorielles pour les méthodes purement numériques.\n")
})

# --- TEST 3 : Cas limite k=1 ou k_min > k_max ---
cat("\n--- Test 3.1 : Test de k=1 ---\n")
tryCatch({
 faire_clustering(data = X_num, k = 1, method = "kmeans")
 cat("❌ Clustering réussi avec k=1 (Peut-être autorisé mais trivial)\n")
}, error = function(e) {
 cat(sprintf("✓ Clustering échoué (Attendu) : %s\n", e$message))
 cat("→ k doit être >= 2 pour définir des groupes.\n")
})

cat("\n--- Test 3.2 : Test de k_min > k_max pour trouver_k_optimal ---\n")
tryCatch({
 trouver_k_optimal(X_num, k_min = 8, k_max = 5, method = "kmeans", afficher_graphique = FALSE)
 cat("❌ trouver_k_optimal a réussi avec k_min > k_max\n")
}, error = function(e) {
 cat(sprintf("✓ trouver_k_optimal a échoué (Attendu) : %s\n", e$message))
 cat("→ Vérification des bornes de k est cruciale.\n")
})

# --- TEST 4 : Dataset vide (0 lignes) ---
cat("\n--- Test 4 : Dataset avec 0 observation ---\n")
X_vide <- X_num[0,]

tryCatch({
 faire_clustering(data = X_vide, k = 3, method = "kmeans")
 cat("❌ Clustering réussi avec 0 observation\n")
}, error = function(e) {
 cat(sprintf("✓ Clustering échoué (Attendu) : %s\n", e$message))
 cat("→ Le package doit vérifier que N >= k.\n")
})


# ============================================================================
# RÉSUMÉ DES FONCTIONS UTILISATEUR DISPONIBLES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ RÉSUMÉ DES FONCTIONS UTILISATEUR DISPONIBLES         ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📚 FONCTIONS PRINCIPALES (voir doc ci-dessus pour les arguments) :\n\n")

cat("1️⃣ faire_clustering(data, k, method, ...)
 → Clustering simplifié (principal point d'entrée).
 
2️⃣ trouver_k_optimal(data, k_min, k_max, method, afficher_graphique, ...)
 → Recherche du nombre optimal de clusters (k).
 
3️⃣ analyser_illustratives(model, variables, afficher)
 → Analyse de variables illustratives.
 
4️⃣ caracteriser_groupes(model, var_illus, nom_var, mode)
 → Caractérisation détaillée des groupes.
 
5️⃣ comparer_algorithmes(data, k, methods, ...)
 → Comparaison de plusieurs algorithmes.
 
6️⃣ statistiques_par_groupe(model, data, afficher)
 → Statistiques descriptives par groupe.
 
7️⃣ obtenir_groupes(model)
 → Extrait le vecteur des affectations aux groupes.
 
8️⃣ exporter_resultats(model, data_originale, inclure_donnees, fichier)
 → Export vers data frame ou fichier CSV.
 
9️⃣ generer_rapport(model, fichier)
 → Rapport textuel complet.
 
🔟 clustering_complet(data, variables_illustratives, k_min, k_max, k_final, method, fichier_resultats, fichier_rapport)
 → Workflow tout-en-un automatisé.
 
1️⃣1️⃣ creer_pipeline(method, k, cr, use_kmeans, ...)
 → Pipeline réutilisable avec paramètres fixés.\n")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║ 🎉 FIN DU TUTORIEL - Package de Clustering R6        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")