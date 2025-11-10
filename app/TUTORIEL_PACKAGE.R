# ============================================================================
# TUTORIEL COMPLET DU PACKAGE DE CLUSTERING
# Utilisation des fonctions utilisateur (user_functions.R)
# ============================================================================

setwd("C:/Users/Romain_admin/Documents/GitHub/RollerClustR/app")
getwd()
# Chargement des librairies et sources
library(R6)

# Charger tous les fichiers du package
source("ClusterAnalysis_parentclass.R")
source("CAH_class.R")
source("kmeans_class.R")
source("ClustOfVar_class.R")
source("Kprototypes_class.R")
source("wrapper.R")
source("user_functions.R")



# ============================================================================
# PARTIE 1 : CLUSTERING SIMPLE AVEC DONNÉES NUMÉRIQUES
# ============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 1 : Clustering simple sur données numériques          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Données d'exemple : iris (dimensions des fleurs)
data(iris)
X_num <- iris[, 1:4]  # Variables numériques uniquement

cat("📊 Données : iris (150 observations, 4 variables numériques)\n")
cat("Variables :", paste(names(X_num), collapse = ", "), "\n\n")

# 1.1 Clustering automatique (sélection auto de l'algorithme)
cat("--- 1.1 Clustering automatique ---\n")
resultat1 <- faire_clustering(X_num, k = 3, method = "auto")
cat("\n")

# 1.2 Forcer l'utilisation de CAH+K-means
cat("--- 1.2 Clustering avec CAH+K-means ---\n")
resultat2 <- faire_clustering(X_num, k = 3, method = "cah_kmeans")
cat("\n")

# 1.3 Afficher un résumé détaillé
cat("--- 1.3 Résumé détaillé du clustering ---\n")
resumer_clustering(resultat2)

# ============================================================================
# PARTIE 2 : TROUVER LE NOMBRE OPTIMAL DE CLUSTERS
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 2 : Recherche du nombre optimal de clusters (k)       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 2.1 Évaluation avec CAH+K-means
cat("--- 2.1 Évaluation de k (de 2 à 8) ---\n")
evaluation_k <- trouver_k_optimal(
  X_num, 
  k_min = 2, 
  k_max = 8,
  method = "cah_kmeans",
  afficher_graphique = TRUE  # Mettre TRUE pour voir le graphique
)

cat("\nRésultats de l'évaluation :\n")
print(evaluation_k)

# Le k optimal est suggéré automatiquement
cat("\n💡 Conseil : Choisir k où le gain d'inertie commence à stagner (méthode du coude)\n")

# ============================================================================
# PARTIE 3 : ANALYSE DES VARIABLES ILLUSTRATIVES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 3 : Analyse de variables illustratives                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Utiliser le clustering avec k=3
model <- faire_clustering(X_num, k = 3, method = "cah_kmeans")

# 3.1 Analyser la variable Species (qualitative)
cat("--- 3.1 Analyse de la variable Species (qualitative) ---\n")
analyse_species <- analyser_illustratives(
  model, 
  iris[5],  # Species
  afficher = TRUE
)

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
cat("║  PARTIE 4 : Comparaison de plusieurs algorithmes              ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 4.1 Comparer CAH+K-means vs K-means standard 
cat("--- 4.1 Comparaison de 3 algorithmes ---\n")
comparateur <- comparer_algorithmes(
  X_num, 
  k = 3,
  methods = c("cah_kmeans", "kmeans")
)

# 4.2 Afficher les résultats de comparaison
cat("\n--- 4.2 Résultats de la comparaison ---\n")
resultats_comp <- comparateur$compare()

# 4.3 Obtenir un algorithme spécifique
model_kmeans <- comparateur$get_result("kmeans")
cat("\n📌 Modèle K-means récupéré de la comparaison\n")
model_kmeans$print()

# ============================================================================
# PARTIE 5 : STATISTIQUES PAR GROUPE
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 5 : Statistiques descriptives par groupe              ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 5.1 Calculer les statistiques pour chaque variable
stats <- statistiques_par_groupe(model, X_num, afficher = TRUE)

# 5.2 Accéder aux statistiques d'une variable spécifique
cat("\n--- 5.2 Statistiques pour Sepal.Length uniquement ---\n")
print(stats$Sepal.Length)

# ============================================================================
# PARTIE 6 : EXPORT DES RÉSULTATS
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 6 : Export des résultats                              ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 6.1 Obtenir les groupes
groupes <- obtenir_groupes(model)
cat("--- 6.1 Groupes obtenus ---\n")
cat("Distribution :\n")
print(table(groupes))

# 6.2 Exporter vers un data frame
resultats_df <- exporter_resultats(
  model, 
  iris,  # Données originales
  inclure_donnees = TRUE,
  fichier = "resultats.csv"  # Mettre "resultats.csv" pour sauvegarder
)

cat("\n--- 6.2 Aperçu des résultats exportés ---\n")
print(head(resultats_df, 10))

# 6.3 Générer un rapport textuel
cat("\n--- 6.3 Rapport de clustering ---\n")
generer_rapport(model, fichier = "rapport.txt")  # Mettre "rapport.txt" pour sauvegarder

# ============================================================================
# PARTIE 7 : CLUSTERING DE VARIABLES (ClustOfVar)
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 7 : Clustering de VARIABLES (ClustOfVar)              ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("ATTENTION : ClustOfVar fait du clustering sur les VARIABLES, pas les observations!\n\n")

# 7.1 Clustering de variables numériques
cat("--- 7.1 Clustering des 4 variables d'iris ---\n")
model_var <- faire_clustering(X_num, k = 2, method = "clustofvar")

# 7.2 Résumé détaillé
cat("\n--- 7.2 Résumé du clustering de variables ---\n")
resumer_clustering(model_var)

# 7.3 Groupes de variables
cat("\n--- 7.3 Composition des groupes de variables ---\n")
groupes_var <- obtenir_groupes(model_var)
print(groupes_var)

for (k in 1:2) {
  vars_k <- names(groupes_var)[groupes_var == k]
  cat("Cluster", k, "contient :", paste(vars_k, collapse = ", "), "\n")
}

# 7.4 Qualité des clusters de variables
cat("\n--- 7.4 Qualité des clusters de variables ---\n")
if (inherits(model_var, "ClustOfVar")) {
  qualites <- model_var$qualite_clusters()
  print(qualites)
  
  # Matrice de corrélations
  cat("\n--- 7.5 Matrice de corrélations variables/clusters ---\n")
  cor_mat <- model_var$matrice_correlations()
  print(round(cor_mat, 3))
}

# ============================================================================
# PARTIE 8 : WORKFLOW COMPLET AUTOMATISÉ
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 8 : Workflow complet automatisé                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("🚀 Lancement d'un workflow complet...\n\n")

# 8.1 Workflow tout-en-un
workflow <- clustering_complet(
  data = X_num,
  variables_illustratives = iris[5],  # Species
  k_min = 2,
  k_max = 6,
  k_final = NULL,  # Détection automatique
  method = "cah_kmeans",
  fichier_resultats = NULL,  # Mettre "workflow_resultats.csv" pour sauvegarder
  fichier_rapport = "workflow_rapport.txt"      # Mettre NULL pour ne pas sauvegarder
)

# 8.2 Accéder aux résultats du workflow
cat("\n--- 8.2 Contenu du workflow ---\n")
cat("✓ Objet de clustering : disponible\n")
cat("✓ Groupes : ", length(workflow$groupes), "observations\n")
cat("✓ Résultats : ", nrow(workflow$resultats), "lignes\n")
cat("✓ Évaluation k : ", nrow(workflow$evaluation_k), "valeurs testées\n")
if (!is.null(workflow$illustratives)) {
  cat("✓ Variables illustratives : ", nrow(workflow$illustratives), "analysées\n")
}

# ============================================================================
# PARTIE 9 : PIPELINE RÉUTILISABLE
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 9 : Création d'un pipeline réutilisable               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 9.1 Créer un pipeline CAH+K-means avec k=3
cat("--- 9.1 Création d'un pipeline CAH+K-means ---\n")
mon_pipeline <- creer_pipeline(
  method = "cah_kmeans",
  k = 3,
  cr = TRUE,
  use_kmeans = TRUE
)

cat("✓ Pipeline créé avec les paramètres :\n")
cat("  - Méthode :", attr(mon_pipeline, "method"), "\n")
cat("  - k =", attr(mon_pipeline, "k"), "\n")

# 9.2 Appliquer le pipeline à différents datasets
cat("\n--- 9.2 Application du pipeline ---\n")

# Sur iris
resultat_iris <- mon_pipeline(iris[1:4])
cat("✓ Pipeline appliqué sur iris\n")
resultat_iris$print()

# Sur mtcars (autre dataset)
cat("\n✓ Pipeline appliqué sur mtcars\n")
data(mtcars)
resultat_mtcars <- mon_pipeline(mtcars[1:7])
resultat_mtcars$print()

# ============================================================================
# PARTIE 10 : DONNÉES MIXTES (numériques + catégorielles)
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 10 : Clustering sur données mixtes                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# 10.1 Créer un dataset mixte
data_mixte <- data.frame(
  age = c(25, 30, 35, 40, 22, 28, 33, 38, 45, 50,
          26, 31, 36, 41, 23, 29, 34, 39, 46, 51),
  salaire = c(30, 40, 50, 60, 28, 38, 48, 58, 70, 80,
              32, 42, 52, 62, 29, 39, 49, 59, 71, 81),
  sexe = factor(rep(c("H", "F", "H", "F"), 5)),
  diplome = factor(rep(c("Bac", "Licence", "Master", "Doctorat"), 5))
)

cat("📊 Dataset mixte créé :\n")
cat("  - 2 variables numériques : age, salaire\n")
cat("  - 2 variables catégorielles : sexe, diplome\n")
cat("  - 20 observations\n\n")

# 10.2 Clustering avec ClustOfVar (gère le mixte)
cat("--- 10.2 Clustering avec ClustOfVar (gère les données mixtes) ---\n")
model_mixte <- faire_clustering(
  data_mixte, 
  k = 2, 
  method = "clustofvar"
)

cat("\n--- 10.3 Résumé du clustering mixte ---\n")
resumer_clustering(model_mixte)

# ============================================================================
# PARTIE 11 : CLUSTERING SUR DONNÉES MIXTES AVEC K-PROTOTYPES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PARTIE 11 : Test de K-PROTOTYPES (Algorithme pour données mixtes)    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# ----------------------------------------------------------------------------
# 11.1 PRÉPARATION DU DATASET MIXTE (Titanic)
# ----------------------------------------------------------------------------
cat("--- 11.1 Préparation du dataset 'Titanic' ---\n")
library(tidyverse)
# Charger les données (disponibles dans R de base)
data(Titanic)
# Transformer la table de fréquences en un data frame d'observations
data_titanic <- as.data.frame(Titanic) %>%
  uncount(Freq) # Crée une ligne par observation (1:1)

# Sélectionner les variables et les convertir en facteurs/numériques

# Solution 1: Conversion explicite des niveaux du facteur
data_mixte_kproto <- data_titanic %>%
  mutate(
    Age = as.factor(Age), 
    Class = as.factor(Class), 
    Sex = as.factor(Sex),
    # Conversion correcte pour Yes/No :
    Survived_Num = as.numeric(Survived == "Yes") 
    # Ceci convertit TRUE/FALSE en 1/0, créant une vraie variable numérique
  ) %>%
  select(Age, Class, Sex, Survived_Num)

# Vous devez vous assurer que Survided_Num est bien de type 'numeric' double
# et non 'integer' dans votre data frame final.

# Vous devez vous assurer que Survided_Num est bien de type 'numeric' double
# et non 'integer' dans votre data frame final.


# On vérifie les types
print(sapply(data_mixte_kproto, class))
cat(sprintf("\n📊 Dataset Titanic (mixte) : %d observations, %d variables.\n", 
            nrow(data_mixte_kproto), ncol(data_mixte_kproto)))
cat("Variables catégorielles : Age, Class, Sex.\n")
cat("Variables numériques : Survived_Num.\n\n")

# ----------------------------------------------------------------------------
# 11.2 CLUSTERING AVEC LA MÉTHODE faire_clustering(method = "kprototypes")
# ----------------------------------------------------------------------------
cat("--- 11.2 Lancement de K-prototypes (k=4) ---\n")

# Note : K-prototypes nécessite de spécifier un poids 'lambda' pour les 
# variables numériques par rapport aux catégorielles.
# Si votre 'faire_clustering' ne le gère pas directement, il faudra ajouter 
# un paramètre 'lambda' ou laisser la classe Kprototypes_class utiliser un défaut.
model_kproto <- faire_clustering(
  data_mixte_kproto, 
  k = 4, # On choisit k=4 (pour les 4 classes de passagers/équipage)
  method = "kprototypes"
)

print(sapply(data_mixte_kproto, class))
cat("\n--- 11.3 Résumé du clustering K-prototypes ---\n")
resumer_clustering(model_kproto)


# ----------------------------------------------------------------------------
# 11.4 TEST DE LA MÉTHODE $predict() - CLASSIFICATION D'UNE NOUVELLE OBSERVATION
# ----------------------------------------------------------------------------
cat("\n\n--- 11.4 Classification d'une nouvelle observation avec $predict() ---\n")

# Création d'une nouvelle observation fictive
nouvelle_obs <- data.frame(
  Age = factor("Child", levels = levels(data_mixte_kproto$Age)),
  Class = factor("1st", levels = levels(data_mixte_kproto$Class)),
  Sex = factor("Female", levels = levels(data_mixte_kproto$Sex)),
  Survived_Num = 0.5 # Valeur médiane hypothétique
)

# Assurer que les niveaux correspondent (très important pour K-prototypes)
# Reconstruire avec les mêmes niveaux pour le test :
nouvelle_obs_test <- data.frame(
  Age = factor("Child", levels = levels(data_mixte_kproto$Age)),
  Class = factor("1st", levels = levels(data_mixte_kproto$Class)),
  Sex = factor("Female", levels = levels(data_mixte_kproto$Sex)),
  Survived_Num = 0.5
)

# Si la méthode $predict(X) renvoie simplement l'affectation du groupe :
if (inherits(model_kproto, "Kprototypes")) {
  # Assurez-vous que la méthode est bien implémentée dans Kprototypes_class.R
  # Et qu'elle accepte un nouveau data.frame X pour la prédiction
  tryCatch({
    prediction_groupe <- model_kproto$predict(nouvelle_obs_test)
    cat(sprintf("✓ Nouvelle observation affectée au Groupe : %s\n", prediction_groupe))
  }, error = function(e) {
    cat(sprintf("❌ Erreur lors du test de $predict() : %s\n", e$message))
    cat("Note : La méthode $predict() pour K-prototypes doit gérer l'affectation de nouvelles données.\n")
  })
}


# ----------------------------------------------------------------------------
# 11.5 TEST DE L'ÉVALUATION DU K OPTIMAL (Applicable aux observations)
# ----------------------------------------------------------------------------
cat("\n\n--- 11.5 Recherche du k optimal pour K-prototypes (3 à 6) ---\n")

# K-prototypes utilise des métriques différentes (coût global)
# Si trouver_k_optimal() est bien implémenté pour Kprototypes (en utilisant 
# le coût des prototypes), il devrait fonctionner :

tryCatch({
  evaluation_k_kproto <- trouver_k_optimal(
    data_mixte_kproto, 
    k_min = 3, 
    k_max = 6,
    method = "kprototypes",
    afficher_graphique = TRUE # Afficher le graphique pour l'inertie/le coût
  )

  cat("\nRésultats de l'évaluation du coût :\n")
  print(evaluation_k_kproto)

}, error = function(e) {
  cat(sprintf("❌ Erreur lors de l'évaluation de k : %s\n", e$message))
  cat("Note : Assurez-vous que 'trouver_k_optimal' gère la métrique de coût pour K-prototypes.\n")
})


# ----------------------------------------------------------------------------
# 11.6 CARACTÉRISATION DES GROUPES (VARIABLES MIXTES)
# ----------------------------------------------------------------------------
cat("\n\n--- 11.6 Caractérisation des groupes K-prototypes ---\n")

# La caractérisation doit afficher :
# - Moyennes (pour Survived_Num)
# - Distribution des fréquences (pour Age, Class, Sex)

# On utilise la variable 'Class' de la source de données pour l'analyse illustrative
var_illus_qual <- data_mixte_kproto$Class
nom_var_illus <- "Class_Ticket"

caracteriser_groupes(
  model_kproto, 
  var_illus_qual, 
  nom_var_illus,
  mode = "complet"
)

cat("\n--- FIN DU TEST K-PROTOTYPES ---\n")


# ============================================================================
# RÉSUMÉ DES FONCTIONS UTILISATEUR DISPONIBLES
# ============================================================================

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  RÉSUMÉ DES FONCTIONS UTILISATEUR DISPONIBLES                 ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📚 FONCTIONS PRINCIPALES :\n\n")

cat("1️⃣  faire_clustering()
   → Clustering simplifié avec sélection automatique d'algorithme
   → Paramètres : data, k, method ('auto', 'cah_kmeans', 'kmeans', 'clustofvar')
   
2️⃣  trouver_k_optimal()
   → Recherche du nombre optimal de clusters
   → Teste plusieurs valeurs de k et affiche le graphique du coude
   
3️⃣  analyser_illustratives()
   → Analyse de variables illustratives via predict()
   → Retourne indicateurs de liaison (rapport corrélation, V de Cramer)
   
4️⃣  caracteriser_groupes()
   → Caractérisation détaillée des groupes avec une variable
   → Mode 'complet' ou 'rapide'
   
5️⃣  comparer_algorithmes()
   → Comparaison de plusieurs algorithmes sur les mêmes données
   → Calcule les matrices de confusion et taux d'accord
   
6️⃣  obtenir_groupes()
   → Extrait le vecteur des affectations aux groupes
   
7️⃣  exporter_resultats()
   → Export vers data frame ou fichier CSV
   → Peut inclure les données originales
   
8️⃣  visualiser_clustering()
   → Visualisations adaptées selon l'algorithme
   → Dendrogramme pour CAH, heatmap pour ClustOfVar
   
9️⃣  resumer_clustering()
   → Résumé détaillé avec statistiques spécifiques
   → Inertie, homogénéité selon l'algorithme
   
🔟 statistiques_par_groupe()
   → Statistiques descriptives pour chaque groupe
   → Moyenne, écart-type, min, max pour variables numériques
   
1️⃣1️⃣ generer_rapport()
   → Rapport textuel complet
   → Sauvegarde optionnelle dans un fichier
   
1️⃣2️⃣ clustering_complet()
   → Workflow tout-en-un automatisé
   → Recherche k optimal + clustering + analyse + export
   
1️⃣3️⃣ creer_pipeline()
   → Pipeline réutilisable avec paramètres fixés
   → Application à différents datasets\n")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  🎉 FIN DU TUTORIEL - Package de Clustering R6                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")
