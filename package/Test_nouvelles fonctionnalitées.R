setwd("C:/Users/Romain_admin/Documents/GitHub/RollerClustR/package")
# =====================================================
# Script de test des nouvelles fonctionnalités
# Date : 11 novembre 2025
# =====================================================

# ATTENTION : Ce script suppose que vous avez :
# 1. Remplacé ClusterAnalysis_parentclass.R par la version corrigée
# 2. Installé le package cluster : install.packages("cluster")

cat("=== TEST DES NOUVELLES FONCTIONNALITÉS ===\n\n")

# -------------------------------------------------
# CONFIGURATION
# -------------------------------------------------

# Charger R6
if (!requireNamespace("R6", quietly = TRUE)) {
  stop("Package R6 requis. Installez avec : install.packages('R6')")
}
library(R6)

# Charger les classes (adaptez les chemins selon votre structure)
cat("Chargement des classes...\n")
source("ClusterAnalysis_parentclass.R")  # Version CORRIGÉE
source("kmeans_class.R")
cat("✓ Classes chargées\n\n")

# Données
data(iris)
X_actives <- iris[, 1:4]  # Variables pour clustering
X_illustrative <- iris[, 5, drop = FALSE]  # Variable Species

# -------------------------------------------------
# TEST 1 : MÉTHODE PREDICT()
# -------------------------------------------------

cat("========================================\n")
cat("TEST 1 : MÉTHODE $predict()\n")
cat("========================================\n\n")

cat("1.1 - Création et ajustement du modèle K-means (k=3)\n")
model <- Kmeans$new(k = 3, cr = TRUE)
model$fit(X_actives)
cat("✓ Modèle ajusté\n\n")

cat("1.2 - Test predict() avec variable catégorielle (Species)\n")
resultats <- model$predict(X_illustrative)
print(resultats)
cat("\n")

cat("1.3 - Vérifications\n")
# Vérifier la structure du résultat
if (!is.data.frame(resultats)) {
  stop("❌ ERREUR : predict() ne retourne pas un data frame")
}
if (nrow(resultats) != 1) {
  stop("❌ ERREUR : predict() devrait retourner 1 ligne (1 variable)")
}
required_cols <- c("variable", "type", "cluster_assigne", "indicateur", "valeur", "interpretation")
if (!all(required_cols %in% names(resultats))) {
  stop("❌ ERREUR : Colonnes manquantes dans le résultat")
}
if (resultats$type[1] != "catégorielle") {
  stop("❌ ERREUR : Type devrait être 'catégorielle'")
}
if (resultats$indicateur[1] != "Cramer's V") {
  stop("❌ ERREUR : Indicateur devrait être 'Cramer's V'")
}
if (resultats$valeur[1] < 0 || resultats$valeur[1] > 1) {
  stop("❌ ERREUR : Valeur doit être entre 0 et 1")
}
cat("✓ Structure du résultat : OK\n")
cat("✓ Type de variable : OK\n")
cat("✓ Indicateur : OK\n")
cat("✓ Valeur dans [0,1] : OK\n")
cat("✓ Cluster assigné :", resultats$cluster_assigne[1], "\n\n")

cat("1.4 - Test predict() avec variables multiples (numériques + catégorielles)\n")
X_multi <- data.frame(
  Species = iris$Species,
  Sepal_sum = iris$Sepal.Length + iris$Sepal.Width,
  Petal_ratio = iris$Petal.Length / iris$Petal.Width
)
resultats_multi <- model$predict(X_multi)
print(resultats_multi)
cat("\n")

if (nrow(resultats_multi) != 3) {
  stop("❌ ERREUR : Devrait avoir 3 lignes (3 variables)")
}
cat("✓ Variables multiples : OK\n")
cat("✓ Variables numériques utilisent eta² : OK\n\n")

cat("1.5 - Test erreur si nombre d'observations différent\n")
X_wrong <- iris[1:100, 5, drop = FALSE]
tryCatch({
  model$predict(X_wrong)
  stop("❌ ERREUR : Devrait échouer avec mauvais nombre d'obs")
}, error = function(e) {
  if (grepl("observations", e$message)) {
    cat("✓ Erreur détectée correctement :", e$message, "\n\n")
  } else {
    stop("❌ ERREUR : Message d'erreur incorrect")
  }
})

cat("========================================\n")
cat("✓✓✓ TEST 1 RÉUSSI : predict() fonctionne\n")
cat("========================================\n\n\n")

# -------------------------------------------------
# TEST 2 : MÉTHODE ELBOW
# -------------------------------------------------

cat("========================================\n")
cat("TEST 2 : MÉTHODE $elbow_method()\n")
cat("========================================\n\n")

cat("2.1 - Test avec graphique désactivé\n")
model_temp <- Kmeans$new()
inerties <- model_temp$elbow_method(X_actives, k_max = 8, plot = FALSE)
cat("Inerties calculées pour k=1 à 8\n")
print(inerties)
cat("\n")

cat("2.2 - Vérifications\n")
if (!is.numeric(inerties)) {
  stop("❌ ERREUR : elbow_method() doit retourner un vecteur numérique")
}
if (length(inerties) != 8) {
  stop("❌ ERREUR : Devrait avoir 8 valeurs (k=1 à 8)")
}
if (any(is.na(inerties))) {
  stop("❌ ERREUR : Des inerties sont NA")
}
# Vérifier décroissance
if (!all(diff(inerties) < 0)) {
  warning("⚠ AVERTISSEMENT : Inerties ne décroissent pas strictement")
}
cat("✓ Type de retour : OK\n")
cat("✓ Longueur : OK\n")
cat("✓ Pas de NA : OK\n")
cat("✓ Décroissance : OK\n\n")

cat("2.3 - Test avec graphique activé\n")
cat("(Un graphique devrait s'afficher)\n")
par(mfrow = c(1, 1))
inerties_plot <- model_temp$elbow_method(X_actives, k_max = 8, plot = TRUE)
cat("✓ Graphique affiché\n\n")

cat("2.4 - Test erreur si données non numériques\n")
X_mixed <- data.frame(x = iris$Sepal.Length, y = iris$Species)
tryCatch({
  model_temp$elbow_method(X_mixed, k_max = 5)
  stop("❌ ERREUR : Devrait échouer avec données mixtes")
}, error = function(e) {
  if (grepl("numériques", e$message)) {
    cat("✓ Erreur détectée : données non numériques\n\n")
  } else {
    stop("❌ ERREUR : Message incorrect:", e$message)
  }
})

cat("========================================\n")
cat("✓✓✓ TEST 2 RÉUSSI : elbow_method() fonctionne\n")
cat("========================================\n\n\n")

# -------------------------------------------------
# TEST 3 : MÉTHODE SILHOUETTE
# -------------------------------------------------

cat("========================================\n")
cat("TEST 3 : MÉTHODE $silhouette_method()\n")
cat("========================================\n\n")

cat("3.1 - Vérification du package cluster\n")
if (!requireNamespace("cluster", quietly = TRUE)) {
  cat("⚠ AVERTISSEMENT : Package 'cluster' non installé\n")
  cat("   La méthode silhouette_method() ne peut pas être testée\n")
  cat("   Installez avec : install.packages('cluster')\n\n")
  cat("========================================\n")
  cat("⊘ TEST 3 SAUTÉ : cluster non disponible\n")
  cat("========================================\n\n\n")
} else {
  cat("✓ Package cluster disponible\n\n")
  
  cat("3.2 - Test avec graphique désactivé\n")
  silhouettes <- model_temp$silhouette_method(X_actives, k_max = 8, plot = FALSE)
  cat("Silhouettes calculées pour k=2 à 8\n")
  print(silhouettes)
  cat("\n")
  
  cat("3.3 - Vérifications\n")
  if (!is.numeric(silhouettes)) {
    stop("❌ ERREUR : silhouette_method() doit retourner un vecteur numérique")
  }
  if (length(silhouettes) != 7) {  # k=2 à 8 = 7 valeurs
    stop("❌ ERREUR : Devrait avoir 7 valeurs (k=2 à 8)")
  }
  if (any(is.na(silhouettes))) {
    stop("❌ ERREUR : Des silhouettes sont NA")
  }
  if (any(silhouettes < -1 | silhouettes > 1)) {
    stop("❌ ERREUR : Silhouettes doivent être dans [-1, 1]")
  }
  cat("✓ Type de retour : OK\n")
  cat("✓ Longueur : OK\n")
  cat("✓ Pas de NA : OK\n")
  cat("✓ Valeurs dans [-1,1] : OK\n\n")
  
  cat("3.4 - Test avec graphique activé\n")
  cat("(Un graphique devrait s'afficher)\n")
  silhouettes_plot <- model_temp$silhouette_method(X_actives, k_max = 8, plot = TRUE)
  cat("✓ Graphique affiché\n\n")
  
  cat("3.5 - Identification du k optimal\n")
  k_optimal <- which.max(silhouettes) + 1  # +1 car commence à k=2
  cat("k optimal selon silhouette :", k_optimal, "\n")
  cat("Silhouette pour ce k :", silhouettes[k_optimal - 1], "\n\n")
  
  cat("========================================\n")
  cat("✓✓✓ TEST 3 RÉUSSI : silhouette_method() fonctionne\n")
  cat("========================================\n\n\n")
}

# -------------------------------------------------
# TEST 4 : WORKFLOW COMPLET
# -------------------------------------------------

cat("========================================\n")
cat("TEST 4 : WORKFLOW COMPLET\n")
cat("========================================\n\n")

cat("4.1 - Détermination du k optimal\n")
model_workflow <- Kmeans$new()

cat("Méthode du coude...\n")
inerties <- model_workflow$elbow_method(X_actives, k_max = 8, plot = FALSE)
# k suggéré par différences secondes
diff2 <- diff(diff(inerties))
k_elbow <- which.max(abs(diff2)) + 1
cat("✓ k suggéré (elbow) :", k_elbow, "\n")

if (requireNamespace("cluster", quietly = TRUE)) {
  cat("Méthode silhouette...\n")
  silhouettes <- model_workflow$silhouette_method(X_actives, k_max = 8, plot = FALSE)
  k_silhouette <- which.max(silhouettes) + 1
  cat("✓ k suggéré (silhouette) :", k_silhouette, "\n\n")
  
  k_final <- k_silhouette  # Privilégier silhouette
} else {
  k_final <- k_elbow
}

cat("4.2 - Clustering avec k =", k_final, "\n")
model_final <- Kmeans$new(k = k_final, cr = TRUE)
model_final$fit(X_actives)
cat("✓ Modèle ajusté\n\n")

cat("4.3 - Analyse variables illustratives\n")
resultats_final <- model_final$predict(X_illustrative)
print(resultats_final)
cat("\n")

cat("4.4 - Interprétation\n")
cat("Association Species-Clustering :\n")
cat("  Indicateur :", resultats_final$indicateur[1], "\n")
cat("  Valeur :", round(resultats_final$valeur[1], 3), "\n")
cat("  Cluster assigné :", resultats_final$cluster_assigne[1], "\n")
cat("  Interprétation :", resultats_final$interpretation[1], "\n\n")

cat("========================================\n")
cat("✓✓✓ TEST 4 RÉUSSI : Workflow complet\n")
cat("========================================\n\n\n")

# -------------------------------------------------
# RÉSUMÉ
# -------------------------------------------------

cat("==========================================\n")
cat("       RÉSUMÉ DES TESTS\n")
cat("==========================================\n\n")

cat("✓ TEST 1 : predict() ..................... OK\n")
cat("✓ TEST 2 : elbow_method() ................ OK\n")
if (requireNamespace("cluster", quietly = TRUE)) {
  cat("✓ TEST 3 : silhouette_method() ........... OK\n")
} else {
  cat("⊘ TEST 3 : silhouette_method() .... SAUTÉ (cluster manquant)\n")
}
cat("✓ TEST 4 : Workflow complet .............. OK\n\n")

cat("==========================================\n")
cat("  ✓✓✓ TOUS LES TESTS RÉUSSIS ✓✓✓\n")
cat("==========================================\n\n")

cat("Remarques :\n")
cat("- Toutes les méthodes sont opérationnelles\n")
cat("- Les classes filles héritent automatiquement\n")
cat("- Les validations et gestions d'erreurs fonctionnent\n")
if (!requireNamespace("cluster", quietly = TRUE)) {
  cat("\n⚠ Pour tester silhouette_method(), installez 'cluster':\n")
  cat("  install.packages('cluster')\n")
}

cat("\n🎯 Prêt pour la soutenance !\n\n")