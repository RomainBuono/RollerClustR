# =======================================================
# SCRIPT DE VÉRIFICATION DU PACKAGE DE CLUSTERING R6
# =======================================================

# --- 0. Chargement des packages de référence ---
# NÉCESSAIRE : R6, mclust (pour ARI), et les packages de référence
library(R6)
library(mclust) # Pour l'Adjusted Rand Index (ARI)
library(cluster) # Pour agnes (CAH de référence)
library(clustMixType) # Pour k-prototypes (référence pour données mixtes)
library(ClustOfVar) # Pour le clustering de variables (référence)
library(FactoMineR) # Pour FactoMineR::HCPC utilisé dans ClustOfVar
library(stats) # Pour la fonction kmeans de base

# --- 1. Préparation des Données ---

# Données numériques pures (pour K-means et CAH)
data_numeric <- iris[, 1:4]
k_num <- 3

# Données mixtes (pour K-prototypes)
data_mixed <- iris
# Ajouter une variable catégorielle simple
data_mixed$Size <- factor(
  ifelse(data_mixed$Sepal.Length > median(data_mixed$Sepal.Length), "Large", "Small")
)
data_mixed$Species <- NULL # On retire Species car c'est la vérité terrain, pas une variable à clusteriser
k_mixed <- 3

# --- 2. Fonctions Utilitaires de Comparaison ---

comparer_partitions <- function(groupes_custom, groupes_ref, algo_nom) {
  cat(paste0("\n*** Comparaison des résultats: ", algo_nom, " ***\n"))
  
  # Nettoyer les NA si le package utilisateur les a gérés (omit ou autre)
  groupes_custom <- na.omit(groupes_custom)
  groupes_ref <- na.omit(groupes_ref)
  
  if (length(groupes_custom) != length(groupes_ref)) {
    warning("ATTENTION: Les tailles des partitions ne correspondent pas (possiblement dû à une gestion différente des NA).")
    return(NA)
  }
  
  # Calcul de l'Adjusted Rand Index (ARI)
  # Un ARI proche de 1 indique des partitions très similaires.
  ari_score <- adjustedRandIndex(groupes_custom, groupes_ref)
  
  cat(paste("  Taille des groupes (Custom):", paste(table(groupes_custom), collapse = ", "), "\n"))
  cat(paste("  Taille des groupes (Référence):", paste(table(groupes_ref), collapse = ", "), "\n"))
  cat(paste("  Adjusted Rand Index (ARI):", round(ari_score, 4), "\n"))
  
  if (ari_score > 0.95) {
    cat("  RÉSULTAT: ✅ SIMILAIRE (ARI > 0.95)\n")
  } else if (ari_score > 0.8) {
    cat("  RÉSULTAT: ⚠️ ACCEPTABLE (ARI > 0.8), les différences d'initialisation peuvent jouer.\n")
  } else {
    cat("  RÉSULTAT: ❌ DIFFÉRENT. Vérifiez l'implémentation de la distance/initialisation.\n")
  }
  
  return(ari_score)
}

# =======================================================
# --- 3. VÉRIFICATION K-MEANS (Numérique Pure) ---
# =======================================================

cat("\n=======================================================\n")
cat("VÉRIFICATION K-MEANS\n")
cat("=======================================================\n")

# A. Modèle de référence (Base R, nstart=10 pour la stabilité)
set.seed(42) # Pour la reproductibilité
km_ref <- kmeans(data_numeric, centers = k_num, nstart = 10, iter.max = 100)
groupes_ref_km <- km_ref$cluster

# B. Modèle Custom (votre package)
# On assume que votre classe s'appelle Kmeans et a une méthode $fit()
# Note: Utilisez le même 'nstart' si possible pour réduire l'impact de l'initialisation.
set.seed(42) # Réutiliser le même seed pour comparer les meilleures initialisations
km_custom_obj <- Kmeans$new(k = k_num, nstart = 10) #
km_custom_obj$fit(data_numeric)
groupes_custom_km <- km_custom_obj$Groupes

# C. Comparaison des Partitions
comparer_partitions(groupes_custom_km, groupes_ref_km, "K-means")

# D. Comparaison de l'Inertie
# L'inertie Totale Intra-classes (tot.withinss) devrait être très similaire
inertie_ref_km <- km_ref$tot.withinss
inertie_custom_km <- km_custom_obj$inertie()$intra

cat("\n*** Comparaison de l'Inertie Totale Intra-Classes ***\n")
cat(paste("  Inertie Référence (stats::kmeans):", round(inertie_ref_km, 4), "\n"))
cat(paste("  Inertie Custom (Votre Kmeans):", round(inertie_custom_km, 4), "\n"))
if (abs(inertie_ref_km - inertie_custom_km) < 1e-6) {
  cat("  RÉSULTAT: ✅ INERTIE IDENTIQUE\n")
} else {
  cat("  RÉSULTAT: ❌ INERTIE DIFFÉRENTE. Vérifiez la fonction de coût.\n")
}

# =======================================================
# --- 4. VÉRIFICATION CAH + K-MEANS (Numérique Pure) ---
# =======================================================

cat("\n=======================================================\n")
cat("VÉRIFICATION CAH + K-MEANS (Classification Hybride)\n")
cat("=======================================================\n")

# A. Modèle de référence (hclust + cutree + kmeans sur centres)
# Standardisation (si votre package le fait par défaut)
Z_ref <- scale(data_numeric)
dist_ref <- dist(Z_ref, method = "euclidean")
cah_ref_arbre <- hclust(dist_ref, method = "ward.D2")
groupes_cah_ref <- cutree(cah_ref_arbre, k = k_num)

# Pour être une vraie référence hybride, on raffine avec kmeans
# Les centres initiaux sont les centres des groupes CAH
centres_init <- sapply(1:k_num, function(i) colMeans(Z_ref[groupes_cah_ref == i, , drop = FALSE]))
centres_init <- t(centres_init)
km_ref_hybride <- kmeans(Z_ref, centers = centres_init, iter.max = 100, nstart = 1)
groupes_ref_hybride <- km_ref_hybride$cluster

# B. Modèle Custom (votre package)
set.seed(42) # Nécessaire si vous utilisez une initialisation aléatoire dans CAH_Kmeans, même si peu probable
cah_km_custom_obj <- CAH_Kmeans$new(k = k_num, method = "ward.D2") #
cah_km_custom_obj$fit(data_numeric)
groupes_custom_cah_km <- cah_km_custom_obj$Groupes

# C. Comparaison des Partitions
comparer_partitions(groupes_custom_cah_km, groupes_ref_hybride, "CAH + K-means Hybride")


# =======================================================
# --- 5. VÉRIFICATION K-PROTOTYPES (Données Mixtes) ---
# =======================================================

cat("\n=======================================================\n")
cat("VÉRIFICATION K-PROTOTYPES (Données Mixtes)\n")
cat("=======================================================\n")

# NOTE: La sensibilité à l'initialisation est TRÈS forte ici. 
# Les résultats peuvent différer même avec un nstart > 1.
# On fixe un lambda arbitraire pour la comparaison.

# A. Modèle de référence (clustMixType::kproto)
lambda_val <- 0.8
set.seed(42)
kproto_ref <- kproto(data_mixed, k = k_mixed, nstart = 5, lambda = lambda_val)
groupes_ref_kproto <- kproto_ref$cluster

# B. Modèle Custom (votre package)
# Assurez-vous d'utiliser le même lambda.
set.seed(42)
kproto_custom_obj <- Kprototypes$new(k = k_mixed, lambda = lambda_val) #
kproto_custom_obj$fit(data_mixed)
groupes_custom_kproto <- kproto_custom_obj$Groupes

# C. Comparaison des Partitions
comparer_partitions(groupes_custom_kproto, groupes_ref_kproto, "K-prototypes")

# D. Comparaison du Coût (tot.withinss dans kproto)
cout_ref_kproto <- kproto_ref$tot.withinss
# Assurez-vous que la méthode $inertie() de Kprototypes renvoie un élément approprié.
cout_custom_kproto <- kproto_custom_obj$inertie()$totale # Supposons $totale est le coût

cat("\n*** Comparaison du Coût Total (Totale Within Distance) ***\n")
cat(paste("  Coût Référence (clustMixType::kproto):", round(cout_ref_kproto, 4), "\n"))
cat(paste("  Coût Custom (Votre Kprototypes):", round(cout_custom_kproto, 4), "\n"))
if (abs(cout_ref_kproto - cout_custom_kproto) < 1e-6) {
  cat("  RÉSULTAT: ✅ COÛT IDENTIQUE\n")
} else {
  cat("  RÉSULTAT: ⚠️ COÛT DIFFÉRENT. Vérifiez le calcul de la distance mixte (lambda).\n")
}


# =======================================================
# --- 6. VÉRIFICATION CLUSTERING DE VARIABLES (ClustOfVar) ---
# =======================================================

cat("\n=======================================================\n")
cat("VÉRIFICATION CLUSTERING DE VARIABLES (ClustOfVar)\n")
cat("=======================================================\n")

# A. Modèle de référence (ClustOfVar::kmeansvar ou ClustOfVar::hclustvar)
# Utiliser hclustvar (approche hiérarchique plus facile à comparer)
data_cov <- data_mixed # On utilise les données mixtes
cov_ref <- hclustvar(data.frame(data_cov))
# Couper l'arbre pour obtenir les partitions
groupes_ref_cov <- cutreevar(cov_ref, k = 3)$cluster

# B. Modèle Custom (votre package)
# On suppose que vous utilisez l'approche itérative (type kmeansvar) ou l'approche hclustvar
# L'approche dans ClustOfVar_class.R semble être itérative (K-means de variables).
k_cov <- 3
set.seed(42) # L'approche itérative nécessite un seed si l'initialisation est aléatoire
cov_custom_obj <- ClustOfVar$new(k = k_cov) #
cov_custom_obj$fit(data_cov)
groupes_custom_cov <- cov_custom_obj$Groupes # Cette fois, Groupes est un vecteur de cluster pour les VARIABLES (colonnes)

# C. Comparaison des Partitions (des VARIABLES)
# Les noms de colonnes sont les "individus" à clusteriser.
groupes_ref_cov_df <- data.frame(Variable = names(groupes_ref_cov), Cluster_Ref = groupes_ref_cov)
groupes_custom_cov_df <- data.frame(Variable = names(groupes_custom_cov), Cluster_Custom = groupes_custom_cov)

comparaison_cov <- merge(groupes_ref_cov_df, groupes_custom_cov_df, by = "Variable")

cat("\n*** Partitions des Variables ***\n")
print(comparaison_cov)

ari_cov <- adjustedRandIndex(comparaison_cov$Cluster_Custom, comparaison_cov$Cluster_Ref)
cat(paste("\n  Adjusted Rand Index (ARI) des Variables:", round(ari_cov, 4), "\n"))

if (ari_cov > 0.95) {
  cat("  RÉSULTAT: ✅ SIMILAIRE (ARI > 0.95)\n")
} else if (ari_cov > 0.8) {
  cat("  RÉSULTAT: ⚠️ ACCEPTABLE (ARI > 0.8)\n")
} else {
  cat("  RÉSULTAT: ❌ DIFFÉRENT. Le clustering de variables est très spécifique.\n")
}

# D. Comparaison de l'Homogénéité (Critère de Qualité)
# La mesure de qualité dans ClustOfVar est l'homogénéité (moyenne des corrélations).
# La méthode $Homogeneite dans votre classe devrait renvoyer la même chose.
homogeneite_ref <- summary(cov_ref)$E$E.rel
homogeneite_custom <- cov_custom_obj$Homogeneite #

cat("\n*** Comparaison de l'Homogénéité (Qualité du Clustering) ***\n")
cat(paste("  Homogénéité Référence (ClustOfVar):", round(homogeneite_ref, 4), "\n"))
cat(paste("  Homogénéité Custom (Votre ClustOfVar):", round(homogeneite_custom, 4), "\n"))
if (abs(homogeneite_ref - homogeneite_custom) < 1e-6) {
  cat("  RÉSULTAT: ✅ HOMOGÉNÉITÉ IDENTIQUE\n")
} else {
  cat("  RÉSULTAT: ❌ HOMOGÉNÉITÉ DIFFÉRENTE. Vérifiez le calcul de la variable synthétique.\n")
}