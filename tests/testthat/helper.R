# ============================================================================
# FONCTIONS UTILITAIRES POUR LES TESTS - RollerClustR
# ============================================================================
# Ce fichier contient des fonctions helper utilisées dans les tests

# ============================================================================
# GÉNÉRATION DE DONNÉES DE TEST
# ============================================================================

#' Générer des données de test numériques
#' 
#' @param n_obs Nombre d'observations
#' @param n_vars Nombre de variables
#' @param seed Graine aléatoire pour reproductibilité
#' @return Data frame avec données numériques
generate_numeric_data <- function(n_obs = 100, n_vars = 10, seed = 123) {
  set.seed(seed)
  as.data.frame(matrix(rnorm(n_obs * n_vars), ncol = n_vars))
}

#' Générer des données avec structure de corrélation
#' 
#' @param n_obs Nombre d'observations
#' @param n_clusters Nombre de groupes de variables corrélées
#' @param vars_per_cluster Variables par cluster
#' @param seed Graine aléatoire
#' @return Data frame avec structure de corrélation
generate_correlated_data <- function(n_obs = 100, n_clusters = 3, 
                                     vars_per_cluster = 3, seed = 123) {
  set.seed(seed)
  data_list <- list()
  
  for (k in 1:n_clusters) {
    # Variable de base pour le cluster
    base_var <- rnorm(n_obs)
    
    # Créer vars_per_cluster variables corrélées
    cluster_data <- matrix(NA, nrow = n_obs, ncol = vars_per_cluster)
    for (j in 1:vars_per_cluster) {
      cluster_data[, j] <- base_var + rnorm(n_obs, 0, 0.3)
    }
    
    colnames(cluster_data) <- paste0("C", k, "V", 1:vars_per_cluster)
    data_list[[k]] <- cluster_data
  }
  
  as.data.frame(do.call(cbind, data_list))
}

#' Générer des données catégorielles
#' 
#' @param n_obs Nombre d'observations
#' @param n_vars Nombre de variables
#' @param n_levels Nombre de niveaux par variable
#' @param seed Graine aléatoire
#' @return Data frame avec données catégorielles
generate_categorical_data <- function(n_obs = 100, n_vars = 5, 
                                      n_levels = 3, seed = 123) {
  set.seed(seed)
  data <- as.data.frame(lapply(1:n_vars, function(i) {
    factor(sample(LETTERS[1:n_levels], n_obs, replace = TRUE))
  }))
  names(data) <- paste0("Var", 1:n_vars)
  data
}

#' Générer des données mixtes (numériques + catégorielles)
#' 
#' @param n_obs Nombre d'observations
#' @param n_num Nombre de variables numériques
#' @param n_cat Nombre de variables catégorielles
#' @param seed Graine aléatoire
#' @return Data frame avec données mixtes
generate_mixed_data <- function(n_obs = 100, n_num = 3, n_cat = 3, seed = 123) {
  set.seed(seed)
  
  # Numériques
  num_data <- generate_numeric_data(n_obs, n_num, seed)
  names(num_data) <- paste0("Num", 1:n_num)
  
  # Catégorielles
  cat_data <- generate_categorical_data(n_obs, n_cat, 3, seed)
  names(cat_data) <- paste0("Cat", 1:n_cat)
  
  cbind(num_data, cat_data)
}

# ============================================================================
# FONCTIONS DE COMPARAISON
# ============================================================================

#' Comparer deux partitions (clustering)
#' 
#' @param groups1 Premier vecteur de groupes
#' @param groups2 Deuxième vecteur de groupes
#' @return Taux d'accord (0 à 1)
compare_partitions <- function(groups1, groups2) {
  if (length(groups1) != length(groups2)) {
    stop("Les deux partitions doivent avoir la même longueur")
  }
  
  n <- length(groups1)
  same_cluster_g1 <- outer(groups1, groups1, "==")
  same_cluster_g2 <- outer(groups2, groups2, "==")
  
  agreement <- sum(same_cluster_g1 == same_cluster_g2) / (n * n)
  return(agreement)
}

#' Vérifier si deux clusterings sont équivalents à permutation près
#' 
#' @param groups1 Premier vecteur de groupes
#' @param groups2 Deuxième vecteur de groupes
#' @return TRUE si équivalents
are_clusterings_equivalent <- function(groups1, groups2) {
  compare_partitions(groups1, groups2) > 0.99
}

# ============================================================================
# FONCTIONS DE VALIDATION
# ============================================================================

#' Calculer la corrélation intra-cluster moyenne
#' 
#' @param data Data frame de données
#' @param groups Vecteur de groupes
#' @param cluster_id ID du cluster à analyser
#' @return Corrélation moyenne intra-cluster
intra_cluster_correlation <- function(data, groups, cluster_id) {
  vars_in_cluster <- names(groups)[groups == cluster_id]
  
  if (length(vars_in_cluster) < 2) {
    return(NA)
  }
  
  cor_matrix <- abs(cor(data[, vars_in_cluster], use = "pairwise.complete.obs"))
  mean(cor_matrix[upper.tri(cor_matrix)])
}

#' Calculer la corrélation inter-cluster moyenne
#' 
#' @param data Data frame de données
#' @param groups Vecteur de groupes
#' @param cluster_id1 ID du premier cluster
#' @param cluster_id2 ID du deuxième cluster
#' @return Corrélation moyenne inter-cluster
inter_cluster_correlation <- function(data, groups, cluster_id1, cluster_id2) {
  vars_in_c1 <- names(groups)[groups == cluster_id1]
  vars_in_c2 <- names(groups)[groups == cluster_id2]
  
  if (length(vars_in_c1) == 0 || length(vars_in_c2) == 0) {
    return(NA)
  }
  
  cor_matrix <- abs(cor(data[, c(vars_in_c1, vars_in_c2)], 
                       use = "pairwise.complete.obs"))
  
  sub_matrix <- cor_matrix[vars_in_c1, vars_in_c2, drop = FALSE]
  mean(sub_matrix)
}

#' Vérifier le principe de séparation (intra > inter)
#' 
#' @param data Data frame de données
#' @param groups Vecteur de groupes
#' @return TRUE si le principe est respecté
check_separation_principle <- function(data, groups) {
  unique_groups <- unique(groups)
  K <- length(unique_groups)
  
  if (K < 2) return(TRUE)  # Pas de comparaison possible
  
  # Calculer corrélations intra
  intra_cors <- sapply(unique_groups, function(k) {
    intra_cluster_correlation(data, groups, k)
  })
  intra_cors <- intra_cors[!is.na(intra_cors)]
  
  # Calculer corrélations inter
  inter_cors <- c()
  for (i in 1:(K-1)) {
    for (j in (i+1):K) {
      cor_ij <- inter_cluster_correlation(data, groups, 
                                          unique_groups[i], unique_groups[j])
      if (!is.na(cor_ij)) {
        inter_cors <- c(inter_cors, cor_ij)
      }
    }
  }
  
  # Principe : moyenne intra > moyenne inter
  if (length(intra_cors) > 0 && length(inter_cors) > 0) {
    return(mean(intra_cors) > mean(inter_cors))
  }
  
  return(TRUE)
}

# ============================================================================
# FONCTIONS DE MESURE
# ============================================================================

#' Mesurer le temps d'exécution
#' 
#' @param expr Expression à évaluer
#' @return Temps en secondes
measure_time <- function(expr) {
  system.time(expr)[3]
}

#' Mesurer l'utilisation mémoire
#' 
#' @param expr Expression à évaluer
#' @return Différence de mémoire en MB
measure_memory <- function(expr) {
  gc()
  mem_before <- sum(gc()[, 2])
  
  eval(expr)
  
  gc()
  mem_after <- sum(gc()[, 2])
  
  mem_after - mem_before
}

# ============================================================================
# FONCTIONS DE VÉRIFICATION
# ============================================================================

#' Vérifier qu'un objet est un modèle RollerClustR valide
#' 
#' @param model Objet modèle
#' @param expected_class Classe attendue
#' @return TRUE si valide
is_valid_model <- function(model, expected_class = NULL) {
  checks <- c(
    "fit" %in% names(model),
    "summary" %in% names(model),
    "K" %in% names(model),
    "Groupes" %in% names(model)
  )
  
  if (!is.null(expected_class)) {
    checks <- c(checks, inherits(model, expected_class))
  }
  
  all(checks)
}

#' Vérifier qu'un vecteur de groupes est valide
#' 
#' @param groups Vecteur de groupes
#' @param K Nombre attendu de clusters
#' @param n_vars Nombre attendu de variables
#' @return TRUE si valide
is_valid_grouping <- function(groups, K = NULL, n_vars = NULL) {
  checks <- c(
    is.integer(groups) || is.numeric(groups),
    all(!is.na(groups)),
    all(groups > 0),
    length(groups) > 0
  )
  
  if (!is.null(K)) {
    checks <- c(checks, length(unique(groups)) == K)
  }
  
  if (!is.null(n_vars)) {
    checks <- c(checks, length(groups) == n_vars)
  }
  
  all(checks)
}

# ============================================================================
# DATASETS DE TEST PRÉCHARGÉS
# ============================================================================

#' Obtenir un dataset de test standard
#' 
#' @param type Type de dataset ("small", "medium", "large", "correlated")
#' @return Data frame de test
get_test_dataset <- function(type = "small") {
  switch(type,
    small = generate_numeric_data(50, 5, 123),
    medium = generate_numeric_data(200, 10, 123),
    large = generate_numeric_data(1000, 20, 123),
    correlated = generate_correlated_data(100, 3, 3, 123),
    mixed = generate_mixed_data(100, 3, 3, 123),
    categorical = generate_categorical_data(100, 5, 3, 123),
    stop("Type de dataset inconnu")
  )
}

# ============================================================================
# FONCTIONS DE SKIP CONDITIONNELS
# ============================================================================

#' Skip test si KmodesVarClust n'est pas disponible
skip_if_no_kmodes <- function() {
  if (!exists("KmodesVarClust")) {
    testthat::skip("KmodesVarClust non disponible")
  }
}

#' Skip test si exécution trop lente
#' 
#' @param threshold Seuil en secondes
skip_if_slow <- function(threshold = 30) {
  if (Sys.getenv("SKIP_SLOW_TESTS") == "TRUE") {
    testthat::skip("Test lent skippé (SKIP_SLOW_TESTS=TRUE)")
  }
}

#' Skip test sur CRAN
skip_on_cran_extended <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "false")) {
    testthat::skip("Test étendu skippé sur CRAN")
  }
}

# ============================================================================
# MESSAGE DE CHARGEMENT
# ============================================================================

# Afficher un message lors du chargement des helpers
if (interactive()) {
  message("✓ Fonctions helper chargées pour les tests RollerClustR")
}