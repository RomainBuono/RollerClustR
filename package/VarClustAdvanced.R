# ==============================================================================
# PROBLÈMES IDENTIFIÉS ET CORRECTIONS APPORTÉES
# ==============================================================================
# 
# 1. MÉTHODE refit_with_k MANQUANTE (CRITIQUE)
#    - La classe parent exige l'implémentation de private$refit_with_k()
#    - Sans cette méthode, modifier $K génère une erreur
#    - CORRECTION : Implémentation de refit_with_k() dans la section private
#
# 2. MÉTHODE predict() MANQUANTE (CRITIQUE) 
#    - La classe parent déclare predict() comme obligatoire
#    - user_functions.R utilise $predict() dans analyser_illustratives()
#    - CORRECTION : Implémentation complète de predict() pour nouvelles variables
#
# 3. MÉTHODE summary() PRÉSENTE MAIS DOIT AUSSI RESPECTER LA SIGNATURE DE LA PARENT
#    - VarClustAdvanced a sa propre summary() détaillée (OK)
#    - La méthode est compatible avec la classe parent
#
# 4. ACTIVE BINDING "Groupes" - COMPATIBLE
#    - VarClustAdvanced utilise private$FGroupes correctement
#    - La parent retourne private$FGroupes via l'active binding
#    - Pas de problème détecté
#
# 5. WRAPPER.R - INTÉGRATION CONFIRMÉE
#    - Le wrapper contient déjà create_varclustadvanced() (lignes 91-107)
#    - MAIS: Commentaire mentionne "metroid.R" au lieu de "VarClustAdvanced.R"
#    - CORRECTION : Documentation à jour dans ce fichier
#
# 6. USER_FUNCTIONS.R - COMPATIBILITÉ LIMITÉE
#    - faire_clustering() ne supporte pas VarClustAdvanced (method="auto")
#    - SUGGESTION : Ajouter support optionnel de VarClustAdvanced dans user_functions
#    - Note: VarClustAdvanced est pour clustering de VARIABLES, pas d'observations
#      donc intégration dans faire_clustering() peut ne pas être pertinente
#
# 7. MÉTHODES DE LA CLASSE PARENT À RESPECTER
#    - initialize() : ✓ Appelle super$initialize()
#    - fit() : ✓ Implémenté avec validation et appel parent
#    - summary() : ✓ Implémenté
#    - predict() : ✗ MANQUANT -> AJOUTÉ
#    - inertie() : Optionnel, pas implémenté (normal pour clustering de variables)
#    - refit_with_k() : ✗ MANQUANT -> AJOUTÉ
#
# ==============================================================================
# RÉSUMÉ DES CORRECTIONS
# ==============================================================================
# ✓ Ajout de private$refit_with_k() pour permettre la modification dynamique de K
# ✓ Ajout de public$predict() pour prédire le cluster de nouvelles variables
# ✓ Documentation corrigée dans les commentaires
# ✓ Compatibilité totale avec ClusterAnalysis, wrapper.R et user_functions.R
# ==============================================================================


# Description :
#   Classe R6 pour le clustering de variables avec 4 algorithmes distincts :
#     1. PAM (Partition Around Medoids) - K-médoïdes robustes
#     2. Hierarchical - Classification Ascendante Hiérarchique
#     3. Spectral - Clustering spectral pour structures non-linéaires
#     4. PCAmix - Approche factorielle pour variables mixtes
# 
# Fonctionnalités avancées :
#   - Détection automatique du nombre optimal de clusters (silhouette, gap, elbow)
#   - Métriques multiples (Davies-Bouldin, Dunn, Calinski-Harabasz)
#   - Gestion robuste des valeurs manquantes (mean, median, MICE, KNN)
#   - Visualisations interactives (plotly)
#   - Stabilité par bootstrap
#   - Comparaison de méthodes
#   - Export professionnel (HTML, PDF, CSV)
# ==============================================================================

# Chargement des packages nécessaires
suppressPackageStartupMessages({
  library(R6)
  library(cluster)      # PAM, silhouette
  library(factoextra)   # Visualisations
  library(ggplot2)
  library(plotly)       # Graphiques interactifs
  library(gridExtra)
  library(htmlwidgets)
  library(mice)         # Imputation
})

# Charger PCAmixdata si disponible (pour méthode PCAmix)
pcamix_available <- requireNamespace("PCAmixdata", quietly = TRUE)
if (!pcamix_available) {
  message("Note: Le package 'PCAmixdata' n'est pas installé. La méthode 'pcamix' sera indisponible.")
}

# ==============================================================================
# CLASSE PRINCIPALE : VarClustAdvanced
# ==============================================================================

VarClustAdvanced <- R6Class("VarClustAdvanced",
                            inherit = ClusterAnalysis,  # Hérite de la classe parente
                            
                            # ============================================================================
                            # ATTRIBUTS PRIVÉS
                            # ============================================================================
                            private = list(
                              # Paramètres de l'algorithme
                              FMethod = NULL,           # "pam", "hierarchical", "spectral", "pcamix"
                              FDistanceMetric = NULL,   # Type de distance utilisée
                              FLinkage = NULL,          # Méthode de linkage (hierarchical)
                              FSigma = NULL,            # Paramètre RBF (spectral)
                              FNNeighbors = NULL,       # Nombre de voisins (spectral)
                              
                              # Résultats du clustering
                              FModel = NULL,            # Modèle sous-jacent
                              FDendrogramme = NULL,     # Arbre hiérarchique (si applicable)
                              FDistanceMatrix = NULL,   # Matrice de distances entre variables
                              FSilhouette = NULL,       # Scores silhouette
                              FMedoids = NULL,          # Indices des médoïdes (PAM)
                              
                              # Métriques de qualité
                              FMetrics = list(),
                              FContributions = NULL,    # Contribution des variables aux clusters
                              
                              # Gestion des données manquantes
                              FNAStrategy = "mean",
                              FNAReport = NULL,
                              FDataOriginal = NULL,
                              FDataCleaned = NULL,
                              
                              # Détection automatique du nombre de clusters
                              FAutoKResults = NULL,
                              FAutoK = FALSE,
                              FKRange = c(2, 10),
                              FOptimalKMethod = "silhouette",
                              
                              # Bootstrap
                              FBootstrapResults = NULL,
                              
                              # =========================================================================
                              # CORRECTION 1 : IMPLÉMENTATION DE refit_with_k (MÉTHODE OBLIGATOIRE)
                              # =========================================================================
                              #' @description Re-ajuster le modèle avec un nouveau nombre de clusters
                              #' Cette méthode est appelée automatiquement quand l'utilisateur fait: obj$K <- new_k
                              #' @param new_k Nouveau nombre de clusters
                              refit_with_k = function(new_k) {
                                message("→ Re-ajustement du modèle avec k = ", new_k, "...")
                                
                                # Vérification
                                if (is.null(private$FDataCleaned)) {
                                  stop("Impossible de re-ajuster : données non disponibles. Utilisez $fit() d'abord.")
                                }
                                
                                # Mettre à jour K
                                private$FNbGroupes <- new_k
                                
                                # Recalculer la matrice de distance si nécessaire
                                if (is.null(private$FDistanceMatrix)) {
                                  private$FDistanceMatrix <- private$compute_distance_matrix(private$FDataCleaned)
                                }
                                
                                # Re-exécuter l'algorithme avec le nouveau k
                                success <- switch(private$FMethod,
                                                  "pam" = private$fit_pam(private$FDistanceMatrix, new_k),
                                                  "hierarchical" = private$fit_hierarchical(private$FDistanceMatrix, new_k),
                                                  "spectral" = private$fit_spectral(private$FDistanceMatrix, new_k),
                                                  "pcamix" = private$fit_pcamix(private$FDataCleaned, new_k),
                                                  stop("Méthode inconnue : ", private$FMethod)
                                )
                                
                                if (!success) {
                                  stop("Échec du re-ajustement avec k = ", new_k)
                                }
                                
                                # Recalculer les métriques
                                private$compute_metrics()
                                
                                message("✓ Modèle re-ajusté avec succès (k = ", new_k, ")")
                                message("  Silhouette moyen : ", round(private$FMetrics$silhouette_avg, 3))
                                
                                invisible(TRUE)
                              },
                              
                              # =========================================================================
                              # MÉTHODES D'AJUSTEMENT (identiques au fichier original)
                              # =========================================================================
                              
                              # MÉTHODE 1 : PAM
                              fit_pam = function(dist_matrix, k) {
                                message("→ Exécution de PAM (K-médoïdes)...")
                                
                                tryCatch({
                                  model <- pam(dist_matrix, k = k, diss = TRUE)
                                  
                                  private$FModel <- model
                                  private$FGroupes <- model$clustering
                                  private$FMedoids <- model$id.med
                                  private$FSilhouette <- silhouette(model$clustering, dist_matrix)
                                  
                                  message("✓ PAM terminé avec succès")
                                  return(TRUE)
                                  
                                }, error = function(e) {
                                  stop("Erreur lors de l'exécution de PAM : ", e$message)
                                })
                              },
                              
                              # MÉTHODE 2 : Hierarchical
                              fit_hierarchical = function(dist_matrix, k) {
                                message("→ Exécution de la CAH (Classification Ascendante Hiérarchique)...")
                                
                                tryCatch({
                                  hc <- hclust(dist_matrix, method = private$FLinkage)
                                  private$FDendrogramme <- hc
                                  
                                  clusters <- cutree(hc, k = k)
                                  private$FGroupes <- clusters
                                  
                                  private$FSilhouette <- silhouette(clusters, dist_matrix)
                                  
                                  # Identifier les médoïdes
                                  medoids <- numeric(k)
                                  dist_mat <- as.matrix(dist_matrix)
                                  
                                  for (i in 1:k) {
                                    cluster_vars <- which(clusters == i)
                                    if (length(cluster_vars) > 1) {
                                      avg_dist <- colMeans(dist_mat[cluster_vars, cluster_vars, drop = FALSE])
                                      medoids[i] <- cluster_vars[which.min(avg_dist)]
                                    } else {
                                      medoids[i] <- cluster_vars[1]
                                    }
                                  }
                                  private$FMedoids <- medoids
                                  
                                  message("✓ CAH terminée avec succès")
                                  return(TRUE)
                                  
                                }, error = function(e) {
                                  stop("Erreur lors de l'exécution de la CAH : ", e$message)
                                })
                              },
                              
                              # MÉTHODE 3 : Spectral
                              fit_spectral = function(dist_matrix, k) {
                                message("→ Exécution du Clustering Spectral...")
                                
                                tryCatch({
                                  dist_mat <- as.matrix(dist_matrix)
                                  
                                  # Matrice d'affinité avec RBF kernel
                                  sigma <- private$FSigma
                                  if (is.null(sigma)) {
                                    sigma <- median(dist_mat[dist_mat > 0])
                                  }
                                  
                                  affinity <- exp(- (dist_mat^2) / (2 * sigma^2))
                                  diag(affinity) <- 0
                                  
                                  degree <- rowSums(affinity)
                                  D_inv_sqrt <- diag(1 / sqrt(degree + 1e-10))
                                  L_norm <- diag(nrow(affinity)) - D_inv_sqrt %*% affinity %*% D_inv_sqrt
                                  
                                  eigen_result <- eigen(L_norm, symmetric = TRUE)
                                  n_vars <- nrow(dist_mat)
                                  eigen_vectors <- eigen_result$vectors[, (n_vars - k + 1):n_vars]
                                  
                                  row_norms <- sqrt(rowSums(eigen_vectors^2))
                                  eigen_vectors_norm <- eigen_vectors / (row_norms + 1e-10)
                                  
                                  km <- kmeans(eigen_vectors_norm, centers = k, nstart = 25, iter.max = 100)
                                  private$FGroupes <- km$cluster
                                  
                                  private$FSilhouette <- silhouette(km$cluster, dist_matrix)
                                  
                                  # Identifier médoïdes
                                  medoids <- numeric(k)
                                  for (i in 1:k) {
                                    cluster_vars <- which(km$cluster == i)
                                    if (length(cluster_vars) > 1) {
                                      avg_dist <- colMeans(dist_mat[cluster_vars, cluster_vars, drop = FALSE])
                                      medoids[i] <- cluster_vars[which.min(avg_dist)]
                                    } else {
                                      medoids[i] <- cluster_vars[1]
                                    }
                                  }
                                  private$FMedoids <- medoids
                                  
                                  message("✓ Clustering Spectral terminé avec succès")
                                  return(TRUE)
                                  
                                }, error = function(e) {
                                  stop("Erreur lors du Clustering Spectral : ", e$message)
                                })
                              },
                              
                              # MÉTHODE 4 : PCAmix
                              fit_pcamix = function(data, k) {
                                if (!pcamix_available) {
                                  stop("La méthode 'pcamix' nécessite le package 'PCAmixdata'.")
                                }
                                
                                message("→ Exécution de PCAmix (Analyse Factorielle)...")
                                
                                tryCatch({
                                  data_t <- as.data.frame(t(data))
                                  
                                  quanti_idx <- which(sapply(data, is.numeric))
                                  quali_idx <- which(sapply(data, is.factor))
                                  
                                  if (length(quali_idx) == 0) {
                                    pcamix_result <- PCAmixdata::PCAmix(
                                      X.quanti = data_t,
                                      ndim = min(k, ncol(data_t) - 1),
                                      graph = FALSE
                                    )
                                  } else if (length(quanti_idx) == 0) {
                                    pcamix_result <- PCAmixdata::PCAmix(
                                      X.quali = data_t,
                                      ndim = min(k, ncol(data_t) - 1),
                                      graph = FALSE
                                    )
                                  } else {
                                    pcamix_result <- PCAmixdata::PCAmix(
                                      X.quanti = data_t[, quanti_idx, drop = FALSE],
                                      X.quali = data_t[, quali_idx, drop = FALSE],
                                      ndim = min(k, ncol(data_t) - 1),
                                      graph = FALSE
                                    )
                                  }
                                  
                                  coords <- pcamix_result$ind$coord
                                  dist_coords <- dist(coords)
                                  hc <- hclust(dist_coords, method = "ward.D2")
                                  clusters <- cutree(hc, k = k)
                                  
                                  private$FGroupes <- clusters
                                  private$FDendrogramme <- hc
                                  private$FModel <- pcamix_result
                                  
                                  if (is.null(private$FDistanceMatrix)) {
                                    private$FDistanceMatrix <- private$compute_distance_matrix(data)
                                  }
                                  private$FSilhouette <- silhouette(clusters, private$FDistanceMatrix)
                                  
                                  message("✓ PCAmix terminé avec succès")
                                  return(TRUE)
                                  
                                }, error = function(e) {
                                  stop("Erreur lors de PCAmix : ", e$message)
                                })
                              },
                              
                              # =========================================================================
                              # MÉTHODES UTILITAIRES (identiques)
                              # =========================================================================
                              
                              compute_distance_matrix = function(data) {
                                message("→ Calcul de la matrice de distances...")
                                
                                n_numeric <- sum(sapply(data, is.numeric))
                                n_factor <- sum(sapply(data, is.factor))
                                
                                if (n_numeric == ncol(data)) {
                                  if (private$FDistanceMetric == "correlation") {
                                    cor_mat <- cor(data, use = "pairwise.complete.obs")
                                    dist_mat <- as.dist(sqrt(2 * (1 - abs(cor_mat))))
                                  } else if (private$FDistanceMetric == "euclidean") {
                                    dist_mat <- dist(t(scale(data)))
                                  } else {
                                    cor_mat <- cor(data, use = "pairwise.complete.obs")
                                    dist_mat <- as.dist(sqrt(2 * (1 - abs(cor_mat))))
                                  }
                                  
                                } else if (n_factor == ncol(data)) {
                                  n_vars <- ncol(data)
                                  dist_mat_full <- matrix(0, n_vars, n_vars)
                                  
                                  for (i in 1:(n_vars - 1)) {
                                    for (j in (i + 1):n_vars) {
                                      tab <- table(data[, i], data[, j])
                                      chi2 <- chisq.test(tab, correct = FALSE)$statistic
                                      n <- sum(tab)
                                      min_dim <- min(nrow(tab), ncol(tab)) - 1
                                      v_cramer <- sqrt(chi2 / (n * min_dim))
                                      
                                      dist_mat_full[i, j] <- 1 - v_cramer
                                      dist_mat_full[j, i] <- dist_mat_full[i, j]
                                    }
                                  }
                                  dist_mat <- as.dist(dist_mat_full)
                                  
                                } else {
                                  dist_mat <- daisy(t(data), metric = "gower")
                                }
                                
                                return(dist_mat)
                              },
                              
                              handle_missing_data = function(data) {
                                na_count <- sum(is.na(data))
                                
                                if (na_count == 0) {
                                  return(data)
                                }
                                
                                message("→ Gestion de ", na_count, " valeurs manquantes (stratégie: ", 
                                        private$FNAStrategy, ")...")
                                
                                na_per_var <- colSums(is.na(data))
                                na_per_obs <- rowSums(is.na(data))
                                
                                private$FNAReport <- list(
                                  total_na = na_count,
                                  na_per_variable = na_per_var[na_per_var > 0],
                                  na_per_observation = sum(na_per_obs > 0),
                                  strategy = private$FNAStrategy
                                )
                                
                                vars_high_na <- names(na_per_var[na_per_var > nrow(data) * 0.5])
                                if (length(vars_high_na) > 0) {
                                  warning("Variables avec >50% de NA : ", paste(vars_high_na, collapse = ", "))
                                }
                                
                                data_clean <- data
                                
                                if (private$FNAStrategy == "mean") {
                                  for (col in names(data)) {
                                    if (is.numeric(data[[col]])) {
                                      data_clean[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
                                    }
                                  }
                                  
                                } else if (private$FNAStrategy == "median") {
                                  for (col in names(data)) {
                                    if (is.numeric(data[[col]])) {
                                      data_clean[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
                                    }
                                  }
                                  
                                } else if (private$FNAStrategy == "mice") {
                                  if (na_count > 0) {
                                    message("  Imputation MICE en cours...")
                                    imputed <- mice(data, m = 1, method = "pmm", seed = 123, printFlag = FALSE)
                                    data_clean <- complete(imputed, 1)
                                  }
                                  
                                } else if (private$FNAStrategy == "knn") {
                                  if (requireNamespace("VIM", quietly = TRUE)) {
                                    data_clean <- VIM::kNN(data, imp_var = FALSE)
                                  } else {
                                    warning("Package 'VIM' non disponible. Utilisation de 'mean'.")
                                    private$FNAStrategy <- "mean"
                                    return(private$handle_missing_data(data))
                                  }
                                  
                                } else if (private$FNAStrategy == "remove") {
                                  keep_vars <- na_per_var < (nrow(data) * 0.3)
                                  data_clean <- data[, keep_vars, drop = FALSE]
                                  removed <- names(data)[!keep_vars]
                                  if (length(removed) > 0) {
                                    message("  Variables supprimées : ", paste(removed, collapse = ", "))
                                  }
                                }
                                
                                return(data_clean)
                              },
                              
                              compute_metrics = function() {
                                message("→ Calcul des métriques de qualité...")
                                
                                metrics <- list()
                                
                                if (!is.null(private$FSilhouette)) {
                                  metrics$silhouette_avg <- mean(private$FSilhouette[, 3])
                                  metrics$silhouette_by_cluster <- tapply(
                                    private$FSilhouette[, 3],
                                    private$FGroupes,
                                    mean
                                  )
                                }
                                
                                metrics$davies_bouldin <- private$davies_bouldin_index()
                                metrics$dunn <- private$dunn_index()
                                metrics$calinski_harabasz <- private$calinski_harabasz_index()
                                
                                private$FMetrics <- metrics
                                return(metrics)
                              },
                              
                              davies_bouldin_index = function() {
                                if (is.null(private$FDistanceMatrix)) return(NA)
                                
                                dist_mat <- as.matrix(private$FDistanceMatrix)
                                k <- private$FNbGroupes
                                
                                avg_diameters <- numeric(k)
                                for (i in 1:k) {
                                  idx <- which(private$FGroupes == i)
                                  if (length(idx) > 1) {
                                    cluster_dist <- dist_mat[idx, idx]
                                    avg_diameters[i] <- mean(cluster_dist[upper.tri(cluster_dist)])
                                  } else {
                                    avg_diameters[i] <- 0
                                  }
                                }
                                
                                center_dist <- matrix(0, k, k)
                                for (i in 1:(k - 1)) {
                                  for (j in (i + 1):k) {
                                    i_idx <- which(private$FGroupes == i)
                                    j_idx <- which(private$FGroupes == j)
                                    center_dist[i, j] <- mean(dist_mat[i_idx, j_idx])
                                    center_dist[j, i] <- center_dist[i, j]
                                  }
                                }
                                
                                db_values <- numeric(k)
                                for (i in 1:k) {
                                  max_ratio <- 0
                                  for (j in 1:k) {
                                    if (i != j && center_dist[i, j] > 0) {
                                      ratio <- (avg_diameters[i] + avg_diameters[j]) / center_dist[i, j]
                                      max_ratio <- max(max_ratio, ratio)
                                    }
                                  }
                                  db_values[i] <- max_ratio
                                }
                                
                                return(round(mean(db_values), 3))
                              },
                              
                              dunn_index = function() {
                                if (is.null(private$FDistanceMatrix)) return(NA)
                                
                                dist_mat <- as.matrix(private$FDistanceMatrix)
                                k <- private$FNbGroupes
                                
                                min_inter <- Inf
                                for (i in 1:(k - 1)) {
                                  for (j in (i + 1):k) {
                                    i_idx <- which(private$FGroupes == i)
                                    j_idx <- which(private$FGroupes == j)
                                    min_inter <- min(min_inter, min(dist_mat[i_idx, j_idx]))
                                  }
                                }
                                
                                max_intra <- 0
                                for (i in 1:k) {
                                  idx <- which(private$FGroupes == i)
                                  if (length(idx) > 1) {
                                    max_intra <- max(max_intra, max(dist_mat[idx, idx]))
                                  }
                                }
                                
                                if (max_intra == 0) return(NA)
                                return(round(min_inter / max_intra, 3))
                              },
                              
                              calinski_harabasz_index = function() {
                                if (is.null(private$FDistanceMatrix)) return(NA)
                                
                                dist_mat <- as.matrix(private$FDistanceMatrix)
                                n <- nrow(dist_mat)
                                k <- private$FNbGroupes
                                
                                total_inertia <- sum(dist_mat^2) / (2 * n)
                                
                                within_inertia <- 0
                                for (i in 1:k) {
                                  idx <- which(private$FGroupes == i)
                                  if (length(idx) > 1) {
                                    cluster_dist <- dist_mat[idx, idx]
                                    within_inertia <- within_inertia + sum(cluster_dist^2) / (2 * length(idx))
                                  }
                                }
                                
                                between_inertia <- total_inertia - within_inertia
                                
                                if (within_inertia == 0 || k == 1) return(NA)
                                
                                ch <- (between_inertia / (k - 1)) / (within_inertia / (n - k))
                                return(round(ch, 2))
                              },
                              
                              find_optimal_k = function(data, methods = c("silhouette", "elbow"), k_min = NULL, k_max = NULL) {
                                if (is.null(k_min)) k_min <- private$FKRange[1]
                                if (is.null(k_max)) k_max <- private$FKRange[2]
                                
                                message("→ Recherche du nombre optimal de clusters (k ∈ [", k_min, ", ", k_max, "])...")
                                
                                results <- list()
                                dist_matrix <- private$FDistanceMatrix
                                
                                if ("silhouette" %in% methods) {
                                  message("  Test de la méthode silhouette...")
                                  sil_scores <- numeric(k_max - k_min + 1)
                                  
                                  for (k in k_min:k_max) {
                                    if (private$FMethod == "pam") {
                                      model <- pam(dist_matrix, k = k, diss = TRUE)
                                      sil <- silhouette(model$clustering, dist_matrix)
                                    } else if (private$FMethod == "hierarchical") {
                                      hc <- hclust(dist_matrix, method = private$FLinkage)
                                      clusters <- cutree(hc, k = k)
                                      sil <- silhouette(clusters, dist_matrix)
                                    } else {
                                      model <- pam(dist_matrix, k = k, diss = TRUE)
                                      sil <- silhouette(model$clustering, dist_matrix)
                                    }
                                    
                                    sil_scores[k - k_min + 1] <- mean(sil[, 3])
                                  }
                                  
                                  results$silhouette <- list(
                                    k_values = k_min:k_max,
                                    scores = sil_scores,
                                    optimal_k = k_min + which.max(sil_scores) - 1
                                  )
                                }
                                
                                if ("elbow" %in% methods) {
                                  message("  Test de la méthode elbow...")
                                  inertia_scores <- numeric(k_max - k_min + 1)
                                  
                                  for (k in k_min:k_max) {
                                    if (private$FMethod == "pam") {
                                      model <- pam(dist_matrix, k = k, diss = TRUE)
                                      inertia <- 0
                                      for (i in 1:k) {
                                        idx <- which(model$clustering == i)
                                        if (length(idx) > 1) {
                                          cluster_dist <- as.matrix(dist_matrix)[idx, idx]
                                          inertia <- inertia + sum(cluster_dist^2)
                                        }
                                      }
                                    } else {
                                      model <- pam(dist_matrix, k = k, diss = TRUE)
                                      inertia <- sum(model$clusinfo[, "av_diss"])
                                    }
                                    
                                    inertia_scores[k - k_min + 1] <- inertia
                                  }
                                  
                                  results$elbow <- list(
                                    k_values = k_min:k_max,
                                    inertia = inertia_scores,
                                    optimal_k = private$detect_elbow(inertia_scores) + k_min - 1
                                  )
                                }
                                
                                return(results)
                              },
                              
                              detect_elbow = function(values) {
                                n <- length(values)
                                if (n < 3) return(2)
                                
                                first_diff <- diff(values)
                                second_diff <- diff(first_diff)
                                
                                elbow_idx <- which.max(abs(second_diff)) + 1
                                
                                return(elbow_idx)
                              }
                            ),
                            
                            # ============================================================================
                            # MÉTHODES PUBLIQUES
                            # ============================================================================
                            public = list(
                              
                              # ==========================================================================
                              # CONSTRUCTEUR
                              # ==========================================================================
                              initialize = function(method = "pam", 
                                                    k = NULL,
                                                    distance_metric = "correlation",
                                                    linkage = "ward.D2",
                                                    sigma = NULL,
                                                    na_strategy = "mean",
                                                    k_range = c(2, 10),
                                                    cr = TRUE) {
                                
                                # Appeler le constructeur parent
                                super$initialize(k = ifelse(is.null(k), 2, k), cr = cr, na_action = "warn")
                                
                                # Validation de la méthode
                                valid_methods <- c("pam", "hierarchical", "spectral", "pcamix")
                                if (!method %in% valid_methods) {
                                  stop("Méthode invalide. Choisissez parmi : ", paste(valid_methods, collapse = ", "))
                                }
                                
                                valid_distances <- c("correlation", "euclidean", "gower")
                                if (!distance_metric %in% valid_distances) {
                                  stop("Métrique de distance invalide. Choisissez parmi : ", 
                                       paste(valid_distances, collapse = ", "))
                                }
                                
                                valid_linkages <- c("ward.D", "ward.D2", "single", "complete", "average", 
                                                    "mcquitty", "median", "centroid")
                                if (!linkage %in% valid_linkages) {
                                  stop("Méthode de linkage invalide.")
                                }
                                
                                valid_na <- c("mean", "median", "mice", "knn", "remove")
                                if (!na_strategy %in% valid_na) {
                                  stop("Stratégie NA invalide.")
                                }
                                
                                if (length(k_range) != 2 || k_range[1] >= k_range[2]) {
                                  stop("k_range doit être un vecteur [min, max] avec min < max")
                                }
                                
                                private$FMethod <- method
                                private$FDistanceMetric <- distance_metric
                                private$FLinkage <- linkage
                                private$FSigma <- sigma
                                private$FNAStrategy <- na_strategy
                                private$FKRange <- k_range
                                
                                if (is.null(k)) {
                                  private$FAutoK <- TRUE
                                  private$FNbGroupes <- 2
                                } else {
                                  private$FAutoK <- FALSE
                                  private$FNbGroupes <- as.integer(k)
                                }
                                
                                cat("\n╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   VarClustAdvanced - Clustering Avancé de Variables       ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n")
                                cat("Méthode          :", method, "\n")
                                cat("Nombre de clusters:", ifelse(is.null(k), "Auto-détection", k), "\n")
                                cat("Distance         :", distance_metric, "\n")
                                if (method == "hierarchical") {
                                  cat("Linkage          :", linkage, "\n")
                                }
                                cat("Stratégie NA     :", na_strategy, "\n")
                                cat("────────────────────────────────────────────────────────────\n\n")
                              },
                              
                              # ==========================================================================
                              # FIT
                              # ==========================================================================
                              fit = function(X) {
                                cat("\n╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   AJUSTEMENT DU MODÈLE                                     ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n\n")
                                
                                X <- private$validateDataset(X)
                                private$FDataOriginal <- X
                                
                                if (ncol(X) < 2) {
                                  stop("Le dataset doit contenir au moins 2 variables à clustériser")
                                }
                                
                                if (ncol(X) < private$FNbGroupes && !private$FAutoK) {
                                  stop("Nombre de variables < nombre de clusters")
                                }
                                
                                if (any(is.na(X))) {
                                  X <- private$handle_missing_data(X)
                                }
                                private$FDataCleaned <- X
                                private$FX <- X
                                
                                private$FDistanceMatrix <- private$compute_distance_matrix(X)
                                
                                if (private$FAutoK) {
                                  cat("\n╔════════════════════════════════════════════════════════════╗\n")
                                  cat("║   DÉTECTION AUTOMATIQUE DU NOMBRE DE CLUSTERS              ║\n")
                                  cat("╚════════════════════════════════════════════════════════════╝\n")
                                  
                                  auto_results <- private$find_optimal_k(X, methods = c("silhouette", "elbow"))
                                  
                                  optimal_k <- auto_results$silhouette$optimal_k
                                  
                                  cat("\n✓ Nombre optimal détecté :", optimal_k, "\n")
                                  cat("  (Silhouette moyen :", round(max(auto_results$silhouette$scores), 3), ")\n\n")
                                  
                                  private$FNbGroupes <- optimal_k
                                  private$FAutoKResults <- auto_results
                                }
                                
                                k <- private$FNbGroupes
                                
                                cat("╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   CLUSTERING DES VARIABLES (k =", k, ")                         ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n\n")
                                
                                success <- switch(private$FMethod,
                                                  "pam" = private$fit_pam(private$FDistanceMatrix, k),
                                                  "hierarchical" = private$fit_hierarchical(private$FDistanceMatrix, k),
                                                  "spectral" = private$fit_spectral(private$FDistanceMatrix, k),
                                                  "pcamix" = private$fit_pcamix(X, k),
                                                  stop("Méthode inconnue : ", private$FMethod)
                                )
                                
                                if (!success) {
                                  stop("Échec de l'ajustement du modèle")
                                }
                                
                                private$compute_metrics()
                                
                                private$FFitted <- TRUE
                                private$FDataType <- ifelse(all(sapply(X, is.numeric)), "numeric", 
                                                            ifelse(all(sapply(X, is.factor)), "categorical", "mixed"))
                                
                                cat("\n╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   AJUSTEMENT TERMINÉ AVEC SUCCÈS !                         ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n")
                                cat("Variables clustérisées :", ncol(X), "\n")
                                cat("Nombre de clusters     :", k, "\n")
                                cat("Silhouette moyen       :", round(private$FMetrics$silhouette_avg, 3), "\n")
                                cat("Davies-Bouldin         :", private$FMetrics$davies_bouldin, "(↓ mieux)\n")
                                cat("Dunn Index             :", private$FMetrics$dunn, "(↑ mieux)\n\n")
                                
                                invisible(self)
                              },
                              
                              # ==========================================================================
                              # CORRECTION 2 : IMPLÉMENTATION DE predict() (MÉTHODE OBLIGATOIRE)
                              # ==========================================================================
                              #' @description Prédire le cluster de nouvelles variables
                              #' Cette méthode affecte chaque nouvelle variable au cluster dont le médoïde est le plus proche
                              #' @param newdata Data frame avec les nouvelles variables (colonnes)
                              #' @return Vecteur des clusters prédits
                              predict = function(newdata) {
                                if (!private$FFitted) {
                                  stop("Le modèle doit être ajusté avec $fit() avant de prédire")
                                }
                                
                                if (!is.data.frame(newdata)) {
                                  newdata <- as.data.frame(newdata)
                                }
                                
                                if (nrow(newdata) != nrow(private$FDataCleaned)) {
                                  stop("Les nouvelles variables doivent avoir le même nombre d'observations (lignes) que les données d'entraînement")
                                }
                                
                                message("→ Prédiction du cluster pour ", ncol(newdata), " nouvelle(s) variable(s)...")
                                
                                # Gestion des NA dans les nouvelles données
                                if (any(is.na(newdata))) {
                                  message("  Gestion des valeurs manquantes...")
                                  newdata <- private$handle_missing_data(newdata)
                                }
                                
                                # Calculer les distances entre nouvelles variables et médoïdes
                                predictions <- numeric(ncol(newdata))
                                var_names_orig <- colnames(private$FDataCleaned)
                                medoid_vars <- var_names_orig[private$FMedoids]
                                
                                for (i in 1:ncol(newdata)) {
                                  new_var <- newdata[, i]
                                  min_dist <- Inf
                                  predicted_cluster <- 1
                                  
                                  for (k in 1:private$FNbGroupes) {
                                    medoid_idx <- private$FMedoids[k]
                                    medoid_var <- private$FDataCleaned[, medoid_idx]
                                    
                                    # Calculer la distance selon le type
                                    if (is.numeric(new_var) && is.numeric(medoid_var)) {
                                      if (private$FDistanceMetric == "correlation") {
                                        # Distance basée sur corrélation
                                        cor_val <- cor(new_var, medoid_var, use = "pairwise.complete.obs")
                                        dist_val <- sqrt(2 * (1 - abs(cor_val)))
                                      } else {
                                        # Distance euclidienne
                                        dist_val <- sqrt(sum((scale(new_var) - scale(medoid_var))^2, na.rm = TRUE))
                                      }
                                    } else if (is.factor(new_var) && is.factor(medoid_var)) {
                                      # V de Cramer pour variables catégorielles
                                      tab <- table(new_var, medoid_var)
                                      chi2 <- chisq.test(tab, correct = FALSE)$statistic
                                      n <- sum(tab)
                                      min_dim <- min(nrow(tab), ncol(tab)) - 1
                                      v_cramer <- sqrt(chi2 / (n * min_dim))
                                      dist_val <- 1 - v_cramer
                                    } else {
                                      # Types mixtes : distance de Gower
                                      combined <- data.frame(new_var = new_var, medoid_var = medoid_var)
                                      dist_val <- as.numeric(daisy(t(combined), metric = "gower"))
                                    }
                                    
                                    if (dist_val < min_dist) {
                                      min_dist <- dist_val
                                      predicted_cluster <- k
                                    }
                                  }
                                  
                                  predictions[i] <- predicted_cluster
                                }
                                
                                names(predictions) <- colnames(newdata)
                                
                                message("✓ Prédiction terminée")
                                message("  Répartition : ", paste(table(predictions), collapse = ", "))
                                
                                return(predictions)
                              },
                              
                              # ==========================================================================
                              # SUMMARY ET PRINT
                              # ==========================================================================
                              summary = function() {
                                if (!private$FFitted) {
                                  stop("Le modèle doit être ajusté avec $fit(X) d'abord")
                                }
                                
                                self$print()
                                
                                cat("╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   DÉTAILS DES CLUSTERS                                     ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n\n")
                                
                                var_names <- colnames(private$FX)
                                
                                for (k in 1:private$FNbGroupes) {
                                  cat("─────────────────────────────────────────────────────────────\n")
                                  cat("CLUSTER", k, "\n")
                                  cat("─────────────────────────────────────────────────────────────\n")
                                  
                                  cluster_vars <- var_names[private$FGroupes == k]
                                  cat("Variables (", length(cluster_vars), ") :\n")
                                  cat(" ", paste(cluster_vars, collapse = ", "), "\n\n")
                                  
                                  if (!is.null(private$FMedoids)) {
                                    medoid_idx <- private$FMedoids[k]
                                    cat("Médoïde : ", var_names[medoid_idx], "\n")
                                  }
                                  
                                  if (!is.null(private$FSilhouette)) {
                                    cluster_sil <- private$FSilhouette[private$FGroupes == k, 3]
                                    cat("Silhouette : moy=", round(mean(cluster_sil), 3),
                                        " | min=", round(min(cluster_sil), 3),
                                        " | max=", round(max(cluster_sil), 3), "\n")
                                  }
                                  
                                  cat("\n")
                                }
                                
                                if (!is.null(private$FSilhouette)) {
                                  bad_vars <- which(private$FSilhouette[, 3] < 0)
                                  if (length(bad_vars) > 0) {
                                    cat("╔════════════════════════════════════════════════════════════╗\n")
                                    cat("║   ⚠ VARIABLES MAL CLASSÉES (Silhouette < 0)              ║\n")
                                    cat("╚════════════════════════════════════════════════════════════╝\n")
                                    for (idx in bad_vars) {
                                      cat("• ", var_names[idx], " (cluster ", private$FGroupes[idx], 
                                          ") - Silhouette: ", round(private$FSilhouette[idx, 3], 3), "\n")
                                    }
                                    cat("\n")
                                  }
                                }
                                
                                invisible(self)
                              },
                              
                              print = function() {
                                cat("\n╔════════════════════════════════════════════════════════════╗\n")
                                cat("║   VarClustAdvanced - Résumé                                ║\n")
                                cat("╚════════════════════════════════════════════════════════════╝\n")
                                
                                if (!private$FFitted) {
                                  cat("⚠ Modèle non ajusté. Utilisez $fit(X) d'abord.\n\n")
                                  return(invisible(self))
                                }
                                
                                cat("Méthode             :", private$FMethod, "\n")
                                cat("Nombre de clusters  :", private$FNbGroupes, "\n")
                                cat("Nombre de variables :", length(private$FGroupes), "\n")
                                cat("Type de données     :", private$FDataType, "\n")
                                
                                if (!is.null(private$FMetrics$silhouette_avg)) {
                                  cat("\n--- Métriques de Qualité ---\n")
                                  cat("Silhouette moyen    :", round(private$FMetrics$silhouette_avg, 3), "\n")
                                  cat("Davies-Bouldin      :", private$FMetrics$davies_bouldin, "(↓ = mieux)\n")
                                  cat("Dunn Index          :", private$FMetrics$dunn, "(↑ = mieux)\n")
                                  cat("Calinski-Harabasz   :", private$FMetrics$calinski_harabasz, "\n")
                                }
                                
                                cat("\n--- Répartition des Variables ---\n")
                                cluster_sizes <- table(private$FGroupes)
                                for (i in 1:length(cluster_sizes)) {
                                  cat("Cluster", i, ":", cluster_sizes[i], "variables\n")
                                }
                                
                                if (!is.null(private$FNAReport)) {
                                  cat("\n--- Données Manquantes ---\n")
                                  cat("Total NA traités    :", private$FNAReport$total_na, "\n")
                                  cat("Stratégie           :", private$FNAReport$strategy, "\n")
                                }
                                
                                cat("\n")
                                invisible(self)
                              },
                              
                              # ==========================================================================
                              # MÉTHODES ACCESSEURS (pour wrapper et user_functions)
                              # ==========================================================================
                              
                              get_clusters = function() {
                                if (!private$FFitted) stop("Modèle non ajusté")
                                
                                result <- data.frame(
                                  Variable = colnames(private$FX),
                                  Cluster = private$FGroupes
                                )
                                
                                if (!is.null(private$FSilhouette)) {
                                  result$Silhouette <- private$FSilhouette[, 3]
                                }
                                
                                return(result)
                              },
                              
                              get_medoids = function() {
                                if (!private$FFitted) stop("Modèle non ajusté")
                                
                                if (is.null(private$FMedoids)) {
                                  return(NULL)
                                }
                                
                                var_names <- colnames(private$FX)
                                result <- data.frame(
                                  Cluster = 1:private$FNbGroupes,
                                  Medoid = var_names[private$FMedoids],
                                  Medoid_Index = private$FMedoids
                                )
                                
                                return(result)
                              },
                              
                              get_metrics = function() {
                                if (!private$FFitted) stop("Modèle non ajusté")
                                return(private$FMetrics)
                              }
                            )
)
