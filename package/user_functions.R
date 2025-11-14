# ==================================================
# FONCTIONS UTILITAIRES POUR LE CLUSTERING DE VARIABLES
# Package de clustering R6
# ==================================================

# ---------------------------------
# 1. Fonctions de Validation de Données
# ---------------------------------

#' @title Valider le type de données pour clustering de variables
#' @description Vérifie si les données sont appropriées pour le clustering de variables
#' @param X Data frame ou matrice
#' @param expected_type Type attendu : "numeric", "categorical", "mixed" (par défaut: NULL, détecte automatiquement)
#' @return Liste avec le type détecté et des infos sur les colonnes
#' @export
validate_data_type <- function(X, expected_type = NULL) {
  
  if (!is.data.frame(X)) {
    if (is.matrix(X)) {
      X <- as.data.frame(X, stringsAsFactors = TRUE)
    } else {
      stop("Les données doivent être un data frame ou une matrice.")
    }
  }
  
  # Analyser les types de colonnes
  col_types <- sapply(X, class)
  numeric_cols <- sum(col_types %in% c("numeric", "integer"))
  categorical_cols <- sum(col_types %in% c("factor", "character"))
  other_cols <- ncol(X) - numeric_cols - categorical_cols
  
  # Déterminer le type global
  if (numeric_cols == ncol(X)) {
    detected_type <- "numeric"
  } else if (categorical_cols == ncol(X)) {
    detected_type <- "categorical"
  } else {
    detected_type <- "mixed"
  }
  
  # Vérification du type attendu
  if (!is.null(expected_type) && detected_type != expected_type) {
    stop(paste0(
      "Type de données incorrect. Attendu: ", expected_type, 
      ", Détecté: ", detected_type
    ))
  }
  
  result <- list(
    type = detected_type,
    n_vars = ncol(X),
    n_obs = nrow(X),
    numeric_cols = numeric_cols,
    categorical_cols = categorical_cols,
    other_cols = other_cols,
    col_names = colnames(X),
    col_types = col_types
  )
  
  return(result)
}

#' @title Vérifier la présence de valeurs manquantes
#' @description Analyse les valeurs manquantes dans le dataset
#' @param X Data frame ou matrice
#' @param verbose Booléen, afficher les détails (défaut: TRUE)
#' @return Liste avec statistiques sur les NA
#' @export
check_missing_values <- function(X, verbose = TRUE) {
  
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X doit être un data frame ou une matrice")
  }
  
  # Statistiques globales
  total_cells <- nrow(X) * ncol(X)
  na_count <- sum(is.na(X))
  na_pct <- (na_count / total_cells) * 100
  
  # Par colonne
  na_by_col <- colSums(is.na(X))
  na_pct_by_col <- (na_by_col / nrow(X)) * 100
  
  # Par ligne
  na_by_row <- rowSums(is.na(X))
  complete_rows <- sum(na_by_row == 0)
  
  result <- list(
    has_missing = na_count > 0,
    total_na = na_count,
    na_percentage = na_pct,
    na_by_column = data.frame(
      variable = names(na_by_col),
      n_missing = na_by_col,
      pct_missing = na_pct_by_col
    ),
    complete_rows = complete_rows,
    incomplete_rows = nrow(X) - complete_rows
  )
  
  if (verbose && result$has_missing) {
    cat("=== ANALYSE DES VALEURS MANQUANTES ===\n")
    cat(sprintf("Total de valeurs manquantes : %d (%.2f%%)\n", na_count, na_pct))
    cat(sprintf("Lignes complètes : %d / %d\n", complete_rows, nrow(X)))
    cat(sprintf("Lignes incomplètes : %d / %d\n\n", result$incomplete_rows, nrow(X)))
    
    # Afficher les colonnes avec NA
    cols_with_na <- result$na_by_column[result$na_by_column$n_missing > 0, ]
    if (nrow(cols_with_na) > 0) {
      cat("Variables avec valeurs manquantes :\n")
      print(cols_with_na, row.names = FALSE)
    }
  }
  
  return(invisible(result))
}

#' @title Vérifier la variance des variables
#' @description Identifie les variables à variance nulle ou très faible
#' @param X Data frame ou matrice (variables numériques uniquement)
#' @param threshold Seuil de variance minimal (défaut: 1e-10)
#' @param verbose Booléen, afficher les détails (défaut: TRUE)
#' @return Vecteur des noms de variables problématiques
#' @export
check_variance <- function(X, threshold = 1e-10, verbose = TRUE) {
  
  # Ne garder que les colonnes numériques
  numeric_cols <- sapply(X, is.numeric)
  
  if (sum(numeric_cols) == 0) {
    if (verbose) {
      message("Aucune variable numérique trouvée. Vérification de variance ignorée.")
    }
    return(character(0))
  }
  
  X_num <- X[, numeric_cols, drop = FALSE]
  
  # Calculer les variances
  variances <- apply(X_num, 2, var, na.rm = TRUE)
  
  # Identifier les variables problématiques
  low_var_cols <- names(variances)[variances < threshold | is.na(variances)]
  
  if (verbose && length(low_var_cols) > 0) {
    cat("=== VARIABLES À VARIANCE NULLE OU FAIBLE ===\n")
    cat(sprintf("Seuil de variance : %.2e\n\n", threshold))
    
    var_df <- data.frame(
      variable = low_var_cols,
      variance = variances[low_var_cols]
    )
    print(var_df, row.names = FALSE)
    
    cat("\nRecommandation : Supprimer ces variables avant le clustering.\n")
  }
  
  return(low_var_cols)
}

# ---------------------------------
# 2. Fonctions de Transformation de Données
# ---------------------------------

#' @title Standardiser les variables numériques
#' @description Centre et réduit les variables numériques (z-score)
#' @param X Data frame ou matrice
#' @param center Booléen, centrer les données (défaut: TRUE)
#' @param scale Booléen, réduire les données (défaut: TRUE)
#' @return Liste avec données standardisées et paramètres
#' @export
standardize_variables <- function(X, center = TRUE, scale = TRUE) {
  
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X doit être un data frame ou une matrice")
  }
  
  # Ne standardiser que les colonnes numériques
  numeric_cols <- sapply(X, is.numeric)
  
  if (sum(numeric_cols) == 0) {
    warning("Aucune variable numérique à standardiser")
    return(list(data = X, means = NULL, sds = NULL))
  }
  
  X_std <- X
  means <- NULL
  sds <- NULL
  
  if (center || scale) {
    X_num <- X[, numeric_cols, drop = FALSE]
    
    if (center) {
      means <- colMeans(X_num, na.rm = TRUE)
      X_num <- sweep(X_num, 2, means, "-")
    }
    
    if (scale) {
      sds <- apply(X_num, 2, sd, na.rm = TRUE)
      
      # Gérer les colonnes à variance nulle
      zero_sd_cols <- which(sds == 0 | is.na(sds))
      if (length(zero_sd_cols) > 0) {
        warning(paste("Colonnes à variance nulle détectées:", 
                      paste(colnames(X_num)[zero_sd_cols], collapse = ", ")))
        sds[zero_sd_cols] <- 1  # Éviter division par zéro
      }
      
      X_num <- sweep(X_num, 2, sds, "/")
    }
    
    X_std[, numeric_cols] <- X_num
  }
  
  return(list(
    data = X_std,
    means = means,
    sds = sds,
    numeric_cols = which(numeric_cols)
  ))
}

#' @title Supprimer les variables à faible variance
#' @description Filtre les variables avec variance inférieure au seuil
#' @param X Data frame ou matrice
#' @param threshold Seuil de variance (défaut: 0.01)
#' @return Data frame filtré
#' @export
remove_low_variance <- function(X, threshold = 0.01) {
  
  # Identifier les colonnes à supprimer
  low_var_cols <- check_variance(X, threshold = threshold, verbose = FALSE)
  
  if (length(low_var_cols) > 0) {
    message(paste("Suppression de", length(low_var_cols), 
                  "variable(s) à faible variance:", 
                  paste(low_var_cols, collapse = ", ")))
    X <- X[, !(colnames(X) %in% low_var_cols), drop = FALSE]
  }
  
  return(X)
}

# ---------------------------------
# 3. Fonctions de Visualisation
# ---------------------------------

#' @title Visualiser la matrice de corrélation
#' @description Crée une heatmap de la matrice de corrélation
#' @param X Data frame ou matrice (variables numériques)
#' @param method Méthode de corrélation : "pearson", "spearman", "kendall" (défaut: "pearson")
#' @param cluster_vars Booléen, ordonner les variables par clustering (défaut: TRUE)
#' @return Matrice de corrélation (invisible)
#' @export
plot_correlation_matrix <- function(X, method = "pearson", cluster_vars = TRUE) {
  
  # Ne garder que les colonnes numériques
  numeric_cols <- sapply(X, is.numeric)
  X_num <- X[, numeric_cols, drop = FALSE]
  
  if (ncol(X_num) < 2) {
    stop("Au moins 2 variables numériques sont nécessaires pour la corrélation")
  }
  
  # Calculer la matrice de corrélation
  cor_matrix <- cor(X_num, use = "pairwise.complete.obs", method = method)
  
  # Ordonner par clustering si demandé
  if (cluster_vars && ncol(cor_matrix) > 2) {
    # Utiliser la distance (1 - |cor|) pour le clustering
    dist_matrix <- as.dist(1 - abs(cor_matrix))
    hc <- hclust(dist_matrix, method = "ward.D2")
    cor_matrix <- cor_matrix[hc$order, hc$order]
  }
  
  # Visualisation
  if (requireNamespace("corrplot", quietly = TRUE)) {
    corrplot::corrplot(cor_matrix, 
                       method = "color",
                       type = "upper",
                       order = ifelse(cluster_vars, "original", "original"),
                       tl.col = "black",
                       tl.srt = 45,
                       addCoef.col = "black",
                       number.cex = 0.7,
                       title = paste("Matrice de corrélation (", method, ")", sep = ""),
                       mar = c(0, 0, 2, 0))
  } else {
    # Fallback avec heatmap de base
    heatmap(cor_matrix, 
            Rowv = if(cluster_vars) as.dendrogram(hc) else NA,
            Colv = if(cluster_vars) as.dendrogram(hc) else NA,
            col = colorRampPalette(c("blue", "white", "red"))(100),
            scale = "none",
            main = paste("Matrice de corrélation (", method, ")", sep = ""))
  }
  
  return(invisible(cor_matrix))
}

#' @title Visualiser un dendrogramme
#' @description Crée un dendrogramme pour la CAH de variables
#' @param X Data frame ou matrice (variables numériques)
#' @param method Méthode de distance : "correlation", "euclidean", "manhattan" (défaut: "correlation")
#' @param linkage Méthode de linkage : "ward.D2", "complete", "average", "single" (défaut: "ward.D2")
#' @param k Nombre de clusters à mettre en évidence (optionnel)
#' @return Objet hclust (invisible)
#' @export
plot_dendrogram <- function(X, method = "correlation", linkage = "ward.D2", k = NULL) {
  
  # Ne garder que les colonnes numériques
  numeric_cols <- sapply(X, is.numeric)
  X_num <- X[, numeric_cols, drop = FALSE]
  
  if (ncol(X_num) < 2) {
    stop("Au moins 2 variables numériques sont nécessaires")
  }
  
  # Transposer pour avoir les variables en lignes
  X_t <- t(X_num)
  
  # Calculer la matrice de distance
  if (method == "correlation") {
    # Distance basée sur la corrélation : d = 1 - |cor|
    cor_matrix <- cor(X_num, use = "pairwise.complete.obs")
    dist_matrix <- as.dist(1 - abs(cor_matrix))
  } else {
    dist_matrix <- dist(X_t, method = method)
  }
  
  # CAH
  hc <- hclust(dist_matrix, method = linkage)
  
  # Visualisation
  if (!is.null(k) && requireNamespace("dendextend", quietly = TRUE)) {
    dend <- dendextend::as.dendrogram(hc)
    dend <- dendextend::color_branches(dend, k = k)
    plot(dend, 
         main = paste("Dendrogramme des variables (", method, " - ", linkage, ")", sep = ""),
         xlab = "Variables",
         ylab = "Hauteur")
    if (k > 1) {
      rect.hclust(hc, k = k, border = 2:6)
    }
  } else {
    plot(hc, 
         main = paste("Dendrogramme des variables (", method, " - ", linkage, ")", sep = ""),
         xlab = "Variables",
         ylab = "Hauteur",
         hang = -1)
    if (!is.null(k) && k > 1) {
      rect.hclust(hc, k = k, border = 2:6)
    }
  }
  
  return(invisible(hc))
}

#' @title Visualiser la qualité du clustering
#' @description Graphique de l'inertie expliquée en fonction du nombre de clusters
#' @param inertia_data Data frame avec colonnes 'k' et 'inertie_expliquee'
#' @return NULL
#' @export
plot_cluster_quality <- function(inertia_data) {
  
  if (!all(c("k", "inertie_expliquee") %in% names(inertia_data))) {
    stop("Le data frame doit contenir les colonnes 'k' et 'inertie_expliquee'")
  }
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(inertia_data, ggplot2::aes(x = k, y = inertie_expliquee)) +
      ggplot2::geom_point(color = "steelblue", size = 4) +
      ggplot2::geom_line(color = "steelblue", size = 1.2) +
      ggplot2::geom_hline(yintercept = 75, linetype = "dashed", color = "red", alpha = 0.5) +
      ggplot2::labs(
        title = "Qualité du Clustering de Variables",
        subtitle = "Ligne pointillée rouge : seuil de 75% d'inertie expliquée",
        x = "Nombre de Clusters (k)",
        y = "Inertie Expliquée (%)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_x_continuous(breaks = inertia_data$k) +
      ggplot2::scale_y_continuous(limits = c(0, 100))
    
    print(p)
  } else {
    plot(inertia_data$k, inertia_data$inertie_expliquee, 
         type = 'b',
         main = "Qualité du Clustering de Variables",
         xlab = "Nombre de Clusters (k)", 
         ylab = "Inertie Expliquée (%)",
         col = "steelblue", 
         pch = 19,
         cex = 1.5,
         ylim = c(0, 100))
    abline(h = 75, lty = 2, col = "red")
    legend("bottomright", 
           legend = "Seuil 75%", 
           lty = 2, 
           col = "red",
           bty = "n")
  }
  
  invisible(NULL)
}

# ---------------------------------
# 4. Fonctions d'Export et de Rapport
# ---------------------------------

#' @title Exporter les résultats du clustering
#' @description Exporte les groupes de variables dans un fichier CSV
#' @param clustering_object Objet de clustering ajusté
#' @param file Chemin du fichier de sortie (défaut: "clustering_results.csv")
#' @return Data frame des résultats (invisible)
#' @export
export_clustering_results <- function(clustering_object, file = "clustering_results.csv") {
  
  if (!("Groupes" %in% names(clustering_object))) {
    stop("L'objet de clustering doit avoir une propriété $Groupes")
  }
  
  # Récupérer les groupes
  groupes <- clustering_object$Groupes
  
  # Créer un data frame
  results <- data.frame(
    Variable = names(groupes),
    Cluster = groupes,
    stringsAsFactors = FALSE
  )
  
  # Trier par cluster puis par nom de variable
  results <- results[order(results$Cluster, results$Variable), ]
  
  # Exporter
  write.csv(results, file = file, row.names = FALSE)
  message(paste("Résultats exportés dans:", file))
  
  return(invisible(results))
}

#' @title Créer un résumé textuel du clustering
#' @description Génère un résumé structuré des résultats
#' @param clustering_object Objet de clustering ajusté
#' @return Caractère avec le résumé
#' @export
summarize_clustering <- function(clustering_object) {
  
  summary_text <- c()
  
  # En-tête
  summary_text <- c(summary_text, "=== RÉSUMÉ DU CLUSTERING DE VARIABLES ===\n")
  
  # Type d'algorithme
  algo_class <- class(clustering_object)[1]
  summary_text <- c(summary_text, paste("Algorithme :", algo_class, "\n"))
  
  # Nombre de clusters
  if ("K" %in% names(clustering_object)) {
    summary_text <- c(summary_text, paste("Nombre de clusters :", clustering_object$K, "\n"))
  }
  
  # Nombre de variables
  if ("Groupes" %in% names(clustering_object)) {
    n_vars <- length(clustering_object$Groupes)
    summary_text <- c(summary_text, paste("Nombre de variables :", n_vars, "\n"))
  }
  
  # Qualité (inertie)
  inertie <- tryCatch({
    clustering_object$inertie()
  }, error = function(e) NULL)
  
  if (!is.null(inertie)) {
    summary_text <- c(summary_text, "\n--- Qualité du Clustering ---\n")
    summary_text <- c(summary_text, sprintf("Inertie expliquée : %.2f%%\n", inertie$pct_expliquee))
    summary_text <- c(summary_text, sprintf("Inertie totale : %.4f\n", inertie$totale))
    summary_text <- c(summary_text, sprintf("Inertie intra : %.4f\n", inertie$intra))
    summary_text <- c(summary_text, sprintf("Inertie inter : %.4f\n", inertie$inter))
  }
  
  # Taille des groupes
  if ("Groupes" %in% names(clustering_object)) {
    summary_text <- c(summary_text, "\n--- Taille des Groupes ---\n")
    tailles <- table(clustering_object$Groupes)
    for (i in seq_along(tailles)) {
      summary_text <- c(summary_text, sprintf("Cluster %d : %d variables\n", 
                                              as.numeric(names(tailles)[i]), 
                                              tailles[i]))
    }
  }
  
  # Convertir en chaîne unique
  result <- paste(summary_text, collapse = "")
  
  cat(result)
  return(invisible(result))
}

# ---------------------------------
# 5. Fonctions d'Aide à la Décision
# ---------------------------------

#' @title Recommander le nombre optimal de clusters
#' @description Analyse les résultats d'évaluation et recommande un k optimal
#' @param evaluation_results Data frame de résultats d'évaluation (avec colonnes k et inertie_expliquee)
#' @param min_inertia Seuil minimal d'inertie expliquée (défaut: 75)
#' @return Liste avec k recommandé et justification
#' @export
recommend_k <- function(evaluation_results, min_inertia = 75) {
  
  if (!all(c("k", "inertie_expliquee") %in% names(evaluation_results))) {
    stop("Le data frame doit contenir les colonnes 'k' et 'inertie_expliquee'")
  }
  
  # Méthode 1 : Premier k atteignant le seuil d'inertie
  k_threshold <- evaluation_results$k[evaluation_results$inertie_expliquee >= min_inertia][1]
  
  # Méthode 2 : Méthode du coude (changement maximal de pente)
  if (nrow(evaluation_results) >= 3) {
    diff1 <- diff(evaluation_results$inertie_expliquee)
    diff2 <- diff(diff1)
    k_elbow <- evaluation_results$k[which.max(abs(diff2)) + 1]
  } else {
    k_elbow <- NA
  }
  
  # Recommandation
  recommendation <- list()
  
  if (!is.na(k_threshold)) {
    recommendation$k_recommended <- k_threshold
    recommendation$method <- "threshold"
    recommendation$reason <- sprintf(
      "Premier k atteignant %.0f%% d'inertie expliquée", 
      min_inertia
    )
  } else if (!is.na(k_elbow)) {
    recommendation$k_recommended <- k_elbow
    recommendation$method <- "elbow"
    recommendation$reason <- "Méthode du coude (changement maximal de courbure)"
  } else {
    recommendation$k_recommended <- max(evaluation_results$k)
    recommendation$method <- "max"
    recommendation$reason <- "Nombre maximal de clusters évalués"
  }
  
  recommendation$inertia_at_k <- evaluation_results$inertie_expliquee[
    evaluation_results$k == recommendation$k_recommended
  ]
  
  # Afficher la recommandation
  cat("=== RECOMMANDATION DE NOMBRE DE CLUSTERS ===\n")
  cat(sprintf("k recommandé : %d\n", recommendation$k_recommended))
  cat(sprintf("Méthode : %s\n", recommendation$method))
  cat(sprintf("Raison : %s\n", recommendation$reason))
  cat(sprintf("Inertie expliquée à k=%d : %.2f%%\n", 
              recommendation$k_recommended, 
              recommendation$inertia_at_k))
  
  return(invisible(recommendation))
}

#' @title Comparer les variables intra-cluster
#' @description Analyse la cohérence des variables au sein de chaque cluster
#' @param X Data frame de données (variables numériques)
#' @param groupes Vecteur des groupes de variables
#' @return Data frame avec statistiques par cluster
#' @export
analyze_cluster_coherence <- function(X, groupes) {
  
  # Ne garder que les colonnes numériques
  numeric_cols <- sapply(X, is.numeric)
  X_num <- X[, numeric_cols, drop = FALSE]
  
  if (ncol(X_num) < 2) {
    stop("Au moins 2 variables numériques sont nécessaires")
  }
  
  # Vérifier la cohérence
  if (length(groupes) != ncol(X_num)) {
    stop("Le vecteur 'groupes' doit avoir la même longueur que le nombre de variables")
  }
  
  # Calculer la matrice de corrélation
  cor_matrix <- cor(X_num, use = "pairwise.complete.obs")
  
  # Analyser chaque cluster
  clusters_uniques <- sort(unique(groupes[!is.na(groupes)]))
  results <- data.frame(
    cluster = integer(),
    n_vars = integer(),
    mean_cor = numeric(),
    min_cor = numeric(),
    max_cor = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (clust in clusters_uniques) {
    vars_in_cluster <- which(groupes == clust)
    
    if (length(vars_in_cluster) >= 2) {
      # Extraire les corrélations intra-cluster
      cor_submatrix <- cor_matrix[vars_in_cluster, vars_in_cluster]
      
      # Extraire le triangle supérieur (sans la diagonale)
      upper_tri <- cor_submatrix[upper.tri(cor_submatrix)]
      
      results <- rbind(results, data.frame(
        cluster = clust,
        n_vars = length(vars_in_cluster),
        mean_cor = mean(abs(upper_tri), na.rm = TRUE),
        min_cor = min(abs(upper_tri), na.rm = TRUE),
        max_cor = max(abs(upper_tri), na.rm = TRUE)
      ))
    } else {
      results <- rbind(results, data.frame(
        cluster = clust,
        n_vars = 1,
        mean_cor = NA,
        min_cor = NA,
        max_cor = NA
      ))
    }
  }
  
  cat("=== COHÉRENCE DES CLUSTERS ===\n")
  cat("(Basée sur les corrélations absolues intra-cluster)\n\n")
  print(results, row.names = FALSE)
  
  return(invisible(results))
}