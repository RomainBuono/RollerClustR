# ==============================================================================
# FONCTIONS UTILITAIRES
# ==============================================================================

#' Valider le type de données pour le clustering
#' @param X Data frame ou matrice
#' @return Liste avec type et informations
validate_data_type <- function(X) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X doit être un data.frame ou une matrice")
  }
  
  # Convertir en data.frame si matrice
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }
  
  # Analyser les types de colonnes
  has_numeric <- any(sapply(X, is.numeric))
  has_categorical <- any(sapply(X, function(x) is.factor(x) || is.character(x)))
  
  # Déterminer le type global
  if (has_numeric && !has_categorical) {
    type <- "numeric"
  } else if (!has_numeric && has_categorical) {
    type <- "categorical"
  } else if (has_numeric && has_categorical) {
    type <- "mixed"
  } else {
    stop("Aucun type de données valide détecté")
  }
  
  return(list(
    type = type,
    has_numeric = has_numeric,
    has_categorical = has_categorical,
    n_vars = ncol(X),
    n_obs = nrow(X)
  ))
}

#' Calculer l'Adjusted Rand Index
#' @param clusters1 Premier vecteur de clusters
#' @param clusters2 Deuxième vecteur de clusters
#' @return Valeur ARI
adjustedRandIndex <- function(clusters1, clusters2) {
  
  if (length(clusters1) != length(clusters2)) {
    stop("Les vecteurs de clusters doivent avoir la même longueur")
  }
  
  # Supprimer les NA
  valid_idx <- !is.na(clusters1) & !is.na(clusters2)
  c1 <- clusters1[valid_idx]
  c2 <- clusters2[valid_idx]
  
  if (length(c1) == 0) {
    return(NA)
  }
  
  # Créer la table de contingence
  tab <- table(c1, c2)
  
  # Sommes par lignes et colonnes
  a <- rowSums(tab)
  b <- colSums(tab)
  n <- sum(tab)
  
  # Combinaisons
  comb_c <- function(n) {
    if (n < 2) return(0)
    return(choose(n, 2))
  }
  
  # Index
  sum_comb_tab <- sum(sapply(tab, comb_c))
  sum_comb_a <- sum(sapply(a, comb_c))
  sum_comb_b <- sum(sapply(b, comb_c))
  
  expected_index <- sum_comb_a * sum_comb_b / comb_c(n)
  max_index <- (sum_comb_a + sum_comb_b) / 2
  
  if (max_index == expected_index) {
    return(0)
  }
  
  ari <- (sum_comb_tab - expected_index) / (max_index - expected_index)
  
  return(ari)
}

#' Générer un rapport HTML du clustering
#' @param model Modèle de clustering
#' @param output_file Chemin du fichier de sortie
generate_clustering_report <- function(model, output_file = "report.html") {
  
  if (is.null(model)) {
    stop("Le modèle est NULL")
  }
  
  # Créer le contenu HTML
  html_content <- paste0(
    "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>Rapport de Clustering</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 20px; }
    h1 { color: #2c3e50; }
    h2 { color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 5px; }
    table { border-collapse: collapse; width: 100%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f2f2f2; }
    .metric { background-color: #ecf0f1; padding: 10px; margin: 10px 0; border-radius: 5px; }
  </style>
</head>
<body>
  <h1>Rapport de Clustering de Variables</h1>
  <p><strong>Date:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>
  
  <h2>Informations du Modèle</h2>
  <div class='metric'>
    <p><strong>Algorithme:</strong> ", class(model)[1], "</p>
    <p><strong>Nombre de clusters:</strong> ", model$K, "</p>
  </div>
  
  <h2>Résultats</h2>
  <p>Le clustering a été effectué avec succès.</p>
  
</body>
</html>")
  
  # Écrire le fichier
  writeLines(html_content, output_file)
  
  message(paste("Rapport généré:", output_file))
  
  return(invisible(output_file))
}

#' Préparer les données pour le clustering
#' @param X Données brutes
#' @param remove_na Action pour les NA: "omit", "fail", "warn"
#' @return Données nettoyées
prepare_data <- function(X, remove_na = "warn") {
  
  # Vérifier les NA
  has_na <- any(is.na(X))
  
  if (has_na) {
    
    if (remove_na == "fail") {
      stop("Les données contiennent des valeurs manquantes (NA). Utilisez remove_na = 'omit' ou nettoyez les données.")
    }
    
    if (remove_na == "warn") {
      warning("Les données contiennent des valeurs manquantes (NA). Elles seront traitées par l'algorithme.")
    }
    
    if (remove_na == "omit") {
      # Supprimer les lignes avec NA
      X <- na.omit(X)
      message(paste("Lignes avec NA supprimées. Données restantes:", nrow(X), "lignes"))
    }
  }
  
  return(X)
}

#' Calculer la matrice de distance basée sur la corrélation
#' @param X Données numériques
#' @return Objet dist
correlation_distance <- function(X) {
  
  if (!all(sapply(X, is.numeric))) {
    stop("Toutes les colonnes doivent être numériques")
  }
  
  # Calculer la matrice de corrélation
  cor_mat <- cor(X, use = "pairwise.complete.obs")
  
  # Distance = 1 - |corrélation|
  dist_mat <- 1 - abs(cor_mat)
  
  # Convertir en objet dist
  return(as.dist(dist_mat))
}

#' Normaliser les données
#' @param X Données à normaliser
#' @param method Méthode: "scale" (standardisation), "minmax" (0-1), "robust" (médiane/IQR)
#' @return Données normalisées
normalize_data <- function(X, method = "scale") {
  
  if (method == "scale") {
    # Standardisation (moyenne = 0, écart-type = 1)
    X_norm <- scale(X, center = TRUE, scale = TRUE)
    
  } else if (method == "minmax") {
    # Normalisation Min-Max (0-1)
    X_norm <- apply(X, 2, function(col) {
      (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
    })
    
  } else if (method == "robust") {
    # Normalisation robuste (médiane et IQR)
    X_norm <- apply(X, 2, function(col) {
      med <- median(col, na.rm = TRUE)
      iqr <- IQR(col, na.rm = TRUE)
      if (iqr == 0) iqr <- 1
      (col - med) / iqr
    })
    
  } else {
    stop("Méthode non reconnue. Utilisez 'scale', 'minmax' ou 'robust'.")
  }
  
  return(X_norm)
}

#' Vérifier la validité d'un modèle de clustering
#' @param model Modèle à vérifier
#' @return TRUE si valide, sinon erreur
validate_model <- function(model) {
  
  if (is.null(model)) {
    stop("Le modèle est NULL")
  }
  
  # Vérifier que le modèle hérite de ClusterAnalysis
  if (!inherits(model, "ClusterAnalysis")) {
    warning("Le modèle n'hérite pas de ClusterAnalysis")
  }
  
  # Vérifier les champs essentiels
  required_fields <- c("K", "Groupes")
  
  for (field in required_fields) {
    if (!field %in% names(model)) {
      stop(paste("Champ manquant dans le modèle:", field))
    }
  }
  
  return(TRUE)
}

#' Comparer deux clusterings
#' @param clusters1 Premier clustering
#' @param clusters2 Deuxième clustering
#' @return Liste avec métriques de comparaison
compare_clusterings <- function(clusters1, clusters2) {
  
  # Aligner les noms
  common_names <- intersect(names(clusters1), names(clusters2))
  
  if (length(common_names) == 0) {
    stop("Aucun nom commun entre les deux clusterings")
  }
  
  c1 <- clusters1[common_names]
  c2 <- clusters2[common_names]
  
  # Calculer l'ARI
  ari <- adjustedRandIndex(c1, c2)
  
  # Calculer l'accord (% même cluster)
  agreement <- mean(c1 == c2, na.rm = TRUE)
  
  # Table de contingence
  contingency <- table(c1, c2)
  
  return(list(
    ari = ari,
    agreement = agreement,
    contingency = contingency,
    n_common = length(common_names)
  ))
}

#' Afficher un résumé visuel du clustering
#' @param model Modèle de clustering
#' @param data Données utilisées
plot_clustering_summary <- function(model, data) {
  
  par(mfrow = c(2, 2))
  
  # 1. Taille des clusters
  cluster_sizes <- table(model$Groupes)
  barplot(cluster_sizes, main = "Taille des Clusters", 
          xlab = "Cluster", ylab = "Nombre de variables",
          col = rainbow(length(cluster_sizes)))
  
  # 2. Matrice de corrélation
  if (all(sapply(data, is.numeric))) {
    cor_mat <- cor(data, use = "pairwise.complete.obs")
    
    # Réorganiser selon les clusters
    cluster_order <- order(model$Groupes)
    cor_mat_ordered <- cor_mat[cluster_order, cluster_order]
    
    image(1:ncol(cor_mat_ordered), 1:nrow(cor_mat_ordered), 
          t(cor_mat_ordered),
          main = "Matrice de Corrélation (ordonnée)",
          xlab = "", ylab = "", col = heat.colors(100))
  }
  
  # 3. Silhouette
  if (all(sapply(data, is.numeric))) {
    dist_mat <- correlation_distance(data)
    sil <- cluster::silhouette(model$Groupes, dist_mat)
    plot(sil, col = rainbow(model$K), main = "Silhouette")
  }
  
  par(mfrow = c(1, 1))
}

#' Exporter les résultats vers un fichier
#' @param model Modèle de clustering
#' @param file Chemin du fichier de sortie
#' @param format Format: "csv", "xlsx", "rds"
export_results <- function(model, file, format = "csv") {
  
  # Préparer les données d'export
  groupes <- model$Groupes
  
  df <- data.frame(
    Variable = names(groupes),
    Cluster = as.integer(groupes),
    stringsAsFactors = FALSE
  )
  
  df <- df[order(df$Cluster, df$Variable), ]
  
  # Exporter selon le format
  if (format == "csv") {
    write.csv(df, file, row.names = FALSE)
    
  } else if (format == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Le package 'writexl' est requis pour l'export Excel")
    }
    writexl::write_xlsx(df, file)
    
  } else if (format == "rds") {
    saveRDS(list(model = model, results = df), file)
    
  } else {
    stop("Format non reconnu. Utilisez 'csv', 'xlsx' ou 'rds'.")
  }
  
  message(paste("Résultats exportés vers:", file))
  
  return(invisible(df))
}