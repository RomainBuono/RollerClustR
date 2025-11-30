# ==============================================================================
# GLOBAL.R - VERSION CORRIGÉE
# ==============================================================================
# Modifications appliquées :
# 1. ✅ Retiré source("KmodesVarClust.R")
# 2. ✅ Retiré source("VARCLUS.R")
#
# Algorithmes conservés : VAR_CAH, VAR_KMEANS, TandemVarClust
# ==============================================================================


# ==============================================================================
# CHARGEMENT DES PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(plotly)
  library(DT)
  library(ggplot2)
  library(R6)
  library(cluster)
  library(factoextra)
  library(readxl)
  library(writexl)
  library(gridExtra)
  library(tsne)
  library(umap)
})

#source("install_packages.R")
source("wrapper.R")
source("ClusterAnalysis.R") 
source("VAR_CAH.R")           
source("VAR_KMEANS.R")
source("TandemVarClust.R")

# -------------------------
# Adaptateur R6 -> structure attendue par server.R
# -------------------------
# Utilitaire pour récupérer une propriété active si existe, sinon fallback
.r6_get <- function(obj, names_vec) {
  for (nm in names_vec) {
    if (!is.null(obj) && nm %in% names(obj)) {
      # si c'est une active binding ou champ public, l'accès renverra la valeur
      val <- tryCatch(obj[[nm]], error = function(e) NULL)
      if (!is.null(val)) return(val)
    }
    # si méthode publique (get_k, get_clusters, etc.)
    if (!is.null(obj) && is.function(obj[[nm]])) {
      val <- tryCatch(obj[[nm]](), error = function(e) NULL)
      if (!is.null(val)) return(val)
    }
  }
  return(NULL)
}

# Calcul rapide des métriques de clustering (fallback si ton package ne les fournit pas)
.compute_metrics <- function(clusters, data_used) {
  
  # Valeurs par défaut si échec
  res <- list(
    silhouette = NA_real_,
    davies_bouldin = NA_real_,
    dunn = NA_real_,
    calinski_harabasz = NA_real_
  )
  
  if (is.null(clusters) || is.null(data_used)) {
    return(res)
  }
  
  k <- length(unique(na.omit(clusters)))
  
  # Vérifications
  if (k < 2 || ncol(data_used) < 2) {
    return(res)
  }
  
  tryCatch({
    
    # ═══════════════════════════════════════════════════════════
    # SILHOUETTE POUR VARIABLES (VRAI CALCUL)
    # ═══════════════════════════════════════════════════════════
    
    if (all(sapply(data_used, is.numeric))) {
      
      # Matrice de corrélation
      cor_mat <- cor(data_used, use = "pairwise.complete.obs")
      
      # Distance = 1 - |corrélation|
      dist_mat <- as.dist(1 - abs(cor_mat))
      
      # Calcul de la silhouette
      sil <- cluster::silhouette(clusters, dist_mat)
      
      # Moyenne
      res$silhouette <- mean(sil[, "sil_width"], na.rm = TRUE)
      
      # ═══════════════════════════════════════════════════════════
      # DAVIES-BOULDIN INDEX (VRAI CALCUL)
      # ═══════════════════════════════════════════════════════════
      
      # Calcul des centres de clusters (PC1 de chaque cluster)
      cluster_centers <- matrix(NA, nrow = k, ncol = nrow(data_used))
      
      for (i in 1:k) {
        vars_in_cluster <- which(clusters == i)
        
        if (length(vars_in_cluster) > 1) {
          X_cluster <- data_used[, vars_in_cluster, drop = FALSE]
          # PC1 comme centre
          pca <- prcomp(t(X_cluster), center = TRUE, scale. = TRUE)
          cluster_centers[i, ] <- pca$x[, 1]
        } else if (length(vars_in_cluster) == 1) {
          cluster_centers[i, ] <- scale(data_used[, vars_in_cluster])
        }
      }
      
      # Distances intra-cluster moyennes
      S <- numeric(k)
      for (i in 1:k) {
        vars_in_cluster <- which(clusters == i)
        if (length(vars_in_cluster) > 0) {
          dists <- as.matrix(dist_mat)[vars_in_cluster, vars_in_cluster]
          S[i] <- mean(dists[lower.tri(dists)])
        }
      }
      
      # Distances entre centres
      center_dists <- dist(cluster_centers, method = "euclidean")
      center_dists_mat <- as.matrix(center_dists)
      
      # Davies-Bouldin
      db <- 0
      for (i in 1:k) {
        max_ratio <- 0
        for (j in 1:k) {
          if (i != j && center_dists_mat[i, j] > 0) {
            ratio <- (S[i] + S[j]) / center_dists_mat[i, j]
            if (ratio > max_ratio) max_ratio <- ratio
          }
        }
        db <- db + max_ratio
      }
      res$davies_bouldin <- db / k
      
      # ═══════════════════════════════════════════════════════════
      # DUNN INDEX (VRAI CALCUL)
      # ═══════════════════════════════════════════════════════════
      
      # Distance minimale inter-cluster
      min_inter <- Inf
      for (i in 1:(k-1)) {
        for (j in (i+1):k) {
          vars_i <- which(clusters == i)
          vars_j <- which(clusters == j)
          
          if (length(vars_i) > 0 && length(vars_j) > 0) {
            dists_ij <- as.matrix(dist_mat)[vars_i, vars_j]
            min_dist <- min(dists_ij)
            if (min_dist < min_inter) min_inter <- min_dist
          }
        }
      }
      
      # Diamètre maximal intra-cluster
      max_intra <- 0
      for (i in 1:k) {
        vars_i <- which(clusters == i)
        if (length(vars_i) > 1) {
          dists_i <- as.matrix(dist_mat)[vars_i, vars_i]
          max_dist <- max(dists_i)
          if (max_dist > max_intra) max_intra <- max_dist
        }
      }
      
      if (max_intra > 0) {
        res$dunn <- min_inter / max_intra
      }
      
      # ═══════════════════════════════════════════════════════════
      # CALINSKI-HARABASZ (VRAI CALCUL)
      # ═══════════════════════════════════════════════════════════
      
      # Centroid global
      global_center <- colMeans(t(data_used), na.rm = TRUE)
      
      # Between-cluster sum of squares
      BCSS <- 0
      for (i in 1:k) {
        vars_in_cluster <- which(clusters == i)
        n_i <- length(vars_in_cluster)
        
        if (n_i > 0) {
          center_i <- cluster_centers[i, ]
          BCSS <- BCSS + n_i * sum((center_i - global_center)^2)
        }
      }
      
      # Within-cluster sum of squares
      WCSS <- 0
      for (i in 1:k) {
        vars_in_cluster <- which(clusters == i)
        
        if (length(vars_in_cluster) > 1) {
          dists <- as.matrix(dist_mat)[vars_in_cluster, vars_in_cluster]
          WCSS <- WCSS + sum(dists^2) / (2 * length(vars_in_cluster))
        }
      }
      
      n <- length(clusters)
      if (WCSS > 0) {
        res$calinski_harabasz <- (BCSS / (k - 1)) / (WCSS / (n - k))
      }
    }
    
  }, error = function(e) {
    warning("Erreur calcul métriques: ", e$message)
  })
  
  return(res)
}

# Fonction principale: transforme un objet R6 en structure 'rv$model' attendue
model_to_shiny <- function(model_obj, algorithm = NA_character_, data_used = NULL) {
  if (is.null(model_obj)) return(NULL)
  
  # Try common names: K / k / get_k
  k_val <- .r6_get(model_obj, c("K", "k", "get_k"))
  # Try groups/clusters: Groupes / groupes / Groupes() / clusters
  clusters_val <- .r6_get(model_obj, c("Groupes", "groupes", "Groupes()", "clusters", "get_clusters"))
  # If clusters are an R6 active binding returning named integer vector, use it as is.
  
  # If clusters not present but model has FGroupes private (rare), try extraction
  if (is.null(clusters_val)) {
    # attempt to call a self$Groupes if exists
    clusters_val <- tryCatch(model_obj$Groupes, error = function(e) NULL)
  }
  
  # ensure clusters are a named integer vector
  if (!is.null(clusters_val) && is.factor(clusters_val)) {
    clusters_val <- as.integer(as.character(clusters_val))
  }
  
  # tamdem
  if (algorithm == "TandemVarClust") {
    clusters_val <- .r6_get(model_obj, c("VariableClusters", "Groupes", "clusters"))
  } else {
    clusters_val <- .r6_get(model_obj, c("Groupes", "groupes", "clusters", "get_clusters"))
  }
  
  # compute fallback metrics
  metrics <- .compute_metrics(clusters_val, data_used)
  
  shiny_model <- list(
    algorithm = ifelse(is.na(algorithm), class(model_obj)[1], algorithm),
    method = NA_character_,
    k = ifelse(is.null(k_val), ifelse(is.null(clusters_val), NA_integer_, length(unique(as.integer(clusters_val)))), as.integer(k_val)),
    clusters = clusters_val,
    silhouette_avg = metrics$silhouette,
    metrics = list(
      davies_bouldin = metrics$davies_bouldin,
      dunn = metrics$dunn,
      calinski_harabasz = metrics$calinski_harabasz,
      silhouette = metrics$silhouette
    ),
    data_used = data_used,
    timestamp = Sys.time(),
    model_object = model_obj
  )
  return(shiny_model)
}



# ==============================================================================
# FONCTION DE GÉNÉRATION DE DONNÉES
# ==============================================================================

generate_sample_data <- function(type = "economic", n = 100, noise = 0.1, seed = 42) {
  set.seed(seed)
  
  data <- switch(type,
                 
                 "economic" = {
                   df <- data.frame(
                     PIB = rnorm(n, 100, 15), Revenu = rnorm(n, 50, 10), Emploi = rnorm(n, 75, 12),
                     Consommation = rnorm(n, 80, 14), Population = rnorm(n, 1000, 200),
                     Natalite = rnorm(n, 15, 3), Mortalite = rnorm(n, 10, 2), Migration = rnorm(n, 5, 2),
                     Temperature = rnorm(n, 20, 5), Precipitation = rnorm(n, 800, 150),
                     Pollution = rnorm(n, 50, 15), Energie = rnorm(n, 100, 20)
                   )
                   df$Revenu        <- df$PIB * 0.7 + rnorm(n, 0, 5)
                   df$Emploi        <- df$PIB * 0.6 + rnorm(n, 0, 8)
                   df$Natalite      <- df$Population * 0.01 + rnorm(n, 0, 2)
                   df$Precipitation <- df$Temperature * (-20) + rnorm(n, 0, 50)
                   df
                 },
                 
                 "biological" = {
                   df <- data.frame(
                     Gene_METAB_1 = rnorm(n, 5, 1), Gene_METAB_2 = rnorm(n, 4.8, 0.9), Gene_METAB_3 = rnorm(n, 5.2, 1.1),
                     Gene_GROWTH_1 = rnorm(n, 8, 1.5), Gene_GROWTH_2 = rnorm(n, 7.5, 1.3), Gene_GROWTH_3 = rnorm(n, 8.5, 1.6),
                     Gene_STRESS_1 = rnorm(n, 3, 0.8), Gene_STRESS_2 = rnorm(n, 2.8, 0.7), Gene_STRESS_3 = rnorm(n, 3.2, 0.9),
                     Gene_IMMUNE_1 = rnorm(n, 6, 1.2), Gene_IMMUNE_2 = rnorm(n, 6.3, 1.1), Gene_IMMUNE_3 = rnorm(n, 5.7, 1.3)
                   )
                   for (i in 1:n) df[i, 1:3] <- df[i, 1:3] + rnorm(1, 0, 0.3)
                   df
                 },
                 
                 "marketing" = {
                   df <- data.frame(
                     Visites_Site = rpois(n, 20), Temps_Site_min = rnorm(n, 15, 5), Pages_Vues = rpois(n, 10),
                     Montant_Achats = rnorm(n, 200, 80), Frequence_Achats = rpois(n, 4), Panier_Moyen = rnorm(n, 50, 20),
                     Note_Satisfaction = rnorm(n, 7.5, 1.5), NPS = rnorm(n, 6, 2)
                   )
                   df$Temps_Site_min <- df$Visites_Site * 0.6 + rnorm(n, 0, 3)
                   df
                 },
                 
                 "mixed" = {
                   data.frame(
                     Age = sample(18:80, n, replace = TRUE),
                     Revenu = rnorm(n, 35000, 15000),
                     Score = rnorm(n, 70, 15),
                     Categorie = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
                     Niveau = factor(sample(c("Débutant", "Intermédiaire", "Expert"), n, replace = TRUE))
                   )
                 },
                 
                 "categorical" = {
                   data.frame(
                     Secteur = factor(sample(c("Tech", "Finance", "Santé", "Industrie"), n, replace = TRUE)),
                     Taille_Entreprise = factor(sample(c("TPE", "PME", "ETI", "GE"), n, replace = TRUE)),
                     Region = factor(sample(c("Nord", "Sud", "Est", "Ouest", "Centre"), n, replace = TRUE)),
                     Type_Client = factor(sample(c("Particulier", "Professionnel", "Entreprise"), n, replace = TRUE)),
                     Niveau_Satisfaction = factor(sample(c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé"), n, replace = TRUE)),
                     Produit_Principal = factor(sample(c("A", "B", "C", "D", "E"), n, replace = TRUE)),
                     Canal_Acquisition = factor(sample(c("Web", "Téléphone", "Magasin", "Partenaire"), n, replace = TRUE)),
                     Statut = factor(sample(c("Actif", "Inactif", "Suspendu"), n, replace = TRUE))
                   )
                 }
  )
  
  # --- Injection NA SÉCURISÉE ---
  if (noise > 0) {
    total_cells <- nrow(data) * ncol(data)
    na_positions <- sample(total_cells, size = round(total_cells * noise * 0.1))
    
    rows <- ((na_positions - 1) %% nrow(data)) + 1
    cols <- ((na_positions - 1) %/% nrow(data)) + 1
    
    for (k in seq_along(rows)) {
      data[rows[k], cols[k]] <- NA
    }
  }
  
  return(data)
}

### Fonctions de detections des types

#' Détecter et convertir automatiquement les types de colonnes
#' @param df Data frame à analyser
#' @return Liste avec df converti et rapport de détection
detect_and_convert_types <- function(df) {
  
  conversions <- list()
  
  for (col_name in names(df)) {
    col <- df[[col_name]]
    original_type <- class(col)[1]
    
    # Si déjà factor, garder tel quel
    if (is.factor(col)) {
      conversions[[col_name]] <- list(
        original = "factor",
        converted = "factor",
        action = "kept"
      )
      next
    }
    
    # Si character, tenter conversion
    if (is.character(col)) {
      
      # Supprimer espaces
      col_clean <- trimws(col)
      
      # Tenter conversion numérique
      col_numeric <- suppressWarnings(as.numeric(col_clean))
      
      # Si plus de 80% des valeurs sont numériques valides
      pct_numeric <- sum(!is.na(col_numeric)) / length(col_numeric)
      
      if (pct_numeric >= 0.8) {
        # Convertir en numérique
        df[[col_name]] <- col_numeric
        conversions[[col_name]] <- list(
          original = "character",
          converted = "numeric",
          action = "converted",
          pct_valid = round(pct_numeric * 100, 1)
        )
      } else {
        # Nombre de valeurs uniques
        n_unique <- length(unique(col_clean[!is.na(col_clean)]))
        n_total <- length(col_clean[!is.na(col_clean)])
        
        # Si moins de 20% de valeurs uniques ou moins de 50 valeurs uniques
        if (n_unique / n_total < 0.2 || n_unique < 50) {
          # Convertir en facteur
          df[[col_name]] <- as.factor(col_clean)
          conversions[[col_name]] <- list(
            original = "character",
            converted = "factor",
            action = "converted",
            n_levels = n_unique
          )
        } else {
          # Garder en character (trop de modalités)
          conversions[[col_name]] <- list(
            original = "character",
            converted = "character",
            action = "kept (too many unique values)",
            n_unique = n_unique
          )
        }
      }
    }
    
    # Si numérique, vérifier si devrait être facteur
    if (is.numeric(col)) {
      n_unique <- length(unique(col[!is.na(col)]))
      
      # Si très peu de valeurs uniques (< 10) et toutes entières
      if (n_unique < 10 && all(col[!is.na(col)] == floor(col[!is.na(col)]))) {
        df[[col_name]] <- as.factor(col)
        conversions[[col_name]] <- list(
          original = "numeric",
          converted = "factor",
          action = "converted (discrete values)",
          n_levels = n_unique
        )
      } else {
        conversions[[col_name]] <- list(
          original = "numeric",
          converted = "numeric",
          action = "kept"
        )
      }
    }
  }
  
  return(list(
    data = df,
    conversions = conversions
  ))
}

#' Afficher un rapport de conversion des types
#' @param conversions Liste des conversions
print_conversion_report <- function(conversions) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("  RAPPORT DE DÉTECTION ET CONVERSION DES TYPES\n")
  cat("═══════════════════════════════════════════════════════\n\n")
  
  converted_count <- 0
  
  for (col_name in names(conversions)) {
    conv <- conversions[[col_name]]
    
    if (conv$action != "kept") {
      converted_count <- converted_count + 1
      cat("Variable :", col_name, "\n")
      cat("  ", conv$original, "→", conv$converted, "\n")
      
      if (!is.null(conv$pct_valid)) {
        cat("  ", conv$pct_valid, "% valeurs numériques valides\n")
      }
      if (!is.null(conv$n_levels)) {
        cat("  ", conv$n_levels, "modalités détectées\n")
      }
      cat("\n")
    }
  }
  
  if (converted_count == 0) {
    cat("✓ Aucune conversion nécessaire\n")
  } else {
    cat("Total conversions :", converted_count, "\n")
  }
  
  cat("═══════════════════════════════════════════════════════\n")
}