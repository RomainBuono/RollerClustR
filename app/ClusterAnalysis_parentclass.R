# ==========================================
# Classe PARENTE : ClusterAnalysis
# ==========================================

ClusterAnalysis <- R6Class("ClusterAnalysis",
  private = list(
    FX = NULL,       # Données d'apprentissage
    FScale = TRUE,     # Standardisation 
    FNbGroupes = 2,
    FGroupes = c(),
    FDataType = "numeric", # "numeric", "categorical", "mixed"
    FFitted = FALSE,    # Le modèle est-il ajusté ?
    FNAAction = "warn", # Action pour les NA : "warn", "fail", "omit"
    FHasMissing = FALSE, # Dataset contient-il des NA ?
    FNAIndices = NULL,  # Indices des lignes avec NA
    
    # Validation complète du dataset
    validateDataset = function(X) {
      # Vérifier que c'est un data frame
      if (!is.data.frame(X)) {
        if (is.matrix(X)) {
          warning("Conversion de la matrice en data frame")
          X <- as.data.frame(X)
        } else if (is.vector(X)) {
          stop("X ne peut pas être un simple vecteur. Utilisez un data frame avec au moins une colonne.")
        } else {
          stop("X doit être un data frame ou une matrice. Type reçu : ", class(X)[1])
        }
      }
      
      # Vérifier qu'il n'est pas vide
      if (nrow(X) == 0 || ncol(X) == 0) {
        stop("Le jeu de données (X) ne peut pas être vide.")
      }
      
      # Gérer les types (définition de FDataType)
      n_numeric <- sum(sapply(X, is.numeric))
      n_factor <- sum(sapply(X, is.factor))
      
      if (n_numeric == ncol(X)) {
        private$FDataType <- "numeric"
      } else if (n_factor == ncol(X)) {
        private$FDataType <- "categorical"
      } else if (n_numeric > 0 && n_factor > 0) {
        private$FDataType <- "mixed"
      } else {
        stop("Le jeu de données doit contenir des variables numériques ou catégorielles (factors).")
      }
      
      return(X)
    },
    
    # Méthode pour nettoyer les données selon na_action
    cleanDataset = function(X) {
      private$FNAIndices <- which(!complete.cases(X))
      private$FHasMissing <- length(private$FNAIndices) > 0
      
      if (!private$FHasMissing) {
        return(X) # Aucune action nécessaire
      }
      
      if (private$FNAAction == "fail") {
        stop(paste0("Le jeu de données contient ", length(private$FNAIndices), 
                    " lignes avec des NA, et na_action='fail' est spécifié."))
      } else if (private$FNAAction == "omit") {
        warning(paste0(length(private$FNAIndices), 
                       " lignes avec NA omises pour l'ajustement du modèle."))
        return(na.omit(X))
      } else { # "warn"
        warning(paste0("Le jeu de données contient ", length(private$FNAIndices), 
                       " lignes avec NA. na_action='warn' ne les supprime pas. Cela peut causer des erreurs dans l'algorithme."))
        return(X)
      }
    }
    
  ),
  
  public = list(
    #' @description Initialisateur générique pour ClusterAnalysis
    #' @param k Nombre de groupes
    #' @param cr Standardiser les données ?
    #' @param na_action Action pour les NA : "warn", "fail", "omit"
    initialize = function(k = 2, cr = TRUE, na_action = "warn") {
      if (k < 1 || !is.numeric(k) || length(k) != 1) {
        stop("Le nombre de groupes (k) doit être un entier positif.")
      }
      private$FNbGroupes <- as.integer(k)
      private$FScale <- cr
      private$FNAAction <- match.arg(tolower(na_action), c("warn", "fail", "omit"))
    },
    
    #' @description Ajuster le modèle (à implémenter dans les classes filles)
    #' @param X Data frame
    fit = function(X) {
      stop("La méthode 'fit()' doit être implémentée dans la classe héritée.")
    },
    
    #' @description Prédire l'appartenance aux clusters pour des variables illustratives
    #' @param X Data frame de variables illustratives (même nombre d'observations)
    #' @return Data frame avec les résultats de prédiction pour chaque variable
    predict = function(X) {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      if (!is.data.frame(X)) {
        if (is.matrix(X)) X <- as.data.frame(X)
        else stop("X doit être un data frame ou une matrice")
      }
      
      if (nrow(X) != nrow(private$FX)) {
        stop(paste0("X doit avoir ", nrow(private$FX), " observations (comme fit)"))
      }
      
      groupes <- private$FGroupes
      resultats <- data.frame(
        variable = character(), type = character(), cluster_assigne = integer(),
        indicateur = character(), valeur = numeric(), interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      for (var_name in names(X)) {
        var_data <- X[[var_name]]
        
        if (is.numeric(var_data)) {
          correlations <- numeric(private$FNbGroupes)
          for (k in 1:private$FNbGroupes) {
            cluster_k <- as.numeric(groupes == k)
            correlations[k] <- abs(cor(var_data, cluster_k, use = "pairwise.complete.obs"))
          }
          best_cluster <- which.max(correlations)
          
          var_total <- var(var_data, na.rm = TRUE)
          if (var_total > 0) {
            var_intra <- sum(tapply(var_data, groupes, var, na.rm = TRUE) * 
                            table(groupes), na.rm = TRUE) / nrow(X)
            eta_squared <- 1 - (var_intra / var_total)
          } else {
            eta_squared <- 0
          }
          
          interpretation <- if (eta_squared > 0.3) {
            paste0("Forte association (cluster ", best_cluster, " dominant)")
          } else if (eta_squared > 0.1) {
            paste0("Association modérée (cluster ", best_cluster, ")")
          } else "Faible association"
          
          resultats <- rbind(resultats, data.frame(
            variable = var_name, type = "numérique", cluster_assigne = best_cluster,
            indicateur = "eta²", valeur = eta_squared, interpretation = interpretation,
            stringsAsFactors = FALSE
          ))
        } else if (is.factor(var_data)) {
          contingency <- table(groupes, var_data)
          chi2 <- suppressWarnings(chisq.test(contingency, correct = FALSE)$statistic)
          n <- sum(contingency)
          min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
          cramer_v <- sqrt(chi2 / (n * min_dim))
          
          proportions <- prop.table(contingency, 1)
          best_cluster <- which.max(apply(proportions, 1, max))
          
          interpretation <- if (cramer_v > 0.3) {
            paste0("Forte dépendance (cluster ", best_cluster, ")")
          } else if (cramer_v > 0.1) {
            paste0("Dépendance modérée (cluster ", best_cluster, ")")
          } else "Faible dépendance"
          
          resultats <- rbind(resultats, data.frame(
            variable = var_name, type = "catégorielle", cluster_assigne = best_cluster,
            indicateur = "Cramer's V", valeur = cramer_v, interpretation = interpretation,
            stringsAsFactors = FALSE
          ))
        }
      }
      return(resultats)
    },
    
    #' @description Méthode du coude (Elbow) pour identifier k optimal
    #' @param X Data frame de données numériques
    #' @param k_max Nombre maximum de clusters (défaut: 10)
    #' @param plot Afficher le graphique (défaut: TRUE)
    #' @return Vecteur des inerties intra-cluster
    elbow_method = function(X, k_max = 10, plot = TRUE) {
      if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data frame ou matrice")
      X <- as.data.frame(X)
      if (!all(sapply(X, is.numeric))) {
        stop("Méthode du coude: données numériques uniquement")
      }
      if (k_max < 2) stop("k_max doit être >= 2")
      if (k_max >= nrow(X)) {
        warning("k_max trop grand, réduction")
        k_max <- nrow(X) - 1
      }
      
      message("Calcul méthode du coude k=1:", k_max, "...")
      X_clean <- na.omit(X)
      if (nrow(X_clean) < k_max) stop("Trop de NA")
      X_scaled <- scale(X_clean)
      
      inerties <- numeric(k_max)
      inerties[1] <- sum(X_scaled^2)
      
      for (k in 2:k_max) {
        tryCatch({
          km <- kmeans(X_scaled, centers = k, nstart = 10, iter.max = 100)
          inerties[k] <- km$tot.withinss
        }, error = function(e) {
          warning("Erreur k=", k)
          inerties[k] <- NA
        })
      }
      
      if (plot) {
        plot(1:k_max, inerties, type = "b", pch = 19, 
             xlab = "Nombre de clusters (k)", ylab = "Inertie intra-cluster",
             main = "Méthode du coude (Elbow Method)", col = "steelblue", lwd = 2)
        grid()
        
        if (k_max >= 3) {
          diff2 <- diff(diff(inerties))
          k_suggest <- which.max(abs(diff2)) + 1
          abline(v = k_suggest, col = "red", lty = 2, lwd = 2)
          legend("topright", legend = paste0("k suggéré = ", k_suggest),
                 col = "red", lty = 2, lwd = 2)
        }
      }
      message("✓ Terminé")
      return(invisible(inerties))
    },
    
    #' @description Méthode Silhouette pour identifier k optimal
    #' @param X Data frame de données numériques
    #' @param k_max Nombre maximum de clusters (défaut: 10)
    #' @param plot Afficher le graphique (défaut: TRUE)
    #' @return Vecteur des silhouettes moyennes
    silhouette_method = function(X, k_max = 10, plot = TRUE) {
      if (!requireNamespace("cluster", quietly = TRUE)) {
        stop("Package 'cluster' requis. Installez: install.packages('cluster')")
      }
      if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être data frame ou matrice")
      X <- as.data.frame(X)
      if (!all(sapply(X, is.numeric))) {
        stop("Méthode silhouette: données numériques uniquement")
      }
      if (k_max < 2) stop("k_max >= 2")
      if (k_max >= nrow(X)) {
        warning("k_max trop grand")
        k_max <- nrow(X) - 1
      }
      
      message("Calcul silhouette k=2:", k_max, "...")
      X_clean <- na.omit(X)
      if (nrow(X_clean) < k_max) stop("Trop de NA")
      X_scaled <- scale(X_clean)
      
      silhouettes <- numeric(k_max - 1)
      for (k in 2:k_max) {
        tryCatch({
          km <- kmeans(X_scaled, centers = k, nstart = 10, iter.max = 100)
          sil <- cluster::silhouette(km$cluster, dist(X_scaled))
          silhouettes[k - 1] <- mean(sil[, 3])
        }, error = function(e) {
          warning("Erreur k=", k)
          silhouettes[k - 1] <- NA
        })
      }
      
      if (plot) {
        plot(2:k_max, silhouettes, type = "b", pch = 19,
             xlab = "Nombre de clusters (k)", ylab = "Silhouette moyenne",
             main = "Méthode de la Silhouette", col = "darkgreen", lwd = 2)
        grid()
        abline(h = c(0.5, 0.7), col = c("orange", "red"), lty = 2)
        k_optimal <- which.max(silhouettes) + 1
        abline(v = k_optimal, col = "blue", lty = 2, lwd = 2)
        legend("topright", 
               legend = c("Bonne (0.5)", "Forte (0.7)", paste0("k opt = ", k_optimal)),
               col = c("orange", "red", "blue"), lty = 2, lwd = 2, cex = 0.8)
      }
      message("✓ Terminé. Interprétation: >0.7=forte, 0.5-0.7=bonne, 0.25-0.5=faible, <0.25=aucune")
      return(invisible(silhouettes))
    },
    #' @description Obtenir les valeurs test pour les modalités d'une variable catégorielle
    #' @param var_name Nom de la variable catégorielle
    #' @param modalite Modalité à tester
    v_test_categorielle = function(var_name, modalite) {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      }
      if (private$FDataType == "numeric") {
        stop("V-Test catégoriel non applicable aux données purement numériques.")
      }
      
      # Données et groupes propres (sans NA si omit)
      if (private$FNAAction == "omit" && private$FHasMissing) {
        X_clean <- na.omit(private$FX)
        groupes_clean <- private$FGroupes
        # On ne peut pas juste utiliser private$FGroupes, car il est mappé sur l'original si on n'est pas ClustOfVar
        # Pour les classes de clustering d'observations, private$FGroupes contient les groupes de X_clean (sauf pour Groupes getter)
        # Il faut renettoyer pour avoir les mêmes indices
        idx_clean <- setdiff(1:nrow(private$FX), private$FNAIndices)
        groupes_clean <- private$FGroupes[idx_clean]
        v_clean <- private$FX[idx_clean, var_name, drop = TRUE]
      } else {
        groupes_clean <- private$FGroupes
        v_clean <- private$FX[, var_name, drop = TRUE]
      }
      
      if (!is.factor(v_clean)) {
        stop(paste0("La variable '", var_name, "' n'est pas un facteur (catégorielle)."))
      }
      
      # Construction de la matrice de contingence
      m <- table(groupes_clean, v_clean)
      numModa <- which(colnames(m) == modalite)
      
      if (length(numModa) == 0) stop("Modalité invalide")
      
      n <- length(v_clean)
      p <- colSums(m)[numModa] / sum(m)
      pg <- m[, numModa] / rowSums(m)
      ng <- tapply(v_clean, groupes_clean, length)
      
      # Formule V-Test (Approximation)
      # La formule standard est vt <- sqrt(ng) * (pg - p) / sqrt(p * (1 - p))
      # La formule corrigée pour l'échantillon est plus stable:
      vt <- sqrt(ng) * (pg - p) / sqrt((n - ng) / (n - 1) * p * (1 - p))
      vt[is.nan(vt) | is.infinite(vt)] <- 0
      
      cat("Proportion de référence :", round(p, 3), "\n")
      cat("Proportions conditionnelles et valeurs test\n")
      for (k in 1:length(ng)) {
        cat("Groupe :", k, "\tProportion :", round(pg[k], 3), "\tV-Test :", round(vt[k], 3), "\n")
      }
      invisible(vt)
    },
    
    #' @description Méthode pour obtenir des infos sur les données manquantes
    missing_info = function() {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      }
      
      if (!private$FHasMissing) {
        message("Aucune donnée manquante détectée")
        return(NULL)
      }
      
      return(list(
        nb_rows_with_na = length(private$FNAIndices),
        indices = private$FNAIndices,
        action_prise = private$FNAAction,
        nb_rows_clean = nrow(private$FX) - length(private$FNAIndices)
      ))
    },
    
    #' @description Résumé du modèle (à surcharger)
    summary = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      
      cat("=== Résumé du Modèle de Clustering ===\n")
      cat("Type de données : ", private$FDataType, "\n")
      cat("Nombre de Groupes : ", private$FNbGroupes, "\n")
      cat("Nombre d'Observations : ", nrow(private$FX), "\n")
      
      if (private$FHasMissing) {
        cat("Gestion des NA : ", private$FNAAction, " (", 
            length(private$FNAIndices), " lignes impactées)\n", sep = "")
      }
      
      cat("\nUtilisez $Groupes pour obtenir les résultats.\n")
      
      invisible(self)
    }
  ),
  
  active = list(
    #' @field Groupes Groupes des observations. Inclut NA si na_action="omit"
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      # Réaffectation des groupes aux observations initiales si NA ont été omis
      if (private$FNAAction == "omit" && private$FHasMissing) {
        result <- rep(NA_integer_, nrow(private$FX))
        # Les indices des lignes complètes sont les indices de private$FX qui ne sont pas dans FNAIndices
        # Pour les classes de clustering d'observations, private$FGroupes contient les résultats pour les lignes propres.
        
        # Trouver les indices qui ont été utilisés (ceux qui ne sont pas NA)
        idx_clean <- setdiff(1:nrow(private$FX), private$FNAIndices)
        
        # Assurez-vous que la taille correspond, sinon il y a un bug interne
        if (length(idx_clean) != length(private$FGroupes)) {
          warning("Erreur interne: La taille des groupes propres ne correspond pas au nombre de lignes propres.")
          return(private$FGroupes) # Retourne le vecteur propre en cas de doute
        }
        
        result[idx_clean] <- private$FGroupes
        return(factor(result))
      }
      
      return(factor(private$FGroupes))
    }
    
    
  )
)