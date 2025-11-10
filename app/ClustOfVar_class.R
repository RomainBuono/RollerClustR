# ==========================================
# ClustOfVar : Clustering de Variables
# Approche factorielle pour variables mixtes
# ==========================================

ClustOfVar <- R6Class("ClustOfVar",
  inherit = ClusterAnalysis,
  
  private = list(
    FMaxIter = 100,
    FTolerance = 1e-6,
    FConverged = FALSE,
    FIterations = 0,
    FVariablesSynthetiques = NULL, # Variables synthétiques par cluster
    FCorrelations = NULL, # Corrélations variables/clusters
    FQualiteClusters = NULL, # Qualité de chaque cluster
    FHomogeneite = NULL, # Homogénéité globale
    FVarNames = NULL,
    FVarTypes = NULL, # Type de chaque variable
    
    # Calculer la variable synthétique d'un cluster
    calculerVariableSynthetique = function(X_cluster) {
      if (ncol(X_cluster) == 0) return(NULL)
      
      # Détecter les types de variables dans le cluster
      types <- sapply(X_cluster, function(v) {
        if (is.numeric(v)) "numeric" else "factor"
      })
      
      n_numeric <- sum(types == "numeric")
      n_factor <- sum(types == "factor")
      
      if (n_numeric == ncol(X_cluster)) {
        # Cluster purement numérique : PCA
        return(private$pca_synthetique(X_cluster))
      } else if (n_factor == ncol(X_cluster)) {
        # Cluster purement catégoriel : MCA
        return(private$mca_synthetique(X_cluster))
      } else {
        # Cluster mixte : FAMD (Factor Analysis of Mixed Data)
        return(private$famd_synthetique(X_cluster))
      }
    },
    
    # Variable synthétique par PCA (variables numériques)
    pca_synthetique = function(X_num) {
      if (ncol(X_num) == 1) {
        return(scale(X_num[, 1]))
      }
      
      # Centrer-réduire
      X_scaled <- scale(X_num)
      
      # PCA simplifiée : première composante principale
      cov_matrix <- cov(X_scaled)
      eigen_result <- eigen(cov_matrix)
      
      # Première composante
      first_pc <- as.matrix(X_scaled) %*% eigen_result$vectors[, 1]
      
      # Normaliser
      return(as.vector(first_pc / sd(first_pc)))
    },
    
    # Variable synthétique par MCA (variables catégorielles)
    mca_synthetique = function(X_cat) {
      if (ncol(X_cat) == 1) {
        # Transformer en indicatrices
        return(private$factor_to_numeric(X_cat[, 1]))
      }
      
      # Créer la matrice disjonctive complète
      Z <- private$create_indicator_matrix(X_cat)
      
      # MCA simplifiée
      n <- nrow(Z)
      p <- ncol(Z)
      
      # Profils lignes
      row_masses <- rowSums(Z) / sum(Z)
      col_masses <- colSums(Z) / sum(Z)
      
      # Centrage
      Z_centered <- sweep(Z, 2, col_masses, "-")
      Z_centered <- sweep(Z_centered, 1, sqrt(row_masses), "/")
      
      # SVD
      svd_result <- svd(Z_centered)
      
      # Première dimension
      first_dim <- svd_result$u[, 1] * svd_result$d[1]
      
      return(as.vector(first_dim / sd(first_dim)))
    },
    
    # Variable synthétique par FAMD (variables mixtes)
    famd_synthetique = function(X_mixed) {
      # Séparer numériques et catégorielles
      types <- sapply(X_mixed, function(v) {
        if (is.numeric(v)) "numeric" else "factor"
      })
      
      idx_num <- which(types == "numeric")
      idx_cat <- which(types == "factor")
      
      synthetiques <- list()
      
      # Variables numériques
      if (length(idx_num) > 0) {
        X_num <- X_mixed[, idx_num, drop = FALSE]
        synthetiques$num <- private$pca_synthetique(X_num)
      }
      
      # Variables catégorielles
      if (length(idx_cat) > 0) {
        X_cat <- X_mixed[, idx_cat, drop = FALSE]
        synthetiques$cat <- private$mca_synthetique(X_cat)
      }
      
      # Combiner les deux
      if (length(synthetiques) == 2) {
        # Moyenne pondérée
        w_num <- length(idx_num) / ncol(X_mixed)
        w_cat <- length(idx_cat) / ncol(X_mixed)
        return(w_num * synthetiques$num + w_cat * synthetiques$cat)
      } else if (length(idx_num) > 0) {
        return(synthetiques$num)
      } else {
        return(synthetiques$cat)
      }
    },
    
    # Convertir factor en numérique pour MCA
    factor_to_numeric = function(f) {
      # Utiliser les fréquences comme poids
      freq <- table(f) / length(f)
      num <- rep(0, length(f))
      for (i in seq_along(levels(f))) {
        num[f == levels(f)[i]] <- freq[i]
      }
      return(scale(num))
    },
    
    # Créer matrice indicatrice (disjonctive complète)
    create_indicator_matrix = function(X_cat) {
      n <- nrow(X_cat)
      matrices <- list()
      
      for (j in 1:ncol(X_cat)) {
        var <- X_cat[, j]
        levels_var <- levels(var)
        mat <- matrix(0, nrow = n, ncol = length(levels_var))
        colnames(mat) <- paste0(names(X_cat)[j], "_", levels_var)
        
        for (i in 1:length(levels_var)) {
          mat[var == levels_var[i], i] <- 1
        }
        matrices[[j]] <- mat
      }
      
      return(do.call(cbind, matrices))
    },
    
    # Calculer la corrélation/association entre variable et variable synthétique
    calculerLiaison = function(var, var_synth) {
      if (is.numeric(var)) {
        # Corrélation de Pearson au carré
        return(cor(var, var_synth)^2)
      } else {
        # Rapport de corrélation (eta²)
        SCT <- sum((var_synth - mean(var_synth))^2)
        
        # NOTE : Si SCT est proche de zéro, le calcul peut donner NA/NaN/Inf.
        if (SCT < 1e-10) return(0) 
        
        moyennes <- tapply(var_synth, var, mean)
        effectifs <- tapply(var_synth, var, length)
        SCE <- sum(effectifs * (moyennes - mean(var_synth))^2)
        return(SCE / SCT)
      }
    },
    
    # Initialisation aléatoire des groupes
    initialiserGroupes = function(n_vars) {
      groupes <- sample(1:private$FNbGroupes, n_vars, replace = TRUE)
      
      # S'assurer que chaque groupe a au moins une variable
      for (k in 1:private$FNbGroupes) {
        if (sum(groupes == k) == 0) {
          # Trouver un index qui n'est pas déjà dans ce groupe
          # La logique de 'sample' dans cette boucle peut être tricky si n_vars est petit
          # On assure juste la présence pour le premier tour d'initialisation
          groupes[sample(1:n_vars, 1)] <- k 
        }
      }
      
      return(groupes)
    },
    
    # Calculer l'homogénéité d'une partition
    calculerHomogeneite = function(X, groupes) {
      homogeneite <- 0
      
      for (k in 1:private$FNbGroupes) {
        idx_cluster <- which(groupes == k)
        if (length(idx_cluster) == 0) next
        
        X_cluster <- X[, idx_cluster, drop = FALSE]
        var_synth <- private$calculerVariableSynthetique(X_cluster)
        
        if (is.null(var_synth) || any(is.na(var_synth))) next
        
        liaisons <- sapply(idx_cluster, function(i) {
          private$calculerLiaison(X[, i], var_synth)
        })
        
        # Vérification des liaisons pour éviter NA/NaN
        if (any(!is.finite(liaisons))) {
          return(NA) # Renvoie NA si une liaison est invalide
        }
        
        homogeneite <- homogeneite + sum(liaisons)
      }
      
      return(homogeneite)
    }
  ),
  
  public = list(
    #' @description Initialiser un objet ClustOfVar
    #' @param k Nombre de groupes (défaut: 2)
    #' @param cr Standardiser les variables numériques ? (défaut: TRUE)
    #' @param max_iter Nombre max d'itérations (défaut: 100)
    #' @param tolerance Critère de convergence (défaut: 1e-6)
    initialize = function(k = 2, cr = TRUE, max_iter = 100, tolerance = 1e-6) {
      super$initialize(k = k, cr = cr)
      private$FMaxIter <- max_iter
      private$FTolerance <- tolerance
    },
    
    #' @description Ajuster le modèle ClustOfVar sur les données
    #' @param X Data frame avec variables numériques et/ou catégorielles
    fit = function(X) {
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      private$FX <- X
      private$FVarNames <- names(X)
      private$FDataType <- private$detectDataType(X)
      
      private$FVarTypes <- sapply(X, function(v) {
        if (is.numeric(v)) "numeric" else "factor"
      })
      
      X_work <- X
      if (private$FScale) {
        for (j in 1:ncol(X)) {
          if (is.numeric(X[, j])) {
            X_work[, j] <- scale(X[, j])
          }
        }
      }
      
      n_vars <- ncol(X_work)
      
      message("Initialisation des groupes...")
      groupes <- private$initialiserGroupes(n_vars)
      homogeneite_old <- private$calculerHomogeneite(X_work, groupes)
      
      message("Démarrage de l'algorithme ClustOfVar...")
      
      for (iter in 1:private$FMaxIter) {
        for (i in 1:n_vars) {
          groupe_actuel <- groupes[i]
          meilleur_groupe <- groupe_actuel
          meilleure_homogeneite <- -Inf
          
          for (k in 1:private$FNbGroupes) {
            # Logique pour éviter de laisser un cluster vide
            if (k != groupe_actuel && sum(groupes == groupe_actuel) == 1) {
              next
            }
            
            groupes_temp <- groupes
            groupes_temp[i] <- k
            h <- private$calculerHomogeneite(X_work, groupes_temp)
            
            # 🎯 CORRECTION : Utiliser is.finite(h) pour ignorer NA/NaN/Inf
            if (is.finite(h) && h > meilleure_homogeneite) {
              meilleure_homogeneite <- h
              meilleur_groupe <- k
            }
          }
          
          if (meilleur_groupe != groupe_actuel) {
            groupes[i] <- meilleur_groupe
          }
        }
        
        homogeneite_new <- private$calculerHomogeneite(X_work, groupes)
        
        if (abs(homogeneite_new - homogeneite_old) < private$FTolerance) {
          private$FConverged <- TRUE
          private$FIterations <- iter
          message("✓ Convergence atteinte après ", iter, " itérations")
          break
        }
        
        homogeneite_old <- homogeneite_new
        
        if (iter %% 10 == 0) {
          message("  Itération ", iter, " - Homogénéité: ", 
                  round(homogeneite_new, 4))
        }
      }
      
      if (!private$FConverged) {
        private$FIterations <- private$FMaxIter
        warning("Nombre max d'itérations atteint sans convergence complète")
      }
      
      private$FGroupes <- groupes
      names(private$FGroupes) <- private$FVarNames
      
      # Finalisation des résultats
      private$FVariablesSynthetiques <- list()
      private$FCorrelations <- matrix(0, nrow = n_vars, ncol = private$FNbGroupes)
      rownames(private$FCorrelations) <- private$FVarNames
      colnames(private$FCorrelations) <- paste0("Cluster_", 1:private$FNbGroupes)
      
      private$FQualiteClusters <- numeric(private$FNbGroupes)
      
      for (k in 1:private$FNbGroupes) {
        idx_cluster <- which(groupes == k)
        if (length(idx_cluster) == 0) next
        
        X_cluster <- X_work[, idx_cluster, drop = FALSE]
        var_synth <- private$calculerVariableSynthetique(X_cluster)
        
        # Vérification finale pour éviter le plantage
        if (is.null(var_synth) || any(!is.finite(var_synth))) next
        
        private$FVariablesSynthetiques[[k]] <- var_synth
        
        for (i in 1:n_vars) {
          private$FCorrelations[i, k] <- private$calculerLiaison(X_work[, i], var_synth)
        }
        
        liaisons_intra <- sapply(idx_cluster, function(i) {
          private$calculerLiaison(X_work[, i], var_synth)
        })
        
        private$FQualiteClusters[k] <- mean(liaisons_intra[is.finite(liaisons_intra)])
      }
      
      private$FHomogeneite <- homogeneite_new
      private$FFitted <- TRUE
      
      message("✓ Modèle ClustOfVar ajusté avec succès (", private$FNbGroupes, " groupes)")
      message("  Homogénéité globale : ", round(private$FHomogeneite, 4))
      
      invisible(self)
    },
    
    #' @description Prédire le cluster pour de nouvelles variables
    predict = function(X) {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      }
      
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      if (nrow(X) != nrow(private$FX)) {
        stop("X doit avoir le même nombre d'observations que les données d'apprentissage (", 
             nrow(private$FX), " observations)")
      }
      
      resultats <- list()
      
      for (var_name in names(X)) {
        var <- X[[var_name]]
        
        liaisons <- numeric(private$FNbGroupes)
        for (k in 1:private$FNbGroupes) {
          if (!is.null(private$FVariablesSynthetiques[[k]])) {
            liaison_val <- private$calculerLiaison(var, private$FVariablesSynthetiques[[k]])
            if (is.finite(liaison_val)) {
              liaisons[k] <- liaison_val
            }
          }
        }
        
        cluster_pred <- which.max(liaisons)
        force_liaison <- liaisons[cluster_pred]
        
        resultats[[var_name]] <- data.frame(
          variable = var_name,
          type = ifelse(is.numeric(var), "quantitative", "qualitative"),
          indicateur = ifelse(is.numeric(var), "correlation_squared", "eta_squared"),
          cluster_predit = cluster_pred,
          force_liaison = force_liaison,
          interpretation = ifelse(force_liaison > 0.5, "forte liaison",
                                  ifelse(force_liaison > 0.2, "liaison modérée", "liaison faible")),
          stringsAsFactors = FALSE
        )
      }
      
      resultats_df <- do.call(rbind, resultats)
      rownames(resultats_df) <- NULL
      
      return(resultats_df)
    },
    
    print = function() {
      super$print()
      if (private$FFitted) {
        cat("Convergence :", ifelse(private$FConverged, "Oui", "Non"), "\n")
        cat("Itérations :", private$FIterations, "\n")
        cat("Homogénéité :", round(private$FHomogeneite, 4), "\n")
      }
      invisible(self)
    },
    
    summary = function() {
      super$summary()
      
      if (private$FFitted) {
        cat("\n--- Spécifique ClustOfVar ---\n")
        cat("Convergence :", ifelse(private$FConverged, "Oui", "Non"), "\n")
        cat("Nombre d'itérations :", private$FIterations, "/", private$FMaxIter, "\n")
        cat("Homogénéité globale :", round(private$FHomogeneite, 4), "\n")
        
        cat("\n--- Qualité des clusters ---\n")
        for (k in 1:private$FNbGroupes) {
          n_vars <- sum(private$FGroupes == k)
          cat("Cluster", k, ":", n_vars, "variable(s), qualité =", 
              round(private$FQualiteClusters[k], 4), "\n")
        }
        
        cat("\n--- Composition des clusters ---\n")
        for (k in 1:private$FNbGroupes) {
          vars_cluster <- names(private$FGroupes)[private$FGroupes == k]
          if (length(vars_cluster) > 0) {
            cat("Cluster", k, ":", paste(vars_cluster, collapse = ", "), "\n")
          }
        }
      }
      
      invisible(self)
    },
    
    variables_synthetiques = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      return(private$FVariablesSynthetiques)
    },
    
    matrice_correlations = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      return(private$FCorrelations)
    },
    
    qualite_clusters = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      
      qualites <- data.frame(
        cluster = 1:private$FNbGroupes,
        n_variables = as.numeric(table(factor(private$FGroupes, levels = 1:private$FNbGroupes))),
        qualite = private$FQualiteClusters
      )
      
      return(qualites)
    },
    
    plot = function(type = "heatmap") {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      
      if (type == "heatmap") {
        cor_mat <- private$FCorrelations
        ordre <- order(private$FGroupes)
        cor_mat_ordered <- cor_mat[ordre, ]
        
        image(1:ncol(cor_mat_ordered), 1:nrow(cor_mat_ordered),
              t(cor_mat_ordered),
              col = colorRampPalette(c("white", "lightblue", "blue", "darkblue"))(100),
              xlab = "Cluster", ylab = "Variables",
              main = "Heatmap : Liaison Variables/Clusters",
              axes = FALSE)
        
        axis(1, at = 1:ncol(cor_mat_ordered), labels = colnames(cor_mat_ordered))
        axis(2, at = 1:nrow(cor_mat_ordered), labels = rownames(cor_mat_ordered)[ordre], 
             las = 2, cex.axis = 0.7)
        
        legend("topright", legend = c("0", "0.5", "1"),
               fill = c("white", "blue", "darkblue"), title = "Liaison", cex = 0.8)
        
      } else if (type == "barplot") {
        qualites <- private$FQualiteClusters
        
        barplot(qualites, names.arg = paste("Cluster", 1:private$FNbGroupes),
                col = "steelblue", main = "Qualité des clusters",
                ylab = "Homogénéité moyenne", ylim = c(0, 1))
        abline(h = mean(qualites), col = "red", lty = 2, lwd = 2)
        legend("topright", legend = "Moyenne", col = "red", lty = 2, lwd = 2)
      }
      
      invisible(self)
    }
  ),
  
  active = list(
    Groupes = function() {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      return(private$FGroupes)
    },
    
    Homogeneite = function() {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      return(private$FHomogeneite)
    },
    
    Converged = function() {
      return(private$FConverged)
    }
  )
)