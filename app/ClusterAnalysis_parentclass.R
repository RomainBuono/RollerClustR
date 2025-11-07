# ==========================================
# Classe PARENTE : ClusterAnalysis
# ==========================================

ClusterAnalysis <- R6Class("ClusterAnalysis",
  private = list(
    FX = NULL,              # Données d'apprentissage
    FScale = TRUE,          # Standardisation 
    FNbGroupes = 2,
    FGroupes = c(),
    FDataType = "numeric",  # "numeric", "categorical", "mixed"
    FFitted = FALSE,        # Le modèle est-il ajusté ?
    
    # Détection du type de données
    detectDataType = function(data) {
      n_numeric <- sum(sapply(data, is.numeric))
      n_factor <- sum(sapply(data, is.factor))
      
      if (n_numeric == ncol(data)) {
        return("numeric")
      } else if (n_factor == ncol(data)) {
        return("categorical")
      } else {
        return("mixed")
      }
    }
  ),
  
  public = list(
    #' @description Initialiser l'objet (sans faire le clustering)
    #' @param k Nombre de groupes (défaut: 2)
    #' @param cr Standardiser les variables numériques ? (défaut: TRUE)
    initialize = function(k = 2, cr = TRUE) {
      private$FNbGroupes <- k
      private$FScale <- cr
      private$FFitted <- FALSE
      message("Objet initialisé. Utilisez $fit(X) pour lancer le clustering.")
    },
    
    #' @description Ajuster le modèle sur les données d'apprentissage
    #' @param X Data frame avec les variables actives
    fit = function(X) {
      stop("Méthode fit() doit être implémentée dans les classes filles")
    },
    
    #' @description Prédire/analyser des variables illustratives
    #' @param X Data frame avec les variables illustratives
    #' @return Data frame avec les indicateurs de liaison
    predict = function(X) {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      }
      
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      # Vérifier la compatibilité
      if (nrow(X) != length(private$FGroupes)) {
        stop("X doit avoir le même nombre d'observations que les données d'apprentissage (", 
             length(private$FGroupes), " observations)")
      }
      
      # Analyser chaque variable illustrative
      resultats <- list()
      
      for (var_name in names(X)) {
        var <- X[[var_name]]
        
        if (is.numeric(var)) {
          # Variable quantitative
          rapport_corr <- self$rapCorrIllusQuanti(var)
          
          # Calculer les valeurs-test
          m <- mean(var)
          n <- length(var)
          sigma <- sd(var)
          mg <- tapply(var, private$FGroupes, mean)
          ng <- tapply(var, private$FGroupes, length)
          
          vt <- (mg - m) / sqrt((n - ng) / (n - 1) * (sigma^2 / ng))
          # Ajout d'une sécurité pour éviter les divisions par 0
          vt[is.nan(vt) | is.infinite(vt)] <- 0
          
          # Groupe le plus caractéristique (|vtest| max)
          groupe_max <- which.max(abs(vt))
          
          resultats[[var_name]] <- data.frame(
            variable = var_name,
            type = "quantitative",
            indicateur = "rapport_correlation",
            valeur = rapport_corr,
            interpretation = ifelse(rapport_corr > 0.5, "forte liaison", 
                                   ifelse(rapport_corr > 0.2, "liaison modérée", "liaison faible")),
            groupe_max = groupe_max,
            vtest_max = vt[groupe_max],
            stringsAsFactors = FALSE
          )
          
        } else if (is.factor(var)) {
          # Variable qualitative
          cramer <- self$cramerIllusQuali(var)
          
          # Table de contingence
          tab <- table(private$FGroupes, var)
          
          # Trouver la modalité la plus discriminante
          chi2_contrib <- chisq.test(tab)$residuals^2
          idx_max <- which(chi2_contrib == max(chi2_contrib), arr.ind = TRUE)[1, ]
          
          resultats[[var_name]] <- data.frame(
            variable = var_name,
            type = "qualitative",
            indicateur = "V_Cramer",
            valeur = cramer,
            interpretation = ifelse(cramer > 0.5, "forte association", 
                                   ifelse(cramer > 0.2, "association modérée", "association faible")),
            groupe_max = idx_max[1],
            modalite_max = colnames(tab)[idx_max[2]],
            stringsAsFactors = FALSE
          )
        }
      }
      
      # Combiner en un seul data frame
      resultats_df <- do.call(rbind, resultats)
      rownames(resultats_df) <- NULL
      
      return(resultats_df)
    },
    
    #' @description Affichage succinct
    print = function() {
      cat("Classe :", class(self)[1], "\n")
      
      if (!private$FFitted) {
        cat("Statut : Non ajusté (utilisez $fit(X))\n")
      } else {
        cat("Statut : Ajusté\n")
        cat("Type de données :", private$FDataType, "\n")
        cat("Nombre d'observations :", length(private$FGroupes), "\n")
        cat("Nombre de variables :", ncol(private$FX), "\n")
        cat("Nombre de groupes :", private$FNbGroupes, "\n")
        cat("Effectifs par groupe :", paste(as.vector(table(private$FGroupes)), collapse = ", "), "\n")
      }
      
      invisible(self)
    },
    
    #' @description Affichage détaillé
    summary = function() {
      cat("========================================\n")
      cat("  RÉSUMÉ DU CLUSTERING\n")
      cat("========================================\n\n")
      
      cat("Algorithme :", class(self)[1], "\n")
      
      if (!private$FFitted) {
        cat("\n⚠ Le modèle n'a pas été ajusté.\n")
        cat("Utilisez $fit(X) pour ajuster le modèle.\n")
        return(invisible(self))
      }
      
      cat("Type de données :", private$FDataType, "\n")
      cat("Standardisation :", ifelse(private$FScale, "Oui", "Non"), "\n\n")
      
      cat("--- Données ---\n")
      cat("Nombre d'observations :", length(private$FGroupes), "\n")
      cat("Nombre de variables :", ncol(private$FX), "\n")
      if (!is.null(names(private$FX))) {
        cat("Variables :", paste(names(private$FX), collapse = ", "), "\n")
      }
      cat("\n")
      
      cat("--- Groupes ---\n")
      cat("Nombre de groupes :", private$FNbGroupes, "\n")
      cat("\nDistribution des groupes :\n")
      dist_table <- table(private$FGroupes)
      for (g in names(dist_table)) {
        pct <- round(dist_table[g] / length(private$FGroupes) * 100, 1)
        cat("  Groupe", g, ":", dist_table[g], "obs. (", pct, "%)\n", sep = " ")
      }
      
      cat("\n========================================\n")
      invisible(self)
    },
    
    # Méthodes communes (anciennes fonctions)
    affichage = function() {
      message("⚠ La méthode $affichage() est dépréciée. Utilisez $print() ou $summary() à la place.")
      self$summary()
    },
    
    rapCorrIllusQuanti = function(v) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      if (!is.numeric(v)) stop("Variable numérique uniquement")
      
      SCT <- sum((v - mean(v))^2)
      moyennes <- tapply(v, private$FGroupes, mean)
      effectifs <- tapply(v, private$FGroupes, length)
      SCE <- sum(effectifs * (moyennes - mean(v))^2)
      return(SCE / SCT)
    },
    
    cramerIllusQuali = function(v) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      if (!is.factor(v)) stop("Variable factor uniquement")
      
      k2 <- chisq.test(private$FGroupes, v)$statistic
      cramer <- sqrt(k2 / (length(v) * min(private$FNbGroupes - 1, nlevels(v) - 1)))
      return(cramer)
    },
    
    vTestIllusQuanti = function(v) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      if (!is.numeric(v)) stop("Variable numérique uniquement")
      
      m <- mean(v)
      n <- length(v)
      sigma <- sd(v)
      mg <- tapply(v, private$FGroupes, mean)
      ng <- tapply(v, private$FGroupes, length)
      vt <- (mg - m) / sqrt((n - ng) / (n - 1) * (sigma^2 / ng))
      # Ajout d'une sécurité pour éviter les divisions par 0
      vt[is.nan(vt) | is.infinite(vt)] <- 0
      
      cat("Moyennes conditionnelles et valeurs test\n")
      for (k in 1:length(ng)) {
        cat("Groupe :", k, "\tMoyenne :", round(mg[k], 3), "\tV-Test :", round(vt[k], 3), "\n")
      }
      invisible(vt)
    },
    
    vTestIllusQuali = function(v, numModa = 1) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord")
      if (!is.factor(v)) stop("Variable factor uniquement")
      
      m <- table(private$FGroupes, v)
      if (!(numModa %in% 1:ncol(m))) stop("Numéro de modalité invalide")
      
      n <- length(v)
      p <- colSums(m)[numModa] / sum(m)
      pg <- m[, numModa] / rowSums(m)
      ng <- tapply(v, private$FGroupes, length)
      vt <- sqrt(ng) * (pg - p) / sqrt((n - ng) / (n - 1) * p * (1 - p))
      
      cat("Proportion de référence :", round(p, 3), "\n")
      cat("Proportions conditionnelles et valeurs test\n")
      for (k in 1:length(ng)) {
        cat("Groupe :", k, "\tProportion :", round(pg[k], 3), "\tV-Test :", round(vt[k], 3), "\n")
      }
      invisible(vt)
    }
  ),
  
  active = list(
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      }
      return(private$FGroupes)
    },
    
    DataType = function() return(private$FDataType),
    
    Fitted = function() return(private$FFitted)
  )
)