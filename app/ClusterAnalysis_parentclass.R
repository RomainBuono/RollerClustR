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