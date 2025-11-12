# =================================================================
# Classe PARENTE : ClusterAnalysis
# Définit la structure, l'interface publique, et les utilitaires de base
# =================================================================

# Nécessite le package R6
library(R6) 

ClusterAnalysis <- R6Class("ClusterAnalysis",
  
  # =================================================================
  # 1. Champs Privés
  # =================================================================
  private = list(
    FX = NULL,           # Données originales d'apprentissage (y compris les NA si existants)
    FScale = TRUE,       # Booléen : standardisation
    FNbGroupes = 2,      # Nombre actuel de groupes (k)
    FGroupes = c(),      # Vecteur des groupes (pour les lignes nettoyées)
    FDataType = "numeric", # Type de données : "numeric", "categorical", "mixed"
    FFitted = FALSE,     # Le modèle est-il ajusté ?
    FNAAction = "warn",  # Action pour les NA : "warn", "fail", "omit"
    FHasMissing = FALSE, # Le dataset contenait-il des NA ?
    FNAIndices = NULL,   # Indices des lignes avec NA dans FX
    
    # =================================================================
    # 2. Utilitaires de Données
    # =================================================================
    
    # Validation complète du dataset
    validateDataset = function(X) {
      if (!is.data.frame(X)) {
        if (is.matrix(X)) {
          X <- as.data.frame(X, stringsAsFactors = TRUE)
        } else {
          stop("Les données doivent être un data frame ou une matrice.")
        }
      }
      
      # Vérifier les types et stocker les infos de base
      col_types <- sapply(X, class)
      
      if (all(col_types %in% c("numeric", "integer"))) {
        private$FDataType <- "numeric"
      } else if (all(col_types %in% c("factor", "character"))) {
        private$FDataType <- "categorical"
      } else {
        private$FDataType <- "mixed"
      }
      
      return(X)
    },
    
    # Nettoyage selon l'action NA (omit/warn/fail)
    cleanDataset = function(X) {
      if (any(is.na(X))) {
        private$FHasMissing <- TRUE
        
        # Trouver les lignes complètes
        idx_clean <- which(complete.cases(X))
        
        if (private$FNAAction == "omit") {
          private$FNAIndices <- setdiff(1:nrow(X), idx_clean)
          message(paste0(length(private$FNAIndices), " lignes avec des valeurs manquantes ont été omises (na_action = 'omit')."))
          return(X[idx_clean, , drop = FALSE])
          
        } else if (private$FNAAction == "fail") {
          stop("Des valeurs manquantes ont été trouvées et na_action = 'fail'. L'exécution a été arrêtée.")
          
        } else { # warn (défaut)
          warning("Des valeurs manquantes ont été trouvées. Par défaut (na_action = 'warn'), les lignes avec des NA seront ignorées par l'algorithme, mais les groupes finaux ($Groupes) incluront des NA pour ces observations.")
          return(X[idx_clean, , drop = FALSE]) # Comportement par défaut: ignore pour le fit
        }
      }
      return(X)
    },
    
    # Standardisation (Centrage-Réduction)
    standardize = function(X, is_new_data = FALSE) {
      if (private$FDataType != "numeric") {
        # Si c'est un type mixte ou catégoriel, la standardisation est gérée
        # par la classe enfant (Kprototypes, etc.) ou ignorée.
        return(X)
      }
      
      if (!private$FScale) {
        return(X)
      }
      
      # Si ce sont les données d'apprentissage (fit)
      if (!is_new_data) {
        # Calculer et stocker les paramètres pour la prédiction future
        private$FMeans <- colMeans(X, na.rm = TRUE)
        private$FSDs <- apply(X, 2, sd, na.rm = TRUE)
      }
      
      # Appliquer la standardisation
      X_scaled <- sweep(X, 2, private$FMeans, "-")
      X_scaled <- sweep(X_scaled, 2, private$FSDs, "/")
      
      # Gérer les colonnes à variance nulle (SD=0)
      zero_sd_cols <- which(private$FSDs == 0)
      if (length(zero_sd_cols) > 0) {
        X_scaled[, zero_sd_cols] <- 0
      }
      
      return(X_scaled)
    },
    
    # ------------------------------------------------
    # CONTRAT D'HÉRITAGE : Doit être surchargé par l'enfant
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      stop("La méthode privée $refit_with_k() doit être implémentée par la classe enfant pour permettre la modification dynamique de $K.")
    }
  ),
  
  # =================================================================
  # 3. Méthodes Publiques (Interface)
  # =================================================================
  public = list(
    
    #' @description Initialiser
    #' @param k Nombre de groupes
    #' @param cr Booléen, centrer et réduire les données numériques
    #' @param na_action Action en cas de NA : "warn" (défaut), "fail", "omit"
    initialize = function(k = 2, cr = TRUE, na_action = "warn", ...) {
      private$FNbGroupes <- as.integer(k)
      private$FScale <- cr
      private$FNAAction <- tolower(na_action)
      
      # Vérification de l'action NA
      if (!private$FNAAction %in% c("warn", "fail", "omit")) {
        warning("na_action non reconnu. Utilisation de 'warn' par défaut.")
        private$FNAAction <- "warn"
      }
    },
    
    #' @description Ajuster le modèle aux données
    #' @param X Data frame ou matrice de données
    #' @return L'objet lui-même (invisible)
    fit = function(X) {
      private$FX <- X
      X_clean <- private$validateDataset(X)
      X_clean <- private$cleanDataset(X_clean)
      
      # Le fit réel de l'algorithme doit être dans la classe enfant
      # La standardisation (si 'numeric') sera appelée dans l'enfant
      
      # Stocker les données propres (utilisées par standardize)
      private$FX_clean <- X_clean 
      
      invisible(X_clean)
    },
    
    #' @description Prédire les groupes de nouvelles observations
    predict = function(newdata) {
      stop("La méthode $predict() doit être implémentée par la classe enfant.")
    },
    
    #' @description Résumé du modèle
    summary = function() {
      stop("La méthode $summary() doit être implémentée par la classe enfant.")
    },
    
    #' @description Obtenir les mesures d'inertie/qualité du clustering
    inertie = function() {
      stop("La méthode $inertie() doit être implémentée par la classe enfant.")
    }
  ),
  
  # =================================================================
  # 4. Active Bindings (Propriétés dynamiques)
  # =================================================================
  active = list(
    
    #' @field X Données d'apprentissage originales
    X = function() private$FX,
    
    #' @field K Nombre de groupes souhaités
    K = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!private$FFitted) {
          stop("Impossible de modifier K tant que le modèle n'a pas été ajusté avec $fit().")
        }
        if (!is.numeric(value) || value < 2) {
          stop("K doit être un nombre entier >= 2.")
        }
        value <- as.integer(value)
        
        # Appel du contrat d'héritage : l'enfant doit gérer le re-fit
        private$refit_with_k(value)
        
        return(private$FNbGroupes)
      }
    },
    
    #' @field Groupes Groupes des observations. Inclut NA si na_action="omit"
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      # Réaffectation des groupes aux observations initiales si NA ont été omis
      if (private$FNAAction == "omit" && private$FHasMissing) {
        result <- rep(NA_integer_, nrow(private$FX))
        idx_clean <- setdiff(1:nrow(private$FX), private$FNAIndices)
        
        if (length(idx_clean) != length(private$FGroupes)) {
          # MODIFICATION : Remplacement de warning() par stop() pour l'intégrité
          stop("Erreur interne critique: La taille des groupes ajustés est incohérente avec les observations propres. Veuillez vérifier l'implémentation de la méthode fit() de la classe enfant.") 
        }
        
        result[idx_clean] <- private$FGroupes
        return(result)
        
      } else {
        # Pas d'omission de NA, ou pas de NA : retourne les groupes propres
        return(private$FGroupes)
      }
    }
  )
)