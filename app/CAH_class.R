# ==========================================
# CAH + K-means : Classification Hybride
# Variables quantitatives uniquement
# VERSION MODIFIÉE : Résiliente aux données manquantes
# ==========================================

CAH_Kmeans <- R6Class("CAH_Kmeans",
 inherit = ClusterAnalysis,
 
 private = list(
  FArbre = NULL,
  FMethod = "ward.D2",
  FKmeansResult = NULL,
  FNstart = 10,
  FMaxIter = 100,
  FCentroides = NULL,
  FInertie = NULL,
  FUseKmeans = TRUE # Activer/désactiver le raffinement K-means
 ),
 
 public = list(
  #' @description Initialiser un objet CAH + K-means hybride
  #' @param k Nombre de groupes (défaut: 2)
  #' @param cr Standardiser les données ? (défaut: TRUE)
  #' @param method Méthode de linkage CAH (défaut: "ward.D2")
  #' @param use_kmeans Utiliser le raffinement K-means ? (défaut: TRUE)
  #' @param nstart Nombre d'initialisations K-means (défaut: 10)
  #' @param max_iter Nombre max d'itérations K-means (défaut: 100)
  #' @param na_action Action pour les NA : "warn" (défaut), "fail", "omit"
  initialize = function(k = 2, cr = TRUE, method = "ward.D2", 
            use_kmeans = TRUE, nstart = 10, max_iter = 100,
            na_action = "warn") {
   super$initialize(k = k, cr = cr, na_action = na_action)
   
   # Validation de method
   valid_methods <- c("ward.D", "ward.D2", "single", "complete", "average", 
            "mcquitty", "median", "centroid")
   if (!method %in% valid_methods) {
    stop("Méthode de linkage CAH non valide : '", method, "'\n",
      "Méthodes valides : ", paste(valid_methods, collapse = ", "))
   }
   
   # Validation de use_kmeans
   if (!is.logical(use_kmeans) || length(use_kmeans) != 1) {
    stop("Le paramètre 'use_kmeans' doit être TRUE ou FALSE")
   }
   
   # Validation de nstart
   if (!is.numeric(nstart) || length(nstart) != 1 || nstart < 1) {
    stop("Le paramètre 'nstart' doit être un entier positif. Valeur reçue : ", nstart)
   }
   nstart <- as.integer(nstart)
   
   # Validation de max_iter
   if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter < 1) {
    stop("Le paramètre 'max_iter' doit être un entier positif. Valeur reçue : ", max_iter)
   }
   max_iter <- as.integer(max_iter)
   
   if (max_iter > 10000) {
    warning("max_iter très élevé (", max_iter, "). Cela peut ralentir le calcul.")
   }
   
   private$FMethod <- method
   private$FUseKmeans <- use_kmeans
   private$FNstart <- nstart
   private$FMaxIter <- max_iter
  },
  
  #' @description Ajuster le modèle CAH + K-means sur les données
  #' @param X Data frame avec variables numériques uniquement
  fit = function(X) {
   # === VALIDATION COMPLÈTE DU DATASET ===
   tryCatch({
    X <- private$validateDataset(X)
    X <- private$validateColumnTypes(X)
   }, error = function(e) {
    stop("Erreur de validation du dataset : ", e$message)
   })
   
   # Vérifier que toutes les variables sont numériques
   non_numeric_cols <- names(X)[!sapply(X, is.numeric)]
   if (length(non_numeric_cols) > 0) {
    stop("CAH_Kmeans accepte uniquement des variables numériques.\n",
      "Variables non-numériques détectées : ", paste(non_numeric_cols, collapse = ", "),
      "\n\nOptions :\n",
      " 1. Supprimez ces variables\n",
      " 2. Convertissez-les en numériques\n",
      " 3. Utilisez Kprototypes pour des données mixtes")
   }
   
   # Vérifier les valeurs infinies
   inf_cols <- names(X)[sapply(X, function(col) any(is.infinite(col), na.rm = TRUE))]
   if (length(inf_cols) > 0) {
    stop("Les colonnes suivantes contiennent des valeurs infinies (Inf/-Inf) : ",
      paste(inf_cols, collapse = ", "),
      "\nRemplacez ou supprimez ces valeurs avant le clustering.")
   }
   
   # === GESTION DES DONNÉES MANQUANTES ===
   X_clean <- private$handleMissingData(X, private$FNAAction)
   
   # Vérifier qu'il reste assez de données
   if (nrow(X_clean) < private$FNbGroupes) {
    stop("Après traitement des NA, il reste moins d'observations que de groupes demandés.\n",
      "Observations restantes : ", nrow(X_clean), 
      " - Groupes demandés : ", private$FNbGroupes,
      "\n\nOptions :\n",
      " 1. Réduire k : model <- CAH_Kmeans$new(k = ", min(2, nrow(X_clean) - 1), ")\n",
      " 2. Traiter les NA différemment\n",
      " 3. Ajouter plus de données")
   }
   
   if (nrow(X_clean) < 10) {
    warning("Très peu d'observations après traitement des NA (", nrow(X_clean), 
        "). Les résultats peuvent être instables.")
   }
   
   # Vérifier le ratio k/n
   if (private$FNbGroupes > nrow(X_clean) / 2) {
    warning("Le nombre de groupes (k=", private$FNbGroupes, ") est très élevé par rapport au nombre d'observations (n=",
        nrow(X_clean), "). Ratio k/n = ", round(private$FNbGroupes / nrow(X_clean), 2),
        "\nConsidérez réduire k pour des résultats plus robustes.")
   }
   
   # Stocker les données
   private$FX <- X_clean
   private$FDataType <- private$detectDataType(X_clean)
   
   # Standardisation avec gestion des colonnes constantes
   Z <- if (private$FScale) {
    tryCatch({
     scale(X_clean)
    }, warning = function(w) {
     # Si une colonne est constante, scale() produit un warning
     message("⚠ Certaines variables sont constantes après traitement des NA")
     # Standardiser manuellement
     X_scaled <- X_clean
     for (i in 1:ncol(X_clean)) {
      sd_val <- sd(X_clean[, i], na.rm = TRUE)
      if (sd_val > 1e-10) {
       X_scaled[, i] <- (X_clean[, i] - mean(X_clean[, i], na.rm = TRUE)) / sd_val
      } else {
       X_scaled[, i] <- 0 # Colonne constante
      }
     }
     as.matrix(X_scaled)
    })
   } else {
    as.matrix(X_clean)
   }
   
   # ÉTAPE 1 : CAH pour initialisation
   message("Étape 1/2 : Classification Ascendante Hiérarchique...")
   d <- dist(Z)
   
   # Vérification de la matrice de distances
   if (any(!is.finite(as.matrix(d)))) {
    stop("La matrice de distances contient des valeurs non-finies.",
      "\nCela peut être dû à des colonnes constantes après traitement des NA.")
   }
   
   private$FArbre <- hclust(d, method = private$FMethod)
   groupes_cah <- cutree(private$FArbre, k = private$FNbGroupes)
   
   if (!private$FUseKmeans) {
    # Si pas de raffinement K-means, utiliser directement CAH
    private$FGroupes <- groupes_cah
    private$FFitted <- TRUE
    message("✓ Modèle CAH ajusté (", private$FNbGroupes, " groupes)")
    return(invisible(self))
   }
   
   # ÉTAPE 2 : K-means avec centres initialisés par CAH
   message("Étape 2/2 : Raffinement par K-means...")
   
   # Calculer les centres initiaux à partir des groupes CAH
   centres_init <- matrix(0, nrow = private$FNbGroupes, ncol = ncol(Z))
   for (k in 1:private$FNbGroupes) {
    idx_k <- which(groupes_cah == k)
    if (length(idx_k) > 0) {
     if (length(idx_k) == 1) {
      centres_init[k, ] <- Z[idx_k, ]
     } else {
      centres_init[k, ] <- colMeans(Z[idx_k, , drop = FALSE], na.rm = TRUE)
     }
    } else {
     # Groupe vide : utiliser une observation aléatoire
     centres_init[k, ] <- Z[sample(1:nrow(Z), 1), ]
    }
   }
   
   # K-means avec centres initialisés
   tryCatch({
    private$FKmeansResult <- kmeans(Z, 
                   centers = centres_init,
                   iter.max = private$FMaxIter,
                   nstart = 1, # 1 seul car on a déjà de bons centres
                   algorithm = "Hartigan-Wong")
    
    private$FGroupes <- private$FKmeansResult$cluster
    private$FCentroides <- private$FKmeansResult$centers
    
    # Calculer l'inertie
    private$FInertie <- list(
     totale = private$FKmeansResult$totss,
     intra = private$FKmeansResult$tot.withinss,
     inter = private$FKmeansResult$betweenss,
     pct_expliquee = (private$FKmeansResult$betweenss / private$FKmeansResult$totss) * 100
    )
    
   }, error = function(e) {
    warning("Erreur K-means, utilisation des groupes CAH: ", e$message)
    private$FGroupes <- groupes_cah
   })
   
   # Marquer comme ajusté
   private$FFitted <- TRUE
   
   message("✓ Modèle CAH+K-means ajusté avec succès (", private$FNbGroupes, " groupes)")
   if (!is.null(private$FInertie)) {
    message(" Inertie expliquée : ", round(private$FInertie$pct_expliquee, 2), "%")
   }
   
   invisible(self)
  },
  
  #' @description Affichage succinct spécifique à CAH+K-means
  print = function() {
   super$print()
   if (private$FFitted) {
    cat("Méthode de linkage CAH :", private$FMethod, "\n")
    cat("Raffinement K-means :", ifelse(private$FUseKmeans, "Oui", "Non"), "\n")
    if (!is.null(private$FInertie)) {
     cat("Inertie expliquée :", round(private$FInertie$pct_expliquee, 2), "%\n")
    }
   }
   invisible(self)
  },
  
  #' @description Affichage détaillé spécifique à CAH+K-means
  summary = function() {
   super$summary()
   
   if (private$FFitted) {
    cat("\n--- Spécifique CAH + K-means ---\n")
    cat("Méthode de linkage CAH :", private$FMethod, "\n")
    cat("Hauteur de fusion CAH :", round(tail(private$FArbre$height, 1), 3), "\n")
    cat("Raffinement K-means :", ifelse(private$FUseKmeans, "Oui", "Non"), "\n")
    
    if (!is.null(private$FInertie)) {
     cat("\n--- Inertie (K-means) ---\n")
     cat("Inertie totale :", round(private$FInertie$totale, 3), "\n")
     cat("Inertie inter-classe :", round(private$FInertie$inter, 3), "\n")
     cat("Inertie intra-classe :", round(private$FInertie$intra, 3), "\n")
     cat("% expliqué :", round(private$FInertie$pct_expliquee, 2), "%\n")
    }
   }
   
   invisible(self)
  },
  
  #' @description Visualiser le dendrogramme CAH
  #' @param showGroups Afficher les groupes ? (défaut: FALSE)
  plot = function(showGroups = FALSE) {
   if (!private$FFitted) {
    stop("Le modèle doit être ajusté avec $fit() d'abord")
   }
   
   plot(private$FArbre, 
     main = "Dendrogramme CAH (avant raffinement K-means)", 
     xlab = "", sub = "")
   if (showGroups) {
    rect.hclust(private$FArbre, k = private$FNbGroupes, border = "red")
   }
   invisible(self)
  },
  
  #' @description Obtenir les informations d'inertie
  #' @return Liste avec les inerties
  inertie = function() {
   if (!private$FFitted) {
    stop("Le modèle doit être ajusté avec $fit() d'abord")
   }
   
   if (is.null(private$FInertie)) {
    stop("Inertie non disponible (K-means n'a pas été appliqué)")
   }
   
   return(private$FInertie)
  },
  
  #' @description Obtenir les centroïdes des groupes
  #' @return Matrice des centroïdes
  centroides = function() {
   if (!private$FFitted) {
    stop("Le modèle doit être ajusté avec $fit() d'abord")
   }
   
   if (is.null(private$FCentroides)) {
    # Calculer les centroïdes si pas disponibles
    Z <- if (private$FScale) {
     tryCatch({
      scale(private$FX)
     }, error = function(e) {
      as.matrix(private$FX)
     })
    } else {
     as.matrix(private$FX)
    }
    
    centres <- matrix(0, nrow = private$FNbGroupes, ncol = ncol(Z))
    for (k in 1:private$FNbGroupes) {
     idx_k <- which(private$FGroupes == k)
     if (length(idx_k) > 0) {
      centres[k, ] <- colMeans(Z[idx_k, , drop = FALSE], na.rm = TRUE)
     }
    }
    colnames(centres) <- colnames(private$FX)
    return(centres)
   }
   
   centres <- private$FCentroides
   colnames(centres) <- colnames(private$FX)
   return(centres)
  },
  
  #' @description Visualiser les groupes dans un espace 2D
  #' @param var_x Index ou nom de la variable X
  #' @param var_y Index ou nom de la variable Y
  plot_groups = function(var_x = 1, var_y = 2) {
   if (!private$FFitted) {
    stop("Le modèle doit être ajusté avec $fit() d'abord")
   }
   
   if (is.character(var_x)) var_x <- which(names(private$FX) == var_x)
   if (is.character(var_y)) var_y <- which(names(private$FX) == var_y)
   
   plot(private$FX[, var_x], private$FX[, var_y],
     col = private$FGroupes, pch = 19,
     xlab = names(private$FX)[var_x],
     ylab = names(private$FX)[var_y],
     main = "Groupes CAH+K-means")
   
   # Ajouter les centroïdes si disponibles
   if (!is.null(private$FCentroides)) {
    centres_originaux <- private$FCentroides
    if (private$FScale) {
     # Dé-standardiser les centres
     tryCatch({
      centres_originaux <- sweep(private$FCentroides, 2, 
                   attr(scale(private$FX), "scaled:scale"), "*")
      centres_originaux <- sweep(centres_originaux, 2, 
                   attr(scale(private$FX), "scaled:center"), "+")
     }, error = function(e) {
      # Si erreur, utiliser les centres standardisés
      centres_originaux <- private$FCentroides
     })
    }
    points(centres_originaux[, var_x], centres_originaux[, var_y],
       col = 1:private$FNbGroupes, pch = 8, cex = 2, lwd = 2)
   }
   
   legend("topright", legend = paste("Groupe", 1:private$FNbGroupes),
      col = 1:private$FNbGroupes, pch = 19)
   
   invisible(self)
  }
 ),
 
 active = list(
  #' @field NbGroupes Définir le nombre de groupes (setter uniquement)
  NbGroupes = function(value) {
   # --- Début du Setter (modification du nombre de groupes) ---
   
   # Le getter est supprimé ici. Si 'value' est manquant, c'est une tentative de lecture.
   if (missing(value)) {
    stop("La propriété active 'NbGroupes' est en mode 'setter' uniquement pour la modification. Utilisez une méthode ou une propriété publique différente si vous souhaitez lire la valeur (ex: $k).")
   }
   
   # Reste du SETTER
   
   # [Validation/Vérification]
   if (!private$FFitted) {
    stop("Le modèle doit être ajusté avec $fit() d'abord")
   }
   private$FNbGroupes <- value
   
   # Recalculer avec CAH
   groupes_cah <- cutree(private$FArbre, k = value)
   
   if (private$FUseKmeans && !is.null(private$FX)) {
    # Refaire K-means avec nouveau k
    Z <- if (private$FScale) {
     tryCatch({ scale(private$FX) }, error = function(e) { as.matrix(private$FX) })
    } else {
     as.matrix(private$FX)
    }
    
    centres_init <- matrix(0, nrow = value, ncol = ncol(Z))
    for (k in 1:value) {
     idx_k <- which(groupes_cah == k)
     if (length(idx_k) > 0) {
      centres_init[k, ] <- colMeans(Z[idx_k, , drop = FALSE], na.rm = TRUE)
     } else {
      # Gestion des groupes vides (très peu probable mais sécuritaire)
      centres_init[k, ] <- Z[sample(1:nrow(Z), 1), ] 
     }
    }
    
    tryCatch({
     private$FKmeansResult <- kmeans(Z, centers = centres_init,
                      iter.max = private$FMaxIter,
                      nstart = 1)
     private$FGroupes <- private$FKmeansResult$cluster
     private$FCentroides <- private$FKmeansResult$centers
     
     private$FInertie <- list(
      totale = private$FKmeansResult$totss,
      intra = private$FKmeansResult$tot.withinss,
      inter = private$FKmeansResult$betweenss,
      pct_expliquee = (private$FKmeansResult$betweenss / private$FKmeansResult$totss) * 100
     )
    }, error = function(e) {
     warning("Erreur K-means: ", e$message)
     private$FGroupes <- groupes_cah
    })
   } else {
    # Si K-means est désactivé, juste mettre à jour les groupes CAH
    private$FGroupes <- groupes_cah
   }
   
   message("Nombre de groupes modifié : k = ", value)
   
   # --- Fin du Setter ---
  }
 )
)