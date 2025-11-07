# =========================================
# WRAPPERS R6 adaptés pour fit()/predict()
# =========================================

# =========================================
# FACTORY : Création d'objets de clustering
# =========================================

#' Factory pour créer des objets de clustering
#' @export
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    #' @description Créer et ajuster un objet CAH
    #' @param X Data frame avec variables numériques
    #' @param k Nombre de groupes (défaut: 2)
    #' @param cr Standardiser ? (défaut: TRUE)
    #' @param fit_now Ajuster immédiatement ? (défaut: TRUE)
    #' @return Objet CAH
    create_cah = function(X, k = 2, cr = TRUE, fit_now = TRUE) {
      obj <- CAH$new(k = k, cr = cr)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kmeans
    #' @param X Data frame avec variables numériques
    #' @param k Nombre de groupes (défaut: 3)
    #' @param cr Standardiser ? (défaut: TRUE)
    #' @param nstart Nombre d'initialisations (défaut: 10)
    #' @param fit_now Ajuster immédiatement ? (défaut: TRUE)
    #' @return Objet Kmeans
    create_kmeans = function(X, k = 3, cr = TRUE, nstart = 10, fit_now = TRUE) {
      obj <- Kmeans$new(k = k, cr = cr, nstart = nstart)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kprototypes
    #' @param X Data frame avec variables numériques et/ou catégorielles
    #' @param k Nombre de groupes (défaut: 3)
    #' @param cr Standardiser les variables numériques ? (défaut: TRUE)
    #' @param lambda Poids des variables catégorielles (défaut: 0.5)
    #' @param max_iter Nombre max d'itérations (défaut: 100)
    #' @param fit_now Ajuster immédiatement ? (défaut: TRUE)
    #' @return Objet Kprototypes
    create_kprototypes = function(X, k = 3, cr = TRUE, lambda = 0.5, max_iter = 100, fit_now = TRUE) {
      obj <- Kprototypes$new(k = k, cr = cr, lambda = lambda, max_iter = max_iter)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer automatiquement l'objet adapté au type de données
    #' @param X Data frame
    #' @param k Nombre de groupes (défaut: 3)
    #' @param method Pour données numériques : "cah" ou "kmeans" (défaut: "cah")
    #' @param cr Standardiser ? (défaut: TRUE)
    #' @param fit_now Ajuster immédiatement ? (défaut: TRUE)
    #' @param ... Arguments supplémentaires
    #' @return Objet CAH, Kmeans ou Kprototypes
    create_auto = function(X, k = 3, method = "cah", cr = TRUE, fit_now = TRUE, ...) {
      if (!is.data.frame(X)) stop("X doit être un data.frame")
      
      # Détection du type de données
      n_numeric <- sum(sapply(X, is.numeric))
      n_total <- ncol(X)
      
      # Données uniquement numériques
      if (n_numeric == n_total) {
        if (method == "cah") {
          message("Création d'un objet CAH (données numériques)")
          return(self$create_cah(X, k = k, cr = cr, fit_now = fit_now))
        } else if (method == "kmeans") {
          message("Création d'un objet Kmeans (données numériques)")
          return(self$create_kmeans(X, k = k, cr = cr, fit_now = fit_now, ...))
        }
      }
      # Données catégorielles ou mixtes
      else {
        message("Création d'un objet Kprototypes (données ", 
                if (n_numeric == 0) "catégorielles" else "mixtes", ")")
        return(self$create_kprototypes(X, k = k, cr = cr, fit_now = fit_now, ...))
      }
    }
  )
)


# =========================================
# EVALUATOR : Évaluation de clustering
# =========================================

#' Évaluateur de clustering
#' @export
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  private = list(
    FData = NULL,
    FResults = NULL
  ),
  
  public = list(
    #' @description Initialiser l'évaluateur
    #' @param X Data frame à analyser
    initialize = function(X) {
      if (!is.data.frame(X)) stop("X doit être un data.frame")
      private$FData <- X
      private$FResults <- list()
    },
    
    #' @description Évaluer plusieurs valeurs de k pour un algorithme
    #' @param k_range Vecteur des k à tester (défaut: 2:10)
    #' @param method "kmeans", "cah" ou "kprototypes" (défaut: "kmeans")
    #' @param ... Arguments supplémentaires
    #' @return Data frame avec résultats
    evaluate_k = function(k_range = 2:10, method = "kmeans", ...) {
      results <- data.frame(k = k_range)
      
      if (method == "kmeans") {
        inertie_expl <- numeric(length(k_range))
        inertie_intra <- numeric(length(k_range))
        inertie_inter <- numeric(length(k_range))
        
        factory <- ClusteringFactory$new()
        
        message("Évaluation de K-means pour k = ", min(k_range), " à ", max(k_range))
        
        for (i in seq_along(k_range)) {
          obj <- factory$create_kmeans(private$FData, k = k_range[i], fit_now = TRUE, ...)
          inertie_info <- obj$inertie()
          inertie_expl[i] <- inertie_info$pct_expliquee
          inertie_intra[i] <- inertie_info$intra
          inertie_inter[i] <- inertie_info$inter
          
          # Stocker l'objet
          private$FResults[[paste0("kmeans_k", k_range[i])]] <- obj
        }
        
        results$inertie_expliquee <- inertie_expl
        results$inertie_intra <- inertie_intra
        results$inertie_inter <- inertie_inter
        
      } else if (method == "cah") {
        message("Pour CAH, créez un objet et utilisez le dendrogramme pour choisir k")
        factory <- ClusteringFactory$new()
        
        for (i in seq_along(k_range)) {
          obj <- factory$create_cah(private$FData, k = k_range[i], fit_now = TRUE, ...)
          private$FResults[[paste0("cah_k", k_range[i])]] <- obj
        }
        
      } else if (method == "kprototypes") {
        message("Évaluation de K-prototypes pour k = ", min(k_range), " à ", max(k_range))
        factory <- ClusteringFactory$new()
        
        for (i in seq_along(k_range)) {
          obj <- factory$create_kprototypes(private$FData, k = k_range[i], fit_now = TRUE, ...)
          private$FResults[[paste0("kprototypes_k", k_range[i])]] <- obj
        }
      }
      
      return(results)
    },
    
    #' @description Visualiser les résultats de l'évaluation
    plot_evaluation = function(results, criterion = "inertie_expliquee") {
      if (!criterion %in% names(results)) {
        stop("Critère invalide")
      }
      
      plot(results$k, results[[criterion]], type = "b", 
           xlab = "Nombre de groupes (k)", 
           ylab = if (criterion == "inertie_expliquee") "% Inertie expliquée" else "Inertie intra-classe",
           main = paste("Évaluation :", criterion),
           col = "blue", pch = 19, lwd = 2)
      grid()
      
      if (criterion == "inertie_expliquee" && nrow(results) >= 3) {
        diffs <- diff(results[[criterion]])
        k_opt <- results$k[which(diffs < mean(diffs) * 0.5)[1]]
        if (!is.na(k_opt)) {
          abline(v = k_opt, col = "red", lty = 2, lwd = 2)
          text(k_opt, max(results[[criterion]]) * 0.9, 
               paste("k optimal ~", k_opt), col = "red", pos = 4)
        }
      }
    },
    
    #' @description Obtenir le meilleur k
    get_best_k = function(results, criterion = "inertie_expliquee") {
      if (criterion == "inertie_expliquee") {
        diffs <- diff(results[[criterion]])
        k_opt_idx <- which(diffs < mean(diffs) * 0.5)[1]
        if (!is.na(k_opt_idx)) {
          return(results$k[k_opt_idx])
        }
      } else if (criterion == "inertie_intra") {
        return(results$k[which.min(results[[criterion]])])
      }
      return(NA)
    },
    
    get_result = function(name) {
      return(private$FResults[[name]])
    },
    
    list_results = function() {
      return(names(private$FResults))
    }
  )
)


# =========================================
# COMPARATOR : Comparaison d'algorithmes
# =========================================

#' Comparateur d'algorithmes de clustering
#' @export
ClusteringComparator <- R6Class("ClusteringComparator",
  private = list(
    FData = NULL,
    FResults = NULL,
    FGroupes = NULL,
    FK = NULL
  ),
  
  public = list(
    initialize = function(X, k = 3) {
      if (!is.data.frame(X)) stop("X doit être un data.frame")
      private$FData <- X
      private$FK <- k
      private$FResults <- list()
      private$FGroupes <- list()
    },
    
    add_algorithm = function(algorithm, ...) {
      factory <- ClusteringFactory$new()
      
      obj <- switch(algorithm,
        cah = factory$create_cah(private$FData, k = private$FK, fit_now = TRUE, ...),
        kmeans = factory$create_kmeans(private$FData, k = private$FK, fit_now = TRUE, ...),
        kprototypes = factory$create_kprototypes(private$FData, k = private$FK, fit_now = TRUE, ...),
        stop("Algorithme inconnu")
      )
      
      private$FResults[[algorithm]] <- obj
      private$FGroupes[[algorithm]] <- obj$Groupes
      
      message("Algorithme '", algorithm, "' ajouté à la comparaison")
      invisible(self)
    },
    
    compare = function() {
      if (length(private$FResults) < 2) {
        stop("Ajoutez au moins 2 algorithmes avec add_algorithm()")
      }
      
      algos <- names(private$FResults)
      n_algos <- length(algos)
      
      cat("=== Comparaison de", n_algos, "algorithmes ===\n\n")
      
      for (algo in algos) {
        cat("Algorithme:", algo, "\n")
        print(table(private$FGroupes[[algo]]))
        cat("\n")
      }
      
      confusion_matrices <- list()
      
      if (n_algos == 2) {
        confusion <- table(private$FGroupes[[algos[1]]], private$FGroupes[[algos[2]]])
        colnames(confusion) <- paste0(algos[2], "_G", 1:private$FK)
        rownames(confusion) <- paste0(algos[1], "_G", 1:private$FK)
        confusion_matrices[[paste(algos[1], "vs", algos[2])]] <- confusion
        
        accord <- sum(apply(confusion, 1, max)) / sum(confusion)
        cat("Accord entre", algos[1], "et", algos[2], ":", 
            round(accord * 100, 2), "%\n")
      } else {
        for (i in 1:(n_algos - 1)) {
          for (j in (i + 1):n_algos) {
            confusion <- table(private$FGroupes[[algos[i]]], private$FGroupes[[algos[j]]])
            colnames(confusion) <- paste0(algos[j], "_G", 1:private$FK)
            rownames(confusion) <- paste0(algos[i], "_G", 1:private$FK)
            confusion_matrices[[paste(algos[i], "vs", algos[j])]] <- confusion
          }
        }
      }
      
      return(list(
        results = private$FResults,
        groupes = private$FGroupes,
        confusion = confusion_matrices
      ))
    },
    
    plot_comparison = function(var_x = 1, var_y = 2) {
      if (!all(sapply(private$FData, is.numeric))) {
        cat("Visualisation nécessite des variables numériques\n")
        return(invisible(self))
      }
      
      if (is.character(var_x)) var_x <- which(names(private$FData) == var_x)
      if (is.character(var_y)) var_y <- which(names(private$FData) == var_y)
      
      algos <- names(private$FResults)
      n_algos <- length(algos)
      
      if (n_algos <= 2) {
        par(mfrow = c(1, n_algos))
      } else if (n_algos <= 4) {
        par(mfrow = c(2, 2))
      } else {
        par(mfrow = c(2, 3))
      }
      
      for (algo in algos) {
        plot(private$FData[, var_x], private$FData[, var_y],
             col = private$FGroupes[[algo]], pch = 19,
             main = paste("Algorithme:", algo),
             xlab = names(private$FData)[var_x],
             ylab = names(private$FData)[var_y])
        legend("topright", legend = paste("G", 1:private$FK),
               col = 1:private$FK, pch = 19, cex = 0.8)
      }
      
      par(mfrow = c(1, 1))
      invisible(self)
    },
    
    get_result = function(algorithm) {
      return(private$FResults[[algorithm]])
    },
    
    list_algorithms = function() {
      return(names(private$FResults))
    }
  )
)


# =========================================
# HELPER : Fonctions utilitaires
# =========================================

#' Helper pour opérations communes
#' @export
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    get_clusters = function(object) {
      if (!inherits(object, c("CAH", "Kmeans", "Kprototypes"))) {
        stop("L'objet doit être de classe CAH, Kmeans ou Kprototypes")
      }
      return(object$Groupes)
    },
    
    export_results = function(object, original_data = NULL, include_data = TRUE) {
      groupes <- self$get_clusters(object)
      
      if (is.null(original_data)) {
        result <- data.frame(
          observation = 1:length(groupes),
          groupe = groupes
        )
      } else {
        if (include_data) {
          result <- data.frame(
            observation = if (!is.null(rownames(original_data))) rownames(original_data) else 1:nrow(original_data),
            groupe = groupes,
            original_data
          )
        } else {
          result <- data.frame(
            observation = if (!is.null(rownames(original_data))) rownames(original_data) else 1:nrow(original_data),
            groupe = groupes
          )
        }
      }
      
      return(result)
    },
    
    group_statistics = function(object, data) {
      groupes <- self$get_clusters(object)
      
      stats <- list()
      
      for (var_name in names(data)) {
        var <- data[[var_name]]
        
        if (is.numeric(var)) {
          stats[[var_name]] <- data.frame(
            groupe = unique(groupes),
            n = as.numeric(table(groupes)),
            moyenne = tapply(var, groupes, mean),
            ecart_type = tapply(var, groupes, sd),
            minimum = tapply(var, groupes, min),
            maximum = tapply(var, groupes, max)
          )
          rownames(stats[[var_name]]) <- NULL
          
        } else if (is.factor(var)) {
          tab <- table(groupes, var)
          stats[[var_name]] <- as.data.frame.matrix(tab)
        }
      }
      
      return(stats)
    },
    
    generate_report = function(object, file = NULL) {
      output <- character()
      
      output <- c(output, "===========================================")
      output <- c(output, "     RAPPORT DE CLUSTERING")
      output <- c(output, "===========================================\n")
      
      algo_type <- class(object)[1]
      output <- c(output, paste("Algorithme utilisé :", algo_type))
      output <- c(output, paste("Type de données     :", object$DataType))
      
      groupes <- self$get_clusters(object)
      output <- c(output, paste("Nombre de groupes   :", length(unique(groupes))))
      output <- c(output, paste("Nombre d'observations:", length(groupes)))
      
      output <- c(output, "\nDistribution des groupes :")
      dist_groupes <- table(groupes)
      for (g in names(dist_groupes)) {
        pct <- round(dist_groupes[g] / length(groupes) * 100, 1)
        output <- c(output, paste("  Groupe", g, ":", dist_groupes[g], "obs. (", pct, "%)"))
      }
      
      if (inherits(object, "Kmeans")) {
        inertie_info <- object$inertie()
        output <- c(output, "\n--- Inertie ---")
        output <- c(output, paste("Inertie expliquée :", round(inertie_info$pct_expliquee, 2), "%"))
      }
      
      output <- c(output, "\n===========================================\n")
      
      if (is.null(file)) {
        cat(paste(output, collapse = "\n"))
      } else {
        writeLines(output, file)
        message("Rapport sauvegardé dans : ", file)
      }
      
      invisible(output)
    }
  )
)