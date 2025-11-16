# =============================================================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering de VARIABLES
# =============================================================================

#' @importFrom R6 R6Class
NULL

# =============================================================================
# 1. ClusteringFactory (Création des objets)
# =============================================================================

#' Factory pour créer et initialiser les objets de clustering de variables
#' 
#' @title ClusteringFactory
#' @description Factory pour creer et initialiser les objets de clustering de variables
#' @export
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser la Factory
    initialize = function() {
      message("ClusteringFactory initialisee - Clustering de variables uniquement")
    },
    
    #' @description Creer et ajuster un objet VAR_CAH (Clustering de Variables - CAH)
    #' @param X Data frame ou matrice de donnees
    #' @param k Nombre de clusters (defaut: 3)
    #' @param scale Booleen, standardiser les donnees (defaut: TRUE)
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet VAR_CAH
    create_var_cah = function(X, k = 3, scale = TRUE, fit_now = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("k doit etre un nombre entier >= 2.")
      }
      if (!is.logical(scale)) {
        stop("scale doit etre un booleen (TRUE/FALSE).")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- VAR_CAH$new(scale = scale, k = k)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Creer et ajuster un objet KmodesVarClust
    #' @param X Data frame ou matrice de donnees
    #' @param k Nombre de clusters (defaut: 3)
    #' @param max_iter Nombre maximum d'iterations (defaut: 100)
    #' @param n_bins Nombre d'intervalles pour discretisation (defaut: 5)
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet KmodesVarClust
    create_kmodes_varclust = function(X, k = 3, max_iter = 100, n_bins = 5, fit_now = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("k doit etre un nombre entier >= 2.")
      }
      if (!is.numeric(max_iter) || max_iter < 1) {
        stop("max_iter doit etre >= 1.")
      }
      if (!is.numeric(n_bins) || n_bins < 2) {
        stop("n_bins doit etre un nombre entier >= 2.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- KmodesVarClust$new(k = k, max_iter = max_iter, n_bins = n_bins)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Creer et ajuster un objet VARCLUS (Clustering de Variables - Descendant)
    #' @param X Data frame ou matrice de donnees
    #' @param stop_eigenvalue Seuil d'arret pour lambda2 (defaut: 1.0)
    #' @param distance_metric Metrique de distance (defaut: "correlation")
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet VARCLUS
    create_varclus = function(X, stop_eigenvalue = 1.0, distance_metric = "correlation", fit_now = TRUE) {
      if (!is.numeric(stop_eigenvalue) || stop_eigenvalue < 0) {
        stop("stop_eigenvalue doit etre un nombre >= 0.")
      }
      if (!is.character(distance_metric)) {
        stop("distance_metric doit etre une chaine de caracteres.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- VARCLUS$new(stop_eigenvalue = stop_eigenvalue, 
                        distance_metric = distance_metric)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    }
  )
)

# =============================================================================
# 2. ClusteringComparator (Comparaison de modeles)
# =============================================================================

#' Outil pour comparer les performances de differents objets de clustering
#' 
#' @title ClusteringComparator
#' @description Outil pour comparer les performances de differents objets de clustering
#' @export
ClusteringComparator <- R6Class("ClusteringComparator",
  public = list(
    
    #' @description Initialiser le Comparator
    initialize = function() {
      message("ClusteringComparator initialise")
    },
    
    #' @description Comparer les inerties de plusieurs modeles
    #' @param ... Objets de clustering a comparer (ajustes avec fit())
    #' @return Un data frame de comparaison
    compare_inertia = function(...) {
      models <- list(...)
      
      if (length(models) == 0) {
        stop("Au moins un modele doit etre fourni pour la comparaison")
      }
      
      results <- list()
      
      for (i in seq_along(models)) {
        model <- models[[i]]
        model_name <- paste0("Modele ", i, " (", class(model)[1], ")")
        
        inertia_data <- tryCatch({
          if (!"inertie" %in% names(model)) {
            stop(paste0("Le modele ", class(model)[1], 
                       " ne possede pas de methode $inertie()"))
          }
          
          inertie <- model$inertie()
          
          if (!all(c("totale", "intra", "inter", "pct_expliquee") %in% names(inertie))) {
            stop("La methode $inertie() ne retourne pas la structure attendue")
          }
          
          data.frame(
            Modele = model_name,
            K = model$K,
            Inertie_Totale = round(inertie$totale, 4),
            Inertie_Intra = round(inertie$intra, 4),
            Inertie_Inter = round(inertie$inter, 4),
            Pct_Expliquee = round(inertie$pct_expliquee, 2),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          warning(paste0("Erreur lors de l'extraction de l'inertie pour ", 
                        model_name, ": ", e$message))
          data.frame(
            Modele = model_name,
            K = NA,
            Inertie_Totale = NA,
            Inertie_Intra = NA,
            Inertie_Inter = NA,
            Pct_Expliquee = NA,
            stringsAsFactors = FALSE
          )
        })
        
        results[[i]] <- inertia_data
      }
      
      comparison <- do.call(rbind, results)
      
      cat("\n===============================================================\n")
      cat("   COMPARAISON DES MODELES - Inertie\n")
      cat("===============================================================\n\n")
      print(comparison, row.names = FALSE)
      cat("\n")
      
      return(comparison)
    },
    
    #' @description Identifier le meilleur modele selon un critere
    #' @param comparison_df Data frame retourne par compare_inertia()
    #' @param criterion Critere de selection (defaut: "Pct_Expliquee")
    #' @return Indice du meilleur modele
    get_best_model = function(comparison_df, criterion = "Pct_Expliquee") {
      if (!criterion %in% names(comparison_df)) {
        stop(paste0("Le critere '", criterion, "' n'existe pas dans le data frame"))
      }
      
      values <- comparison_df[[criterion]]
      
      valid_idx <- which(!is.na(values))
      if (length(valid_idx) == 0) {
        stop("Aucune valeur valide trouvee pour le critere specifie")
      }
      
      if (criterion %in% c("Pct_Expliquee", "Inertie_Inter")) {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      } else if (criterion == "Inertie_Intra") {
        best_idx <- valid_idx[which.min(values[valid_idx])]
      } else {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      }
      
      cat("\nMeilleur modele selon", criterion, ":\n")
      cat("->", comparison_df$Modele[best_idx], "\n")
      cat("  Valeur:", values[best_idx], "\n\n")
      
      return(best_idx)
    }
  )
)

# =============================================================================
# 3. ClusteringEvaluator (Evaluation du nombre de clusters)
# =============================================================================

#' Outil pour evaluer le nombre optimal de clusters de variables
#' 
#' @title ClusteringEvaluator
#' @description Outil pour evaluer le nombre optimal de clusters de variables
#' @export
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  private = list(
    FData = NULL
  ),
  
  public = list(
    
    #' @description Initialiser l'Evaluator avec les donnees (optionnel)
    #' @param data Data frame de donnees (optionnel)
    initialize = function(data = NULL) {
      if (!is.null(data)) {
        private$FData <- data
      }
      message("ClusteringEvaluator initialise - Clustering de variables")
    },
    
    #' @description Methode du coude (Elbow Method)
    #' @param X Data frame de donnees
    #' @param max_k Nombre maximal de clusters a tester (defaut: 10)
    #' @param method Methode de clustering (defaut: "var_cah")
    #' @param ... Parametres additionnels pour la methode
    #' @return Liste contenant le graphique (ggplot) et les donnees
    elbow_method = function(X, max_k = 10, method = "var_cah", ...) {
      
      if (max_k < 2) {
        stop("max_k doit etre >= 2")
      }
      
      if (!method %in% c("var_cah", "kmodes_varclust")) {
        stop(paste0("Methode non reconnue: ", method))
      }
      
      factory <- ClusteringFactory$new()
      inerties <- numeric(max_k - 1)
      k_values <- 2:max_k
      
      cat("\n===============================================================\n")
      cat("   METHODE DU COUDE - Evaluation de k\n")
      cat("===============================================================\n\n")
      cat("Methode :", method, "\n")
      cat("Plage de k :", min(k_values), "a", max(k_values), "\n\n")
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        cat("-> Evaluation pour k =", k, "... ")
        
        model <- tryCatch({
          if (method == "var_cah") {
            suppressMessages(factory$create_var_cah(X, k = k, fit_now = TRUE, ...))
          } else if (method == "kmodes_varclust") {
            suppressMessages(suppressWarnings(
              factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
            ))
          }
        }, error = function(e) { 
          cat("ERREUR\n")
          warning(paste("Erreur pour k=", k, ":", e$message))
          NULL 
        })
        
        if (!is.null(model)) {
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            warning(paste("Impossible d'extraire l'inertie pour k=", k))
            list(intra = NA)
          })
          
          inerties[i] <- inertie$intra
          cat("OK (inertie intra =", round(inertie$intra, 4), ")\n")
        } else {
          inerties[i] <- NA
          cat("ECHEC\n")
        }
      }
      
      results <- data.frame(
        k = k_values,
        inertie_intra = inerties
      )
      
      cat("\n")
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(results, ggplot2::aes(x = k, y = inertie_intra)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Methode du Coude - Clustering de Variables",
            subtitle = paste("Methode:", method),
            x = "Nombre de Clusters (k)",
            y = "Inertie Intra-Classe"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(breaks = k_values)
        
        print(p)
        
        return(list(plot = p, data = results))
      } else {
        plot(results$k, results$inertie_intra, type = 'b',
             main = "Methode du Coude - Clustering de Variables",
             xlab = "Nombre de Clusters (k)", 
             ylab = "Inertie Intra-Classe",
             col = "steelblue", pch = 19, lwd = 2)
        
        return(list(plot = NULL, data = results))
      }
    },
    
    #' @description Evaluer une plage de valeurs de k
    #' @param X Data frame de donnees
    #' @param k_range Vecteur de valeurs de k a tester (defaut: 2:10)
    #' @param method Methode de clustering (defaut: "var_cah")
    #' @param ... Parametres additionnels
    #' @return Data frame avec les resultats pour chaque k
    evaluate_k_range = function(X, k_range = 2:10, method = "var_cah", ...) {
      
      if (!method %in% c("var_cah", "kmodes_varclust")) {
        stop(paste0("Methode non reconnue: ", method))
      }
      
      factory <- ClusteringFactory$new()
      
      resultats <- data.frame(
        k = integer(),
        inertie_totale = numeric(),
        inertie_intra = numeric(),
        inertie_inter = numeric(),
        inertie_expliquee = numeric(),
        stringsAsFactors = FALSE
      )
      
      cat("\n===============================================================\n")
      cat("   EVALUATION DE K - Plage complete\n")
      cat("===============================================================\n\n")
      cat("Methode :", method, "\n")
      cat("Plage de k :", min(k_range), "a", max(k_range), "\n\n")
      
      for (k in k_range) {
        cat("-> Evaluation pour k =", k, "... ")
        
        model <- tryCatch({
          if (method == "var_cah") {
            suppressMessages(factory$create_var_cah(X, k = k, fit_now = TRUE, ...))
          } else if (method == "kmodes_varclust") {
            suppressMessages(suppressWarnings(
              factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
            ))
          }
        }, error = function(e) { 
          cat("ERREUR\n")
          warning(paste("Erreur pour k=", k, ":", e$message))
          NULL 
        })
        
        if (!is.null(model)) {
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            warning(paste("Impossible d'extraire l'inertie pour k=", k))
            list(totale = NA, intra = NA, inter = NA, pct_expliquee = NA)
          })
          
          resultats <- rbind(resultats, data.frame(
            k = k,
            inertie_totale = inertie$totale,
            inertie_intra = inertie$intra,
            inertie_inter = inertie$inter,
            inertie_expliquee = inertie$pct_expliquee
          ))
          
          cat("OK (", round(inertie$pct_expliquee, 2), "% explique)\n", sep = "")
        } else {
          cat("ECHEC\n")
        }
      }
      
      cat("\n")
      
      if (nrow(resultats) > 0) {
        cat("Resultats:\n")
        print(resultats, row.names = FALSE)
        cat("\n")
      }
      
      return(resultats)
    },
    
    #' @description Visualiser l'evaluation
    #' @param resultats Data frame de resultats (de evaluate_k_range)
    #' @param criterion Critere a visualiser (defaut: "inertie_expliquee")
    plot_evaluation = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        stop(paste0("Critere '", criterion, "' non trouve dans les resultats"))
      }
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(resultats, ggplot2::aes_string(x = "k", y = criterion)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Evaluation du nombre de clusters de variables",
            x = "Nombre de Clusters (k)",
            y = criterion
          ) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(breaks = resultats$k)
        
        print(p)
      } else {
        plot(resultats$k, resultats[[criterion]], type = 'b',
             main = "Evaluation du nombre de clusters de variables",
             xlab = "Nombre de Clusters (k)", ylab = criterion,
             col = "steelblue", pch = 19, lwd = 2)
      }
      
      invisible(self)
    },
    
    #' @description Obtenir le meilleur k selon un critere
    #' @param resultats Data frame de resultats
    #' @param criterion Critere (defaut: "inertie_expliquee")
    #' @return Valeur de k recommandee
    get_best_k = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        warning("Critere non trouve, retourne NA")
        return(NA)
      }
      
      valeurs <- resultats[[criterion]]
      k_values <- resultats$k
      
      if (length(valeurs) < 3) {
        return(k_values[1])
      }
      
      diff1 <- diff(valeurs)
      diff2 <- diff(diff1)
      
      if (length(diff2) > 0) {
        idx <- which.max(abs(diff2))
        best_k <- k_values[idx + 1]
        
        cat("\nk optimal recommande (methode du coude):", best_k, "\n")
        cat("Valeur du critere:", valeurs[idx + 1], "\n\n")
        
        return(best_k)
      } else {
        return(k_values[2])
      }
    }
  )
)

# =============================================================================
# 4. ClusteringHelper (Fonctions utilitaires)
# =============================================================================

#' Fonctions utilitaires pour le package de clustering de variables
#' 
#' @title ClusteringHelper
#' @description Fonctions utilitaires pour le package de clustering de variables
#' @importFrom utils write.csv
#' @export
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    
    #' @description Initialiser le Helper
    initialize = function() {
      message("ClusteringHelper initialise")
    },
    
    #' @description Genere un rapport textuel synthetique du clustering
    #' @param objet_clustering Objet de clustering ajuste (VAR_CAH, VARCLUS, ou KmodesVarClust)
    #' @param file Chemin du fichier ou ecrire le rapport (NULL pour affichage console)
    #' @return Invisiblement, l'objet lui-meme. Affiche le rapport.
    generate_report = function(objet_clustering, file = NULL) {
      if (!is.null(file)) {
        sink(file = file)
      }
      
      cat("===============================================================\n")
      cat("   RAPPORT D'ANALYSE DE CLUSTERING\n")
      cat("       (Clustering de Variables)\n")
      cat("===============================================================\n\n")
      
      cat("--- Resume du Modele ---\n")
      tryCatch({
        objet_clustering$summary()
      }, error = function(e) {
        cat("La methode $summary() n'est pas disponible ou a echoue.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      cat("\n--- Qualite du clustering (Inertie) ---\n")
      tryCatch({
        inertie <- objet_clustering$inertie()
        cat("Inertie totale     :", round(inertie$totale, 4), "\n")
        cat("Inertie intra      :", round(inertie$intra, 4), "\n")
        cat("Inertie inter      :", round(inertie$inter, 4), "\n")
        cat("Variance expliquee :", round(inertie$pct_expliquee, 2), "%\n")
      }, error = function(e) {
        cat("Mesures d'inertie non disponibles pour cet algorithme.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      cat("\n--- Taille des Groupes ---\n")
      tryCatch({
        groupes_table <- table(objet_clustering$Groupes)
        print(groupes_table)
      }, error = function(e) {
        cat("Propriete $Groupes non disponible.\n")
      })
      
      if (inherits(objet_clustering, "VAR_CAH")) {
        cat("\n--- Homogeneite des Clusters (VAR_CAH) ---\n")
        tryCatch({
          for (k in 1:objet_clustering$K) {
            vars <- objet_clustering$get_cluster_variables(k)
            cat("Cluster", k, ":", length(vars), "variable(s)\n")
            cat("  Variables:", paste(vars, collapse = ", "), "\n")
          }
        }, error = function(e) {
          cat("Details non disponibles.\n")
        })
      }
      
      if (inherits(objet_clustering, "VARCLUS")) {
        cat("\n--- Informations VARCLUS ---\n")
        cat("Algorithme : Clustering descendant hierarchique de variables\n")
        cat("Detection automatique du nombre de clusters basee sur lambda2\n")
      }
      
      if (inherits(objet_clustering, "KmodesVarClust")) {
        cat("\n--- Informations KmodesVarClust ---\n")
        cat("Algorithme : K-Modes pour clustering de variables categorielles\n")
      }
      
      cat("\n===============================================================\n")
      
      if (!is.null(file)) {
        sink()
        message(paste("Rapport genere dans:", file))
      }
      
      invisible(objet_clustering)
    },
    
    #' @description Affiche les methodes disponibles dans le package
    show_methods = function() {
      cat("===============================================================\n")
      cat("   METHODES DE CLUSTERING DISPONIBLES\n")
      cat("       (Clustering de Variables)\n")
      cat("===============================================================\n\n")
      
      cat("1. VAR_CAH\n")
      cat("   - Classification Ascendante Hierarchique sur variables\n")
      cat("   - Type de donnees : Numeriques\n")
      cat("   - Utilisation : factory$create_var_cah(X, k = 3)\n\n")
      
      cat("2. VARCLUS\n")
      cat("   - Clustering descendant hierarchique de variables\n")
      cat("   - Type de donnees : Numeriques\n")
      cat("   - Detection automatique du nombre optimal de clusters\n")
      cat("   - Utilisation : factory$create_varclus(X)\n\n")
      
      cat("3. KmodesVarClust\n")
      cat("   - K-Modes pour variables categorielles\n")
      cat("   - Type de donnees : Categorielles ou mixtes\n")
      cat("   - Utilisation : factory$create_kmodes_varclust(X, k = 3)\n\n")
      
      cat("===============================================================\n")
      invisible(self)
    },
    
    #' @description Exporter les resultats du clustering
    #' @param objet_clustering Objet de clustering ajuste
    #' @param file Chemin du fichier CSV de sortie
    export_results = function(objet_clustering, file = "clustering_results.csv") {
      tryCatch({
        groupes <- objet_clustering$Groupes
        
        results <- data.frame(
          Variable = names(groupes),
          Cluster = groupes,
          stringsAsFactors = FALSE
        )
        
        results <- results[order(results$Cluster, results$Variable), ]
        
        write.csv(results, file = file, row.names = FALSE)
        message(paste("Resultats exportes dans:", file))
        
        invisible(results)
      }, error = function(e) {
        stop(paste("Erreur lors de l'export:", e$message))
      })
    }
  )
)