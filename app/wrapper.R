#=========================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering R6
#=========================================

# ---------------------------------
# 1. ClusteringFactory (Création des objets)
# ---------------------------------

#' @title ClusteringFactory
#' @description Factory pour créer et initialiser les objets de clustering
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser
    initialize = function() {
      message("ClusteringFactory initialisée")
    },
    
    #' @description Créer et ajuster un objet CAH_Kmeans (CORRIGÉ)
    #' @return Objet CAH_Kmeans
    create_cah_kmeans = function(X, k = 2, cr = TRUE, fit_now = TRUE, ...) {
      # Utilise le nom de classe corrigé
      obj <- CAH_Kmeans$new(k = k, cr = cr, ...)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kmeans (CORRIGÉ)
    #' @return Objet Kmeans
    create_kmeans = function(X, k = 3, cr = TRUE, nstart = 10, fit_now = TRUE, ...) {
      # Utilise le nom de classe 'Kmeans'
      obj <- Kmeans$new(k = k, cr = cr, nstart = nstart)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet ClustOfVar
    #' @return Objet ClustOfVar
    create_clustofvar = function(X, k = 3, cr = TRUE, fit_now = TRUE, ...) {
      obj <- ClustOfVar$new(k = k, cr = cr, ...)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kprototypes
    #' @return Objet Kprototypes
    create_kprototypes = function(X, k = 3, cr = FALSE, fit_now = TRUE, ...) {
      obj <- Kprototypes$new(k = k, cr = cr, ...)
      if (fit_now) {
        obj$fit(X)
      }
      return(obj)
    }
  )
)

# ---------------------------------
# 2. ClusteringComparator (Comparaison)
# ---------------------------------

#' @title ClusteringComparator
#' @description Comparer les résultats de plusieurs algorithmes
ClusteringComparator <- R6Class("ClusteringComparator",
  private = list(
    FData = NULL,
    FK = 3,
    FResults = list(),
    FGroupes = list()
  ),
  
  public = list(
    #' @description Initialiser
    initialize = function(data, k = 3) {
      private$FData <- data
      private$FK <- k
      message("ClusteringComparator initialisé pour k=", k)
    },
    
    #' @description Ajouter un algorithme à la comparaison (CORRIGÉ)
    add_algorithm = function(algorithm, ...) {
      factory <- ClusteringFactory$new()
      
      # 🎯 CORRECTION : Utilisation de "cah_kmeans" comme étiquette et nom de fonction
      obj <- switch(algorithm,
        cah_kmeans = factory$create_cah_kmeans(private$FData, k = private$FK, fit_now = TRUE, ...),
        kmeans = factory$create_kmeans(private$FData, k = private$FK, fit_now = TRUE, ...),
        clustofvar = factory$create_clustofvar(private$FData, k = private$FK, fit_now = TRUE, ...),
        kprototypes = factory$create_kprototypes(private$FData, k = private$FK, fit_now = TRUE, ...),
        stop("Algorithme inconnu")
      )
      
      private$FResults[[algorithm]] <- obj
      # Stocke les groupes. Note: Pour clustofvar, ce seront les groupes de variables (4).
      # Le bug de longueur sera géré par l'exclusion de clustofvar de la comparaison d'observations.
      private$FGroupes[[algorithm]] <- obj$Groupes
      
      message("Algorithme '", algorithm, "' ajouté à la comparaison")
      invisible(self)
    },
    
    #' @description Afficher le tableau de comparaison des groupes
    compare = function() {
      algos <- names(private$FResults)
      n_algos <- length(algos)
      
      if (n_algos < 2) {
        stop("Ajoutez au moins deux algorithmes avec $add_algorithm()")
      }
      
      cat("=== Comparaison de", n_algos, "algorithmes ===\n\n")
      
      # 1. Afficher la taille des groupes
      for (algo in algos) {
        cat("Algorithme:", algo, "\n")
        print(table(private$FGroupes[[algo]]))
        cat("\n")
      }
      
      # 2. Afficher les tables de confusion (indice de similarité)
      cat("\n--- Matrices de Confusion (Similarité des observations) ---\n")
      
      # 🎯 NOTE: Cette partie ne fonctionne que pour les clustering d'OBSERVATIONS
      # => Exclure ClustOfVar lors de l'appel pour éviter 'all arguments must have the same length'
      
      for (i in 1:(n_algos - 1)) {
        for (j in (i + 1):n_algos) {
          # Vérification de la longueur (pour gérer le cas ClustOfVar non exclu)
          if (length(private$FGroupes[[algos[i]]]) != length(private$FGroupes[[algos[j]]])) {
            warning(paste("Comparaison", algos[i], "vs", algos[j], ": longueurs inégales. (Skipped)"))
            next
          }
          
          cat("\n", toupper(algos[i]), "vs", toupper(algos[j]), ":\n")
          confusion <- table(private$FGroupes[[algos[i]]], private$FGroupes[[algos[j]]])
          print(confusion)
          
          # Calcul de l'indice de Rand ajusté
          # N'est pas implémenté ici pour éviter de dépendre d'un package externe (ex: mclust)
          
        }
      }
      invisible(self)
    },
    
    #' @description Récupérer un objet de clustering spécifique
    get_result = function(algorithm) {
      if (!algorithm %in% names(private$FResults)) {
        stop(paste("Algorithme", algorithm, "non trouvé"))
      }
      return(private$FResults[[algorithm]])
    }
  )
)

# ---------------------------------
# 3. ClusteringEvaluator (Évaluation de K)
# ---------------------------------

#' @title ClusteringEvaluator
#' @description Évaluer la qualité du clustering et les métriques
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  private = list(
    FData = NULL,
    FResults = list()
  ),
  
  public = list(
    #' @description Initialiser
    initialize = function(data) {
      private$FData <- data
      message("ClusteringEvaluator initialisé")
    },
    
    #' @description Évaluer la qualité du clustering pour différentes valeurs de k (CORRIGÉ)
    evaluate_k = function(k_range = 2:10, method = "kmeans", ...) {
      results <- data.frame(k = k_range)
      factory <- ClusteringFactory$new()
      
      # 🎯 CORRECTION : Inclure cah_kmeans pour le calcul d'inertie
      if (method %in% c("kmeans", "cah_kmeans")) { 
        inertie_expl <- numeric(length(k_range))
        inertie_intra <- numeric(length(k_range))
        inertie_inter <- numeric(length(k_range))
        
        message("Évaluation de ", method, " pour k = ", min(k_range), " à ", max(k_range))
        
        for (i in seq_along(k_range)) {
          
          # 🎯 Utiliser la bonne fonction de factory
          if (method == "kmeans") {
            obj <- factory$create_kmeans(private$FData, k = k_range[i], fit_now = TRUE, ...)
          } else { # method == "cah_kmeans"
            obj <- factory$create_cah_kmeans(private$FData, k = k_range[i], fit_now = TRUE, ...)
          }
          
          # Vérifier que l'objet a la méthode inertie()
          if (!"inertie" %in% names(obj)) {
            stop("L'objet créé par la factory ne possède pas de méthode $inertie()")
          }
          
          inertie_info <- obj$inertie()
          inertie_expl[i] <- inertie_info$pct_expliquee
          inertie_intra[i] <- inertie_info$intra
          inertie_inter[i] <- inertie_info$inter
          
          private$FResults[[paste0(method, "_k", k_range[i])]] <- obj
        }
        
        results$inertie_expliquee <- inertie_expl
        results$inertie_intra <- inertie_intra
        results$inertie_inter <- inertie_inter
        
      } else if (method == "clustofvar") {
        # Logique pour clustofvar (utilise l'homogénéité, pas l'inertie classique)
        # La colonne 'inertie_expliquee' sera manquante, le tracé sera sauté.
        message("ClustOfVar: utilise l'homogénéité. Evaluation par inertie non supportée pour le tracé automatique.")
        
      } else { 
        message("La méthode '", method, "' n'est pas optimisée pour l'évaluation de k par inertie.")
      }
      
      return(results)
    },
    
    #' @description Déterminer le k optimal (méthode du coude) (AJOUTÉ)
    get_best_k = function(df_results) {
      if (!"inertie_expliquee" %in% names(df_results)) {
        return(NA)
      }
      # Calculer le gain et la courbure (seconde dérivée approximée)
      df_results$gain <- c(NA, diff(df_results$inertie_expliquee))
      df_results$courbure <- c(NA, diff(df_results$gain))
      
      # Le coude est souvent le k où la courbure est la plus négative (plus grande décélération)
      if (sum(!is.na(df_results$courbure)) > 0) {
        best_k_index <- which.min(df_results$courbure)
        best_k <- df_results$k[best_k_index]
        
        # S'assurer que le k retourné n'est pas le min(k_range)
        if (best_k == min(df_results$k) && length(df_results$k) > 1) {
          # Si le coude est k_min, on suggère k_min + 1
          return(min(df_results$k) + 1)
        }
        return(best_k)
      }
      
      return(NA)
    },
    
    #' @description Tracer l'évaluation de k (AJOUTÉ)
    plot_evaluation = function(df_results, criterion = "inertie_expliquee") {
      if (!criterion %in% names(df_results)) {
        stop("Le critère '", criterion, "' est absent des résultats. La méthode doit être kmeans ou cah_kmeans.")
      }
      
      plot(df_results$k, df_results[[criterion]], type = "b", pch = 19,
           main = paste("Méthode du Coude (Elbow Method) -", toupper(gsub("_", " ", criterion))),
           xlab = "Nombre de groupes (k)",
           ylab = paste(criterion, "(%)"))
      
      best_k <- self$get_best_k(df_results)
      if (!is.na(best_k)) {
        abline(v = best_k, col = "red", lty = 2)
        text(best_k, max(df_results[[criterion]]) * 0.9, 
             labels = paste("k suggéré =", best_k), pos = 4, col = "red")
      }
      
      invisible(self)
    }
  )
)

# ---------------------------------
# 4. ClusteringHelper (Export et Rapport)
# ---------------------------------

#' @title ClusteringHelper
#' @description Fonctions utilitaires d'aide
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    
    #' @description Exporte les résultats
    export_results = function(objet_clustering, donnees = NULL, inclure_donnees = TRUE) {
      groupes <- objet_clustering$Groupes
      
      if (is.null(donnees) && inclure_donnees) {
        # Tente de récupérer les données actives si non fournies
        tryCatch({
          donnees <- objet_clustering$ActiveData
        }, error = function(e) {
          inclure_donnees <<- FALSE
          warning("Données actives non accessibles, l'export n'inclura que les groupes.")
        })
      }
      
      if (inclure_donnees && !is.null(donnees)) {
        resultats <- data.frame(donnees, Groupe = groupes)
      } else {
        resultats <- data.frame(Groupe = groupes)
      }
      
      return(resultats)
    },
    
    #' @description Calcule des statistiques par groupe
    group_statistics = function(objet_clustering, donnees) {
      if (!is.data.frame(donnees)) stop("'donnees' doit être un data frame")
      groupes <- objet_clustering$Groupes
      stats_list <- list()
      
      for (var_name in names(donnees)) {
        variable <- donnees[[var_name]]
        
        if (is.numeric(variable)) {
          stats <- data.frame(
            groupe = sort(unique(groupes)),
            n = as.numeric(table(groupes)),
            moyenne = tapply(variable, groupes, mean),
            ecart_type = tapply(variable, groupes, sd),
            min = tapply(variable, groupes, min),
            max = tapply(variable, groupes, max)
          )
          rownames(stats) <- NULL
          stats_list[[var_name]] <- stats
        } else if (is.factor(variable) || is.character(variable)) {
          tab <- table(Groupe = groupes, Variable = variable)
          perc <- round(prop.table(tab, 1) * 100, 1)
          stats_list[[var_name]] <- list(
            contingence = tab,
            pourcentages_ligne = perc
          )
        }
      }
      return(stats_list)
    },
    
    #' @description Génère un rapport textuel complet
    generate_report = function(objet_clustering, file = NULL) {
      if (!is.null(file)) {
        sink(file = file)
      }
      
      cat("=========================================\n")
      cat("     RAPPORT D'ANALYSE DE CLUSTERING\n")
      cat("=========================================\n\n")
      
      # Résumé de base
      objet_clustering$summary()
      
      # Qualité
      tryCatch({
        inertie <- objet_clustering$inertie()
        cat("\n--- Qualité du clustering ---\n")
        cat("Inertie expliquée :", round(inertie$pct_expliquee, 2), "%\n")
      }, error = function(e) {})
      
      # Taille des groupes
      cat("\n--- Taille des Groupes ---\n")
      print(table(objet_clustering$Groupes))
      
      cat("\n=========================================\n")
      
      if (!is.null(file)) {
        sink()
        message("Rapport généré et sauvegardé dans : ", file)
      }
      
      invisible(objet_clustering)
    }
  )
)

