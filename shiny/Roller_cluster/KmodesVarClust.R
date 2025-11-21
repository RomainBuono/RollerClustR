# =======================================================
# KmodesVarClust : Clustering de Variables par K-Modes 
# Algorithme de clustering de variables catégorielles/mixtes
# =======================================================
library(R6)

# Utilité pour trouver le mode d'un vecteur (si plusieurs, prend le premier)
find_mode <- function(x) {
  # Fonction pour trouver la modalité la plus fréquente (le mode)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

KmodesVarClust <- R6Class("KmodesVarClust",
                          inherit = ClusterAnalysis,
                          
                          private = list(
                            # =================================================================
                            # CHAMPS SPÉCIFIQUES À KmodesVarClust
                            # =================================================================
                            FModes = NULL,             # Prototypes (modes) de variables : Df (k x N)
                            FVarNames = NULL,          # Noms des variables
                            FMaxIter = 100,
                            FConverged = FALSE,
                            FInertie = NULL,
                            FNBins = 5,                # Nombre d'intervalles pour la discrétisation
                            FHasNumericVars = FALSE,   # Indicateur de présence de variables numériques
                            
                            # ------------------------------------------------
                            # Méthode pour nettoyer les données
                            # ------------------------------------------------
                            cleanDataset = function(X) {
                              # Simple nettoyage : supprimer les lignes avec NA si nécessaire
                              # Pour l'instant, on retourne X tel quel
                              # (le traitement des NA est fait dans les calculs via na.rm)
                              return(X)
                            },
                            
                            # ------------------------------------------------
                            # Algorithme K-Modes pour Variables (Logique de Base)
                            # ------------------------------------------------
                            
                            #' @description Calcule le désaccord entre un vecteur (variable) et un mode
                            #' @param vec Un vecteur d'observations (une ligne de X^T : une variable)
                            #' @param mode Le prototype (mode de cluster, une ligne de FModes)
                            #' @return Distance de désaccord simple
                            calc_dissimilarity = function(vec, mode) {
                              # La distance de désaccord simple est le nombre de fois où les valeurs ne correspondent pas
                              return(sum(vec != mode, na.rm = TRUE))
                            },
                            
                            # ------------------------------------------------
                            # IMPLÉMENTATION : do_fit (appelée par la méthode publique fit())
                            # ------------------------------------------------
                            do_fit = function(X) {
                              private$FX <- X
                              private$FVarNames <- colnames(X)
                              
                              # Vérifier la présence de variables numériques
                              private$FHasNumericVars <- any(sapply(X, is.numeric))
                              
                              # Émettre un avertissement si des variables numériques sont présentes
                              if (private$FHasNumericVars) {
                                warning(paste0(
                                  "Des variables numériques ont été détectées et seront discrétisées en ",
                                  private$FNBins, 
                                  " intervalles.\n",
                                  "Attention : Le choix du nombre d'intervalles (n_bins) peut influencer les résultats du clustering.\n",
                                  "Il est recommandé de tester différentes valeurs de n_bins pour évaluer la stabilité des résultats."
                                ), call. = FALSE)
                              }
                              
                              # Appeler la logique de K-modes
                              private$do_refit_with_k(private$FNbGroupes)
                              private$FFitted <- TRUE
                              
                              invisible(self)
                            },
                            
                            # ------------------------------------------------
                            # IMPLÉMENTATION : do_refit_with_k (appelée par K <- value)
                            # ------------------------------------------------
                            do_refit_with_k = function(new_k) {
                              private$FNbGroupes <- new_k
                              
                              X_clean <- private$cleanDataset(private$FX)
                              
                              # Convertir toutes les colonnes en facteur (K-Modes) avec n_bins paramétrable
                              X_factor <- data.frame(lapply(X_clean, function(v) {
                                if (is.numeric(v)) {
                                  cut(v, breaks = private$FNBins, include.lowest = TRUE) 
                                } else {
                                  as.factor(v)
                                }
                              }))
                              
                              # X_vars : Variables deviennent les observations (lignes)
                              # X_vars : Observations deviennent les features (colonnes)
                              X_vars <- t(X_factor)
                              X_vars <- as.data.frame(X_vars) # Convertir en DF pour les opérations par colonne
                              
                              # 1. Initialisation
                              D <- nrow(X_vars) # Nombre de variables à clusteriser
                              # Sélectionner K variables comme modes initiaux
                              initial_modes_indices <- sample(1:D, new_k)
                              current_modes <- X_vars[initial_modes_indices, , drop = FALSE]
                              rownames(current_modes) <- paste0("Mode_", 1:new_k)
                              
                              current_groups <- rep(NA_integer_, D)
                              iter <- 0
                              
                              for (iter in 1:private$FMaxIter) {
                                
                                # 2. Étape d'affectation (Assignment Step)
                                old_groups <- current_groups
                                
                                for (i in 1:D) {
                                  variable_i <- X_vars[i, ]
                                  distances <- sapply(1:new_k, function(k_idx) {
                                    # Calculer la distance de la variable i au mode k
                                    private$calc_dissimilarity(variable_i, current_modes[k_idx, ])
                                  })
                                  current_groups[i] <- which.min(distances)
                                }
                                
                                # 3. Étape de mise à jour (Update Step) - NOUVEAU MODE
                                new_modes <- current_modes
                                for (k_idx in 1:new_k) {
                                  cluster_vars_indices <- which(current_groups == k_idx)
                                  
                                  if (length(cluster_vars_indices) > 0) {
                                    # Le nouveau mode est la variable qui est la plus représentative du cluster
                                    # Dans le K-Modes standard, c'est le mode majoritaire pour chaque colonne (Observation)
                                    X_cluster <- X_vars[cluster_vars_indices, , drop = FALSE]
                                    
                                    # Appliquer 'find_mode' à chaque colonne (Observation) du sous-ensemble
                                    new_mode_vec <- sapply(X_cluster, find_mode)
                                    new_modes[k_idx, ] <- new_mode_vec
                                  } else {
                                    # Si le cluster est vide, réinitialiser le mode (ex: prendre une variable au hasard)
                                    new_modes[k_idx, ] <- X_vars[sample(1:D, 1), ]
                                  }
                                }
                                current_modes <- new_modes
                                
                                # 4. Vérification de la convergence
                                if (!any(is.na(current_groups)) && !any(is.na(old_groups)) && 
                                    all(current_groups == old_groups)) {
                                  private$FConverged <- TRUE
                                  break
                                }
                              }
                              
                              # Stockage des résultats
                              private$FGroupes <- current_groups
                              names(private$FGroupes) <- private$FVarNames
                              private$FModes <- current_modes
                              
                              # Calcul d'une inertie de simulation (basée sur le désaccord total intra-cluster)
                              total_diss <- sum(sapply(1:D, function(i) {
                                k_idx <- private$FGroupes[i]
                                private$calc_dissimilarity(X_vars[i, ], private$FModes[k_idx, ])
                              }))
                              
                              private$FInertie <- list(
                                totale = NA,
                                intra = total_diss,
                                inter = NA,
                                pct_expliquee = NA,
                                converged = private$FConverged,
                                iterations = iter
                              )
                              
                              invisible(self)
                            },
                            
                            # ------------------------------------------------
                            # IMPLÉMENTATION : do_predict
                            # ------------------------------------------------
                            do_predict = function(newdata) {
                              if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
                              if (is.null(private$FModes)) stop("Les modes sont manquants pour la prédiction.")
                              
                              # 1. Préparation des nouvelles variables (transposition et factorisation)
                              # Les nouvelles colonnes de 'newdata' sont les variables à assigner.
                              X_new_factor <- data.frame(lapply(newdata, function(v) {
                                if (is.numeric(v)) {
                                  cut(v, breaks = private$FNBins, include.lowest = TRUE)
                                } else {
                                  as.factor(v)
                                }
                              }))
                              
                              X_new_vars <- t(X_new_factor)
                              X_new_vars <- as.data.frame(X_new_vars)
                              
                              # 2. Vérification de la structure (les colonnes doivent correspondre aux observations originales)
                              if (ncol(X_new_vars) != ncol(private$FModes)) {
                                stop(paste0("Le nombre d'observations dans newdata (", ncol(X_new_vars), 
                                            ") ne correspond pas au nombre d'observations du fit original (", 
                                            ncol(private$FModes), ")."))
                              }
                              
                              predictions <- rep(NA_integer_, nrow(X_new_vars))
                              
                              # 3. Calcul de la distance à chaque mode de variable
                              for (i in 1:nrow(X_new_vars)) {
                                variable_i <- X_new_vars[i, ]
                                
                                # Calculer la distance de la nouvelle variable 'i' à chaque mode existant
                                distances <- sapply(1:private$FNbGroupes, function(k_idx) {
                                  mode_k <- private$FModes[k_idx, ]
                                  private$calc_dissimilarity(variable_i, mode_k)
                                })
                                
                                # Le groupe le plus proche (indice de la distance minimale)
                                if (all(is.na(distances))) {
                                  predictions[i] <- NA_integer_
                                } else {
                                  predictions[i] <- which.min(distances)
                                }
                              }
                              
                              names(predictions) <- colnames(newdata)
                              return(predictions)
                            },
                            
                            # ------------------------------------------------
                            # IMPLÉMENTATION : do_summary
                            # ------------------------------------------------
                            do_summary = function() {
                              if (!private$FFitted) {
                                stop("Le modèle doit être ajusté avec $fit() d'abord.")
                              }
                              
                              cat("\n═══════════════════════════════════════════════════════════\n")
                              cat("   KmodesVarClust - Résumé du Clustering K-Modes\n")
                              cat("═══════════════════════════════════════════════════════════\n\n")
                              cat("Algorithme : K-Modes (Réallocation de Variables)\n")
                              cat("Nombre de clusters (k) :", private$FNbGroupes, "\n")
                              cat("Nombre d'intervalles (n_bins) :", private$FNBins, "\n")
                              cat("Iterations :", private$FInertie$iterations, "\n")
                              cat("Convergence :", ifelse(private$FConverged, "Oui", "Non"), "\n")
                              cat("Désaccord Intra-cluster Total :", round(private$FInertie$intra, 2), "\n\n")
                              
                              cat("Distribution des Groupes de Variables :\n")
                              print(table(self$Groupes))
                              cat("\n")
                            }
                          ),
                          
                          public = list(
                            #' @description Initialiser l'objet KmodesVarClust
                            #' @param k Nombre de clusters (défaut: 2)
                            #' @param max_iter Nombre maximum d'itérations (défaut: 100)
                            #' @param n_bins Nombre d'intervalles pour la discrétisation des variables numériques (défaut: 5)
                            initialize = function(k = 2, max_iter = 100, n_bins = 5, ...) {
                              if (!is.numeric(k) || k < 2) stop("k doit être un nombre entier >= 2.")
                              if (!is.numeric(max_iter) || max_iter < 1) stop("max_iter doit être >= 1.")
                              if (!is.numeric(n_bins) || n_bins < 2) stop("n_bins doit être un nombre entier >= 2.")
                              
                              private$FNbGroupes <- as.integer(k)
                              private$FMaxIter <- as.integer(max_iter)
                              private$FNBins <- as.integer(n_bins)
                              private$FDataType <- "mixed"
                            }
                          ),
                          
                          # =========================================================================
                          # Active bindings spécifiques (surcharge de Groupes pour variables)
                          # =========================================================================
                          active = list(
                            #' @field Groupes Groupes des variables (surcharge avec noms de variables)
                            Groupes = function() {
                              if (!private$FFitted) {
                                stop("Le modèle doit être ajusté avec $fit() d'abord")
                              }
                              
                              # Pour KmodesVarClust, les groupes concernent les VARIABLES (colonnes)
                              if (!is.null(private$FVarNames)) {
                                names(private$FGroupes) <- private$FVarNames
                              }
                              return(private$FGroupes)
                            }
                          )
)