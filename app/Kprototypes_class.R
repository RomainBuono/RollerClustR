# ==================================================
# K-prototypes : Clustering pour données mixtes (R6 Class)
# Vitesse optimisée par vectorisation matricielle (N x K)
# Résolution du blocage initial et des warnings 'drop'
# MODIFICATION : max_iter augmenté à 500 par défaut
# ==================================================

Kprototypes <- R6Class("Kprototypes",
inherit = ClusterAnalysis,

private = list(
 FModel = NULL,
 FNumericCols = NULL,
 FCategoricalCols = NULL,
 FLambda = 0.5,
 FMaxIter = 500, # <--- MODIFIÉ : 500 itérations par défaut
 
 # =================================================================
 # 1. calcDistanceMixte (Distance 1-à-1)
 # Utilisée uniquement pour l'initialisation. Contient la correction des warnings.
 # =================================================================
 calcDistanceMixte = function(x1, x2, lambda = 0.5) {
 n_num <- length(private$FNumericCols)
 n_cat <- length(private$FCategoricalCols)
 
 # Extraction Numérique : Utilisation d'unlist() pour forcer le vecteur et éviter les warnings 'drop' sur une colonne.
 x1_num <- as.numeric(unlist(x1[private$FNumericCols]))
 x2_num <- as.numeric(unlist(x2[private$FNumericCols]))
 
 # Extraction Catégorielle : unlist() pour transformer la sélection data.frame 1xN en vecteur.
 x1_cat <- unlist(x1[, private$FCategoricalCols, drop = FALSE])
 x2_cat <- unlist(x2[, private$FCategoricalCols, drop = FALSE])
 
 
 # CAS 1 : Que des variables numériques → K-means classique
 if (n_num > 0 && n_cat == 0) {
  num_diff <- x1_num - x2_num
  return(sqrt(sum(num_diff^2)))
 }
 
 # CAS 2 : Que des variables catégorielles → K-modes
 if (n_num == 0 && n_cat > 0) {
  cat_diff <- x1_cat != x2_cat
  return(sum(cat_diff))
 }
 
 # CAS 3 : Variables mixtes → K-prototypes véritable
 if (n_num > 0 && n_cat > 0) {
  # Distance euclidienne (normalisée)
  num_diff <- x1_num - x2_num
  dist_num <- sqrt(sum(num_diff^2)) / n_num
  
  # Distance de Hamming
  cat_diff <- x1_cat != x2_cat
  dist_cat <- sum(cat_diff) / n_cat
  
  # Pondération par lambda
  return((1 - lambda) * dist_num + lambda * dist_cat)
 }
 
 stop("Aucune variable détectée")
 },
 
 # =================================================================
 # 2. calcDistanceMatrixToPrototype (Distance N-à-1)
 # Utilisée pour la réassignation (E-step). Logique vectorielle rapide.
 # =================================================================
 calcDistanceMatrixToPrototype = function(data_work, prototype, lambda = 0.5) {
 n_num <- length(private$FNumericCols)
 n_cat <- length(private$FCategoricalCols)
 n_obs <- nrow(data_work)
 
 dist_num <- 0
 if (n_num > 0) {
  # Partie Numérique: Euclidean (Vectorisé)
  num_data <- data_work[, private$FNumericCols, drop = FALSE]
  num_proto <- as.numeric(unlist(prototype[private$FNumericCols]))
  
  # Calcul de la différence au carré, ligne par ligne (sweep)
  diff_sq <- sweep(as.matrix(num_data), 2, num_proto, FUN = "-")^2
  dist_num <- sqrt(rowSums(diff_sq)) / n_num
 }
 
 dist_cat <- 0
 if (n_cat > 0) {
  # Partie Catégorielle: Hamming (Vectorisé)
  cat_data <- data_work[, private$FCategoricalCols, drop = FALSE]
  cat_proto <- unlist(prototype[, private$FCategoricalCols, drop = FALSE])
  
  # Construction d'une matrice prototype n_obs x n_cat pour comparaison
  diff_matrix <- as.matrix(cat_data) != matrix(cat_proto, 
             nrow = n_obs, 
             ncol = n_cat, 
             byrow = TRUE)
  dist_cat <- rowSums(diff_matrix) / n_cat
 }
 
 # Distance combinée
 if (n_num > 0 && n_cat > 0) {
  return((1 - lambda) * dist_num + lambda * dist_cat)
 } else if (n_num > 0) {
  return(dist_num)
 } else if (n_cat > 0) {
  return(dist_cat)
 }
 },
 
 # 3. calcPrototype (Calcul du centre/mode de groupe, inchangé)
 calcPrototype = function(data_groupe) {
 prototype <- as.data.frame(t(rep(NA, ncol(data_groupe))), stringsAsFactors = FALSE)
 names(prototype) <- names(data_groupe)
 
 # Moyenne pour variables numériques
 if (length(private$FNumericCols) > 0) {
  for (j in private$FNumericCols) {
  prototype[1, j] <- mean(as.numeric(data_groupe[, j]), na.rm = TRUE)
  }
 }
 
 # Mode pour variables catégorielles
 if (length(private$FCategoricalCols) > 0) {
  for (j in private$FCategoricalCols) {
  tab <- table(data_groupe[, j])
  mode_value <- names(tab)[which.max(tab)]
  prototype[1, j] <- factor(mode_value, levels = levels(data_groupe[, j]))
  }
 }
 
 return(prototype)
 }
),

public = list(
 initialize = function(k = 3, cr = TRUE, lambda = 0.5, max_iter = 500) { # <--- MODIFIÉ : 500 itérations par défaut
 super$initialize(k = k, cr = cr)
 private$FLambda <- lambda
 private$FMaxIter <- max_iter
 },
 
 fit = function(X) {
 # ... (Validation et préparation) ...
 if (!is.data.frame(X)) { stop("X doit être un data frame") }
 char_cols <- which(sapply(X, is.character))
 if (length(char_cols) > 0) { stop("Les variables en charactère doivent être converties en facteur") }
 
 private$FNumericCols <- which(sapply(X, is.numeric) | sapply(X, is.integer))
 private$FCategoricalCols <- which(sapply(X, is.factor))
 
 n_num <- length(private$FNumericCols)
 n_cat <- length(private$FCategoricalCols)
 if (n_num == 0 && n_cat == 0) { stop("Aucune variable numérique ou catégorielle détectée") }
 
 private$FX <- X
 private$FDataType <- private$detectDataType(X)
 
 if (n_num > 0 && n_cat == 0) { message("Mode K-means : données uniquement numériques")
 } else if (n_num == 0 && n_cat > 0) { message("Mode K-modes : données uniquement catégorielles")
 } else { message("Mode K-prototypes : données mixtes (", n_num, " numériques, ", n_cat, " catégorielles)") }
 
 data_work <- X
 if (private$FScale && n_num > 0) {
  data_work[, private$FNumericCols] <- scale(data_work[, private$FNumericCols])
 }
 
 n <- nrow(data_work)
 
 
 # Initialisation : k observations aléatoires
 init_indices <- sample(1:n, private$FNbGroupes)
 
 # Assigner tous les individus au prototype initial le plus proche (utilisation du 1-à-1 corrigé)
 private$FGroupes <- sapply(1:n, function(i) {
  current_obs <- data_work[i, , drop = FALSE]
  
  distances <- sapply(init_indices, function(idx) {
  private$calcDistanceMixte(current_obs, 
         data_work[idx, , drop = FALSE], 
         private$FLambda)
  })
  return(which.min(distances))
 })
 
 
 # Itérations
 converged <- FALSE
 for (iter in 1:private$FMaxIter) {
  message(sprintf("→ Itération %d...", iter))
  groupes_old <- private$FGroupes
  
  # Étape 1 : Calculer les prototypes (M-step)
  prototypes <- lapply(1:private$FNbGroupes, function(g) {
  indices_g <- which(private$FGroupes == g)
  if (length(indices_g) > 0) {
   private$calcPrototype(data_work[indices_g, , drop = FALSE])
  } else {
   # Groupe vide : observation aléatoire
   return(data_work[sample(1:n, 1), , drop = FALSE])
  }
  })
  
  # Étape 2 : Réassigner les groupes (E-step RAPIDE)
  # Calcul matriciel N x K des distances
  
  distance_matrix <- sapply(prototypes, function(proto) {
  # Utilisation de la fonction vectorisée N-à-1
  private$calcDistanceMatrixToPrototype(data_work, proto, private$FLambda)
  })
  
  # Trouver l'indice (le groupe) de la distance minimale pour chaque ligne (observation)
  private$FGroupes <- apply(distance_matrix, 1, which.min)
  
  
  # Convergence ?
  if (all(private$FGroupes == groupes_old)) {
  message("✓ Convergence atteinte après ", iter, " itérations")
  converged <- TRUE
  break
  }
 }
 
 if (!converged) {
  # Si on atteint la fin du max_iter sans convergence, on ajoute un warning.
  warning("⚠ Nombre maximum d'itérations atteint sans convergence. Le résultat est la dernière partition trouvée.")
 }
 
 # Stocker les résultats
 private$FModel <- list(
  prototypes = prototypes,
  lambda = private$FLambda,
  iter = if(converged) iter else private$FMaxIter,
  converged = converged
 )
 
 private$FFitted <- TRUE
 
 message("✓ Modèle K-prototypes ajusté avec succès (", private$FNbGroupes, " groupes)")
 invisible(self)
 },
 
 # ... (Méthodes d'affichage et de visualisation non modifiées) ...
 
 print = function() {
 super$print()
 if (private$FFitted) {
  n_num <- length(private$FNumericCols)
  n_cat <- length(private$FCategoricalCols)
  cat("Variables numériques :", n_num, "\n")
  cat("Variables catégorielles :", n_cat, "\n")
  if (n_num > 0 && n_cat > 0) {
  cat("Lambda (poids catég.) :", private$FLambda, "\n")
  }
 }
 invisible(self)
 },
 
 summary = function() {
 super$summary()
 
 if (private$FFitted) {
  cat("\n--- Spécifique K-prototypes ---\n")
  cat("Variables numériques :", length(private$FNumericCols), "\n")
  if (length(private$FNumericCols) > 0) {
  cat(" →", paste(names(private$FX)[private$FNumericCols], collapse = ", "), "\n")
  }
  cat("Variables catégorielles :", length(private$FCategoricalCols), "\n")
  if (length(private$FCategoricalCols) > 0) {
  cat(" →", paste(names(private$FX)[private$FCategoricalCols], collapse = ", "), "\n")
  }
  
  if (length(private$FNumericCols) > 0 && length(private$FCategoricalCols) > 0) {
  cat("Lambda (poids variables catégorielles) :", private$FLambda, "\n")
  }
  
  cat("Nombre d'itérations :", private$FModel$iter, "\n")
  cat("Convergence :", ifelse(private$FModel$converged, "Oui", "Non"), "\n")
 }
 
 invisible(self)
 },
 
 plot = function() {
 if (!private$FFitted) { stop("Le modèle doit être ajusté avec $fit() d'abord") }
 
 n_num <- length(private$FNumericCols)
 n_cat <- length(private$FCategoricalCols)
 
 # CAS 1 : Que des variables numériques
 if (n_num >= 2 && n_cat == 0) {
  plot(private$FX[, private$FNumericCols[1:2]], 
   col = private$FGroupes, pch = 19,
   main = "K-prototypes (mode K-means)",
   xlab = names(private$FX)[private$FNumericCols[1]],
   ylab = names(private$FX)[private$FNumericCols[2]])
  legend("topright", legend = paste("Groupe", 1:private$FNbGroupes),
   col = 1:private$FNbGroupes, pch = 19)
 }
 
 # CAS 2 : Que des variables catégorielles
 else if (n_num == 0 && n_cat >= 2) {
  par(mfrow = c(1, 2))
  
  tab1 <- table(private$FGroupes, private$FX[, private$FCategoricalCols[1]])
  barplot(tab1, beside = TRUE, legend = TRUE,
    main = "Répartition par groupe",
    xlab = names(private$FX)[private$FCategoricalCols[1]],
    ylab = "Effectif",
    col = 1:private$FNbGroupes)
  
  tab2 <- table(private$FGroupes, private$FX[, private$FCategoricalCols[2]])
  barplot(tab2, beside = TRUE, legend = TRUE,
    main = "Répartition par groupe",
    xlab = names(private$FX)[private$FCategoricalCols[2]],
    ylab = "Effectif",
    col = 1:private$FNbGroupes)
  
  par(mfrow = c(1, 1))
 }
 
 # CAS 3 : Variables mixtes
 else if (n_num > 0 && n_cat > 0) {
  par(mfrow = c(1, 2))
  
  num_col <- private$FNumericCols[1]
  boxplot(private$FX[, num_col] ~ private$FGroupes,
    main = "Distribution numérique",
    xlab = "Groupe", ylab = names(private$FX)[num_col],
    col = 1:private$FNbGroupes)
  
  cat_col <- private$FCategoricalCols[1]
  tab <- table(private$FGroupes, private$FX[, cat_col])
  barplot(tab, beside = TRUE, legend = TRUE,
    main = "Répartition catégorielle",
    xlab = names(private$FX)[cat_col],
    ylab = "Effectif",
    col = 1:private$FNbGroupes)
  
  par(mfrow = c(1, 1))
 }
 
 else { cat("Visualisation non disponible pour ce type de données\n") }
 
 invisible(self)
 },
 
 prototypes = function() {
 if (!private$FFitted) { stop("Le modèle doit être ajusté avec $fit() d'abord") }
 
 cat("=== Prototypes des", private$FNbGroupes, "groupes ===\n\n")
 
 for (g in 1:private$FNbGroupes) {
  cat("Groupe", g, "(n =", sum(private$FGroupes == g), ") :\n")
  proto <- private$FModel$prototypes[[g]][1, ]
  
  # Variables numériques
  if (length(private$FNumericCols) > 0) {
  cat(" Variables numériques :\n")
  for (j in private$FNumericCols) {
   cat(" ", names(private$FX)[j], ":", round(as.numeric(proto[j]), 3), "\n")
  }
  }
  
  # Variables catégorielles
  if (length(private$FCategoricalCols) > 0) {
  cat(" Variables catégorielles :\n")
  for (j in private$FCategoricalCols) {
   cat(" ", names(private$FX)[j], ":", proto[j], "\n")
  }
  }
  cat("\n")
 }
 
 invisible(self)
 }
),

active = list(
 NbGroupes = function(value) {
 if (missing(value)) {
  return(private$FNbGroupes)
 } else {
  message("Pour changer le nombre de groupes avec K-prototypes, recréez un nouvel objet avec k différent et réajustez avec $fit()")
  return(private$FNbGroupes)
 }
 }
)
)