# ==================================================
# K-prototypes : Clustering pour données mixtes (R6 Class)
# Vitesse optimisée par vectorisation matricielle (N x K)
# VERSION MODIFIÉE : Résiliente aux données manquantes
# ==================================================

Kprototypes <- R6Class("Kprototypes",
inherit = ClusterAnalysis,

private = list(
FModel = NULL,
FNumericCols = NULL,
FCategoricalCols = NULL,
FLambda = 0.5,
FMaxIter = 500,

# =================================================================
# 1. calcDistanceMixte (Distance 1-à-1) avec gestion des NA
# =================================================================
calcDistanceMixte = function(x1, x2, lambda = 0.5) {
n_num <- length(private$FNumericCols)
n_cat <- length(private$FCategoricalCols)

# Extraction Numérique avec gestion des NA
x1_num <- as.numeric(unlist(x1[private$FNumericCols]))
x2_num <- as.numeric(unlist(x2[private$FNumericCols]))

# Extraction Catégorielle avec gestion des NA
x1_cat <- unlist(x1[, private$FCategoricalCols, drop = FALSE])
x2_cat <- unlist(x2[, private$FCategoricalCols, drop = FALSE])

# CAS 1 : Que des variables numériques
if (n_num > 0 && n_cat == 0) {
 # Ignorer les paires avec NA
 valid_idx <- !is.na(x1_num) & !is.na(x2_num)
 if (sum(valid_idx) == 0) return(Inf)  # Pas de données valides
 
 num_diff <- x1_num[valid_idx] - x2_num[valid_idx]
 return(sqrt(sum(num_diff^2)) / sqrt(sum(valid_idx)))
}

# CAS 2 : Que des variables catégorielles
if (n_num == 0 && n_cat > 0) {
 # Ignorer les paires avec NA
 valid_idx <- !is.na(x1_cat) & !is.na(x2_cat)
 if (sum(valid_idx) == 0) return(Inf)
 
 cat_diff <- x1_cat[valid_idx] != x2_cat[valid_idx]
 return(sum(cat_diff) / sum(valid_idx))
}

# CAS 3 : Variables mixtes
if (n_num > 0 && n_cat > 0) {
 # Distance euclidienne (normalisée) - ignorer NA
 valid_num <- !is.na(x1_num) & !is.na(x2_num)
 if (sum(valid_num) > 0) {
  num_diff <- x1_num[valid_num] - x2_num[valid_num]
  dist_num <- sqrt(sum(num_diff^2)) / sum(valid_num)
 } else {
  dist_num <- 0
 }
 
 # Distance de Hamming - ignorer NA
 valid_cat <- !is.na(x1_cat) & !is.na(x2_cat)
 if (sum(valid_cat) > 0) {
  cat_diff <- x1_cat[valid_cat] != x2_cat[valid_cat]
  dist_cat <- sum(cat_diff) / sum(valid_cat)
 } else {
  dist_cat <- 0
 }
 
 # Pondération par lambda
 return((1 - lambda) * dist_num + lambda * dist_cat)
}

stop("Aucune variable détectée")
},

# =================================================================
# 2. calcDistanceMatrixToPrototype (Distance N-à-1) avec gestion des NA
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
 
 # Gestion des NA : remplacer par distance infinie ou ignorer
 # Calcul de la différence au carré, ligne par ligne
 diff_sq <- sweep(as.matrix(num_data), 2, num_proto, FUN = "-")^2
 
 # Compter le nombre de variables valides par observation
 valid_counts <- rowSums(!is.na(num_data))
 valid_counts[valid_counts == 0] <- 1  # Éviter division par 0
 
 # Remplacer NA par 0 pour le calcul
 diff_sq[is.na(diff_sq)] <- 0
 
 dist_num <- sqrt(rowSums(diff_sq)) / sqrt(valid_counts)
}

dist_cat <- 0
if (n_cat > 0) {
 # Partie Catégorielle: Hamming (Vectorisé)
 cat_data <- data_work[, private$FCategoricalCols, drop = FALSE]
 cat_proto <- unlist(prototype[, private$FCategoricalCols, drop = FALSE])
 
 # Construction d'une matrice prototype pour comparaison
 diff_matrix <- as.matrix(cat_data) != matrix(cat_proto, 
      nrow = n_obs, 
      ncol = n_cat, 
      byrow = TRUE)
 
 # Compter les variables valides
 valid_matrix <- !is.na(as.matrix(cat_data))
 valid_counts <- rowSums(valid_matrix)
 valid_counts[valid_counts == 0] <- 1  # Éviter division par 0
 
 # Ignorer les NA dans le calcul
 diff_matrix[is.na(diff_matrix)] <- FALSE
 
 dist_cat <- rowSums(diff_matrix) / valid_counts
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

# 3. calcPrototype (Calcul du centre/mode de groupe) avec gestion NA
calcPrototype = function(data_groupe) {
prototype <- as.data.frame(t(rep(NA, ncol(data_groupe))), stringsAsFactors = FALSE)
names(prototype) <- names(data_groupe)

# Moyenne pour variables numériques (en ignorant NA)
if (length(private$FNumericCols) > 0) {
 for (j in private$FNumericCols) {
  val <- mean(as.numeric(data_groupe[, j]), na.rm = TRUE)
  # Si toutes les valeurs sont NA, utiliser 0
  if (is.na(val)) val <- 0
  prototype[1, j] <- val
 }
}

# Mode pour variables catégorielles (en ignorant NA)
if (length(private$FCategoricalCols) > 0) {
 for (j in private$FCategoricalCols) {
  # Exclure les NA pour trouver le mode
  valid_vals <- data_groupe[!is.na(data_groupe[, j]), j]
  
  if (length(valid_vals) > 0) {
   tab <- table(valid_vals)
   mode_value <- names(tab)[which.max(tab)]
   prototype[1, j] <- factor(mode_value, levels = levels(data_groupe[, j]))
  } else {
   # Si toutes les valeurs sont NA, prendre le premier niveau
   prototype[1, j] <- factor(levels(data_groupe[, j])[1], 
                             levels = levels(data_groupe[, j]))
  }
 }
}

return(prototype)
}
),

public = list(
initialize = function(k = 3, cr = TRUE, lambda = 0.5, max_iter = 500, na_action = "warn") {
super$initialize(k = k, cr = cr, na_action = na_action)

# Validation de lambda
if (!is.numeric(lambda) || length(lambda) != 1) {
 stop("Le paramètre 'lambda' doit être un nombre unique. Valeur reçue : ", paste(lambda, collapse = ", "))
}

if (is.na(lambda) || !is.finite(lambda)) {
 stop("Le paramètre 'lambda' ne peut pas être NA ou infini")
}

if (lambda < 0 || lambda > 1) {
 stop("Le paramètre 'lambda' doit être entre 0 et 1. Valeur reçue : ", lambda,
      "\nlambda = 0 : K-means pur (distance numérique uniquement)",
      "\nlambda = 1 : K-modes pur (distance catégorielle uniquement)",
      "\nlambda = 0.5 : Équilibre (recommandé)")
}

# Validation de max_iter
if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter < 1) {
 stop("Le paramètre 'max_iter' doit être un entier positif. Valeur reçue : ", max_iter)
}
max_iter <- as.integer(max_iter)

if (max_iter > 5000) {
 warning("max_iter très élevé (", max_iter, "). Cela peut ralentir significativement le calcul.")
}

private$FLambda <- lambda
private$FMaxIter <- max_iter
},

fit = function(X) {
# === VALIDATION COMPLÈTE DU DATASET ===
tryCatch({
 X <- private$validateDataset(X)
}, error = function(e) {
 stop("Erreur de validation du dataset : ", e$message)
})

# Vérification spécifique : pas de character
char_cols <- which(sapply(X, is.character))
if (length(char_cols) > 0) {
 char_names <- names(X)[char_cols]
 stop("Les variables de type 'character' doivent être converties en 'factor'.\n",
      "Variables concernées : ", paste(char_names, collapse = ", "),
      "\n\nUtilisez : X$", char_names[1], " <- as.factor(X$", char_names[1], ")")
}

# Conversion automatique des logical en factor
logical_cols <- which(sapply(X, is.logical))
if (length(logical_cols) > 0) {
 warning("Conversion automatique des colonnes 'logical' en 'factor' : ",
         paste(names(X)[logical_cols], collapse = ", "))
 for (col_idx in logical_cols) {
  X[[col_idx]] <- as.factor(X[[col_idx]])
 }
}

# Identifier les colonnes par type
private$FNumericCols <- which(sapply(X, is.numeric) | sapply(X, is.integer))
private$FCategoricalCols <- which(sapply(X, is.factor))

n_num <- length(private$FNumericCols)
n_cat <- length(private$FCategoricalCols)

# Vérifier qu'on a au moins des variables
if (n_num == 0 && n_cat == 0) { 
 stop("Aucune variable numérique ou catégorielle détectée.\n",
      "Types de colonnes détectés :\n",
      paste(sapply(X, function(col) paste0("  ", class(col)[1])), collapse = "\n"))
}

# Vérifier les valeurs infinies dans les colonnes numériques
if (n_num > 0) {
 inf_cols <- names(X)[private$FNumericCols][sapply(X[, private$FNumericCols, drop = FALSE], 
                                                     function(col) any(is.infinite(col), na.rm = TRUE))]
 if (length(inf_cols) > 0) {
  stop("Les colonnes numériques suivantes contiennent des valeurs infinies : ",
       paste(inf_cols, collapse = ", "),
       "\nRemplacez ou supprimez ces valeurs.")
 }
}

# Vérifier les facteurs avec des niveaux vides
if (n_cat > 0) {
 for (col_idx in private$FCategoricalCols) {
  col <- X[[col_idx]]
  valid_levels <- levels(col)[levels(col) %in% unique(col[!is.na(col)])]
  
  if (length(valid_levels) == 0) {
   stop("La colonne '", names(X)[col_idx], "' est un facteur sans niveau valide (toutes valeurs NA)")
  }
  
  if (length(valid_levels) == 1) {
   warning("La colonne '", names(X)[col_idx], "' n'a qu'un seul niveau. ",
           "Elle n'apportera pas d'information pour le clustering.")
  }
  
  # Nettoyer les niveaux inutilisés
  if (length(levels(col)) > length(valid_levels)) {
   X[[col_idx]] <- droplevels(col)
  }
 }
}

# === GESTION DES DONNÉES MANQUANTES ===
# Pour K-prototypes, on peut gérer les NA différemment :
# - Soit on les supprime (na_action = "omit")
# - Soit on les garde et on les traite dans les calculs de distance
missing_info <- private$checkMissingData(X)

if (missing_info$has_missing) {
 if (private$FNAAction == "fail") {
  stop("Données manquantes détectées. Veuillez traiter les NA avant le clustering.")
 } else if (private$FNAAction == "omit") {
  X <- X[-missing_info$na_rows, , drop = FALSE]
  message("→ ", length(missing_info$na_rows), " observations avec NA supprimées")
  private$FHasMissing <- TRUE
  private$FNAIndices <- missing_info$na_rows
 } else {
  # na_action == "warn" : on garde les NA et on les gère dans les calculs
  message("→ Les NA seront ignorés dans les calculs de distance")
  private$FHasMissing <- TRUE
 }
}

# Vérifier qu'il reste assez de données
if (nrow(X) < private$FNbGroupes) {
 stop("Pas assez d'observations pour le nombre de groupes demandé.\n",
      "Observations : ", nrow(X), " - Groupes : ", private$FNbGroupes,
      "\n\nOptions :\n",
      "  1. Réduire k : model <- Kprototypes$new(k = ", min(2, nrow(X) - 1), ")\n",
      "  2. Ajouter plus de données")
}

if (nrow(X) < 10) {
 warning("Très peu d'observations (", nrow(X), "). Les résultats peuvent être instables.")
}

# Vérifier le ratio k/n
if (private$FNbGroupes > nrow(X) / 2) {
 warning("Le nombre de groupes (k=", private$FNbGroupes, ") est très élevé par rapport au nombre d'observations (n=",
         nrow(X), "). Ratio k/n = ", round(private$FNbGroupes / nrow(X), 2))
}

private$FX <- X
private$FDataType <- private$detectDataType(X)

if (n_num > 0 && n_cat == 0) { 
 message("Mode K-means : données uniquement numériques")
} else if (n_num == 0 && n_cat > 0) { 
 message("Mode K-modes : données uniquement catégorielles")
} else { 
 message("Mode K-prototypes : données mixtes (", n_num, " numériques, ", n_cat, " catégorielles)") 
}

data_work <- X
if (private$FScale && n_num > 0) {
 # Standardiser en ignorant les NA
 for (col in private$FNumericCols) {
  col_mean <- mean(data_work[, col], na.rm = TRUE)
  col_sd <- sd(data_work[, col], na.rm = TRUE)
  if (col_sd > 1e-10) {
   data_work[, col] <- (data_work[, col] - col_mean) / col_sd
  }
 }
}

n <- nrow(data_work)

# Initialisation : k observations aléatoires (sans NA si possible)
# Trouver des observations avec le moins de NA possible
na_counts_per_row <- rowSums(is.na(data_work))
best_rows <- order(na_counts_per_row)[1:min(private$FNbGroupes * 2, n)]
init_indices <- sample(best_rows, min(private$FNbGroupes, length(best_rows)))

# Si pas assez d'indices, compléter avec des indices aléatoires
if (length(init_indices) < private$FNbGroupes) {
 remaining <- setdiff(1:n, init_indices)
 init_indices <- c(init_indices, sample(remaining, private$FNbGroupes - length(init_indices)))
}

# Assigner tous les individus au prototype initial le plus proche
private$FGroupes <- sapply(1:n, function(i) {
 current_obs <- data_work[i, , drop = FALSE]
 
 distances <- sapply(init_indices, function(idx) {
  private$calcDistanceMixte(current_obs, 
     data_work[idx, , drop = FALSE], 
     private$FLambda)
 })
 
 # Si toutes les distances sont infinies, assigner aléatoirement
 if (all(is.infinite(distances))) {
  return(sample(1:private$FNbGroupes, 1))
 }
 
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
 distance_matrix <- sapply(prototypes, function(proto) {
  private$calcDistanceMatrixToPrototype(data_work, proto, private$FLambda)
 })
 
 # Gérer les cas où toutes les distances sont infinies
 private$FGroupes <- apply(distance_matrix, 1, function(row) {
  if (all(is.infinite(row))) {
   return(sample(1:private$FNbGroupes, 1))
  }
  return(which.min(row))
 })
 
 # Convergence ?
 if (all(private$FGroupes == groupes_old)) {
  message("✓ Convergence atteinte après ", iter, " itérations")
  converged <- TRUE
  break
 }
}

if (!converged) {
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
   val <- as.numeric(proto[j])
   if (is.na(val)) {
    cat(" ", names(private$FX)[j], ": NA\n")
   } else {
    cat(" ", names(private$FX)[j], ":", round(val, 3), "\n")
   }
  }
 }
 
 # Variables catégorielles
 if (length(private$FCategoricalCols) > 0) {
  cat(" Variables catégorielles :\n")
  for (j in private$FCategoricalCols) {
   cat(" ", names(private$FX)[j], ":", as.character(proto[j]), "\n") 
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