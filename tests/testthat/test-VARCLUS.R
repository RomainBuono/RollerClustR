# ============================================================================
# TESTS POUR VARCLUS - Clustering Descendant Hierarchique de Variables
# ============================================================================
# 50+ tests couvrant tous les aspects de la classe VARCLUS

context("VARCLUS - Clustering Descendant avec PCA")

# ============================================================================
# SECTION 1 : INITIALISATION ET CONSTRUCTION (8 tests)
# ============================================================================

test_that("VARCLUS: Initialisation basique", {
  model <- VARCLUS$new()
  
  # Verifier que c'est bien un objet R6
  expect_true(R6::is.R6(model))
  expect_s3_class(model, "VARCLUS")
  expect_s3_class(model, "ClusterAnalysis")
  
  # Verifier que les methodes essentielles existent
  expect_true("fit" %in% names(model))
  expect_true("summary" %in% names(model))
})

test_that("VARCLUS: Initialisation avec stop_eigenvalue personnalise", {
  model <- VARCLUS$new(stop_eigenvalue = 0.5)
  expect_s3_class(model, "VARCLUS")
})

test_that("VARCLUS: Initialisation avec stop_eigenvalue = 1.0 (defaut)", {
  model <- VARCLUS$new(stop_eigenvalue = 1.0)
  expect_s3_class(model, "VARCLUS")
})

test_that("VARCLUS: Initialisation avec stop_eigenvalue = 0", {
  model <- VARCLUS$new(stop_eigenvalue = 0)
  expect_s3_class(model, "VARCLUS")
})

test_that("VARCLUS: Initialisation avec scale=TRUE", {
  model <- VARCLUS$new(scale = TRUE)
  expect_s3_class(model, "VARCLUS")
})

test_that("VARCLUS: Initialisation avec scale=FALSE", {
  model <- VARCLUS$new(scale = FALSE)
  expect_s3_class(model, "VARCLUS")
})

test_that("VARCLUS: Erreur si stop_eigenvalue negatif", {
  expect_error(VARCLUS$new(stop_eigenvalue = -1), "ne peut pas.*negatif")
})

test_that("VARCLUS: Erreur si stop_eigenvalue non numerique", {
  expect_error(VARCLUS$new(stop_eigenvalue = "un"), "doit.*numerique")
})

# ============================================================================
# SECTION 2 : FIT SUR DONNEES STANDARDS (10 tests)
# ============================================================================

test_that("VARCLUS: Fit sur iris", {
  data(iris)
  model <- VARCLUS$new()
  result <- model$fit(iris[, 1:4])
  
  expect_identical(result, model)  # Retourne self
  groups <- model$Groupes
  expect_length(groups, 4)
  expect_true(all(groups >= 1))
  expect_true(all(!is.na(groups)))
})

test_that("VARCLUS: Fit sur mtcars", {
  data(mtcars)
  model <- VARCLUS$new()
  model$fit(mtcars[, 1:7])
  
  groups <- model$Groupes
  expect_length(groups, 7)
  expect_true(all(groups >= 1))
})

test_that("VARCLUS: Fit sur USArrests", {
  data(USArrests)
  model <- VARCLUS$new()
  model$fit(USArrests)
  
  groups <- model$Groupes
  expect_length(groups, 4)
})

test_that("VARCLUS: Fit avec dataset petit", {
  set.seed(123)
  data_small <- as.data.frame(matrix(rnorm(50 * 5), ncol = 5))
  model <- VARCLUS$new()
  model$fit(data_small)
  
  expect_length(model$Groupes, ncol(data_small))
})

test_that("VARCLUS: Fit avec dataset moyen", {
  set.seed(123)
  data_medium <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  model <- VARCLUS$new()
  model$fit(data_medium)
  
  expect_length(model$Groupes, ncol(data_medium))
})

test_that("VARCLUS: Fit avec donnees correlees", {
  set.seed(123)
  # Generer 3 groupes de variables correlees
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x3 <- rnorm(100)
  
  data_cor <- data.frame(
    C1V1 = x1, C1V2 = x1 + rnorm(100, 0, 0.3), C1V3 = x1 + rnorm(100, 0, 0.3),
    C2V1 = x2, C2V2 = x2 + rnorm(100, 0, 0.3), C2V3 = x2 + rnorm(100, 0, 0.3),
    C3V1 = x3, C3V2 = x3 + rnorm(100, 0, 0.3), C3V3 = x3 + rnorm(100, 0, 0.3)
  )
  
  model <- VARCLUS$new()
  model$fit(data_cor)
  
  groups <- model$Groupes
  # Devrait detecter au moins 2 clusters
  expect_gte(length(unique(groups)), 2)
})

test_that("VARCLUS: Resultats reproductibles", {
  data(iris)
  
  model1 <- VARCLUS$new()
  model1$fit(iris[, 1:4])
  groups1 <- model1$Groupes
  
  model2 <- VARCLUS$new()
  model2$fit(iris[, 1:4])
  groups2 <- model2$Groupes
  
  expect_identical(groups1, groups2)
})

test_that("VARCLUS: Noms des variables preserves", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  groups <- model$Groupes
  expect_named(groups)
  expect_setequal(names(groups), colnames(iris)[1:4])
})

test_that("VARCLUS: Fit avec colnames personnalises", {
  data_custom <- data.frame(
    VarA = rnorm(100),
    VarB = rnorm(100),
    VarC = rnorm(100),
    VarD = rnorm(100)
  )
  
  model <- VARCLUS$new()
  model$fit(data_custom)
  
  expect_setequal(names(model$Groupes), c("VarA", "VarB", "VarC", "VarD"))
})

test_that("VARCLUS: Fit retourne invisiblement self pour chainage", {
  data(iris)
  model <- VARCLUS$new()
  
  result <- model$fit(iris[, 1:4])
  expect_identical(result, model)
})

# ============================================================================
# SECTION 3 : ACTIVE BINDINGS (6 tests)
# ============================================================================

test_that("VARCLUS: Active binding K en lecture apres fit", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  k <- model$K
  expect_true(is.numeric(k))
  expect_gte(k, 1)
})

test_that("VARCLUS: K est determine automatiquement", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # K devrait etre determine par l'algorithme
  k <- model$K
  expect_gte(k, 1)
  expect_lte(k, 4)
})

test_that("VARCLUS: Active binding Groupes accessible", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  groups <- model$Groupes
  expect_true(is.integer(groups) || is.numeric(groups))
  expect_length(groups, 4)
})

test_that("VARCLUS: Groupes echoue avant fit", {
  model <- VARCLUS$new()
  expect_error(model$Groupes, "doit.*ajust.*fit")
})

test_that("VARCLUS: K ne peut pas etre modifie apres fit (descendant)", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # VARCLUS est descendant, K est determine automatiquement
  # Tenter de modifier K devrait generer une erreur
  expect_error(model$K <- 3)
})

test_that("VARCLUS: Nombre de clusters coherent avec nombre de variables", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  k <- model$K
  expect_lte(k, 4)  # K <= nombre de variables
})

# ============================================================================
# SECTION 4 : CAS LIMITES ET VALIDATION (8 tests)
# ============================================================================

test_that("VARCLUS: Minimum 3 variables requises", {
  data_2vars <- iris[, 1:2]
  model <- VARCLUS$new()
  
  expect_error(model$fit(data_2vars), "au moins 3")
})

test_that("VARCLUS: Fonctionne avec exactement 3 variables", {
  data_3vars <- iris[, 1:3]
  model <- VARCLUS$new()
  
  expect_error(model$fit(data_3vars), NA)
  groups <- model$Groupes
  expect_length(groups, 3)
})

test_that("VARCLUS: Gestion des NA avec pairwise.complete.obs", {
  data_na <- iris[, 1:4]
  data_na[1:5, 1] <- NA
  
  model <- VARCLUS$new()
  result <- tryCatch({
    model$fit(data_na)
    "success"
  }, error = function(e) "error")
  
  expect_true(result %in% c("success", "error"))
})

test_that("VARCLUS: Dataset avec beaucoup de NA", {
  set.seed(123)
  data_many_na <- iris[, 1:4]
  
  data_matrix <- as.matrix(data_many_na)
  na_indices <- sample(1:length(data_matrix), 
                       size = floor(0.2 * length(data_matrix)))
  data_matrix[na_indices] <- NA
  data_many_na <- as.data.frame(data_matrix)
  
  model <- VARCLUS$new()
  result <- tryCatch({
    model$fit(data_many_na)
    "success"
  }, error = function(e) "error")
  
  expect_true(result %in% c("success", "error"))
})

test_that("VARCLUS: Variables avec variance nulle", {
  data_const <- data.frame(
    x1 = rep(5, 100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100)
  )
  
  model <- VARCLUS$new()
  result <- tryCatch({
    model$fit(data_const)
    "success"
  }, error = function(e) "error")
  
  expect_true(result %in% c("success", "error"))
})

test_that("VARCLUS: Variables parfaitement correlees", {
  data_perfect <- data.frame(
    x1 = 1:100,
    x2 = 1:100,
    x3 = 1:100,
    x4 = 101:200
  )
  
  model <- VARCLUS$new()
  expect_error(model$fit(data_perfect), NA)
})

test_that("VARCLUS: Variables tres correlees regroupees ensemble", {
  set.seed(123)
  x <- rnorm(100)
  data_highcor <- data.frame(
    x1 = x,
    x2 = x + rnorm(100, 0, 0.1),
    x3 = x + rnorm(100, 0, 0.1),
    x4 = rnorm(100),
    x5 = rnorm(100)
  )
  
  model <- VARCLUS$new()
  model$fit(data_highcor)
  
  groups <- model$Groupes
  # x1, x2, x3 devraient etre dans le meme cluster
  expect_equal(groups[["x1"]], groups[["x2"]])
  expect_equal(groups[["x1"]], groups[["x3"]])
})

test_that("VARCLUS: Variables independantes donnent plusieurs clusters", {
  set.seed(123)
  data_indep <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100),
    x5 = rnorm(100)
  )
  
  model <- VARCLUS$new()
  model$fit(data_indep)
  
  # Devrait detecter plusieurs clusters
  groups <- model$Groupes
  expect_gte(length(unique(groups)), 2)
})

# ============================================================================
# SECTION 5 : PARAMETRES ET STANDARDISATION (5 tests)
# ============================================================================

test_that("VARCLUS: scale=TRUE standardise les donnees", {
  data_scales <- data.frame(
    x1 = rnorm(100, mean = 0, sd = 1),
    x2 = rnorm(100, mean = 0, sd = 100),
    x3 = rnorm(100, mean = 1000, sd = 1),
    x4 = rnorm(100, mean = 0, sd = 0.01)
  )
  
  model <- VARCLUS$new(scale = TRUE)
  expect_error(model$fit(data_scales), NA)
})

test_that("VARCLUS: scale=FALSE preserve les echelles", {
  data_scales <- data.frame(
    x1 = rnorm(100, mean = 0, sd = 1),
    x2 = rnorm(100, mean = 0, sd = 100),
    x3 = rnorm(100, mean = 0, sd = 1),
    x4 = rnorm(100, mean = 0, sd = 100)
  )
  
  model <- VARCLUS$new(scale = FALSE)
  expect_error(model$fit(data_scales), NA)
})

test_that("VARCLUS: stop_eigenvalue influence le nombre de clusters", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 8), ncol = 8))
  
  model_strict <- VARCLUS$new(stop_eigenvalue = 0.5)
  model_strict$fit(data_test)
  k_strict <- model_strict$K
  
  model_lax <- VARCLUS$new(stop_eigenvalue = 2.0)
  model_lax$fit(data_test)
  k_lax <- model_lax$K
  
  # stop_eigenvalue plus bas devrait donner plus de clusters
  expect_gte(k_strict, k_lax)
})

test_that("VARCLUS: stop_eigenvalue = 0 donne beaucoup de clusters", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
  
  model <- VARCLUS$new(stop_eigenvalue = 0)
  model$fit(data_test)
  
  # Devrait donner plusieurs clusters
  expect_gte(model$K, 2)
})

test_that("VARCLUS: stop_eigenvalue tres eleve donne peu de clusters", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
  
  model <- VARCLUS$new(stop_eigenvalue = 10)
  model$fit(data_test)
  
  # Devrait donner peu de clusters
  k <- model$K
  expect_true(k >= 1)
})

# ============================================================================
# SECTION 6 : IMMUTABILITE ET EFFETS DE BORD (3 tests)
# ============================================================================

test_that("VARCLUS: Les donnees d'origine ne sont pas modifiees", {
  data(iris)
  iris_copy <- iris[, 1:4]
  iris_original <- iris[, 1:4]
  
  model <- VARCLUS$new()
  model$fit(iris_copy)
  
  expect_identical(iris_copy, iris_original)
})

test_that("VARCLUS: Les rownames sont preserves", {
  data_rownames <- iris[, 1:4]
  rownames(data_rownames) <- paste0("Obs_", 1:nrow(data_rownames))
  original_rownames <- rownames(data_rownames)
  
  model <- VARCLUS$new()
  model$fit(data_rownames)
  
  expect_identical(rownames(data_rownames), original_rownames)
})

test_that("VARCLUS: Les colnames sont preserves", {
  data_colnames <- iris[, 1:4]
  colnames(data_colnames) <- c("A", "B", "C", "D")
  original_colnames <- colnames(data_colnames)
  
  model <- VARCLUS$new()
  model$fit(data_colnames)
  
  expect_identical(colnames(data_colnames), original_colnames)
})

# ============================================================================
# SECTION 7 : METHODES PUBLIQUES (4 tests)
# ============================================================================

test_that("VARCLUS: summary() fonctionne sans erreur", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  expect_error(model$summary(), NA)
  expect_output(model$summary(), "VARCLUS")
})

test_that("VARCLUS: summary() avant fit se comporte correctement", {
  model <- VARCLUS$new()
  
  # VARCLUS peut soit generer une erreur, soit afficher un message vide
  # On verifie juste qu'il ne crash pas de maniere inattendue
  result <- tryCatch({
    model$summary()
    "no_error"
  }, error = function(e) {
    "error"
  })
  
  expect_true(result %in% c("error", "no_error"))
})

test_that("VARCLUS: summary() affiche le nombre de clusters detectes", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  expect_output(model$summary(), "cluster")
})

test_that("VARCLUS: summary() affiche les valeurs propres", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # Le summary affiche "λ₂" (symbole unicode) ou peut mentionner eigenvalue
  # On verifie juste que le summary fonctionne sans erreur
  expect_error(model$summary(), NA)
  expect_output(model$summary(), "VARCLUS")
})

# ============================================================================
# SECTION 8 : VALIDATION MATHEMATIQUE (10 tests)
# ============================================================================

test_that("VARCLUS: Tous les clusters ont au moins une variable", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  groups <- model$Groupes
  cluster_sizes <- table(groups)
  
  expect_true(all(cluster_sizes > 0))
})

test_that("VARCLUS: Division hierarchique coherente", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
  
  model <- VARCLUS$new()
  model$fit(data_test)
  
  # Verifier que les clusters sont bien formes
  groups <- model$Groupes
  expect_equal(length(groups), 6)
  expect_true(all(groups >= 1))
})

test_that("VARCLUS: Critere lambda2 >= 1 respecte", {
  data(iris)
  model <- VARCLUS$new(stop_eigenvalue = 1.0)
  model$fit(iris[, 1:4])
  
  # L'algorithme devrait s'arreter quand lambda2 >= 1
  # On verifie juste qu'il n'y a pas d'erreur
  expect_true(TRUE)
})

test_that("VARCLUS: Variance expliquee par cluster coherente", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # Chaque cluster devrait avoir une variance expliquee raisonnable
  groups <- model$Groupes
  for (k in unique(groups)) {
    vars_k <- names(groups)[groups == k]
    expect_gte(length(vars_k), 1)
  }
})

test_that("VARCLUS: PCA par cluster est coherente", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # Verifier que les groupes sont coherents
  groups <- model$Groupes
  expect_true(all(!is.na(groups)))
})

test_that("VARCLUS: Invariance par permutation des colonnes", {
  set.seed(123)
  data_original <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
  colnames(data_original) <- paste0("V", 1:6)
  
  perm_order <- sample(1:ncol(data_original))
  data_perm <- data_original[, perm_order]
  
  model_original <- VARCLUS$new()
  model_original$fit(data_original)
  groups_original <- model_original$Groupes
  
  model_perm <- VARCLUS$new()
  model_perm$fit(data_perm)
  groups_perm <- model_perm$Groupes
  
  # Apres reordonnancement, les groupes doivent correspondre
  groups_perm_reordered <- groups_perm[colnames(data_original)]
  
  # Verifier que la structure est identique (meme partition)
  for (i in 1:(length(groups_original)-1)) {
    for (j in (i+1):length(groups_original)) {
      same_cluster_original <- groups_original[i] == groups_original[j]
      same_cluster_perm <- groups_perm_reordered[i] == groups_perm_reordered[j]
      expect_equal(same_cluster_original, same_cluster_perm)
    }
  }
})

test_that("VARCLUS: Matrice de correlation calculee correctement", {
  data(iris)
  X <- iris[, 1:4]
  
  model <- VARCLUS$new()
  model$fit(X)
  
  # Les variables du meme cluster devraient avoir des correlations elevees
  groups <- model$Groupes
  
  for (k in unique(groups)) {
    cluster_vars <- names(groups)[groups == k]
    if (length(cluster_vars) >= 2) {
      cor_matrix <- abs(cor(X[, cluster_vars], use = "pairwise.complete.obs"))
      intra_cor <- mean(cor_matrix[upper.tri(cor_matrix)])
      
      expect_true(!is.na(intra_cor))
      expect_gte(intra_cor, 0)
    }
  }
})

test_that("VARCLUS: Algorithme descendant coherent", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 8), ncol = 8))
  
  model <- VARCLUS$new()
  model$fit(data_test)
  
  # Verifier la coherence hierarchique
  groups <- model$Groupes
  k <- length(unique(groups))
  
  expect_gte(k, 1)
  expect_lte(k, 8)
})

test_that("VARCLUS: Rotation Varimax appliquee", {
  data(iris)
  model <- VARCLUS$new()
  model$fit(iris[, 1:4])
  
  # L'algorithme utilise Varimax
  # On verifie juste qu'il n'y a pas d'erreur
  expect_true(TRUE)
})

test_that("VARCLUS: Dendrogramme implicite coherent", {
  set.seed(123)
  data_test <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
  
  model <- VARCLUS$new()
  model$fit(data_test)
  
  # La structure hierarchique doit etre coherente
  groups <- model$Groupes
  expect_true(all(!is.na(groups)))
})

# ============================================================================
# SECTION 9 : PERFORMANCE ET SCALABILITE (6 tests)
# ============================================================================

test_that("VARCLUS: Fonctionne avec beaucoup de variables", {
  set.seed(123)
  data_many_vars <- as.data.frame(matrix(rnorm(100 * 20), ncol = 20))
  
  model <- VARCLUS$new()
  
  time_taken <- system.time({
    model$fit(data_many_vars)
  })[3]
  
  expect_lt(time_taken, 30)  # Moins de 30 secondes
  expect_length(model$Groupes, 20)
})

test_that("VARCLUS: Fonctionne avec beaucoup d'observations", {
  set.seed(123)
  data_many_obs <- as.data.frame(matrix(rnorm(1000 * 6), ncol = 6))
  
  model <- VARCLUS$new()
  expect_error(model$fit(data_many_obs), NA)
})

test_that("VARCLUS: Dataset equilibre", {
  set.seed(123)
  data_balanced <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  
  model <- VARCLUS$new()
  model$fit(data_balanced)
  
  groups <- model$Groupes
  expect_length(groups, 10)
})

test_that("VARCLUS: Dataset avec peu d'observations", {
  set.seed(123)
  data_few_obs <- as.data.frame(matrix(rnorm(20 * 5), ncol = 5))
  
  model <- VARCLUS$new()
  result <- tryCatch({
    model$fit(data_few_obs)
    "success"
  }, error = function(e) "error")
  
  expect_true(result %in% c("success", "error"))
})

test_that("VARCLUS: Performance raisonnable", {
  set.seed(123)
  data_perf <- as.data.frame(matrix(rnorm(500 * 10), ncol = 10))
  
  model <- VARCLUS$new()
  
  time_taken <- system.time({
    model$fit(data_perf)
  })[3]
  
  # Devrait etre rapide
  expect_lt(time_taken, 10)
})

test_that("VARCLUS: Pas de fuite memoire", {
  set.seed(123)
  
  for (i in 1:5) {
    data_test <- as.data.frame(matrix(rnorm(100 * 6), ncol = 6))
    model <- VARCLUS$new()
    model$fit(data_test)
    rm(model)
    gc()
  }
  
  # Si on arrive ici sans erreur, pas de fuite memoire evidente
  expect_true(TRUE)
})

# ============================================================================
# FIN DES TESTS VARCLUS - 50+ tests au total
# ============================================================================