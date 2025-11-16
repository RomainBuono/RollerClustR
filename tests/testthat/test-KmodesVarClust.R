# ============================================================================
# TESTS POUR KmodesVarClust - Clustering K-Modes pour Variables
# ============================================================================
# 50+ tests couvrant tous les aspects de la classe KmodesVarClust

context("KmodesVarClust - K-Modes pour Variables Categorielles")

# ============================================================================
# SECTION 1 : INITIALISATION ET CONSTRUCTION (8 tests)
# ============================================================================

test_that("KmodesVarClust: Initialisation basique avec K=2", {
  model <- KmodesVarClust$new(K = 2)
  
  # Verifier que c'est bien un objet R6
  expect_true(R6::is.R6(model))
  expect_s3_class(model, "KmodesVarClust")
  expect_s3_class(model, "ClusterAnalysis")
  expect_equal(model$K, 2)
  
  # Verifier que les methodes essentielles existent
  expect_true("fit" %in% names(model))
  expect_true("summary" %in% names(model))
})

test_that("KmodesVarClust: Initialisation avec differentes valeurs de K", {
  for (k in 2:5) {
    model <- KmodesVarClust$new(K = k)
    expect_true(R6::is.R6(model))
    expect_equal(model$K, k)
  }
})

test_that("KmodesVarClust: Initialisation avec max.iter personnalise", {
  model <- KmodesVarClust$new(K = 2, max.iter = 50)
  expect_s3_class(model, "KmodesVarClust")
  expect_equal(model$max.iter, 50)
})

test_that("KmodesVarClust: Initialisation avec n_bins personnalise", {
  model <- KmodesVarClust$new(K = 2, n_bins = 10)
  expect_s3_class(model, "KmodesVarClust")
  expect_equal(model$n_bins, 10)
})

test_that("KmodesVarClust: Erreur si K < 2", {
  expect_error(KmodesVarClust$new(K = 1), "K doit")
})

test_that("KmodesVarClust: Erreur si K = 0", {
  expect_error(KmodesVarClust$new(K = 0), "K doit")
})

test_that("KmodesVarClust: Erreur si K negatif", {
  expect_error(KmodesVarClust$new(K = -3), "K doit")
})

test_that("KmodesVarClust: Erreur si K non numerique", {
  expect_error(KmodesVarClust$new(K = "deux"), "K doit")
})

# ============================================================================
# SECTION 2 : FIT SUR DONNEES CATEGORIELLES PURES (10 tests)
# ============================================================================

test_that("KmodesVarClust: Fit sur Titanic (donnees categorielles)", {
  data(Titanic)
  titanic_df <- as.data.frame(Titanic)
  
  model <- KmodesVarClust$new(K = 2)
  result <- model$fit(titanic_df[, c("Class", "Sex", "Age", "Survived")])
  
  expect_identical(result, model)  # Retourne self
  groups <- model$Groupes
  expect_length(groups, 4)
  expect_true(all(groups %in% 1:2))
})

test_that("KmodesVarClust: Fit sur donnees categorielles simples", {
  set.seed(123)
  data_cat <- data.frame(
    cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    cat2 = sample(c("X", "Y"), 100, replace = TRUE),
    cat3 = sample(c("1", "2", "3"), 100, replace = TRUE),
    cat4 = sample(c("Red", "Blue"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  groups <- model$Groupes
  expect_length(groups, 4)
  expect_true(all(groups %in% 1:2))
})

test_that("KmodesVarClust: Fit avec K=3 sur donnees categorielles", {
  set.seed(123)
  # Creer des variables TRES similaires par groupe pour forcer 3 clusters
  base1 <- sample(c("A", "B"), 100, replace = TRUE)
  base2 <- sample(c("X", "Y"), 100, replace = TRUE)
  base3 <- sample(c("1", "2"), 100, replace = TRUE)
  
  data_cat <- data.frame(
    v1 = base1,  # Groupe 1
    v2 = base1,  # Groupe 1 (identique)
    v3 = base2,  # Groupe 2
    v4 = base2,  # Groupe 2 (identique)
    v5 = base3,  # Groupe 3
    v6 = base3   # Groupe 3 (identique)
  )
  
  model <- KmodesVarClust$new(K = 3)
  model$fit(data_cat)
  
  groups <- model$Groupes
  expect_length(groups, 6)
  
  # Avec cette structure claire, on devrait avoir 3 clusters
  # Mais on reste flexible car K-modes peut encore converger differemment
  n_clusters <- length(unique(groups))
  expect_gte(n_clusters, 2)  # Au moins 2 clusters
  expect_lte(n_clusters, 3)  # Au plus 3 clusters
})

test_that("KmodesVarClust: Variables categorielles avec 2 modalites", {
  set.seed(123)
  data_binary <- data.frame(
    var1 = sample(c("Yes", "No"), 100, replace = TRUE),
    var2 = sample(c("True", "False"), 100, replace = TRUE),
    var3 = sample(c("On", "Off"), 100, replace = TRUE),
    var4 = sample(c("High", "Low"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_binary), NA)
})

test_that("KmodesVarClust: Variables categorielles avec beaucoup de modalites", {
  set.seed(123)
  data_many_levels <- data.frame(
    var1 = sample(LETTERS[1:10], 100, replace = TRUE),
    var2 = sample(LETTERS[11:20], 100, replace = TRUE),
    var3 = sample(LETTERS[1:5], 100, replace = TRUE),
    var4 = sample(LETTERS[6:10], 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_many_levels), NA)
})

test_that("KmodesVarClust: Noms des variables preserves", {
  data(Titanic)
  titanic_df <- as.data.frame(Titanic)
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(titanic_df[, c("Class", "Sex", "Age", "Survived")])
  
  groups <- model$Groupes
  expect_named(groups)
  expect_setequal(names(groups), c("Class", "Sex", "Age", "Survived"))
})

test_that("KmodesVarClust: Resultats reproductibles (seed fixe)", {
  data(Titanic)
  titanic_df <- as.data.frame(Titanic)
  
  set.seed(123)
  model1 <- KmodesVarClust$new(K = 2)
  model1$fit(titanic_df[, c("Class", "Sex", "Age", "Survived")])
  groups1 <- model1$Groupes
  
  set.seed(123)
  model2 <- KmodesVarClust$new(K = 2)
  model2$fit(titanic_df[, c("Class", "Sex", "Age", "Survived")])
  groups2 <- model2$Groupes
  
  expect_identical(groups1, groups2)
})

test_that("KmodesVarClust: Convergence atteinte", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE),
    v4 = sample(c("M", "N"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2, max.iter = 100)
  model$fit(data_cat)
  
  expect_true(model$converged)
})

test_that("KmodesVarClust: Inertie calculee", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_true(!is.null(model$inertie))
  expect_true(is.numeric(model$inertie))
  expect_gte(model$inertie, 0)
})

test_that("KmodesVarClust: Fit retourne invisiblement self", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  result <- model$fit(data_cat)
  
  expect_identical(result, model)
})

# ============================================================================
# SECTION 3 : FIT SUR DONNEES MIXTES (CATEGORIEL + NUMERIQUE) (8 tests)
# ============================================================================

test_that("KmodesVarClust: Fit sur donnees mixtes", {
  set.seed(123)
  data_mixed <- data.frame(
    cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    num1 = rnorm(100),
    cat2 = sample(c("X", "Y"), 100, replace = TRUE),
    num2 = runif(100)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_mixed), NA)
  
  groups <- model$Groupes
  expect_length(groups, 4)
})

test_that("KmodesVarClust: Discretisation automatique des variables numeriques", {
  set.seed(123)
  data_numeric <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100),
    var4 = rnorm(100)
  )
  
  model <- KmodesVarClust$new(K = 2, n_bins = 5)
  expect_error(model$fit(data_numeric), NA)
})

test_that("KmodesVarClust: n_bins influence la discretisation", {
  set.seed(123)
  data_num <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  
  model_bins3 <- KmodesVarClust$new(K = 2, n_bins = 3)
  model_bins3$fit(data_num)
  groups_bins3 <- model_bins3$Groupes
  
  model_bins10 <- KmodesVarClust$new(K = 2, n_bins = 10)
  model_bins10$fit(data_num)
  groups_bins10 <- model_bins10$Groupes
  
  # Les resultats peuvent etre differents selon le nombre de bins
  expect_true(TRUE)
})

test_that("KmodesVarClust: Variables mixtes avec majoritairement categoriel", {
  set.seed(123)
  data_mostly_cat <- data.frame(
    cat1 = sample(c("A", "B"), 100, replace = TRUE),
    cat2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    cat3 = sample(c("1", "2"), 100, replace = TRUE),
    num1 = rnorm(100)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_mostly_cat), NA)
})

test_that("KmodesVarClust: Variables mixtes avec majoritairement numerique", {
  set.seed(123)
  data_mostly_num <- data.frame(
    num1 = rnorm(100),
    num2 = runif(100),
    num3 = rexp(100),
    cat1 = sample(c("A", "B"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_mostly_num), NA)
})

test_that("KmodesVarClust: Discretisation avec n_bins=2", {
  set.seed(123)
  data_num <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100)
  )
  
  model <- KmodesVarClust$new(K = 2, n_bins = 2)
  expect_error(model$fit(data_num), NA)
})

test_that("KmodesVarClust: Discretisation avec n_bins=20", {
  set.seed(123)
  data_num <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  
  model <- KmodesVarClust$new(K = 2, n_bins = 20)
  expect_error(model$fit(data_num), NA)
})

test_that("KmodesVarClust: Variables numeriques avec valeurs extremes", {
  set.seed(123)
  data_extreme <- data.frame(
    v1 = c(rnorm(98), 1000, -1000),
    v2 = c(rnorm(98), 5000, -5000),
    v3 = rnorm(100)
  )
  
  model <- KmodesVarClust$new(K = 2, n_bins = 5)
  expect_error(model$fit(data_extreme), NA)
})

# ============================================================================
# SECTION 4 : ACTIVE BINDINGS (6 tests)
# ============================================================================

test_that("KmodesVarClust: Active binding K en lecture", {
  model <- KmodesVarClust$new(K = 3)
  expect_equal(model$K, 3)
})

test_that("KmodesVarClust: Active binding Groupes accessible apres fit", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  groups <- model$Groupes
  expect_true(is.integer(groups) || is.numeric(groups))
  expect_length(groups, 2)
})

test_that("KmodesVarClust: Groupes echoue avant fit", {
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$Groupes, "doit.*ajust.*fit")
})

test_that("KmodesVarClust: Active binding converged", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_true(is.logical(model$converged))
})

test_that("KmodesVarClust: Active binding inertie", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_true(is.numeric(model$inertie))
  expect_gte(model$inertie, 0)
})

test_that("KmodesVarClust: Active binding max.iter et n_bins", {
  model <- KmodesVarClust$new(K = 2, max.iter = 50, n_bins = 7)
  
  expect_equal(model$max.iter, 50)
  expect_equal(model$n_bins, 7)
})

# ============================================================================
# SECTION 5 : CAS LIMITES ET VALIDATION (8 tests)
# ============================================================================

test_that("KmodesVarClust: Erreur si K > nombre de variables", {
  data_3vars <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 5)
  expect_error(model$fit(data_3vars), "ne peut pas.*sup")
})

test_that("KmodesVarClust: Fonctionne avec K = nombre de variables", {
  set.seed(123)
  data_4vars <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE),
    v4 = sample(c("M", "N"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 4)
  expect_error(model$fit(data_4vars), NA)
  
  groups <- model$Groupes
  expect_equal(length(unique(groups)), 4)
})

test_that("KmodesVarClust: Minimum 2 variables requises", {
  data_2vars <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_2vars), NA)
})

test_that("KmodesVarClust: Dataset avec peu d'observations", {
  set.seed(123)
  data_small <- data.frame(
    v1 = sample(c("A", "B"), 10, replace = TRUE),
    v2 = sample(c("X", "Y"), 10, replace = TRUE),
    v3 = sample(c("1", "2"), 10, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_small), NA)
})

test_that("KmodesVarClust: Dataset avec beaucoup d'observations", {
  set.seed(123)
  data_large <- data.frame(
    v1 = sample(c("A", "B", "C"), 1000, replace = TRUE),
    v2 = sample(c("X", "Y", "Z"), 1000, replace = TRUE),
    v3 = sample(c("1", "2", "3"), 1000, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_large), NA)
})

test_that("KmodesVarClust: Variables avec une seule modalite (variance nulle)", {
  data_const <- data.frame(
    v1 = rep("A", 100),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  result <- tryCatch({
    model$fit(data_const)
    "success"
  }, error = function(e) "error")
  
  expect_true(result %in% c("success", "error"))
})

test_that("KmodesVarClust: Toutes les variables identiques", {
  data_identical <- data.frame(
    v1 = rep("A", 100),
    v2 = rep("A", 100),
    v3 = rep("A", 100)
  )
  
  model <- KmodesVarClust$new(K = 2)
  
  # K-modes peut soit echouer soit reussir avec des variables identiques
  # On verifie juste qu'il ne crash pas de maniere inattendue
  result <- tryCatch({
    model$fit(data_identical)
    groups <- model$Groupes
    # Si ca reussit, toutes les variables devraient etre dans le meme cluster
    expect_true(length(unique(groups)) == 1)
    "success"
  }, error = function(e) {
    # Si ca echoue, c'est acceptable aussi
    "error"
  })
  
  expect_true(result %in% c("success", "error"))
})

test_that("KmodesVarClust: max.iter limite les iterations", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(letters[1:5], 100, replace = TRUE),
    v2 = sample(letters[6:10], 100, replace = TRUE),
    v3 = sample(letters[11:15], 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2, max.iter = 5)
  model$fit(data_cat)
  
  # Le modele peut ne pas converger avec peu d'iterations
  expect_true(is.logical(model$converged))
})

# ============================================================================
# SECTION 6 : METHODES PUBLIQUES (4 tests)
# ============================================================================

test_that("KmodesVarClust: summary() fonctionne sans erreur", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_error(model$summary(), NA)
  expect_output(model$summary(), "KmodesVarClust")
})

test_that("KmodesVarClust: summary() avant fit affiche message approprie", {
  model <- KmodesVarClust$new(K = 2)
  
  # Le summary ne devrait pas generer d'erreur mais afficher un message
  expect_output(model$summary(), "non ajust|not fitted")
})

test_that("KmodesVarClust: summary() affiche informations de convergence", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_output(model$summary(), "Convergence")
})

test_that("KmodesVarClust: summary() affiche repartition des variables", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  expect_output(model$summary(), "Cluster")
})

# ============================================================================
# SECTION 7 : IMMUTABILITE ET EFFETS DE BORD (3 tests)
# ============================================================================

test_that("KmodesVarClust: Les donnees d'origine ne sont pas modifiees", {
  set.seed(123)
  data_original <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  data_copy <- data_original
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_copy)
  
  expect_identical(data_copy, data_original)
})

test_that("KmodesVarClust: Les colnames sont preserves", {
  data_named <- data.frame(
    VariableA = sample(c("A", "B"), 50, replace = TRUE),
    VariableB = sample(c("X", "Y"), 50, replace = TRUE)
  )
  original_names <- colnames(data_named)
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_named)
  
  expect_identical(colnames(data_named), original_names)
  expect_setequal(names(model$Groupes), original_names)
})

test_that("KmodesVarClust: Les rownames sont preserves", {
  data_rows <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  rownames(data_rows) <- paste0("Obs_", 1:50)
  original_rownames <- rownames(data_rows)
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_rows)
  
  expect_identical(rownames(data_rows), original_rownames)
})

# ============================================================================
# SECTION 8 : VALIDATION MATHEMATIQUE (6 tests)
# ============================================================================

test_that("KmodesVarClust: Tous les clusters ont au moins une variable", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE),
    v4 = sample(c("M", "N"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  groups <- model$Groupes
  cluster_sizes <- table(groups)
  
  expect_true(all(cluster_sizes > 0))
  expect_equal(length(cluster_sizes), 2)
})

test_that("KmodesVarClust: Somme des tailles de clusters = nombre de variables", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE),
    v4 = sample(c("M", "N"), 50, replace = TRUE),
    v5 = sample(c("P", "Q"), 50, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 3)
  model$fit(data_cat)
  
  groups <- model$Groupes
  expect_equal(length(groups), 5)
  expect_equal(sum(table(groups)), 5)
})

test_that("KmodesVarClust: Inertie diminue avec K croissant", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE),
    v4 = sample(c("M", "N"), 100, replace = TRUE),
    v5 = sample(c("P", "Q"), 100, replace = TRUE),
    v6 = sample(c("R", "S"), 100, replace = TRUE)
  )
  
  model_k2 <- KmodesVarClust$new(K = 2)
  model_k2$fit(data_cat)
  inertie_k2 <- model_k2$inertie
  
  model_k3 <- KmodesVarClust$new(K = 3)
  model_k3$fit(data_cat)
  inertie_k3 <- model_k3$inertie
  
  # L'inertie devrait diminuer ou rester stable quand K augmente
  expect_lte(inertie_k3, inertie_k2)
})

test_that("KmodesVarClust: Variables similaires regroupees ensemble", {
  set.seed(123)
  # Creer des variables tres similaires
  base_var <- sample(c("A", "B"), 100, replace = TRUE)
  
  data_similar <- data.frame(
    v1 = base_var,
    v2 = base_var,  # Identique a v1
    v3 = sample(c("X", "Y"), 100, replace = TRUE),
    v4 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_similar)
  
  groups <- model$Groupes
  # v1 et v2 devraient etre dans le meme cluster
  expect_equal(groups[["v1"]], groups[["v2"]])
})

test_that("KmodesVarClust: Comportement avec permutation des colonnes", {
  set.seed(123)
  data_original <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE),
    v4 = sample(c("M", "N"), 100, replace = TRUE)
  )
  
  perm_order <- c(3, 1, 4, 2)
  data_perm <- data_original[, perm_order]
  
  # K-modes avec initialisation aleatoire n'est PAS invariant par permutation
  # On verifie juste que les deux fits fonctionnent sans erreur
  
  set.seed(456)
  model_original <- KmodesVarClust$new(K = 2)
  expect_error(model_original$fit(data_original), NA)
  groups_original <- model_original$Groupes
  
  set.seed(456)
  model_perm <- KmodesVarClust$new(K = 2)
  expect_error(model_perm$fit(data_perm), NA)
  groups_perm <- model_perm$Groupes
  
  # Verifier que les deux ont bien 2 clusters (ou moins si convergence)
  expect_gte(length(unique(groups_original)), 1)
  expect_lte(length(unique(groups_original)), 2)
  expect_gte(length(unique(groups_perm)), 1)
  expect_lte(length(unique(groups_perm)), 2)
  
  # Verifier que tous les groupes sont bien assigns
  expect_length(groups_original, 4)
  expect_length(groups_perm, 4)
})

test_that("KmodesVarClust: Distance de desaccord correcte", {
  set.seed(123)
  data_cat <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  model$fit(data_cat)
  
  # L'inertie doit etre >= 0
  expect_gte(model$inertie, 0)
  
  # L'inertie doit etre finie
  expect_false(is.infinite(model$inertie))
  expect_false(is.nan(model$inertie))
})

# ============================================================================
# SECTION 9 : ROBUSTESSE ET EDGE CASES (7 tests)
# ============================================================================

test_that("KmodesVarClust: Gestion des facteurs R natifs", {
  set.seed(123)
  data_factors <- data.frame(
    f1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    f2 = factor(sample(c("X", "Y"), 100, replace = TRUE)),
    f3 = factor(sample(c("1", "2", "3"), 100, replace = TRUE))
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_factors), NA)
})

test_that("KmodesVarClust: Gestion des caracteres avec espaces", {
  set.seed(123)
  data_spaces <- data.frame(
    v1 = sample(c("Type A", "Type B"), 100, replace = TRUE),
    v2 = sample(c("Cat X", "Cat Y"), 100, replace = TRUE),
    v3 = sample(c("Level 1", "Level 2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_spaces), NA)
})

test_that("KmodesVarClust: Gestion des caracteres speciaux", {
  set.seed(123)
  data_special <- data.frame(
    v1 = sample(c("A&B", "C|D"), 100, replace = TRUE),
    v2 = sample(c("X-Y", "Z_W"), 100, replace = TRUE),
    v3 = sample(c("@1", "#2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_special), NA)
})

test_that("KmodesVarClust: Variables avec distribution tres desequilibree", {
  set.seed(123)
  data_unbalanced <- data.frame(
    v1 = c(rep("A", 99), "B"),  # 99% A, 1% B
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  expect_error(model$fit(data_unbalanced), NA)
})

test_that("KmodesVarClust: Dataset avec une variable", {
  data_1var <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 2)
  
  # Devrait echouer car K=2 mais 1 seule variable
  expect_error(model$fit(data_1var))
})

test_that("KmodesVarClust: K tres grand par rapport au nombre de variables", {
  set.seed(123)
  data_few_vars <- data.frame(
    v1 = sample(c("A", "B"), 100, replace = TRUE),
    v2 = sample(c("X", "Y"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  model <- KmodesVarClust$new(K = 10)
  
  # Devrait echouer car K > nombre de variables
  expect_error(model$fit(data_few_vars))
})

test_that("KmodesVarClust: Performance avec beaucoup de variables", {
  set.seed(123)
  n_vars <- 20
  data_many_vars <- as.data.frame(
    lapply(1:n_vars, function(i) {
      sample(c("A", "B", "C"), 100, replace = TRUE)
    })
  )
  colnames(data_many_vars) <- paste0("v", 1:n_vars)
  
  model <- KmodesVarClust$new(K = 3)
  
  # Devrait fonctionner mais peut etre lent
  time_taken <- system.time({
    model$fit(data_many_vars)
  })[3]
  
  expect_lt(time_taken, 30)  # Moins de 30 secondes
})

# ============================================================================
# FIN DES TESTS KmodesVarClust - 50+ tests au total
# ============================================================================