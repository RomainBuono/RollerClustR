test_that("roller_clust works with var_cah method", {
  data(iris)
  
  result <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)
  
  expect_s3_class(result, "VAR_CAH")
  expect_equal(result$K, 2)
  expect_length(result$Groupes, 4)
})

test_that("roller_clust works with varclus method", {
  data(iris)
  
  result <- roller_clust(iris[, 1:4], method = "varclus", K = 2)
  
  expect_s3_class(result, "VARCLUS")
  # VARCLUS détermine automatiquement K, donc on vérifie juste que K >= 1
  expect_true(result$K >= 1)
})

test_that("roller_clust works with kmodes method", {
  data(Titanic)
  titanic_df <- as.data.frame(Titanic)
  
  result <- roller_clust(
    titanic_df[, c("Class", "Sex", "Age")],
    method = "kmodes",
    K = 2
  )
  
  expect_s3_class(result, "KmodesVarClust")
  expect_equal(result$K, 2)
})

test_that("roller_clust validates K parameter", {
  data(iris)
  
  expect_error(
    roller_clust(iris[, 1:4], K = 1),
    "K doit être un nombre entier >= 2"
  )
  
  expect_error(
    roller_clust(iris[, 1:4], K = -3),
    "K doit être un nombre entier >= 2"
  )
})

test_that("roller_clust validates method parameter", {
  data(iris)
  
  expect_error(
    roller_clust(iris[, 1:4], method = "invalid_method"),
    "'arg' should be one of"
  )
})

test_that("roller_clust validates input data", {
  # Pas un data.frame ni une matrice
  expect_error(
    roller_clust(c(1, 2, 3), K = 2),
    "X doit être un data.frame ou une matrice"
  )
})

test_that("roller_clust default parameters work", {
  data(iris)
  
  # Devrait utiliser les valeurs par défaut
  result <- roller_clust(iris[, 1:4])
  
  expect_s3_class(result, "VAR_CAH")  # méthode par défaut
  expect_equal(result$K, 2)  # K par défaut
})
