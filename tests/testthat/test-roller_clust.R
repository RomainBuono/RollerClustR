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

test_that("roller_clust works with tandem method", {
  # Create mixed data (categorical + numeric)
  data(iris)
  iris_mixed <- iris
  iris_mixed$Species_char <- as.character(iris$Species)
  
  result <- roller_clust(
    iris_mixed[, c(1:4, 6)],  # numeric + categorical
    method = "tandem",
    K = 3,
    n_bins = 3
  )
  
  expect_s3_class(result, "TandemVarClust")
  expect_equal(result$K, 3)
})

test_that("roller_clust validates K parameter", {
  data(iris)
  
  expect_error(
    roller_clust(iris[, 1:4], K = 1),
    "K must be an integer >= 2"
  )
  
  expect_error(
    roller_clust(iris[, 1:4], K = -3),
    "K must be an integer >= 2"
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
    "X must be a data.frame or a matrix"  
  )
})

test_that("roller_clust default parameters work", {
  data(iris)
  
  # Devrait utiliser les valeurs par défaut
  result <- roller_clust(iris[, 1:4])
  
  expect_s3_class(result, "VAR_CAH")  # méthode par défaut
  expect_equal(result$K, 2)  # K par défaut
})
