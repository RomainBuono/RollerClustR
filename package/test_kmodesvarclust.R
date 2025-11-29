
library(testthat)
library(R6)
setwd("C:/Users/Romain_admin/Documents/GitHub/RollerClustR/package")
source("ClusterAnalysis_parentclass.R")
source("VAR_CAH.R")
source("KmodesVarClust.R")
source("VAR_CAH.R")
source("wrapper.R")
source("user_functions.R")


# =============================================================================
# SUITE DE 100 TESTS POUR LE PACKAGE KmodesVarClust
# =============================================================================

context("Tests de validation du package KmodesVarClust")

# =============================================================================
# SECTION 1: Tests sur petits datasets (n < 100, p < 10)
# Tests 1-20
# =============================================================================

test_that("Test 1: Dataset minimal (n=10, p=3)", {
  set.seed(123)
  data <- data.frame(
    v1 = sample(letters[1:3], 10, replace = TRUE),
    v2 = sample(letters[1:3], 10, replace = TRUE),
    v3 = sample(letters[1:3], 10, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_s3_class(result, "R6")
  expect_equal(nrow(result$cluster_assignments), nrow(data))
})

test_that("Test 2: Dataset minimal avec 2 variables (n=15, p=2)", {
  set.seed(124)
  data <- data.frame(
    v1 = sample(LETTERS[1:4], 15, replace = TRUE),
    v2 = sample(LETTERS[1:4], 15, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 2)
})

test_that("Test 3: Dataset avec n=20, p=4", {
  set.seed(125)
  data <- data.frame(
    v1 = sample(c("A", "B", "C"), 20, replace = TRUE),
    v2 = sample(c("X", "Y", "Z"), 20, replace = TRUE),
    v3 = sample(c("1", "2"), 20, replace = TRUE),
    v4 = sample(c("α", "β"), 20, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(ncol(result$variable_clusters), 4)
})

test_that("Test 4: Dataset avec n=25, p=5", {
  set.seed(126)
  data <- data.frame(
    v1 = sample(letters[1:2], 25, replace = TRUE),
    v2 = sample(letters[3:5], 25, replace = TRUE),
    v3 = sample(letters[6:8], 25, replace = TRUE),
    v4 = sample(letters[9:10], 25, replace = TRUE),
    v5 = sample(letters[11:13], 25, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 5: Dataset avec n=30, p=3, haute cardinalité", {
  set.seed(127)
  data <- data.frame(
    v1 = sample(letters[1:10], 30, replace = TRUE),
    v2 = sample(letters[11:20], 30, replace = TRUE),
    v3 = sample(letters[21:26], 30, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 6: Dataset avec n=35, p=6", {
  set.seed(128)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C"), 35*6, replace = TRUE),
    nrow = 35, ncol = 6
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 35)
})

test_that("Test 7: Dataset avec n=40, p=4, 2 clusters", {
  set.seed(129)
  data <- data.frame(
    v1 = c(rep("A", 20), rep("B", 20)),
    v2 = c(rep("X", 20), rep("Y", 20)),
    v3 = sample(c("1", "2"), 40, replace = TRUE),
    v4 = sample(c("α", "β", "γ"), 40, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(length(unique(result$cluster_assignments$cluster)) >= 1)
})

test_that("Test 8: Dataset avec n=45, p=7", {
  set.seed(130)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 45*7, replace = TRUE),
    nrow = 45, ncol = 7
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 9: Dataset avec n=50, p=5, faible cardinalité", {
  set.seed(131)
  data <- data.frame(
    v1 = sample(c("A", "B"), 50, replace = TRUE),
    v2 = sample(c("X", "Y"), 50, replace = TRUE),
    v3 = sample(c("1", "2"), 50, replace = TRUE),
    v4 = sample(c("α", "β"), 50, replace = TRUE),
    v5 = sample(c("I", "II"), 50, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(nrow(result$cluster_assignments), 50)
})

test_that("Test 10: Dataset avec n=55, p=8", {
  set.seed(132)
  data <- as.data.frame(matrix(
    sample(letters[1:4], 55*8, replace = TRUE),
    nrow = 55, ncol = 8
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(result$cluster_assignments$cluster %in% 1:3))
})

test_that("Test 11: Dataset avec n=60, p=4, cardinalités variées", {
  set.seed(133)
  data <- data.frame(
    v1 = sample(letters[1:2], 60, replace = TRUE),
    v2 = sample(letters[1:5], 60, replace = TRUE),
    v3 = sample(letters[1:10], 60, replace = TRUE),
    v4 = sample(letters[1:3], 60, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(nrow(result$variable_clusters) > 0)
})

test_that("Test 12: Dataset avec n=65, p=6, clusters bien séparés", {
  set.seed(134)
  cluster_size <- 65 %/% 3
  data <- data.frame(
    v1 = c(rep("A", cluster_size), rep("B", cluster_size), rep("C", 65 - 2*cluster_size)),
    v2 = c(rep("X", cluster_size), rep("Y", cluster_size), rep("Z", 65 - 2*cluster_size)),
    v3 = sample(letters[1:3], 65, replace = TRUE),
    v4 = sample(letters[4:6], 65, replace = TRUE),
    v5 = sample(letters[7:9], 65, replace = TRUE),
    v6 = sample(letters[10:12], 65, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 65)
})

test_that("Test 13: Dataset avec n=70, p=9", {
  set.seed(135)
  data <- as.data.frame(matrix(
    sample(c("Cat1", "Cat2", "Cat3"), 70*9, replace = TRUE),
    nrow = 70, ncol = 9
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 4)
})

test_that("Test 14: Dataset avec n=75, p=5, haute cardinalité", {
  set.seed(136)
  data <- data.frame(
    v1 = sample(letters, 75, replace = TRUE),
    v2 = sample(letters, 75, replace = TRUE),
    v3 = sample(letters[1:10], 75, replace = TRUE),
    v4 = sample(letters[1:5], 75, replace = TRUE),
    v5 = sample(letters[1:3], 75, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(!is.null(result$cluster_centers))
})

test_that("Test 15: Dataset avec n=80, p=7", {
  set.seed(137)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:6], 80*7, replace = TRUE),
    nrow = 80, ncol = 7
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(nrow(result$cluster_assignments), 80)
})

test_that("Test 16: Dataset avec n=85, p=4, un seul cluster", {
  set.seed(138)
  data <- data.frame(
    v1 = rep("A", 85),
    v2 = rep("X", 85),
    v3 = sample(c("1", "2"), 85, replace = TRUE),
    v4 = sample(c("α", "β"), 85, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 1)
  expect_equal(length(unique(result$cluster_assignments$cluster)), 1)
})

test_that("Test 17: Dataset avec n=90, p=8", {
  set.seed(139)
  data <- as.data.frame(matrix(
    sample(letters[1:5], 90*8, replace = TRUE),
    nrow = 90, ncol = 8
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 18: Dataset avec n=95, p=6, cardinalités binaires", {
  set.seed(140)
  data <- as.data.frame(matrix(
    sample(c("Yes", "No"), 95*6, replace = TRUE),
    nrow = 95, ncol = 6
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(ncol(result$variable_clusters), 6)
})

test_that("Test 19: Dataset avec n=98, p=5", {
  set.seed(141)
  data <- data.frame(
    v1 = sample(c("Low", "Medium", "High"), 98, replace = TRUE),
    v2 = sample(c("Small", "Large"), 98, replace = TRUE),
    v3 = sample(c("Type1", "Type2", "Type3"), 98, replace = TRUE),
    v4 = sample(c("A", "B", "C", "D"), 98, replace = TRUE),
    v5 = sample(c("X", "Y"), 98, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 20: Dataset avec n=99, p=9, nombreux clusters", {
  set.seed(142)
  data <- as.data.frame(matrix(
    sample(letters[1:4], 99*9, replace = TRUE),
    nrow = 99, ncol = 9
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 6)
})

# =============================================================================
# SECTION 2: Tests sur datasets moyens (100 <= n < 500, 10 <= p < 30)
# Tests 21-40
# =============================================================================

test_that("Test 21: Dataset moyen (n=100, p=10)", {
  set.seed(201)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 100*10, replace = TRUE),
    nrow = 100, ncol = 10
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 100)
})

test_that("Test 22: Dataset avec n=150, p=12", {
  set.seed(202)
  data <- as.data.frame(matrix(
    sample(letters[1:6], 150*12, replace = TRUE),
    nrow = 150, ncol = 12
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 23: Dataset avec n=200, p=15", {
  set.seed(203)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C", "D"), 200*15, replace = TRUE),
    nrow = 200, ncol = 15
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(nrow(result$cluster_assignments), 200)
})

test_that("Test 24: Dataset avec n=250, p=10, haute cardinalité", {
  set.seed(204)
  data <- as.data.frame(matrix(
    sample(letters, 250*10, replace = TRUE),
    nrow = 250, ncol = 10
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 25: Dataset avec n=300, p=20", {
  set.seed(205)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:8], 300*20, replace = TRUE),
    nrow = 300, ncol = 20
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 26: Dataset avec n=350, p=25", {
  set.seed(206)
  data <- as.data.frame(matrix(
    sample(letters[1:5], 350*25, replace = TRUE),
    nrow = 350, ncol = 25
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(ncol(result$variable_clusters), 25)
})

test_that("Test 27: Dataset avec n=400, p=18, cardinalités variées", {
  set.seed(207)
  data <- as.data.frame(lapply(1:18, function(i) {
    sample(letters[1:sample(2:10, 1)], 400, replace = TRUE)
  }))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(nrow(result$cluster_assignments), 400)
})

test_that("Test 28: Dataset avec n=450, p=22", {
  set.seed(208)
  data <- as.data.frame(matrix(
    sample(c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5"), 450*22, replace = TRUE),
    nrow = 450, ncol = 22
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 7)
})

test_that("Test 29: Dataset avec n=120, p=11", {
  set.seed(209)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:4], 120*11, replace = TRUE),
    nrow = 120, ncol = 11
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(!is.null(result$cluster_centers))
})

test_that("Test 30: Dataset avec n=180, p=14", {
  set.seed(210)
  data <- as.data.frame(matrix(
    sample(letters[1:7], 180*14, replace = TRUE),
    nrow = 180, ncol = 14
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), 180)
})

test_that("Test 31: Dataset avec n=220, p=16, faible cardinalité", {
  set.seed(211)
  data <- as.data.frame(matrix(
    sample(c("Yes", "No"), 220*16, replace = TRUE),
    nrow = 220, ncol = 16
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(all(result$cluster_assignments$cluster %in% 1:2))
})

test_that("Test 32: Dataset avec n=280, p=19", {
  set.seed(212)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:6], 280*19, replace = TRUE),
    nrow = 280, ncol = 19
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(nrow(result$variable_clusters) > 0)
})

test_that("Test 33: Dataset avec n=320, p=23", {
  set.seed(213)
  data <- as.data.frame(matrix(
    sample(letters[1:8], 320*23, replace = TRUE),
    nrow = 320, ncol = 23
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_equal(nrow(result$cluster_assignments), 320)
})

test_that("Test 34: Dataset avec n=360, p=27", {
  set.seed(214)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C", "D", "E", "F"), 360*27, replace = TRUE),
    nrow = 360, ncol = 27
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 8)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 35: Dataset avec n=410, p=21", {
  set.seed(215)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 410*21, replace = TRUE),
    nrow = 410, ncol = 21
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 36: Dataset avec n=460, p=24", {
  set.seed(216)
  data <- as.data.frame(matrix(
    sample(letters[1:4], 460*24, replace = TRUE),
    nrow = 460, ncol = 24
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_equal(ncol(result$variable_clusters), 24)
})

test_that("Test 37: Dataset avec n=130, p=13, clusters bien séparés", {
  set.seed(217)
  cluster_size <- 130 %/% 4
  cluster_labels <- rep(1:4, each = cluster_size)[1:130]
  data <- as.data.frame(lapply(1:13, function(i) {
    sapply(cluster_labels, function(c) paste0("Cat", c, "_", sample(1:3, 1)))
  }))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), 130)
})

test_that("Test 38: Dataset avec n=270, p=17", {
  set.seed(218)
  data <- as.data.frame(matrix(
    sample(c("Low", "Medium", "High", "Very High"), 270*17, replace = TRUE),
    nrow = 270, ncol = 17
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 5)
})

test_that("Test 39: Dataset avec n=390, p=26", {
  set.seed(219)
  data <- as.data.frame(matrix(
    sample(letters[1:9], 390*26, replace = TRUE),
    nrow = 390, ncol = 26
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 40: Dataset avec n=480, p=29", {
  set.seed(220)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:7], 480*29, replace = TRUE),
    nrow = 480, ncol = 29
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 8)
  expect_equal(nrow(result$cluster_assignments), 480)
})

# =============================================================================
# SECTION 3: Tests sur grands datasets (n >= 500, p >= 30)
# Tests 41-60
# =============================================================================

test_that("Test 41: Grand dataset (n=500, p=30)", {
  set.seed(301)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 500*30, replace = TRUE),
    nrow = 500, ncol = 30
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(nrow(result$cluster_assignments), 500)
})

test_that("Test 42: Dataset avec n=600, p=35", {
  set.seed(302)
  data <- as.data.frame(matrix(
    sample(letters[1:6], 600*35, replace = TRUE),
    nrow = 600, ncol = 35
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_true(!is.null(result$cluster_centers))
})

test_that("Test 43: Dataset avec n=700, p=40", {
  set.seed(303)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C", "D", "E"), 700*40, replace = TRUE),
    nrow = 700, ncol = 40
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_equal(ncol(result$variable_clusters), 40)
})

test_that("Test 44: Dataset avec n=800, p=32", {
  set.seed(304)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:8], 800*32, replace = TRUE),
    nrow = 800, ncol = 32
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 8)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 45: Dataset avec n=900, p=45", {
  set.seed(305)
  data <- as.data.frame(matrix(
    sample(letters[1:4], 900*45, replace = TRUE),
    nrow = 900, ncol = 45
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 9)
  expect_equal(nrow(result$cluster_assignments), 900)
})

test_that("Test 46: Dataset avec n=1000, p=50", {
  set.seed(306)
  data <- as.data.frame(matrix(
    sample(c("Cat1", "Cat2", "Cat3", "Cat4"), 1000*50, replace = TRUE),
    nrow = 1000, ncol = 50
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 10)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 47: Dataset avec n=550, p=38", {
  set.seed(307)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:6], 550*38, replace = TRUE),
    nrow = 550, ncol = 38
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 5)
})

test_that("Test 48: Dataset avec n=650, p=42", {
  set.seed(308)
  data <- as.data.frame(matrix(
    sample(letters[1:7], 650*42, replace = TRUE),
    nrow = 650, ncol = 42
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_equal(nrow(result$cluster_assignments), 650)
})

test_that("Test 49: Dataset avec n=750, p=36", {
  set.seed(309)
  data <- as.data.frame(matrix(
    sample(c("Yes", "No", "Maybe"), 750*36, replace = TRUE),
    nrow = 750, ncol = 36
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 50: Dataset avec n=850, p=48", {
  set.seed(310)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 850*48, replace = TRUE),
    nrow = 850, ncol = 48
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 8)
  expect_equal(ncol(result$variable_clusters), 48)
})

test_that("Test 51: Dataset avec n=950, p=33", {
  set.seed(311)
  data <- as.data.frame(matrix(
    sample(letters[1:9], 950*33, replace = TRUE),
    nrow = 950, ncol = 33
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 9)
  expect_true(all(result$cluster_assignments$cluster %in% 1:9))
})

test_that("Test 52: Dataset avec n=1100, p=55", {
  set.seed(312)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C", "D", "E", "F"), 1100*55, replace = TRUE),
    nrow = 1100, ncol = 55
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 10)
  expect_equal(nrow(result$cluster_assignments), 1100)
})

test_that("Test 53: Dataset avec n=520, p=31, haute cardinalité", {
  set.seed(313)
  data <- as.data.frame(matrix(
    sample(letters, 520*31, replace = TRUE),
    nrow = 520, ncol = 31
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 54: Dataset avec n=680, p=39", {
  set.seed(314)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:10], 680*39, replace = TRUE),
    nrow = 680, ncol = 39
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 6)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 55: Dataset avec n=780, p=44", {
  set.seed(315)
  data <- as.data.frame(matrix(
    sample(letters[1:5], 780*44, replace = TRUE),
    nrow = 780, ncol = 44
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_equal(nrow(result$cluster_assignments), 780)
})

test_that("Test 56: Dataset avec n=880, p=37", {
  set.seed(316)
  data <- as.data.frame(matrix(
    sample(c("Low", "Medium", "High"), 880*37, replace = TRUE),
    nrow = 880, ncol = 37
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 8)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 57: Dataset avec n=980, p=46", {
  set.seed(317)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:7], 980*46, replace = TRUE),
    nrow = 980, ncol = 46
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 9)
  expect_equal(ncol(result$variable_clusters), 46)
})

test_that("Test 58: Dataset avec n=1200, p=60", {
  set.seed(318)
  data <- as.data.frame(matrix(
    sample(letters[1:6], 1200*60, replace = TRUE),
    nrow = 1200, ncol = 60
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 10)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 10)
})

test_that("Test 59: Dataset avec n=1500, p=70, nombreux clusters", {
  set.seed(319)
  data <- as.data.frame(matrix(
    sample(c("A", "B", "C", "D"), 1500*70, replace = TRUE),
    nrow = 1500, ncol = 70
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 12)
  expect_equal(nrow(result$cluster_assignments), 1500)
})

test_that("Test 60: Dataset avec n=2000, p=80", {
  set.seed(320)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:8], 2000*80, replace = TRUE),
    nrow = 2000, ncol = 80
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 15)
  expect_true(nrow(result$cluster_centers) > 0)
})

# =============================================================================
# SECTION 4: Tests de cas limites et edge cases
# Tests 61-80
# =============================================================================

test_that("Test 61: Dataset avec une seule observation (n=1)", {
  set.seed(401)
  data <- data.frame(
    v1 = "A",
    v2 = "B",
    v3 = "C"
  )
  
  expect_error(KmodesVarClust$new(data, n_clusters = 1), NA)
})

test_that("Test 62: Dataset avec toutes valeurs identiques", {
  set.seed(402)
  data <- data.frame(
    v1 = rep("A", 100),
    v2 = rep("B", 100),
    v3 = rep("C", 100)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 1)
  expect_equal(length(unique(result$cluster_assignments$cluster)), 1)
})

test_that("Test 63: Dataset avec une variable constante", {
  set.seed(403)
  data <- data.frame(
    v1 = rep("A", 100),
    v2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    v3 = sample(c("1", "2"), 100, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(nrow(result$cluster_assignments), 100)
})

test_that("Test 64: Dataset avec nombre de clusters égal au nombre d'observations", {
  set.seed(404)
  data <- data.frame(
    v1 = letters[1:10],
    v2 = letters[11:20],
    v3 = letters[21:30][1:10]
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 10)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 10)
})

test_that("Test 65: Dataset avec cardinalité maximale (toutes valeurs uniques)", {
  set.seed(405)
  data <- data.frame(
    v1 = paste0("Val", 1:50),
    v2 = paste0("Cat", 1:50),
    v3 = paste0("Item", 1:50)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 50)
})

test_that("Test 66: Dataset très déséquilibré (cardinalités 2 à 100)", {
  set.seed(406)
  data <- data.frame(
    v1 = sample(c("A", "B"), 200, replace = TRUE),
    v2 = sample(letters, 200, replace = TRUE),
    v3 = sample(c("Yes", "No"), 200, replace = TRUE),
    v4 = sample(LETTERS, 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 67: Dataset avec noms de variables complexes", {
  set.seed(407)
  data <- data.frame(
    `Variable-1` = sample(c("A", "B"), 100, replace = TRUE),
    `Var.2` = sample(c("X", "Y"), 100, replace = TRUE),
    `var_3` = sample(c("1", "2"), 100, replace = TRUE),
    check.names = FALSE
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(ncol(result$variable_clusters), 3)
})

test_that("Test 68: Dataset avec valeurs NA simulées comme catégorie", {
  set.seed(408)
  data <- data.frame(
    v1 = sample(c("A", "B", "NA_value"), 100, replace = TRUE),
    v2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    v3 = sample(c("1", "2", "missing"), 100, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 100)
})

test_that("Test 69: Dataset avec caractères spéciaux", {
  set.seed(409)
  data <- data.frame(
    v1 = sample(c("α", "β", "γ"), 100, replace = TRUE),
    v2 = sample(c("é", "è", "ê"), 100, replace = TRUE),
    v3 = sample(c("¿", "¡", "§"), 100, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 70: Dataset avec valeurs numériques comme caractères", {
  set.seed(410)
  data <- data.frame(
    v1 = sample(as.character(1:5), 100, replace = TRUE),
    v2 = sample(as.character(10:15), 100, replace = TRUE),
    v3 = sample(as.character(100:102), 100, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 100)
})

test_that("Test 71: Dataset parfaitement équilibré", {
  set.seed(411)
  data <- data.frame(
    v1 = rep(c("A", "B", "C"), each = 100),
    v2 = rep(c("X", "Y", "Z"), each = 100),
    v3 = rep(c("1", "2", "3"), each = 100)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 72: Dataset avec beaucoup de variables (p=100)", {
  set.seed(412)
  data <- as.data.frame(matrix(
    sample(letters[1:5], 200*100, replace = TRUE),
    nrow = 200, ncol = 100
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(ncol(result$variable_clusters), 100)
})

test_that("Test 73: Dataset très sparse (peu de modalités fréquentes)", {
  set.seed(413)
  data <- as.data.frame(lapply(1:10, function(i) {
    probs <- c(0.01, rep(0.99/25, 25))
    sample(letters[1:26], 500, replace = TRUE, prob = probs)
  }))
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), 500)
})

test_that("Test 74: Dataset avec distribution uniforme parfaite", {
  set.seed(414)
  data <- data.frame(
    v1 = rep(c("A", "B", "C", "D"), each = 100),
    v2 = rep(c("W", "X", "Y", "Z"), each = 100),
    v3 = rep(c("1", "2", "3", "4"), each = 100)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 4)
})

test_that("Test 75: Dataset avec exactement 2 clusters bien séparés", {
  set.seed(415)
  data <- data.frame(
    v1 = c(rep("A", 100), rep("B", 100)),
    v2 = c(rep("X", 100), rep("Y", 100)),
    v3 = c(rep("1", 100), rep("2", 100)),
    v4 = c(rep("α", 100), rep("β", 100))
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(length(unique(result$cluster_assignments$cluster)), 2)
})

test_that("Test 76: Dataset avec longues chaînes de caractères", {
  set.seed(416)
  long_cats <- paste0("Category_", sprintf("%05d", 1:10))
  data <- as.data.frame(lapply(1:5, function(i) {
    sample(long_cats, 100, replace = TRUE)
  }))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 100)
})

test_that("Test 77: Dataset avec espaces dans les valeurs", {
  set.seed(417)
  data <- data.frame(
    v1 = sample(c("Type A", "Type B", "Type C"), 100, replace = TRUE),
    v2 = sample(c("Class 1", "Class 2"), 100, replace = TRUE),
    v3 = sample(c("Cat X", "Cat Y", "Cat Z"), 100, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(!is.null(result$cluster_centers))
})

test_that("Test 78: Dataset rectangulaire extrême (n << p)", {
  set.seed(418)
  data <- as.data.frame(matrix(
    sample(letters[1:3], 20*50, replace = TRUE),
    nrow = 20, ncol = 50
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(ncol(result$variable_clusters), 50)
})

test_that("Test 79: Dataset rectangulaire extrême (n >> p)", {
  set.seed(419)
  data <- data.frame(
    v1 = sample(letters[1:5], 1000, replace = TRUE),
    v2 = sample(letters[6:10], 1000, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), 1000)
})

test_that("Test 80: Dataset avec pattern cyclique", {
  set.seed(420)
  pattern <- rep(c("A", "B", "C"), length.out = 300)
  data <- data.frame(
    v1 = pattern,
    v2 = sample(c("X", "Y"), 300, replace = TRUE),
    v3 = rev(pattern)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

# =============================================================================
# SECTION 5: Tests de robustesse et performance
# Tests 81-100
# =============================================================================

test_that("Test 81: Dataset avec seed fixé - reproductibilité", {
  set.seed(501)
  data <- data.frame(
    v1 = sample(letters[1:5], 100, replace = TRUE),
    v2 = sample(letters[6:10], 100, replace = TRUE)
  )
  
  set.seed(1000)
  result1 <- KmodesVarClust$new(data, n_clusters = 3)
  set.seed(1000)
  result2 <- KmodesVarClust$new(data, n_clusters = 3)
  
  expect_equal(result1$cluster_assignments$cluster, result2$cluster_assignments$cluster)
})

test_that("Test 82: Dataset avec différentes initialisations", {
  set.seed(502)
  data <- as.data.frame(matrix(
    sample(LETTERS[1:5], 200*10, replace = TRUE),
    nrow = 200, ncol = 10
  ))
  
  results <- lapply(1:3, function(i) {
    set.seed(500 + i)
    KmodesVarClust$new(data, n_clusters = 4)
  })
  
  expect_true(all(sapply(results, function(r) nrow(r$cluster_assignments) == 200)))
})

test_that("Test 83: Dataset mixte avec cardinalités très variées", {
  set.seed(503)
  data <- data.frame(
    v1 = sample(c("A", "B"), 300, replace = TRUE),
    v2 = sample(letters[1:5], 300, replace = TRUE),
    v3 = sample(letters[1:10], 300, replace = TRUE),
    v4 = sample(letters, 300, replace = TRUE),
    v5 = paste0("Cat", 1:300)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_equal(nrow(result$cluster_assignments), 300)
})

test_that("Test 84: Dataset avec clusters imbriqués", {
  set.seed(504)
  data <- data.frame(
    v1 = c(rep("A", 150), rep("B", 150)),
    v2 = rep(c("X", "Y"), each = 75, times = 2),
    v3 = rep(c("1", "2"), each = 37, length.out = 300),
    v4 = sample(letters[1:4], 300, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 85: Dataset avec bruit ajouté", {
  set.seed(505)
  signal <- c(rep("A", 100), rep("B", 100))
  noise <- sample(c("A", "B", "C", "D"), 200, replace = TRUE)
  data <- data.frame(
    v1 = signal,
    v2 = sample(c("X", "Y"), 200, replace = TRUE),
    v3 = noise
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(nrow(result$cluster_assignments), 200)
})

test_that("Test 86: Dataset avec structure hiérarchique", {
  set.seed(506)
  data <- data.frame(
    level1 = rep(c("Cat1", "Cat2"), each = 200),
    level2 = rep(c("Sub1", "Sub2", "Sub3", "Sub4"), each = 100),
    level3 = rep(letters[1:8], each = 50),
    attr1 = sample(c("A", "B", "C"), 400, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 87: Dataset temporel simulé", {
  set.seed(507)
  n <- 365
  data <- data.frame(
    season = rep(c("Winter", "Spring", "Summer", "Fall"), each = n/4, length.out = n),
    month = rep(month.abb, length.out = n),
    day_type = sample(c("Weekday", "Weekend"), n, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), n)
})

test_that("Test 88: Dataset géographique simulé", {
  set.seed(508)
  data <- data.frame(
    continent = sample(c("Africa", "Asia", "Europe", "Americas", "Oceania"), 300, replace = TRUE),
    climate = sample(c("Tropical", "Temperate", "Arctic", "Desert"), 300, replace = TRUE),
    development = sample(c("Developed", "Developing", "Emerging"), 300, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 89: Dataset avec variables corrélées", {
  set.seed(509)
  base <- sample(c("A", "B", "C"), 200, replace = TRUE)
  data <- data.frame(
    v1 = base,
    v2 = ifelse(base == "A", "X", ifelse(base == "B", "Y", "Z")),
    v3 = sample(c("1", "2"), 200, replace = TRUE),
    v4 = base
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 200)
})

test_that("Test 90: Dataset avec variables indépendantes", {
  set.seed(510)
  data <- data.frame(
    v1 = sample(letters[1:5], 200, replace = TRUE),
    v2 = sample(letters[6:10], 200, replace = TRUE),
    v3 = sample(letters[11:15], 200, replace = TRUE),
    v4 = sample(letters[16:20], 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 91: Dataset avec distribution de Zipf", {
  set.seed(511)
  zipf_sample <- function(n, k) {
    probs <- 1/(1:k)
    probs <- probs/sum(probs)
    sample(letters[1:k], n, replace = TRUE, prob = probs)
  }
  
  data <- data.frame(
    v1 = zipf_sample(300, 10),
    v2 = zipf_sample(300, 10),
    v3 = zipf_sample(300, 10)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), 300)
})

test_that("Test 92: Dataset avec outliers catégoriels", {
  set.seed(512)
  data <- data.frame(
    v1 = c(rep("A", 95), rep("B", 95), paste0("Rare", 1:10)),
    v2 = c(rep("X", 95), rep("Y", 95), paste0("Unusual", 1:10)),
    v3 = sample(c("1", "2"), 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 93: Dataset avec nombre optimal de clusters ambigu", {
  set.seed(513)
  data <- as.data.frame(matrix(
    sample(letters[1:6], 300*8, replace = TRUE),
    nrow = 300, ncol = 8
  ))
  
  result <- KmodesVarClust$new(data, n_clusters = 5)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 5)
})

test_that("Test 94: Dataset avec structure en blocs", {
  set.seed(514)
  block_size <- 50
  data <- data.frame(
    v1 = rep(c("A", "B", "C", "D"), each = block_size),
    v2 = rep(c("W", "X", "Y", "Z"), each = block_size),
    v3 = rep(c("1", "2", "3", "4"), each = block_size),
    v4 = sample(letters[1:5], 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 4)
  expect_equal(nrow(result$cluster_assignments), 200)
})

test_that("Test 95: Dataset avec multimodalité", {
  set.seed(515)
  data <- data.frame(
    v1 = c(rep("A", 60), rep("B", 60), rep("C", 60), sample(c("A","B","C"), 20, replace=TRUE)),
    v2 = c(rep("X", 60), rep("Y", 60), rep("Z", 60), sample(c("X","Y","Z"), 20, replace=TRUE)),
    v3 = sample(letters[1:4], 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(nrow(result$cluster_centers) > 0)
})

test_that("Test 96: Dataset avec gradient de séparation", {
  set.seed(516)
  data <- data.frame(
    v1 = c(rep("A", 100), sample(c("A", "B"), 50, replace = TRUE), rep("B", 50)),
    v2 = c(rep("X", 100), sample(c("X", "Y"), 50, replace = TRUE), rep("Y", 50)),
    v3 = sample(letters[1:3], 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_equal(nrow(result$cluster_assignments), 200)
})

test_that("Test 97: Dataset avec clusters de tailles très différentes", {
  set.seed(517)
  data <- data.frame(
    v1 = c(rep("A", 150), rep("B", 30), rep("C", 20)),
    v2 = c(rep("X", 150), rep("Y", 30), rep("Z", 20)),
    v3 = sample(letters[1:4], 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_true(all(!is.na(result$cluster_assignments$cluster)))
})

test_that("Test 98: Dataset avec pattern en damier", {
  set.seed(518)
  pattern <- rep(c("A", "B"), length.out = 200)
  data <- data.frame(
    v1 = pattern,
    v2 = rev(pattern),
    v3 = sample(c("X", "Y"), 200, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 2)
  expect_true(!is.null(result$variable_clusters))
})

test_that("Test 99: Dataset avec transition progressive", {
  set.seed(519)
  n <- 300
  transition <- seq(0, 1, length.out = n)
  data <- data.frame(
    v1 = ifelse(transition < 0.5, "A", "B"),
    v2 = ifelse(transition < 0.33, "X", ifelse(transition < 0.67, "Y", "Z")),
    v3 = sample(letters[1:3], n, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 3)
  expect_equal(nrow(result$cluster_assignments), n)
})

test_that("Test 100: Dataset complexe multi-dimensionnel", {
  set.seed(520)
  n <- 500
  data <- data.frame(
    cat_high = sample(letters, n, replace = TRUE),
    cat_medium = sample(LETTERS[1:10], n, replace = TRUE),
    cat_low = sample(c("A", "B", "C"), n, replace = TRUE),
    bin1 = sample(c("Yes", "No"), n, replace = TRUE),
    bin2 = sample(c("True", "False"), n, replace = TRUE),
    ord1 = sample(c("Low", "Medium", "High"), n, replace = TRUE),
    ord2 = sample(c("Small", "Large"), n, replace = TRUE),
    nom1 = sample(paste0("Type", 1:15), n, replace = TRUE),
    nom2 = sample(paste0("Cat", 1:8), n, replace = TRUE),
    nom3 = sample(paste0("Class", 1:5), n, replace = TRUE)
  )
  
  result <- KmodesVarClust$new(data, n_clusters = 7)
  expect_equal(nrow(result$cluster_assignments), n)
  expect_equal(ncol(result$variable_clusters), 10)
  expect_true(length(unique(result$cluster_assignments$cluster)) <= 7)
  expect_true(nrow(result$cluster_centers) > 0)
})

cat("\n=== SUITE DE 100 TESTS TERMINÉE ===\n")
cat("Tests couvrant:\n")
cat("- Tests 1-20: Petits datasets (n < 100, p < 10)\n")
cat("- Tests 21-40: Datasets moyens (100 ≤ n < 500, 10 ≤ p < 30)\n")
cat("- Tests 41-60: Grands datasets (n ≥ 500, p ≥ 30)\n")
cat("- Tests 61-80: Cas limites et edge cases\n")
cat("- Tests 81-100: Tests de robustesse et performance\n")
