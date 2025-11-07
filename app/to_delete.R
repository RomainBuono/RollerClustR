# ==========================================
# SCRIPT DE TEST (CORRIGÉ)
# Package de clustering R6
# ==========================================
library(R6)

# --- 1. CHARGEMENT DES CLASSES ---
# Assurez-vous que les fichiers sont dans le bon ordre de dépendance
# (Parent -> Enfants -> Wrappers -> Fonctions)

source_files <- c(
  "ClusterAnalysis_parentclass.R",
  "CAH_class.R",
  "kmeans_class.R",
  "Kprototypes_class.R",
  "wrapper.R",
  "user_functions.R"
)

tryCatch({
  for (f in source_files) {
    cat("Loading:", f, "\n")
    source(f, encoding = "UTF-8")
  }
  cat("\n✓ Tous les fichiers ont été chargés avec succès.\n")
}, error = function(e) {
  stop("Erreur lors du chargement des fichiers : ", e$message)
})


# --- 2. CRÉATION DES JEUX DE DONNÉES ---

# 2.1. Données Numériques (iris)
data_num <- iris[1:4]
data_num_illus <- iris[5] # Species

# 2.2. Données Mixtes (simple)
data_mixte <- data.frame(
  Age = c(25, 45, 30, 60, 55, 28),
  Score = c(100.5, 200.2, 150.0, 500.9, 450.1, 120.0),
  Sexe = factor(c("H", "F", "H", "F", "F", "H")),
  CSP = factor(c("A", "B", "A", "C", "C", "B"))
)

# 2.3. Données Catégorielles (simple)
data_cat <- data_mixte[, c("Sexe", "CSP")]

# 2.4. Données avec 'character' (pour tester les bugs)
data_char <- data_mixte
data_char$Sexe <- as.character(data_char$Sexe)


cat("\n--- Prêt à tester. 3 jeux de données créés. ---\n\n")

# --- 3. TEST DES CLASSES INDIVIDUELLES ---

# 3.1. Test CAH (Numérique)
cat("=== 3.1. Test CAH ===\n")
tryCatch({
  cah_obj <- CAH$new(k = 3, cr = TRUE)
  print(cah_obj) # Doit afficher "Non ajusté"
  cah_obj$fit(data_num)
  cah_obj$print() # Doit afficher les infos
  cah_obj$summary()
  
  # Test du plot (va s'ouvrir)
  # cah_obj$plot(showGroups = TRUE) 
  
  cat("Groupes initiaux (k=3) :", head(cah_obj$Groupes), "...\n")
  
  # Test binding actif
  cah_obj$NbGroupes <- 4
  cat("Groupes modifiés (k=4) :", head(cah_obj$Groupes), "...\n")
  
  # Test analyse illustrative
  print(cah_obj$predict(data_num_illus))
  
  cat("✓ Test CAH réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR CAH :", e$message, "\n\n") })

# 3.2. Test Kmeans (Numérique)
cat("=== 3.2. Test Kmeans ===\n")
tryCatch({
  km_obj <- Kmeans$new(k = 3, nstart = 25)
  km_obj$fit(data_num)
  km_obj$print()
  km_obj$summary()
  print(km_obj$inertie())
  
  # Test du plot
  # km_obj$plot()
  
  cat("Groupes initiaux (k=3) :", head(km_obj$Groupes), "...\n")
  
  # Test binding actif
  km_obj$NbGroupes <- 2
  cat("Groupes modifiés (k=2) :", head(km_obj$Groupes), "...\n")
  
  # Test analyse illustrative
  print(km_obj$predict(data_num_illus))
  
  cat("✓ Test Kmeans réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR Kmeans :", e$message, "\n\n") })

# 3.3. Test Kprototypes (Mixte)
cat("=== 3.3. Test Kprototypes (Mixte) ===\n")
tryCatch({
  kproto_obj <- Kprototypes$new(k = 2, lambda = 0.5)
  kproto_obj$fit(data_mixte)
  kproto_obj$print()
  kproto_obj$summary()
  kproto_obj$prototypes()
  
  # Test binding actif (doit juste afficher un message)
  kproto_obj$NbGroupes <- 3
  
  # Test analyse illustrative mixte
  print(kproto_obj$predict(data.frame(IllusNum = c(1,2,3,4,5,6), IllusCat = factor(c("A","B","A","B","A","B")))))
  
  cat("✓ Test Kprototypes (Mixte) réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR Kprototypes (Mixte) :", e$message, "\n\n") })

# 3.4. Test Kprototypes (Numérique seul -> mode K-means)
cat("=== 3.4. Test Kprototypes (Mode K-means) ===\n")
tryCatch({
  kproto_km_mode <- Kprototypes$new(k = 3)
  kproto_km_mode$fit(data_num) # Doit fonctionner
  kproto_km_mode$summary()
  kproto_km_mode$prototypes() # Doit montrer que des variables numériques
  
  cat("✓ Test Kprototypes (Mode K-means) réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR Kprototypes (Mode K-means) :", e$message, "\n\n") })

# 3.5. Test Kprototypes (Catégoriel seul -> mode K-modes)
cat("=== 3.5. Test Kprototypes (Mode K-modes) ===\n")
tryCatch({
  kproto_kmodes_mode <- Kprototypes$new(k = 2)
  kproto_kmodes_mode$fit(data_cat) # Doit fonctionner
  kproto_kmodes_mode$summary()
  kproto_kmodes_mode$prototypes() # Doit montrer que des variables catégorielles
  
  cat("✓ Test Kprototypes (Mode K-modes) réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR Kprototypes (Mode K-modes) :", e$message, "\n\n") })


# --- 4. TEST DES FONCTIONS UTILISATEUR ET WRAPPERS ---

cat("=== 4.1. Test faire_clustering (auto) ===\n")
tryCatch({
  # Doit choisir CAH par défaut
  res_auto_cah <- faire_clustering(data_num, k = 3, method = "auto") 
  print(res_auto_cah)
  
  # Doit choisir Kmeans
  res_auto_km <- faire_clustering(data_num, k = 3, method = "kmeans") 
  print(res_auto_km)
  
  # Doit choisir Kprototypes
  res_auto_kproto <- faire_clustering(data_mixte, k = 2, method = "auto") 
  print(res_auto_kproto)
  
  cat("✓ Test faire_clustering réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR faire_clustering :", e$message, "\n\n") })


cat("=== 4.2. Test trouver_k_optimal (Kmeans) ===\n")
tryCatch({
  # Test de l'évaluateur
  resultats_k <- trouver_k_optimal(data_num, k_min = 2, k_max = 5, 
                                   method = "kmeans", afficher_graphique = FALSE)
  print(resultats_k)
  
  cat("✓ Test trouver_k_optimal réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR trouver_k_optimal :", e$message, "\n\n") })


cat("=== 4.3. Test comparer_algorithmes ===\n")
tryCatch({
  # Test du comparateur
  comparaison <- comparer_algorithmes(data_num, k = 3, 
                                      algorithmes = c("cah", "kmeans"), 
                                      afficher_graphique = FALSE)
  print(comparaison$confusion)
  
  cat("✓ Test comparer_algorithmes réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR comparer_algorithmes :", e$message, "\n\n") })


cat("=== 4.4. Test clustering_complet (WORKFLOW FINAL) ===\n")
tryCatch({
  workflow_result <- clustering_complet(
    data_num, 
    variables_illustratives = data_num_illus,
    k_min = 2, 
    k_max = 5,
    method = "kmeans",
    fichier_resultats = NULL,
    fichier_rapport = NULL
  )
  
  cat("\n--- Résultat du workflow : ---\n")
  print(names(workflow_result))
  print(workflow_result$objet)
  
  cat("✓ Test clustering_complet réussi.\n\n")
}, error = function(e) { cat("✗ ERREUR clustering_complet :", e$message, "\n\n") })


# --- 5. TEST DES CAS LIMITES ET BUGS ---

cat("=== 5.1. Test Kprototypes avec données 'character' (Doit échouer) ===\n")
tryCatch({
  res_char <- Kprototypes$new(k = 2)$fit(data_char)
  cat("✗ TEST ÉCHOUÉ : Kprototypes n'a pas planté avec des 'character'.\n\n")
}, error = function(e) { 
  cat("✓ TEST RÉUSSI : Kprototypes a bien planté :\n", e$message, "\n\n") 
})

cat("=== 5.2. Test 'faire_clustering' avec données 'character' (Doit échouer) ===\n")
tryCatch({
  res_char_user <- faire_clustering(data_char, k = 2)
  cat("✗ TEST ÉCHOUÉ : faire_clustering n'a pas planté avec des 'character'.\n\n")
}, error = function(e) { 
  cat("✓ TEST RÉUSSI : faire_clustering a bien planté :\n", e$message, "\n\n") 
})

cat("=== 5.3. Test CAH avec données mixtes (Doit échouer) ===\n")
tryCatch({
  res_cah_mixte <- CAH$new(k = 2)$fit(data_mixte)
  cat("✗ TEST ÉCHOUÉ : CAH n'a pas planté avec des données mixtes.\n\n")
}, error = function(e) { 
  cat("✓ TEST RÉUSSI : CAH a bien planté :\n", e$message, "\n\n") 
})


cat("--- FIN DES TESTS ---\n")