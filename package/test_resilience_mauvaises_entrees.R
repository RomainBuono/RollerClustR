# =======================================================
# SCRIPT DE TEST DE RÉSILIENCE ET DE GESTION D'ERREURS
# =======================================================

# --- 0. Chargement des packages et des classes ---
library(R6)
# Assurez-vous que toutes vos classes R6 sont chargées :
# source("ClusterAnalysis_parentclass.R")
# source("kmeans_class.R")
# source("CAH_class.R")
# source("Kprototypes_class.R")
# source("ClustOfVar_class.R")

# Données de référence
data_num <- iris[, 1:4]
data_mixte <- iris
data_mixte$Species <- as.character(data_mixte$Species) # Rendre 'Species' de type char (non-facteur) pour un test

# =======================================================
# Fonction de Test Générique
# =======================================================

tester_resilience <- function(classe_r6, test_data, test_nom, params = list()) {
  
  cat(paste("\n--- TESTER CLASSE :", deparse(substitute(classe_r6)), "-", test_nom, "---\n"))
  
  # Construire la commande pour instancier et ajuster l'objet
  args_new <- c(list(k = 3), params) # Ajouter k=3 par défaut
  args_fit <- list(X = test_data)
  
  # Instanciation
  instance <- tryCatch({
    do.call(classe_r6$new, args_new)
  }, error = function(e) {
    message(paste("  ✅ [PASS] Instanciation échouée (Attendu : STOP):", e$message))
    return(NULL) # L'erreur d'initialisation est parfois liée aux paramètres
  })
  
  if (is.null(instance)) return(invisible(NULL))
  
  # Tentative d'ajustement
  result <- tryCatch({
    do.call(instance$fit, args_fit)
    "SUCCESS" # Retourne une chaîne si réussi
  }, error = function(e) {
    message(paste("  ✅ [PASS] Échec de $fit() (Attendu : STOP):", e$message))
    "FAIL_EXPECTED"
  }, warning = function(w) {
    message(paste("  ⚠️ [PASS] $fit() a émis un AVERTISSEMENT :", w$message))
    "SUCCESS_WITH_WARNING"
  })
  
  if (result == "SUCCESS") {
    cat("  ❌ [FAIL] La méthode $fit() a réussi. Une erreur ou un avertissement était attendu.\n")
  }
}

# =======================================================
# --- 1. Tests de Type d'Objet (input X) ---
# =======================================================
cat("\n\n#######################################################\n")
cat("## 1. TESTS : TYPE D'OBJET ET STRUCTURE DU DATASET\n")
cat("#######################################################\n")

# 1.1. Simple vecteur (pas un data.frame ou matrice)
vecteur_simple <- 1:10
tester_resilience(Kmeans, vecteur_simple, "Input: Vecteur simple (doit STOP)")
tester_resilience(CAH_Kmeans, vecteur_simple, "Input: Vecteur simple (doit STOP)")
tester_resilience(Kprototypes, vecteur_simple, "Input: Vecteur simple (doit STOP)")

# 1.2. Data frame vide (0 ligne, 4 colonnes)
df_zero_ligne <- data_num[0, ]
tester_resilience(Kmeans, df_zero_ligne, "Input: Data frame vide (0 ligne, doit STOP)")

# 1.3. Data frame avec une seule colonne (doit potentiellement WARN ou FAIL selon l'algo)
df_une_col <- data_num[, 1, drop = FALSE]
tester_resilience(Kmeans, df_une_col, "Input: Data frame à 1 colonne")


# =======================================================
# --- 2. Tests de Type de Variable (Data Type) ---
# =======================================================
cat("\n\n#######################################################\n")
cat("## 2. TESTS : TYPAGE DES VARIABLES\n")
cat("#######################################################\n")

# 2.1. K-means et CAH : Données catégorielles (doit STOP ou WARN/FAIL si l'algo R de base est appelé)
data_cat_pure <- data.frame(
  Var1 = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  Var2 = factor(sample(c("X", "Y"), 10, replace = TRUE))
)
tester_resilience(Kmeans, data_cat_pure, "Kmeans: Input catégoriel pur (doit STOP)")
tester_resilience(CAH_Kmeans, data_cat_pure, "CAH_Kmeans: Input catégoriel pur (doit STOP)")

# 2.2. K-prototypes : Variables de type 'character' (doit être FACTOR)
data_char_col <- data_mixte
tester_resilience(Kprototypes, data_char_col, "Kprototypes: Input 'character' (doit STOP/WARN pour factor)")

# 2.3. K-prototypes : Variable non valide (ex: dates)
data_date_col <- data.frame(
  Num = 1:10,
  DateCol = seq(as.Date("2023-01-01"), by = "day", length.out = 10)
)
tester_resilience(Kprototypes, data_date_col, "Kprototypes: Variable de type Date (doit STOP)")


# =======================================================
# --- 3. Tests de Paramètres Invalides (dans $new()) ---
# =======================================================
cat("\n\n#######################################################\n")
cat("## 3. TESTS : PARAMÈTRES INVALIDES\n")
cat("#######################################################\n")

# 3.1. Nombre de groupes (k) invalide
tester_resilience(Kmeans, data_num, "Kmeans: k=0 (doit STOP)", params = list(k = 0))
tester_resilience(Kmeans, data_num, "Kmeans: k=NA (doit STOP)", params = list(k = NA))
tester_resilience(Kmeans, data_num, "Kmeans: k=Vector (doit STOP)", params = list(k = c(3, 4)))

# 3.2. nstart invalide
tester_resilience(Kmeans, data_num, "Kmeans: nstart=-1 (doit STOP)", params = list(nstart = -1))

# 3.3. lambda invalide (pour Kprototypes)
tester_resilience(Kprototypes, data_mixte, "Kprototypes: lambda=-1 (doit STOP)", params = list(lambda = -1))

# 3.4. method CAH invalide
tester_resilience(CAH_Kmeans, data_num, "CAH_Kmeans: method='invalide' (doit STOP)", params = list(method = "invalide"))


# =======================================================
# --- 4. Tests de Données Manquantes (NA) ---
# =======================================================
cat("\n\n#######################################################\n")
cat("## 4. TESTS : GESTION DES DONNÉES MANQUANTES (NA)\n")
cat("#######################################################\n")

# Créer un dataset avec NA
data_with_na <- data_num
data_with_na[1, 1] <- NA # NA en ligne 1
data_with_na[5, 4] <- NA # NA en ligne 5

# 4.1. Kmeans avec na_action="fail" (doit STOP)
tester_resilience(Kmeans, data_with_na, 'Kmeans: NA avec na_action="fail" (doit STOP)', params = list(na_action = "fail"))

# 4.2. Kprototypes avec na_action="fail" (doit STOP)
tester_resilience(Kprototypes, data_with_na, 'Kprototypes: NA avec na_action="fail" (doit STOP)', params = list(na_action = "fail"))

# 4.3. Kmeans avec na_action="warn" (doit WARN)
# Note : La classe parente doit retourner un avertissement mais continuer l'ajustement
tester_resilience(Kmeans, data_with_na, 'Kmeans: NA avec na_action="warn" (doit WARN)', params = list(na_action = "warn"))

# 4.4. CAH_Kmeans avec na_action="omit" (doit OMIT et WARN/INFO)
tester_resilience(CAH_Kmeans, data_with_na, 'CAH_Kmeans: NA avec na_action="omit" (doit WARN)', params = list(na_action = "omit"))