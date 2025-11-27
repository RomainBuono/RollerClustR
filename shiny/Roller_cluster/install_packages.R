# ==============================================================================
# INSTALLATION DES PACKAGES NÉCESSAIRES
# Exécuter ce script UNE FOIS avant de lancer l'application
# ==============================================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  Installation des packages pour RollerClustR\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Liste complète des packages
packages <- c(
  # ─────────────────────────────────────────────────────────
  # Packages SHINY (Interface)
  # ─────────────────────────────────────────────────────────
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  
  # ─────────────────────────────────────────────────────────
  # Packages VISUALISATION
  # ─────────────────────────────────────────────────────────
  "plotly",          # Graphiques interactifs
  "ggplot2",         # Graphiques statiques
  "corrplot",        # Matrices de corrélation
  "DT",              # Tables interactives
  "gridExtra",       # Arrangement de graphiques
  
  # ─────────────────────────────────────────────────────────
  # Packages CLUSTERING & ANALYSE
  # ─────────────────────────────────────────────────────────
  "cluster",         # Silhouette, PAM, etc.
  "factoextra",      # Extraction et visualisation de résultats
  "mclust",          # Adjusted Rand Index
  "R6",              # Programmation orientée objet
  
  # ─────────────────────────────────────────────────────────
  # Packages RÉDUCTION DE DIMENSION
  # ─────────────────────────────────────────────────────────
  "tsne",            # t-SNE
  "umap",            # UMAP
  
  # ─────────────────────────────────────────────────────────
  # Packages IMPORT/EXPORT
  # ─────────────────────────────────────────────────────────
  "readxl",          # Lire Excel
  "writexl",         # Écrire Excel
  "openxlsx"         # Alternative pour Excel
)

# Fonction pour installer un package s'il n'est pas déjà installé
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(" Installation de", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  } else {
    cat("✓", pkg, "déjà installé\n")
  }
}

# Installer tous les packages
cat("\n═══ Vérification et installation des packages ═══\n\n")

for (pkg in packages) {
  install_if_missing(pkg)
}

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ✓ Installation terminée !\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Vérification finale
cat("═══ Vérification finale ═══\n\n")

missing_packages <- c()

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat("✗", pkg, "NON INSTALLÉ\n")
  } else {
    cat("✓", pkg, "OK\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\n⚠️ ATTENTION : Les packages suivants n'ont pas pu être installés :\n")
  cat(paste(" -", missing_packages, collapse = "\n"), "\n\n")
  cat("Veuillez les installer manuellement avec :\n")
  cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n")
} else {
  cat("\n🎉 Tous les packages sont installés correctement !\n")
  cat("Vous pouvez maintenant lancer l'application avec :\n")
  cat("  shiny::runApp()\n\n")
}

# ==============================================================================
# STRUCTURE DES FICHIERS NÉCESSAIRES
# ==============================================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  Structure des fichiers attendue\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Votre dossier doit contenir :\n\n")

cat("📁 Dossier racine/\n")
cat("  ├── 📄 app.R                  (Lance l'application)\n")
cat("  ├── 📄 global.R               (Chargement packages & fonctions)\n")
cat("  ├── 📄 ui.R                   (Interface utilisateur)\n")
cat("  ├── 📄 server.R               (Logique serveur)\n")
cat("  ├── 📄 user_functions.R       (Fonctions utilitaires)\n")
cat("  ├── 📄 ClusterAnalysis.R      (Classe parente R6)\n")
cat("  ├── 📄 VAR_CAH.R              (Algorithme VAR_CAH)\n")
cat("  ├── 📄 VAR_KMEANS.R           (Algorithme VAR_KMEANS)\n")
cat("  └── 📄 TandemVarClust.R       (Algorithme TandemVarClust)\n\n")

cat("═══════════════════════════════════════════════════════════\n\n")

cat(" Notes importantes :\n\n")
cat("1. Les fichiers VARCLUS.R et KmodesVarClust.R ne sont PLUS nécessaires\n")
cat("   (retirés de l'application selon vos consignes)\n\n")
cat("2. Assurez-vous que tous les fichiers sont dans le MÊME dossier\n\n")
cat("3. Pour lancer l'application :\n")
cat("   - Ouvrir RStudio\n")
cat("   - Définir le working directory : setwd('chemin/vers/dossier')\n")
cat("   - Exécuter : shiny::runApp()\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("  Prêt à démarrer !\n")
cat("═══════════════════════════════════════════════════════════\n")