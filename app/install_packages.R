#!/usr/bin/env Rscript

# ==============================================================================
# SCRIPT D'INSTALLATION DES PACKAGES REQUIS
# Application Shiny - Clustering de Variables
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  INSTALLATION DES PACKAGES POUR L'APPLICATION SHINY\n")
cat("  Clustering de Variables (VAR_CAH, VAR_KMEANS, TandemVarClust)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Liste des packages requis
required_packages <- c(
  # Shiny core
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  
  # Visualisation
  "plotly",
  "DT",
  "ggplot2",
  
  # R6 et clustering
  "R6",
  "cluster",
  "factoextra",
  
  # Import/Export
  "readxl",
  "writexl",
  
  # Autres
  "gridExtra"
)

cat("Packages requis :\n")
cat(paste("  -", required_packages, collapse = "\n"))
cat("\n\n")

# Vérifier les packages installés
installed <- installed.packages()[, "Package"]
missing <- required_packages[!(required_packages %in% installed)]

if (length(missing) == 0) {
  cat("✅ Tous les packages sont déjà installés !\n\n")
  
  # Afficher les versions
  cat("Versions installées :\n")
  for (pkg in required_packages) {
    version <- packageVersion(pkg)
    cat(sprintf("  %-20s : %s\n", pkg, version))
  }
  
} else {
  cat("⚠️  Packages manquants :\n")
  cat(paste("  -", missing, collapse = "\n"))
  cat("\n\n")
  
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  INSTALLATION EN COURS...\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  # Installer les packages manquants
  for (pkg in missing) {
    cat(paste0("Installation de ", pkg, "...\n"))
    
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      cat(paste0("  ✅ ", pkg, " installé avec succès\n\n"))
    }, error = function(e) {
      cat(paste0("  ❌ Erreur lors de l'installation de ", pkg, "\n"))
      cat(paste0("     ", e$message, "\n\n"))
    })
  }
  
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  VÉRIFICATION POST-INSTALLATION\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  # Re-vérifier
  installed_after <- installed.packages()[, "Package"]
  still_missing <- required_packages[!(required_packages %in% installed_after)]
  
  if (length(still_missing) == 0) {
    cat("✅ Tous les packages ont été installés avec succès !\n\n")
  } else {
    cat("⚠️  Les packages suivants n'ont pas pu être installés :\n")
    cat(paste("  -", still_missing, collapse = "\n"))
    cat("\n\n")
    cat("Veuillez les installer manuellement avec :\n")
    cat(paste0('install.packages(c("', paste(still_missing, collapse = '", "'), '"))\n\n'))
  }
}

# Test de chargement
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST DE CHARGEMENT DES PACKAGES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

success_count <- 0
fail_count <- 0

for (pkg in required_packages) {
  result <- tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("  ✅ %-20s chargé\n", pkg))
    success_count <- success_count + 1
    TRUE
  }, error = function(e) {
    cat(sprintf("  ❌ %-20s ERREUR : %s\n", pkg, e$message))
    fail_count <- fail_count + 1
    FALSE
  })
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  RÉSUMÉ\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("Packages testés      : %d\n", length(required_packages)))
cat(sprintf("Chargements réussis  : %d ✅\n", success_count))
cat(sprintf("Échecs               : %d ❌\n", fail_count))
cat("\n")

if (fail_count == 0) {
  cat("🎉 PARFAIT ! Tous les packages sont prêts.\n")
  cat("Vous pouvez maintenant lancer l'application avec :\n")
  cat("   shiny::runApp()\n\n")
} else {
  cat("⚠️  Certains packages ont échoué. Veuillez résoudre les problèmes avant de lancer l'application.\n\n")
}

cat("═══════════════════════════════════════════════════════════════════\n")

# Optionnel : Créer un rapport
write_report <- function() {
  report_file <- "installation_report.txt"
  
  sink(report_file)
  cat("RAPPORT D'INSTALLATION - Application Shiny Clustering\n")
  cat("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("Packages requis :\n")
  for (pkg in required_packages) {
    if (pkg %in% installed.packages()[, "Package"]) {
      version <- packageVersion(pkg)
      cat(sprintf("  [OK] %-20s : %s\n", pkg, version))
    } else {
      cat(sprintf("  [KO] %-20s : NON INSTALLÉ\n", pkg))
    }
  }
  
  cat("\n")
  cat("Session Info :\n")
  print(sessionInfo())
  
  sink()
  
  cat("\n📄 Rapport sauvegardé dans :", report_file, "\n")
}

# Décommenter pour générer un rapport
# write_report()