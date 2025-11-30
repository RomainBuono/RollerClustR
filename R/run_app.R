#' Launch ClusterAnalysis Shiny Application
#'
#' @description
#' Launches the interactive Shiny application for variable clustering analysis.
#' This app provides a user-friendly interface for VAR_CAH, VAR_KMEANS, and 
#' TandemVarClust algorithms.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @param launch.browser Logical. Should the app open in browser? Default TRUE.
#' @param port Integer. Port number for the app. Default is random.
#' @param host Character. Host IP. Default is "127.0.0.1" (localhost).
#'
#' @return No return value. Launches the Shiny app.
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' run_cluster_app()
#' 
#' # Launch on specific port
#' run_cluster_app(port = 8080)
#' 
#' # Launch without opening browser
#' run_cluster_app(launch.browser = FALSE)
#' }
#'
#' @export
#' @import shiny
#' @import shinydashboard
run_cluster_app <- function(..., 
                            launch.browser = TRUE,
                            port = NULL,
                            host = "127.0.0.1") {
  
  # Vérifier que Shiny est installé
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Le package 'shiny' est requis pour lancer l'application.\n",
      "Installez-le avec: install.packages('shiny')",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop(
      "Le package 'shinydashboard' est requis pour lancer l'application.\n",
      "Installez-le avec: install.packages('shinydashboard')",
      call. = FALSE
    )
  }
  
  # Trouver le chemin de l'app
  app_dir <- system.file("app", package = "RollerClustR")
  
  # Vérifier que l'app existe
  if (app_dir == "") {
    stop(
      "L'application Shiny n'a pas été trouvée dans le package.\n",
      "Vérifiez que le dossier 'inst/app' existe.",
      call. = FALSE
    )
  }
  
  # Message de bienvenue
  message("\n")
  message("═══════════════════════════════════════════════════════════")
  message("   ClusterAnalysis - Interactive Shiny Application")
  message("═══════════════════════════════════════════════════════════")
  message("\nAlgorithmes disponibles :")
  message("  • VAR_CAH        : Classification Ascendante Hiérarchique")
  message("  • VAR_KMEANS     : K-Means pour Variables")
  message("  • TandemVarClust : Approche Tandem (AFDM + CAH)")
  message("\nLancement de l'application...")
  message("Pour arrêter l'app : appuyez sur Echap ou Ctrl+C\n")
  
  # Lancer l'app
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host,
    ...
  )
}


#' @rdname run_cluster_app
#' @export
runRollerClustR <- run_cluster_app  # 