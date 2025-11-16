#' Interface Unifiée pour le Clustering de Variables
#'
#' @description
#' `roller_clust()` est la fonction principale du package RollerClustR. Elle
#' fournit une interface simple et unifiée pour accéder aux trois algorithmes
#' de clustering de variables : VAR_CAH, VARCLUS et KmodesVarClust.
#'
#' @param X Data frame ou matrice contenant les données. Les colonnes représentent
#'   les variables à clustériser.
#' @param method Méthode de clustering à utiliser :
#'   - `"var_cah"` : Clustering Ascendant Hiérarchique (défaut)
#'   - `"varclus"` : Clustering Descendant avec ACP
#'   - `"kmodes"` : K-modes pour variables catégorielles
#' @param K Nombre de clusters souhaités (défaut : 2). Doit être >= 2.
#' @param scale Booléen indiquant si les variables numériques doivent être
#'   centrées-réduites (défaut : TRUE). Ignoré pour "kmodes".
#' @param na.action Action en cas de valeurs manquantes :
#'   - `"warn"` : Émet un avertissement (défaut)
#'   - `"omit"` : Supprime les observations avec NA
#'   - `"fail"` : Arrête l'exécution
#' @param ... Arguments supplémentaires spécifiques à chaque méthode :
#'   - Pour VAR_CAH : `max.iter`, `tolerance`
#'   - Pour VARCLUS : `stop_eigenvalue`
#'   - Pour KmodesVarClust : `n_bins`, `discretization_method`
#'
#' @return Un objet R6 de la classe correspondante (VAR_CAH, VARCLUS ou
#'   KmodesVarClust) déjà ajusté sur les données.
#'
#' @details
#' ## Choix de la méthode
#'
#' - **VAR_CAH** : Approche ascendante, fusion progressive. Recommandé pour une
#'   première exploration ou quand les variables sont numériques continues.
#'
#' - **VARCLUS** : Approche descendante, division récursive. Meilleur pour
#'   identifier des structures hiérarchiques complexes.
#'
#' - **KmodesVarClust** : Spécialisé pour variables catégorielles. Utilise une
#'   discrétisation automatique pour les variables continues.
#'
#' ## Valeurs de retour
#'
#' L'objet retourné dispose des méthodes et propriétés suivantes :
#'
#' - `$summary()` : Affiche un résumé détaillé du clustering
#' - `$Groupes` : Accède au vecteur des affectations
#' - `$K` : Lit ou modifie le nombre de clusters
#'
#' @examples
#' # Exemple 1 : Clustering hiérarchique ascendant
#' library(RollerClustR)
#' data(iris)
#'
#' model_cah <- roller_clust(
#'   X = iris[, 1:4],
#'   method = "var_cah",
#'   K = 2,
#'   scale = TRUE
#' )
#' model_cah$summary()
#'
#' # Exemple 2 : Clustering descendant VARCLUS
#' model_vc <- roller_clust(
#'   X = iris[, 1:4],
#'   method = "varclus",
#'   K = 3
#' )
#' print(model_vc$Groupes)
#'
#' # Exemple 3 : K-modes pour données catégorielles
#' data(Titanic)
#' titanic_df <- as.data.frame(Titanic)
#'
#' model_km <- roller_clust(
#'   X = titanic_df[, c("Class", "Sex", "Age", "Survived")],
#'   method = "kmodes",
#'   K = 2
#' )
#' model_km$summary()
#'
#' # Exemple 4 : Modifier K après ajustement
#' model_cah$K <- 3  # Ré-ajuste automatiquement avec K=3
#'
#' @seealso [VAR_CAH], [VARCLUS], [KmodesVarClust]
#'
#' @export
roller_clust <- function(X,
                         method = c("var_cah", "varclus", "kmodes"),
                         K = 2,
                         scale = TRUE,
                         na.action = c("warn", "omit", "fail"),
                         ...) {
  
  # Validation des arguments
  method <- match.arg(method)
  na.action <- match.arg(na.action)
  
  if (!is.numeric(K) || K < 2) {
    stop("K doit être un nombre entier >= 2")
  }
  
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X doit être un data.frame ou une matrice")
  }
  
  # Créer et ajuster le modèle selon la méthode choisie
  model <- switch(method,
    "var_cah" = {
      obj <- VAR_CAH$new(K = K, scale = scale, na.action = na.action, ...)
      obj$fit(X)
      obj
    },
    "varclus" = {
      obj <- VARCLUS$new(K = K, scale = scale, na.action = na.action, ...)
      obj$fit(X)
      obj
    },
    "kmodes" = {
      obj <- KmodesVarClust$new(K = K, na.action = na.action, ...)
      obj$fit(X)
      obj
    }
  )
  
  return(model)
}
