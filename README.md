# RollerClustR <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/yourusername/RollerClustR/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/RollerClustR/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Vue d'ensemble

**RollerClustR** est un package R qui implémente des algorithmes avancés de clustering pour variables avec une architecture orientée objet R6. Le package propose trois méthodes de clustering spécialisées :

- **VAR_CAH** : Clustering hiérarchique ascendant pour variables
- **KmodesVarClust** : Clustering pour données catégorielles
- **VARCLUS** : Clustering descendant avec division récursive basée sur l'ACP

## Caractéristiques principales

✨ **Architecture R6** : Classes orientées objet avec héritage et encapsulation

🔧 **Flexibilité** : Paramètres de discrétisation configurables pour variables continues

📊 **Robustesse** : Gestion complète des valeurs manquantes et validation des données

🎯 **Simplicité** : Interface uniforme via la fonction wrapper `roller_clust()`

## Installation

Vous pouvez installer la version de développement depuis GitHub :

```r
# install.packages("devtools")
devtools::install_github("yourusername/RollerClustR")
```

## Exemple rapide

```r
library(RollerClustR)

# Utilisation simple avec la fonction wrapper
data(iris)
result <- roller_clust(
  X = iris[, 1:4],
  method = "varclus",
  K = 3
)

# Afficher le résumé
result$summary()

# Accéder aux groupes
groups <- result$Groupes
```

## Utilisation avancée avec les classes R6

```r
# Clustering hiérarchique
model_cah <- VAR_CAH$new(K = 3, scale = TRUE)
model_cah$fit(iris[, 1:4])
model_cah$summary()

# Clustering VARCLUS
model_vc <- VARCLUS$new(K = 2)
model_vc$fit(iris[, 1:4])
model_vc$summary()

# Modifier le nombre de clusters après ajustement
model_vc$K <- 3
```

## Documentation

Pour plus de détails, consultez :

- La vignette d'introduction : `vignette("introduction", package = "RollerClustR")`
- La documentation des fonctions : `help(package = "RollerClustR")`
- Les exemples reproductibles : `example(roller_clust)`

## Contribution

Les contributions sont les bienvenues ! Veuillez consulter [CONTRIBUTING.md](CONTRIBUTING.md) pour les directives.

## Citation

Si vous utilisez RollerClustR dans vos travaux, veuillez citer :

```r
citation("RollerClustR")
```

## Licence

MIT © 2025 Bryan (Your Full Name)
