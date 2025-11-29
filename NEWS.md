# RollerClustR 0.1.0

## Première version

* Implémentation de trois algorithmes de clustering de variables :
  - VAR_CAH : Clustering combinant ACP et CAH
  - VARCLUS : Clustering Descendant avec ACP
  - tandemVarClust : Clustering combinant AFDM et CAH

* Architecture R6 orientée objet avec :
  - Classe parente ClusterAnalysis
  - Pattern Template Method
  - Active bindings pour API propre

* Fonctionnalités principales :
  - Fonction wrapper `roller_clust()` pour interface simple
  - Paramètres de discrétisation configurables
  - Gestion robuste des valeurs manquantes
  - Validation automatique des types de données

* Documentation complète :
  - Vignette d'introduction
  - Documentation roxygen2 pour toutes les fonctions
  - Exemples reproductibles
  - Tests unitaires avec testthat

## Améliorations futures planifiées

* Méthodes de visualisation (dendrogrammes, heatmaps)
* Critères de sélection automatique de K
* Support de méthodes de clustering supplémentaires
* Optimisations de performance pour grandes dimensions
