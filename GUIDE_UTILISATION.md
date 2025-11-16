# Guide Complet : Transformer ton Code R6 en Package R

## 📦 Structure Créée

Voici la structure complète du package RollerClustR :

```
RollerClustR/
├── DESCRIPTION              # Métadonnées du package
├── NAMESPACE                # Exports (généré par roxygen2)
├── LICENSE                  # Fichier de licence
├── README.md                # Page d'accueil du package
├── NEWS.md                  # Historique des versions
├── CONTRIBUTING.md          # Guide de contribution
├── .Rbuildignore           # Fichiers à ignorer lors du build
│
├── R/                       # Code source du package
│   ├── RollerClustR-package.R     # Documentation du package
│   ├── ClusterAnalysis.R          # Classe parente (documentée)
│   ├── VAR_CAH.R                  # Classe VAR_CAH (documentée)
│   ├── VARCLUS.R                  # Classe VARCLUS (documentée)
│   ├── KmodesVarClust.R           # Classe KmodesVarClust (documentée)
│   ├── roller_clust.R             # Fonction wrapper principale
│   ├── utils.R                    # Fonctions utilitaires
│   └── wrapper.R                  # Anciennes fonctions wrapper
│
├── man/                     # Documentation (générée par roxygen2)
│   └── (sera généré automatiquement)
│
├── tests/                   # Tests unitaires
│   ├── testthat.R
│   └── testthat/
│       ├── test-VAR_CAH.R
│       └── test-roller_clust.R
│
├── vignettes/              # Tutoriels et guides
│   └── introduction.Rmd
│
└── data-raw/               # Scripts pour créer les datasets
```

## 🚀 Étapes Suivantes

### Étape 1 : Personnaliser DESCRIPTION

Ouvre `DESCRIPTION` et modifie :

```r
Authors@R: c(
    person("Bryan", "TON_NOM_DE_FAMILLE", 
           email = "ton.email@universite.fr",
           role = c("aut", "cre"),
           comment = c(ORCID = "XXXX-XXXX-XXXX-XXXX"))  # Optionnel
    )
```

Si tu n'as pas d'ORCID, supprime simplement la ligne `comment`.

### Étape 2 : Installer les Dépendances Nécessaires

Sur ta machine locale (avec R installé) :

```r
# Installer les packages de développement
install.packages(c(
  "devtools",      # Outils de développement
  "roxygen2",      # Génération de documentation
  "testthat",      # Tests unitaires
  "knitr",         # Vignettes
  "rmarkdown"      # Vignettes
))
```

### Étape 3 : Générer la Documentation

```r
# Charger le projet (dans RStudio : File > Open Project > RollerClustR.Rproj)
# Ou en console :
setwd("path/to/RollerClustR")

# Générer la documentation à partir des commentaires roxygen2
devtools::document()

# Cela crée automatiquement :
# - Les fichiers .Rd dans man/
# - Met à jour NAMESPACE
```

### Étape 4 : Vérifier le Package

```r
# Vérification complète (recommandé avant publication)
devtools::check()

# Vérification rapide (pendant le développement)
devtools::load_all()  # Charge le package en mémoire
```

La commande `check()` va :
- Vérifier la structure du package
- Compiler la documentation
- Exécuter les tests
- Chercher les erreurs potentielles

**Attendu-toi à quelques WARNINGS/NOTES** :
- Pas de fichier `LICENSE.md` → Ajoute-le si tu veux (optionnel)
- Variables globales non définies → Normal pour R6

### Étape 5 : Tester le Package

```r
# Exécuter tous les tests
devtools::test()

# Exécuter un fichier de test spécifique
testthat::test_file("tests/testthat/test-VAR_CAH.R")
```

### Étape 6 : Construire et Installer

```r
# Construire le package (.tar.gz)
devtools::build()

# Installer localement
devtools::install()

# Ensuite, tu peux l'utiliser comme n'importe quel package :
library(RollerClustR)
?roller_clust
```

### Étape 7 : Créer le fichier .Rproj (Optionnel mais Recommandé)

Si tu utilises RStudio :

1. File > New Project > Existing Directory
2. Sélectionner le dossier `RollerClustR`
3. RStudio va créer un fichier `.Rproj`

Cela active plein de fonctionnalités utiles dans RStudio.

## 📝 Modifications à Faire

### Problèmes à Corriger dans le Code

1. **Retirer les `library()` des fichiers R** :

Dans `R/VAR_CAH.R`, `R/VARCLUS.R`, etc., SUPPRIME les lignes :
```r
library(R6)
```

Les packages doivent importer leurs dépendances via `DESCRIPTION` et `NAMESPACE`, pas via `library()`.

2. **Vérifier que `validate_data_type()` est exportée** :

Dans `R/utils.R`, assure-toi qu'il y a bien `@export` avant la fonction.

3. **Corriger les références circulaires** :

Les classes filles (VAR_CAH, etc.) doivent pouvoir référencer `ClusterAnalysis` car elle est définie dans le même package.

### Ordre de Chargement

R charge les fichiers dans l'ordre alphabétique. Pour garantir que `ClusterAnalysis` soit chargée en premier :

Option 1 : Renommer en `R/aaa-ClusterAnalysis.R`
Option 2 : Utiliser `@include` dans roxygen2 (plus propre)

Dans `R/VAR_CAH.R`, ajoute AVANT la documentation :
```r
#' @include ClusterAnalysis.R
```

## 🔧 Workflow de Développement Quotidien

Quand tu développes :

```r
# 1. Modifier le code dans R/

# 2. Recharger le package
devtools::load_all()  # Raccourci : Ctrl+Shift+L dans RStudio

# 3. Tester interactivement
model <- VAR_CAH$new(K = 2)
model$fit(iris[, 1:4])

# 4. Quand satisfait, regénérer la doc
devtools::document()  # Raccourci : Ctrl+Shift+D dans RStudio

# 5. Exécuter les tests
devtools::test()      # Raccourci : Ctrl+Shift+T dans RStudio

# 6. Vérifier le package
devtools::check()     # Raccourci : Ctrl+Shift+E dans RStudio
```

## 📚 Ajouter des Datasets au Package

Si tu veux inclure des datasets (comme tes exemples Iris/Titanic) :

```r
# 1. Créer les données
iris_subset <- iris[, 1:4]

# 2. Sauvegarder dans data/
usethis::use_data(iris_subset, overwrite = TRUE)

# 3. Documenter dans R/data.R
#' Sous-ensemble du jeu de données Iris
#'
#' @format Un data frame avec 150 lignes et 4 variables :
#' \describe{
#'   \item{Sepal.Length}{Longueur du sépale en cm}
#'   \item{Sepal.Width}{Largeur du sépale en cm}
#'   \item{Petal.Length}{Longueur du pétale en cm}
#'   \item{Petal.Width}{Largeur du pétale en cm}
#' }
"iris_subset"
```

## 📖 Générer le Site Web du Package (pkgdown)

Pour créer un site web pour ton package :

```r
# Installer pkgdown
install.packages("pkgdown")

# Créer le site
pkgdown::build_site()

# Le site sera dans docs/
# Tu peux l'héberger sur GitHub Pages
```

## 🐛 Débogage

Si tu rencontres des erreurs :

```r
# Afficher les détails d'une erreur
traceback()

# Déboguer une fonction
debug(VAR_CAH$new)
# Puis exécute ton code
# undebug(VAR_CAH$new) pour arrêter

# Vérifier les dépendances
devtools::dev_package_deps()
```

## 📤 Publier sur GitHub

```bash
# Initialiser git
cd RollerClustR
git init
git add .
git commit -m "Initial commit: RollerClustR package"

# Créer un repo sur GitHub puis :
git remote add origin https://github.com/TON_USERNAME/RollerClustR.git
git push -u origin main
```

Ensuite, les utilisateurs peuvent installer avec :
```r
devtools::install_github("TON_USERNAME/RollerClustR")
```

## 📋 Checklist Avant Publication

- [ ] `devtools::check()` ne retourne aucune ERROR
- [ ] Tous les tests passent (`devtools::test()`)
- [ ] Documentation complète et à jour
- [ ] README.md avec exemples fonctionnels
- [ ] NEWS.md à jour
- [ ] LICENSE correct
- [ ] Vignette d'introduction fonctionnelle
- [ ] DESCRIPTION avec tes infos
- [ ] Code conforme au style guide

## 🎓 Ressources Supplémentaires

- **Livre R Packages (2e)** : https://r-pkgs.org/
- **Documentation roxygen2** : https://roxygen2.r-lib.org/
- **Guide testthat** : https://testthat.r-lib.org/
- **Style guide tidyverse** : https://style.tidyverse.org/

## 💡 Conseils

1. **Commence petit** : Assure-toi que le package se charge avant d'ajouter plus de fonctionnalités
2. **Documente au fur et à mesure** : C'est plus facile que de tout documenter à la fin
3. **Teste régulièrement** : `devtools::load_all()` est ton meilleur ami
4. **Utilise les raccourcis RStudio** : Ça accélère énormément le développement
5. **Lis les messages d'erreur** : `devtools::check()` est très verbeux mais très utile

## ❓ Problèmes Courants

**"could not find function"**
→ Assure-toi que la fonction est exportée (`@export`) ou importée (`@importFrom`)

**"object not found"**
→ Vérifie que toutes les dépendances sont dans `DESCRIPTION`

**"namespace not loaded"**
→ Relance `devtools::load_all()` ou redémarre R

**Tests qui échouent**
→ Vérifie que les fixtures de test sont correctes

## 🎉 Prochaines Étapes

Une fois le package fonctionnel :

1. Publier sur CRAN (optionnel, processus plus strict)
2. Ajouter des méthodes de visualisation (ggplot2)
3. Créer des shiny apps pour l'utilisation interactive
4. Ajouter plus de méthodes de clustering
5. Optimiser les performances pour grandes dimensions

Bonne chance avec ton package ! N'hésite pas si tu as des questions.
