# Guide de Contribution à RollerClustR

Merci de votre intérêt pour contribuer à RollerClustR ! Ce document vous guide pour contribuer efficacement au projet.

## Code de Conduite

En participant à ce projet, vous acceptez de respecter notre Code de Conduite. Soyez respectueux, inclusif et professionnel dans toutes les interactions.

## Comment Contribuer

### Rapporter des Bugs

Si vous trouvez un bug :

1. **Vérifiez** que le bug n'a pas déjà été rapporté dans les [Issues](https://github.com/yourusername/RollerClustR/issues)
2. **Créez** une nouvelle issue en incluant :
   - Une description claire du problème
   - Un exemple minimal reproductible
   - Votre version de R et du package (`sessionInfo()`)
   - Le comportement attendu vs observé

### Suggérer des Améliorations

Pour suggérer une nouvelle fonctionnalité :

1. Ouvrez une issue avec le label "enhancement"
2. Expliquez clairement le cas d'usage
3. Décrivez la solution proposée
4. Discutez des alternatives envisagées

### Contribuer du Code

#### Processus

1. **Fork** le dépôt
2. **Créez** une branche pour votre fonctionnalité (`git checkout -b feature/ma-fonctionnalite`)
3. **Développez** en suivant le style du projet
4. **Testez** votre code (voir section Tests)
5. **Documentez** avec roxygen2
6. **Commitez** (`git commit -m 'Ajout de ma fonctionnalité'`)
7. **Pushez** (`git push origin feature/ma-fonctionnalite`)
8. **Créez** une Pull Request

#### Standards de Code

**Style R :**
- Suivre le [tidyverse style guide](https://style.tidyverse.org/)
- Utiliser `snake_case` pour les fonctions et variables
- Utiliser `PascalCase` pour les classes R6
- Limiter les lignes à 80 caractères

**Architecture R6 :**
- Les classes filles doivent hériter de `ClusterAnalysis`
- Implémenter tous les contrats d'héritage (méthodes `do_*`)
- Utiliser des champs privés avec préfixe `F` (ex: `FNbGroupes`)
- Documenter les active bindings

**Exemple :**
```r
#' @title Ma Nouvelle Classe
#' @description Description claire
#' @export
MaClasse <- R6Class("MaClasse",
  inherit = ClusterAnalysis,
  
  private = list(
    FMonChamp = NULL,
    
    do_fit = function(X) {
      # Implémentation
    }
  ),
  
  public = list(
    initialize = function(...) {
      super$initialize(...)
    }
  )
)
```

### Documentation

Toute contribution de code doit inclure :

**Documentation roxygen2 :**
```r
#' @title Titre Court
#' @description Description détaillée
#' @param x Description du paramètre
#' @return Description du retour
#' @examples
#' # Exemple fonctionnel
#' ma_fonction(x = 1)
#' @export
ma_fonction <- function(x) {
  # code
}
```

**Vignettes :**
- Pour les fonctionnalités majeures, ajouter une vignette
- Utiliser R Markdown avec chunks exécutables
- Inclure des exemples pratiques

### Tests

**Tests obligatoires :**
- Tous les nouveaux codes doivent avoir des tests
- Utiliser `testthat` (voir `tests/testthat/`)
- Viser >80% de couverture de code

**Structure des tests :**
```r
test_that("description du test", {
  # Arrange
  data <- data.frame(x = 1:10, y = rnorm(10))
  
  # Act
  result <- ma_fonction(data)
  
  # Assert
  expect_equal(nrow(result), 10)
  expect_true(all(!is.na(result)))
})
```

**Catégories de tests à couvrir :**
- Tests de base (happy path)
- Tests de validation des paramètres
- Tests de gestion d'erreurs
- Tests de cas limites (edge cases)
- Tests de robustesse (grandes données, données manquantes)

### Processus de Review

Les Pull Requests seront reviewées selon :

1. **Conformité au style** : Le code suit-il les conventions ?
2. **Tests** : Les tests sont-ils présents et passent-ils ?
3. **Documentation** : La documentation est-elle claire et complète ?
4. **Performance** : Y a-t-il des régressions de performance ?
5. **Compatibilité** : Le code est-il compatible avec R >= 4.0.0 ?

## Structure du Projet

```
RollerClustR/
├── R/                      # Code source
│   ├── ClusterAnalysis.R   # Classe parente
│   ├── VAR_CAH.R          # Classe VAR_CAH
│   ├── VARCLUS.R          # Classe VARCLUS
│   ├── KmodesVarClust.R   # Classe KmodesVarClust
│   ├── roller_clust.R     # Fonction wrapper
│   └── utils.R            # Fonctions utilitaires
├── tests/                  # Tests unitaires
│   └── testthat/
├── vignettes/             # Vignettes
├── man/                   # Documentation générée
├── DESCRIPTION            # Métadonnées du package
├── NAMESPACE              # Exports (généré par roxygen2)
└── README.md
```

## Workflow de Développement

### Installation en Mode Développement

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Cloner le repo et installer
devtools::install()

# Charger le package en développement
devtools::load_all()
```

### Workflow Typique

```r
# 1. Modifier le code dans R/

# 2. Générer la documentation
devtools::document()

# 3. Vérifier le package
devtools::check()

# 4. Tester
devtools::test()

# 5. Construire le package
devtools::build()
```

### Vérifications Avant Pull Request

Avant de soumettre une PR, assurez-vous que :

```r
# Tous les tests passent
devtools::test()

# Aucune erreur dans check
devtools::check()

# Le code est bien formaté
# (installer styler si nécessaire)
styler::style_pkg()

# La documentation est à jour
devtools::document()
```

## Questions ?

Si vous avez des questions :

1. Consultez la [documentation](https://github.com/yourusername/RollerClustR)
2. Cherchez dans les [Issues existantes](https://github.com/yourusername/RollerClustR/issues)
3. Ouvrez une nouvelle issue avec le label "question"

## Remerciements

Merci à tous les contributeurs qui aident à améliorer RollerClustR !
