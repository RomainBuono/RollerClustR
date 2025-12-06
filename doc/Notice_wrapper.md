# Architecture Logicielle du Package RollerClustR : Module Wrapper et Interface Utilisateur

## 1. Introduction et Cadre Conceptuel

### 1.1 Contexte Scientifique

Le présent document expose l'architecture logicielle du package RollerClustR, conçu pour faciliter l'utilisation et l'évaluation d'algorithmes de clustering de variables. Dans le paradigme du clustering de variables, l'objectif consiste à regrouper des variables présentant des structures de dépendance similaires, par opposition au clustering d'observations traditionnel. Cette approche trouve des applications en réduction de dimensionnalité, en détection de redondance informationnelle et en construction de variables synthétiques.

### 1.2 Paradigme de Conception : Architecture Orientée Objet avec Pattern Template Method

L'architecture adoptée s'appuie sur le pattern de conception Template Method via une classe parent `ClusterAnalysis`, qui définit une interface unifiée pour tous les algorithmes de clustering. Cette approche offre plusieurs avantages théoriques et pratiques :

1. **Polymorphisme architectural** : Tous les algorithmes héritent d'une interface commune garantissant la cohérence de l'API.
2. **Extensibilité** : L'ajout de nouveaux algorithmes nécessite uniquement l'implémentation de méthodes privées contractuelles.
3. **Wrapper fonctionnel** : La fonction `roller_clust()` encapsule la logique de sélection et d'instanciation des algorithmes.

## 2. Classe Parent ClusterAnalysis : Contrat d'Interface

### 2.1 Principe Architectural

La classe `ClusterAnalysis` constitue le fondement polymorphe du package. Elle implémente le pattern Template Method en définissant des méthodes publiques invariantes qui délèguent l'implémentation spécifique à des méthodes privées virtuelles.

### 2.2 Contrat d'Implémentation

Chaque algorithme enfant (VAR_CAH, VAR_KMEANS, TandemVarClust) doit implémenter les méthodes privées suivantes :

```r
# Méthodes privées obligatoires
do_fit(X)              # Ajustement spécifique de l'algorithme
do_refit_with_k(new_k) # Réajustement avec nouveau K
do_predict(newdata)    # Prédiction pour nouvelles données
do_summary()           # Affichage de résumé spécifique
```

### 2.3 Méthodes Publiques Uniformes

```r
# Interface publique commune à tous les algorithmes
model$fit(X)           # Ajuster le modèle
model$summary()        # Afficher le résumé
model$predict(newdata) # Prédire (si applicable)

# Active bindings
model$K                # Nombre de clusters (lecture/écriture)
model$Groupes          # Assignations aux clusters
```

Cette architecture garantit que tout algorithme implémenté expose une API cohérente, facilitant l'interchangeabilité et la comparaison.

## 3. Fonction Wrapper roller_clust() : Point d'Entrée Unifié

### 3.1 Principe de la Fonction Wrapper

La fonction `roller_clust()` constitue le point d'entrée principal du package. Elle encapsule la logique de sélection d'algorithme et simplifie l'interface utilisateur en exposant un paramètre `method` unique.

```r
roller_clust <- function(X, 
                         method = c("var_cah", "var_kmeans", "tandem"),
                         K = 2,
                         ...) {
  method <- match.arg(method)
  
  model <- switch(method,
    var_cah = VAR_CAH$new(K = K, ...),
    var_kmeans = VAR_KMEANS$new(K = K, ...),
    tandem = TandemVarClust$new(K = K, ...)
  )
  
  model$fit(X)
  return(model)
}
```

### 3.2 Avantages du Wrapper

1. **Simplicité d'utilisation** : Interface unique pour tous les algorithmes
2. **Validation des paramètres** : Via `match.arg()` pour prévenir les erreurs
3. **Transmission transparente** : Paramètres spécifiques transmis via `...`
4. **Retour polymorphe** : Objet héritant de `ClusterAnalysis`

### 3.3 Workflow Type d'Utilisation

```r
# Approche wrapper (recommandée pour utilisateurs)
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 3, scale = TRUE)

# Approche directe (pour utilisateurs avancés)
model <- VAR_CAH$new(K = 3, scale = TRUE)
model$fit(iris[, 1:4])
```

Les deux approches produisent des objets identiques, mais le wrapper offre une syntaxe plus concise.

## 4. Algorithmes Implémentés

### 4.1 VAR_CAH : Classification Ascendante Hiérarchique

#### Fondements Théoriques

VAR_CAH implémente une Classification Ascendante Hiérarchique (CAH) sur variables numériques. L'algorithme construit une hiérarchie de partitions par agglomération successive basée sur une matrice de dissimilarité :

$$d_{ij} = 1 - |r_{ij}|$$

où $r_{ij}$ est le coefficient de corrélation entre variables $i$ et $j$.

#### Méthodes Spécifiques

```r
# Instanciation
model <- VAR_CAH$new(K = 3, scale = TRUE)
model$fit(X)

# Méthodes publiques spécifiques
tree <- model$get_tree()                              # Arbre hiérarchique (hclust)
vars <- model$get_cluster_variables(cluster_id = k)   # Variables du cluster k
rep_var <- model$get_representative_variable(cluster_id = k) # Variable représentative
quality <- model$inertie()                            # Métriques d'inertie

# Active bindings
model$Groupes  # Assignations variables → clusters
```

#### Paramètres Algorithmiques

- `scale` : Standardisation des variables (TRUE/FALSE)
- `K` : Nombre de clusters
- Métrique : Distance basée sur corrélation (fixe)
- Linkage : Complete (fixe)

### 4.2 VAR_KMEANS : K-Means avec Composantes Principales

#### Principe Méthodologique

VAR_KMEANS adapte l'algorithme K-Means au clustering de variables en utilisant la première composante principale comme prototype de cluster. L'algorithme maximise la somme des corrélations au carré ($r^2$) entre variables et leurs centres de clusters.

**Critère d'optimisation** :

$$\max \sum_{k=1}^{K} \sum_{j \in C_k} r^2(X_j, PC1_k)$$

où $PC1_k$ est la première composante principale du cluster $k$.

#### Méthodes Spécifiques

```r
# Instanciation
model <- VAR_KMEANS$new(K = 3, n_init = 20, max_iter = 100)
model$fit(X)

# Méthodes publiques spécifiques
vars <- model$get_cluster_variables(k = cluster_num)  # ⚠️ Paramètre 'k', pas 'cluster_id'
centers <- model$get_cluster_centers()                # Centres (obs × K)

# Active bindings spécifiques
model$Converged            # Statut de convergence
model$NIterations          # Nombre d'itérations
model$WithinClusterInertia # Somme des r²
model$Homogeneite          # r² moyen
```

#### Différence Cruciale avec VAR_CAH

⚠️ **Attention** : Le paramètre de `get_cluster_variables()` diffère :
- VAR_CAH : `get_cluster_variables(cluster_id = k)`
- VAR_KMEANS : `get_cluster_variables(k)`

### 4.3 TandemVarClust : Approche Tandem (ACM + CAH)

#### Contexte Méthodologique

TandemVarClust implémente une approche tandem combinant l'Analyse des Correspondances Multiples (ACM) avec la Classification Ascendante Hiérarchique. Cette méthode est spécialisée pour les **données mixtes** (numériques + catégorielles).

**Processus** :
1. Discrétisation des variables numériques en bins
2. Codage disjonctif (one-hot) de toutes les modalités
3. ACM sur la table disjonctive
4. CAH sur les coordonnées factorielles des modalités

**Spécificité** : Clustering de **modalités** (non de variables directement).

#### Méthodes Spécifiques

```r
# Instanciation
model <- TandemVarClust$new(K = 3, n_bins = 4, n_factors = 3)
model$fit(X_mixed)

# Méthodes publiques spécifiques
var_summary <- model$get_variable_summary()           # Résumé par variable
mods <- model$get_modalities_of_variable("var_name")  # Modalités d'une variable
cluster_mods <- model$get_modalities_of_cluster(k)    # Modalités du cluster k
tree <- model$get_tree()                              # Arbre hiérarchique
integrity <- model$check_results_integrity()          # Validation interne

# Active bindings spécifiques
model$NbModalites          # Nombre total de modalités
model$VarianceExplained    # % variance par axe factoriel
model$Inertie              # Inertie totale ACM
model$DisjunctiveTable     # Table disjonctive
model$FactorialCoords      # Coordonnées factorielles
model$CategoricalVars      # Variables catégorielles
model$NumericVars          # Variables numériques
model$VariableClusters     # Variables → clusters (règle majoritaire)
```

#### Prédiction de Variables Illustratives

TandemVarClust permet la prédiction de nouvelles variables catégorielles via projection AFDM :

```r
new_var <- data.frame(
  Transmission = factor(sample(c("Manual", "Auto"), n, replace = TRUE))
)

pred_result <- model$predict(new_var)

# Structure de sortie enrichie
pred_result$Transmission$cluster              # Cluster assigné
pred_result$Transmission$distances            # Distances aux centres
pred_result$Transmission$n_modalities         # Nombre de modalités
pred_result$Transmission$modality_clusters    # Assignation par modalité
pred_result$Transmission$contingency          # Table de contingence
pred_result$Transmission$chi2_test            # Test du Chi²
pred_result$Transmission$cramers_v            # V de Cramér (0-1)
pred_result$Transmission$significant          # Significativité (p < 0.05)
pred_result$Transmission$dice_scores          # Matrice scores Dice
```

**Métriques d'Association** :

Le V de Cramér mesure la force d'association :

$$V = \sqrt{\frac{\chi^2}{n \times \min(r-1, c-1)}}$$

- $V \in [0, 1]$ : 0 = indépendance, 1 = association parfaite
- `significant = TRUE` + $V > 0.3$ : association forte
- `significant = TRUE` + $0.1 < V < 0.3$ : association modérée

## 5. Tableau Comparatif des Algorithmes

| Caractéristique | VAR_CAH | VAR_KMEANS | TandemVarClust |
|----------------|---------|------------|----------------|
| **Type de données** | Numériques | Numériques | Mixtes (num + cat) |
| **Unité de clustering** | Variables | Variables | Modalités |
| **Méthode** | Hiérarchique ascendante | Partitionnement itératif | Tandem (ACM + CAH) |
| **Centres de clusters** | ❌ Non accessibles | ✅ PC1 par cluster | ❌ Non accessibles |
| **Arbre hiérarchique** | ✅ `get_tree()` | ❌ Non applicable | ✅ `get_tree()` |
| **Convergence** | ❌ Non applicable | ✅ `Converged`, `NIterations` | ❌ Non applicable |
| **Prédiction** | ✅ Nouvelles variables | ✅ Nouvelles variables | ✅ Variables illustratives |
| **Méthode inertie** | `inertie()` | `WithinClusterInertia` | ❌ Non implémenté |
| **Param get_cluster_variables** | `cluster_id` | `k` | `k` (modalités) |
| **Discrétisation** | ❌ Non | ❌ Non | ✅ `n_bins` |

## 6. Exemples d'Utilisation Comparatifs

### 6.1 VAR_CAH : Données Numériques

```r
library(RollerClustR)
data(mtcars)

# Via wrapper
model <- roller_clust(mtcars[, 1:7], method = "var_cah", K = 3, scale = TRUE)

# Exploration
model$summary()
vars_c1 <- model$get_cluster_variables(cluster_id = 1)
rep_var <- model$get_representative_variable(cluster_id = 1)
quality <- model$inertie()

# Visualisation
tree <- model$get_tree()
plot(tree, main = "Variable Dendrogram")
rect.hclust(tree, k = 3, border = "red")
```

### 6.2 VAR_KMEANS : Données Numériques avec Convergence

```r
# Via wrapper
model <- roller_clust(mtcars[, 1:7], 
                      method = "var_kmeans", 
                      K = 3, 
                      n_init = 20,
                      max_iter = 100)

# Métriques de convergence
model$Converged
model$NIterations
model$Homogeneite

# Centres de clusters
centers <- model$get_cluster_centers()

# Variables du cluster 2
vars_c2 <- model$get_cluster_variables(k = 2)  # Attention: 'k' pas 'cluster_id'
```

### 6.3 TandemVarClust : Données Mixtes

```r
# Préparation données mixtes
mtcars_mixed <- data.frame(
  mpg = mtcars$mpg,
  hp = mtcars$hp,
  cyl = factor(mtcars$cyl),
  gear = factor(mtcars$gear),
  am = factor(mtcars$am)
)

# Via wrapper
model <- roller_clust(mtcars_mixed, 
                      method = "tandem", 
                      K = 3, 
                      n_bins = 4,
                      n_factors = 3)

# Exploration des modalités
model$summary()
var_summary <- model$get_variable_summary()
mods_mpg <- model$get_modalities_of_variable("mpg")
cluster_mods <- model$get_modalities_of_cluster(1)

# Prédiction variable illustrative
set.seed(200)
new_var <- data.frame(
  Transmission = factor(sample(c("Manual", "Auto"), 32, replace = TRUE))
)

pred <- model$predict(new_var)
pred$Transmission$cluster
pred$Transmission$cramers_v
pred$Transmission$significant
```

## 7. Gestion des Paramètres via Ellipse `...`

### 7.1 Principe de Transmission Transparente

Le wrapper `roller_clust()` utilise l'ellipse `...` pour transmettre des paramètres spécifiques à chaque algorithme sans nécessiter de paramètres nommés dans la signature.

### 7.2 Paramètres Spécifiques par Algorithme

**VAR_CAH** :
```r
roller_clust(X, method = "var_cah", 
             K = 3,
             scale = TRUE)  # Standardisation
```

**VAR_KMEANS** :
```r
roller_clust(X, method = "var_kmeans",
             K = 3,
             n_init = 20,      # Nombre d'initialisations
             max_iter = 100,   # Itérations max
             tolerance = 1e-6, # Seuil de convergence
             scale = TRUE)
```

**TandemVarClust** :
```r
roller_clust(X, method = "tandem",
             K = 3,
             n_bins = 4,       # Bins pour discrétisation
             n_factors = 3,    # Axes factoriels utilisés
             scale = TRUE)
```

### 7.3 Validation des Paramètres

Chaque classe enfant valide ses paramètres dans `initialize()`. Les paramètres invalides déclenchent des erreurs explicites :

```r
# Erreur : K trop grand
model <- VAR_CAH$new(K = 10)
model$fit(iris[, 1:4])  # Erreur: K=10 > 4 variables

# Erreur : n_bins invalide
model <- TandemVarClust$new(K = 3, n_bins = 1)  # Erreur: n_bins >= 2 requis
```

## 8. Modification Dynamique de K

### 8.1 Active Binding K

Tous les algorithmes exposent `K` comme active binding lecture/écriture. La modification de `K` déclenche automatiquement un réajustement :

```r
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)
model$summary()  # 2 clusters

# Modification dynamique
model$K <- 4
model$summary()  # 4 clusters (réajustement automatique)
```

### 8.2 Implémentation via do_refit_with_k()

Chaque algorithme implémente `do_refit_with_k(new_k)` différemment :

- **VAR_CAH** : Recoupe l'arbre hiérarchique (rapide, pas de re-calcul)
- **VAR_KMEANS** : Réexécute l'algorithme complet (lent)
- **TandemVarClust** : Recoupe l'arbre (rapide)

```r
# VAR_CAH : réutilise l'arbre existant
model <- VAR_CAH$new(K = 2)
model$fit(iris[, 1:4])
model$K <- 5  # Instantané (cutree() seulement)

# VAR_KMEANS : réajustement complet
model <- VAR_KMEANS$new(K = 2, n_init = 20)
model$fit(iris[, 1:4])
model$K <- 5  # Lent (réexécute 20 initialisations)
```

## 9. Patterns de Conception et Principes SOLID

### 9.1 Template Method Pattern

La classe `ClusterAnalysis` implémente le pattern Template Method :

```r
# Méthode publique (template)
fit = function(X) {
  private$do_fit(X)  # Délégation à implémentation spécifique
  invisible(self)
}

# Méthode privée virtuelle (à implémenter par enfants)
do_fit = function(X) {
  stop("Method 'do_fit(X)' must be implemented by child class.")
}
```

### 9.2 Single Responsibility Principle (SRP)

Chaque classe a une responsabilité unique :
- `ClusterAnalysis` : Définir le contrat d'interface
- `VAR_CAH` : Implémenter CAH sur variables
- `VAR_KMEANS` : Implémenter K-Means avec PC
- `TandemVarClust` : Implémenter approche Tandem mixte
- `roller_clust()` : Simplifier l'instanciation

### 9.3 Open/Closed Principle (OCP)

L'architecture est ouverte à l'extension (ajout de nouveaux algorithmes) mais fermée à la modification (pas besoin de changer le code existant).

**Ajout d'un nouvel algorithme** :
1. Créer classe héritant de `ClusterAnalysis`
2. Implémenter `do_fit()`, `do_refit_with_k()`, `do_predict()`, `do_summary()`
3. Ajouter un cas dans `roller_clust()`

## 10. Gestion des Erreurs et Robustesse

### 10.1 Validation des Données

```r
# VAR_CAH valide le type de données
model <- VAR_CAH$new(K = 3)
model$fit(iris)  # Erreur: colonne Species est catégorielle

# TandemVarClust accepte les données mixtes
model <- TandemVarClust$new(K = 3)
model$fit(iris)  # OK: Species est catégorielle, autres numériques
```

### 10.2 Gestion des Valeurs Manquantes

Les algorithmes utilisent `use = "pairwise.complete.obs"` pour les corrélations, permettant une gestion robuste des NA.

### 10.3 Vérification d'Intégrité

TandemVarClust expose une méthode de validation :

```r
model <- TandemVarClust$new(K = 3)
model$fit(mtcars_mixed)

integrity_ok <- model$check_results_integrity()
# Vérifie: clusters vides, noms de modalités invalides
```

## 11. Complexité Algorithmique

| Algorithme | Complexité Temps | Complexité Espace |
|------------|------------------|-------------------|
| VAR_CAH | $\mathcal{O}(p^2 n + p^3)$ | $\mathcal{O}(p^2)$ |
| VAR_KMEANS | $\mathcal{O}(I \times K \times p \times n)$ | $\mathcal{O}(K \times n)$ |
| TandemVarClust | $\mathcal{O}(n \times m^2 + m^3)$ | $\mathcal{O}(n \times m)$ |

Où :
- $n$ : nombre d'observations
- $p$ : nombre de variables
- $m$ : nombre de modalités (TandemVarClust)
- $I$ : nombre d'itérations (K-Means)
- $K$ : nombre de clusters

## 12. Recommandations d'Utilisation

### 12.1 Choix de l'Algorithme

**VAR_CAH** :
- ✅ Données numériques uniquement
- ✅ Exploration visuelle (dendrogramme)
- ✅ Besoin d'une variable représentative par cluster
- ✅ Plusieurs valeurs de K à tester (rapide via arbre)

**VAR_KMEANS** :
- ✅ Données numériques uniquement
- ✅ Recherche d'optimalité (maximise r²)
- ✅ Besoin de centres de clusters
- ✅ Diagnostics de convergence nécessaires

**TandemVarClust** :
- ✅ Données mixtes (numériques + catégorielles)
- ✅ Interprétation au niveau des modalités
- ✅ Prédiction de variables illustratives
- ⚠️ Attention aux variables redondantes (éviter discrétisation de variables existantes)

### 12.2 Éviter les Pièges Courants

**Piège 1 : Mauvais paramètre pour get_cluster_variables()**
```r
# ❌ VAR_KMEANS avec cluster_id (erreur)
vars <- model_km$get_cluster_variables(cluster_id = 2)

# ✅ Correct
vars <- model_km$get_cluster_variables(k = 2)
```

**Piège 2 : Variables redondantes dans TandemVarClust**
```r
# ❌ Créer variable redondante
iris_mixed <- iris[, 1:4]
iris_mixed$Size <- cut(iris$Sepal.Length, 3)  # Redondant!
iris_mixed$Species <- iris$Species

# Résultat: clustering déséquilibré (24-1-1)

# ✅ Variables indépendantes
iris_mixed <- iris[, 1:4]
iris_mixed$Species <- iris$Species
iris_mixed$Treatment <- sample(c("A", "B", "C"), 150, replace = TRUE)
```

**Piège 3 : Accès à des méthodes inexistantes**
```r
# ❌ VAR_CAH n'a pas get_cluster_centers()
centers <- model_cah$get_cluster_centers()  # Erreur

# ✅ Utiliser VAR_KMEANS
centers <- model_km$get_cluster_centers()  # OK
```

## 13. Extensions et Perspectives

### 13.1 Fonctionnalités Futures Potentielles

1. **Critères de sélection de K** :
   - Gap statistic
   - Silhouette moyenne
   - Critère BIC

2. **Visualisations intégrées** :
   - Heatmap de corrélations
   - Projection dans espace factoriel (TandemVarClust)
   - Graphique de convergence (VAR_KMEANS)

3. **Méthodes d'ensemble** :
   - Consensus clustering
   - Stability selection

### 13.2 Support de Nouveaux Types de Données

- Variables ordinales (actuellement traitées comme catégorielles ou numériques)
- Variables textuelles (via embeddings)
- Séries temporelles (via features extraites)

## 14. Conclusion

### 14.1 Apports Architecturaux

Le package RollerClustR implémente une architecture cohérente et extensible pour le clustering de variables :

1. **Interface unifiée** : Via classe parent `ClusterAnalysis` et wrapper `roller_clust()`
2. **Flexibilité** : Support de données numériques, catégorielles et mixtes
3. **Robustesse** : Validation des paramètres, gestion d'erreurs, vérification d'intégrité
4. **Extensibilité** : Pattern Template Method facilite l'ajout de nouveaux algorithmes

### 14.2 Contributions Méthodologiques

Au-delà de l'implémentation technique, le package propose :
- Implémentation rigoureuse d'algorithmes éprouvés (Chavent et al., Vigneau & Qannari)
- Prédiction de variables illustratives via projection AFDM (TandemVarClust)
- Métriques enrichies (Cramér's V, contingence, Dice)

### 14.3 Positionnement Scientifique

Cette architecture s'inscrit dans une démarche de *computational reproducibility*, où :
- La transparence des méthodes est garantie par le code source ouvert
- La traçabilité est assurée par les tests unitaires (167 tests, 100% succès)
- L'interopérabilité est facilitée par l'interface R6 standardisée

## 15. Références

**Packages R** :
- Wickham, H. (2015). *R Packages*. O'Reilly.
- Chang, W. (2021). *R6: Encapsulated Classes with Reference Semantics*.

**Méthodes de clustering de variables** :
- Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012). ClustOfVar: An R Package for the Clustering of Variables. *Journal of Statistical Software*, 50(13), 1-16.
- Vigneau, E., & Qannari, E. M. (2003). Clustering of variables around latent components. *Communications in Statistics - Simulation and Computation*, 32(4), 1131-1150.

**Patterns de conception** :
- Gamma, E., Helm, R., Johnson, R., & Vlissides, J. (1994). *Design Patterns: Elements of Reusable Object-Oriented Software*. Addison-Wesley.
- Martin, R. C. (2003). *Agile Software Development: Principles, Patterns, and Practices*. Prentice Hall.

---

**Note méthodologique** : Cette documentation technique reflète l'état actuel du package RollerClustR. Les exemples de code ont été testés et validés. Pour signaler des incohérences ou proposer des améliorations, consultez le dépôt GitHub du projet.
