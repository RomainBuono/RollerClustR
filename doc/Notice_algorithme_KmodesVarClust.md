# Implémentation de l'Algorithme K-Modes pour le Clustering de Variables dans la Classe KmodesVarClust

## 1. Fondements Théoriques

### 1.1 Principe Général

L'implémentation proposée réalise un clustering de variables catégorielles selon une approche K-Modes adaptée au contexte du clustering de variables. Contrairement à l'algorithme K-Modes classique qui opère sur les observations (lignes d'une matrice de données), cette implémentation effectue une **transposition conceptuelle** : les variables deviennent les unités à partitionner, et les observations constituent l'espace de caractérisation.

Cette approche s'inscrit dans le paradigme du clustering par réallocation itérative, où l'objectif consiste à minimiser le désaccord (dissimilarité) intra-cluster entre variables présentant des profils modaux similaires sur l'ensemble des observations. L'algorithme étend le K-Means au domaine catégoriel en substituant les moyennes par des modes et la distance euclidienne par une mesure de désaccord simple.

### 1.2 Spécificités du Clustering de Variables Catégorielles

Le clustering de variables catégorielles présente des défis méthodologiques distincts de ceux rencontrés dans le clustering d'observations :

1. **Absence de métrique euclidienne naturelle** : Les variables catégorielles ne disposent pas d'un espace métrique continu permettant l'application directe de mesures de distance traditionnelles.

2. **Hétérogénéité des modalités** : Chaque variable peut présenter un nombre différent de modalités, rendant problématique toute tentative de normalisation uniforme.

3. **Nature transposée de l'espace** : Dans le clustering de variables, la matrice de données $\mathbf{X}_{n \times p}$ est conceptuellement transposée en $\mathbf{X}^T_{p \times n}$, où $p$ variables sont traitées comme observations et $n$ observations originales deviennent les caractéristiques.

### 1.3 Architecture Logicielle

La classe `KmodesVarClust` hérite de la classe abstraite `ClusterAnalysis` et implémente une architecture R6 garantissant l'encapsulation et la maintenabilité. Les attributs privés assurent la persistance des prototypes modaux et de la convergence algorithmique, nécessaires aux prédictions ultérieures sur de nouvelles variables.

## 2. Traduction Algorithmique de la Phase d'Ajustement

### 2.1 Phase de Prétraitement et Transformation

#### Nettoyage et Factorisation des Données

```r
X_clean <- private$cleanDataset(private$FX)
X_factor <- data.frame(lapply(X_clean, function(v) 
  if (is.numeric(v)) cut(v, breaks = 5, include.lowest = TRUE) 
  else as.factor(v)
))
```

La méthode héritée `cleanDataset()` réalise la validation du jeu de données selon la stratégie définie par `na_action`. Une étape de factorisation est ensuite appliquée :

- **Variables qualitatives** : Conversion en facteurs R standardisés
- **Variables quantitatives** : Discrétisation automatique en 5 intervalles équilibrés via la fonction `cut()`

Cette discrétisation préalable permet l'application du K-Modes à des données mixtes en homogénéisant leur représentation catégorielle.

#### Transposition Conceptuelle de l'Espace des Variables

```r
X_vars <- t(X_factor)
X_vars <- as.data.frame(X_vars)
```

L'étape cruciale de transposition transforme la structure des données :

$$\mathbf{X}_{n \times p} \rightarrow \mathbf{X}^T_{p \times n}$$

où :
- **Lignes de $\mathbf{X}^T$** : $p$ variables à clusteriser (désormais traitées comme observations)
- **Colonnes de $\mathbf{X}^T$** : $n$ observations originales (désormais traitées comme caractéristiques catégorielles)

Cette transposition matricielle constitue la transformation fondamentale permettant d'appliquer un algorithme de clustering d'observations (K-Modes) à un problème de clustering de variables.

### 2.2 Mesure de Dissimilarité

#### Désaccord Simple (Simple Matching Dissimilarity)

```r
calc_dissimilarity = function(vec, mode) {
  return(sum(vec != mode, na.rm = TRUE))
}
```

La mesure de dissimilarité entre une variable $V_i$ (représentée par un vecteur de modalités sur $n$ observations) et un mode $M_k$ est définie par :

$$d(V_i, M_k) = \sum_{j=1}^{n} \mathbb{1}_{V_i(j) \neq M_k(j)}$$

où $\mathbb{1}$ désigne la fonction indicatrice. Cette métrique compte le nombre d'observations pour lesquelles la modalité de la variable $V_i$ diffère du prototype modal $M_k$.

**Propriétés de la mesure** :

1. **Positivité** : $d(V_i, M_k) \geq 0$
2. **Symétrie** : $d(V_i, M_k) = d(M_k, V_i)$
3. **Séparabilité** : $d(V_i, V_i) = 0$
4. **Borne supérieure** : $d(V_i, M_k) \leq n$ (désaccord maximal si toutes les observations diffèrent)

Cette mesure s'oppose à la distance euclidienne utilisée dans K-Means et constitue l'adaptation naturelle du critère de similarité au domaine catégoriel.

### 2.3 Algorithme Itératif de K-Modes pour Variables

#### Initialisation des Modes

```r
D <- nrow(X_vars)  # Nombre de variables à clusteriser
initial_modes_indices <- sample(1:D, k)
current_modes <- X_vars[initial_modes_indices, , drop = FALSE]
rownames(current_modes) <- paste0("Mode_", 1:k)
```

L'initialisation consiste à sélectionner aléatoirement $k$ variables parmi les $p$ variables disponibles pour servir de prototypes initiaux. Cette approche, bien que simple, présente une sensibilité aux conditions initiales — une limitation reconnue dans la littérature sur K-Modes.

**Stratégies d'initialisation alternatives** (non implémentées ici mais mentionnées dans la littérature) :

- **Méthode de Huang** : Sélection basée sur la fréquence maximale des modalités
- **Méthode de Cao et al.** : Initialisation par densité pour maximiser la dissimilarité entre modes initiaux
- **Evidence Accumulation** : Agrégation de multiples partitions pour identifier des modes robustes

#### Étape d'Affectation (Assignment Step)

```r
for (i in 1:D) {
  variable_i <- X_vars[i, ]
  distances <- sapply(1:k, function(k_idx) {
    private$calc_dissimilarity(variable_i, current_modes[k_idx, ])
  })
  current_groups[i] <- which.min(distances)
}
```

Chaque variable $V_i$ est assignée au cluster $C_k$ dont le mode $M_k$ minimise le désaccord :

$$C_k^{(t)} = \{V_i \mid k = \arg\min_{j=1,\ldots,K} d(V_i, M_j^{(t-1)})\}$$

où $t$ désigne l'itération courante de l'algorithme. Cette règle d'affectation garantit que chaque variable est assignée au prototype le plus représentatif au sens de la dissimilarité simple.

#### Étape de Mise à Jour des Modes (Update Step)

```r
for (k_idx in 1:k) {
  cluster_vars_indices <- which(current_groups == k_idx)
  
  if (length(cluster_vars_indices) > 0) {
    X_cluster <- X_vars[cluster_vars_indices, , drop = FALSE]
    new_mode_vec <- sapply(X_cluster, find_mode)
    new_modes[k_idx, ] <- new_mode_vec
  }
}
```

Pour chaque cluster $C_k$, le nouveau mode est calculé comme le vecteur de modes colonne par colonne :

$$M_k^{(t)}(j) = \text{mode}\{V_i(j) \mid V_i \in C_k^{(t)}\}$$

où $\text{mode}$ désigne la modalité la plus fréquente pour l'observation $j$ parmi toutes les variables du cluster $C_k$.

**Justification théorique** : Cette mise à jour minimise localement la fonction de coût :

$$J = \sum_{k=1}^{K} \sum_{V_i \in C_k} d(V_i, M_k)$$

En effet, pour chaque observation $j$, la modalité majoritaire minimise le nombre de désaccords avec les variables du cluster.

### 2.4 Critère de Convergence

```r
if (all(current_groups == old_groups)) {
  private$FConverged <- TRUE
  break
}
```

L'algorithme converge lorsque les affectations de variables aux clusters demeurent stables entre deux itérations consécutives :

$$C^{(t)} = C^{(t-1)} \quad \forall k \in \{1, \ldots, K\}$$

Une limite maximale d'itérations (`max_iter`) est imposée pour garantir la terminaison dans les cas où la convergence stricte n'est pas atteinte.

### 2.5 Calcul de l'Inertie Intra-Cluster

```r
total_diss <- sum(sapply(1:D, function(i) {
  k_idx <- private$FGroupes[i]
  private$calc_dissimilarity(X_vars[i, ], private$FModes[k_idx, ])
}))

private$FInertie <- list(
  intra = total_diss,
  converged = private$FConverged,
  iterations = iter
)
```

L'inertie intra-cluster est définie comme le désaccord total entre chaque variable et le mode de son cluster :

$$W_K = \sum_{k=1}^{K} \sum_{V_i \in C_k} d(V_i, M_k)$$

Cette métrique quantifie la compacité des clusters : une valeur faible indique une forte homogénéité modale au sein de chaque cluster.

## 3. Prédiction de Nouvelles Variables

### 3.1 Principe de Projection

La méthode `predict()` permet d'assigner de nouvelles variables à des clusters préexistants sans modifier la structure du clustering initial. Cette fonctionnalité repose sur le calcul du désaccord entre chaque nouvelle variable et les modes établis lors de la phase d'ajustement.

### 3.2 Prétraitement des Nouvelles Données

```r
X_new_vars <- t(sapply(newdata, as.factor))
X_new_vars <- as.data.frame(X_new_vars)
```

Les nouvelles variables subissent les mêmes transformations que lors de l'entraînement :

1. **Factorisation** : Conversion en variables catégorielles
2. **Transposition** : Passage de colonnes (variables) à lignes (observations à clusteriser)

**Contrainte de compatibilité** : Le nombre de colonnes de `X_new_vars` doit correspondre au nombre de colonnes des modes stockés (`FModes`), c'est-à-dire au nombre d'observations du jeu d'entraînement.

### 3.3 Calcul des Désaccords et Assignation

```r
for (i in 1:nrow(X_new_vars)) {
  variable_i <- X_new_vars[i, ]
  
  distances <- sapply(1:private$FNbGroupes, function(k_idx) {
    mode_k <- private$FModes[k_idx, ]
    private$calc_dissimilarity(variable_i, mode_k)
  })
  
  predictions[i] <- which.min(distances)
}
```

Chaque nouvelle variable $V_{\text{new}}$ est assignée au cluster dont le mode minimise le désaccord :

$$\hat{C}(V_{\text{new}}) = \arg\min_{k=1,\ldots,K} d(V_{\text{new}}, M_k)$$

Cette règle garantit la cohérence avec le critère d'optimisation utilisé durant la phase d'ajustement.

### 3.4 Gestion des Cas Limites

```r
if (all(is.na(distances))) {
  predictions[i] <- NA_integer_
} else {
  predictions[i] <- which.min(distances)
}
```

Lorsque toutes les dissimilarités sont indéfinies (cas de données entièrement manquantes ou incompatibles), la prédiction retourne `NA` pour préserver l'intégrité des résultats.

## 4. Fondements Mathématiques de la Méthode

### 4.1 Fonction Objectif du K-Modes

L'algorithme K-Modes pour clustering de variables minimise la fonction de coût suivante :

$$\min_{C_1, \ldots, C_K} \sum_{k=1}^{K} \sum_{V_i \in C_k} d(V_i, M_k)$$

sous la contrainte que les $C_k$ forment une partition de l'ensemble des variables :

$$C_k \cap C_j = \emptyset \quad \forall k \neq j, \quad \bigcup_{k=1}^{K} C_k = \{V_1, \ldots, V_p\}$$

### 4.2 Optimalité Locale de la Mise à Jour des Modes

La stratégie de mise à jour des modes par modalité majoritaire est optimale au sens des moindres désaccords. Pour une observation $j$ donnée, le choix de la modalité $m_j$ qui minimise le désaccord avec les variables du cluster $C_k$ est :

$$m_j^* = \arg\min_{m \in \mathcal{M}_j} \sum_{V_i \in C_k} \mathbb{1}_{V_i(j) \neq m}$$

où $\mathcal{M}_j$ désigne l'ensemble des modalités possibles pour l'observation $j$.

Cette minimisation est résolue par le mode (modalité majoritaire) :

$$m_j^* = \text{mode}\{V_i(j) \mid V_i \in C_k\}$$

### 4.3 Convergence de l'Algorithme

L'algorithme K-Modes converge vers un minimum local de la fonction objectif. La décroissance monotone du coût est garantie par la construction :

1. **Étape d'affectation** : Réaffecte chaque variable au cluster minimisant le désaccord → diminution ou stagnation du coût
2. **Étape de mise à jour** : Recalcule les modes optimaux pour chaque cluster → diminution ou stagnation du coût

Cependant, comme pour K-Means, la convergence vers le minimum global n'est pas garantie, et le résultat dépend fortement de l'initialisation.

### 4.4 Transposition et Dualité Observations-Variables

La transposition opérée dans cet algorithme illustre une dualité fondamentale en analyse de données :

$$\text{Clustering d'observations sur } p \text{ variables} \leftrightarrow \text{Clustering de } p \text{ variables sur } n \text{ observations}$$

Cette dualité permet d'appliquer des algorithmes conçus pour le clustering d'observations au problème structurellement différent du clustering de variables. Elle repose sur l'invariance de la mesure de désaccord par transposition :

$$\sum_{i=1}^{n} \mathbb{1}_{V_a(i) \neq V_b(i)} = \sum_{j=1}^{p} \mathbb{1}_{O_a(j) \neq O_b(j)}$$

où $V_a, V_b$ désignent des variables et $O_a, O_b$ des observations.

## 5. Complexité Algorithmique

### 5.1 Phase d'Apprentissage

Pour $p$ variables, $n$ observations, $K$ clusters et $T$ itérations :

1. **Transposition initiale** : $O(np)$
2. **Étape d'affectation** : $O(TKnp)$ — calcul de $K$ désaccords de taille $n$ pour chacune des $p$ variables
3. **Étape de mise à jour** : $O(Tnp)$ — calcul du mode pour chaque observation sur les variables de chaque cluster

La complexité globale est **$O(TKnp)$**, dominée par l'étape d'affectation.

### 5.2 Phase de Prédiction

Pour $p'$ nouvelles variables :

- **Calcul des désaccords** : $O(Knp')$
- **Assignation** : $O(Kp')$

La complexité de prédiction est **$O(Knp')$**, linéaire en le nombre de nouvelles variables.

### 5.3 Comparaison avec K-Means pour Variables Numériques

| Algorithme | Complexité itération | Type de données | Mesure |
|------------|---------------------|-----------------|--------|
| K-Means    | $O(Knp)$           | Quantitatives   | Distance euclidienne |
| K-Modes    | $O(Knp)$           | Catégorielles   | Désaccord simple |

Les deux algorithmes présentent une complexité asymptotique identique, mais K-Modes évite les opérations arithmétiques coûteuses (calculs de moyennes, distances euclidiennes) au profit de comparaisons logiques simples.

## 6. Limitations et Extensions Possibles

### 6.1 Limitations Actuelles

1. **Sensibilité à l'initialisation** : Comme K-Means, K-Modes est sensible au choix des modes initiaux. Des initialisations sous-optimales peuvent conduire à des minima locaux de faible qualité.

2. **Discrétisation arbitraire des variables quantitatives** : La transformation automatique des variables numériques en 5 catégories via `cut()` peut entraîner une perte d'information substantielle.

3. **Absence de pondération des observations** : Toutes les observations contribuent équitablement au calcul du mode, ce qui peut être problématique en présence de données déséquilibrées ou bruitées.

4. **Traitement uniforme des modalités** : La mesure de désaccord simple traite toutes les modalités comme équidistantes, ignorant toute structure ordinale potentielle.

### 6.2 Perspectives d'Extension

1. **Initialisations robustes** : Implémentation des méthodes de Huang ou Cao pour réduire la dépendance à l'initialisation aléatoire.

2. **Mesures de dissimilarité enrichies** : Intégration de la mesure proposée par Ng et al., qui pondère les modalités par leur fréquence relative au sein des clusters.

3. **K-Prototypes pour clustering de variables** : Extension permettant de traiter conjointement variables quantitatives et catégorielles sans discrétisation préalable, en définissant une dissimilarité mixte.

4. **Sélection automatique de $K$** : Intégration de critères d'optimalité (silhouette adaptée aux données catégorielles, critère de Calinski-Harabasz modifié) pour déterminer le nombre optimal de clusters.

5. **Clustering flou (Fuzzy K-Modes)** : Autoriser des appartenances partielles des variables aux clusters, capturant ainsi l'ambiguïté inhérente aux données catégorielles.

## 7. Cas d'Usage et Applications

### 7.1 Réduction de Dimensionnalité pour Données Catégorielles

Dans des contextes de haute dimensionnalité avec variables qualitatives (enquêtes sociologiques, données médicales avec symptômes catégoriels), cette approche permet d'identifier des groupes de variables redondantes et de sélectionner des représentants pour chaque cluster.

### 7.2 Feature Engineering pour Apprentissage Supervisé

Le clustering de variables catégorielles facilite la construction de méta-variables :

- **Regroupement de modalités** : Les variables d'un même cluster peuvent être agrégées en une variable synthétique via la modalité conjointe la plus fréquente.
- **Détection de colinéarité catégorielle** : Identification de variables fortement associées au sens du chi-deux ou du V de Cramér.

### 7.3 Analyse Exploratoire de Questionnaires

Dans l'analyse de questionnaires à échelles nominales ou ordinales, K-Modes permet de découvrir la structure latente des items :

- Identification de dimensions thématiques
- Détection de questions redondantes
- Simplification de batteries d'items pour raccourcir les questionnaires

### 7.4 Pré-traitement pour Algorithmes de Sélection de Variables

En amont d'algorithmes supervisés, le clustering de variables peut servir à :

- Réduire la multicolinéarité en ne conservant qu'un représentant par cluster
- Accélérer les procédures de sélection séquentielle (forward/backward) en limitant l'espace de recherche

## 8. Comparaison avec les Approches Alternatives

### 8.1 Distinction avec ClustOfVar

| Aspect | ClustOfVar (CAH) | KmodesVarClust (K-Modes) |
|--------|------------------|--------------------------|
| **Type de données** | Quantitatives | Catégorielles/Mixtes |
| **Méthode** | Hiérarchique (dendrogramme) | Partitionnement itératif |
| **Mesure** | $1 - \|\rho\|$ (corrélation) | Désaccord simple |
| **Détermination de $K$** | Coupure d'arbre | Spécification a priori |
| **Complexité** | $O(p^2 \log p)$ | $O(TKnp)$ |
| **Réaffectabilité** | Non (structure fixe) | Oui (réallocation possible) |

### 8.2 Distinction avec VarClustAdvanced

`VarClustAdvanced` propose une approche plus sophistiquée avec 4 algorithmes (PAM, hiérarchique, spectral, PCAmix) et gestion avancée des métriques de qualité. `KmodesVarClust` se concentre sur la simplicité et l'efficacité pour données catégorielles pures, avec une implémentation légère adaptée aux grands ensembles de variables.

## 9. Références Théoriques

L'implémentation s'inspire des travaux fondateurs et extensions suivantes :

### Références Principales

- **Huang, Z. (1997).** *Clustering large data sets with mixed numeric and categorical values*. Proceedings of the First Pacific Asia Knowledge Discovery and Data Mining Conference, Singapore: World Scientific, pp. 21-34.  
  → Introduction de l'algorithme K-Modes et K-Prototypes.

- **Huang, Z. (1998).** *Extensions to the k-Means Algorithm for Clustering Large Data Sets with Categorical Values*. Data Mining and Knowledge Discovery, 2(3), 283-304.  
  → Formalisation théorique et analyse de convergence du K-Modes.

- **Huang, Z., & Ng, M. K. (1999).** *A fuzzy k-modes algorithm for clustering categorical data*. IEEE Transactions on Fuzzy Systems, 7(4), 446-452.  
  → Extension floue de K-Modes permettant des appartenances partielles.

### Extensions et Améliorations

- **Cao, F., Liang, J., & Bai, L. (2009).** *A new initialization method for categorical data clustering*. Expert Systems with Applications, 36(7), 10223-10228.  
  → Méthode d'initialisation basée sur la densité pour améliorer la convergence.

- **Ng, M. K., Li, M. J., Huang, J. Z., & He, Z. (2007).** *On the impact of dissimilarity measure in k-modes clustering algorithm*. IEEE Transactions on Pattern Analysis and Machine Intelligence, 29(3), 503-507.  
  → Proposition d'une mesure de dissimilarité améliorée basée sur les fréquences relatives.

- **San, O. M., Huynh, V. N., & Nakamori, Y. (2004).** *An alternative extension of the k-means algorithm for clustering categorical data*. International Journal of Applied Mathematics and Computer Science, 14(2), 241-247.  
  → Variante k-populations utilisant des distributions de probabilité comme prototypes.

### Travaux Complémentaires

- **Ahmad, A., & Khan, S. S. (2019).** *Survey of state-of-the-art mixed data clustering algorithms*. IEEE Access, 7, 31883-31902.  
  → Revue exhaustive des algorithmes de clustering pour données mixtes.

- **Chicco, D., & Jurman, G. (2023).** *Categorical data clustering: 25 years beyond K-modes*. Expert Systems with Applications, 244, 122929.  
  → Synthèse récente des avancées méthodologiques depuis l'introduction de K-Modes.

### Contexte de Clustering de Variables

- **Vigneau, E., & Qannari, E. M. (2003).** *Clustering of variables around latent components*. Communications in Statistics - Simulation and Computation, 32(4), 1131-1150.  
  → Fondements théoriques du clustering de variables par composantes latentes.

## 10. Contribution Distinctive de cette Implémentation

La contribution spécifique de cette implémentation réside dans :

1. **Adaptation du K-Modes au clustering de variables** : Application d'un algorithme originellement conçu pour des observations à un contexte de clustering de variables via transposition matricielle.

2. **Gestion intégrée des données mixtes** : Discrétisation automatique des variables quantitatives permettant un traitement unifié dans le paradigme catégoriel.

3. **Cohérence architecturale** : Héritage de la classe `ClusterAnalysis` garantissant une interface unifiée avec les autres méthodes de clustering du package (CAH, K-Means, K-Prototypes, ClustOfVar).

4. **Méthode de prédiction pour nouvelles variables** : Extension permettant l'assignation incrémentale de nouvelles variables sans réentraînement complet, fonctionnalité peu documentée dans la littérature sur K-Modes pour variables.

Cette implémentation offre ainsi une solution efficace et légère pour le clustering de variables catégorielles, complémentaire aux approches par corrélation (ClustOfVar) et par méthodes avancées (VarClustAdvanced).