# Implémentation de l'Algorithme TandemVarClust dans la Classe TandemVarClust

## 1. Fondements Théoriques

### 1.1 Principe Général

L'implémentation proposée réalise un clustering hiérarchique de **modalités** (et non directement de variables) selon une approche Tandem combinant :

1. **AFDM** (Analyse Factorielle de Données Mixtes) pour la réduction de dimensionnalité
2. **CAH** (Classification Ascendante Hiérarchique) sur les coordonnées factorielles

Cette méthode se distingue fondamentalement des approches classiques de clustering de variables car elle opère au niveau des **modalités** : chaque catégorie d'une variable qualitative ou chaque intervalle discrétisé d'une variable quantitative constitue une unité à classer. Ce changement d'échelle permet de capturer des structures plus fines et de traiter naturellement des données mixtes (quantitatives et qualitatives).

### 1.2 Architecture Logicielle

La classe `TandemVarClust` hérite de la classe abstraite `ClusterAnalysis` et implémente une architecture R6 garantissant l'encapsulation des données et la modularité des traitements. Les attributs privés assurent la persistance des résultats intermédiaires nécessaires aux analyses ultérieures et à l'assignation de variables illustratives.

### 1.3 Positionnement Méthodologique

Le clustering Tandem (aussi appelé "two-step clustering") consiste en une approche séquentielle où l'analyse factorielle précède le clustering proprement dit. Bien que critiquée par certains auteurs (Arabie & Hubert, 1994 ; De Soete & Carroll, 1994) pour des raisons d'optimalité conjointe, cette approche présente l'avantage de la simplicité algorithmique et de l'interprétabilité : les axes factoriels retenus pour le clustering peuvent être examinés indépendamment.

L'utilisation de l'AFDM comme première étape permet de traiter des tableaux mixtes (variables quantitatives et qualitatives) dans un cadre unifié, ce qui constitue un avantage décisif dans les applications pratiques où la mixité des données est la règle plutôt que l'exception.

## 2. Traduction Algorithmique

### 2.1 Phase de Prétraitement : Préparation des Données Mixtes

#### Identification des Types de Variables

```r
prepare_mixed_data = function(X) {
  var_types <- sapply(X, function(col) {
    if (is.factor(col) || is.character(col)) "categorical"
    else if (is.numeric(col)) "numeric"
    else "unknown"
  })
  
  private$FCatVarNames <- names(var_types)[var_types == "categorical"]
  private$FNumVarNames <- names(var_types)[var_types == "numeric"]
  private$FHasCategoricalVars <- length(private$FCatVarNames) > 0
  private$FHasNumericVars <- length(private$FNumVarNames) > 0
}
```

La méthode débute par une inspection du type de chaque variable. Cette distinction est cruciale car le traitement diffère radicalement selon la nature des variables :

- **Variables qualitatives** : Codées par tableau disjonctif complet (indicatrices)
- **Variables quantitatives** : Discrétisées puis codées par indicatrices

#### Construction du Tableau Disjonctif : Variables Qualitatives

Pour chaque variable qualitative $V_j$ possédant $m_j$ modalités, on crée $m_j$ colonnes indicatrices. Si la variable $V$ possède les modalités $\{A, B, C\}$, on construit trois colonnes :

$$
\begin{pmatrix}
V.A \\
V.B \\
V.C
\end{pmatrix}
=
\begin{pmatrix}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 0 & 1 \\
1 & 0 & 0
\end{pmatrix}
$$

où l'élément $(i,k)$ vaut 1 si l'observation $i$ possède la modalité $k$, et 0 sinon.

```r
for (var_name in private$FCatVarNames) {
  col <- X[[var_name]]
  if (!is.factor(col)) col <- as.factor(col)
  
  levels_var <- levels(col)
  for (level in levels_var) {
    indicator <- as.integer(col == level)
    col_name <- paste0(var_name, ".", level)
    disjunctive_list[[col_name]] <- indicator
  }
}
```

Cette transformation est au cœur de l'Analyse des Correspondances Multiples (ACM), composante de l'AFDM pour les variables qualitatives.

#### Discrétisation et Codage : Variables Quantitatives

Les variables quantitatives ne peuvent être directement intégrées dans un tableau disjonctif. L'approche adoptée consiste à :

1. **Discrétiser** la variable en $n_{\text{bins}}$ intervalles (par défaut 5)
2. **Coder** chaque intervalle par une indicatrice

```r
for (var_name in private$FNumVarNames) {
  col <- X[[var_name]]
  
  # Standardisation optionnelle
  if (private$FScale) {
    col <- scale(col)[, 1]
  }
  
  # Discrétisation par quantiles
  breaks <- quantile(col, probs = seq(0, 1, length.out = private$FNBins + 1), 
                     na.rm = TRUE)
  breaks <- unique(breaks)
  
  discretized <- cut(col, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  # Création des indicatrices
  n_bins_actual <- max(discretized, na.rm = TRUE)
  for (bin in 1:n_bins_actual) {
    indicator <- as.integer(discretized == bin)
    col_name <- paste0(var_name, ".bin", bin)
    disjunctive_list[[col_name]] <- indicator
  }
}
```

**Justification de la discrétisation** : Cette étape transforme une variable continue en variable ordinale, permettant son intégration dans le cadre de l'ACM. Les quantiles sont privilégiés pour garantir des effectifs équilibrés dans chaque intervalle, optimisant ainsi la qualité de l'analyse factorielle ultérieure.

**Nombre de bins** : Le paramètre `n_bins` (par défaut 5) contrôle le niveau de granularité :
- **Peu de bins (3-5)** : Perte d'information mais robustesse accrue
- **Beaucoup de bins (10-20)** : Conservation de l'information mais risque de sur-ajustement

#### Structure Finale du Tableau Disjonctif

À l'issue de cette étape, on dispose d'une matrice $\mathbf{Z}$ de dimensions $n \times p^*$ où :

$$p^* = \sum_{j \in \text{qual}} m_j + \sum_{j \in \text{quant}} n_{\text{bins},j}$$

Cette matrice vérifie la propriété de **partition** : pour chaque variable d'origine, exactement une modalité est active par observation.

### 2.2 Phase d'Analyse Factorielle : AFDM

L'AFDM réalise une analyse factorielle sur le tableau disjonctif $\mathbf{Z}$, combinant les principes de l'ACP (pour les variables quantitatives originelles) et de l'ACM (pour les variables qualitatives).

#### Pondération et Centrage

```r
perform_factorial_analysis = function() {
  Z <- private$FDisjunctiveTable
  n <- nrow(Z)
  p <- ncol(Z)
  
  # Pondération des colonnes (inverse des fréquences marginales)
  col_margins <- colSums(Z) / n
  col_weights <- 1 / col_margins
  col_weights[!is.finite(col_weights)] <- 0
  
  # Pondération des lignes (uniforme)
  row_weights <- rep(1/n, n)
  
  # Centrage
  Z_centered <- sweep(Z, 2, col_margins, "-")
}
```

**Justification de la pondération** : Les colonnes du tableau disjonctif sont pondérées par l'inverse de leur fréquence marginale. Cette pondération compense le déséquilibre des effectifs et garantit que :

1. Les modalités rares ne dominent pas l'analyse
2. Toutes les modalités contribuent équitablement à l'inertie totale

Mathématiquement, pour une modalité $j$ de fréquence marginale $p_j = \frac{n_j}{n}$, le poids est :

$$w_j = \frac{1}{p_j} = \frac{n}{n_j}$$

#### Décomposition en Valeurs Singulières (SVD)

```r
D_col_sqrt <- diag(sqrt(col_weights))
D_row_sqrt <- diag(sqrt(row_weights))

Z_weighted <- D_row_sqrt %*% as.matrix(Z_centered) %*% D_col_sqrt

svd_res <- svd(Z_weighted)

private$FFactorialCoords <- D_col_sqrt %*% svd_res$v
rownames(private$FFactorialCoords) <- colnames(Z)

eigenvalues <- svd_res$d^2
private$FACMVariance <- 100 * eigenvalues / sum(eigenvalues)
```

La SVD décompose la matrice pondérée $\mathbf{Z}_w$ selon :

$$\mathbf{Z}_w = \mathbf{U} \mathbf{D} \mathbf{V}^T$$

où :
- $\mathbf{U}$ : vecteurs singuliers à gauche (coordonnées des lignes)
- $\mathbf{D}$ : matrice diagonale des valeurs singulières
- $\mathbf{V}$ : vecteurs singuliers à droite (coordonnées des colonnes/modalités)

**Coordonnées factorielles** : Les coordonnées des modalités dans l'espace factoriel sont obtenues par :

$$\mathbf{F} = \mathbf{D}_{\text{col}}^{1/2} \mathbf{V}$$

Ces coordonnées représentent les modalités dans un espace de dimension réduite où les axes (facteurs) capturent l'essentiel de la variabilité du tableau disjonctif.

**Inertie et variance expliquée** : Les valeurs propres $\lambda_k = d_k^2$ mesurent l'inertie (variance) portée par chaque axe factoriel. La proportion de variance expliquée par l'axe $k$ est :

$$\frac{\lambda_k}{\sum_{k'} \lambda_{k'}} \times 100\%$$

### 2.3 Phase de Clustering : CAH sur Coordonnées Factorielles

#### Sélection des Facteurs

```r
perform_clustering = function() {
  n_factors_to_use <- if (is.null(private$FNFactors)) {
    private$FNFactorsTotal
  } else {
    min(private$FNFactors, private$FNFactorsTotal)
  }
  
  coords <- private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
}
```

**Choix du nombre de facteurs** : L'utilisateur peut spécifier le nombre de facteurs à retenir via le paramètre `n_factors`. Par défaut, tous les facteurs sont conservés. En pratique :

- **Peu de facteurs (2-5)** : Réduction importante de dimensionnalité, robustesse accrue, mais perte d'information
- **Tous les facteurs** : Conservation totale de l'information, mais risque de bruit

Le critère usuel consiste à retenir les facteurs expliquant au moins 5-10% de la variance, ou un nombre de facteurs tel que la variance cumulée atteigne 70-90%.

#### Calcul de la Matrice de Distances

```r
dist_matrix <- dist(coords, method = "euclidean")
```

La distance euclidienne entre deux modalités $i$ et $j$ dans l'espace factoriel de dimension $q$ est :

$$d(i,j) = \sqrt{\sum_{k=1}^{q} (f_{ik} - f_{jk})^2}$$

où $f_{ik}$ désigne la coordonnée de la modalité $i$ sur l'axe factoriel $k$.

**Justification** : La distance euclidienne dans l'espace factoriel préserve les distances du Chi² dans l'espace original (grâce à la pondération appliquée lors de la SVD), tout en opérant dans un espace de dimension réduite.

#### Classification Ascendante Hiérarchique

```r
private$FHclustTree <- hclust(dist_matrix, method = private$FMethodCAH)
private$FGroupes <- cutree(private$FHclustTree, k = private$FNbGroupes)
```

La CAH agrège itérativement les modalités en clusters, formant une hiérarchie représentée par un dendrogramme. À chaque étape, les deux clusters $C_i$ et $C_j$ minimisant un critère d'agrégation sont fusionnés.

**Critère de Ward** (par défaut) : Le critère de Ward minimise l'accroissement de l'inertie intra-classe lors de la fusion. Pour deux clusters $C_i$ et $C_j$ de centres respectifs $\bar{\mathbf{f}}_i$ et $\bar{\mathbf{f}}_j$ et d'effectifs $n_i$ et $n_j$, le critère est :

$$\Delta(C_i, C_j) = \frac{n_i n_j}{n_i + n_j} \|\bar{\mathbf{f}}_i - \bar{\mathbf{f}}_j\|^2$$

**Coupe de l'arbre** : La partition finale en $K$ clusters est obtenue en coupant le dendrogramme au niveau approprié.

#### Calcul des Centres de Clusters

```r
cluster_centers <- matrix(NA, nrow = private$FNbGroupes, ncol = n_factors_to_use)

for (k in 1:private$FNbGroupes) {
  modalities_in_cluster <- which(private$FGroupes == k)
  
  if (length(modalities_in_cluster) > 0) {
    cluster_centers[k, ] <- colMeans(coords[modalities_in_cluster, , drop = FALSE])
  }
}

private$FClusterCenters <- cluster_centers
```

Pour chaque cluster $k$, le centre (barycentre) est calculé comme la moyenne des coordonnées factorielles des modalités appartenant à ce cluster :

$$\bar{\mathbf{f}}_k = \frac{1}{n_k} \sum_{i \in C_k} \mathbf{f}_i$$

où $n_k$ est le nombre de modalités dans le cluster $k$ et $\mathbf{f}_i$ les coordonnées factorielles de la modalité $i$.

**Importance** : Ces centres de clusters sont essentiels pour la méthode `predict_variable()`, qui projette de nouvelles variables dans l'espace factoriel et calcule leur distance aux centres pour déterminer l'assignation.

### 2.4 Phase d'Assignation : Méthode de Dice pour les Observations

#### Principe de l'Indice de Dice

L'indice de Dice mesure la similarité entre deux ensembles. Pour une observation $i$ (caractérisée par l'ensemble de ses modalités actives $M_i$) et un cluster $k$ (ensemble de modalités $C_k$), l'indice est :

$$\text{Dice}(i, k) = \frac{2 |M_i \cap C_k|}{|M_i| + |C_k|}$$

où :
- $M_i$ : ensemble des modalités actives pour l'observation $i$ (colonnes du tableau disjonctif valant 1)
- $C_k$ : ensemble des modalités appartenant au cluster $k$
- $|M_i \cap C_k|$ : nombre de modalités communes
- $|M_i|$ et $|C_k|$ : cardinalités respectives

**Interprétation** : L'indice de Dice varie entre 0 (aucune modalité commune) et 1 (identité parfaite). Il s'interprète comme une proportion de chevauchement harmonique entre l'observation et le cluster.

#### Implémentation

```r
assign_observations_to_clusters = function() {
  Z <- private$FDisjunctiveTable
  n <- nrow(Z)
  groupes <- private$FGroupes
  K <- private$FNbGroupes
  
  obs_clusters <- integer(n)
  dice_scores <- matrix(0, nrow = n, ncol = K)
  
  cluster_modalities <- lapply(1:K, function(k) {
    which(groupes == k)
  })
  
  for (i in 1:n) {
    modalities_i <- which(Z[i, ] == 1)
    n_modalities_i <- length(modalities_i)
    
    for (k in 1:K) {
      modalities_k <- cluster_modalities[[k]]
      n_modalities_k <- length(modalities_k)
      
      intersection <- length(intersect(modalities_i, modalities_k))
      
      dice_scores[i, k] <- (2 * intersection) / (n_modalities_i + n_modalities_k)
    }
    
    obs_clusters[i] <- which.max(dice_scores[i, ])
  }
  
  private$FObsClusters <- obs_clusters
  private$FDiceScores <- dice_scores
}
```

**Algorithme** :
1. Pour chaque observation $i$, identifier ses modalités actives $M_i$
2. Pour chaque cluster $k$, calculer l'indice de Dice
3. Assigner l'observation au cluster maximisant l'indice

**Justification** : L'indice de Dice est préféré à l'indice de Jaccard car il accorde un poids double à l'intersection, ce qui favorise les assignations avec un fort chevauchement. Il est aussi préféré aux mesures non normalisées (simple intersection) car il tient compte des tailles relatives des ensembles.

## 3. Méthode `predict_variable()` : Assignation de Variables Illustratives

### 3.1 Principe Général

La méthode `predict_variable()` permet de projeter de nouvelles variables (dites **illustratives** ou **supplémentaires**) dans l'espace factoriel construit lors de l'analyse, sans recalculer l'AFDM. L'objectif est de déterminer à quel cluster de modalités ces nouvelles variables sont le plus étroitement associées.

**Différence fondamentale avec `predict()` des autres algorithmes** :
- **VAR_CAH et VAR_KMEANS** : Assignent directement les variables aux clusters par corrélation
- **TandemVarClust** : Projette d'abord les modalités dans l'espace factoriel, puis calcule les distances aux centres de clusters

Cette approche exploite pleinement la structure factorielle établie lors du clustering et garantit la cohérence méthodologique avec l'AFDM.

### 3.2 Architecture de la Méthode

#### Vue d'Ensemble

```r
predict_variable = function(new_vars) {
  # 1. Validation des entrées
  # 2. Pour chaque nouvelle variable :
  #    a. Traiter selon son type (catégorielle vs numérique)
  #    b. Construire tableau disjonctif de la variable
  #    c. Projeter les modalités dans l'espace AFDM
  #    d. Calculer distances aux centres de clusters
  #    e. Assigner au cluster le plus proche
  #    f. Calculer métriques d'association (Chi², Cramér's V, Dice)
  # 3. Retourner résultats enrichis
}
```

#### Étapes Détaillées

##### Étape 1 : Validation des Entrées

```r
if (is.null(private$FGroupes)) {
  stop("The model is not fitted. Please call $fit() first.")
}

if (!is.data.frame(new_vars)) {
  if (is.vector(new_vars)) {
    new_vars <- data.frame(NewVar1 = new_vars)
  } else {
    stop("new_vars must be a data.frame or a vector")
  }
}

if (nrow(new_vars) != nrow(private$FX)) {
  stop("new_vars must have the same number of observations as training data")
}
```

**Points de contrôle** :
- Le modèle doit être ajusté (`fit()` déjà appelé)
- Les nouvelles variables doivent être au format `data.frame` ou vecteur
- Le nombre d'observations doit correspondre aux données d'entraînement

##### Étape 2a : Traitement des Variables Catégorielles

```r
process_categorical_variable = function(var_name, var_col) {
  if (!is.factor(var_col)) {
    var_col <- as.factor(var_col)
  }
  
  levels_var <- levels(var_col)
  disjunctive_list <- list()
  
  for (level in levels_var) {
    indicator <- as.integer(var_col == level)
    col_name <- paste0(var_name, ".", level)
    disjunctive_list[[col_name]] <- indicator
  }
  
  Z_new <- as.data.frame(disjunctive_list)
  return(Z_new)
}
```

**Transformation** : Chaque modalité de la variable catégorielle devient une colonne indicatrice (0/1), exactement comme dans le tableau disjonctif d'entraînement.

##### Étape 2b : Traitement des Variables Numériques

```r
process_numeric_variable = function(var_name, var_col) {
  if (private$FScale) {
    var_col <- scale(var_col)[, 1]
  }
  
  breaks <- quantile(var_col, probs = seq(0, 1, length.out = private$FNBins + 1),
                     na.rm = TRUE)
  breaks <- unique(breaks)
  
  discretized <- cut(var_col, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  disjunctive_list <- list()
  n_bins_actual <- max(discretized, na.rm = TRUE)
  
  for (bin in 1:n_bins_actual) {
    indicator <- as.integer(discretized == bin)
    col_name <- paste0(var_name, ".bin", bin)
    disjunctive_list[[col_name]] <- indicator
  }
  
  Z_new <- as.data.frame(disjunctive_list)
  return(Z_new)
}
```

**Transformation** : La variable numérique est discrétisée en bins (par quantiles) puis chaque bin est codé comme une modalité.

**Note importante** : La discrétisation utilise les quantiles de la **nouvelle variable**, pas ceux des variables d'entraînement. Ceci peut introduire une légère incohérence si les distributions diffèrent substantiellement, mais garantit que chaque bin contient environ le même nombre d'observations dans la nouvelle variable.

##### Étape 2c : Projection dans l'Espace AFDM

```r
project_variable_to_factorial_space = function(Z_new) {
  n <- nrow(Z_new)
  
  # Fréquences marginales de la nouvelle variable
  new_col_margins <- colSums(Z_new) / n
  
  # Centrage par rapport aux marges d'entraînement (approximation)
  # Note: Utilise les marges d'entraînement pour cohérence
  Z_new_centered <- sweep(as.matrix(Z_new), 2, new_col_margins, "-")
  
  # Pondération par les poids d'entraînement
  D_col_sqrt <- private$FDColSqrt
  
  # Projection : coordonnées factorielles = Z_centered %*% D_col_sqrt %*% V
  # où V est la matrice des vecteurs singuliers à droite
  new_coords <- Z_new_centered %*% D_col_sqrt %*% private$FSVDv
  
  return(new_coords)
}
```

**Formule mathématique** : La projection d'une nouvelle modalité dans l'espace factoriel suit la même transformation que lors de l'AFDM :

$$\mathbf{f}_{\text{new}} = (\mathbf{z}_{\text{new}} - \bar{\mathbf{z}}) \mathbf{D}_{\text{col}}^{1/2} \mathbf{V}$$

où :
- $\mathbf{z}_{\text{new}}$ : vecteur indicateur de la nouvelle modalité
- $\bar{\mathbf{z}}$ : vecteur des fréquences marginales (de la variable illustrative)
- $\mathbf{D}_{\text{col}}^{1/2}$ : matrice diagonale des poids (racine carrée)
- $\mathbf{V}$ : matrice des vecteurs singuliers à droite (de la SVD d'entraînement)

**Cohérence** : Cette formule garantit que les nouvelles modalités sont positionnées de manière cohérente avec les modalités d'entraînement dans l'espace factoriel.

##### Étape 2d : Calcul des Distances aux Centres de Clusters

```r
calculate_distances_to_centers = function(new_coords) {
  n_factors <- ncol(private$FClusterCenters)
  new_coords_subset <- new_coords[, 1:n_factors, drop = FALSE]
  
  K <- private$FNbGroupes
  distances <- numeric(K)
  
  for (k in 1:K) {
    center_k <- private$FClusterCenters[k, ]
    
    # Distance euclidienne moyenne entre modalités de la variable et centre du cluster
    dists <- apply(new_coords_subset, 1, function(coord) {
      sqrt(sum((coord - center_k)^2))
    })
    
    distances[k] <- mean(dists)
  }
  
  return(distances)
}
```

**Logique** :
1. Pour chaque modalité de la nouvelle variable, on obtient ses coordonnées factorielles
2. On calcule la distance euclidienne entre chaque modalité et le centre de chaque cluster
3. On moyenne ces distances pour obtenir une distance représentative de la variable au cluster

**Distance euclidienne** : Entre une modalité projetée $\mathbf{f}_m$ et le centre du cluster $k$, la distance est :

$$d(m, k) = \|\mathbf{f}_m - \bar{\mathbf{f}}_k\| = \sqrt{\sum_{j=1}^{q} (f_{mj} - \bar{f}_{kj})^2}$$

**Distance agrégée** : Pour une variable possédant $M$ modalités, la distance au cluster $k$ est :

$$D(\text{var}, k) = \frac{1}{M} \sum_{m=1}^{M} d(m, k)$$

##### Étape 2e : Assignation au Cluster le Plus Proche

```r
predicted_cluster <- which.min(distances)
```

La variable est assignée au cluster dont le centre est le plus proche (distance moyenne minimale).

**Justification** : Cette règle d'assignation est cohérente avec la logique de la CAH et du critère de Ward, qui minimisent l'inertie intra-classe (somme des carrés des distances aux centres).

##### Étape 2f : Calcul des Métriques d'Association

Après avoir assigné chaque observation au cluster (via la méthode de Dice), on construit une **table de contingence** croisant la nouvelle variable illustrative avec les clusters :

```r
obs_clusters <- private$FObsClusters
contingency <- table(var_col, obs_clusters)
```

**Test du Chi²** :

```r
chi2_test <- chisq.test(contingency)
```

Le test du Chi² évalue l'indépendance entre la variable illustrative et les clusters. L'hypothèse nulle est que la répartition des modalités de la variable est identique dans tous les clusters.

**V de Cramér** :

```r
calculate_cramers_v = function(chi2_stat, n, r, c) {
  V <- sqrt(chi2_stat / (n * min(r - 1, c - 1)))
  return(V)
}
```

Le V de Cramér normalise la statistique du Chi² pour fournir une mesure d'association comprise entre 0 et 1 :

$$V = \sqrt{\frac{\chi^2}{n \times \min(r-1, c-1)}}$$

où $r$ et $c$ sont les nombres de lignes et colonnes de la table de contingence.

**Scores de Dice par modalité** :

Pour chaque modalité de la variable illustrative, on calcule l'indice de Dice avec chaque cluster :

```r
calculate_dice_scores_for_modality = function(modality_vector, cluster_assignments) {
  K <- max(cluster_assignments)
  dice_scores <- numeric(K)
  
  for (k in 1:K) {
    obs_in_modality <- which(modality_vector == 1)
    obs_in_cluster <- which(cluster_assignments == k)
    
    intersection <- length(intersect(obs_in_modality, obs_in_cluster))
    
    dice_scores[k] <- (2 * intersection) / (length(obs_in_modality) + length(obs_in_cluster))
  }
  
  return(dice_scores)
}
```

**Interprétation** : Un score de Dice élevé entre une modalité et un cluster indique que les observations possédant cette modalité sont concentrées dans ce cluster.

### 3.3 Structure du Résultat

La méthode `predict_variable()` retourne une liste structurée pour chaque variable illustrative :

```r
list(
  variable_name = list(
    cluster = predicted_cluster,           # Cluster assigné (1 à K)
    n_modalities = n_modalities,          # Nombre de modalités
    modality_clusters = modality_clusters, # Vecteur des clusters par modalité
    distances = distances,                 # Distances aux centres de clusters
    contingency = contingency,             # Table de contingence
    chi2_test = chi2_test,                # Résultat du test Chi²
    cramers_v = cramers_v,                # V de Cramér
    significant = significant,             # Booléen (p < 0.05)
    dice_scores = dice_scores             # Matrice Dice (observations × clusters)
  )
)
```

**Éléments principaux** :
- **cluster** : Cluster auquel la variable est assignée (basé sur distances)
- **distances** : Vecteur de longueur $K$ donnant la distance moyenne de la variable à chaque centre de cluster
- **contingency** : Table de contingence croisant modalités de la variable et clusters d'observations
- **chi2_test** : Résultats du test d'indépendance (statistique, p-value, df)
- **cramers_v** : Mesure d'association normalisée (0 à 1)
- **significant** : Booléen indiquant si l'association est significative (p < 0.05)
- **dice_scores** : Matrice donnant pour chaque observation son indice de Dice avec chaque cluster

### 3.4 Comparaison Méthodologique avec les Autres Algorithmes

| Aspect | VAR_CAH / VAR_KMEANS | TandemVarClust |
|--------|----------------------|----------------|
| **Nature des variables** | Quantitatives uniquement | Mixtes (quant. + qual.) |
| **Unité de clustering** | Variables | Modalités |
| **Méthode d'assignation** | Corrélation directe | Projection AFDM + distance |
| **Métrique principale** | Coefficient de corrélation | Distance euclidienne factorielle |
| **Métriques d'association** | Scores de corrélation | Chi², Cramér's V, Dice |
| **Traitement illustratif** | Corrélation avec cluster moyen | Projection + contingence |

**Avantage de TandemVarClust** : La projection dans l'espace AFDM permet de traiter de manière unifiée les variables catégorielles et numériques (après discrétisation), tout en exploitant la structure factorielle qui réduit la dimensionnalité et capture les associations principales.

**Limitation** : La discrétisation des variables numériques entraîne une perte d'information et introduit une dépendance au choix de `n_bins`.

## 4. Méthodes Utilitaires

### 4.1 Méthode `summary()` : Vue Synthétique du Modèle

```r
summary = function() {
  if (is.null(private$FGroupes)) {
    cat("Model not fitted yet.\n")
    return(invisible(NULL))
  }
  
  cat("\n")
  cat("==================================================================\n")
  cat("   TandemVarClust - Summary                                      \n")
  cat("==================================================================\n\n")
  
  cat("Number of clusters (K): ", private$FNbGroupes, "\n")
  cat("Number of modalities: ", length(private$FGroupes), "\n")
  cat("Number of original variables: ", ncol(private$FX), "\n")
  cat("  - Categorical: ", length(private$FCatVarNames), "\n")
  cat("  - Numeric: ", length(private$FNumVarNames), "\n")
  cat("Number of observations: ", nrow(private$FX), "\n\n")
  
  cat("Factorial analysis:\n")
  cat("  - Total inertia: ", round(private$FInertie, 3), "\n")
  cat("  - Number of factors: ", private$FNFactorsTotal, "\n")
  
  n_factors_show <- min(5, private$FNFactorsTotal)
  cat("  - Variance explained by first ", n_factors_show, " factors:\n")
  for (i in 1:n_factors_show) {
    cat("    Dim", i, ": ", round(private$FACMVariance[i], 2), "%\n", sep = "")
  }
  
  cat("\nCluster distribution:\n")
  cluster_counts <- table(private$FGroupes)
  for (k in 1:private$FNbGroupes) {
    count <- cluster_counts[as.character(k)]
    cat("  Cluster ", k, ": ", count, " modalities\n", sep = "")
  }
  
  cat("\n==================================================================\n")
  invisible(self)
}
```

**Informations fournies** :
- Paramètres du modèle ($K$, nombre de modalités, variables)
- Résumé de l'analyse factorielle (inertie, variance expliquée)
- Distribution des modalités dans les clusters

### 4.2 Méthode `plot_dendrogram()` : Visualisation de la Hiérarchie

```r
plot_dendrogram = function(main = "TandemVarClust Dendrogram", 
                          sub = NULL,
                          hang = -1,
                          ...) {
  if (is.null(private$FHclustTree)) {
    stop("The model must be fitted before plotting the dendrogram.")
  }
  
  plot(private$FHclustTree, 
       main = main,
       sub = sub,
       hang = hang,
       xlab = "Modalities",
       ylab = "Height",
       ...)
  
  if (!is.null(private$FNbGroupes)) {
    rect.hclust(private$FHclustTree, k = private$FNbGroupes, border = 2:6)
  }
  
  invisible(self)
}
```

**Visualisation** : Le dendrogramme représente la hiérarchie de fusion des modalités. Les rectangles indiquent la partition en $K$ clusters.

### 4.3 Méthode `plot_factorial()` : Projection Factorielle

```r
plot_factorial = function(axes = c(1, 2),
                         col_by_cluster = TRUE,
                         main = "Factorial Map of Modalities",
                         ...) {
  if (is.null(private$FFactorialCoords)) {
    stop("The model must be fitted before plotting the factorial map.")
  }
  
  coords <- private$FFactorialCoords
  
  if (max(axes) > ncol(coords)) {
    stop("Requested axes exceed the number of available factors.")
  }
  
  x <- coords[, axes[1]]
  y <- coords[, axes[2]]
  
  if (col_by_cluster && !is.null(private$FGroupes)) {
    colors <- rainbow(private$FNbGroupes)[private$FGroupes]
  } else {
    colors <- "black"
  }
  
  plot(x, y,
       col = colors,
       pch = 19,
       xlab = paste0("Dim ", axes[1], " (", round(private$FACMVariance[axes[1]], 1), "%)"),
       ylab = paste0("Dim ", axes[2], " (", round(private$FACMVariance[axes[2]], 1), "%)"),
       main = main,
       ...)
  
  if (col_by_cluster && !is.null(private$FGroupes)) {
    legend("topright",
           legend = paste("Cluster", 1:private$FNbGroupes),
           col = rainbow(private$FNbGroupes),
           pch = 19,
           bty = "n")
  }
  
  invisible(self)
}
```

**Visualisation** : Les modalités sont représentées dans le plan factoriel, colorées par cluster. Cette visualisation permet d'évaluer la séparabilité des clusters dans l'espace de dimension réduite.

### 4.4 Méthode `get_variable_summary()` : Agrégation par Variable

```r
get_variable_summary = function() {
  if (is.null(private$FGroupes)) {
    stop("The model must be fitted first.")
  }
  
  groupes_modalites <- private$FGroupes
  noms_modalites <- names(groupes_modalites)
  noms_variables <- sub("\\..*", "", noms_modalites)
  
  df <- data.frame(
    modalite = noms_modalites,
    variable = noms_variables,
    cluster = groupes_modalites,
    stringsAsFactors = FALSE
  )
  
  result <- do.call(rbind, lapply(unique(noms_variables), function(v) {
    subset_v <- df[df$variable == v, ]
    tbl <- table(subset_v$cluster)
    cluster_principal <- as.integer(names(tbl)[which.max(tbl)])
    purity <- max(tbl) / sum(tbl)
    
    data.frame(
      variable = v,
      n_modalites = nrow(subset_v),
      cluster_principal = cluster_principal,
      purity = purity,
      stringsAsFactors = FALSE
    )
  }))
  
  result <- result[order(-result$purity), ]
  rownames(result) <- NULL
  
  return(result)
}
```

**Objectif** : Résumer l'assignation des variables à partir de leurs modalités.

**Logique** :
- **Cluster principal** : Cluster contenant le plus de modalités de la variable
- **Pureté** : Proportion de modalités dans le cluster principal

$$\text{Pureté}(v) = \frac{\max_k |\{m \in v : \text{cluster}(m) = k\}|}{|v|}$$

Une pureté de 1 signifie que toutes les modalités de la variable sont dans le même cluster.

### 4.5 Méthode `get_modalities_of_variable()` : Modalités d'une Variable

```r
get_modalities_of_variable = function(variable_name) {
  if (is.null(private$FGroupes)) {
    stop("The model is not fitted.")
  }
  
  groupes <- private$FGroupes
  noms <- names(groupes)
  
  pattern <- paste0("^", variable_name, "\\.")
  indices <- grep(pattern, noms)
  
  if (length(indices) == 0) {
    stop("Variable '", variable_name, "' not found")
  }
  
  result <- data.frame(
    modalite = sub("^[^\\.]+\\.", "", noms[indices]),
    cluster = groupes[indices],
    stringsAsFactors = FALSE
  )
  
  return(result)
}
```

**Utilité** : Inspecter l'assignation des modalités d'une variable spécifique.

### 4.6 Méthode `get_modalities_of_cluster()` : Modalités d'un Cluster

```r
get_modalities_of_cluster = function(cluster_id) {
  if (is.null(private$FGroupes)) {
    stop("The model is not fitted.")
  }
  
  if (cluster_id < 1 || cluster_id > private$FNbGroupes) {
    stop("k must be between 1 and ", private$FNbGroupes)
  }
  
  groupes <- private$FGroupes
  noms <- names(groupes)[groupes == cluster_id]
  
  result <- data.frame(
    variable = sub("\\..*", "", noms),
    modalite = sub("^[^\\.]+\\.", "", noms),
    stringsAsFactors = FALSE
  )
  
  return(result)
}
```

**Utilité** : Caractériser le contenu d'un cluster en listant toutes ses modalités et variables associées.

### 4.7 Méthode `refit_with_k()` : Réajustement avec un Nouveau K

```r
refit_with_k = function(new_k) {
  if (is.null(private$FHclustTree)) {
    stop("The model must be fitted before calling refit_with_k().")
  }
  
  if (new_k < 2) {
    stop("K must be >= 2")
  }
  
  n_modalities <- length(private$FGroupes)
  if (new_k > n_modalities) {
    stop("K (", new_k, ") cannot exceed the number of modalities (", n_modalities, ")")
  }
  
  self$K <- new_k
  
  private$assign_observations_to_clusters()
  
  cat("Model refitted with K =", new_k, "\n")
  invisible(self)
}
```

**Utilité** : Modifier le nombre de clusters sans recalculer l'AFDM et la CAH (qui sont coûteuses). Seule la coupe de l'arbre et le réassignation des observations sont effectuées.

**Cas d'usage** : Exploration rapide de différentes valeurs de $K$ pour trouver le nombre optimal de clusters.

## 5. Active Bindings : Accès aux Résultats

Les **active bindings** fournissent un accès en lecture seule (ou contrôlé en écriture pour `K`) aux résultats du modèle :

```r
active = list(
  K = function(value) { ... },                    # Nombre de clusters (lecture/écriture)
  Groupes = function() { ... },                   # Assignation des modalités
  NbModalites = function() { ... },               # Nombre de modalités
  Tree = function() { ... },                      # Arbre hiérarchique
  VarianceExplained = function() { ... },         # Variance expliquée par facteur
  Inertie = function() { ... },                   # Inertie totale
  DisjunctiveTable = function() { ... },          # Tableau disjonctif
  FactorialCoords = function() { ... },           # Coordonnées factorielles
  CategoricalVars = function() { ... },           # Noms des variables catégorielles
  NumericVars = function() { ... },               # Noms des variables numériques
  VariableClusters = function() { ... }           # Assignation des variables (agrégée)
)
```

### Détails sur `VariableClusters`

```r
VariableClusters = function() {
  if (is.null(private$FGroupes)) {
    return(NULL)
  }
  
  modality_names <- names(private$FGroupes)
  var_names <- sub("\\..*", "", modality_names)
  
  unique_vars <- unique(var_names)
  var_clusters <- integer(length(unique_vars))
  names(var_clusters) <- unique_vars
  
  for (var in unique_vars) {
    var_modalities <- modality_names[var_names == var]
    clusters <- unique(private$FGroupes[var_modalities])
    
    if (length(clusters) == 1) {
      var_clusters[var] <- clusters[1]
    } else {
      cluster_counts <- table(private$FGroupes[var_modalities])
      var_clusters[var] <- as.integer(names(cluster_counts)[which.max(cluster_counts)])
    }
  }
  
  return(var_clusters)
}
```

**Logique** : Pour chaque variable, déterminer le cluster majoritaire de ses modalités. Si toutes les modalités sont dans le même cluster, ce cluster est assigné. Sinon, le cluster contenant le plus de modalités est choisi.

## 6. Fonction Wrapper `roller_clust()`

### 6.1 Interface Unifiée

La fonction `roller_clust()` fournit une interface unifiée pour les trois algorithmes du package :

```r
roller_clust <- function(X, 
                        K, 
                        method = c("VAR_CAH", "VAR_KMEANS", "TandemVarClust"),
                        ...) {
  method <- match.arg(method)
  
  model <- switch(method,
    VAR_CAH = VAR_CAH$new(K = K, ...),
    VAR_KMEANS = VAR_KMEANS$new(K = K, ...),
    TandemVarClust = TandemVarClust$new(K = K, ...)
  )
  
  model$fit(X)
  
  return(model)
}
```

### 6.2 Utilisation

```r
# Clustering avec TandemVarClust
model <- roller_clust(my_data, K = 3, method = "TandemVarClust", n_bins = 5)

# Accès aux résultats
model$summary()
model$plot_dendrogram()

# Prédiction sur variables illustratives
new_vars <- data.frame(illus1 = sample(c("A", "B"), 100, replace = TRUE))
predictions <- model$predict(new_vars)
```

## 7. Cas d'Usage et Applications

### 7.1 Segmentation Marketing

**Contexte** : Segmentation de clients sur la base de données socio-démographiques mixtes (âge, revenu, catégorie socio-professionnelle, région, situation familiale).

**Avantages de TandemVarClust** :
- Traitement naturel des types de données mixtes
- Clustering au niveau des modalités capturant des profils fins (ex : "Célibataires urbains 25-34 ans cadres" vs "Familles rurales 45-54 ans employés")
- Projection de variables illustratives (comportement d'achat, satisfaction) pour valider la pertinence des segments
- Métriques d'association (Chi², Cramér's V) pour quantifier la force du lien entre comportement et segment

**Exemple de workflow** :

```r
# 1. Clustering des variables socio-démographiques
data_demo <- data.frame(
  age = sample(18:70, 500, replace = TRUE),
  csp = sample(c("Cadre", "Employé", "Ouvrier", "Retraité"), 500, replace = TRUE),
  region = sample(c("Nord", "Sud", "Est", "Ouest"), 500, replace = TRUE),
  situation = sample(c("Célibataire", "Marié", "Divorcé"), 500, replace = TRUE)
)

model <- TandemVarClust$new(K = 4, n_bins = 5)
model$fit(data_demo)

# 2. Projection de variables comportementales illustratives
data_comportement <- data.frame(
  achat_premium = sample(c("Oui", "Non"), 500, replace = TRUE),
  satisfaction = sample(c("Satisfait", "Neutre", "Insatisfait"), 500, replace = TRUE)
)

pred <- model$predict(data_comportement)

# 3. Analyse des associations
print(pred$achat_premium$cramers_v)  # Force de l'association
print(pred$achat_premium$chi2_test)  # Significativité
```

### 7.2 Analyse de Questionnaires

**Contexte** : Enquête de satisfaction avec questions Likert (ordinales), questions ouvertes recodées (nominales), et quelques métriques quantitatives (âge, ancienneté).

**Avantages de TandemVarClust** :
- Identification de patterns de réponses cohérents au niveau des modalités
- Clustering des modalités révélant des profils de répondants
- Analyse des libres commentaires (variables illustratives) pour enrichir l'interprétation
- Métriques d'association aidant à identifier les questions les plus discriminantes

### 7.3 Épidémiologie et Santé Publique

**Contexte** : Étude des déterminants de la santé à partir de données individuelles hétérogènes (antécédents médicaux, habitudes de vie, facteurs socio-économiques).

**Avantages de TandemVarClust** :
- Intégration de variables binaires (présence/absence de pathologies), ordinales (fréquence de consommation), et continues (IMC, âge)
- Identification de syndromes de facteurs de risque (combinaisons de modalités à risque)
- Projection de variables illustratives (survenue d'événements de santé) pour valider les facteurs de risque
- Scores de Dice permettant l'identification de patients avec des profils de risque peu clairs nécessitant un suivi plus étroit

### 7.4 Analyse de Données Textuelles Codées

**Contexte** : Analyse de contenu d'entretiens ou de documents, avec variables décrivant la présence de thèmes (binaire), intensité de tonalité (ordinale), et métadonnées (nominale et continue).

**Avantages de TandemVarClust** :
- Typologie de documents basée sur leurs caractéristiques sémantiques et contextuelles
- Clustering fin au niveau des modalités révélant des co-occurrences thématiques
- Projection de variables illustratives (auteur, date, source) pour contextualiser les clusters
- Métriques d'association aidant à identifier les thèmes les plus structurants

## 8. Références Théoriques

### 8.1 Analyse Factorielle de Données Mixtes (AFDM)

- **Escofier, B.** (1979). *Traitement simultané de variables quantitatives et qualitatives*. Les Cahiers de l'Analyse des Données, IV(2), 137-146.
  > Article fondateur introduisant le principe de l'analyse simultanée de variables mixtes dans le cadre de l'ACM.

- **Pagès, J.** (2004). *Analyse factorielle de données mixtes*. Revue de Statistique Appliquée, 52(4), 93-111.
  > Article de référence consolidant les travaux antérieurs et démontrant l'équivalence entre les approches.

- **Husson, F., Lê, S., & Pagès, J.** (2017). *Exploratory Multivariate Analysis by Example Using R* (2e éd.). CRC Press.
  > Guide pratique des méthodes d'analyse multivariée avec R et FactoMineR, incluant l'AFDM.

### 8.2 Clustering Tandem et Approches Séquentielles

- **Arabie, P., & Hubert, L. J.** (1994). *Cluster analysis in marketing research*. In: *Advanced Methods of Marketing Research* (R. P. Bagozzi, Ed.), Blackwell, 160-189.
  > Critique de l'approche tandem séquentielle et introduction du terme "tandem analysis".

- **Vichi, M., & Kiers, H. A. L.** (2001). *Factorial k-means analysis for two-way data*. Computational Statistics & Data Analysis, 37(1), 49-64.
  > Proposition du Factorial K-means (FKM), généralisant RKM. Comparaison théorique et empirique avec l'approche tandem.

### 8.3 Indices de Similarité

- **Dice, L. R.** (1945). *Measures of the amount of ecologic association between species*. Ecology, 26(3), 297-302.
  > Article original introduisant le coefficient de Dice pour mesurer la similarité entre ensembles.

- **Kaufman, L., & Rousseeuw, P. J.** (1990). *Finding Groups in Data: An Introduction to Cluster Analysis*. Wiley.
  > Ouvrage de référence sur le clustering, incluant la discussion des indices de similarité pour données catégorielles.

- **Tan, P. N., Steinbach, M., & Kumar, V.** (2005). *Introduction to Data Mining*. Pearson.
  > Traitement complet des mesures de similarité incluant Dice, Jaccard, et leurs applications.

### 8.4 Classification Ascendante Hiérarchique

- **Ward, J. H., Jr.** (1963). *Hierarchical grouping to optimize an objective function*. Journal of the American Statistical Association, 58(301), 236-244.
  > Article original proposant le critère de Ward pour la CAH.

- **Murtagh, F., & Contreras, P.** (2012). *Algorithms for hierarchical clustering: An overview*. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 2(1), 86-97.
  > Revue des méthodes de CAH avec analyse de complexité.

### 8.5 Tests d'Association pour Variables Qualitatives

- **Cramér, H.** (1946). *Mathematical Methods of Statistics*. Princeton University Press.
  > Ouvrage classique introduisant le coefficient V de Cramér.

- **Agresti, A.** (2013). *Categorical Data Analysis* (3e éd.). Wiley.
  > Référence moderne sur l'analyse de données catégorielles, incluant les tests du Chi² et les mesures d'association.

### 8.6 Éléments Supplémentaires en Analyse Factorielle

- **Greenacre, M.** (2007). *Correspondence Analysis in Practice* (2e éd.). Chapman & Hall/CRC.
  > Traitement complet de l'analyse des correspondances incluant les éléments supplémentaires.

- **Le Roux, B., & Rouanet, H.** (2004). *Geometric Data Analysis: From Correspondence Analysis to Structured Data Analysis*. Kluwer Academic Publishers.
  > Approche géométrique de l'analyse de données, incluant le traitement des individus supplémentaires et la caractérisation de classes.

## 9. Conclusion

L'implémentation de `TandemVarClust` offre une approche focalisée et théoriquement solide pour le clustering de modalités dans le contexte de données mixtes. En combinant l'AFDM pour la réduction de dimensionnalité, la CAH pour le clustering, et deux méthodes complémentaires d'assignation (Dice pour les observations, projection pour les variables), cette méthode fournit :

1. **Traitement naturel des données hétérogènes** : Variables quantitatives et qualitatives dans un même cadre
2. **Capture de structures fines** : Clustering au niveau des modalités révélant des patterns que le clustering de variables masquerait
3. **Interprétabilité riche** : Via les méthodes utilitaires d'agrégation par variable et d'analyse de variables illustratives
4. **Rigueur méthodologique** : 
   - Indice de Dice pour l'assignation des observations (mesure de chevauchement harmonique)
   - Projection AFDM pour l'assignation des variables (cohérence avec l'espace factoriel)
   - Métriques d'association statistiques (Chi², Cramér's V)
5. **Flexibilité** : Paramètres de régularisation (`n_factors`, `n_bins`) permettant d'adapter la méthode au contexte
6. **Confiance d'assignation** : 
   - Scores de Dice pour évaluer la qualité des assignations d'observations
   - Distances aux centres pour quantifier la proximité des variables aux clusters

### Distinction Méthodologique : Deux Approches d'Assignation

L'implémentation propose deux méthodes d'assignation complémentaires :

**Pour les observations** (méthode de Dice) :
- Mesure le chevauchement entre modalités actives de l'observation et modalités du cluster
- Indice normalisé entre 0 et 1, interprétable comme une proportion de similarité
- Traite symétriquement les tailles d'observation et de cluster (harmonique)

**Pour les variables illustratives** (méthode de projection) :
- Projette les modalités dans l'espace AFDM établi lors de l'entraînement
- Calcule la distance euclidienne moyenne aux centres de clusters factoriels
- Garantit la cohérence méthodologique avec la structure factorielle du clustering
- Enrichit l'assignation avec des métriques d'association (Chi², Cramér's V)

Cette dualité méthodologique est justifiée par la nature différente des deux tâches :
- **Observations** : Entités déjà présentes lors de l'analyse, caractérisées par un profil de modalités actives fixe
- **Variables illustratives** : Nouvelles dimensions projetées après coup, dont on évalue l'association globale avec les clusters

La méthode s'inscrit dans la tradition de l'école française d'analyse des données, privilégiant l'exploration et l'interprétation sur la modélisation paramétrique. Elle constitue un outil précieux pour les praticiens confrontés à des données réelles complexes et hétérogènes, notamment dans les domaines du marketing, des sciences sociales, de la santé publique et de l'analyse textuelle.

La cohérence entre le clustering de modalités (via AFDM + CAH) et l'assignation de variables illustratives (via projection AFDM + distance) garantit que tous les résultats s'inscrivent dans un même espace méthodologique unifié, facilitant l'interprétation globale de l'analyse.

---

**Document rédigé dans le cadre du développement du package R `RollerClustR`**  
**Auteur** : Romain Buono  
**Date** : Novembre 2025  
**Licence** : MIT