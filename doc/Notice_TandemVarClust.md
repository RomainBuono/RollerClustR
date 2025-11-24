# Implémentation de l'Algorithme TandemVarClust dans la Classe TandemVarClust

## 1. Fondements Théoriques

### 1.1 Principe Général

L'implémentation proposée réalise un clustering hiérarchique de **modalités** (et non directement de variables) selon une approche Tandem combinant :

1. **AFDM** (Analyse Factorielle de Données Mixtes) pour la réduction de dimensionnalité
2. **CAH** (Classification Ascendante Hiérarchique) sur les coordonnées factorielles

Cette méthode se distingue fondamentalement des approches classiques de clustering de variables (VAR_CAH, VARCLUS) car elle opère au niveau des **modalités** : chaque catégorie d'une variable qualitative ou chaque intervalle discrétisé d'une variable quantitative constitue une unité à classer. Ce changement d'échelle permet de capturer des structures plus fines et de traiter naturellement des données mixtes (quantitatives et qualitatives).

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

**Pondération des modalités** : L'utilisation de l'inverse des fréquences marginales $f_k = \frac{n_k}{n}$ où $n_k$ est l'effectif de la modalité $k$ garantit que :

$$w_k = \frac{1}{f_k}$$

Cette pondération compense le déséquilibre potentiel entre modalités fréquentes et rares. Une modalité rare (faible $f_k$) reçoit un poids élevé ($w_k$ grand), augmentant son influence dans l'analyse factorielle.

#### Décomposition en Valeurs Singulières

```r
# Matrices de pondération
D_col_sqrt <- diag(sqrt(col_weights))
D_row_sqrt <- diag(sqrt(row_weights))

# Matrice pondérée
Z_weighted <- D_row_sqrt %*% Z_centered %*% D_col_sqrt

# SVD
svd_res <- svd(Z_weighted)
```

La SVD de la matrice pondérée $\mathbf{Z}_w = \mathbf{D}_r^{1/2} \mathbf{Z}_c \mathbf{D}_c^{1/2}$ s'écrit :

$$\mathbf{Z}_w = \mathbf{U} \boldsymbol{\Lambda} \mathbf{V}^T$$

où :
- $\mathbf{U}$ : Matrice des vecteurs propres gauches (axes factoriels pour les observations)
- $\boldsymbol{\Lambda}$ : Matrice diagonale des valeurs singulières
- $\mathbf{V}$ : Matrice des vecteurs propres droits (axes factoriels pour les modalités)

#### Coordonnées Factorielles des Modalités

```r
# Coordonnées factorielles des modalités
private$FFactorialCoords <- D_col_sqrt %*% svd_res$v
rownames(private$FFactorialCoords) <- colnames(Z)
```

Les coordonnées factorielles $\boldsymbol{\Phi}$ des modalités sont données par :

$$\boldsymbol{\Phi} = \mathbf{D}_c^{1/2} \mathbf{V}$$

Chaque ligne de $\boldsymbol{\Phi}$ correspond à une modalité, chaque colonne à un axe factoriel. Ces coordonnées positionnent chaque modalité dans l'espace factoriel et serviront de base au clustering.

#### Valeurs Propres et Inertie

```r
# Valeurs propres et variance expliquée
eigenvalues <- svd_res$d^2
private$FACMVariance <- 100 * eigenvalues / sum(eigenvalues)
private$FInertie <- sum(eigenvalues)
```

Les valeurs propres $\lambda_k = d_k^2$ quantifient la variance expliquée par chaque axe factoriel. L'**inertie totale** est définie par :

$$I_{\text{tot}} = \sum_{k=1}^{p^*} \lambda_k$$

Le pourcentage de variance expliquée par l'axe $k$ est :

$$\text{Var\%}_k = \frac{\lambda_k}{I_{\text{tot}}} \times 100$$

Cette métrique permet de sélectionner le nombre d'axes factoriels à retenir pour le clustering.

### 2.3 Phase de Classification Hiérarchique

#### Sélection des Axes Factoriels

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

Le paramètre `n_factors` permet de régulariser le clustering en limitant le nombre d'axes factoriels utilisés. Deux stratégies existent :

- **`n_factors = NULL`** : Utilisation de tous les axes (approche exhaustive)
- **`n_factors = 2-5`** : Régularisation par restriction aux premiers axes principaux

**Justification théorique** : Les derniers axes factoriels capturent souvent du bruit plutôt que de la structure. Les restreindre améliore la robustesse du clustering au détriment d'une légère perte d'information.

#### Distance Euclidienne et Arbre Hiérarchique

```r
# Distance euclidienne dans l'espace factoriel
dist_matrix <- dist(coords, method = "euclidean")

# Classification Ascendante Hiérarchique
private$FHclustTree <- hclust(dist_matrix, method = private$FMethodCAH)
```

La CAH construit un dendrogramme en agrégeant itérativement les modalités selon un critère de fusion. La distance entre deux modalités $i$ et $j$ dans l'espace factoriel à $q$ dimensions est :

$$d(i,j) = \sqrt{\sum_{k=1}^{q} (\phi_{ik} - \phi_{jk})^2}$$

où $\phi_{ik}$ désigne la coordonnée de la modalité $i$ sur l'axe $k$.

#### Méthode de Ward

La méthode de Ward (par défaut `"ward.D2"`) minimise l'augmentation d'inertie intra-classe à chaque fusion. Pour deux clusters $C_i$ et $C_j$, le critère de fusion est :

$$\Delta(C_i, C_j) = \frac{|C_i| \cdot |C_j|}{|C_i| + |C_j|} \|m_i - m_j\|^2$$

où $m_i$ et $m_j$ sont les centroïdes des clusters, et $|\cdot|$ dénote la cardinalité.

Cette méthode favorise la création de clusters compacts et équilibrés en effectif.

#### Coupe de l'Arbre

```r
# Partition en K clusters
private$FGroupes <- cutree(private$FHclustTree, k = private$FNbGroupes)
names(private$FGroupes) <- rownames(coords)
```

La partition finale est obtenue en coupant le dendrogramme au niveau $K$ spécifié. On obtient un vecteur d'assignations :

$$\mathbf{c} = (c_1, \ldots, c_{p^*}) \quad \text{où} \quad c_i \in \{1, \ldots, K\}$$

Chaque modalité $i$ est assignée à un cluster $c_i$.

### 2.4 Reconfiguration Dynamique : Modification de K

```r
refit_with_k = function(new_k) {
  if (is.null(private$FHclustTree)) {
    stop("Le modèle n'est pas encore ajusté.")
  }
  
  private$FNbGroupes <- new_k
  private$FGroupes <- cutree(private$FHclustTree, k = new_k)
  
  message("Modèle réajusté avec K = ", new_k)
}
```

L'avantage de la CAH réside dans sa structure hiérarchique : le dendrogramme encode **toutes les partitions possibles**. Modifier $K$ ne nécessite donc qu'une nouvelle coupe de l'arbre, sans réexécuter les étapes d'AFDM et de calcul de distances.

Cette fonctionnalité permet une exploration rapide de différentes valeurs de $K$ pour identifier la partition optimale selon un critère externe (silhouette, gap statistic, expertise métier).

## 3. Prédiction de Variables Illustratives

### 3.1 Principe : du Clustering de Modalités au Clustering d'Observations

**Problématique fondamentale** : Le modèle TandemVarClust produit des clusters de **modalités**, mais pour analyser une variable illustrative, il faut assigner chaque **observation** à un cluster.

**Solution adoptée** : Assignation par indice de similarité de Dice.

### 3.2 Assignation par Indice de Dice

#### Fondement Théorique

L'assignation des observations aux clusters repose sur l'**indice de Dice** (Dice, 1945), coefficient de similarité établi pour mesurer la ressemblance entre deux ensembles.

Pour une observation $i$ possédant un ensemble de modalités actives $M_i$ et un cluster $k$ contenant un ensemble de modalités $C_k$, l'indice de Dice est défini par :

$$\text{Dice}(M_i, C_k) = \frac{2|M_i \cap C_k|}{|M_i| + |C_k|}$$

où $|M_i \cap C_k|$ représente le nombre de modalités communes entre l'observation et le cluster.

L'observation est assignée au cluster maximisant cet indice :

$$\hat{c}_i = \arg\max_{k=1,\ldots,K} \text{Dice}(M_i, C_k)$$

#### Implémentation

```r
assign_observations_to_clusters = function() {
  Z <- private$FDisjunctiveTable
  n <- nrow(Z)
  groupes <- private$FGroupes
  K <- private$FNbGroupes
  
  obs_clusters <- integer(n)
  dice_scores <- matrix(0, nrow = n, ncol = K)
  
  # Identifier les modalités de chaque cluster
  cluster_modalities <- lapply(1:K, function(k) {
    which(groupes == k)
  })
  
  for (i in 1:n) {
    # Modalités actives de l'observation i
    modalities_i <- which(Z[i, ] == 1)
    n_modalities_i <- length(modalities_i)
    
    if (n_modalities_i == 0) {
      obs_clusters[i] <- 1
      dice_scores[i, ] <- NA
      next
    }
    
    # Calculer Dice avec chaque cluster
    for (k in 1:K) {
      modalities_k <- cluster_modalities[[k]]
      n_modalities_k <- length(modalities_k)
      
      # Intersection
      intersection <- length(intersect(modalities_i, modalities_k))
      
      # Indice de Dice
      if (n_modalities_i + n_modalities_k > 0) {
        dice_scores[i, k] <- (2 * intersection) / (n_modalities_i + n_modalities_k)
      } else {
        dice_scores[i, k] <- 0
      }
    }
    
    # Assigner au cluster avec Dice maximum
    obs_clusters[i] <- which.max(dice_scores[i, ])
  }
  
  return(list(
    clusters = obs_clusters,
    scores = dice_scores
  ))
}
```

#### Justification Méthodologique

L'indice de Dice est particulièrement adapté au clustering de modalités car il :

1. **Respecte la nature discrète** des ensembles de modalités (Kaufman & Rousseeuw, 1990)
2. **Corrige le biais** induit par les clusters de tailles différentes grâce à une normalisation symétrique
3. **Fournit une mesure de similarité normalisée** entre 0 et 1, facilitant l'interprétation
4. **Gère les profils d'observations variables** : Une observation avec peu de modalités actives n'est pas pénalisée par rapport à une observation avec beaucoup de modalités

**Comparaison avec l'indice de Jaccard** : L'indice de Dice est préféré à l'indice de Jaccard :

$$\text{Jaccard}(M_i, C_k) = \frac{|M_i \cap C_k|}{|M_i \cup C_k|}$$

L'indice de Dice (basé sur la moyenne harmonique) donne plus de poids à l'intersection que le Jaccard (basé sur la moyenne arithmétique), ce qui est plus approprié lorsque la présence de modalités est plus informative que l'absence.

Cette approche est cohérente avec les pratiques de caractérisation de classes en analyse de données catégorielles (Le Roux & Rouanet, 2004).

**Sortie** : Cette méthode retourne non seulement les assignations aux clusters mais également une matrice de scores de Dice pour chaque paire observation-cluster, permettant une analyse post-hoc de la confiance d'assignation.

#### Propriétés de l'Indice de Dice

**Propriétés mathématiques** :

1. **Symétrie** : $\text{Dice}(A, B) = \text{Dice}(B, A)$
2. **Bornes** : $0 \leq \text{Dice}(A, B) \leq 1$
3. **Identité** : $\text{Dice}(A, A) = 1$
4. **Similarité nulle** : $\text{Dice}(A, B) = 0 \Leftrightarrow A \cap B = \emptyset$

**Avantages par rapport au vote majoritaire simple** :

- **Normalisation des tailles** : Le dénominateur $|M_i| + |C_k|$ normalise à la fois pour les tailles d'observation et de cluster
- **Évaluation quantitative** : Fournit des scores continus (0 à 1) plutôt que des comptages discrets
- **Fondement théorique** : Bien établi en écologie et en recherche d'information
- **Interprétabilité** : Le score représente la proportion de chevauchement par rapport au total des modalités

**Efficacité computationnelle** : L'algorithme a une complexité $O(n \times K \times p^*)$ où :
- $n$ : nombre d'observations
- $K$ : nombre de clusters
- $p^*$ : nombre de modalités

Ceci est efficace pour les tailles de jeux de données typiquement rencontrées en pratique.

### 3.3 Construction du Tableau de Contingence

Une fois les observations assignées aux clusters via l'indice de Dice, l'analyse des variables illustratives procède :

```r
predict = function(newdata) {
  # Assigner les observations aux clusters via l'indice de Dice
  result <- private$assign_observations_to_clusters()
  obs_clusters <- result$clusters
  dice_scores <- result$scores
  
  for (var_name in names(newdata)) {
    var_col <- newdata[[var_name]]
    if (!is.factor(var_col)) var_col <- as.factor(var_col)
    
    # Tableau de contingence
    contingency <- table(var_col, obs_clusters)
    colnames(contingency) <- paste0("Cluster_", colnames(contingency))
  }
}
```

Le tableau de contingence croise les modalités de la variable illustrative avec les clusters d'observations :

$$
\begin{array}{c|ccc|c}
& \text{Cluster 1} & \text{Cluster 2} & \text{Cluster 3} & \text{Total} \\
\hline
\text{Modalité A} & n_{A1} & n_{A2} & n_{A3} & n_{A\cdot} \\
\text{Modalité B} & n_{B1} & n_{B2} & n_{B3} & n_{B\cdot} \\
\text{Modalité C} & n_{C1} & n_{C2} & n_{C3} & n_{C\cdot} \\
\hline
\text{Total} & n_{\cdot 1} & n_{\cdot 2} & n_{\cdot 3} & n
\end{array}
$$

### 3.4 Tests Statistiques : Chi-Deux et V de Cramer

#### Test du Chi-Deux

Le test du $\chi^2$ évalue l'indépendance entre la variable illustrative et les clusters :

$$\chi^2 = \sum_{i=1}^{r} \sum_{j=1}^{c} \frac{(n_{ij} - e_{ij})^2}{e_{ij}}$$

où $e_{ij} = \frac{n_{i\cdot} n_{\cdot j}}{n}$ est l'effectif théorique sous l'hypothèse d'indépendance.

```r
chi2_test <- chisq.test(contingency)
```

**Interprétation** :
- **p-valeur < 0.05** : Rejet de l'indépendance → La variable illustrative est significativement liée aux clusters
- **p-valeur ≥ 0.05** : Non-rejet → Pas de liaison significative détectée

#### Coefficient V de Cramer

Le V de Cramer mesure l'**intensité** de la liaison entre deux variables qualitatives :

$$V = \sqrt{\frac{\chi^2}{n \times \min(r-1, c-1)}}$$

```r
compute_cramers_v = function(contingency) {
  chi2 <- suppressWarnings(chisq.test(contingency)$statistic)
  n <- sum(contingency)
  min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
  
  if (min_dim == 0) return(NA)
  
  v <- sqrt(chi2 / (n * min_dim))
  return(as.numeric(v))
}
```

**Échelle d'interprétation** :

| V de Cramer | Interprétation | Signification pratique |
|-------------|----------------|------------------------|
| 0.00 - 0.10 | Liaison négligeable | Clusters et variable indépendants |
| 0.10 - 0.30 | Liaison faible | Association détectable mais limitée |
| 0.30 - 0.50 | Liaison modérée | Structure partiellement expliquée |
| 0.50 - 0.70 | Liaison forte | Bonne correspondance |
| > 0.70 | Liaison très forte | Clusters largement déterminés par la variable |

**Avantage du V de Cramer** : Contrairement au $\chi^2$, le V de Cramer est normalisé dans $[0, 1]$ et indépendant de la taille d'échantillon, permettant des comparaisons entre études.

### 3.5 Structure des Résultats

```r
results[[var_name]] <- list(
  contingency = contingency,
  percentages_by_modality = pct_row,
  percentages_by_cluster = pct_col,
  chi2_test = chi2_test,
  cramers_v = private$compute_cramers_v(contingency),
  significant = chi2_test$p.value < 0.05,
  dice_scores = dice_scores
)
```

La méthode `predict()` retourne une liste structurée pour chaque variable illustrative :

1. **`contingency`** : Tableau de contingence brut (effectifs)
2. **`percentages_by_modality`** : Profils lignes (somme = 100% par modalité)
3. **`percentages_by_cluster`** : Profils colonnes (somme = 100% par cluster)
4. **`chi2_test`** : Objet de classe `htest` contenant le $\chi^2$, ddl et p-valeur
5. **`cramers_v`** : Coefficient V de Cramer
6. **`significant`** : Booléen indiquant la significativité au seuil de 5%
7. **`dice_scores`** : Matrice des scores de Dice pour chaque paire observation-cluster

L'inclusion de `dice_scores` permet aux utilisateurs de :
- Évaluer la confiance d'assignation pour chaque observation
- Identifier les observations avec des assignations ambiguës (scores similaires sur plusieurs clusters)
- Réaliser des analyses de sensibilité sur les assignations aux clusters

### 3.6 Interprétation des Profils

#### Profils Lignes (Percentages by Modality)

Les profils lignes indiquent **où se répartissent** les observations d'une modalité donnée :

Exemple :
```
            Cluster_1  Cluster_2  Cluster_3
Modalité_A     25%        60%        15%
```

**Lecture** : Parmi les observations ayant la modalité A, 25% appartiennent au cluster 1, 60% au cluster 2, 15% au cluster 3.

#### Profils Colonnes (Percentages by Cluster)

Les profils colonnes indiquent **la composition** de chaque cluster :

Exemple :
```
            Cluster_1  Cluster_2  Cluster_3
Modalité_A     40%        20%        10%
Modalité_B     30%        50%        30%
Modalité_C     30%        30%        60%
```

**Lecture** : Dans le cluster 1, 40% des observations ont la modalité A, 30% la modalité B, 30% la modalité C.

### 3.7 Cas d'Usage Pratique

**Scénario** : Clustering de modalités de données socio-démographiques (Sexe, Âge discrétisé, CSP, Région). Variable illustrative : `Comportement d'achat` (3 modalités : Économe, Équilibré, Dépensier).

```r
# Clustering principal
model <- TandemVarClust$new(K = 3, n_factors = 3, n_bins = 5)
model$fit(data_socio_demo)

# Analyse de la variable illustrative
results <- model$predict(newdata = data.frame(Comportement = comportement_achat))

# Examen des résultats
cat("V de Cramer:", results$Comportement$cramers_v, "\n")
# V de Cramer: 0.52

cat("\nTest du Chi-deux:\n")
print(results$Comportement$chi2_test)
# p-value < 0.001

cat("\nProfils par modalité:\n")
print(results$Comportement$percentages_by_modality)
#              Cluster_1  Cluster_2  Cluster_3
# Économe          12%        78%        10%
# Équilibré        45%        40%        15%
# Dépensier        68%        20%        12%

# Examen de la confiance d'assignation
cat("\nScores Dice moyens par cluster:\n")
print(colMeans(results$Comportement$dice_scores, na.rm = TRUE))
# Cluster_1: 0.67  Cluster_2: 0.71  Cluster_3: 0.58
```

**Interprétation** :

1. **V = 0.52** : Liaison forte entre comportement d'achat et clusters de modalités socio-démographiques
2. **p < 0.001** : Liaison statistiquement très significative
3. **Profils** :
   - Cluster 2 : Majoritairement "Économes" (78%) avec une confiance d'assignation élevée (0.71)
   - Cluster 1 : Plutôt "Dépensiers" (68%) avec une bonne confiance (0.67)
   - Cluster 3 : Distribution plus homogène avec une confiance plus faible (0.58)
4. **Scores Dice** : Les scores Dice moyens indiquent que les assignations du Cluster 3 sont moins confiantes, suggérant que ces observations ont des profils plus hétérogènes

**Conclusion** : Les clusters capturent des profils socio-démographiques fortement liés au comportement d'achat. Une analyse détaillée des modalités de chaque cluster révélera les caractéristiques démographiques typiques de chaque segment comportemental.

## 4. Méthodes Utilitaires

### 4.1 Synthèse par Variable : `get_variable_summary()`

```r
get_variable_summary = function() {
  groupes_modalites <- self$Groupes
  noms_modalites <- names(groupes_modalites)
  noms_variables <- sub("\\..*", "", noms_modalites)
  
  # Pour chaque variable, identifier le cluster principal et la pureté
  result <- do.call(rbind, lapply(unique(noms_variables), function(v) {
    subset_v <- df[df$variable == v, ]
    tbl <- table(subset_v$cluster)
    cluster_principal <- as.integer(names(tbl)[which.max(tbl)])
    purity <- max(tbl) / sum(tbl)
    
    data.frame(
      variable = v,
      n_modalites = nrow(subset_v),
      cluster_principal = cluster_principal,
      purity = purity
    )
  }))
}
```

Cette méthode agrège les résultats au niveau des variables d'origine, malgré le clustering des modalités. Deux indicateurs sont calculés :

1. **Cluster principal** : Cluster contenant la majorité des modalités de la variable
2. **Pureté** : Proportion de modalités dans le cluster principal

$$\text{Pureté}(V_j) = \frac{\max_k \left|\{m \in V_j : c_m = k\}\right|}{|V_j|}$$

**Interprétation de la pureté** :

- **Pureté = 1.0** : Toutes les modalités de la variable dans un seul cluster → Variable homogène
- **Pureté = 0.5** : Modalités dispersées → Variable hétérogène
- **Pureté < 0.6** : Signal d'alerte pour variables à examiner

**Exemple** :

```
variable       n_modalites  cluster_principal  purity
Sexe                   2              1         1.00
Age_discretise         5              2         0.80
Region                10              3         0.40
```

**Lecture** :
- `Sexe` : Pureté parfaite → Les 2 modalités (Homme, Femme) sont dans le même cluster
- `Age_discretise` : 80% des 5 bins d'âge dans le cluster 2 → Structure d'âge assez homogène
- `Region` : 40% des 10 régions dans le cluster 3 → Forte dispersion géographique

### 4.2 Modalités d'une Variable : `get_modalities_of_variable()`

```r
get_modalities_of_variable = function(variable_name) {
  groupes <- self$Groupes
  noms <- names(groupes)
  
  pattern <- paste0("^", variable_name, "\\.")
  indices <- grep(pattern, noms)
  
  if (length(indices) == 0) {
    stop("Variable '", variable_name, "' non trouvée")
  }
  
  result <- data.frame(
    modalite = sub("^[^\\.]+\\.", "", noms[indices]),
    cluster = groupes[indices]
  )
}
```

Cette méthode détaille l'assignation de chaque modalité d'une variable spécifique, permettant une analyse fine de la structure capturée.

**Exemple d'utilisation** :

```r
model$get_modalities_of_variable("Region")

#   modalite  cluster
#   Nord          1
#   Sud           2
#   Est           2
#   Ouest         1
#   Centre        3
```

**Interprétation** : Les régions Nord et Ouest partagent le cluster 1, Sud et Est le cluster 2, tandis que Centre est isolé dans le cluster 3. Cela suggère une structure géographique sous-jacente capturée par le clustering.

### 4.3 Modalités d'un Cluster : `get_modalities_of_cluster()`

```r
get_modalities_of_cluster = function(k) {
  groupes <- self$Groupes
  noms <- names(groupes)[groupes == k]
  
  result <- data.frame(
    variable = sub("\\..*", "", noms),
    modalite = sub("^[^\\.]+\\.", "", noms)
  )
}
```

Cette méthode inverse permet d'examiner la **composition** d'un cluster en termes de modalités et de variables d'origine.

**Exemple** :

```r
model$get_modalities_of_cluster(1)

#   variable       modalite
#   Sexe           Homme
#   Age            bin1
#   Age            bin2
#   CSP            Cadre
#   Region         Nord
```

**Interprétation** : Le cluster 1 regroupe des hommes, jeunes (bins 1-2), cadres, du Nord. Profil socio-démographique cohérent suggérant un segment de "jeunes cadres urbains du Nord".

### 4.4 Vérification d'Intégrité : `check_results_integrity()`

```r
check_results_integrity = function() {
  groupes <- self$Groupes
  issues <- list()
  
  # Vérification 1 : Tous les clusters ont des modalités
  cluster_counts <- table(groupes)
  empty_clusters <- (1:self$K)[!1:self$K %in% as.integer(names(cluster_counts))]
  if (length(empty_clusters) > 0) {
    issues$empty_clusters <- empty_clusters
  }
  
  # Vérification 2 : Format des noms
  if (!all(grepl("\\.", names(groupes)))) {
    issues$invalid_names <- names(groupes)[!grepl("\\.", names(groupes))]
  }
  
  return(issues)
}
```

Cette méthode diagnostique assure la cohérence des résultats :

1. **Clusters vides** : Vérifie qu'aucun cluster n'est vide (peut arriver avec certaines valeurs de K)
2. **Format des noms** : Vérifie que chaque modalité suit le format `variable.modalite`

## 5. Fondements Mathématiques et Validations

### 5.1 Décomposition en Valeurs Singulières Pondérée

La SVD pondérée utilisée dans l'AFDM repose sur la décomposition :

$$\mathbf{D}_r^{1/2} (\mathbf{Z} - \mathbf{1} \mathbf{f}^T) \mathbf{D}_c^{1/2} = \mathbf{U} \boldsymbol{\Lambda} \mathbf{V}^T$$

où :
- $\mathbf{D}_r$ : Matrice diagonale des poids des observations (lignes)
- $\mathbf{D}_c$ : Matrice diagonale des poids des modalités (colonnes)
- $\mathbf{f}$ : Vecteur des fréquences marginales des modalités

Cette décomposition généralise simultanément l'ACP (pour les variables quantitatives) et l'ACM (pour les variables qualitatives).

### 5.2 Relation entre Inertie et Valeurs Propres

L'inertie totale du nuage de points (modalités) dans l'espace factoriel est :

$$I_{\text{tot}} = \sum_{i=1}^{p^*} \sum_{k=1}^{q} w_i (\phi_{ik} - \bar{\phi}_k)^2 = \sum_{k=1}^{q} \lambda_k$$

où $q$ est le rang de $\mathbf{Z}_c$ (nombre d'axes factoriels non triviaux).

La variance expliquée par les $q'$ premiers axes est :

$$\text{Var\%}_{1:q'} = \frac{\sum_{k=1}^{q'} \lambda_k}{I_{\text{tot}}} \times 100$$

Cette métrique guide le choix de `n_factors` : on retient typiquement les axes expliquant cumulativement 70-80% de l'inertie.

### 5.3 Propriétés du V de Cramer

Le V de Cramer vérifie les propriétés suivantes :

1. **Domaine** : $V \in [0, 1]$
2. **Indépendance** : $V = 0 \Leftrightarrow$ indépendance stricte
3. **Liaison parfaite** : $V = 1$ si et seulement si une liaison déterministe existe
4. **Invariance d'échelle** : Insensible à la taille d'échantillon (à distribution fixée)

La relation entre V et $\chi^2$ est :

$$V^2 = \frac{\chi^2}{n \times \min(r-1, c-1)}$$

Cette normalisation par $\min(r-1, c-1)$ compense l'effet du nombre de degrés de liberté, rendant V comparable entre tableaux de dimensions différentes.

### 5.4 Optimalité de la CAH avec Ward

Le critère de Ward minimise la **perte d'inertie intra-classe** à chaque fusion. Formellement, pour une partition $\mathcal{P} = \{C_1, \ldots, C_K\}$, l'inertie intra-classe est :

$$I_{\text{intra}}(\mathcal{P}) = \sum_{k=1}^{K} \sum_{i \in C_k} w_i \|\boldsymbol{\phi}_i - \mathbf{m}_k\|^2$$

où $\mathbf{m}_k = \frac{\sum_{i \in C_k} w_i \boldsymbol{\phi}_i}{\sum_{i \in C_k} w_i}$ est le centroïde pondéré du cluster $C_k$.

À chaque étape, Ward fusionne les clusters $C_i$ et $C_j$ minimisant :

$$\Delta I_{\text{intra}}(C_i, C_j) = I_{\text{intra}}(C_i \cup C_j) - [I_{\text{intra}}(C_i) + I_{\text{intra}}(C_j)]$$

Ce critère garantit la construction de clusters compacts dans l'espace factoriel.

### 5.5 Indice de Dice : Fondement Mathématique

Le coefficient de Dice peut s'exprimer en termes de cardinalités d'ensembles :

$$\text{Dice}(A, B) = \frac{2|A \cap B|}{|A| + |B|}$$

Cette formule peut être réécrite en termes de précision et de rappel :

$$\text{Dice}(A, B) = \frac{2 \cdot \text{Précision} \cdot \text{Rappel}}{\text{Précision} + \text{Rappel}}$$

où :
- $\text{Précision} = \frac{|A \cap B|}{|B|}$
- $\text{Rappel} = \frac{|A \cap B|}{|A|}$

Cela montre que Dice est la moyenne harmonique de la précision et du rappel, lui conférant des propriétés favorables pour les comparaisons d'ensembles asymétriques.

**Relation avec le F1-score** : En recherche d'information, le coefficient de Dice est équivalent au F1-score, une métrique largement utilisée pour la qualité de classification.

## 6. Complexité Algorithmique

### 6.1 Phase d'Apprentissage

| Étape | Complexité | Dominante pour |
|-------|------------|----------------|
| Tableau disjonctif | $O(np)$ | $n$ grand |
| AFDM (SVD) | $O(\min(n^2 p^*, np^{*2}))$ | $p^* > n$ |
| Distances | $O(p^{*2} q)$ | $p^*$ grand |
| CAH | $O(p^{*2} \log p^*)$ | $p^*$ modéré |

où :
- $n$ : Nombre d'observations
- $p$ : Nombre de variables d'origine
- $p^*$ : Nombre de modalités (typiquement $p^* = 2-10 \times p$)
- $q$ : Nombre d'axes factoriels retenus

**Complexité globale** : Dominée par la SVD, soit $O(\min(n^2 p^*, np^{*2}))$.

### 6.2 Phase de Prédiction

| Étape | Complexité |
|-------|------------|
| Assignation Dice | $O(n \times K \times p^*)$ |
| Tableau de contingence | $O(n \times m_{\text{illus}})$ |
| Test du Chi-deux | $O(r \times c)$ |

où :
- $m_{\text{illus}}$ : Nombre de modalités de la variable illustrative
- $r$ : Nombre de lignes du tableau de contingence
- $c = K$ : Nombre de clusters

**Complexité globale** : $O(n \times K \times p^*)$ pour l'assignation Dice, qui est linéaire en $n$ et typiquement efficace pour les applications pratiques.

## 7. Limitations et Extensions Possibles

### 7.1 Limitations Actuelles

1. **Sensibilité à la discrétisation** : Le choix de `n_bins` influence fortement les résultats pour les variables quantitatives. Une discrétisation trop grossière perd de l'information, une trop fine augmente la dimensionnalité et le bruit.

2. **Perte d'information ordinale** : La discrétisation transforme des variables continues en variables nominales, perdant la structure d'ordre. Des extensions (ACM ordinale) pourraient préserver cette information.

3. **Difficulté d'interprétation au niveau des variables** : Le clustering opérant sur les modalités, l'agrégation au niveau des variables (via `get_variable_summary()`) peut masquer des structures fines.

4. **Hypothèse d'indépendance conditionnelle** : L'AFDM suppose implicitement une forme d'indépendance locale entre modalités d'une même variable, ce qui peut être violé dans certains contextes.

5. **Méthode d'assignation unique** : Bien que l'indice de Dice soit bien adapté à la plupart des applications, certains contextes spécifiques pourraient bénéficier d'approches alternatives.

### 7.2 Perspectives d'Extension

#### Extension 1 : AFDM Ordinale

Intégrer des contraintes d'ordre pour les variables quantitatives discrétisées, en utilisant des codages ordinaux plutôt que nominaux. Cela préserverait la structure de rang des bins.

#### Extension 2 : Sélection Automatique de K

Implémenter des critères d'optimalité pour déterminer automatiquement le nombre de clusters :

- **Indice de Davies-Bouldin** : Minimise le ratio compacité/séparation
- **Silhouette** : Maximise la cohésion intra-cluster et séparation inter-clusters
- **Gap Statistic** : Compare l'inertie intra-classe observée à celle sous hypothèse nulle

#### Extension 3 : Approche Simultanée (Factorial K-Means)

Plutôt qu'une approche séquentielle (AFDM puis CAH), développer une approche simultanée optimisant conjointement la réduction de dimensionnalité et le clustering, à la manière du Factorial K-Means de Vichi & Kiers (2001).

#### Extension 4 : Robustification par Bootstrap

Pour chaque cluster, estimer des intervalles de confiance sur :
- La composition en modalités
- Les coordonnées du centroïde dans l'espace factoriel
- Le V de Cramer pour les variables illustratives
- Les distributions de scores Dice

Cela fournirait des mesures de stabilité des clusters et des intervalles de confiance pour les assignations.

#### Extension 5 : Variables Illustratives Quantitatives

Étendre `predict()` aux variables illustratives quantitatives, en calculant :
- Moyennes et écarts-types par cluster
- Tests ANOVA
- Coefficients de corrélation point-bisériale
- Tailles d'effet (d de Cohen, eta-carré)

#### Extension 6 : Visualisation Interactive

Développer des graphiques interactifs permettant de :
- Explorer le dendrogramme et visualiser l'effet de différentes coupes
- Projeter les clusters dans le premier plan factoriel avec étiquettes de modalités
- Afficher dynamiquement les modalités d'un cluster sélectionné
- Montrer les distributions de scores Dice pour l'évaluation de la confiance d'assignation

#### Extension 7 : Assignation Douce

Étendre l'assignation basée sur Dice pour fournir des appartenances probabilistes (soft) aux clusters plutôt que des assignations strictes (hard), en utilisant les scores Dice normalisés comme probabilités d'appartenance aux clusters.

## 8. Cas d'Usage et Applications

### 8.1 Segmentation Marketing

**Contexte** : Segmentation de clients sur la base de données socio-démographiques mixtes (âge, revenu, catégorie socio-professionnelle, région, situation familiale).

**Avantages de TandemVarClust** :
- Traitement naturel des types de données mixtes
- Clustering au niveau des modalités capturant des profils fins (ex : "Célibataires urbains 25-34 ans cadres" vs "Familles rurales 45-54 ans employés")
- Assignation basée sur Dice fournissant des scores de confiance pour l'appartenance de chaque client au segment
- Variables illustratives (comportement d'achat, satisfaction) pour valider la pertinence des segments

**Valeur de l'indice de Dice** : Les scores de confiance d'assignation permettent aux équipes marketing de :
- Identifier les clients avec une appartenance ambiguë au segment
- Prioriser les stratégies de ciblage en fonction de la confiance d'assignation
- Détecter les cas limites nécessitant une révision manuelle

### 8.2 Analyse de Questionnaires

**Contexte** : Enquête de satisfaction avec questions Likert (ordinales), questions ouvertes recodées (nominales), et quelques métriques quantitatives (âge, ancienneté).

**Avantages de TandemVarClust** :
- Identification de patterns de réponses cohérents au niveau des modalités
- Clustering des modalités révélant des profils de répondants
- Analyse des libres commentaires (variables illustratives) pour enrichir l'interprétation
- Confiance d'assignation aidant à identifier les répondants avec des patterns de réponse incohérents ou atypiques

### 8.3 Épidémiologie et Santé Publique

**Contexte** : Étude des déterminants de la santé à partir de données individuelles hétérogènes (antécédents médicaux, habitudes de vie, facteurs socio-économiques).

**Avantages de TandemVarClust** :
- Intégration de variables binaires (présence/absence de pathologies), ordinales (fréquence de consommation), et continues (IMC, âge)
- Identification de syndromes de facteurs de risque (combinaisons de modalités à risque)
- Variables illustratives (survenue d'événements de santé) pour valider les facteurs de risque
- Scores Dice permettant l'identification de patients avec des profils de risque peu clairs nécessitant un suivi plus étroit

### 8.4 Analyse de Données Textuelles Codées

**Contexte** : Analyse de contenu d'entretiens ou de documents, avec variables décrivant la présence de thèmes (binaire), intensité de tonalité (ordinale), et métadonnées (nominale et continue).

**Avantages de TandemVarClust** :
- Typologie de documents basée sur leurs caractéristiques sémantiques et contextuelles
- Clustering fin au niveau des modalités révélant des co-occurrences thématiques
- Variables illustratives (auteur, date, source) pour contextualiser les clusters
- Confiance d'assignation aidant à identifier les documents avec des thèmes mixtes ou ambigus

## 9. Références Théoriques

### 9.1 Analyse Factorielle de Données Mixtes (AFDM)

- **Escofier, B.** (1979). *Traitement simultané de variables quantitatives et qualitatives*. Les Cahiers de l'Analyse des Données, IV(2), 137-146.
  > Article fondateur introduisant le principe de l'analyse simultanée de variables mixtes dans le cadre de l'ACM.

- **Pagès, J.** (2004). *Analyse factorielle de données mixtes*. Revue de Statistique Appliquée, 52(4), 93-111.
  > Article de référence consolidant les travaux antérieurs et démontrant l'équivalence entre les approches.

- **Husson, F., Lê, S., & Pagès, J.** (2017). *Exploratory Multivariate Analysis by Example Using R* (2e éd.). CRC Press.
  > Guide pratique des méthodes d'analyse multivariée avec R et FactoMineR, incluant l'AFDM.

### 9.2 Clustering Tandem et Approches Séquentielles

- **Arabie, P., & Hubert, L. J.** (1994). *Cluster analysis in marketing research*. In: *Advanced Methods of Marketing Research* (R. P. Bagozzi, Ed.), Blackwell, 160-189.
  > Critique de l'approche tandem séquentielle et introduction du terme "tandem analysis".

- **Vichi, M., & Kiers, H. A. L.** (2001). *Factorial k-means analysis for two-way data*. Computational Statistics & Data Analysis, 37(1), 49-64.
  > Proposition du Factorial K-means (FKM), généralisant RKM. Comparaison théorique et empirique avec l'approche tandem.

### 9.3 Indices de Similarité

- **Dice, L. R.** (1945). *Measures of the amount of ecologic association between species*. Ecology, 26(3), 297-302.
  > Article original introduisant le coefficient de Dice pour mesurer la similarité entre ensembles.

- **Kaufman, L., & Rousseeuw, P. J.** (1990). *Finding Groups in Data: An Introduction to Cluster Analysis*. Wiley.
  > Ouvrage de référence sur le clustering, incluant la discussion des indices de similarité pour données catégorielles.

- **Tan, P. N., Steinbach, M., & Kumar, V.** (2005). *Introduction to Data Mining*. Pearson.
  > Traitement complet des mesures de similarité incluant Dice, Jaccard, et leurs applications.

### 9.4 Classification Ascendante Hiérarchique

- **Ward, J. H., Jr.** (1963). *Hierarchical grouping to optimize an objective function*. Journal of the American Statistical Association, 58(301), 236-244.
  > Article original proposant le critère de Ward pour la CAH.

- **Murtagh, F., & Contreras, P.** (2012). *Algorithms for hierarchical clustering: An overview*. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 2(1), 86-97.
  > Revue des méthodes de CAH avec analyse de complexité.

### 9.5 Tests d'Association pour Variables Qualitatives

- **Cramér, H.** (1946). *Mathematical Methods of Statistics*. Princeton University Press.
  > Ouvrage classique introduisant le coefficient V de Cramér.

- **Agresti, A.** (2013). *Categorical Data Analysis* (3e éd.). Wiley.
  > Référence moderne sur l'analyse de données catégorielles, incluant les tests du Chi² et les mesures d'association.

### 9.6 Éléments Supplémentaires en Analyse Factorielle

- **Greenacre, M.** (2007). *Correspondence Analysis in Practice* (2e éd.). Chapman & Hall/CRC.
  > Traitement complet de l'analyse des correspondances incluant les éléments supplémentaires.

- **Le Roux, B., & Rouanet, H.** (2004). *Geometric Data Analysis: From Correspondence Analysis to Structured Data Analysis*. Kluwer Academic Publishers.
  > Approche géométrique de l'analyse de données, incluant le traitement des individus supplémentaires et la caractérisation de classes.

## 10. Conclusion

L'implémentation de `TandemVarClust` offre une approche focalisée et théoriquement solide pour le clustering de modalités dans le contexte de données mixtes. En combinant l'AFDM pour la réduction de dimensionnalité, la CAH pour le clustering, et l'indice de Dice pour l'assignation des observations, cette méthode fournit :

1. **Traitement naturel des données hétérogènes** : Variables quantitatives et qualitatives dans un même cadre
2. **Capture de structures fines** : Clustering au niveau des modalités révélant des patterns que le clustering de variables masquerait
3. **Interprétabilité riche** : Via les méthodes utilitaires d'agrégation par variable et d'analyse de variables illustratives
4. **Rigueur méthodologique** : L'indice de Dice fournit une méthode d'assignation mathématiquement solide et bien établie
5. **Flexibilité** : Paramètres de régularisation (`n_factors`, `n_bins`) permettant d'adapter la méthode au contexte
6. **Confiance d'assignation** : Les scores Dice permettant l'évaluation de la qualité des assignations aux clusters

Le choix de l'indice de similarité de Dice comme unique méthode d'assignation est justifié par :
- Son fondement théorique solide dans la mesure de similarité d'ensembles
- Son traitement équilibré des tailles d'observation et de cluster
- Son interprétabilité naturelle comme proportion de chevauchement
- Son historique éprouvé en écologie, recherche d'information, et analyse de données catégorielles
- Sa fourniture de scores de confiance continus pour les assignations

La méthode s'inscrit dans la tradition de l'école française d'analyse des données, privilégiant l'exploration et l'interprétation sur la modélisation paramétrique. Elle constitue un outil précieux pour les praticiens confrontés à des données réelles complexes et hétérogènes, notamment dans les domaines du marketing, des sciences sociales, de la santé publique et de l'analyse textuelle.

La focalisation de l'implémentation sur une méthode d'assignation unique et bien justifiée améliore :
- La maintenabilité et la clarté du code
- L'expérience utilisateur (pas de choix de méthode requis)
- La reproductibilité des analyses
- La cohérence théorique de l'approche globale

---

**Document rédigé dans le cadre du développement du package R `RollerClustR`**  
**Auteur** : Romain Buono
**Date** : Novembre 2025  
**Licence** : MIT