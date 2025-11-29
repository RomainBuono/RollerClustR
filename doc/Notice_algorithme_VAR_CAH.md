# Implémentation de l'Algorithme VAR-CAH dans la Classe VAR_CAH

## 1. Fondements Théoriques

### 1.1 Principe Général

L'implémentation proposée réalise un clustering hiérarchique de variables quantitatives selon une approche VAR-CAH (Variables - Classification Ascendante Hiérarchique). L'objectif consiste à regrouper des variables présentant de fortes corrélations en ensembles homogènes, puis à synthétiser l'information de chaque groupe par une variable latente unidimensionnelle.

Cette approche s'inscrit dans le paradigme de la réduction de dimensionnalité supervisée, où la structure de corrélation entre variables guide la formation des groupes, et où chaque groupe est ensuite représenté par sa composante principale dominante.

### 1.2 Architecture Logicielle

La classe `VAR_CAH` hérite de la classe abstraite `ClusterAnalysis` et implémente une architecture R6 garantissant l'encapsulation des données et la réutilisabilité des méthodes. Les attributs privés assurent la persistance des résultats intermédiaires nécessaires aux prédictions ultérieures.

## 2. Traduction Algorithmique

### 2.1 Phase de Prétraitement

#### Nettoyage des Données
```r
X_clean <- super$fit(X)
private$FVarNames <- names(X_clean)
```

La méthode héritée `super$fit()` réalise la validation du jeu de données, incluant la vérification du type des variables et le traitement des valeurs manquantes selon la stratégie définie (`na_action`). Seules les variables numériques sont conservées pour l'analyse, conformément aux exigences de l'analyse en composantes principales ultérieure.

### 2.2 Construction de la Matrice de Dissimilarité

#### Calcul de la Matrice de Corrélation
```r
cor_matrix <- cor(X_clean, use = "pairwise.complete.obs")
dist_vars <- as.dist(1 - abs(cor_matrix))
```

La dissimilarité entre variables est définie par la transformation :

$$d(X_i, X_j) = 1 - |\rho(X_i, X_j)|$$

où $\rho(X_i, X_j)$ désigne le coefficient de corrélation de Pearson entre les variables $X_i$ et $X_j$. L'utilisation de la valeur absolue garantit que des variables fortement corrélées négativement sont considérées comme similaires, ce qui est pertinent dans un contexte de synthèse dimensionnelle.

L'option `use = "pairwise.complete.obs"` permet une estimation robuste en présence de données manquantes, en calculant chaque corrélation sur l'ensemble des observations disponibles pour la paire de variables considérée.

### 2.3 Classification Hiérarchique Ascendante

#### Construction du Dendrogramme
```r
private$FArbre <- hclust(dist_vars, method = "ward.D2")
groupes_vars <- cutree(private$FArbre, k = private$FNbGroupes)
```

La classification ascendante hiérarchique est réalisée selon le critère de Ward, qui minimise l'inertie intra-classe à chaque étape de fusion. Plus formellement, à chaque itération, on agrège les clusters $C_i$ et $C_j$ qui minimisent l'augmentation de variance :

$$\Delta(C_i, C_j) = \frac{n_i n_j}{n_i + n_j} \|m_i - m_j\|^2$$

Où :

- $\|m_i - m_j\|^2$ (Distance entre centres) : représente le carré de la distance euclidienne entre le centroïde $m_i$ du cluster $C_i$ et le centroïde $m_j$ du cluster $C_j$. C'est la source principale de l'augmentation d'inertie.

- $\frac{n_i n_j}{n_i + n_j}$ (Facteur de pondération) : Ce terme pondère la distance entre les centres par la taille relative des clusters ($n_i$ et $n_j$ sont les effectifs, c'est-à-dire le nombre de variables dans chaque cluster).

La partition finale est obtenue par coupure de l'arbre hiérarchique au niveau $k$ spécifié, produisant $k$ groupes de variables disjoints.

### 2.4 Synthèse par Composante Principale

#### Extraction de la Variable Latente
```r
pca_synthetique = function(X_cluster) {
  X_clean <- na.omit(X_cluster)
  pca_res <- prcomp(X_clean, center = TRUE, scale. = private$FScale)
  full_synthetique <- rep(NA_real_, nrow(X_cluster))
  full_synthetique[as.numeric(rownames(X_clean))] <- pca_res$x[, 1, drop = TRUE]
  
  return(list(
    synthetic = full_synthetique,
    loadings = pca_res$rotation[, 1],
    center = pca_res$center,
    scale = pca_res$scale,
    variance_explained = pca_res$sdev[1]^2 / sum(pca_res$sdev^2),
    n_vars = ncol(X_clean)
  ))
}
```

Pour chaque cluster $C_k$ de variables, une analyse en composantes principales est effectuée. La première composante principale (PC1) est retenue comme variable synthétique $S_k$, celle-ci maximisant la variance expliquée dans l'espace des variables du cluster.

Mathématiquement, $S_k$ est définie par :

$$S_k = \sum_{j \in C_k} w_j X_j$$

où les poids $w_j$ correspondent au premier vecteur propre de la matrice de covariance (ou corrélation si standardisation) des variables de $C_k$.

#### Gestion des Cas Particuliers

Lorsqu'un cluster ne contient qu'une seule variable, celle-ci est directement utilisée comme variable synthétique, avec une variance expliquée fixée à 1.0 par convention. Cette approche évite les complications numériques liées à l'ACP sur un espace unidimensionnel.

#### Persistance des Paramètres

```r
pca_params[[k_idx]] <- list(
  loadings = pca_result$loadings,
  center = pca_result$center,
  scale = pca_result$scale,
  variance_explained = pca_result$variance_explained,
  variables = vars_in_cluster
)
private$FPCAParams <- pca_params
```

Les paramètres de l'ACP (loadings, paramètres de centrage et réduction) sont stockés pour chaque cluster. Cette persistance est essentielle pour la projection ultérieure de nouvelles variables sans nécessiter un réajustement complet du modèle.

### 2.5 Évaluation de l'Homogénéité

#### Mesure de Cohésion Intra-Cluster
```r
for (i in 1:length(vars_in_cluster)) {
  var_name <- vars_in_cluster[i]
  cor_val <- cor(X_cluster[, var_name], pca_result$synthetic, 
                 use = "pairwise.complete.obs")
  correlations[match(var_name, private$FVarNames)] <- abs(cor_val)
}
```

Pour chaque variable $X_j$ appartenant au cluster $C_k$, on calcule sa corrélation avec la variable synthétique $S_k$ :

$$r_{jk} = \text{cor}(X_j, S_k)$$

Cette corrélation mesure dans quelle proportion la variable $X_j$ est expliquée par la composante synthétique de son cluster. Une corrélation élevée indique une forte cohésion entre la variable et son groupe.

#### Indices Globaux de Qualité
```r
qualite <- tapply(correlations, groupes_vars, mean, na.rm = TRUE)
private$FQualiteClusters <- qualite
private$FHomogeneite <- sum(correlations, na.rm = TRUE) / n_vars
```

Deux indices de qualité sont calculés :

1. **Homogénéité par cluster** : moyenne des corrélations des variables avec leur synthétique respective
$$H(C_k) = \frac{1}{|C_k|} \sum_{j \in C_k} |r_{jk}|$$

2. **Homogénéité globale** : moyenne de toutes les corrélations sur l'ensemble des variables
$$H_{global} = \frac{1}{p} \sum_{k=1}^{K} \sum_{j \in C_k} |r_{jk}|$$

où $p$ désigne le nombre total de variables et $K$ le nombre de clusters.

### 2.6 Reconfiguration Dynamique

#### Modification du Nombre de Clusters
```r
refit_with_k = function(new_k) {
  if (is.null(private$FArbre)) {
    stop("Le modèle n'est pas encore ajusté. Lancez $fit() d'abord.")
  }
  private$FNbGroupes <- new_k
  groupes_vars <- cutree(private$FArbre, k = new_k)
  private$FGroupes <- groupes_vars
  private$evaluate_clusters()
}
```

Cette méthode permet de modifier le nombre de clusters sans réexécuter l'intégralité de l'algorithme. Le dendrogramme préalablement construit est simplement coupé à un nouveau niveau, puis les variables synthétiques et métriques de qualité sont recalculées. Cette approche tire parti de la nature hiérarchique de la méthode, où l'arbre de classification encode toutes les partitions possibles.

## 3. Prédiction de Nouvelles Variables

### 3.1 Principe de Projection

La méthode `predict_variable()` constitue une extension innovante permettant d'assigner de nouvelles variables à des clusters préexistants sans modifier la structure du clustering initial. Cette fonctionnalité repose sur le calcul d'un score de similarité entre chaque nouvelle variable et les clusters établis.

### 3.2 Méthode par Corrélation avec les Synthétiques

#### Implémentation
```r
score_correlation = function(new_var, cluster_id) {
  synthetic <- private$FSynthetiques[, cluster_id]
  abs(cor(new_var, synthetic, use = "pairwise.complete.obs"))
}
```

Le score d'appartenance d'une nouvelle variable $Y$ au cluster $C_k$ est calculé comme la valeur absolue de la corrélation entre $Y$ et la variable synthétique $S_k$ :

$$\text{score}_{\text{corr}}(Y, C_k) = |\text{cor}(Y, S_k)|$$

**Avantages** :
- Efficacité computationnelle : $O(n)$ par cluster, où $n$ est le nombre d'observations
- Interprétation directe : mesure l'alignement avec la dimension principale du cluster
- Cohérence avec la logique de construction des clusters (basée sur la première composante)

**Contexte d'application optimal** :
- Clusters bien représentés par leur première composante principale (variance expliquée élevée)
- Besoin d'une assignation rapide pour un grand nombre de nouvelles variables

### 3.3 Méthode par Distance Moyenne

#### Implémentation
```r
score_distance = function(new_var, cluster_id) {
  cluster_vars <- names(private$FGroupes)[private$FGroupes == cluster_id]
  X_cluster <- private$FX[, cluster_vars, drop = FALSE]
  cors <- sapply(1:ncol(X_cluster), function(i) {
    abs(cor(new_var, X_cluster[, i], use = "pairwise.complete.obs"))
  })
  score <- mean(cors, na.rm = TRUE)
}
```

Le score d'appartenance est ici calculé comme la moyenne des corrélations de la nouvelle variable avec toutes les variables du cluster :

$$\text{score}(Y, C_k) = \frac{1}{|C_k|} \sum_{j \in C_k} |\text{cor}(Y, X_j)|$$

Cette méthode évalue la proximité globale de la nouvelle variable avec l'ensemble du cluster, plutôt qu'avec sa seule représentation synthétique.

**Avantages** :
- Robustesse aux clusters déséquilibrés ou hétérogènes
- Capture une notion de distance plus globale dans l'espace des variables
- Moins sensible à une faible variance expliquée par PC1

**Contexte d'application optimal** :
- Clusters présentant une structure multidimensionnelle complexe
- Situations où la première composante principale n'est pas suffisamment discriminante

### 3.4 Assignation et Restitution des Résultats

```r
assigned_clusters <- apply(scores_matrix, 1, which.max)
names(assigned_clusters) <- colnames(new_variables)

if (return_scores) {
  result <- as.data.frame(scores_matrix)
  result$assigned_cluster <- assigned_clusters
  return(result)
} else {
  return(assigned_clusters)
}
```

Chaque nouvelle variable est assignée au cluster pour lequel le score calculé est maximal. L'utilisateur peut choisir entre :
- Un vecteur d'assignations (mode par défaut)
- Une matrice complète des scores pour tous les clusters (mode diagnostique)

La restitution des scores complets permet d'évaluer l'ambiguïté de l'assignation : des scores similaires pour plusieurs clusters suggèrent une variable à la frontière entre groupes.

## 4. Méthodes Utilitaires

### 4.1 Extraction de Variables par Cluster
```r
get_cluster_variables = function(cluster_id) {
  vars <- names(private$FGroupes)[private$FGroupes == cluster_id]
  return(vars)
}
```

Cette méthode retourne l'ensemble des variables appartenant à un cluster spécifié.

### 4.2 Identification de la Variable Représentative
```r
get_representative_variable = function(cluster_id) {
  vars <- self$get_cluster_variables(cluster_id)
  cors <- private$FCorrelations[match(vars, private$FVarNames)]
  return(vars[which.max(cors)])
}
```

Pour un cluster donné, la variable présentant la corrélation maximale avec la variable synthétique est identifiée comme représentative. Cette variable peut être utilisée comme proxy simplifié du cluster dans des analyses ultérieures.

### 4.3 Accès au Dendrogramme
```r
get_tree = function() {
  return(private$FArbre)
}
```

L'objet `hclust` sous-jacent est accessible pour visualisation (dendrogramme) ou analyses complémentaires de la structure hiérarchique.

## 5. Fondements Mathématiques de la Méthode

### 5.1 Justification de la Distance Basée sur la Corrélation

La distance $d(X_i, X_j) = 1 - |\rho(X_i, X_j)|$ satisfait les propriétés suivantes :

1. **Positivité** : $0 \leq d(X_i, X_j) \leq 2$
2. **Symétrie** : $d(X_i, X_j) = d(X_j, X_i)$
3. **Séparabilité** : $d(X_i, X_i) = 0$

Bien que cette métrique ne satisfasse pas strictement l'inégalité triangulaire, elle constitue une mesure de dissimilarité pertinente dans le contexte du clustering de variables, où l'objectif est de regrouper des variables colinéaires.

### 5.2 Optimalité de la Première Composante Principale

La première composante principale $S_k$ d'un cluster $C_k$ est solution du problème d'optimisation :

$$\max_{\|w\|=1} \text{Var}\left(\sum_{j \in C_k} w_j X_j\right)$$

Elle représente donc la combinaison linéaire des variables du cluster qui maximise la variance expliquée. Cette propriété garantit que $S_k$ constitue la meilleure synthèse unidimensionnelle de l'information contenue dans $C_k$ au sens des moindres carrés.

### 5.3 Interprétation de l'Homogénéité

L'indice d'homogénéité $H(C_k)$ quantifie la capacité de la variable synthétique à représenter les variables du cluster. Une valeur proche de 1 indique que $S_k$ capture efficacement la structure du cluster, suggérant une forte unidimensionnalité du groupe. Inversement, une homogénéité faible peut signaler la présence de sous-structures non capturées par la partition à $k$ clusters.

## 6. Complexité Algorithmique

### 6.1 Phase d'Apprentissage

1. **Calcul de la matrice de corrélation** : $O(p^2 n)$ où $p$ est le nombre de variables et $n$ le nombre d'observations
2. **Classification hiérarchique** : $O(p^2 \log p)$
3. **ACP par cluster** : $O\left(\sum_{k=1}^K p_k^2 n\right)$ où $p_k$ est le nombre de variables dans le cluster $k$

La complexité globale est dominée par le calcul de la matrice de corrélation initiale, soit $O(p^2 n)$.

### 6.2 Phase de Prédiction

- **Méthode "correlation"** : $O(Kn)$ où $K$ est le nombre de clusters
- **Méthode "distance"** : $O(pn)$ dans le pire cas (si un cluster contient toutes les variables)

La méthode par corrélation est donc plus efficace pour un grand nombre de variables distribuées sur plusieurs clusters.

## 7. Limitations et Extensions Possibles

### 7.1 Limitations Actuelles

1. **Variables quantitatives uniquement** : L'approche repose sur la corrélation de Pearson et l'ACP, limitant son application aux données numériques continues.

2. **Sensibilité aux valeurs aberrantes** : La corrélation de Pearson étant sensible aux observations extrêmes, la structure de clustering peut être affectée par la présence d'outliers.

3. **Hypothèse de linéarité** : Les relations non-linéaires entre variables ne sont pas capturées par la corrélation de Pearson.

### 7.2 Perspectives d'Extension

1. **Support des variables catégorielles** : Intégration de l'approche PCAMIX pour traiter conjointement variables quantitatives et qualitatives.

2. **Algorithme K-means itératif** : Implémentation d'une variante par réallocation itérative, potentiellement plus efficace sur de grands ensembles de variables.

3. **Sélection automatique du nombre de clusters** : Intégration de critères d'optimalité (silhouette, gap statistic) ou d'approches par bootstrap pour déterminer $k$ de manière objective.

4. **Robustification** : Utilisation de corrélations robustes (Spearman, Kendall) ou de méthodes de détection d'outliers préalables.

## 8. Cas d'Usage et Applications

### 8.1 Réduction de Dimensionnalité

Dans des contextes de haute dimensionnalité, cette approche permet de remplacer un ensemble de $p$ variables corrélées par $k$ variables synthétiques non corrélées, facilitant les analyses ultérieures (régression, classification).

### 8.2 Feature Engineering

La méthode `predict_variable()` offre un cadre formel pour évaluer l'intérêt de nouvelles variables dérivées. Des scores faibles sur tous les clusters suggèrent qu'une variable apporte une information complémentaire non redondante avec les variables existantes.

### 8.3 Détection de Redondance

L'identification de variables fortement représentatives dans chaque cluster permet de sélectionner un sous-ensemble parcimonieux de variables pour la modélisation, réduisant la multicolinéarité et les coûts de collecte de données.

### 8.4 Monitoring et Maintenance de Modèles

Dans des contextes de production, l'assignation incrémentale de nouvelles variables collectées permet de maintenir une cohérence avec la structure de clustering établie, sans nécessiter un réapprentissage complet.

## 9. Références Théoriques

L'implémentation s'inspire des travaux suivants :

- **Chavent et al. (2012)** : Formalisation de l'algorithme de clustering de variables et développement de méthodes VAR-CAH.
- **Vigneau & Qannari (2003)** : Fondements théoriques du clustering de variables par composantes principales.
- **Ward (1963)** : Méthode de classification hiérarchique par minimisation de l'inertie intra-classe.

La contribution distinctive de cette implémentation réside dans l'extension par la méthode `predict_variable()`, offrant une fonctionnalité de projection non disponible dans les implémentations standards.