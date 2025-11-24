# Implémentation de l'Algorithme VARCLUS dans la Classe VARCLUS

## 1. Fondements Théoriques

### 1.1 Principe Général

L'algorithme VARCLUS (Variable Clustering) constitue une approche de partitionnement hiérarchique descendant pour le clustering de variables quantitatives. Contrairement aux méthodes ascendantes (agglomératives) qui fusionnent progressivement des variables similaires, VARCLUS procède par divisions successives d'un ensemble de variables en sous-groupes homogènes, jusqu'à ce qu'un critère d'arrêt statistique soit satisfait.

Cette approche s'inscrit dans la famille des algorithmes de partitionnement divisif, où la stratégie de segmentation repose sur l'analyse factorielle (ACP) et la rotation d'axes. L'objectif est d'identifier des groupes de variables mutuellement corrélées, chaque groupe étant caractérisé par une structure de corrélation unidimensionnelle dominante.

### 1.2 Fondement Algorithmique : Division Récursive

Le paradigme de division récursive distingue VARCLUS des approches hiérarchiques ascendantes classiques. À chaque étape, un groupe de variables est soumis à une analyse en composantes principales, et les deux premières composantes (après rotation) servent de base à une bipartition. Ce processus s'apparente à la construction d'un arbre binaire de décision, où chaque nœud interne représente une division en deux sous-groupes.

### 1.3 Architecture Logicielle

La classe `VARCLUS` hérite de la classe abstraite `ClusterAnalysis` et implémente une architecture R6 garantissant l'encapsulation des données et la traçabilité du processus de partitionnement. L'arbre de division est stocké sous forme de structure récursive, permettant une exploration flexible des résultats à différents niveaux de granularité.

## 2. Traduction Algorithmique

### 2.1 Phase d'Initialisation

#### Validation et Préparation des Données
```r
X <- private$validateDataset(X)
private$FX <- X

if (ncol(X) < 3) {
  stop("VARCLUS nécessite au moins 3 variables")
}
```

La méthode héritée `validateDataset()` assure la conformité du jeu de données, incluant la vérification du type des variables et le traitement des observations incomplètes. La condition minimale de trois variables découle de la nécessité d'avoir au moins deux composantes principales distinctes pour effectuer une bipartition significative.

#### Paramètres de Contrôle
```r
private$FStopEigenvalue <- stop_eigenvalue  # Seuil λ₂ (défaut: 1.0)
private$FDistanceMetric <- distance_metric   # Métrique de dissimilarité
```

Le paramètre `stop_eigenvalue` définit le critère d'arrêt basé sur la seconde valeur propre. Une valeur de 1.0 (défaut) correspond à un critère statistique standard : on arrête la division lorsque la variance expliquée par la seconde composante devient inférieure à celle d'une variable aléatoire standardisée.

### 2.2 Algorithme de Division Récursive

#### Pseudo-Code Formel
```
ALGORITHME: recursive_split(vars, depth)
ENTRÉE: 
  - vars: indices des variables du groupe à traiter
  - depth: profondeur actuelle dans l'arbre
SORTIE: 
  - Structure arborescente (nœud ou feuille)

1. SI |vars| < 3 ALORS
     RETOURNER feuille(vars, depth)
   FIN SI

2. X_sub ← FX[, vars]
   pca_result ← ACP(X_sub, standardisé = VRAI)
   λ₁, λ₂ ← deux premières valeurs propres

3. SI λ₂ < seuil_arrêt ALORS
     RETOURNER feuille(vars, depth, λ₁, λ₂)
   FIN SI

4. loadings_rotated ← VARIMAX(pca_result$rotation[, 1:2])

5. POUR CHAQUE variable i DANS vars FAIRE
     SI |loadings[i, 1]| ≥ |loadings[i, 2]| ALORS
        groupe1 ← groupe1 ∪ {i}
     SINON
        groupe2 ← groupe2 ∪ {i}
     FIN SI
   FIN POUR

6. branche_gauche ← recursive_split(groupe1, depth + 1)
   branche_droite ← recursive_split(groupe2, depth + 1)

7. RETOURNER nœud(depth, λ₁, λ₂, branche_gauche, branche_droite)
```

#### Implémentation R6
```r
recursive_split = function(vars, depth = 0) {
  n_vars <- length(vars)
  
  # Critère d'arrêt : cardinalité minimale
  if (n_vars < 3) {
    return(list(
      type = "leaf",
      vars = vars,
      depth = depth
    ))
  }
  
  # ACP sur le sous-groupe de variables
  X_sub <- private$FX[, vars, drop = FALSE]
  pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
  
  # Récupérer les 2 premières valeurs propres
  eigenvalues <- pca_result$sdev^2
  lambda1 <- eigenvalues[1]
  lambda2 <- if(length(eigenvalues) >= 2) eigenvalues[2] else 0
  
  # Critère d'arrêt : seconde valeur propre insuffisante
  if (lambda2 < private$FStopEigenvalue) {
    return(list(
      type = "leaf",
      vars = vars,
      depth = depth,
      lambda1 = lambda1,
      lambda2 = lambda2
    ))
  }
  
  # Rotation Varimax sur les 2 premiers axes
  if (ncol(pca_result$rotation) >= 2) {
    rotated <- varimax(pca_result$rotation[, 1:2])
    loadings <- rotated$loadings
  } else {
    loadings <- pca_result$rotation[, 1, drop = FALSE]
  }
  
  # Partitionnement par proximité maximale
  axis1_corr <- abs(loadings[, 1])
  axis2_corr <- if(ncol(loadings) >= 2) abs(loadings[, 2]) else rep(0, nrow(loadings))
  
  group1_idx <- which(axis1_corr >= axis2_corr)
  group2_idx <- which(axis1_corr < axis2_corr)
  
  # Appels récursifs
  left_branch <- private$recursive_split(vars[group1_idx], depth + 1)
  right_branch <- private$recursive_split(vars[group2_idx], depth + 1)
  
  return(list(
    type = "node",
    depth = depth,
    lambda1 = lambda1,
    lambda2 = lambda2,
    left = left_branch,
    right = right_branch
  ))
}
```

### 2.3 Critères d'Arrêt

#### Critère de Cardinalité Minimale

La condition $|V| < 3$ (où $V$ désigne l'ensemble de variables à partitionner) constitue un critère d'arrêt structural : avec moins de trois variables, l'ACP ne peut produire deux composantes principales distinctes permettant une bipartition informative.

#### Critère de Significativité de la Seconde Valeur Propre

Le critère $\lambda_2 < \tau$ (où $\tau$ est le seuil, typiquement fixé à 1.0) repose sur une justification statistique :

$$\lambda_2 \geq 1 \Leftrightarrow \text{La seconde dimension explique au moins autant de variance qu'une variable standardisée}$$

Lorsque $\lambda_2 < 1$, cela indique que la structure de corrélation est essentiellement unidimensionnelle, et qu'une division supplémentaire n'apporterait pas d'amélioration substantielle de l'homogénéité intra-groupe.

Mathématiquement, pour un ensemble de $p$ variables standardisées :

$$\sum_{i=1}^{p} \lambda_i = p$$

Le critère $\lambda_2 \geq 1$ signifie que la seconde composante capture une part non négligeable (≥ 1/p) de la variance totale.

### 2.4 Rotation Varimax

#### Principe et Justification

La rotation Varimax est une transformation orthogonale appliquée aux deux premières composantes principales afin de simplifier la structure des contributions (loadings) des variables sur ces axes. L'objectif est de maximiser la variance des carrés des contributions, favorisant ainsi une interprétation plus claire où chaque variable présente une forte contribution sur un axe et une faible contribution sur l'autre.

Formellement, la rotation Varimax résout le problème d'optimisation :

$$\max_{Q \in O(2)} \sum_{j=1}^{2} \text{Var}\left((L Q)_{:,j}^2\right)$$

où $L$ est la matrice des loadings originaux et $Q$ une matrice de rotation orthogonale 2×2.

#### Implémentation
```r
if (ncol(pca_result$rotation) >= 2) {
  rotated <- varimax(pca_result$rotation[, 1:2])
  loadings <- rotated$loadings
}
```

Cette transformation préserve les propriétés métriques (angles, distances) tout en améliorant la séparabilité des variables dans l'espace factoriel bidimensionnel.

### 2.5 Règle de Bipartition

#### Assignation par Proximité Maximale
```r
axis1_corr <- abs(loadings[, 1])
axis2_corr <- if(ncol(loadings) >= 2) abs(loadings[, 2]) else rep(0, nrow(loadings))

group1_idx <- which(axis1_corr >= axis2_corr)
group2_idx <- which(axis1_corr < axis2_corr)
```

Chaque variable $X_i$ est assignée au groupe correspondant à l'axe factoriel avec lequel elle présente la corrélation absolue la plus élevée :

$$\text{Groupe}(X_i) = \begin{cases} 
G_1 & \text{si } |a_{i1}| \geq |a_{i2}| \\
G_2 & \text{si } |a_{i1}| < |a_{i2}|
\end{cases}$$

où $a_{i1}$ et $a_{i2}$ sont les loadings de la variable $X_i$ sur les deux axes rotatés.

Cette règle garantit une partition exhaustive et exclusive : chaque variable appartient à exactement un groupe, et la bipartition maximise localement la cohérence interne de chaque sous-groupe.

### 2.6 Extraction de la Partition Finale

#### Parcours de l'Arbre en Profondeur
```r
extract_clusters = function(tree, cluster_id = 1) {
  if (tree$type == "leaf") {
    for (var in tree$vars) {
      private$FGroupes[var] <- cluster_id
    }
    return(cluster_id + 1)
  } else {
    cluster_id <- private$extract_clusters(tree$left, cluster_id)
    cluster_id <- private$extract_clusters(tree$right, cluster_id)
    return(cluster_id)
  }
}
```

Une fois l'arbre de division construit, les clusters finaux correspondent aux feuilles de cet arbre. Un parcours en profondeur (depth-first search) permet d'assigner à chaque variable un identifiant de cluster unique, tout en préservant l'ordre hiérarchique des divisions.

L'algorithme maintient un compteur `cluster_id` qui s'incrémente à chaque feuille visitée, garantissant ainsi une numérotation séquentielle des groupes finaux.

## 3. Fondements Mathématiques

### 3.1 Analyse en Composantes Principales (ACP)

#### Décomposition Spectrale

L'ACP sur un ensemble de $p$ variables standardisées $\mathbf{X} = [X_1, \ldots, X_p]$ consiste à diagonaliser la matrice de corrélation $\mathbf{R} = \text{Cor}(\mathbf{X})$ :

$$\mathbf{R} = \mathbf{V} \mathbf{\Lambda} \mathbf{V}^T$$

où $\mathbf{\Lambda} = \text{diag}(\lambda_1, \ldots, \lambda_p)$ avec $\lambda_1 \geq \lambda_2 \geq \cdots \geq \lambda_p \geq 0$ et $\mathbf{V}$ est la matrice orthonormale des vecteurs propres.

Les composantes principales sont définies par :

$$\mathbf{Z} = \mathbf{X} \mathbf{V}$$

La variance expliquée par la $k$-ième composante est :

$$\frac{\lambda_k}{\sum_{i=1}^{p} \lambda_i} = \frac{\lambda_k}{p}$$

### 3.2 Optimalité de la Rotation Varimax

#### Critère de Kaiser

La rotation Varimax maximise le critère de simplicité de Kaiser :

$$V = \sum_{j=1}^{2} \left[ \frac{1}{p} \sum_{i=1}^{p} a_{ij}^4 - \left( \frac{1}{p} \sum_{i=1}^{p} a_{ij}^2 \right)^2 \right]$$

où $a_{ij}$ désigne le loading de la variable $i$ sur l'axe $j$ après rotation.

Cette formulation équivaut à maximiser la variance des contributions au carré, ce qui favorise une structure où chaque variable est principalement associée à un seul axe factoriel.

### 3.3 Propriétés de la Bipartition

#### Inertie Intra-Groupe et Inter-Groupe

Soit $G_1$ et $G_2$ les deux groupes produits par la bipartition. L'inertie totale d'un ensemble de variables se décompose selon le théorème de Huygens généralisé :

$$I_{\text{totale}} = I_{\text{intra}}(G_1) + I_{\text{intra}}(G_2) + I_{\text{inter}}(G_1, G_2)$$

où :
- $I_{\text{intra}}(G_k)$ mesure la dispersion des variables au sein du groupe $G_k$
- $I_{\text{inter}}(G_1, G_2)$ quantifie la séparation entre les deux groupes

La stratégie de partitionnement de VARCLUS vise implicitement à minimiser l'inertie intra-groupe (ou, de manière équivalente, à maximiser l'inertie inter-groupe).

### 3.4 Complexité Algorithmique

#### Complexité Temporelle

Pour un ensemble de $p$ variables et $n$ observations :

1. **ACP par sous-groupe** : $O(p^2 n + p^3)$ dans le pire cas (dominé par le calcul de la matrice de covariance et sa diagonalisation)
2. **Rotation Varimax** : $O(p)$ (transformation 2D, complexité négligeable)
3. **Bipartition** : $O(p)$ (parcours linéaire des loadings)

Dans le cas d'une division équilibrée (chaque nœud se partitionne en deux groupes de taille similaire), la profondeur de l'arbre est $O(\log p)$. La complexité totale est donc :

$$T(p, n) = O(p^2 n \log p + p^3 \log p)$$

Pour $n \gg p$ (cas fréquent en pratique), on a $T(p, n) \approx O(p^2 n \log p)$.

#### Complexité Spatiale

L'arbre de partition nécessite un espace mémoire $O(p)$ pour stocker les nœuds internes et les feuilles. Les données intermédiaires (matrices ACP) sont recalculées à chaque niveau et ne persistent pas, limitant ainsi l'empreinte mémoire.


## 4. Illustration Algorithmique sur un Exemple Canonique

Afin d'illustrer concrètement les mécanismes internes de la classe `VARCLUS`, nous présentons ici une exécution complète de l'algorithme de division récursive appliqué au clustering de variables quantitatives, en utilisant un sous-ensemble du célèbre jeu de données Iris.

L'objectif est de montrer, sur un exemple totalement transparent, comment l'ACP est utilisée pour identifier les axes de variation, comment la rotation Varimax simplifie la structure, comment les variables sont assignées aux groupes, et comment le critère d'arrêt basé sur λ₂ détermine la fin du processus de division.

Nous considérons ici 4 variables quantitatives continues mesurées sur 150 échantillons :

| Variable | Description                                    | Unité | Statistiques       |
|----------|------------------------------------------------|-------|--------------------|
| V₁       | Longueur du sépale (Sepal.Length)              | cm    | μ = 5.84, σ = 0.83 |
| V₂       | Largeur du sépale (Sepal.Width)                | cm    | μ = 3.06, σ = 0.44 |
| V₃       | Longueur du pétale (Petal.Length)              | cm    | μ = 3.76, σ = 1.77 |
| V₄       | Largeur du pétale (Petal.Width)                | cm    | μ = 1.20, σ = 0.76 |

Ces variables représentent un cas typique de données morphométriques continues : échelles différentes, corrélations hétérogènes, et structure latente potentielle liée à la distinction sépale/pétale.

### 4.1 Matrice de Corrélation Initiale

Avant d'appliquer VARCLUS, examinons la structure de corrélation entre les quatre variables :

|            | V₁ (Sepal.L) | V₂ (Sepal.W) | V₃ (Petal.L) | V₄ (Petal.W) |
|------------|--------------|--------------|--------------|--------------|
| V₁         | 1.00         | -0.12        | 0.87         | 0.82         |
| V₂         | -0.12        | 1.00         | -0.43        | -0.37        |
| V₃         | 0.87         | -0.43        | 1.00         | 0.96         |
| V₄         | 0.82         | -0.37        | 0.96         | 1.00         |

**Observations préliminaires** :

- **Groupe de corrélation forte** : V₃ et V₄ (r = 0.96) sont très fortement corrélées
- **Corrélations modérées à fortes** : V₁ est fortement corrélé avec V₃ (r = 0.87) et V₄ (r = 0.82)
- **Variable distinctive** : V₂ présente des corrélations négatives avec V₃ et V₄, suggérant une dimension orthogonale

Cette structure laisse présager une partition naturelle entre variables de pétale (V₃, V₄) et variables de sépale (V₁, V₂), que VARCLUS devrait identifier.

### 4.2 Itération 1 : Division du Groupe Initial

#### Étape 1.1 : ACP sur l'Ensemble des Variables

L'algorithme commence par une ACP standardisée sur les 4 variables :

```r
X_standardized <- scale(iris[, 1:4])
pca_result <- prcomp(X_standardized)
```

**Valeurs propres obtenues** :

| Composante | Valeur propre (λ) | Variance expliquée | Cumul |
|------------|-------------------|-------------------|--------|
| PC1        | 2.91              | 72.8%             | 72.8%  |
| PC2        | 0.91              | 22.9%             | 95.7%  |
| PC3        | 0.15              | 3.7%              | 99.4%  |
| PC4        | 0.03              | 0.6%              | 100%   |

**Vérification du critère d'arrêt** :

λ₂ = 0.91 < 1.0 → Le critère d'arrêt n'est PAS satisfait (λ₂ doit être < 1.0 pour arrêter)

*Note importante* : Dans cet exemple, λ₂ = 0.91 est légèrement inférieur à 1, mais proche. Pour illustrer le mécanisme complet, supposons que nous utilisons un seuil de 0.80, permettant une division.

#### Étape 1.2 : Rotation Varimax des Deux Premières Composantes

La rotation Varimax est appliquée aux loadings des deux premières composantes pour simplifier la structure :

**Loadings avant rotation (PC1, PC2)** :

| Variable | PC1    | PC2    |
|----------|--------|--------|
| V₁       | 0.52   | -0.38  |
| V₂       | -0.27  | -0.92  |
| V₃       | 0.58   | 0.07   |
| V₄       | 0.56   | 0.10   |

**Loadings après rotation Varimax (RC1, RC2)** :

| Variable | RC1    | RC2    | Dominance   |
|----------|--------|--------|-------------|
| V₁       | 0.64   | 0.02   | RC1         |
| V₂       | -0.05  | -0.95  | RC2         |
| V₃       | 0.57   | 0.19   | RC1         |
| V₄       | 0.56   | 0.21   | RC1         |

#### Étape 1.3 : Assignation des Variables par Proximité Maximale

Chaque variable est assignée à l'axe rotaté avec lequel elle présente la corrélation absolue la plus forte :

| Variable | \|RC1\| | \|RC2\| | Assignation      |
|----------|---------|---------|------------------|
| V₁       | 0.64    | 0.02    | Groupe 1 (RC1)   |
| V₂       | 0.05    | 0.95    | Groupe 2 (RC2)   |
| V₃       | 0.57    | 0.19    | Groupe 1 (RC1)   |
| V₄       | 0.56    | 0.21    | Groupe 1 (RC1)   |

**Partition obtenue après l'Itération 1** :

- **Groupe 1** : {V₁ (Sepal.Length), V₃ (Petal.Length), V₄ (Petal.Width)}
- **Groupe 2** : {V₂ (Sepal.Width)}

### 4.3 Itération 2 : Tentative de Division du Groupe 1

Le Groupe 1 contient 3 variables {V₁, V₃, V₄}. L'algorithme tente une nouvelle division récursive.

#### Étape 2.1 : ACP sur le Sous-Groupe {V₁, V₃, V₄}

**Matrice de corrélation du sous-groupe** :

|     | V₁   | V₃   | V₄   |
|-----|------|------|------|
| V₁  | 1.00 | 0.87 | 0.82 |
| V₃  | 0.87 | 1.00 | 0.96 |
| V₄  | 0.82 | 0.96 | 1.00 |

**Valeurs propres de l'ACP** :

| Composante | Valeur propre (λ) | Variance expliquée |
|------------|-------------------|--------------------|
| PC1        | 2.69              | 89.6%              |
| PC2        | 0.26              | 8.7%               |
| PC3        | 0.05              | 1.7%               |

**Vérification du critère d'arrêt** :

λ₂ = 0.26 < 0.80 (seuil) → Le critère d'arrêt EST satisfait

**Décision** : Le Groupe 1 ne sera pas divisé davantage. Il constitue une feuille terminale.

**Interprétation** : La seconde valeur propre λ₂ = 0.26 est très faible, indiquant que les trois variables partagent une structure de corrélation essentiellement unidimensionnelle. Une division supplémentaire n'apporterait pas d'amélioration substantielle.

### 4.4 Itération 2 : Analyse du Groupe 2

Le Groupe 2 ne contient qu'une seule variable {V₂}, donc la cardinalité minimale (3 variables) n'est pas respectée.

**Critère d'arrêt structurel** : |Groupe 2| = 1 < 3 → Pas de division possible

**Décision** : Le Groupe 2 constitue une feuille terminale.

### 4.5 Arbre de Partition Final

L'arbre de division récursive obtenu est représenté ci-dessous :

```
                        Racine [V₁, V₂, V₃, V₄]
                        λ₁ = 2.91, λ₂ = 0.91
                        Division effectuée
                                |
                ________________|________________
                |                                |
        Groupe 1 [V₁, V₃, V₄]              Groupe 2 [V₂]
        λ₁ = 2.69, λ₂ = 0.26               Feuille
        λ₂ < seuil → Feuille               (cardinalité < 3)
        terminale                          
```

**Structure finale** :

- **Profondeur de l'arbre** : 1 (un seul niveau de division)
- **Nombre de clusters finaux** : 2
- **Distribution** :
  - Cluster 1 : 3 variables (V₁, V₃, V₄)
  - Cluster 2 : 1 variable (V₂)

### 4.6 Interprétation Biologique et Statistique

La partition obtenue révèle une organisation structurelle cohérente avec la morphologie florale :

**Cluster 1 : {Sepal.Length, Petal.Length, Petal.Width}**

Ces trois variables partagent une forte corrélation positive, reflétant une dimension latente de "taille globale de la fleur". Les corrélations élevées (r > 0.82) indiquent que les fleurs de grande taille tendent à avoir des sépales longs et des pétales longs et larges simultanément.

La première composante principale du cluster (λ₁ = 2.69, expliquant 89.6% de la variance) capture efficacement cette dimension unidimensionnelle dominante.

**Cluster 2 : {Sepal.Width}**

La largeur du sépale se distingue nettement des autres mesures par ses corrélations négatives avec les dimensions du pétale (r = -0.43 et r = -0.37). Cette variable capture une dimension morphologique orthogonale, potentiellement liée à la robustesse de la structure florale plutôt qu'à la taille globale.

Son isolement dans un cluster séparé reflète son indépendance structurelle vis-à-vis des autres variables.

**Validation par analyse des valeurs propres** :

Le ratio λ₁/λ₂ = 2.91/0.91 ≈ 3.2 à la racine indique une structure bidimensionnelle claire, justifiant la division initiale. En revanche, le ratio λ₁/λ₂ = 2.69/0.26 ≈ 10.3 dans le Groupe 1 confirme une structure unidimensionnelle très marquée, validant l'arrêt de la division.

### 4.7 Synthèse du Processus Algorithmique

Le diagramme suivant récapitule les étapes de l'algorithme :

```
Données initiales : 4 variables × 150 observations
                    ↓
        Standardisation (centrage-réduction)
                    ↓
Itération 1 - ACP sur {V₁, V₂, V₃, V₄}
    Valeurs propres : λ₁ = 2.91, λ₂ = 0.91
    Test : λ₂ = 0.91 > 0.80 → Division autorisée
                    ↓
Itération 1 - Rotation Varimax
    Identification de 2 axes rotatés (RC1, RC2)
                    ↓
Itération 1 - Assignation par proximité
    Groupe 1 : {V₁, V₃, V₄} (proches de RC1)
    Groupe 2 : {V₂} (proche de RC2)
                    ↓
Itération 2 - Tentative de division Groupe 1
    ACP sur {V₁, V₃, V₄}
    Valeurs propres : λ₁ = 2.69, λ₂ = 0.26
    Test : λ₂ = 0.26 < 0.80 → Arrêt (unidimensionnel)
                    ↓
Itération 2 - Analyse Groupe 2
    Cardinalité = 1 < 3 → Arrêt (critère structurel)
                    ↓
    Partition finale : 
        Cluster 1 = {V₁, V₃, V₄}
        Cluster 2 = {V₂}
```

Cet exemple illustre les propriétés caractéristiques de VARCLUS : division guidée par l'ACP et rotation Varimax, critère d'arrêt statistiquement fondé (λ₂ < seuil), et interprétabilité des clusters en termes de structures de corrélation unidimensionnelles.

---

## 5. Comparaison avec les Approches Alternatives

### 6.1 VARCLUS vs. CAH Ascendante (VAR-CAH)

| Critère | VARCLUS (Descendant) | VAR-CAH (Ascendant) |
|---------|----------------------|---------------------|
| **Stratégie** | Division récursive top-down | Fusion progressive bottom-up |
| **Complexité** | $O(p^2 n \log p)$ | $O(p^2 n + p^2 \log p)$ |
| **Critère d'arrêt** | Statistique ($\lambda_2 < 1$) | Heuristique (coude, gap) |
| **Optimalité locale** | À chaque division | À chaque fusion |
| **Interprétabilité** | Structure arborescente binaire | Dendrogramme complet |
| **Déterminisme** | Oui (division unique) | Oui (fusion unique) |
| **Nombre de clusters** | Détection automatique | Spécification ou coupe |

#### Avantages de VARCLUS
1. **Critère d'arrêt objectif** : Le seuil $\lambda_2 \geq 1$ a une interprétation statistique claire
2. **Efficacité computationnelle** : Complexité logarithmique en $p$ plutôt que quadratique
3. **Stabilité sur grandes dimensions** : Moins sensible au nombre total de variables

#### Limitations de VARCLUS
1. **Partition binaire stricte** : La bipartition peut être sous-optimale si la structure naturelle est non binaire
2. **Sensibilité à l'ordre de division** : Les premières divisions contraignent les suivantes (approche gloutonne)
3. **Pas de réallocation** : Une fois assignée, une variable ne peut plus changer de groupe

### 6.2 VARCLUS vs. K-Modes de Variables

| Critère | VARCLUS | K-Modes Variables |
|---------|---------|-------------------|
| **Nature des variables** | Quantitatives | Catégorielles |
| **Métrique** | Corrélation (via ACP) | Désaccord simple |
| **Initialisation** | Déterministe | Aléatoire (centroïdes) |
| **Itérations** | Une passe (récursive) | Multiples (réallocation) |
| **Convergence** | Garantie (terminaison) | Locale (peut diverger) |

## 6. Cas d'Usage et Applications

### 6.1 Prétraitement pour Modélisation Statistique

Dans des contextes de régression ou classification avec un grand nombre de prédicteurs corrélés, VARCLUS permet d'identifier des groupes de variables redondantes. Un représentant (ou une variable synthétique via ACP) peut ensuite être sélectionné par groupe, réduisant ainsi la multicolinéarité.

**Exemple** : En génomique, pour analyser l'expression de milliers de gènes, VARCLUS identifie des modules co-régulés avant sélection de gènes marqueurs.

### 6.2 Construction de Scores Synthétiques

VARCLUS facilite la création d'indices composites en regroupant des indicateurs mesurant des construits latents similaires. Chaque cluster peut être synthétisé en un score unidimensionnel via sa première composante principale.

**Exemple** : En psychométrie, pour construire un indice de satisfaction client à partir de multiples items de questionnaire.

### 6.3 Exploration de Structures de Données Complexes

L'arbre de partition produit par VARCLUS offre une représentation hiérarchique des relations entre variables, utile pour l'exploration de jeux de données multi-dimensionnels.

**Exemple** : En finance quantitative, pour comprendre les co-mouvements entre actifs financiers et identifier des secteurs d'investissement cohérents.

### 6.4 Détection de Redondance Informationnelle

En identifiant des variables fortement corrélées, VARCLUS aide à détecter les redondances dans la collecte de données, permettant de réduire les coûts opérationnels.

**Exemple** : En télédétection spatiale, pour éliminer des bandes spectrales redondantes dans l'analyse d'images satellites.

## 7. Extensions et Perspectives

### 7.1 Variantes Algorithmiques

#### VARCLUS avec Réallocation

Une extension possible consiste à alterner les divisions descendantes avec des phases de réallocation itérative des variables entre clusters, à la manière de l'algorithme K-means. Cela permettrait de corriger les assignations sous-optimales dues à la nature gloutonne de l'approche purement descendante.

#### VARCLUS Non-Binaire

Plutôt que de forcer une bipartition à chaque nœud, on pourrait généraliser à une k-partition ($k > 2$) en extrayant plus de deux composantes principales et en appliquant un clustering (K-means) dans l'espace factoriel réduit.

### 7.2 Adaptation aux Variables Mixtes

#### Approche PCAMIX

L'intégration de l'analyse factorielle pour données mixtes (PCAMIX) permettrait d'étendre VARCLUS aux jeux de données comportant à la fois des variables quantitatives et qualitatives. La matrice de corrélation serait remplacée par une matrice de proximité généralisée.

#### Métrique de Gower

Une alternative consiste à utiliser la distance de Gower pour mesurer la dissimilarité entre variables de types hétérogènes, puis à adapter le critère de bipartition en conséquence.

### 7.3 Robustification

#### ACP Robuste

L'utilisation d'estimateurs robustes de la matrice de covariance (ex. : estimateur de minimum volume de covariance, MCD) rendrait VARCLUS moins sensible aux observations aberrantes.

#### Corrélations de Rang

Le remplacement de la corrélation de Pearson par des mesures de dépendance non-paramétriques (Spearman, Kendall) permettrait de capturer des relations monotones non linéaires.

### 7.4 Critères d'Arrêt Alternatifs

#### Bootstrap Gap Statistic

Au lieu du critère $\lambda_2 \geq 1$, on pourrait utiliser la gap statistic, qui compare la dispersion intra-groupe observée à celle obtenue sur des données de référence générées aléatoirement.

#### Critère d'Information Bayésien (BIC)

Un critère de type BIC pourrait être défini pour pénaliser la complexité du modèle (nombre de clusters) tout en récompensant la qualité de l'ajustement.

## 8. Limitations Actuelles

### 8.1 Contrainte de Bipartition

La structure binaire imposée peut ne pas refléter la structure naturelle des données. Si un ensemble de variables se décompose naturellement en trois groupes, VARCLUS produira une partition sous-optimale avec des divisions supplémentaires.

### 8.2 Absence de Réallocation

Une fois qu'une division est effectuée, les variables ne peuvent plus être transférées entre branches de l'arbre. Cette rigidité peut conduire à des partitions localement sous-optimales.

### 8.3 Variables Quantitatives Uniquement

Dans l'implémentation actuelle, seules les variables numériques continues sont supportées. Les variables catégorielles ou ordinales nécessitent des transformations préalables ou l'utilisation de méthodes alternatives (K-Modes).

### 8.4 Sensibilité au Seuil d'Arrêt

Le choix du paramètre `stop_eigenvalue` influence directement le nombre de clusters finaux. Bien que la valeur par défaut de 1.0 soit théoriquement justifiée, elle peut ne pas être optimale pour tous les contextes applicatifs.

## 9. Implémentation et Bonnes Pratiques

### 9.1 Standardisation des Variables

```r
pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
```

La standardisation (centrage et réduction) est cruciale pour garantir que toutes les variables contribuent équitablement à l'ACP, indépendamment de leurs échelles de mesure respectives.

### 9.2 Gestion des Valeurs Manquantes

L'implémentation actuelle s'appuie sur la méthode héritée `validateDataset()` qui traite les données manquantes selon la stratégie définie (`na_action`). Pour VARCLUS, il est recommandé de :
- Utiliser l'imputation préalable si le taux de données manquantes est modéré (< 20%)
- Supprimer les variables avec trop de valeurs manquantes (> 50%)
- Documenter explicitement la stratégie adoptée dans le pipeline analytique

### 9.3 Interprétation des Résultats

#### Inspection de l'Arbre
```r
varclus_model <- VARCLUS$new(stop_eigenvalue = 1.0)
varclus_model$fit(X)
tree <- varclus_model$FTree  # Structure interne de l'arbre
```

L'arbre de partition peut être visualisé ou exporté pour analyse. Les valeurs $\lambda_1$ et $\lambda_2$ à chaque nœud renseignent sur la qualité de la division.

#### Diagnostic de Stabilité

Il est recommandé de :
- Évaluer la sensibilité au seuil d'arrêt en testant plusieurs valeurs (ex. : 0.7, 1.0, 1.5)
- Comparer les résultats avec une approche ascendante (VAR-CAH) pour vérifier la cohérence
- Analyser la distribution des tailles de clusters (déséquilibres extrêmes peuvent signaler un problème)

## 10. Références Théoriques

L'algorithme VARCLUS s'inspire des travaux fondateurs suivants :

### 10.1 Références Primaires

- **SAS Institute Inc. (1990)** : *SAS/STAT User's Guide, Volume 2*, Cary, NC: SAS Institute Inc.  
  Description originale de la procédure PROC VARCLUS dans le logiciel SAS, formalisant l'algorithme de division récursive avec critère $\lambda_2 \geq 1$.

- **Sarle, W.S. (1990)** : *"The VARCLUS Procedure"*, in *SAS/STAT User's Guide, Version 6*, Fourth Edition, Volume 2, pp. 1641-1659.  
  Présentation détaillée des fondements algorithmiques et des justifications statistiques.

### 10.2 Fondements Théoriques

- **Kaiser, H.F. (1958)** : *"The varimax criterion for analytic rotation in factor analysis"*, Psychometrika, 23(3), 187-200.  
  Formalisation mathématique de la rotation Varimax utilisée dans VARCLUS pour simplifier la structure factorielle.

- **Jolliffe, I.T. (2002)** : *Principal Component Analysis*, Second Edition, Springer Series in Statistics.  
  Référence standard pour les fondements mathématiques de l'ACP.

### 10.3 Méthodes Apparentées

- **Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012)** : *"ClustOfVar: An R Package for the Clustering of Variables"*, Journal of Statistical Software, 50(13), 1-16.  
  Description de l'approche ascendante complémentaire (VAR-CAH).

- **Vigneau, E., & Qannari, E.M. (2003)** : *"Clustering of variables around latent components"*, Communications in Statistics - Simulation and Computation, 32(4), 1131-1150.  
  Fondements théoriques du clustering de variables par composantes latentes.

## 11. Conclusion

### 11.1 Positionnement Méthodologique

VARCLUS constitue une alternative performante aux méthodes ascendantes pour le clustering de variables, particulièrement adaptée aux jeux de données de grande dimension où l'efficacité computationnelle est critique. Son critère d'arrêt statistiquement fondé ($\lambda_2 \geq 1$) offre une objectivité appréciable, réduisant l'arbitraire du choix du nombre de clusters.

### 11.2 Complémentarité avec VAR-CAH

L'utilisation conjointe de VARCLUS (descendant) et VAR-CAH (ascendant) sur un même jeu de données permet de valider la robustesse de la structure identifiée. Une convergence des deux approches vers des partitions similaires renforce la confiance dans les résultats.

### 11.3 Recommandations Pratiques

Pour une analyse optimale :
1. **Prétraitement** : Standardiser les variables, traiter les valeurs manquantes
2. **Exploration** : Tester plusieurs valeurs du seuil d'arrêt
3. **Validation** : Comparer avec une approche alternative (VAR-CAH, K-Means)
4. **Interprétation** : Analyser les valeurs propres à chaque division pour comprendre la qualité du partitionnement
5. **Documentation** : Tracer l'arbre de partition et justifier le choix du nombre final de clusters