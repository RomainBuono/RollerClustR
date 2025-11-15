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

## 4. Comparaison avec les Approches Alternatives

### 4.1 VARCLUS vs. CAH Ascendante (VAR-CAH)

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

### 4.2 VARCLUS vs. K-Modes de Variables

| Critère | VARCLUS | K-Modes Variables |
|---------|---------|-------------------|
| **Nature des variables** | Quantitatives | Catégorielles |
| **Métrique** | Corrélation (via ACP) | Désaccord simple |
| **Initialisation** | Déterministe | Aléatoire (centroïdes) |
| **Itérations** | Une passe (récursive) | Multiples (réallocation) |
| **Convergence** | Garantie (terminaison) | Locale (peut diverger) |

## 5. Cas d'Usage et Applications

### 5.1 Prétraitement pour Modélisation Statistique

Dans des contextes de régression ou classification avec un grand nombre de prédicteurs corrélés, VARCLUS permet d'identifier des groupes de variables redondantes. Un représentant (ou une variable synthétique via ACP) peut ensuite être sélectionné par groupe, réduisant ainsi la multicolinéarité.

**Exemple** : En génomique, pour analyser l'expression de milliers de gènes, VARCLUS identifie des modules co-régulés avant sélection de gènes marqueurs.

### 5.2 Construction de Scores Synthétiques

VARCLUS facilite la création d'indices composites en regroupant des indicateurs mesurant des construits latents similaires. Chaque cluster peut être synthétisé en un score unidimensionnel via sa première composante principale.

**Exemple** : En psychométrie, pour construire un indice de satisfaction client à partir de multiples items de questionnaire.

### 5.3 Exploration de Structures de Données Complexes

L'arbre de partition produit par VARCLUS offre une représentation hiérarchique des relations entre variables, utile pour l'exploration de jeux de données multi-dimensionnels.

**Exemple** : En finance quantitative, pour comprendre les co-mouvements entre actifs financiers et identifier des secteurs d'investissement cohérents.

### 5.4 Détection de Redondance Informationnelle

En identifiant des variables fortement corrélées, VARCLUS aide à détecter les redondances dans la collecte de données, permettant de réduire les coûts opérationnels.

**Exemple** : En télédétection spatiale, pour éliminer des bandes spectrales redondantes dans l'analyse d'images satellites.

## 6. Extensions et Perspectives

### 6.1 Variantes Algorithmiques

#### VARCLUS avec Réallocation

Une extension possible consiste à alterner les divisions descendantes avec des phases de réallocation itérative des variables entre clusters, à la manière de l'algorithme K-means. Cela permettrait de corriger les assignations sous-optimales dues à la nature gloutonne de l'approche purement descendante.

#### VARCLUS Non-Binaire

Plutôt que de forcer une bipartition à chaque nœud, on pourrait généraliser à une k-partition ($k > 2$) en extrayant plus de deux composantes principales et en appliquant un clustering (K-means) dans l'espace factoriel réduit.

### 6.2 Adaptation aux Variables Mixtes

#### Approche PCAMIX

L'intégration de l'analyse factorielle pour données mixtes (PCAMIX) permettrait d'étendre VARCLUS aux jeux de données comportant à la fois des variables quantitatives et qualitatives. La matrice de corrélation serait remplacée par une matrice de proximité généralisée.

#### Métrique de Gower

Une alternative consiste à utiliser la distance de Gower pour mesurer la dissimilarité entre variables de types hétérogènes, puis à adapter le critère de bipartition en conséquence.

### 6.3 Robustification

#### ACP Robuste

L'utilisation d'estimateurs robustes de la matrice de covariance (ex. : estimateur de minimum volume de covariance, MCD) rendrait VARCLUS moins sensible aux observations aberrantes.

#### Corrélations de Rang

Le remplacement de la corrélation de Pearson par des mesures de dépendance non-paramétriques (Spearman, Kendall) permettrait de capturer des relations monotones non linéaires.

### 6.4 Critères d'Arrêt Alternatifs

#### Bootstrap Gap Statistic

Au lieu du critère $\lambda_2 \geq 1$, on pourrait utiliser la gap statistic, qui compare la dispersion intra-groupe observée à celle obtenue sur des données de référence générées aléatoirement.

#### Critère d'Information Bayésien (BIC)

Un critère de type BIC pourrait être défini pour pénaliser la complexité du modèle (nombre de clusters) tout en récompensant la qualité de l'ajustement.

## 7. Limitations Actuelles

### 7.1 Contrainte de Bipartition

La structure binaire imposée peut ne pas refléter la structure naturelle des données. Si un ensemble de variables se décompose naturellement en trois groupes, VARCLUS produira une partition sous-optimale avec des divisions supplémentaires.

### 7.2 Absence de Réallocation

Une fois qu'une division est effectuée, les variables ne peuvent plus être transférées entre branches de l'arbre. Cette rigidité peut conduire à des partitions localement sous-optimales.

### 7.3 Variables Quantitatives Uniquement

Dans l'implémentation actuelle, seules les variables numériques continues sont supportées. Les variables catégorielles ou ordinales nécessitent des transformations préalables ou l'utilisation de méthodes alternatives (K-Modes).

### 7.4 Sensibilité au Seuil d'Arrêt

Le choix du paramètre `stop_eigenvalue` influence directement le nombre de clusters finaux. Bien que la valeur par défaut de 1.0 soit théoriquement justifiée, elle peut ne pas être optimale pour tous les contextes applicatifs.

## 8. Implémentation et Bonnes Pratiques

### 8.1 Standardisation des Variables

```r
pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
```

La standardisation (centrage et réduction) est cruciale pour garantir que toutes les variables contribuent équitablement à l'ACP, indépendamment de leurs échelles de mesure respectives.

### 8.2 Gestion des Valeurs Manquantes

L'implémentation actuelle s'appuie sur la méthode héritée `validateDataset()` qui traite les données manquantes selon la stratégie définie (`na_action`). Pour VARCLUS, il est recommandé de :
- Utiliser l'imputation préalable si le taux de données manquantes est modéré (< 20%)
- Supprimer les variables avec trop de valeurs manquantes (> 50%)
- Documenter explicitement la stratégie adoptée dans le pipeline analytique

### 8.3 Interprétation des Résultats

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

## 9. Références Théoriques

L'algorithme VARCLUS s'inspire des travaux fondateurs suivants :

### 9.1 Références Primaires

- **SAS Institute Inc. (1990)** : *SAS/STAT User's Guide, Volume 2*, Cary, NC: SAS Institute Inc.  
  Description originale de la procédure PROC VARCLUS dans le logiciel SAS, formalisant l'algorithme de division récursive avec critère $\lambda_2 \geq 1$.

- **Sarle, W.S. (1990)** : *"The VARCLUS Procedure"*, in *SAS/STAT User's Guide, Version 6*, Fourth Edition, Volume 2, pp. 1641-1659.  
  Présentation détaillée des fondements algorithmiques et des justifications statistiques.

### 9.2 Fondements Théoriques

- **Kaiser, H.F. (1958)** : *"The varimax criterion for analytic rotation in factor analysis"*, Psychometrika, 23(3), 187-200.  
  Formalisation mathématique de la rotation Varimax utilisée dans VARCLUS pour simplifier la structure factorielle.

- **Jolliffe, I.T. (2002)** : *Principal Component Analysis*, Second Edition, Springer Series in Statistics.  
  Référence standard pour les fondements mathématiques de l'ACP.

### 9.3 Méthodes Apparentées

- **Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012)** : *"ClustOfVar: An R Package for the Clustering of Variables"*, Journal of Statistical Software, 50(13), 1-16.  
  Description de l'approche ascendante complémentaire (VAR-CAH).

- **Vigneau, E., & Qannari, E.M. (2003)** : *"Clustering of variables around latent components"*, Communications in Statistics - Simulation and Computation, 32(4), 1131-1150.  
  Fondements théoriques du clustering de variables par composantes latentes.

## 10. Conclusion

### 10.1 Positionnement Méthodologique

VARCLUS constitue une alternative performante aux méthodes ascendantes pour le clustering de variables, particulièrement adaptée aux jeux de données de grande dimension où l'efficacité computationnelle est critique. Son critère d'arrêt statistiquement fondé ($\lambda_2 \geq 1$) offre une objectivité appréciable, réduisant l'arbitraire du choix du nombre de clusters.

### 10.2 Complémentarité avec VAR-CAH

L'utilisation conjointe de VARCLUS (descendant) et VAR-CAH (ascendant) sur un même jeu de données permet de valider la robustesse de la structure identifiée. Une convergence des deux approches vers des partitions similaires renforce la confiance dans les résultats.

### 10.3 Recommandations Pratiques

Pour une analyse optimale :
1. **Prétraitement** : Standardiser les variables, traiter les valeurs manquantes
2. **Exploration** : Tester plusieurs valeurs du seuil d'arrêt
3. **Validation** : Comparer avec une approche alternative (VAR-CAH, K-Means)
4. **Interprétation** : Analyser les valeurs propres à chaque division pour comprendre la qualité du partitionnement
5. **Documentation** : Tracer l'arbre de partition et justifier le choix du nombre final de clusters