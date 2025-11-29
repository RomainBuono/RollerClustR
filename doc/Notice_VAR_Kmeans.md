# Implémentation de l'Algorithme VAR-KMEANS dans la Classe VAR\_KMEANS

## 1\. Fondements Théoriques

### 1.1 Principe Général

L'implémentation proposée réalise un clustering de variables selon une approche de partitionnement itératif (K-Means), basée sur les travaux de **Vigneau & Qannari**. L'objectif est de partitionner un ensemble de $p$ variables en $K$ groupes disjoints, de telle sorte que les variables d'un même groupe soient fortement corrélées à une variable synthétique latente représentant ce groupe.

Cette méthode se distingue du K-Means classique par deux aspects fondamentaux :

1.  **L'objet classé** : On classe des variables (colonnes) basées sur leurs profils d'observations.
2.  **La représentation du centre** : Le centre d'un cluster n'est pas la moyenne arithmétique des variables, mais la **première composante principale** du groupe.

### 1.2 Architecture Logicielle

La classe `VAR_KMEANS` hérite de la classe abstraite `ClusterAnalysis` et implémente une architecture R6. Contrairement à l'approche hiérarchique (VAR\_CAH) qui nécessite le calcul coûteux d'une matrice de dissimilarité $p \times p$, cette implémentation est optimisée pour traiter des jeux de données de haute dimension grâce à une complexité linéaire en nombre de variables.

Les attributs privés assurent la persistance des centres des clusters (les composantes principales), permettant l'utilisation du modèle pour la projection de nouvelles variables (mode prédictif).

## 2\. Traduction Algorithmique

### 2.1 Phase de Prétraitement et Initialisation

#### Standardisation et Structure des Données

L'algorithme opère sur des variables centrées et réduites pour garantir l'équivalence entre covariance et corrélation.

```r
if (private$FScale) {
  X_scaled <- scale(X, center = TRUE, scale = TRUE)
}
# Transposition : les lignes deviennent les variables pour le traitement interne
private$FX_scaled <- t(X_scaled)
```

La transposition interne (`private$FX_scaled`) est cruciale : les opérations vectorielles s'effectuent sur les lignes de cette matrice (qui sont les variables d'origine).

#### Stratégie d'Initialisation Multiple

Le K-Means étant sensible aux optimums locaux, l'algorithme implémente une stratégie de redémarrage multiple (`n_init`) pour assurer la robustesse de la solution. En effet, un des défauts de l'algorithme K-Means dans cette implémentation concerne le problème de l'optimum local. À chaque étape d'itération, l'algorithme cherche la meilleure amélioration possible à partir de l'état actuel.Cependant, il n'explore pas toutes les configurations possibles. Si l'initialisation des centres est mauvaise, l'algorithme peut rapidement converger vers une solution qui est localement optimale (la meilleure possible dans un voisinage immédiat) mais qui est loin de l'optimum global (la meilleure solution possible sur l'ensemble du domaine). Le critère d'optimisation est l'inertie définie par la somme des corrélations au carré ($r^2$) :$$\max T = \sum_{k=1}^K \sum_{j \in C_k} r^2(x_j, u_k)$$Une mauvaise initialisation peut conduire à une partition qui donne une valeur $T$ faible, même si l'algorithme a "convergé" (c'est-à-dire que l'affectation des variables n'évolue plus).

```r
kmeans_multiple_runs = function() {
  best_criterion <- -Inf
  for (i in 1:private$Fn_init) {
    # ... exécution d'un run ...
    if (result$inertia > best_criterion) {
      best_result <- result
    }
  }
}
```

L'initialisation des centres est protégée pour garantir qu'aucun cluster ne soit vide au démarrage, combinant assignation aléatoire et vérification de contraintes.

### 2.2 Algorithme Itératif (Allocation-Représentation)

L'algorithme alterne entre deux étapes jusqu'à convergence ou atteinte du nombre maximal d'itérations (`max_iter`).

#### Étape 1 : Allocation (Assignment Step)

Chaque variable est affectée au cluster dont le centre (variable latente) est le plus proche. La mesure de proximité est le **coefficient de corrélation au carré** ($r^2$).

```r
assign_to_clusters = function(centers) {
  for (j in 1:n_vars) {
    var_j <- private$FX_scaled[j, ]
    # Calcul des corrélations carrées avec chaque centre k
    scores <- sapply(1:K, function(k) cor(var_j, centers[k, ])^2)
    clusters[j] <- which.max(scores)
  }
}
```

On assigne la variable $x_j$ au cluster $k$ maximisant $r^2(x_j, u_k)$, où $u_k$ est le centre du cluster $k$. Cela permet de regrouper des variables corrélées positivement ou négativement (colinéarité).

#### Étape 2 : Représentation (Update Step)

Les centres des clusters sont mis à jour. Pour un cluster $C_k$ contenant un sous-ensemble de variables, le nouveau centre $u_k$ est la première composante principale de ces variables.

```r
compute_cluster_center = function(var_indices) {
  # ... extraction des données du cluster ...
  cluster_centered <- ...
  
  # Décomposition spectrale de la matrice de corrélation
  R <- cor(t(cluster_centered))
  eigen_result <- eigen(R, symmetric = TRUE)
  
  # Calcul du score factoriel (Composante Principale)
  v1 <- eigen_result$vectors[, 1]
  pc1 <- as.vector(t(cluster_centered) %*% v1)
  
  # Normalisation
  return(pc1 / sd(pc1))
}
```

Cette étape garantit que le centre $u_k$ résume au mieux la variance structurelle du groupe.

### 2.3 Critère de Convergence

La boucle s'arrête lorsque l'affectation des variables aux clusters ne change plus entre deux itérations consécutives (`all(clusters == old_clusters)`).

L'algorithme maximise le critère d'inertie intra-classe défini par :
$$T = \sum_{k=1}^K \sum_{j \in C_k} r^2(x_j, u_k)$$
Où :
$\|m_i - m_j\|^2$ (Distance entre Centres) : C'est le carré de la distance euclidienne entre le centroïde $m_i$ du cluster $C_i$ et le centroïde $m_j$ du cluster $C_j$. C'est la source principale de l'augmentation d'inertie.

$\frac{n_i n_j}{n_i + n_j}$ (Facteur de Pondération) : Ce terme pondère la distance entre les centres par la taille relative des clusters ($n_i$ et $n_j$ sont les effectifs, c'est-à-dire le nombre de variables dans chaque cluster).


## 3\. Prédiction de Nouvelles Variables

### 3.1 Principe : Projection sur Espace Fixe

La méthode `do_predict` permet d'associer de **nouvelles variables** aux clusters existants. Une particularité importante de cette méthode est qu'elle requiert que les nouvelles variables soient mesurées sur les **mêmes observations** (mêmes individus) que celles ayant servi à l'apprentissage.

### 3.2 Vérification d'Intégrité Dimensionnelle

```r
if (nrow(newdata) != ncol(private$FX_scaled)) {
  stop("newdata must have ", ncol(private$FX_scaled), " observations")
}
```

Cette contrainte est mathématique : pour calculer la corrélation entre une nouvelle variable $v_{new}$ et un centre de cluster $u_k$, les deux vecteurs doivent avoir la même longueur $n$ (nombre d'observations). Le "centre" est ici un vecteur de scores factoriels défini sur les individus de référence.

### 3.3 Algorithme de Prédiction

Pour chaque nouvelle variable fournie :

1.  Calcul de sa corrélation avec chacun des $K$ centres mémorisés (`private$FClusterCenters`).
2.  Identification du cluster maximisant le $r^2$.
3.  Restitution du cluster d'appartenance et du score de proximité.

## 4\. Méthodes Utilitaires

### 4.1 `get_cluster_centers()`

Retourne la matrice des composantes principales (variables latentes) de chaque cluster.

  * **Dimensions** : $n$ observations $\times K$ clusters.
  * **Usage** : Ces centres peuvent servir de "super-variables" pour réduire la dimensionnalité dans des modèles ultérieurs (régression sur composantes latentes).

### 4.2 `summary()`

Affiche les métriques de qualité du clustering :

  * **Sum of r² (criterion)** : La valeur de la fonction objectif maximisée.
  * **Homogénéité** : Moyenne pondérée des $r^2$, indiquant la part de variance des variables expliquée par la partition.
  * **Détails par cluster** : Liste des variables et homogénéité interne de chaque groupe.

## 5\. Fondements Mathématiques

### 5.1 Décomposition en Valeurs Propres

L'algorithme repose sur le résultat théorique suivant : pour un ensemble de variables centrées-réduites $X_k$, le vecteur $u$ de norme 1 qui maximise la somme des corrélations au carré $\sum r^2(x_j, u)$ est le premier vecteur propre de la matrice $X_k'X_k$ (ou de la matrice de corrélation).

L'algorithme est donc une alternance entre :

  * Partitionnement conditionnel aux centres.
  * Analyse en Composantes Principales (ACP) locale conditionnelle à la partition.

### 5.2 Relation avec l'Inertie

Dans l'espace des variables (sphère unité), maximiser la corrélation au carré $r^2(x, u)$ revient à minimiser la distance sinusoïdale ou la distance euclidienne carrée entre la variable $x$ et l'axe porté par $u$.

## 6\. Complexité Algorithmique

### 6.1 Phase d'Apprentissage

| Étape | Complexité | Comparaison avec VAR\_CAH |
| :--- | :--- | :--- |
| Initialisation | $O(n_{\text{init}} \cdot K)$ | Négligeable |
| Itération (Allocation) | $O(n \cdot p \cdot K)$ | Très rapide |
| Itération (Update) | $O(n \cdot p)$ (dominé par SVD locale) | Rapide si $p$ est bien réparti |
| **Total** | **$O(I \cdot n \cdot p \cdot K)$** | **Linéaire en $p$** |

  * $n$ : nombre d'observations
  * $p$ : nombre de variables
  * $K$ : nombre de clusters
  * $I$ : nombre d'itérations

L'algorithme est particulièrement adapté aux cas où $p$ (nombre de variables) est très grand, là où une CAH en $O(p^2)$ deviendrait prohibitive.

## 7\. Limitations et Extensions

### 7.1 Limitations

1.  **Choix de K** : Le nombre de clusters doit être fixé a priori. Il n'y a pas de structure hiérarchique permettant de couper l'arbre a posteriori.
2.  **Sensibilité à l'initialisation** : Bien que compensée par `n_init`, la convergence vers un optimum global n'est pas garantie.
3.  **Contrainte de prédiction** : Impossible de projeter le modèle sur de *nouveaux individus* directement (les centres sont liés aux individus d'entraînement).

### 7.2 Extensions Possibles

  * **Algorithme Hybride** : Utiliser VAR\_KMEANS pour réduire un très grand nombre de variables (ex: 10 000) à un nombre modéré de groupes (ex: 100), puis appliquer une CAH sur les centres de ces groupes (approche "Two-step clustering of variables").
  * **Sélection de variables** : Introduire un seuil de $r^2$ minimal pour exclure les variables "bruit" qui ne s'alignent avec aucun centre (cluster "poubelle").

## 8\. Cas d'Usage et Applications

### 8.1 Réduction de Dimensionnalité (Feature Extraction)

Dans les contextes de "High-Dimensional Low-Sample Size" (HDLSS), comme en génomique ou chimiométrie, VAR\_KMEANS permet de résumer des milliers de variables corrélées en quelques composantes synthétiques interprétables, sans les problèmes de multicolinéarité.

### 8.2 Analyse Sensorielle

Pour regrouper des descripteurs sensoriels (ex: "fruité", "sucré", "agrumes") qui covarient, et identifier les dimensions latentes de perception des produits.

## 9\. Références Théoriques

### 9.1 Algorithme de Base

  * **Vigneau, E., & Qannari, E. M. (2003)**. *Clustering of variables around latent components*. Communications in Statistics-Simulation and Computation, 32(4), 1131-1150.
    > Article fondateur décrivant l'algorithme itératif avec recentrage sur la composante principale.

### 9.2 Implémentation de Référence

  * **Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012)**. *ClustOfVar: An R Package for the Clustering of Variables*. Journal of Statistical Software, 50(13), 1-16.
    > Description du package R standard pour ces méthodes, dont s'inspire cette implémentation R6.


    ---

**Document rédigé dans le cadre du développement du package R `RollerClustR`**  
**Auteur** : Romain Buono
**Date** : Novembre 2025  
**Licence** : MIT