# RollerClustR
# PACKAGE DE CLUSTERING R6 
# ========================

## FICHIERS PRINCIPAUX

### 1. ClusterAnalysis_parentclass.R
**Description**: Classe parente pour tous les algorithmes de clustering
- Méthodes communes : fit(), predict(), print(), summary()
- Gestion des variables illustratives (quantitatives et qualitatives)
- Calculs : rapport de corrélation, V de Cramer, valeurs-test
- Compatible avec tous les algorithmes enfants

### 2. CAH_class.R (CAH + K-means Hybride)
**Description**: Classification Ascendante Hiérarchique avec raffinement K-means
- **Algorithme**: Utilise CAH (ward.D2) pour initialiser, puis K-means pour raffiner
- **Type de données**: Variables numériques uniquement
- **Avantages**: Combine stabilité CAH et optimisation K-means
- **Méthodes spécifiques**:
  - `inertie()`: Obtenir les inerties (totale, intra, inter, % expliqué)
  - `centroides()`: Obtenir les centroïdes des groupes
  - `plot()`: Visualiser le dendrogramme
  - `plot_groups()`: Visualiser les groupes dans un espace 2D
  - `NbGroupes`: Propriété active pour changer k dynamiquement

### 3. ClustOfVar_class.R
**Description**: Clustering de variables par approche factorielle
- **Algorithme**: Approche itérative avec variables synthétiques
  - PCA pour variables numériques
  - MCA pour variables catégorielles  
  - FAMD pour variables mixtes
- **Type de données**: Numérique, catégoriel ou mixte
- **Particularité**: Cluster les VARIABLES (colonnes), pas les observations
- **Méthodes spécifiques**:
  - `variables_synthetiques()`: Obtenir les variables synthétiques par cluster
  - `matrice_correlations()`: Matrice liaisons variables/clusters
  - `qualite_clusters()`: Qualité de chaque cluster
  - `plot(type="heatmap")`: Heatmap des liaisons
  - `plot(type="barplot")`: Qualité des clusters
  - `Homogeneite`: Propriété active (mesure de qualité globale)

### 4. wrapper.R 
**Description**: Wrappers et utilitaires pour faciliter l'utilisation
- **ClusteringFactory**: Création d'objets de clustering
  - `create_cah_kmeans()`: Créer CAH+K-means
  - `create_kmeans()`: Créer K-means standard
  - `create_kprototypes()`: Créer K-prototypes
  - `create_clustofvar()`: Créer ClustOfVar [À AJOUTER]
  - `create_auto()`: Sélection automatique selon type de données
  
- **ClusteringEvaluator**: Évaluation du nombre optimal de clusters
  - `evaluate_k()`: Tester plusieurs valeurs de k
  - `plot_evaluation()`: Visualiser les résultats
  - `get_best_k()`: Obtenir le k optimal
  
- **ClusteringComparator**: Comparaison d'algorithmes
  - `add_algorithm()`: Ajouter un algorithme à comparer
  - `compare()`: Comparer les résultats
  - `plot_comparison()`: Visualiser les comparaisons
  
- **ClusteringHelper**: Fonctions utilitaires
  - `get_clusters()`: Extraire les groupes
  - `export_results()`: Exporter vers data frame/CSV
  - `group_statistics()`: Statistiques par groupe
  - `generate_report()`: Générer un rapport textuel

### 5. user_functions.R
**Description**: Fonctions utilisateur simplifiées pour workflow complet
- `faire_clustering()`: Clustering simplifié avec sélection auto
- `analyser_illustratives()`: Analyser variables illustratives via predict()
- `caracteriser_groupes()`: Caractérisation détaillée des groupes
- `trouver_k_optimal()`: Recherche du nombre optimal de clusters
- `comparer_algorithmes()`: Comparaison multi-algorithmes
- `obtenir_groupes()`: Extraire les affectations
- `exporter_resultats()`: Export vers CSV
- `visualiser_clustering()`: Visualisations adaptées par algorithme
- `resumer_clustering()`: Résumé détaillé
- `statistiques_par_groupe()`: Stats descriptives par groupe
- `generer_rapport()`: Rapport textuel complet
- `clustering_complet()`: Workflow tout-en-un automatisé
- `creer_pipeline()`: Pipeline réutilisable

## FICHIERS DE SUPPORT

### 6. INTEGRATION_GUIDE_ClustOfVar.R
**Description**: Guide d'intégration complet de ClustOfVar dans le package
- Modifications précises à apporter dans wrapper.R
- Modifications précises à apporter dans user_functions.R
- Exemples d'utilisation détaillés
- Points importants à retenir

### 7. test_cah_kmeans.R
**Description**: Script de test complet pour CAH+K-means
- 10 tests couvrant toutes les fonctionnalités
- Validation de l'intégration avec le package
- Exemples d'utilisation pratiques

### 8. test_ClustOfVar.R
**Description**: Script de test complet pour ClustOfVar
- 7 tests sur différents types de données
- Validation des méthodes spécifiques
- Exemples de visualisation et prédiction

## STRUCTURE DU PACKAGE

```
Package/
│
├── Classes principales
│   ├── ClusterAnalysis_parentclass.R  (Base)
│   ├── CAH_class.R                    (Algorithme 1: CAH+K-means)
│   └── ClustOfVar_class.R             (Algorithme 2: ClustOfVar)
│
├── Infrastructure
    ├── wrapper.R                      (Factories et utilitaires)
    └── user_functions.R               (Interface simplifiée)


```

## LES 3 ALGORITHMES DU PACKAGE

### 1. CAH + K-means Hybride (CAH_class.R)
- ✓ Réallocation : Oui (K-means)
- ✓ Variables qualitatives : Non (uniquement numériques)
- 🎯 Usage: Données numériques, besoin de stabilité + optimisation

### 2. K-prototypes (à implémenter séparément)
- ✓ Réallocation : Oui
- ✓ Variables qualitatives : Oui (traitement spécifique)
- 🎯 Usage: Données mixtes (numériques + catégorielles)

### 3. ClustOfVar (ClustOfVar_class.R)
- ✓ Réallocation : Oui (réaffectation itérative)
- ✓ Variables qualitatives : Oui (via MCA/FAMD)
- 🎯 Usage: Clustering de variables, réduction de dimensionnalité

## COMPATIBILITÉ AVEC LE CAHIER DES CHARGES

## UTILISATION TYPIQUE

```r
# 1. Charger le package
library(R6)
source("ClusterAnalysis_parentclass.R")
source("CAH_class.R")
source("ClustOfVar_class.R")
source("wrapper.R")
source("user_functions.R")

# 2. Clustering simple
data(iris)
resultat <- faire_clustering(iris[1:4], k = 3, method = "cah_kmeans")
resultat$summary()

# 3. Clustering de variables
model_var <- faire_clustering(iris[1:4], k = 2, method = "clustofvar")
model_var$summary()

# 4. Workflow complet
workflow <- clustering_complet(
  iris[1:4], 
  variables_illustratives = iris[5],
  k_max = 6,
  method = "cah_kmeans",
  fichier_resultats = "resultats.csv"
)

# 5. Comparaison d'algorithmes
comparateur <- comparer_algorithmes(
  iris[1:4], 
  k = 3,
  methods = c("cah_kmeans", "kmeans", "clustofvar")
)
resultats_comp <- comparateur$compare()
```

## NOTES IMPORTANTES

- **ClustOfVar** cluster les VARIABLES (colonnes), pas les observations
- Pour **predict()** avec ClustOfVar, X doit avoir le même nombre d'observations
- **CAH+K-means** combine deux approches pour meilleure performance
- Tous les algorithmes suivent le même pattern fit/predict
- Le package est conçu pour être extensible (ajout facile de nouveaux algorithmes)

