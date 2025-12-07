# Architecture du Système Expert - Récapitulatif Technique

## Analyse de l'architecture

L'architecture actuelle est **CORRECTE** et **CONFORME** aux exigences du sujet TP3.

### Conformité avec le sujet

| Exigence | Fichier correspondant | Statut |
|----------|----------------------|---------|
| Moteurs d'inférence (chaînage avant/arrière) | `src/moteur.lisp` | Défini |
| Base de faits | `src/base-faits.lisp` | Défini |
| Base de règles | `src/base-regles.lisp` + `src/gestion-regles.lisp` | Défini |
| Interface utilisateur | `src/interface.lisp` | Défini |
| Intégration des composants | `main.lisp` | Défini |
| Scénarios de test | `tests/scenario-1.lisp` | Défini |
| Données externes | `donnees/recettes.lisp` | Défini |

---

## Structure détaillée du projet

```txt
tp3-systeme-expert/
│
├── main.lisp                    # Point d'entrée principal
│   └─> Charge tous les modules et lance l'interface
│
├── src/                         # Code source du système expert
│   ├── base-faits.lisp          # Gestion de la base de faits (ordre 0+)
│   │   ├─> Structure : *base-faits* (liste d'association)
│   │   ├─> Opérations : ajouter, obtenir, modifier, comparer
│   │   └─> Traçabilité : *historique-faits*
│   │
│   ├── base-regles.lisp         # Données brutes des recettes
│   │   ├─> *donnees-recettes* (recettes finales)
│   │   └─> *donnees-compositions* (compositions intermédiaires)
│   │
│   ├── gestion-regles.lisp      # Gestion de la base de règles
│   │   ├─> Structure : REGLE (defstruct)
│   │   ├─> Opérations : ajouter, évaluer, sélectionner
│   │   └─> Filtrage : saisons, type, végétarien
│   │
│   ├── moteur.lisp              # Moteurs d'inférence
│   │   ├─> chainage-avant() : stratégie en largeur
│   │   ├─> chainage-avant-profondeur() : stratégie en profondeur
│   │   ├─> chainage-arriere(but) : goal-driven
│   │   ├─> chainage-mixte(buts) : combinaison
│   │   └─> Traçabilité : *regles-declenchees*
│   │
│   └── interface.lisp           # Interface utilisateur
│       ├─> interface-principale() : menu
│       ├─> Saisie : ingrédients, matériel, filtres
│       ├─> Lancement : recherche, vérification
│       └─> Affichage : résultats, trace, statistiques
│
├── donnees/
│   └── recettes.lisp            # Parsing et chargement des règles
│       ├─> charger-recettes()
│       ├─> charger-compositions-intermediaires()
│       └─> charger-recettes-finales()
│
├── tests/
│   └── scenario-1.lisp          # Scénarios de test
│       ├─> executer-scenario-1() : quiche lorraine
│       ├─> executer-scenario-2() : végétarien
│       └─> executer-scenario-3() : chaînage arrière
│
└── docs/
    ├── SUJET-TP3.md             # Sujet du TP
    ├── RAPPORT-TP2.md           # Rapport précédent
    └── RAPPORT-TP3.md           # Rapport à compléter
```

---

## Composants principaux

### 1. Base de faits (`src/base-faits.lisp`)

**Responsabilité** : Gestion des connaissances courantes (état actuel du système)

**Structures de données** :

- `*base-faits*` : liste d'association (clé . valeur)
- `*historique-faits*` : traçabilité des modifications

**Fonctions principales** :

- `initialiser-base-faits()` : réinitialisation
- `ajouter-fait(cle, valeur)` : ajout/mise à jour
- `obtenir-fait(cle)` : récupération
- `comparer-fait(cle, operateur, valeur)` : comparaison pour conditions
- `decremente-fait(cle, quantite)` : pour ordre 0+
- `afficher-base-faits()` : visualisation

### 2. Base de règles (`src/gestion-regles.lisp`)

**Responsabilité** : Gestion des connaissances générales (règles du domaine)

**Structures de données** :

```lisp
(defstruct regle
  nom          ; Identifiant unique
  description  ; Description textuelle
  conditions   ; Liste de conditions
  conclusion   ; Fait déduit
  actions      ; Actions à effectuer
  priorite     ; Niveau de priorité
  profondeur   ; 0=finale, 1+=intermédiaire
  metadata)    ; Saisons, type, végétarien
```

**Fonctions principales** :

- `ajouter-regle(...)` : ajout d'une règle
- `evaluer-conditions(conditions)` : vérification
- `regles-candidates()` : règles applicables (chaînage avant)
- `regles-pour-but(but)` : règles concluant un but (chaînage arrière)
- `appliquer-regle(regle)` : exécution
- `regle-respecte-filtres-p(regle)` : filtrage

### 3. Moteur d'inférence (`src/moteur.lisp`)

**Responsabilité** : Raisonnement et déduction de nouveaux faits

**Moteurs implémentés** :

#### Chaînage avant (Forward Chaining)

- **Stratégie** : partir des faits → déduire conclusions
- **Fonction** : `chainage-avant()`
- **Algorithme** :
  1. Trouver toutes les règles applicables
  2. Trier par profondeur (compositions d'abord)
  3. Appliquer les règles
  4. Répéter jusqu'à saturation

#### Chaînage arrière (Backward Chaining)

- **Stratégie** : partir d'un but → prouver conditions
- **Fonction** : `chainage-arriere(but)`
- **Algorithme** :
  1. Vérifier si le but est déjà prouvé
  2. Sinon, chercher règles concluant ce but
  3. Prouver récursivement les conditions
  4. Appliquer la règle si toutes conditions vraies

#### Chaînage mixte

- **Stratégie** : combinaison avant + arrière
- **Fonction** : `chainage-mixte(buts-prioritaires)`

**Traçabilité** :

- `*regles-declenchees*` : historique des règles appliquées
- `obtenir-trace-inference()` : trace complète
- `expliquer-conclusion(fait)` : arbre de déduction

### 4. Interface utilisateur (`src/interface.lisp`)

**Responsabilité** : Interaction avec l'utilisateur

**Fonctionnalités** :

- Saisie des ingrédients disponibles
- Saisie du matériel de cuisine
- Configuration des filtres (végétarien, saisons, types)
- Lancement de la recherche (chaînage avant)
- Vérification d'une recette spécifique (chaînage arrière)
- Affichage des résultats avec explication
- Visualisation de la trace du raisonnement
- Statistiques

### 5. Données des recettes (`donnees/recettes.lisp`)

**Responsabilité** : Parser les données brutes et créer les règles

**Fonction principale** : `charger-recettes()`

- Parse `*donnees-compositions*` et `*donnees-recettes*`
- Crée les structures REGLE correspondantes
- Ajoute les règles à `*base-regles*`

---

## Ordre 0+ : Caractéristiques

### Format des faits

```lisp
;; Ingrédients (numériques)
(beurre . 200)
(oeuf . 12)

;; Matériel (booléens)
(bol . t)
(four . nil)

;; Filtres (booléens)
(recette_vegetarienne . t)
(type_plat . t)
```

### Format des règles

```lisp
;; Conditions : (cle operateur valeur)
'((beurre >= 20)
  (oeuf >= 8)
  (bol = t)
  (type_plat = t))

;; Actions : décrémenter les ingrédients
'((decremente beurre 20)
  (decremente oeuf 8))

;; Conclusion : nouveau fait déduit
'quiche_lorraine
```

---

## Flux d'exécution

### Scénario typique

```txt
1. Démarrage
   └─> main.lisp charge tous les modules

2. Initialisation
   ├─> initialiser-base-faits()
   └─> charger-recettes()

3. Interface utilisateur
   ├─> Saisie ingrédients → ajouter-fait()
   ├─> Saisie matériel → ajouter-fait()
   └─> Configuration filtres → ajouter-fait()

4. Raisonnement
   ├─> chainage-avant() OU
   └─> chainage-arriere(recette)

5. Résultats
   ├─> afficher-recettes-realisables()
   ├─> afficher-trace-complete()
   └─> expliquer-exclusion()
```

---

## État actuel du projet

### Ce qui est fait

- [x] Architecture complète définie
- [x] Tous les fichiers créés avec structure
- [x] Signatures de toutes les fonctions
- [x] Commentaires détaillés (TODO à implémenter)
- [x] Documentation des structures de données
- [x] 14 recettes finales définies
- [x] 7 compositions intermédiaires définies

### Ce qui reste à implémenter

#### Priorité 1 (Fonctionnalités obligatoires)

- [ ] Corps des fonctions de `base-faits.lisp`
- [ ] Corps des fonctions de `gestion-regles.lisp`
- [ ] Moteurs d'inférence dans `moteur.lisp`
- [ ] Interface utilisateur dans `interface.lisp`
- [ ] Parsing des recettes dans `donnees/recettes.lisp`

#### Priorité 2 (Tests et validation)

- [ ] Implémenter les 3 scénarios de test
- [ ] Tester le chaînage avant
- [ ] Tester le chaînage arrière
- [ ] Valider la traçabilité

#### Priorité 3 (Extensions optionnelles)

- [ ] Visualisation graphique de l'arbre de raisonnement
- [ ] Export des résultats (CSV, JSON)
- [ ] Interface web (optionnel)

---

## Simplicité du design

Le projet est conçu pour être **simple et maintenable** :

1. **Séparation claire** : chaque fichier a une responsabilité unique
2. **Nomenclature française** : cohérence avec le domaine (cuisine)
3. **Structures simples** : listes d'association et defstruct
4. **Pas de dépendances externes** : Common Lisp standard uniquement
5. **Commentaires exhaustifs** : chaque fonction est documentée
6. **Traçabilité intégrée** : historiques et traces pour débogage

---

## Prochaines étapes recommandées

### Ordre de développement suggéré

1. **base-faits.lisp** (fondation)
   - Implémenter les opérations de base
   - Tester avec des faits simples

2. **gestion-regles.lisp** (structure)
   - Implémenter ajouter-regle et evaluer-conditions
   - Créer quelques règles de test

3. **donnees/recettes.lisp** (contenu)
   - Parser les données brutes
   - Créer les 21 règles

4. **moteur.lisp** (intelligence)
   - Implémenter chainage-avant d'abord
   - Puis chainage-arriere
   - Ajouter la traçabilité

5. **interface.lisp** (interaction)
   - Menu principal
   - Saisies utilisateur
   - Affichage des résultats

6. **tests/scenario-1.lisp** (validation)
   - Tester avec des cas concrets
   - Valider les résultats attendus

---

## Notes importantes

- **Ordre 0+** : Gère les quantités numériques ET les booléens
- **Profondeur** : 0 = recette finale, 1+ = composition intermédiaire
- **Filtres** : Si aucun filtre actif dans une catégorie → tous acceptés
- **Traçabilité** : Essentielle pour l'explicabilité (critère d'évaluation)
- **Tests** : Minimum 3 scénarios différents requis

---

Auteurs : Nathan GUEVARA, Loïc ZHOU
Cours : IA01 - UTC A25
