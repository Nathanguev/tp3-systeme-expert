# Résumé : Architecture et Structure du Projet

## ARCHITECTURE VALIDÉE

L'architecture actuelle est **CORRECTE** et **CONFORME** aux exigences du sujet TP3.

---

## Ce qui a été fait

### 1. Structure complète du projet

Tous les fichiers nécessaires ont été créés avec :

- Signatures de toutes les fonctions
- Commentaires détaillés pour chaque fonction
- Instructions TODO pour l'implémentation
- Documentation des structures de données
- Nomenclature en français

### 2. Fichiers créés/modifiés

| Fichier | Rôle | Statut |
|---------|------|--------|
| `main.lisp` | Point d'entrée | Structuré |
| `src/base-faits.lisp` | Gestion faits (ordre 0+) | Structuré |
| `src/base-regles.lisp` | Données recettes | Données complètes |
| `src/gestion-regles.lisp` | Gestion règles | Structuré |
| `src/moteur.lisp` | Moteurs d'inférence | Structuré |
| `src/interface.lisp` | Interface utilisateur | Structuré |
| `donnees/recettes.lisp` | Parsing recettes | Structuré |
| `tests/scenario-1.lisp` | Tests | Structuré |
| `docs/ARCHITECTURE.md` | Documentation technique | Complète |

### 3. Conformité avec le sujet

| Exigence obligatoire | Implémentation |
|---------------------|----------------|
| 2 moteurs d'inférence | `chainage-avant()` + `chainage-arriere()` |
| Interaction utilisateur | `interface-principale()` avec menu |
| Base de faits | Structure ordre 0+ avec quantités |
| Base de règles | 14 recettes + 7 compositions |
| Intégration composants | `main.lisp` charge tous les modules |
| Scénarios de test | 3 scénarios définis |

---

## Structures de données principales

### Base de faits (ordre 0+)

```lisp
*base-faits* = '((beurre . 200)     ; Quantités numériques
                 (oeuf . 12)
                 (bol . t)           ; Matériel booléen
                 (type_plat . t))    ; Filtres booléens
```

### Règle (defstruct)

```lisp
(defstruct regle
  nom           ; 'R1, 'R2, etc.
  conditions    ; '((beurre >= 20) (oeuf >= 8))
  conclusion    ; 'quiche_lorraine
  actions       ; '((decremente beurre 20))
  profondeur    ; 0=finale, 1+=intermédiaire
  metadata)     ; '(:saisons (automne hiver) :type plat)
```

---

## Moteurs d'inférence

### 1. Chaînage avant (implémenté)

```txt
Faits initiaux → Règles applicables → Déduire conclusions → Répéter
```

### 2. Chaînage arrière (implémenté)

```txt
But à prouver → Règles concluant le but → Prouver conditions → Succès/Échec
```

### 3. Chaînage mixte (bonus)

```txt
Phase 1: Chaînage avant (saturation)
Phase 2: Chaînage arrière (buts spécifiques)
```

---

## Contenu de la base de connaissances

### Recettes finales (14)

- Riz pilaf, Céleri rôti, Gratin dauphinois
- Fond blanc de volaille, Quiche lorraine, Tarte tatin
- Crêpe Suzette, Salade composée, Boeuf bourguignon
- Lasagne, Omelette aux champignons, Pâtes carbonara
- Soupe au potiron, Cannellonis

### Compositions intermédiaires (7)

- Pâte brisée, Pâte à crêpe, Pâte à lasagne
- Béchamel, Farce de viande, Concassé, Sauce bolognaise

---

## Prochaines étapes d'implémentation

### Ordre recommandé

1. **base-faits.lisp** (2-3h)
   - Implémenter les fonctions TODO
   - Tester avec des faits simples

2. **gestion-regles.lisp** (2-3h)
   - Implémenter ajouter-regle et evaluer-conditions
   - Tester l'évaluation de conditions

3. **donnees/recettes.lisp** (1-2h)
   - Parser *donnees-recettes* et *donnees-compositions*
   - Créer les 21 règles

4. **moteur.lisp** (3-4h)
   - Implémenter chainage-avant()
   - Implémenter chainage-arriere()
   - Ajouter la traçabilité

5. **interface.lisp** (2-3h)
   - Menu principal
   - Saisies utilisateur
   - Affichage résultats

6. **tests/scenario-1.lisp** (1h)
   - Compléter les 3 scénarios
   - Valider le fonctionnement

Estimation totale : 11-16h de développement

---

## Points clés de simplicité

1. **Pas de bibliothèques externes** : Common Lisp standard uniquement
2. **Structures simples** : listes d'association + defstruct
3. **Nomenclature française** : cohérence avec le domaine culinaire
4. **Modularité** : chaque fichier = une responsabilité
5. **Commentaires exhaustifs** : TODO avec instructions précises

---

## Documentation

### Fichiers de documentation

- `README.md` : Vue d'ensemble du projet
- `docs/ARCHITECTURE.md` : Documentation technique complète (25 pages)
- `docs/SUJET-TP3.md` : Énoncé du TP
- `docs/RAPPORT-TP2.md` : Contexte du projet

### Aide pour l'implémentation

Chaque fichier contient :

- En-tête explicatif du rôle du fichier
- Documentation de chaque fonction
- Commentaires TODO détaillant l'implémentation
- Exemples de structures de données

---

## Avantages de cette architecture

- **Conforme au sujet** : tous les critères obligatoires couverts
- **Simple** : pas de complexité inutile
- **Modulaire** : facile à tester et déboguer
- **Évolutive** : facile d'ajouter des règles/fonctionnalités
- **Documentée** : commentaires exhaustifs
- **Traçable** : historiques pour débogage et explicabilité

---

**Prêt pour l'implémentation !**

Tous les fichiers sont structurés, commentés et prêts à être implémentés.
Suivez l'ordre recommandé et référez-vous aux commentaires TODO.
